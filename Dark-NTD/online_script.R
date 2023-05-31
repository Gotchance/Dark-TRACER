argv <- commandArgs(TRUE)				# 外部の引数を受け取る方法

#====トラフィック解析エンジン オンライン実行用メインスクリプト====#

library(jsonlite)
library(slam);library(doMC);library(foreach);library(dplyr);library(tidyr)
library(reshape2)
library(TensorSlam)   # Installation: devtools::install_github('liberaldays/TensorSlam')
library(slackr)

# source("./online_param.R")        # by Han

#古い，3階専用だが高速なスクリプトを直接読むならここで
# source("./TensorSlam/old/FSTD.R")
# source("./TensorSlam/old/HOSVD.R")
# source("./TensorSlam/old/MU-NTD.R")


#====パラメータ (by Han)====#
year <- as.numeric(argv[1])
month <- as.numeric(argv[2])
day <- as.numeric(argv[3])
hour <- as.numeric(argv[4])
minute <- as.numeric(argv[5])

SENSORS <- as.character(argv[6])   				 # センサ名 (入力ファイル名と連携)
PROGRAM_PATH <- as.character(argv[7])				#プログラムファイルパス
INPUT_FILE_MASTER <- as.character(argv[8])		#==入力ファイルパス
OUTPUT_PATH <- as.character(argv[9])				#結果出力先
JSON_OUTPUT <- as.character(argv[10])				#JSONアラート出力先ディレクトリ

#==NTD基底数
#NTDの基底の数を指定する
#小さいほど計算が速くなるが，4程度は欲しい
CORE_SIZE <- as.numeric(argv[11])

#==FSTDの選択fiber数(基底数はこれの二乗)
#低ランク近似のFSTDを行うさい，選択するfiberの数を指定する．
#低ランク近似の基底数はこの二乗になる．
#CORE_SIZE以上の値であることが望ましい．
FSTD_NUM <- as.numeric(argv[12])

#反復回数
ITERATION_NUM <- as.numeric(argv[13])


############
# 固定パラメータ
alert_threshold<-c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
# alert_threshold<-c(0)
ignore.list.port <- c(22,23,80,81,445,1433,2323,3389,5555,8080,52869)				# 無視したいportリスト
WINDOW_SIZE <- 30*60				#何秒おきにテンソルを作成するかを指定
SMALL_WINDOW_SIZE <- WINDOW_SIZE #昔の (現在不要)
PERIOD_TO_SAVE <- WINDOW_SIZE				#==結果保存間隔 (現在不要)
IP_oct <- 2					#==IPアドレスの考慮すべき上位オクテット数

# 並列計算コア数
# detectCores()/2 -1で最高効率
# (搭載スレッド数の半分，搭載物理コア数から並列計算のマスタースレッドを除いた数)
CORE_NUM <- 2

# slackにアラートを投稿するかどうか (incoming webhook)
# ~/.slackr に必要な情報を書き込む．
SEND_SLACK <- F

year <- as.character(year)
month <- formatC(month, width=2, flag="0")
month <- as.character(month)
day <- formatC(day, width=2, flag="0")
day <- as.character(day)
hour <- formatC(hour, width=2, flag="0")
hour <- as.character(hour)
minute <- formatC(minute, width=2, flag="0")
minute <- as.character(minute)
date <- paste0(year,month,day,hour,minute)
startdate<-paste0(year,"-",month,"-",day," ",hour,":",minute,":00")
# startdate_UNIX <- as.numeric(as.POSIXct(startdate, origin="1970-1-1",tz="Japan"))

FIRST_FILE_TIME <- as.POSIXct(strptime(startdate,'%Y-%m-%d %H:%M:%S'), tz="Japan")				#==分析開始時刻
dir.create(OUTPUT_PATH, showWarnings = F, recursive = T)				#=出力先を作成
dir.create(JSON_OUTPUT, showWarnings = F, recursive = T)				#=アラート出力先を作成

source(paste0(PROGRAM_PATH, "functions_calc.R"))
IGNORE_LIST <- paste0(PROGRAM_PATH, "ShodanList")				#Shodan等無視したいIPのリストが記載されたファイル．


#====ここから実行される====#

#====FIRST_FILE_TIMEの時刻から，ファイルの読み込みを始める====#
# SENSORS <- scan(SENSOR_PATH, what=character(), sep="\n")      # by Han (センサ名直接読み込む)
# 実験番号 基本的に各センサ，スクリーニング状況ごとに固有番号が振られる
EXP_NUMBER <- 1:length(SENSORS)
MASTER <- makeMASTER_LIST(EXP_NUMBER,SENSORS)
# 1センサにつきTCPとUDPで2つのテンソルをつくるので，EXP_NUMBERを2倍しておく
# EXP_NUMBER <- 1:(length(SENSORS)*2)       # by Han (UDPは計算しない)
EXP_NUMBER <- 1:length(SENSORS)
METHOD <- "FSTD"
# tcpdumpのfilter. Shodan等の無視したいIPのリストが記載されたIGNORE_LISTを読む
filter.ip <- ""
if ((IGNORE_LIST != "") && (!is.null(IGNORE_LIST)))       # by Han
  filter.ip <- load.tcpdump.filter(IGNORE_LIST, ignore.list.port)		# by Han, filter済みの場合コメントアウト

message("変化点検知エンジン稼働開始")

#=初期読み込み時刻は毎分であるので，FIRST_FILE_TIMEを変更する
FIRST_FILE_TIME <- floorTime(FIRST_FILE_TIME,1)

registerDoMC(2)
message("データ読み込み開始")

#====データ読み込み開始(初回のみ無条件でファイルを読み込む．以降は必要に応じてファイルを読んでゆく)====#
# 初期データ読み込み
MASTER <- foreach(e = EXP_NUMBER)%dopar%{
  message(paste0("EXP_NUMBER: ",e))
  for(i in 1:length(MASTER[[e]])){ #初期オブジェクトの用意
    assign(names(MASTER[[e]])[i],MASTER[[e]][[i]])
  }

  firstFilePath <- convertPOSIXTimeToFilePath(FIRST_FILE_TIME,SENSORNAME,INPUT_FILE_MASTER,FILENAME_EXTENTION)
  data <- loadPcapFile(fullFilePath_vec = firstFilePath,IP_oct,
                       FILENAME_EXTENTION=".dmp",PROTOCOL=PROTOCOL,filter.ip=filter.ip)
  lastLoadedTime <- FIRST_FILE_TIME #最後に読んだファイルの時刻

  MASTER[[e]]$data <- data
  MASTER[[e]]$lastLoadedTime <- lastLoadedTime
  return(MASTER[[e]])
}

#どれだけの数の結果を得たら保存すべきか
maxLengthStored <- PERIOD_TO_SAVE/SMALL_WINDOW_SIZE

#====処理ループ開始 全ファイルを処理(nextFileIdxが存在しないファイルを示し，且つそのファイルの読み込み指示が生じるまで)するまでループ====#

# while(TRUE) {     # by Han

#   source("./update_param.R")      # by Han
  # ~/.slackr から設定読み込み
  if (SEND_SLACK) slackr_setup()
  registerDoMC(CORE_NUM)

  MASTER <- foreach(e=EXP_NUMBER)%dopar%{
    beforeIO_time <- Sys.time() # 処理速度計測用
    message(paste0("EXP_NUMBER: ",e))
    for(i in 1:length(MASTER[[e]])){ # オブジェクトの用意
      assign(names(MASTER[[e]])[i],MASTER[[e]][[i]])
    }
    message(paste0("dim: ",dim(data)))
    #===フラグで指定された場合,新しいデータを読み込む===#
    if(LOAD_NEXT_FLAG == T){
      # SMALL_WINDOW_SIZE分まで1分毎のファイルを読み込む
      nextLoadTime <- lastLoadedTime + (1:(SMALL_WINDOW_SIZE/60))*60
      lastLoadedTime <- nextLoadTime[length(nextLoadTime)] #POSIXct型
      message(paste0(lastLoadedTime,"を読み込みます"))
      nextFilePath <- sapply(nextLoadTime,function(i)convertPOSIXTimeToFilePath(i,sensorName=SENSORNAME,INPUT_FILE_MASTER,FILENAME_EXTENTION))

      ############################################
      # 次のファイル読み込み
      # maxFileWaitTime秒だけ待つ
      # パケットがない場合ファイルが吐き出されないので，
      # 最後のファイルが見つからなくても10分先までのファイルがどれか見つかった場合，待機しない
      ############################################
      futureLoadTime <- nextLoadTime + (SMALL_WINDOW_SIZE/60 - 1)*60
      futureFilePath <- sapply(futureLoadTime,
                          function(i)convertPOSIXTimeToFilePath(i,sensorName=SENSORNAME,INPUT_FILE_MASTER,FILENAME_EXTENTION))

      # 1(1分ごとにファイル配置)+30(オマケ)分だけ待ってみる
      maxFileWaitTime <- lastLoadedTime+ (1+30)*60
      while(!any(file.exists(futureFilePath)) & Sys.time() < maxFileWaitTime ){
        message(paste0(SENSORNAME,": ファイルが存在していないので待機します"))
        Sys.sleep(60)
      }
      # 時間までまったけど結局ファイルが存在しなかった場合
      if(!any(file.exists(futureFilePath)) & Sys.time()>=maxFileWaitTime){
        message("センサ停止と判断 今までのデータを破棄します")
        # 実際はnullうめ 再度LOAD_NEXT_FLAGがTで，次の30minが読まれるはず．
        newdata <- NULL
      }else{  # 読み込める場合
        newdata <- loadPcapFile(fullFilePath_vec = nextFilePath,
                                IP_oct,
                                PROTOCOL = PROTOCOL,
                                FILENAME_EXTENTION = ".dmp",
                                filter.ip = filter.ip)
      }
      LOAD_NEXT_FLAG <- F #filesToUseから次のファイルを読み込むか否かのフラグ
    }

    #===前ループまでに使用していたデータを併せトラフィックデータを納めたdataを作る===#
    # 前ステップで使用し，今回も使用するデータdataToUseがある場合
    if(!is.logical(dataToUse)){
      message("dataToUseが存在します")
      data <- dataToUse
      dataToUse <- FALSE
      # 前ステップで，必要な窓幅分より後の時間帯のデータdataForLaterを結合
      if(!is.logical(dataForLater)){
        data <- bind_rows(data,dataForLater)
        message("dataForLaterと結合しました")
        dataForLater <- FALSE
      }
      if(exists("newdata")){ #新しく読んだデータがある場合これも結合
        data <- bind_rows(data,newdata)
        message("newdataと結合しました")
        rm(newdata)
      }
    }

    if(dataForLater!=FALSE){ #おそらく不要な文 dataToUseが存在せず，でもdataForLaterはある場合の処理
      if(exists("newdata")){
        data <- bind_rows(dataForLater,newdata)
        message("dataForLaterとnewdataを結合しました")
        rm(newdata)
      }
      dataForLater <- FALSE
    }

    #=WINDOW_SIZE分のデータが揃っているか確認=#
    if(nrow(data)>0){
    firstTimeIndata <- min(data[,1])
    lastTimeIndata <- max(data[,1])
    TimeDiffIndata <- lastTimeIndata - firstTimeIndata + 1
    }else{#データがない場合
    TimeDiffIndata <- 0
    firstTimeIndata <- FALSE
    lastTimeToUse <- FALSE
    }
    message(paste("TimeDiffIndata:",TimeDiffIndata))
    message(paste0("dim(data)=",dim(data)))

    GOTO_LOOP_BOTTOM <- F #ループの終わりに飛ぶ(ループは抜けない) (現在は使っていない)

    afterIO_time <- Sys.time() #処理速度計測用

    # 十分な時間分のデータが揃っていない場合
    # (イコールも含める，次のファイルに同じ時間のデータが格納されている可能性がある)
#     if(TimeDiffIndata <= WINDOW_SIZE){    # by Han
    if(TimeDiffIndata < WINDOW_SIZE){
      message("十分なデータが存在していません 次のデータの読み込みを行います")
      GOTO_LOOP_BOTTOM <- T #ループをやり直し，新たなデータを結合させる
#       LOAD_NEXT_FLAG <- T     # by Han
      LOAD_NEXT_FLAG <- F
      dataToUse <- data
      data <- FALSE

    }else{ #十分なデータが存在している場合
      firstTimeToUse <- firstTimeIndata #テンソルの最初
      message(paste0("データの本当の最初の時刻: ",
                     as.POSIXct(firstTimeToUse, origin = "1970-01-01", tz="Japan")))
#       firstTimeToUse <- floorTime(firstTimeToUse, 30)		# by Han
      message(paste0("floorした時刻: ", firstTimeToUse))
      lastTimeToUse <- firstTimeToUse + WINDOW_SIZE - 1
      rowIdxToUse <- which(data$time >= firstTimeToUse & data$time <= lastTimeToUse)
      dataToUse <- data[rowIdxToUse, ]
      dataForLater <- data[-rowIdxToUse, ]

      #==dataToUseを元に解析を行う==#
      #dataToUseはtime|src_IPs|src_ports|dst_IPs|dst_portsのカラムを持つdataframe
      try({
        message(paste0("テンソル作成開始時間:",
                       as.POSIXlt(firstTimeToUse, origin = "1970-01-01", tz="Japan")))
        message(paste0("終了時間:",
                       as.POSIXlt(lastTimeToUse, origin = "1970-01-01", tz="Japan")))
        alertTime <- as.POSIXlt(firstTimeToUse, origin = "1970-01-01", tz="Japan")
        alertTime$sec <- 0
        alertTime$min <- alertTime$min %/%
                         (WINDOW_SIZE / 60) * (WINDOW_SIZE / 60)
        alertTime <- as.numeric(alertTime)
        message(paste0("アラート判定時刻:",
                   as.POSIXlt(alertTime,origin="1970-01-01", tz="Japan")))

        # テンソル分解によるアラート判定
        # --------------------------------
        # テンソルの作成
        X.arr <- mkTensor(data=dataToUse,TENSOR_WINDOWSIZE=WINDOW_SIZE,TENSOR_COLSIZE=60,IP_oct)

        # 高速なテンソル分解
        res <- calcNTDWithError(X.arr,CORE_SIZE=CORE_SIZE,FSTD_NUM=FSTD_NUM,method=METHOD)

        # directoryの作成
        dirs1_name <- paste("FSTD_NUM",FSTD_NUM,"method",METHOD,"CORE_SIZE",CORE_SIZE,"ITERATION",ITERATION_NUM,"/",sep="_")
        dirs2_name <- paste0(SENSORNAME,"-",FILENAME_EXTENTION,"/")
        dirs3_name <- paste0(as.Date(as.POSIXlt(firstTimeToUse,origin="1970-01-01", tz="Japan")),"/")
        dir.create(path = paste0(OUTPUT_PATH,dirs1_name,dirs2_name,dirs3_name),showWarnings = F,recursive = T)

        for(j in 1:length(alert_threshold)){
		      # 分解結果をもとにアラート判定
		      abP <- raiseAlert(res,NOW_NUM=firstTimeToUse,alert_threshold[j])
		      abP_text <- paste0("abP_threshold", alert_threshold[j])

		      message(abP_text,":")
		      print(abP)
		      # save as JSON-ish file
		      if(abP[1,1] != -1){
		      	JSON_OUTPUT_tmp <- paste0(JSON_OUTPUT,SENSORNAME,"_FSTD_NUM_",FSTD_NUM,"_CORE_SIZE_",CORE_SIZE,"_TH_",alert_threshold[j],"_ITERATION_",ITERATION_NUM,".log")
		        addJSON(abP, SENSORID = SENSORNAME, PROTOCOL = PROTOCOL,
		                JSON_OUTPUT = JSON_OUTPUT_tmp, slack = SEND_SLACK)
		        message(paste0("saved to ", JSON_OUTPUT))
		      }
		    }
        result <- list(res=res,abP=abP)

        # 処理速度計測用
        afterCalc_time <- Sys.time()
        IOtime <- afterIO_time - beforeIO_time
        Calctime <- afterCalc_time - afterIO_time
        Totaltime <- IOtime + Calctime
        # 結果保存関連
        # 分解結果をRdataで保存する
        result <- append(result,IOtime)
        result <- append(result,Calctime)
        result <- append(result,Totaltime)

        result <- append(result,firstTimeToUse)
        # テンソル作成に使用した元データも一緒に保存
        # result <- append(result,list(dataToUse))
        # 分解対象のテンソルも一緒に保存
        # result <- append(result,list(X.arr))
        result <- append(result,PROTOCOL)
        filename <- paste0(as.POSIXlt(firstTimeToUse,origin="1970-01-01", tz="Japan"),PROTOCOL,".Rdata")
        savedir <- paste0(OUTPUT_PATH,dirs1_name,dirs2_name,dirs3_name,filename)
        save(result,file = savedir)
        message(paste0("saved results to ",savedir))
      })
      #==解析終了後，次のステップのためにdataToUseの前部を削除する==#
      lastTimeToSplit <- firstTimeToUse + SMALL_WINDOW_SIZE - 1
      dataToUse <- filter(dataToUse,time > lastTimeToSplit)

      #==次ファイルの読み込みを行う
#       LOAD_NEXT_FLAG <- T     # by Han
      LOAD_NEXT_FLAG <- F
    }

    MASTER[[e]]$data <- data
    MASTER[[e]]$LOAD_NEXT_FLAG <- LOAD_NEXT_FLAG
    MASTER[[e]]$dataToUse <- dataToUse
    MASTER[[e]]$dataForLater <- dataForLater
    MASTER[[e]]$firstTimeIndata <- firstTimeIndata
    MASTER[[e]]$lastTimeIndata <- lastTimeIndata
    MASTER[[e]]$firstTimeToUse <- firstTimeToUse
    MASTER[[e]]$lastTimeToUse <- lastTimeToUse
    MASTER[[e]]$lastLoadedTime <- lastLoadedTime
    return(MASTER[[e]])
  }
# }     # by Han
