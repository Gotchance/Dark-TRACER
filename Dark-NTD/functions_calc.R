
#====使用関数群 データ読み込み関係====#
getINPUT_FILE_PATHS <- function(INPUT_FILE_MASTER){ #入力データの一覧を返す
  INPUT_FULL_PATHS <- sapply(INPUT_FILE_MASTER,function(path){
  dirs <- list.files(path,full.names=T,pattern=".*end$") #各ファイルのフルパス取得
  })
  INPUT_FULL_PATHS <- as.vector(INPUT_FULL_PATHS)
  return(INPUT_FULL_PATHS)
}


makeMASTER_LIST <- function(EXP_NUMBER,SENSORS){
  #エンジンでは異なるセンサ，プロトコル，前処理(全部まとめて環境とする)のデータを並列計算する
  #データが送信される時間はまちまちで，たまに届かないこともある．処理がエラーになることもある．
  #どの環境の処理がどこまで終わったのか，各種パラメータと一緒に記したタスクリストを作成する
  #このタスクリストは処理の開始前後に更新される
  #EXP_NUMBER:環境の数(online_script.R内でファイルパスを元に決定)
  #データが置かれるディレクトリの一覧(online_script.Rで自動生成)
  masterTCP <- lapply(EXP_NUMBER,function(i){
    data <- FALSE

    SENSORNAME <- SENSORS[i]

    FILENAME_EXTENTION  <- ".end"

    PROTOCOL <- "TCP"
    LOAD_NEXT_FLAG <- F
    dataToUse <- FALSE
    dataForLater <- FALSE
    firstTimeIndata <- FALSE
    lastTimeIndata <- FALSE
    firstTimeToUse <- FALSE
    lastTimeToUse <- FALSE
    lastLoadedTime <- FALSE
    return(list(data=data,SENSORNAME=SENSORNAME,
      FILENAME_EXTENTION=FILENAME_EXTENTION,
      PROTOCOL=PROTOCOL,
      LOAD_NEXT_FLAG=LOAD_NEXT_FLAG,
      dataToUse=dataToUse,dataForLater=dataForLater,
      firstTimeIndata=firstTimeIndata,lastTimeIndata=lastTimeIndata,
      firstTimeToUse=firstTimeToUse,lastTimeToUse=lastTimeToUse,
      lastLoadedTime=lastLoadedTime
    ))
  })
  masterUDP <- lapply(EXP_NUMBER,function(i){
    data <- FALSE

    SENSORNAME <- SENSORS[i]

    FILENAME_EXTENTION  <- ".end"

    PROTOCOL <- "UDP"
    LOAD_NEXT_FLAG <- F
    dataToUse <- FALSE
    dataForLater <- FALSE
    firstTimeIndata <- FALSE
    lastTimeIndata <- FALSE
    firstTimeToUse <- FALSE
    lastTimeToUse <- FALSE
    lastLoadedTime <- FALSE
    return(list(data=data,
      SENSORNAME=SENSORNAME,
      FILENAME_EXTENTION=FILENAME_EXTENTION,
      PROTOCOL=PROTOCOL,
      LOAD_NEXT_FLAG=LOAD_NEXT_FLAG,
      dataToUse=dataToUse,dataForLater=dataForLater,
      firstTimeIndata=firstTimeIndata,lastTimeIndata=lastTimeIndata,
      firstTimeToUse=firstTimeToUse,lastTimeToUse=lastTimeToUse,
      lastLoadedTime=lastLoadedTime
    ))
  })
  master <- c(masterTCP,masterUDP)

  return(master)
}



loadNextFile <- function(filesToUse,nextFileIdx,FILENAME_EXTENTION,PROTOCOL=0){
  #オフライン解析用 今は使われていない関数
  if(FILENAME_EXTENTION == ".rdata" | FILENAME_EXTENTION == ".Rdata"){
    load(filesToUse[nextFileIdx])
    data <- data
    if(PROTOCOL=="UDP"){
      data <- ungroup(data)
      data <- filter(data,protoID==17)
    }
    if(PROTOCOL=="TCP"){
      data <- ungroup(data)
      data <- filter(data,protoID==6)
    }
  }
  if(FILENAME_EXTENTION == "csv"){
    cat("csv入力は動作未検証！\n")
    cat("警告:csvのヘッダーは作りません\n")
    data <- read.csv(file = filesToUse[nextFileIdx],header = F)
  }
  return(data)
}

# by Han (portもparam指定できるように)
# # ファイルからtcpdump用のIPフィルタを作成
# load.tcpdump.filter <- function(filepath="./ShodanList") {
#   ignore.list.ip <- readLines(filepath)
#   ignore.list.port <- c(22, 23)
#   filter.ip <- paste0(" 'ip and not (dst port ",
#                       paste(ignore.list.port, collapse = " or dst port "),
#                       " or src net ",
#                       paste(ignore.list.ip, collapse = " or src net "),
#                       ")'")
#   message(paste0("tcpdump filter: ", filter.ip))
#   return(filter.ip)
# }

# ファイルからtcpdump用のIPフィルタを作成
load.tcpdump.filter <- function(filepath="./ShodanList", ignore.list.port) {
  ignore.list.ip <- readLines(filepath)
  filter.ip <- paste0(" 'tcp[13] & 255 = 2 and ip and not (dst port ",
                      paste(ignore.list.port, collapse = " or dst port "),
                      " or src net ",
                      paste(ignore.list.ip, collapse = " or src net "),
                      ")'")
  message(paste0("tcpdump filter: ", filter.ip))
  return(filter.ip)
}


loadPcapFile <- function(fullFilePath_vec, IP_oct, FILENAME_EXTENTION=".dmp",
                         PROTOCOL="ALL", filter.ip=""){
  # fullFilePath_vecで渡されたファイルパスのデータ(tcpdump出力)を読み込む．
  # fullFilePath_vec:データ(tcpdumpで読めるもの)のファイルパス，ベクトルで与えればベクトル内の全てを読む
  # PROTOCOLで"ALL"を設定しても，別にALL用の設定はない．TCP,UDPのみのフィルタリング機能に引っかからないだけ．
  # ファイルの書き込みが終わると.endが吐き出される．中身は.dmpに入るので，ここではdmpを指定
  # TODO:ALL指定時にICMPも入るときもある
  # ファイルが存在していないときはNULLが戻る
  data <- c()

  for(filePath in fullFilePath_vec){
    filePath.dmp <- paste0(gsub("\\..+$", "", filePath), FILENAME_EXTENTION)
    # 読めなかった場合character(0)がかえる．下のエラー処理必要なかった．
    e <- try({
      tmpdata <- system(paste0(
         "tcpdump -nnttq -r ", filePath.dmp, filter.ip
      ), intern=T)
    })
    if(class(e)=="try-error") tmpdata <- NULL

    data <- c(data, tmpdata)
  }
  if(!is.null(data)){
    #文字列なので，要素ごとに分割する
    splitted <- strsplit(data," |>|:|\\.")
    #ICMPのパケットは除く(ポート番号等の情報がないのでこの後エラーになる)
    # is.icmp <- (sapply(splitted,length)==15)
    # splitted <- splitted[!is.icmp]
    #
    # is.noport <- (sapply(splitted,length)==14) #ポート番号等の情報がないのでこの後エラーになる
    # splitted <- splitted[!is.noport]
    # is.noport <- (sapply(splitted,length)==16) #ICMPの亜種
    # splitted <- splitted[!is.noport]
    #
    # is.noport <- (sapply(splitted,length)==25) #ICMP unreachable
    # splitted <- splitted[!is.noport] #icmp unreachable
    # is.packetexceeded <- (sapply(splitted,length)==4)
    # splitted <- splitted[!is.packetexceeded]
    is.valid <- (sapply(splitted,length)==18)#最も一般的なパケット
    is.valid2 <- (sapply(splitted,length)==19)#最も一般的なパケット
    splitted <- splitted[is.valid|is.valid2]


    if(PROTOCOL == "TCP"){
      is.tcp <- (sapply(splitted,"[[",17)=="tcp")
      splitted <- splitted[is.tcp]
    }
    if(PROTOCOL == "UDP"){
      is.udp <- (sapply(splitted,"[[",17)=="UDP,")
      splitted <- splitted[is.udp]
    }

    Time <- as.numeric(sapply(splitted,"[[",1))
    src_IPs <- sapply(splitted,function(i){
      IPs <- i[4:(4+(IP_oct-1))]
      IPs <- paste(IPs,collapse=".")
    })
    src_ports <- as.numeric(sapply(splitted,"[[",8))
    dst_IPs <- sapply(splitted,function(i){
      IPs <- i[11:(11+(IP_oct-1))]
      IPs <- paste(IPs,collapse=".")
    })

    dst_ports <- as.numeric(sapply(splitted,"[[",15))

    data <- data.frame(time=Time,src_IPs,src_ports,dst_IPs,dst_ports)
  }

  return(data)
}

#====時刻計算関係====#
convertPOSIXTimeToFilePath <- function(timePosix,sensorName,INPUT_FILE_PATH,FILENAME_EXTENTION){
  #時間を入れると，その時間(秒以下切り捨て)に対応したファイルのフルパスを返す
  Date <- format(as.Date(timePosix),"%Y%m%d")
#   filename <- paste0(sensorName,"_",format(timePosix,"%Y%m%d%H%M"),"00",FILENAME_EXTENTION)
  filename <- paste0(sensorName,"_",format(timePosix,"%Y%m%d%H%M"),FILENAME_EXTENTION)      # by Han (ファイル名の無駄な00を削除)

  fileFullPath <- paste0(INPUT_FILE_PATH,"/",filename)
  return(fileFullPath)
}

convertPOSIXTimeToTimeStamp <- function(timePosix){
  timeStamp <- as.numeric(paste0(format(timePosix,"%Y%m%d%H%M"),"00"))
  return(timeStamp)
}

#====システム関係====#

addJSON <- function(abp, SENSORID, JSON_OUTPUT, PROTOCOL, slack=F){
  #raiseAlertの結果(abp)をjsonに吐き出す関数
  if (nrow(abp) > 0) {
    now <- as.POSIXlt(abp$times[1], origin = "1970-01-01")
    Datetime <- format(now, "%Y-%m-%dT%H:%M:%S+09:00")
    SensorID <- SENSORID

    devNull <- by(abp, INDICES = as.numeric(factor(abp$abPort)), function(d) {
      DstPort <- as.character(d[1,"abPort"])
      BotnetIP <- as.vector(d$abIP)

      alert.list <- list(Datetime = Datetime,
                         SensorID = SENSORID,
                         BotnetIP = BotnetIP,
                         DstPort = paste0(DstPort, "/", PROTOCOL))
      alert.json <- toJSON(alert.list, auto_unbox = T)

      message("Alert:")
      print(alert.json)
      # post to slack channel
      alert <- as.data.frame(alert.list)
      if (slack) slackr_bot(alert)

      if (!file.exists(JSON_OUTPUT)) {
        write(alert.json, file = JSON_OUTPUT)
      } else {
        write(alert.json, file = JSON_OUTPUT, append = T)
      }
    })
  }
}

mkTensor <- function(data,TENSOR_WINDOWSIZE=60*30,TENSOR_COLSIZE=60,IP_oct=2){
  #データのマトリクス(or データフレーム)からテンソルを作成する
  #テンソルの値は2以上のみ残す(1はfilter)
  #IP_oct=4の時しか実装していない
  tmpdata <- ungroup(data)
  hdata <- tmpdata
  if(nrow(hdata)>0){
    hdata <- data.frame(hdata)
    hdata <- mutate(hdata,minuteId=ceiling((time-(hdata[1,1]))/TENSOR_COLSIZE))
    hdata$minuteId[which(hdata$minuteId==0)] <- 1

    d <- group_by(hdata,minuteId,src_IPs,dst_ports)
    dd <- summarise(d,n())
    colnames(dd) <- c("minuteId","src_IP","dest_port","n")
    dd <- ungroup(dd)
    dd <- filter(dd,n>=2) #テンソルの値が2以上のところのみ残す

    if(IP_oct==2){
      oct2 <- strsplit(as.character(dd$src_IP),"\\.")
      oct <- as.vector(mapply(paste,sapply(oct2,"[[",1),sapply(oct2,"[[",2),sep="."))
      dd$src_IP <- oct
    }
    dd <- group_by(dd,minuteId,src_IP,dest_port)
    dd<- summarise(dd,sum(n))
    colnames(dd) <- c("minuteId","src_IP","dest_port","n")
    dd <- ungroup(dd)

    X.arr <- simple_sparse_zero_array(rep(1,(ncol(dd)-1)))
    idx <- list(c(NULL),c(NULL),c(NULL))
    idx[[1]] <- unlist(1:(TENSOR_WINDOWSIZE/TENSOR_COLSIZE))
    tmp <- expand.grid(0:255,0:255)[,c(2,1)] #2016/11/05 IPを変更
    tmp <- apply(tmp,1,function(i){paste(i[1],i[2],sep=".")})
    idx[[2]] <- unlist(tmp)
    idx[[3]] <- unlist(0:65535)

    for(i in 1:(ncol(dd)-1)){
      idx[[i]] <- unique(c(idx[[i]],levels(dd[,i])))
    }

    rdata <- dd
    dims <- rep(0,(ncol(dd)-1))
    for(i in 1:(ncol(dd)-1)){
      rdata[,i] <- match(unlist(dd[,i]), idx[[i]], nomatch=-1) #data.dfの各要素が、idxの何番目にあるか。
      dims[i] <- length(idx[[i]])
    }

    idx[[1]] <- as.character(idx[[1]]) #数字はcharacterにしないとエラー
    idx[[3]] <- as.character(idx[[3]])
    X.arr <- simple_sparse_array(i=as.matrix(rdata[,c(1:length(dims))]),
                                 v=unlist(rdata[,c(length(dims)+1)]),
                                 dimnames=idx,dim=dims)
    cat("dim(X.arr):\n")
    print(dim(X.arr))
    return(X.arr)
  }
}

calcNTD <- function(X.arr,CORE_SIZE,FSTD_NUM,method){
  #NTDを実施する関数
  #lra_ranksの違いを吸収する
  #lapplyの引数を変えれば同一データに対し複数回テンソル分解を実施できる
  #         source("~/Documents/Tensor/NTD/FSTD.R")
  #         source("~/Documents/Tensor/NTD/HOSVD.R")
  #         source("~/Documents/Tensor/NTD/MU-NTD.R")
  res <- lapply(1:1,function(i){
    if(method=="FSTD"){
            r <- MUNTD(X.arr,core_dims=rep(CORE_SIZE,3),lra_ranks=FSTD_NUM,method="FSTD")
    }
    if(method=="HOSVD"){
            r <- MUNTD(X.arr,core_dims=rep(CORE_SIZE,3),lra_ranks=rep(FSTD_NUM,3),method="HOSVD")
    }
    return(r)
  })
  return(res)
}



calcNTDWithError <- function(X.arr,CORE_SIZE,FSTD_NUM,method){
  # デバッグ用関数 いまは動かない
  e <- try({
          res <- calcNTD(X.arr,CORE_SIZE,FSTD_NUM,method=method)
  })
  # TODO: 到達不可能
  while(class(e)=="try-error"){
          e <- try({
          res <- calcNTD(X.arr,CORE_SIZE,FSTD_NUM,method=method)
          })
  }
  return(res)
}

raiseAlert <- function(res,NOW_NUM,threshold){ #ALERTOUTPUTも引数に入れる?
  # テンソル分解結果(res)から，アラートを出力する関数
  # NOW_NUM:分解したテンソルのデータ先頭時刻(numeric)
  IP_THRESHOULD <- 2
  EPS <- 2^-16

  res <- res[[1]] #余分なリストから出す
  feature.ip <- res[[2]][[2]]
  basis <- ncol(feature.ip)
  abip.g <- data.frame()
  if(threshold ==0){
    # Adaptive Thresholding
    LEVELS <- apply(feature.ip, 2, function(f){ otsu(f) })      # by Han
  }else{
    LEVELS <- rep(threshold,basis)
  }
  message("IP thresholds:")
  print(LEVELS)

  abIP <- data.frame(which(feature.ip >= LEVELS, arr.ind = T)) #不審IPの一覧
  abIP.num <- colSums(feature.ip >= LEVELS)
  abIP.num[which(abIP.num > 65000)] <- 0 #全部1なら0

  # abnormal group = indices of basis vectors
  abgroup.ip <- which(abIP.num >= IP_THRESHOULD)
  if (length(abgroup.ip) == 0) return(matrix(c(-1, -1, -1), 1, 3))

  # get abnormal port numbers
  feature.port <- res[[2]][[3]]
  if(threshold ==0){
    # Adaptive Thresholding
    LEVELS <- apply(feature.port, 2, function(f){ otsu(f) })        # by Han
  }else{
    LEVELS <- rep(threshold,basis)
  }
  message("Port thresholds:")
  print(LEVELS)
  # TODO: ポート特定アルゴリズムの改善
  # feature.portから値 LEVELS以上のポートを取ってくる
  abPortEachCol <- lapply(1:ncol(feature.port), function(col){
    abPort <- which(feature.port[, col] >= LEVELS[col]) - 1
    return(abPort)
  })
  abPortEachCol <- abPortEachCol[abgroup.ip] #abIPにかかるものだけを取る
  abPort <- unique(as.numeric(unlist(abPortEachCol)))
  if(length(abPort) > 65000) abPort <- -1

  abports <- lapply(abgroup.ip, function(id){
    # コアテンソルの特定IP特徴に関わる箇所
    core <- res[[1]][,id,]

    abIPThisFeatureID <- filter(abIP, col==id)[,1]
    abPort <- expand.grid(abPort=abPort,abIP=abIPThisFeatureID) #abIPとPortの組み合わせ

    return(abPort)
  })

  abports <- bind_rows(abports)
  abports <- summarise(group_by(abports,abPort,abIP))
  # abports <- distinct(bind_rows(abports)) #重複排除(同じIP,同じPortのものがもし存在すれば)

  times <- as.numeric(NOW_NUM)
  cat("ABNORMAL\n")
  cat("times:\n")
  print(times)
  abports <- as.data.frame(abports)
  cat("abports:\n")
  print(abports)
  abp <- cbind(times, abports)
  abp <- data.frame(abp)
  colnames(abp) <- c("times", "abPort", "abIP")

  abp <- filter(abp, abPort != -1)
  abp <- filter(abp, !is.na(abPort))

  # index2IP
  mother <- (abp$abIP-1)%/%256
  child <- (abp$abIP-1)%%256

  ip <- paste(mother,child,sep=".")
  abp$abIP <- ip
  return(abp)
}

floorTime <- function(Time,minimumTime){
  #時刻の切り下げ関数 12:34 -> 12:30のように切り良く揃える
  #Time;入力時刻 numeric,POSIXlt,POSIXct
  #minimumTime;切り揃える最小単位(分)
  if(0){
    Time <- as.POSIXct("2015-01-01 12:34:56",origin="1970-01-01")
    minimumTime <- 5
    floorTime(Time,minimumTime)

    Time <- as.POSIXlt("2015-01-01 12:42:56",origin="1970-01-01")
    minimumTime <- 20
    floorTime(Time,minimumTime)

    Time <- as.numeric(as.POSIXct("2015-01-01 12:34:56",origin="1970-01-01"))
    minimumTime <- 5
    floorTime(Time,minimumTime)
  }
  if(any(class(Time)=="numeric")){
    TimeLt <- as.POSIXlt(Time,origin="1970-01-01")
  }else if(any(class(Time)=="POSIXct")){
    TimeLt <- as.POSIXlt.POSIXct(Time)
  }else if(any(class(Time)=="POSIXlt")){
    TimeLt <- as.POSIXlt(as.numeric(Time),origin="1970-01-01")
  }else{
    stop(paste0("Unknown Class; class(Time)=",class(Time)))
  }

  if(any(class(TimeLt)=="POSIXlt")){
    TimeLt$sec <- 0 #秒は0
    TimeLt$min <- TimeLt$min %/% (minimumTime) * minimumTime
  }else{
    stop("Unknown Class of TimeLt")
  }
  return(as.POSIXct(TimeLt))
}


ceilingTime <- function(Time,minimumTime){
  #時刻の切り上げ関数 12:34 -> 12:35のように切り良く揃える
  #Time;入力時刻 numeric,POSIXlt,POSIXct
  #minimumTime;切り揃える最小単位(分)
  if(0){
    Time <- as.POSIXct("2015-01-01 12:34:56",origin="1970-01-01")
    minimumTime <- 5
    ceilingTime(Time,minimumTime)

    Time <- as.POSIXlt("2015-01-01 12:42:56",origin="1970-01-01")
    minimumTime <- 20
    ceilingTime(Time,minimumTime)

    Time <- as.numeric(as.POSIXct("2015-01-01 12:34:56",origin="1970-01-01"))
    minimumTime <- 15
    ceilingTime(Time,minimumTime)
  }
  if(any(class(Time)=="numeric")){
    TimeLt <- as.POSIXlt(Time,origin="1970-01-01",tz="UTC")
  }else if(any(class(Time)=="POSIXct")){
    TimeLt <- as.POSIXlt.POSIXct(Time,tz="UTC")
  }else if(any(class(Time)=="POSIXlt")){
    TimeLt <- as.POSIXlt(as.numeric(Time),origin="1970-01-01",tz="UTC")
  }else{
    stop(paste0("Unknown Class; class(Time)=",class(Time)))
  }

  if(any(class(TimeLt)=="POSIXlt")){
    TimeLt$sec <- 0 #秒は0
    TimeLt$min <- (TimeLt$min %/% minimumTime + 1) * minimumTime
  }else{
    stop("Unknown Class of TimeLt")
  }
  return(as.POSIXct(TimeLt))
}

#-その他解析用
getDataFromResult <- function(INPUT_FILE_MASTER,
                               timeUTC,SensorID,Protocol,Screen=0,windowsize=30*60){
  #後の解析用
  #時刻，センサ，プロトコル，スクリーン，(窓幅)を入力することで，該当時間のデータを再取得する
  if(0){
    INPUT_FILE_MASTER <- "XXX"
    timeUTC <- strptime("2015-12-19 01:30:00",format = "%Y-%m-%d %T",tz="UTC")
    SensorID <-"sensor061"
    Protocol <- "TCP"

  }
  stopifnot(Screen==0) #スクリーンありの場合の解析には現在非対応

  timeWindow <- seq(timeUTC,by = 60,length.out = windowsize/60)
  files <- convertPOSIXTimeToFilePath(timeWindow,sensorName = SensorID,
                                      INPUT_FILE_PATH = paste0(INPUT_FILE_MASTER,SensorID,"/"),FILENAME_EXTENTION = ".pcap")
  data <- loadPcapFile(files,IP_oct = 2,PROTOCOL = Protocol)
  return(data)
}

# Otsu Thresholding
# https://github.com/aoles/EBImage
otsu <- function(x, range = c(0, 1), levels = 256, eps=1e-10){
  if ( !is.numeric(range) || length(range) != 2 ) stop("'range' must be a numeric vector of length 2.")
  levels = as.integer(levels)
  if ( is.na(levels) || levels < 1 ) stop("Levels must be at least equal 1.")
  breaks = seq(range[1], range[2], length.out = levels+1)

  x = x[x > eps]
  h = hist.default(x, breaks = breaks, plot = FALSE)
  counts = as.double(h$counts)
  mids = as.double(h$mids)
  len = length(counts)
  w1 = cumsum(counts)
  w2 = w1[len] + counts - w1
  cm = counts * mids
  m1 = cumsum(cm)
  m2 = m1[len] + cm - m1
  var = w1 * w2 * (m2/w2 - m1/w1)^2
  maxi = which(var == max(var, na.rm = TRUE))
  (mids[maxi[1]] + mids[maxi[length(maxi)]] ) /2
}
