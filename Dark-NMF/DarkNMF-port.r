options(digits=5)   # 表示桁数
options(scipen=5)   # 大きな数値の指数表現を避ける
argv <- commandArgs(TRUE)				# 外部の引数を受け取る方法

# パラメータ指定
sensor <- as.character(argv[1])
user_home <- as.character(argv[2])
year <- as.numeric(argv[3])
month <- as.numeric(argv[4])
day <- as.numeric(argv[5])
hour <- as.numeric(argv[6])
minute <- as.numeric(argv[7])

# Make MatrixV
useOctet <- as.numeric(argv[8])          # select 2,3,4
MatrixVrow <- as.numeric(argv[9])         # データ行列Vの行数
dataRowTime <- as.numeric(argv[10])        # データ行列Vの1行あたりの分数
# 前処理
ver<-as.numeric(argv[11]) # チューニング用 バージョンを分けてディレクトリ管理
prepro<-as.numeric(argv[12]) # 前処理を行うかどうか
one_packet <- as.numeric(argv[13])              # 1パケットのみのホストの除外有無
PacketLimit_oneSample <-as.numeric(argv[14])     # パケット数の上限(1Sample) 有無
PacketLimit_oneTimeslot<-as.numeric(argv[15])     # パケット数の上限(1Timeslot) 有無

# NMF
basis_rstart <- as.numeric(argv[16])      # 基底数rの始まり
basis_rmax <- as.numeric(argv[17])    # 基底数rの最大値
Tmax <- as.numeric(argv[18])         # 乗法型更新アルゴリズムの反復上限

## 固定パラメータ (異常検知，アラートレベル)
pdf_flag<-0 # pdf出力
mdl_flag<-0 # mdl計算
message_flag<-0 # message出力

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
startdate<-paste0(year,"/",month,"/",day," ",hour,":",minute,":00")
startdate_UNIX <- as.numeric(as.POSIXct(startdate, origin="1970-1-1",tz="Japan"))

if(PacketLimit_oneSample == 1){
    if(useOctet == 4){
        NumofPacketLimit_Sample <- 30 * dataRowTime
    }else if(useOctet == 3){
        NumofPacketLimit_Sample <- 45 * dataRowTime
    }else if(useOctet == 2){
        NumofPacketLimit_Sample <- 60 * dataRowTime
    }
}
if(PacketLimit_oneTimeslot == 1){
    if(useOctet == 4){
        NumofPacketLimit_Timeslot <- 15 * dataRowTime * MatrixVrow
    }else if(useOctet == 3){
        NumofPacketLimit_Timeslot <- 20 * dataRowTime * MatrixVrow
    }else if(useOctet == 2){
        NumofPacketLimit_Timeslot <- 30 * dataRowTime * MatrixVrow
    }
}

################
# ディレクトリ/ファイル管理
filespace <- paste0(user_home, "sensor", sensor, "/", year,month, "/", year, month, day, "/", date, "/")
result_space <- paste0(filespace,"port_result_prepro", prepro, "_octet", useOctet, "_ver", ver, "/")
NMFcsv_space<-paste0(result_space, "NMF_csv/")
MDL_space<-paste0(result_space, "MDL/")
if(pdf_flag==1){
    NMFpdf_space<-paste0(result_space, "NMF_pdf/")
}
prepro_file <- paste0(result_space,"prepro_",date,".txt")
prepro_host_file <- paste0(filespace,"result_octet",useOctet,"_ver",ver,"/prepro_",date,".txt")
logfile <- paste0(result_space,date,"_NMF.log")



if(!file.exists(NMFcsv_space)){
    dir.create(NMFcsv_space,recursive=T)
    if(mdl_flag==1){
        dir.create(MDL_space)
    }
    if(pdf_flag==1){
        dir.create(NMFpdf_space)
    }
}

# パラメータ記録
parameter_name <- c("sensor <-", "user_home <-", "startdate <-", "startdate_UNIX <-", "useOctet <-", "MatrixVrow <-", "dataRowTime <-", "ver <-",  "prepro <-", "one_packet <-", "PacketLimit_oneSample <-", "PacketLimit_oneTimeslot <-", "basis_rstart <-", "basis_rmax <-","Tmax <-")
parameter_value <- c(sensor, user_home, startdate, startdate_UNIX, useOctet, MatrixVrow, dataRowTime, ver, prepro, one_packet, PacketLimit_oneSample, PacketLimit_oneTimeslot, basis_rstart, basis_rmax, Tmax)
parameter <- data.frame(NAME = parameter_name, VALUE = parameter_value)

write.table(parameter, logfile, quote=F,col.names=F,row.names=F,append=T)
try(rm(parameter_name, parameter_value, parameter), silent=TRUE)


#################
# MakeMatrixV
Start_MakeMatrixV_proctime <- round(proc.time()[3], digits = 3)

# hour minute second srcIP dstPort dstIP
if(file.exists(prepro_file)){
    prepro_data <- read.table(prepro_file)
    if(message_flag==1){
        message(paste0("prepro_data <- read.table(",prepro_file,")"))
    }
}else if(file.exists(prepro_host_file)){
    invisible(file.copy(prepro_host_file, prepro_file))
    prepro_data <- read.table(prepro_file)
    if(message_flag==1){
        message(paste0("prepro_data <- read.table(",prepro_host_file,")"))
    }
}else{
    if(message_flag==1){
        message(paste0("No prepro_data. Make prepro_data from inputfile"))
    }
    # 入力データ確認
    inputfile <- paste0(filespace,"input/",date,"_ver",ver,".txt")
    if(!file.exists(inputfile)){
        stop("No input data")
    }

    x <- read.table(inputfile,sep=":",fill=T)
    x<-x[1]
    x<-as.matrix(x)
    x<-t(matrix(unlist(strsplit(x," ")),nrow=5))

    datatime <- x[,1]
    datatime<-as.numeric(datatime)
    datatime <- datatime - startdate_UNIX
    hour_list <- as.integer(datatime%/%60%/%60)
    minute_list <- as.integer(datatime%/%60%%60)
    second_list <- as.integer(datatime%%60)

    source_host <- x[,3]
    source_host_matrix <- t(matrix(unlist(strsplit(as.character(source_host), "\\.")), nrow=5))
    temp_nrow=nrow(source_host_matrix)
    source_host_matrix <- matrix(as.integer(source_host_matrix), nrow=temp_nrow)

    destination_host <- x[,5]
    destination_host_matrix <- t(matrix(unlist(strsplit(as.character(destination_host), "\\.")), nrow=5))
    temp_nrow <- nrow(destination_host_matrix)
    destination_host_matrix <- matrix(as.integer(destination_host_matrix), nrow=temp_nrow)

    # scanner 除外

    prepro_data <- cbind(hour_list,minute_list,second_list,source_host_matrix[,1:4],destination_host_matrix[,5],destination_host_matrix[,3:4])
    try(rm(x, datatime, hour_list, minute_list, second_list, source_host, source_host_matrix, temp_nrow, destination_host, destination_host_matrix), silent=TRUE)


    # 前処理あり
    if(prepro == 1){

        # IP順にソートし，IPリスト作成
        w <- prepro_data
        if(useOctet == 4){
            w<-w[order(w[,4], w[,5], w[,6], w[,7]),]
            IP<-w[,as.numeric(c(4:7))]
            before_prepro_IPnum<-nrow(unique(IP))
            if(one_packet == 1){
                IP<-IP[duplicated(IP[,c(1:4)]),]  #only more than 2 packets host
            }
        }else if(useOctet == 3){
            w <- w[,-7]   #第4オクテットの情報を削除
            w<-w[order(w[,4], w[,5], w[,6]),]
            IP<-w[,as.numeric(c(4:6))]
            before_prepro_IPnum<-nrow(unique(IP))
            if(one_packet == 1){
                IP<-IP[duplicated(IP[,c(1:3)]),]  #only more than 2 packets host
            }
        }else if(useOctet == 2){
            w <- w[,-c(6,7)]
            w<-w[order(w[,4], w[,5]),]
            IP<-w[,as.numeric(c(4:5))]
            before_prepro_IPnum<-nrow(unique(IP))
            if(one_packet == 1){
                IP<-IP[duplicated(IP[,c(1:2)]),]  #only more than 2 packets host
            }
        }

        IP<-unique(IP)
        one_packet_hostnum<-before_prepro_IPnum-nrow(IP)

        write.table("----------------",logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
        write.table(paste0("#srcIPs (before prepro): ",before_prepro_IPnum),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
        write.table(paste0("#packets (before prepro): ",nrow(w)),logfile,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)


        # IPの数値化 (1.1.1.1 -> 001001001001)
        if(useOctet == 4){
            IPnumeric<-c(as.numeric(paste0(formatC(IP[,1], width=3, flag="0"),formatC(IP[,2], width=3, flag="0"),formatC(IP[,3], width=3, flag="0"),formatC(IP[,4], width=3, flag="0"))))
            allIPnumeric<-c(as.numeric(paste0(formatC(w[,4], width=3, flag="0"),formatC(w[,5], width=3, flag="0"),formatC(w[,6], width=3, flag="0"),formatC(w[,7], width=3, flag="0"))))
        }else if(useOctet == 3){
            IPnumeric<-c(as.numeric(paste0(formatC(IP[,1], width=3, flag="0"),formatC(IP[,2], width=3, flag="0"),formatC(IP[,3], width=3, flag="0"))))
            allIPnumeric<-c(as.numeric(paste0(formatC(w[,4], width=3, flag="0"),formatC(w[,5], width=3, flag="0"),formatC(w[,6], width=3, flag="0"))))
        }else if(useOctet == 2){
            IPnumeric<-c(as.numeric(paste0(formatC(IP[,1], width=3, flag="0"),formatC(IP[,2], width=3, flag="0"))))
            allIPnumeric<-c(as.numeric(paste0(formatC(w[,4], width=3, flag="0"),formatC(w[,5], width=3, flag="0"))))
        }

        # MatrixV  左からIPホストが小さい順にソートされている
        MatrixV<-matrix(0,MatrixVrow,nrow(IP))
        packetcount_MatrixV<-which(is.element(allIPnumeric,IPnumeric)==TRUE)
        for(i in 1:length(packetcount_MatrixV)){
                packetcountpoint<-which(is.element(IPnumeric,allIPnumeric[packetcount_MatrixV[i]])==TRUE)
                u<-(w[packetcount_MatrixV[i],1]*60+w[packetcount_MatrixV[i],2])%/%dataRowTime+1
                MatrixV[u,packetcountpoint]<-MatrixV[u,packetcountpoint]+1
        }
        try(rm(IPnumeric, allIPnumeric, packetcount_MatrixV, u), silent=TRUE)

        # 前処理
        if(PacketLimit_oneSample==1 || PacketLimit_oneTimeslot == 1){
            # パケット数の上限(1Sample)
            overPacketLimit_oneSample <- NULL
            if(PacketLimit_oneSample == 1){
                for(k in 1:MatrixVrow){
                    overPacketLimit_oneSample_tmp <- which(MatrixV[k,] > NumofPacketLimit_Sample)
                    overPacketLimit_oneSample <- append(overPacketLimit_oneSample,overPacketLimit_oneSample_tmp)
                }
                overPacketLimit_oneSample<-unique(sort(overPacketLimit_oneSample))
            }

            # パケット数の上限(1Timeslot)
            overPacketLimit_oneTimeslot <- NULL
            if(PacketLimit_oneTimeslot == 1){
                overPacketLimit_oneTimeslot<-which(apply(MatrixV, 2, sum) > NumofPacketLimit_Timeslot)
            }
            PacketLimite_Hosts<-unique(sort(c(overPacketLimit_oneSample,overPacketLimit_oneTimeslot)))

            MatrixV<-MatrixV[,-PacketLimite_Hosts]
            IP<-IP[-PacketLimite_Hosts,]

            write.table("",logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
            write.table(paste0("#1 Packet Hosts: ",one_packet_hostnum),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
            write.table(paste0("#Packet Limit Hosts: ", length(PacketLimite_Hosts), " (oneSample: ", length(overPacketLimit_oneSample), ", oneTimeslot: ",length(overPacketLimit_oneTimeslot),")"),logfile,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
            write.table("",logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
            write.table(paste0("#Hosts (after prepro): ",ncol(MatrixV)),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
            write.table(paste0("#Packets (after prepro): ", sum(MatrixV)),logfile,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
            try(rm(overPacketLimit_oneSample_tmp, overPacketLimit_oneSample, overPacketLimit_oneTimeslot, PacketLimite_Hosts), silent=TRUE)
        }

        # IPの数値化 (1.1.1.1 -> 001001001001)
        if(useOctet == 4){
            IPnumeric<-c(as.numeric(paste0(formatC(IP[,1], width=3, flag="0"),formatC(IP[,2], width=3, flag="0"),formatC(IP[,3], width=3, flag="0"),formatC(IP[,4], width=3, flag="0"))))
            allIPnumeric<-c(as.numeric(paste0(formatC(prepro_data[,4], width=3, flag="0"),formatC(prepro_data[,5], width=3, flag="0"),formatC(prepro_data[,6], width=3, flag="0"),formatC(prepro_data[,7], width=3, flag="0"))))
        }else if(useOctet == 3){
            IPnumeric<-c(as.numeric(paste0(formatC(IP[,1], width=3, flag="0"),formatC(IP[,2], width=3, flag="0"),formatC(IP[,3], width=3, flag="0"))))
            allIPnumeric<-c(as.numeric(paste0(formatC(prepro_data[,4], width=3, flag="0"),formatC(prepro_data[,5], width=3, flag="0"),formatC(prepro_data[,6], width=3, flag="0"))))
        }else if(useOctet == 2){
            IPnumeric<-c(as.numeric(paste0(formatC(IP[,1], width=3, flag="0"),formatC(IP[,2], width=3, flag="0"))))
            allIPnumeric<-c(as.numeric(paste0(formatC(prepro_data[,4], width=3, flag="0"),formatC(prepro_data[,5], width=3, flag="0"))))
        }
        prepro_data<-prepro_data[which(is.element(allIPnumeric,IPnumeric)==TRUE),]

        try(rm(IPnumeric, allIPnumeric,w, IP), silent=TRUE)

    }
}


# dstPort順にソートする
w <- prepro_data
w<-w[order(w[,8]),]
dstPort<-unique(w[,8])

write.table("----------------",logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
write.table(paste0("#dstPort: ",length(dstPort)),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
write.table(paste0("#packets: ",nrow(w)),logfile,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)

# MatrixV  左からdstPortが小さい順にソートされている
MatrixV<-matrix(0,MatrixVrow,length(dstPort))
# packetcount_MatrixV<-which(is.element(w[,8],dstPort)==TRUE)
for(i in 1:nrow(w)){
        packetcountpoint<-which(is.element(dstPort,w[,8][i])==TRUE)
        u<-(w[i,1]*60+w[i,2])%/%dataRowTime+1
        MatrixV[u,packetcountpoint]<-MatrixV[u,packetcountpoint]+1
}

enddate_UNIX<-startdate_UNIX + dataRowTime * (MatrixVrow-1) * 60
colnames(MatrixV)<-dstPort
rownames(MatrixV)<-seq(startdate_UNIX,enddate_UNIX,60*dataRowTime)
write.csv(MatrixV, paste0(NMFcsv_space,"MatrixV_",date,".csv"))

End_MakeMatrixV_proctime <- round(proc.time()[3], digits = 3)
write.table("----------------",logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
write.table(paste0("MakeMatrixV_Time: ",round(End_MakeMatrixV_proctime-Start_MakeMatrixV_proctime, digits = 3)," (sec)"),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)

write.table(prepro_data,prepro_file,quote=F,col.names=F,row.names=F,append=F)

# MakeMatrixV 終了
##################
# APIを用いてMatrixVを読み込むとき (前処理が別途必要かも!!!)
# MatrixV <- as.matrix(read.csv(paste0(NMFcsv_space,"MatrixV_",date,".csv"), check.names=F, row.names=1))



#################
# Modeling (NMF+MDL)
Start_Modeling_proctime <- round(proc.time()[3], digits = 3)

N_nrowV <- nrow(MatrixV)
M_ncolV <- ncol(MatrixV)
dataDL_list<-NULL
modelDL_list<-NULL
MDL_list<-NULL

if(basis_rmax > N_nrowV || basis_rmax > M_ncolV){
    basis_rmax <- min(N_nrowV,M_ncolV) - 1
}

# SVD 特異値分解
MatrixV_SVD<-svd(MatrixV,nu=N_nrowV,nv=M_ncolV,LINPACK=FALSE)
MatrixV_SVD_U<-abs(MatrixV_SVD$u)
MatrixV_SVD_V<-t(abs(MatrixV_SVD$v))
MatrixV_SVD_D<-MatrixV_SVD$d
# SVD 検証
# MatrixV==round(MatrixV_SVD$u %*% cbind(diag(MatrixV_SVD_D),matrix(0,30,290)) %*% t(MatrixV_SVD$v), digits = 3)

for(basis_r in basis_rstart:basis_rmax){
    Start_NMF_proctime <- round(proc.time()[3], digits = 3)
    # 初期化
    Fnorm<-0
    Fnorm_list<-NULL
    MatrixW<-matrix(MatrixV_SVD_U[1:N_nrowV,1:basis_r],N_nrowV,basis_r)
    MatrixH<-matrix(MatrixV_SVD_V[1:basis_r,1:M_ncolV],basis_r,M_ncolV)

    # NMF乗法型更新
    for(t in 1:Tmax){  # t: 更新回数
        # MatrixH更新
        WtV<-t(MatrixW)%*%MatrixV
        WtWH<-t(MatrixW)%*%MatrixW%*%MatrixH
        tempIndex <- which((WtWH==0|MatrixH==0), arr.ind=T)
        if(length(tempIndex)==0){
            MatrixH <- MatrixH*(WtV/WtWH)
        }else{
            tempIndex2 <- which((WtWH!=0&MatrixH!=0), arr.ind=T)
            MatrixH[tempIndex] <- 0
            MatrixH[tempIndex2] <- MatrixH[tempIndex2] * (WtV[tempIndex2]/WtWH[tempIndex2])
        }

        # MatrixW更新
        VHt<-MatrixV%*%t(MatrixH)
        WHHt<-MatrixW%*%MatrixH%*%t(MatrixH)
        tempIndex <- which((WHHt==0|MatrixW==0), arr.ind=T)
        if(length(tempIndex)==0){
            MatrixW <- MatrixW*(VHt/WHHt)
        }else{
            tempIndex2 <- which((WHHt!=0&MatrixW!=0), arr.ind=T)
            MatrixW[tempIndex] <- 0
            MatrixW[tempIndex2] <- MatrixW[tempIndex2] * (VHt[tempIndex2]/WHHt[tempIndex2])
        }

        # 終了判定 (フロベニウスノルムによる近似誤差チェック)

        preFnorm<-Fnorm
        MatrixWH<-MatrixW%*%MatrixH
        Fnorm <- sum((MatrixV-MatrixWH)^2) /(N_nrowV*M_ncolV)
        Fnorm_list <-append(Fnorm_list,abs(preFnorm-Fnorm))
#         write.table(paste0("#Iteration: ",t,", Error: ",Fnorm),paste0(NMFcsv_space,"Error_basisR_",basis_r,".log") ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)

        if(abs(preFnorm-Fnorm)<0.00001){
            break
        }
    }
    # Error Iteration graph
    if(pdf_flag==1){
        pdf(paste0(NMFpdf_space,"Error_BasisR",basis_r,".pdf"),onefile = T)
        plot(Fnorm_list,xlim=c(1,t*1.05),ylim=c(0,max(Fnorm_list)),xlab="Iteration", ylab="Error",type="l",lty=1,col="blue")
        invisible(dev.off())
    }

    # Matrix W, H 記録
    colnames(MatrixW) <- seq(1:basis_r)
    rownames(MatrixH) <- seq(1:basis_r)
    write.csv(MatrixW,paste0(NMFcsv_space,"MatrixW_BasisR",basis_r,".csv"))
    write.csv(MatrixH,paste0(NMFcsv_space,"MatrixH_BasisR",basis_r,".csv"))

    # Matrix W 正規化
    Lambda<-matrix(0,basis_r,basis_r)
    diag(Lambda)<-rowSums(MatrixH)
    Nomalized_MatrixW<-MatrixW%*%Lambda

    # Matrix W 記録
    colnames(Nomalized_MatrixW) <- seq(1:basis_r)
#     write.csv(Nomalized_MatrixW,paste0(NMFcsv_space,"Nomalized_MatrixW_BasisR",basis_r,".csv"))

    # NMF Nomalized MatrixW graph
    if(pdf_flag==1){
        maxNomalizedMatrixW<-max(apply(Nomalized_MatrixW,2,max))
        pdf(paste0(NMFpdf_space,"Nomalized_MatrixW_BasisR",basis_r,".pdf"), onefile=T)
        if(basis_r<=4){
            par(mfcol=c(2,2))
        }else if(basis_r<=9){
            par(mfcol=c(3,3))
        }else{
            par(mfcol=c(4,3))
        }
        for(i in 1:basis_r){
            plot(Nomalized_MatrixW[,i],xlim=c(1,N_nrowV),ylim=c(0,maxNomalizedMatrixW),xlab=paste0("Time(W), r",i),ylab="Packets",type="l",col="red")
        }
        invisible(dev.off())
    }
    if(mdl_flag==1){
        #MDL
        pi<-3.141592
        sigma <- sum((MatrixV-MatrixWH)^2)/(N_nrowV*M_ncolV)   # data variance
        dataDL <- -(-((N_nrowV*M_ncolV*log(2*pi*sigma))/2)-(sum((MatrixV-MatrixWH)^2)/(2*sigma)))  # data description length, Maximum log-likelihood
        modelDL <- ((N_nrowV*basis_r+basis_r*M_ncolV-basis_r^2)/2)*log((N_nrowV*M_ncolV)/(2*pi))  # model description length
        MDL<- dataDL+modelDL

        dataDL_list<-round(append(dataDL_list,dataDL), digits = 3)
        modelDL_list<-round(append(modelDL_list,modelDL), digits = 3)
        MDL_list<-round(append(MDL_list,MDL), digits = 3)
        write.table(paste0("Basis r=",basis_r, ", MDL: ",MDL,", dataDL: ",dataDL,", modelDL: ",modelDL),paste0(MDL_space,"MDL_value.txt") ,quote=F,col.names=F,row.names=F,append=T)

        End_NMF_proctime <- round(proc.time()[3], digits = 3)
        write.table(paste0("NMF_Time (Basis r=",basis_r, ", Iteration=",t,"): ",round(End_NMF_proctime-Start_NMF_proctime, digits = 3)," (sec)"),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)
    }
}

if(mdl_flag==1){
    minMDL_r<-which.min(MDL_list)
    write.table(paste0("Minimum MDL basis r: ",minMDL_r),paste0(MDL_space,"MDL_value.txt") ,quote=F,col.names=F,row.names=F,append=T)
    # invisible(file.create(paste0(MDL_space,minMDL_r)))

    # MDL graph
    if(pdf_flag==1){
        pdf(paste0(MDL_space,"MDL.pdf"),onefile=T)
        plot(MDL_list,xlim=c(basis_rstart,basis_rmax),ylim=c(min(dataDL_list,modelDL_list)*0.9,max(MDL_list)*1.05),xlab="Basis r",ylab="Description Length",type="o",pch=20,lty=1,col="black")
        par(new = T)
        plot(dataDL_list,xlim=c(basis_rstart,basis_rmax),ylim=c(min(dataDL_list,modelDL_list)*0.9,max(MDL_list)*1.05),xlab="",ylab="",type="o",pch=20,lty=1,col="blue")
        par(new = T)
        plot(modelDL_list,xlim=c(basis_rstart,basis_rmax),ylim=c(min(dataDL_list,modelDL_list)*0.9,max(MDL_list)*1.05),xlab="",ylab="",type="o",pch=20,lty=1,col="red")
        legend("topleft",legend = c("MDL (dataDL+modelDL)","dataDL","modelDL"),col=c("black","blue","red"),lty=1, bty = "n")
        invisible(dev.off())
    }

    End_Modeling_proctime <- round(proc.time()[3], digits = 3)
    write.table(paste0("Modeling(NMF+MDL)_Time: ",round(End_Modeling_proctime-Start_Modeling_proctime, digits = 3)," (sec)"),logfile ,quote=FALSE,append=T,col.names=FALSE,row.names=FALSE)

    if(pdf_flag==1){
        invisible(file.copy(paste0(NMFpdf_space,"Error_BasisR",minMDL_r,".pdf"), paste0(MDL_space,"Error_BasisR",minMDL_r,".pdf")))
        invisible(file.copy(paste0(NMFpdf_space,"Nomalized_MatrixW_BasisR",minMDL_r,".pdf"), paste0(MDL_space,"Nomalized_MatrixW_BasisR",minMDL_r,".pdf")))
    }
}
# Modeling (NMF+MDL) 終了
#################
