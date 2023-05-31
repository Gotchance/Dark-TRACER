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
useOctet <- as.numeric(argv[8])         # select 2,3,4
# 前処理
ver<-as.numeric(argv[9]) # チューニング用 バージョンを分けてディレクトリ管理
prepro<-as.numeric(argv[10]) # 前処理を行うかどうか

# NMF
basis_rstart <- as.numeric(argv[11])      # 基底数rの始まり
basis_rmax <- as.numeric(argv[12])    # 基底数rの最大値

# Anomaly detection
Choose_Portinfo_ActivedstPort <- as.numeric(argv[13]) # portinfoを異常アクティブホストだけで行うなら1，全アクティブホストで行うなら0
AnomalyActivedstPort_packet_threshold <- as.numeric(argv[14]) # ActivedstPortの異常ホスト判定パケット数閾値(最大パケット数のパーセンテージ)
AnomalyActivedstPort_threshold <- as.numeric(argv[15]) # ActivedstPortの異常グループ判定閾値
AnomalydstPorts_list <- as.character(argv[16])
memo_flag<-as.numeric(argv[17])

## 固定パラメータ (異常検知，アラートレベル)
ActivedstPort_threshold <- 1          # ActivedstPort判定パケット数閾値
pdf_flag<-0
mdl_flag<-0 # mdl計算
alert_srcIP_flag<-1 # alertのsrcIP関連csv,txt等のファイル出力


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
AnomalydstPorts<-sort(unique(as.numeric(unlist(strsplit(as.character(AnomalydstPorts_list) , ","), use.names=FALSE))))


################
# ディレクトリ/ファイル管理
filespace <- paste0(user_home, "sensor", sensor, "/", year,month, "/", year, month, day, "/", date, "/")
result_space <- paste0(filespace,"port_result_prepro", prepro, "_octet", useOctet, "_ver", ver, "/")
NMFcsv_space<-paste0(result_space, "NMF_csv/")
if(pdf_flag==1){
    NMFpdf_space<-paste0(result_space, "NMF_pdf/")
    if(!file.exists(NMFpdf_space)){
        dir.create(NMFpdf_space)
    }
}
alert_space<-paste0(result_space, "alert", Choose_Portinfo_ActivedstPort,AnomalyActivedstPort_packet_threshold,AnomalyActivedstPort_threshold,"/")
# portinfo_space<-paste0(alert_space, "portinfo/")
alertALL_space<-paste0(alert_space, "alert_ALL/")
alertALL_summary_space<-paste0(user_home,"Alert_DarkNMF/port_alert_", year, month, "_sensor", sensor,"_prepro", prepro, "_octet", useOctet,"_ver", ver, "_", Choose_Portinfo_ActivedstPort,AnomalyActivedstPort_packet_threshold,AnomalyActivedstPort_threshold, ".csv")
alert_summary_srcIP_space<-paste0(user_home,"Alert_DarkNMF/", year, month, "/", year, month, day, "/port/")
if(mdl_flag==1){
    MDL_space<-paste0(result_space, "MDL/")
    alertMDL_space<-paste0(alert_space, "alert_MDL/")
    alertMDL_summary_space<-paste0(user_home,"Alert_DarkNMF/port_alert_", year, month, "_sensor", sensor,"_MDL_prepro", prepro, "_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActivedstPort,AnomalyActivedstPort_packet_threshold,AnomalyActivedstPort_threshold, ".csv")
}

prepro_file <- paste0(result_space,"prepro_",date,".txt")
logfile <- paste0(result_space,date,"_NMF.log")

if(pdf_flag==1){
    if(!file.exists(alert_space)){
        dir.create(alert_space)
    #     dir.create(portinfo_space)
        dir.create(alertALL_space)
        if(mdl_flag==1){
            dir.create(alertMDL_space)
        }
    }
}
if(!file.exists(alert_summary_srcIP_space)){
    dir.create(alert_summary_srcIP_space,recursive = T)
}

if(mdl_flag==1){
    # minMDL_r 読み込み
    MDL_value<-scan(paste0(MDL_space,"MDL_value.txt"),what = character(), sep = "\n", blank.lines.skip = F, quiet = T)
    MDL_value<-MDL_value[grep("Minimum", MDL_value)]
    minMDL_r<-as.numeric(regmatches(MDL_value, regexpr("[0-9]+", MDL_value, perl=TRUE)))
}else{
    minMDL_r<-0
}

####
if(memo_flag==1){
    # MDL メモ
    if(mdl_flag==1){
        write.table(paste(date,minMDL_r,sep=","), paste0(user_home, "sensor", sensor, "/pot-MDL_sensor",sensor,".txt"),quote=F,col.names=F,row.names=F,append=T)
    }

    # iteration, time メモ
    logdata<-scan(logfile, what = character(), sep = "\n", blank.lines.skip = F, quiet = T)
    for(i in 1:10){
        write.table(logdata[grep("NMF_Time", logdata)][i], paste0(user_home, "sensor", sensor, "/port-sensor",sensor,"NMF_iteration",i,".txt"),quote=F,col.names=F,row.names=F,append=T)
    }
    write.table(logdata[grep("Modeling", logdata)], paste0(user_home, "sensor", sensor, "/port-sensor",sensor,"NMF_Total.txt"),quote=F,col.names=F,row.names=F,append=T)
}
####

prepro_data <- read.table(prepro_file)



#################
# portinfo 各basis_r
for(basis_r in basis_rstart:basis_rmax){
    MatrixH <- as.matrix(read.csv(paste0(NMFcsv_space,"MatrixH_BasisR",basis_r,".csv"), check.names=F, row.names=1))
    MatrixW <- as.matrix(read.csv(paste0(NMFcsv_space,"MatrixW_BasisR",basis_r,".csv"), check.names=F, row.names=1))

    # MatrixH 正規化
    Lambda_Inv<-matrix(0,basis_r,basis_r)
    diag(Lambda_Inv)<-colSums(MatrixW)
    Nomalized_MatrixH<-Lambda_Inv%*%MatrixH

    rownames(Nomalized_MatrixH) <- seq(1:basis_r)

    # NMF Nomalized MatrixH graph (ActivedstPort)
    if(pdf_flag==1){
        maxNomalizedMatrixH<-max(apply(Nomalized_MatrixH,2,max))
        pdf(paste0(NMFpdf_space,"Nomalized_MatrixH_BasisR",basis_r,"_",ActivedstPort_threshold,"_",AnomalyActivedstPort_packet_threshold,".pdf"), onefile=T)
        if(basis_r<=4){
            par(mfcol=c(2,2))
        }else if(basis_r<=9){
            par(mfcol=c(3,3))
        }else{
            par(mfcol=c(4,3))
        }
    }

    # MatrixHの1グループ別異常検知
    for(group in 1:basis_r){
        Nomalized_MatrixHcol<-Nomalized_MatrixH[group,]
        ActivedstPort_Nomalized_MatrixHcol<-Nomalized_MatrixHcol[which(Nomalized_MatrixHcol>=ActivedstPort_threshold)]
        AnomalyActivedstPort<-which(ActivedstPort_Nomalized_MatrixHcol>=max(ActivedstPort_Nomalized_MatrixHcol)*(AnomalyActivedstPort_packet_threshold/100))

        # NMF Nomalized MatrixH graph (ActivedstPort)
#         write.table(ActivedstPort_Nomalized_MatrixHcol,paste0(NMFcsv_space,"ActivedstPort_Nomalized_MatrixH_BasisR",basis_r,"_Group",group,"_",ActivedstPort_threshold,".csv"),quote=T, sep=",",col.names=F, row.names=T, append=F)
        if(pdf_flag==1){
            plot(Nomalized_MatrixH[group,],xlim=c(1,ncol(Nomalized_MatrixH)),ylim=c(0,maxNomalizedMatrixH),xlab=paste0("#ActivedstPort: ",length(ActivedstPort_Nomalized_MatrixHcol),", #AnomalydstPort: ", length(AnomalyActivedstPort)),ylab="packet",type="l",col="blue")
        }
    }
    if(pdf_flag==1){
        invisible(dev.off())
    }

    for(group in 1:basis_r){
        Nomalized_MatrixHcol<-Nomalized_MatrixH[group,]
        ActivedstPort_Nomalized_MatrixHcol<-Nomalized_MatrixHcol[which(Nomalized_MatrixHcol>=ActivedstPort_threshold)]
        AnomalyActivedstPort<-which(ActivedstPort_Nomalized_MatrixHcol>=max(ActivedstPort_Nomalized_MatrixHcol)*(AnomalyActivedstPort_packet_threshold/100))

        if(length(AnomalyActivedstPort)>=AnomalyActivedstPort_threshold){
            # 異常グループのportinfo
            # portinfoを異常アクティブホストだけで行うか，全アクティブホストで行うか選択
            if(Choose_Portinfo_ActivedstPort==1){
                ActivedstPort_list<-names(AnomalyActivedstPort)
            }else{
                ActivedstPort_list<-names(ActivedstPort_Nomalized_MatrixHcol)
            }

            if(useOctet == 4){
                ActivedstPort_data<-prepro_data[is.element(prepro_data[,8],names(AnomalyActivedstPort)),]
                unique_ActivedstPort_srcIPdstPort <- unique(ActivedstPort_data[,4:8])
                unique_ActivedstPort_srcIP <- unique(ActivedstPort_data[,4:7])
            }else if(useOctet == 3){
                ActivedstPort_data<-prepro_data[is.element(prepro_data[,8],names(AnomalyActivedstPort)),]
                unique_ActivedstPort_srcIPdstPort <- unique(ActivedstPort_data[,c(4,5,6,8)])
                unique_ActivedstPort_srcIP <- unique(ActivedstPort_data[,4:6])
            }else if(useOctet == 2){
                ActivedstPort_data<-prepro_data[is.element(prepro_data[,8],names(AnomalyActivedstPort)),]
                unique_ActivedstPort_srcIPdstPort <- unique(ActivedstPort_data[,c(4,5,8)])
                unique_ActivedstPort_srcIP <- unique(ActivedstPort_data[,4:5])
            }

            unique_ActivedstPort_dstPort_countpacket<-NULL
            unique_ActivedstPort_dstPort_countsrcIP<-NULL
            unique_ActivedstPort_dstPort <- sort(unique(ActivedstPort_data[,8]))

            # ホスト同期性異常検知でのポートと共通するポートがない場合 NEXT
            if(length(intersect(AnomalydstPorts, unique_ActivedstPort_dstPort)) == 0) next

            # unique ActivedstPort dstPort別のパケット数，srcIP数カウント
            for(k in 1:length(unique_ActivedstPort_dstPort)){
                unique_ActivedstPort_dstPort_countpacket[k] <- length(which(ActivedstPort_data[,8]==unique_ActivedstPort_dstPort[k]))
                if(useOctet == 4){
                    unique_ActivedstPort_dstPort_countsrcIP[k] <- length(which(unique_ActivedstPort_srcIPdstPort[,5]==unique_ActivedstPort_dstPort[k]))
                }else if(useOctet == 3){
                    unique_ActivedstPort_dstPort_countsrcIP[k] <- length(which(unique_ActivedstPort_srcIPdstPort[,4]==unique_ActivedstPort_dstPort[k]))
                }else if(useOctet == 2){
                    unique_ActivedstPort_dstPort_countsrcIP[k] <- length(which(unique_ActivedstPort_srcIPdstPort[,3]==unique_ActivedstPort_dstPort[k]))
                }
            }

            # unique ActivedstPort dstPort別のパケット数，srcIP数パーセンテージ
            unique_ActivedstPort_dstPort_percentpacket <- round(unique_ActivedstPort_dstPort_countpacket/sum(unique_ActivedstPort_dstPort_countpacket) * 100, digits = 2)
            unique_ActivedstPort_dstPort_percentsrcIP <- round(unique_ActivedstPort_dstPort_countsrcIP/nrow(unique_ActivedstPort_srcIP) * 100, digits = 2)

        #     packet_by_unique_ActivedstPort_dstPort <- cbind(unique_ActivedstPort_dstPort, unique_ActivedstPort_dstPort_countpacket, unique_ActivedstPort_dstPort_percentpacket)
            srcIP_by_unique_ActivedstPort_dstPort <- cbind(unique_ActivedstPort_dstPort, unique_ActivedstPort_dstPort_countsrcIP, unique_ActivedstPort_dstPort_percentsrcIP, unique_ActivedstPort_dstPort_countpacket, unique_ActivedstPort_dstPort_percentpacket)
            if(length(unique_ActivedstPort_dstPort)!=1){
                srcIP_by_unique_ActivedstPort_dstPort <- srcIP_by_unique_ActivedstPort_dstPort[order(-srcIP_by_unique_ActivedstPort_dstPort[,2]),]
        #         packet_by_unique_ActivedstPort_dstPort <- packet_by_unique_ActivedstPort_dstPort[order(-packet_by_unique_ActivedstPort_dstPort[,2]),]
            }
            rownames(srcIP_by_unique_ActivedstPort_dstPort)<-NULL
            colnames(srcIP_by_unique_ActivedstPort_dstPort)<-c("dstPort","#srcIP","%srcIP","#packet","%packet")
#             write.csv(srcIP_by_unique_ActivedstPort_dstPort, paste0(alertALL_space,"BasisR",basis_r,"_Group",group,"_PortinfoActivedstPort.csv"), row.names=F)

            num_unique_ActivedstPort_dstPort<-length(unique_ActivedstPort_dstPort)

            ###
            if(pdf_flag==1){
                # dstPort別パケット数，ユニークホスト数の時系列
                pdf(paste0(alertALL_space,"BasisR",basis_r,"_Group",group,"_dstPort_TimeSeries.pdf"), onefile=T)
                if(num_unique_ActivedstPort_dstPort<=4){
                    par(mfcol=c(2,2))
                }else if(num_unique_ActivedstPort_dstPort<=9){
                    par(mfcol=c(3,3))
                }else{
    #                 mfcol_row<-(num_unique_ActivedstPort_dstPort+2)%/%3
    #                 par(mfcol=c(mfcol_row,3))
                    par(mfcol=c(4,3))
                }

                for(k in 1:num_unique_ActivedstPort_dstPort){
                    # packet
                    ActivedstPort_data_dstPort<-ActivedstPort_data[ActivedstPort_data[,8]==unique_ActivedstPort_dstPort[k],]
                    sumActivedstPort_data_dstPort<-length(ActivedstPort_data_dstPort[,2])

                    # uniq host
                    if(useOctet == 4){
                        ActivedstPort_data_dstPort_IPv4<-paste(ActivedstPort_data_dstPort[,4],ActivedstPort_data_dstPort[,5],ActivedstPort_data_dstPort[,6],ActivedstPort_data_dstPort[,7],sep=".")
                    }else if(useOctet == 3){
                        ActivedstPort_data_dstPort_IPv4<-paste(ActivedstPort_data_dstPort[,4],ActivedstPort_data_dstPort[,5],ActivedstPort_data_dstPort[,6],sep=".")
                    }else if(useOctet == 2){
                        ActivedstPort_data_dstPort_IPv4<-paste(ActivedstPort_data_dstPort[,4],ActivedstPort_data_dstPort[,5],sep=".")
                    }
                    table_uniqHost_ActivedstPort_data_dstPort<-table(unique(cbind(ActivedstPort_data_dstPort[,2], ActivedstPort_data_dstPort_IPv4))[,1])
                    sumtable_uniqHost_ActivedstPort_data_dstPort<-length(unique(cbind(ActivedstPort_data_dstPort[,2], ActivedstPort_data_dstPort_IPv4)[,2]))

                    # Figure 1: packet
                    plot(table(ActivedstPort_data_dstPort[,2]), xlim=c(0,30), ylim=c(0,max(table(ActivedstPort_data_dstPort[,2]))*1.2),
                         type="p",pch=1,col=rgb(1,0,1, alpha=0.5), lty=2, cex=1,xaxt="n",
                         xlab=paste0("Time, dstPort:",unique_ActivedstPort_dstPort[k]), ylab="")
                    axis(1)
                    axis(2)
                    mtext(2, text = paste0("Packets (Sum: ",sumActivedstPort_data_dstPort,")"), line = 1.8, col="magenta", cex=0.7)

                    par(new=T)
                    plot(table(ActivedstPort_data_dstPort[,2]), xlim=c(0,30), ylim=c(0,max(table(ActivedstPort_data_dstPort[,2]))*1.2),
                         type="h",col=rgb(1,0,1, alpha=0.5), lty=2,lwd=2,xlab="",ylab="",axes=FALSE, xaxt="n")

                    # Figure 2: Uniq Host
                    par(new=T)
                    plot(table_uniqHost_ActivedstPort_data_dstPort, xlim=c(0,30), ylim=c(0,max(table_uniqHost_ActivedstPort_data_dstPort)*1.2),
                         type="p",pch=4,col=rgb(0,156/256,209/256,alpha=0.5),lty=3, cex=1,xlab="", ylab="",axes=FALSE,xaxt="n")
                    mtext(text=paste0("UniqHosts (Sum: ",sumtable_uniqHost_ActivedstPort_data_dstPort,")"),side = 4, line = 1.8, col="cyan", cex=0.7)
                    axis(4)

                    par(new=T)
                    plot(table_uniqHost_ActivedstPort_data_dstPort, xlim=c(0,30), ylim=c(0,max(table_uniqHost_ActivedstPort_data_dstPort)*1.2),
                         type="h",col=rgb(0,156/256,209/256,alpha=0.5),lty=3, lwd=2, xlab="", ylab="",axes=FALSE,xaxt="n")
                }
                invisible(dev.off())
                if(basis_r==minMDL_r && mdl_flag==1){
#                     invisible(file.copy(paste0(alertALL_space,"BasisR",basis_r,"_Group",group,"_PortinfoActivedstPort.csv"), paste0(alertMDL_space,"BasisR",basis_r,"_Group",group,"_PortinfoActivedstPort.csv")))
                    invisible(file.copy(paste0(alertALL_space,"BasisR",basis_r,"_Group",group,"_dstPort_TimeSeries.pdf"), paste0(alertMDL_space,"BasisR",basis_r,"_Group",group,"_dstPort_TimeSeries.pdf")))
                }
            }
            ###

            ###
            # alert_info
            AnomalPorts<-Reduce(function(x,y){paste(x,y,sep=",")},unique_ActivedstPort_dstPort)
#             AnomalPorts<-paste0("[",AnomalPorts,"]")

            if(useOctet == 4){
                ActivedstPort_data_IPv4<-paste(ActivedstPort_data[,4],ActivedstPort_data[,5],ActivedstPort_data[,6],ActivedstPort_data[,7],sep=".")
                unique_ActivedstPort_srcIPdstPort<-unique_ActivedstPort_srcIPdstPort[order(unique_ActivedstPort_srcIPdstPort[,1], unique_ActivedstPort_srcIPdstPort[,2], unique_ActivedstPort_srcIPdstPort[,3], unique_ActivedstPort_srcIPdstPort[,4]),]
                unique_ActivedstPort_srcIPdstPort_IPv4<-paste(unique_ActivedstPort_srcIPdstPort[,1],unique_ActivedstPort_srcIPdstPort[,2],unique_ActivedstPort_srcIPdstPort[,3],unique_ActivedstPort_srcIPdstPort[,4],sep=".")
            }else if(useOctet == 3){
                ActivedstPort_data_IPv4<-paste(ActivedstPort_data[,4],ActivedstPort_data[,5],ActivedstPort_data[,6],sep=".")
                unique_ActivedstPort_srcIPdstPort<-unique_ActivedstPort_srcIPdstPort[order(unique_ActivedstPort_srcIPdstPort[,1], unique_ActivedstPort_srcIPdstPort[,2],unique_ActivedstPort_srcIPdstPort[,3]),]
                unique_ActivedstPort_srcIPdstPort_IPv4<-paste(unique_ActivedstPort_srcIPdstPort[,1],unique_ActivedstPort_srcIPdstPort[,2],unique_ActivedstPort_srcIPdstPort[,3],sep=".")
            }else if(useOctet == 2){
                ActivedstPort_data_IPv4<-paste(ActivedstPort_data[,4],ActivedstPort_data[,5],sep=".")
                unique_ActivedstPort_srcIPdstPort<-unique_ActivedstPort_srcIPdstPort[order(unique_ActivedstPort_srcIPdstPort[,1], unique_ActivedstPort_srcIPdstPort[,2]),]
                unique_ActivedstPort_srcIPdstPort_IPv4<-paste(unique_ActivedstPort_srcIPdstPort[,1],unique_ActivedstPort_srcIPdstPort[,2],sep=".")
            }
            unique_ActivedstPort_data_IPv4<-unique(ActivedstPort_data_IPv4)
            table_unique_ActivedstPort_srcIPdstPort_IPv4<-table(unique_ActivedstPort_srcIPdstPort_IPv4)
            intersection_unique_ActivedstPort_srcIPdstPort_IPv4<-names(table_unique_ActivedstPort_srcIPdstPort_IPv4[which(table_unique_ActivedstPort_srcIPdstPort_IPv4 == num_unique_ActivedstPort_dstPort)])
            largest_table_unique_ActivedstPort_srcIPdstPort_IPv4<-round(sort(table_unique_ActivedstPort_srcIPdstPort_IPv4, decreasing = T)[1]/num_unique_ActivedstPort_dstPort*100, digits = 2)

            alert_info<-data.frame(basis_r,group,AnomalPorts,length(unique_ActivedstPort_data_IPv4), length(intersection_unique_ActivedstPort_srcIPdstPort_IPv4),largest_table_unique_ActivedstPort_srcIPdstPort_IPv4)
            rownames(alert_info)<-NULL
            colnames(alert_info)<-c("BasisR","Group","Anomal dstPorts","#srcIP(Union)","#srcIP(Inter)","Max%srcIP(Inter)")
#             write.table(alert_info, paste0(alertALL_space,"BasisR",basis_r,"_Group",group,"_alert.csv"),quote=T,sep=",",col.names=F,row.names=F,append=F)


            # srcIPs
            data_frame_unique_ActivedstPort_srcIPdstPort_IPv4<-as.data.frame(table_unique_ActivedstPort_srcIPdstPort_IPv4)
            data_frame_unique_ActivedstPort_srcIPdstPort_IPv4<-data_frame_unique_ActivedstPort_srcIPdstPort_IPv4[order(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4[,2], decreasing = T),]
            data_frame_unique_ActivedstPort_srcIPdstPort_IPv4[,2]<-round(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4[,2]/num_unique_ActivedstPort_dstPort*100, digits = 2)
            data_frame_unique_ActivedstPort_srcIPdstPort_IPv4[,1]<-as.character(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4[,1])
            rownames(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4)<-NULL
            colnames(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4)<-c("srcIPs","%srcIP(Inter)")
#             write.table(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4, paste0(alertALL_space,"BasisR",basis_r,"_Group",group,"_srcIP.csv"),quote=T,sep=",",col.names=F,row.names=F,append=F)

            # alert summary
            ##############
            # unique_ActivedstPort_data_IPv4: アクティブポートに送信したsrcIPたち
            # intersection_unique_ActivedstPort_srcIPdstPort_IPv4: アクティブポート全てに送信したsrcIPたち (Intersection)
            # largest_table_unique_ActivedstPort_srcIPdstPort_IPv4: アクティブポートに一番多く送信したsrcIPが，何個のアクティブポートに送信したか割合．(Intersectionが1個でもあれば100%となる)
            ##############
            alert_info_summary<-data.frame(startdate,AnomalPorts,AnomalydstPorts_list,length(unique_ActivedstPort_data_IPv4),length(intersection_unique_ActivedstPort_srcIPdstPort_IPv4),largest_table_unique_ActivedstPort_srcIPdstPort_IPv4,basis_r,group,alert_space)
            rownames(alert_info_summary)<-NULL
            colnames(alert_info_summary)<-c("Date","Anomal dstPorts","Anomal dstPorts from HostAlert","#srcIP(Union)","#srcIP(Inter)","Max%srcIP(Inter)","BasisR","Group","Filespace")
            write.table(alert_info_summary, alertALL_summary_space,quote=T,sep=",",col.names=F,row.names=F,append=T)

            if(alert_srcIP_flag==1){
                write.table(intersection_unique_ActivedstPort_srcIPdstPort_IPv4, paste0(alert_summary_srcIP_space, date, "_", AnomalPorts, "_sensor", sensor,"_prepro", prepro, "_octet", useOctet,"_ver", ver, "_", Choose_Portinfo_ActivedstPort, AnomalyActivedstPort_packet_threshold, AnomalyActivedstPort_threshold, "_BasisR", basis_r, "_Group", group, "_Intersection_srcIP.txt"), quote=T,col.names=F,row.names=F,append=F) # 積集合
                write.table(unique_ActivedstPort_data_IPv4, paste0(alert_summary_srcIP_space, date, "_", AnomalPorts, "_sensor", sensor,"_prepro", prepro, "_octet", useOctet,"_ver", ver, "_", Choose_Portinfo_ActivedstPort,AnomalyActivedstPort_packet_threshold,AnomalyActivedstPort_threshold, "_BasisR", basis_r, "_Group", group, "_Union_srcIP.txt"), quote=T,col.names=F,row.names=F,append=F) # 和集合
            }

            # minMDL_r
            if(basis_r==minMDL_r && mdl_flag==1){
#                 write.table(alert_info, paste0(alertMDL_space,"BasisR",basis_r,"_Group",group,"_alert.csv"),quote=T,sep=",",col.names=F,row.names=F,append=F)
#                 write.table(data_frame_unique_ActivedstPort_srcIPdstPort_IPv4, paste0(alertMDL_space,"BasisR",basis_r,"_Group",group,"_srcIP.csv"),quote=T,sep=",",col.names=F,row.names=F,append=F)
#                 write.csv(srcIP_by_unique_ActivedstPort_dstPort, paste0(alertMDL_space,"BasisR",basis_r,"_Group",group,"_PortinfoActivedstPort.csv"), row.names=F)

                # alert summary MDL
                write.table(alert_info_summary, alertMDL_summary_space,quote=T,sep=",",col.names=F,row.names=F,append=T)
                if(alert_srcIP_flag==1){
                    write.table(intersection_unique_ActivedstPort_srcIPdstPort_IPv4, paste0(alert_summary_srcIP_space, date, "_", AnomalPorts, "_sensor", sensor,"_prepro", prepro, "_octet", useOctet,"_ver", ver, "_", Choose_Portinfo_ActivedstPort,AnomalyActivedstPort_packet_threshold,AnomalyActivedstPort_threshold, "_BasisR", basis_r, "_Group", group, "_Intersection_srcIP_MDL.txt"), quote=T,col.names=F,row.names=F,append=F) # 積集合
                    write.table(unique_ActivedstPort_data_IPv4, paste0(alert_summary_srcIP_space, date, "_", AnomalPorts, "_sensor", sensor,"_prepro", prepro, "_octet", useOctet,"_ver", ver, "_", Choose_Portinfo_ActivedstPort,AnomalyActivedstPort_packet_threshold,AnomalyActivedstPort_threshold, "_BasisR", basis_r, "_Group", group, "_Union_srcIP_MDL.txt"), quote=T,col.names=F,row.names=F,append=F) # 和集合
                }
            }

            ###
        }

    }
}
if(pdf_flag==1 && mdl_flag==1){
    invisible(file.copy(paste0(NMFpdf_space,"Nomalized_MatrixH_BasisR",minMDL_r,"_",ActivedstPort_threshold,"_",AnomalyActivedstPort_packet_threshold,".pdf"), paste0(MDL_space,"Nomalized_MatrixH_BasisR",minMDL_r,"_",ActivedstPort_threshold,"_",AnomalyActivedstPort_packet_threshold,".pdf")))
}
#################
