options(digits=5)   # 表示桁数
options(scipen=5)   # 大きな数値の指数表現を避ける
argv <- commandArgs(TRUE)				# 外部の引数を受け取る方法

#################
# alertだけのスクリプト
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
ver<-as.numeric(argv[9]) # チューニング用 バージョンを分けてディレクトリ管理

# NMF
basis_rstart <- as.numeric(argv[10])      # 基底数rの始まり
basis_rmax <- as.numeric(argv[11])    # 基底数rの最大値

# Anomaly detection
Choose_Portinfo_ActiveHost <- as.numeric(argv[12]) # portinfoを異常アクティブホストだけで行うなら1，全アクティブホストで行うなら0
AnomalyActiveHost_packet_threshold <- as.numeric(argv[13]) # ActiveHostの異常ホスト判定パケット数閾値(最大パケット数のパーセンテージ)
AnomalyActiveHost_threshold <- as.numeric(argv[14]) # ActiveHostの異常グループ判定閾値
alert_percent_upper_bound<-as.numeric(argv[15]) # アラート判定閾値(%srcIP)
alert_num_upper_bound<-as.numeric(argv[16]) # アラート判定閾値(#srcIP)
memo_flag<-as.numeric(argv[17])

## 固定パラメータ (異常検知，アラートレベル)
ActiveHost_threshold <- 1          # ActiveHost判定パケット数閾値
dst_percent_upper_bound<-70 # アラート判定閾値(一点集中型)
pdf_flag<-0 # pdf出力
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


################
# ディレクトリ/ファイル管理
filespace <- paste0(user_home, "sensor", sensor, "/", year,month, "/", year, month, day, "/", date, "/")
result_space <- paste0(filespace,"result_octet", useOctet, "_ver", ver, "/")
NMFcsv_space<-paste0(result_space, "NMF_csv/")
if(pdf_flag==1){
    NMFpdf_space<-paste0(result_space, "NMF_pdf/")
    if(!file.exists(NMFpdf_space)){
        dir.create(NMFpdf_space)
    }
}
alert_space<-paste0(result_space, "alert", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound,alert_num_upper_bound,"/")
# portinfo_space<-paste0(alert_space, "portinfo/")
alertALL_space<-paste0(alert_space, "alert_ALL/")
alertALL_summary_space<-paste0(user_home,"Alert_DarkNMF/alert_", year, month, "_sensor", sensor,"_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound,alert_num_upper_bound, ".csv")
alert_summary_srcIP_space<-paste0(user_home,"Alert_DarkNMF/", year, month, "/", year, month, day, "/")
if(mdl_flag==1){
    MDL_space<-paste0(result_space, "MDL/")
    alertMDL_space<-paste0(alert_space, "alert_MDL/")
    alertMDL_summary_space<-paste0(user_home,"Alert_DarkNMF/alert_", year, month, "_sensor", sensor,"_MDL_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound,alert_num_upper_bound, ".csv")
}

prepro_file <- paste0(result_space,"prepro_",date,".txt")
logfile <- paste0(result_space,date,"_NMF.log")

if(!file.exists(alert_space)){
    dir.create(alert_space,recursive=T)
    dir.create(alertALL_space)
    if(mdl_flag==1){
        dir.create(alertMDL_space)
    }
#     dir.create(portinfo_space)
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
        write.table(paste(date,minMDL_r,sep=","), paste0(user_home, "sensor", sensor, "/MDL_sensor",sensor,".txt"),quote=F,col.names=F,row.names=F,append=T)
    }

    # iteration, time メモ
    logdata<-scan(logfile, what = character(), sep = "\n", blank.lines.skip = F, quiet = T)
    for(i in 1:10){
        write.table(logdata[grep("NMF_Time", logdata)][i], paste0(user_home, "sensor", sensor, "/sensor",sensor,"NMF_iteration",i,".txt"), quote=F,col.names=F,row.names=F,append=T)
    }
    write.table(logdata[grep("Modeling", logdata)], paste0(user_home, "sensor", sensor, "/sensor",sensor,"NMF_Total.txt"), quote=F,col.names=F,row.names=F,append=T)
}
####

prepro_data <- read.table(prepro_file)

# IPの数値化 (1.1.1.1 -> 001001001001), srcIPdstPortのunique, srcIPのunique
if(useOctet == 4){
    allIPnumeric<-c(as.numeric(paste0(formatC(prepro_data[,4], width=3, flag="0"),formatC(prepro_data[,5], width=3, flag="0"),formatC(prepro_data[,6], width=3, flag="0"),formatC(prepro_data[,7], width=3, flag="0"))))
}else if(useOctet == 3){
    allIPnumeric<-c(as.numeric(paste0(formatC(prepro_data[,4], width=3, flag="0"),formatC(prepro_data[,5], width=3, flag="0"),formatC(prepro_data[,6], width=3, flag="0"))))
}else if(useOctet == 2){
    allIPnumeric<-c(as.numeric(paste0(formatC(prepro_data[,4], width=3, flag="0"),formatC(prepro_data[,5], width=3, flag="0"))))
}

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

    # NMF Nomalized MatrixH graph (ActiveHost)
    if(pdf_flag==1){
        maxNomalizedMatrixH<-max(apply(Nomalized_MatrixH,2,max))
        pdf(paste0(NMFpdf_space,"Nomalized_MatrixH_BasisR",basis_r,"_",ActiveHost_threshold,"_",AnomalyActiveHost_packet_threshold,".pdf"), onefile=T)
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
        ActiveHost_Nomalized_MatrixHcol<-Nomalized_MatrixHcol[which(Nomalized_MatrixHcol>=ActiveHost_threshold)]
        AnomalyActiveHost<-which(ActiveHost_Nomalized_MatrixHcol>=max(ActiveHost_Nomalized_MatrixHcol)*(AnomalyActiveHost_packet_threshold/100))

        # NMF Nomalized MatrixH graph (ActiveHost)
        if(pdf_flag==1){
            write.table(ActiveHost_Nomalized_MatrixHcol,paste0(NMFcsv_space,"ActiveHost_Nomalized_MatrixH_BasisR",basis_r,"_Group",group,"_",ActiveHost_threshold,".csv"),quote=T, sep=",",col.names=F, row.names=T, append=F)
            plot(Nomalized_MatrixH[group,],xlim=c(1,ncol(Nomalized_MatrixH)),ylim=c(0,maxNomalizedMatrixH),xlab=paste0("#ActiveHost: ",length(ActiveHost_Nomalized_MatrixHcol),", #AnomalyHost: ", length(AnomalyActiveHost)),ylab="packet",type="l",col="blue")
        }

        # Anomalyなグループ検知
        if(length(AnomalyActiveHost)>=AnomalyActiveHost_threshold){
            # 異常グループのportinfo
            # portinfoを異常アクティブホストだけで行うか，全アクティブホストで行うか選択
            if(Choose_Portinfo_ActiveHost==1){
                ActiveHost_list<-data.frame(strsplit(names(AnomalyActiveHost),"\\."))
            }else{
                ActiveHost_list<-data.frame(strsplit(names(ActiveHost_Nomalized_MatrixHcol),"\\."))
            }

            if(useOctet == 4){
                ActiveHost_list<-t(matrix(as.numeric(as.matrix(ActiveHost_list)), 4))
                names(ActiveHost_list) <- NULL
                ActiveHost_listnumeric<-c(as.numeric(paste0(formatC(ActiveHost_list[,1], width=3, flag="0"),formatC(ActiveHost_list[,2], width=3, flag="0"),formatC(ActiveHost_list[,3], width=3, flag="0"),formatC(ActiveHost_list[,4], width=3, flag="0"))))
                ActiveHost_data <- prepro_data[is.element(allIPnumeric, ActiveHost_listnumeric),]
                unique_ActiveHost_srcIPdstPort <- unique(ActiveHost_data[,4:8])
            }else if(useOctet == 3){
                ActiveHost_list<-t(matrix(as.numeric(as.matrix(ActiveHost_list)), 3))
                names(ActiveHost_list) <- NULL
                ActiveHost_listnumeric<-c(as.numeric(paste0(formatC(ActiveHost_list[,1], width=3, flag="0"),formatC(ActiveHost_list[,2], width=3, flag="0"),formatC(ActiveHost_list[,3], width=3, flag="0"))))
                ActiveHost_data <- prepro_data[is.element(allIPnumeric, ActiveHost_listnumeric),]
                unique_ActiveHost_srcIPdstPort <- unique(ActiveHost_data[,c(4,5,6,8)])
            }else if(useOctet == 2){
                ActiveHost_list<-t(matrix(as.numeric(as.matrix(ActiveHost_list)), 2))
                names(ActiveHost_list) <- NULL
                ActiveHost_listnumeric<-c(as.numeric(paste0(formatC(ActiveHost_list[,1], width=3, flag="0"),formatC(ActiveHost_list[,2], width=3, flag="0"))))
                ActiveHost_data <- prepro_data[is.element(allIPnumeric, ActiveHost_listnumeric),]
                unique_ActiveHost_srcIPdstPort <- unique(ActiveHost_data[,c(4,5,8)])
            }

            unique_ActiveHost_dstPort_countpacket<-NULL
            unique_ActiveHost_dstPort_countsrcIP<-NULL
            unique_ActiveHost_dstPort <- sort(unique(ActiveHost_data[,8]))

            # unique ActiveHost dstPort別のパケット数，srcIP数カウント
            for(k in 1:length(unique_ActiveHost_dstPort)){
                unique_ActiveHost_dstPort_countpacket[k] <- length(which(ActiveHost_data[,8]==unique_ActiveHost_dstPort[k]))
                if(useOctet == 4){
                    unique_ActiveHost_dstPort_countsrcIP[k] <- length(which(unique_ActiveHost_srcIPdstPort[,5]==unique_ActiveHost_dstPort[k]))
                }else if(useOctet == 3){
                    unique_ActiveHost_dstPort_countsrcIP[k] <- length(which(unique_ActiveHost_srcIPdstPort[,4]==unique_ActiveHost_dstPort[k]))
                }else if(useOctet == 2){
                    unique_ActiveHost_dstPort_countsrcIP[k] <- length(which(unique_ActiveHost_srcIPdstPort[,3]==unique_ActiveHost_dstPort[k]))
                }
            }

            # unique ActiveHost dstPort別のパケット数，srcIP数パーセンテージ
            unique_ActiveHost_dstPort_percentpacket <- round(unique_ActiveHost_dstPort_countpacket/sum(unique_ActiveHost_dstPort_countpacket) * 100, digits = 2)
            unique_ActiveHost_dstPort_percentsrcIP <- round(unique_ActiveHost_dstPort_countsrcIP/nrow(ActiveHost_list) * 100, digits = 2)

#             packet_by_unique_ActiveHost_dstPort <- cbind(unique_ActiveHost_dstPort, unique_ActiveHost_dstPort_countpacket, unique_ActiveHost_dstPort_percentpacket)
            srcIP_by_unique_ActiveHost_dstPort <- cbind(unique_ActiveHost_dstPort, unique_ActiveHost_dstPort_countsrcIP, unique_ActiveHost_dstPort_percentsrcIP)
            if(length(unique_ActiveHost_dstPort)!=1){
#                 packet_by_unique_ActiveHost_dstPort <- packet_by_unique_ActiveHost_dstPort[order(-packet_by_unique_ActiveHost_dstPort[,2]),]
                srcIP_by_unique_ActiveHost_dstPort <- srcIP_by_unique_ActiveHost_dstPort[order(-srcIP_by_unique_ActiveHost_dstPort[,2]),]
            }

#             write.csv(packet_by_unique_ActiveHost_dstPort, paste0(portinfo_space,"PortinfoActiveHost_packet_BasisR",basis_r,"_Group",group,".csv"), row.names=F)
#             write.csv(srcIP_by_unique_ActiveHost_dstPort, paste0(portinfo_space,"PortinfoActiveHost_srcIP_BasisR",basis_r,"_Group",group,".csv"), row.names=F)

            # Alert Level
            count_alert_upper <- 0	# アラートとされた1つ時間帯の中にアラートレベルを判定し，アラート対象になるポートの数．必ず1つのポートはアラートとして上げる
            alert_level <- NULL			# 1:attack 2:survey measurement 3:weak survey measurement
            repeat{
                count_alert_upper <- count_alert_upper + 1
                if(nrow(srcIP_by_unique_ActiveHost_dstPort) < count_alert_upper){
                    count_alert_upper <- count_alert_upper - 1
                    break
                }

                if(srcIP_by_unique_ActiveHost_dstPort[count_alert_upper,3] >= alert_percent_upper_bound && srcIP_by_unique_ActiveHost_dstPort[count_alert_upper,2] >= alert_num_upper_bound){
                    alert_level <- c(alert_level, 1)
#                 }else if(srcIP_by_unique_ActiveHost_dstPort[count_alert_upper,3] >= alert_percent_upper_bound && srcIP_by_unique_ActiveHost_dstPort[count_alert_upper,2] < alert_num_upper_bound){
#                     alert_level <- c(alert_level, 2)
#                 }else if(srcIP_by_unique_ActiveHost_dstPort[count_alert_upper,2] >= alert_num_upper_bound){
#                     alert_level <- c(alert_level, 3)
#                     if(srcIP_by_unique_ActiveHost_dstPort[count_alert_upper,2] != srcIP_by_unique_ActiveHost_dstPort[count_alert_upper+1,2]){
#                         count_alert_upper <- count_alert_upper - 1
#                         break
#                     }
                }else{
                    count_alert_upper <- count_alert_upper - 1
                    break
                }
            }

            # Alert情報収集
            for(p in 1:count_alert_upper){
                if(count_alert_upper==0){
                    break
                }
                Anomaly_dstPort <- srcIP_by_unique_ActiveHost_dstPort[p,1]

                if(useOctet == 4){
                    dstPort_srcdstIP<-ActiveHost_data[which(ActiveHost_data[,8]==Anomaly_dstPort),c(4,5,6,7,9,10)]
                    unique_dstPort_srcdstIP<-unique(dstPort_srcdstIP)
                    unique_srcIPs<-paste(unique(unique_dstPort_srcdstIP[,1:4])[,1],unique(unique_dstPort_srcdstIP[,1:4])[,2],unique(unique_dstPort_srcdstIP[,1:4])[,3],unique(unique_dstPort_srcdstIP[,1:4])[,4],sep=".",collapse = " ")

                    # Per(dst[packet])
                    num_packet_srcdstIP <- nrow(dstPort_srcdstIP)
                    num_most_packet_dstIP<-sort(table(dstPort_srcdstIP[,5:6]), decreasing=T)[1]
                    packet_dst_percent <- round(num_most_packet_dstIP / num_packet_srcdstIP * 100, digits = 2)

                    # Per(dst[srcIP])
                    num_host_uniq_src_dst_ip <- nrow(unique_dstPort_srcdstIP)
                    num_most_host_dst_ip <- sort(table(unique_dstPort_srcdstIP[,5:6]), decreasing=T)[1]
                    host_dst_percent <- round(num_most_host_dst_ip / num_host_uniq_src_dst_ip * 100, digits = 2)
                }else if(useOctet == 3){
                    dstPort_srcdstIP<-ActiveHost_data[which(ActiveHost_data[,8]==Anomaly_dstPort),c(4,5,6,9,10)]
                    unique_dstPort_srcdstIP<-unique(dstPort_srcdstIP)
                    unique_srcIPs<-paste(unique(unique_dstPort_srcdstIP[,1:4])[,1],unique(unique_dstPort_srcdstIP[,1:4])[,2],unique(unique_dstPort_srcdstIP[,1:4])[,3],sep=".",collapse = " ")

                    # Per(dst[packet])
                    num_packet_srcdstIP <- nrow(dstPort_srcdstIP)
                    num_most_packet_dstIP<-sort(table(dstPort_srcdstIP[,4:5]), decreasing=T)[1]
                    packet_dst_percent <- round(num_most_packet_dstIP / num_packet_srcdstIP * 100, digits = 2)

                    # Per(dst[srcIP])
                    num_host_uniq_src_dst_ip <- nrow(unique_dstPort_srcdstIP)
                    num_most_host_dst_ip <- sort(table(unique_dstPort_srcdstIP[,4:5]), decreasing=T)[1]
                    host_dst_percent <- round(num_most_host_dst_ip / num_host_uniq_src_dst_ip * 100, digits = 2)
                }else if(useOctet == 2){
                    dstPort_srcdstIP<-ActiveHost_data[which(ActiveHost_data[,8]==Anomaly_dstPort),c(4,5,9,10)]
                    unique_dstPort_srcdstIP<-unique(dstPort_srcdstIP)
                    unique_srcIPs<-paste(unique(unique_dstPort_srcdstIP[,1:2])[,1],unique(unique_dstPort_srcdstIP[,1:2])[,2],sep=".",collapse = " ")

                    # Per(dst[packet])
                    num_packet_srcdstIP <- nrow(dstPort_srcdstIP)
                    num_most_packet_dstIP<-sort(table(dstPort_srcdstIP[,3:4]), decreasing=T)[1]
                    packet_dst_percent <- round(num_most_packet_dstIP / num_packet_srcdstIP * 100, digits = 2)

                    # Per(dst[srcIP])
                    num_host_uniq_src_dst_ip <- nrow(unique_dstPort_srcdstIP)
                    num_most_host_dst_ip <- sort(table(unique_dstPort_srcdstIP[,3:4]), decreasing=T)[1]
                    host_dst_percent <- round(num_most_host_dst_ip / num_host_uniq_src_dst_ip * 100, digits = 2)
                }

                # 一点集中型判定
                if(host_dst_percent > dst_percent_upper_bound && packet_dst_percent > dst_percent_upper_bound){
                    alert_level[p] <- 4
                }

                # アラート記録
                alert_info<-data.frame(basis_r,group,alert_level[p],srcIP_by_unique_ActiveHost_dstPort[p,2],srcIP_by_unique_ActiveHost_dstPort[p,3],unique_srcIPs)
                rownames(alert_info)<-NULL
                colnames(alert_info)<-c("BasisR","Group","Lv","#srcIP","%srcIP","srcIPs")
                write.table(alert_info, paste0(alertALL_space,Anomaly_dstPort,"_srcIPs.csv"),quote=T,sep=",",col.names=F,row.names=F,append=T)

                # minMDL_r
                if(basis_r==minMDL_r && mdl_flag==1){
                    write.table(alert_info, paste0(alertMDL_space,Anomaly_dstPort,"_srcIPs.csv"),quote=T,sep=",",col.names=F,row.names=F,append=T)
                }
            }
        }
    }
    if(pdf_flag==1){
        invisible(dev.off())
    }
}
if(pdf_flag==1 && mdl_flag==1){
    invisible(file.copy(paste0(NMFpdf_space,"Nomalized_MatrixH_BasisR",minMDL_r,"_",ActiveHost_threshold,"_",AnomalyActiveHost_packet_threshold,".pdf"), paste0(MDL_space,"Nomalized_MatrixH_BasisR",minMDL_r,"_",ActiveHost_threshold,"_",AnomalyActiveHost_packet_threshold,".pdf")))
}
#################

#################
# アラートを一つにまとめる
dQuote_startdate<-paste0('"',startdate,'"')
dQuote_alert_space<-paste0('"',alert_space,'"')
alert_list<-list.files(alertALL_space)
Anomaly_dstPort_list<-NULL
Anomaly_dstPort_list_MDL<-NULL
for(i in 1:length(alert_list)){
    if(length(alert_list)==0){
        break
    }
    alert_info<-read.table(paste0(alertALL_space,alert_list[i]),header = F,sep=",")

    # Anomaly_dstPort
    Anomaly_dstPort<-strsplit(alert_list[i] , "_")[[1]][1]
    Anomaly_dstPort_list<-c(Anomaly_dstPort,Anomaly_dstPort_list)
    # alert level
    if(any(alert_info[,3]==4)){
        alert_level<-4
    }else if(any(alert_info[,3]==1)){
        alert_level<-1
    }else if(any(alert_info[,3]==2)){
        alert_level<-2
    }else if(any(alert_info[,3]==3)){
        alert_level<-3
    }

    #srcIP
    if(alert_srcIP_flag==1){
        alert_srcIP_list<-strsplit(as.character(alert_info[,6]), " ")
        intersect_alert_srcIP_list<-unlist(alert_srcIP_list[1])
        for(j in 1:nrow(alert_info)){
            intersect_alert_srcIP_list<-intersect(intersect_alert_srcIP_list, unlist(alert_srcIP_list[j]))
        }
        write.table(intersect_alert_srcIP_list, paste0(alert_summary_srcIP_space, date, "_", Anomaly_dstPort, "_sensor", sensor,"_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound,alert_num_upper_bound, "_Intersection_srcIP.txt"), quote=T,col.names=F,row.names=F,append=F) # 積集合
        write.table(unique(unlist(alert_srcIP_list)), paste0(alert_summary_srcIP_space, date, "_", Anomaly_dstPort, "_sensor", sensor,"_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound, alert_num_upper_bound, "_Union_srcIP.txt"), quote=T,col.names=F,row.names=F,append=F) # 和集合
    }

    # 1つにまとめたalert_info_summary
   if(alert_srcIP_flag==1){
               alert_info_summary<-paste(dQuote_startdate,Anomaly_dstPort,nrow(alert_info),alert_level,length(unique(unlist(alert_srcIP_list))),length(intersect_alert_srcIP_list),max(alert_info[,5]),min(alert_info[,5]),round(median(alert_info[,5]),digits=2),dQuote_alert_space,sep=",")
    }else{
        alert_info_summary<-paste(dQuote_startdate,Anomaly_dstPort,nrow(alert_info),alert_level,max(alert_info[,5]),min(alert_info[,5]),round(median(alert_info[,5]),digits=2),dQuote_alert_space,sep=",")
   }
#     write.table(alert_info_summary, paste0(alert_space,"alert.csv"), quote=F,col.names=F,row.names=F,append=T)
    write.table(alert_info_summary, alertALL_summary_space, quote=F,col.names=F,row.names=F,append=T)
}

if(!file.exists(paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list/"))){
    dir.create(paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list/"))
}
if(length(Anomaly_dstPort_list)==0){
    write.table("0",paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list/", year, month, "_Anomaly_dstPort_list_ver",ver,".txt"), quote=F,col.names=F,row.names=F,append=F)
}else{
    Anomaly_dstPort_list<-paste(Anomaly_dstPort_list,collapse=",")
    write.table(Anomaly_dstPort_list,paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list/", year, month, "_Anomaly_dstPort_list_ver",ver,".txt"), quote=F,col.names=F,row.names=F,append=F)
}

# minMDL_r
if(mdl_flag==1){
    alert_list<-list.files(alertMDL_space)

    for(i in 1:length(alert_list)){
        if(length(alert_list)==0){
            break
        }
        alert_info<-read.table(paste0(alertMDL_space,alert_list[i]),header = F,sep=",")

        # Anomaly_dstPort
        Anomaly_dstPort<-strsplit(alert_list[i] , "_")[[1]][1]
        Anomaly_dstPort_list_MDL<-c(Anomaly_dstPort,Anomaly_dstPort_list_MDL)

        # alert level
        if(any(alert_info[,3]==4)){
            alert_level<-4
        }else if(any(alert_info[,3]==1)){
            alert_level<-1
        }else if(any(alert_info[,3]==2)){
            alert_level<-2
        }else if(any(alert_info[,3]==3)){
            alert_level<-3
        }

        #srcIP
        if(alert_srcIP_flag==1){
            alert_srcIP_list<-strsplit(as.character(alert_info[,6]), " ")
            intersect_alert_srcIP_list<-unlist(alert_srcIP_list[1])
            for(j in 1:nrow(alert_info)){
                intersect_alert_srcIP_list<-intersect(intersect_alert_srcIP_list, unlist(alert_srcIP_list[j]))
            }
            write.table(intersect_alert_srcIP_list, paste0(alert_summary_srcIP_space, date, "_", Anomaly_dstPort, "_sensor", sensor,"_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound, alert_num_upper_bound, "_Intersection_srcIP_MDL.txt"), quote=T,col.names=F,row.names=F,append=F) # 積集合
            write.table(unique(unlist(alert_srcIP_list)), paste0(alert_summary_srcIP_space, date, "_", Anomaly_dstPort, "_sensor", sensor,"_octet", useOctet, "_ver", ver, "_", Choose_Portinfo_ActiveHost,AnomalyActiveHost_packet_threshold,AnomalyActiveHost_threshold,"_",alert_percent_upper_bound, alert_num_upper_bound, "_Union_srcIP_MDL.txt"), quote=T,col.names=F,row.names=F,append=F) # 和集合
        }

        # 1つにまとめたalert_info_summary
       if(alert_srcIP_flag==1){
            alert_info_summary<-paste(dQuote_startdate,Anomaly_dstPort,nrow(alert_info),alert_level,length(unique(unlist(alert_srcIP_list))),length(intersect_alert_srcIP_list),max(alert_info[,5]),min(alert_info[,5]),round(median(alert_info[,5]),digits=2),dQuote_alert_space,sep=",")
       }else{
            alert_info_summary<-paste(dQuote_startdate,Anomaly_dstPort,nrow(alert_info),alert_level,max(alert_info[,5]),min(alert_info[,5]),round(median(alert_info[,5]),digits=2),dQuote_alert_space,sep=",")
       }
    #     write.table(alert_info_summary, paste0(alert_space,"alert_MDL.csv"), quote=F,col.names=F,row.names=F,append=T)
        write.table(alert_info_summary, alertMDL_summary_space, quote=F,col.names=F,row.names=F,append=T)
    }
    if(!file.exists(paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list_MDL/"))){
        dir.create(paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list_MDL/"))
    }
    if(length(Anomaly_dstPort_list_MDL)==0){
        write.table("0",paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list_MDL/", year, month, "_Anomaly_dstPort_list_MDL_ver", ver, ".txt"), quote=F,col.names=F,row.names=F,append=F)
    }else{
        Anomaly_dstPort_list_MDL<-paste(Anomaly_dstPort_list_MDL,collapse=",")
        write.table(Anomaly_dstPort_list_MDL,paste0(user_home, "sensor", sensor, "/Anomaly_dstPort_list_MDL/", year, month, "_Anomaly_dstPort_list_MDL_ver", ver, ".txt"), quote=F,col.names=F,row.names=F,append=F)
    }
}

#################
