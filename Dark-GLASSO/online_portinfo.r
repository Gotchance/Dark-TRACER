####################################
# initialization
####################################
rm(list=ls())
options(digits=16)
options(scipen=25)
library(glasso)
#library(igraph)
old.op <- options(max.print=999999)
argv <- commandArgs(TRUE)
####################################

####################################
# Parameters
####################################
sensor <- as.character(argv[1])		# sensor ID		sensor <- "sensor103"
data_filespace <- as.character(argv[2])	# user name

year <- as.numeric(argv[3])
month <- as.numeric(argv[4])
day <- as.numeric(argv[5])
hour <- as.numeric(argv[6])
minute <- as.numeric(argv[7])

rho <- as.numeric(argv[8])			# value of r
rho_update <- as.numeric(argv[9])	# update interval of r value
rho_stop <- as.numeric(argv[10])		# value of r to stop

octet <- as.numeric(argv[11])						# 2~4: up to 2nd~4th octet of source host
alert_percent_upper_bound <- as.numeric(argv[12])	# Percentage limit of hosts by port for alert determination
alert_num_upper_bound <- as.numeric(argv[13])
dst_percent_upper_bound <- as.numeric(argv[14])
theta <- as.numeric(argv[15])
set_interval <- as.numeric(argv[16])
num_of_sample <- as.numeric(argv[17])

protocol <- as.character(argv[18])
name_result <- as.character(argv[19])

portinfo_flag <- as.numeric(argv[20])		# Calculate portinfo:1 Do not calculate:0
RT_rmalert_flag <- as.numeric(argv[21])
ver <- as.numeric(argv[22])
alert_json_txtname <- as.character(argv[23])
alert_window_size <- as.numeric(argv[24])

interval <- set_interval * num_of_sample

year <- as.character(year)
month <- formatC(month, width=2, flag="0")
month <- as.character(month)
day <- formatC(day, width=2, flag="0")
day <- as.character(day)
hour <- formatC(hour, width=2, flag="0")
hour <- as.character(hour)
minute <- formatC(minute, width=2, flag="0")
minute <- as.character(minute)

entire_alertlist <- NULL
variance <- function(x) var(x)*(length(x)-1)/length(x)
log_alert <- NULL		# Log of alerts per rho


###############################################################
# FILE
###############################################################
filespace <- paste(data_filespace, sensor, "/", year, month, "/", year, month, day, "/", year, month, day, hour, minute, "/", sep="")
result_filespace <- paste(filespace, name_result, sep="")

alert_resultspace <- paste(data_filespace, "alert_result/", alert_json_txtname, sep="")
if(!file.exists(paste(data_filespace, "alert_result/", sep=""))){
	dir.create(paste(data_filespace, "alert_result/", sep=""))
}

if(octet == 4){
	density_old_dir <- paste("density_old_octet4_", theta, sep="")
}else if(octet == 2){
	density_old_dir <- paste("density_old_", theta, sep="")
}
density_dir <- paste("density_", theta, sep="")
eps_dir <- paste("eps_", theta, sep="")
portinfo_dir <- paste("portinfo_", theta, sep="")

# ディレクトリ作成
dir.create(paste(result_filespace, density_dir, "/", sep=""))
dir.create(paste(result_filespace, eps_dir, "/", sep=""))
if(portinfo_flag == 1){
	dir.create(paste(result_filespace, portinfo_dir, "/", sep=""))
}


input_filespace <- paste(filespace, "input/", sep="")
UH_filespace <- paste(result_filespace,"unique_host/",sep="")

filedate <- paste(year, month, day, hour, minute, sep="")
if(octet == 2){
	filedate_txt <- paste(year, month, day, hour, minute, "_ver", ver, ".txt", sep="")
}else if(octet == 4){
	filedate_txt <- paste(year, month, day, hour, minute, "_ver", ver, "_octet4.txt", sep="")
}

###############################################################
# Log
###############################################################
variable_name <- c("theta <- ", "alert_json_txtname <-")
variable_value <- c(theta, alert_json_txtname)
(variable <- data.frame(NAME = variable_name, VALUE = variable_value))

write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)	# 空白行
write.table(variable, paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)			# 変数記録
write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)	# 空白行
write.table(paste("Moment","User ", "System", "Progress", sep="		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)		# 実行時間記録

if(!file.exists(paste(UH_filespace, "entire_unique_host/", sep=""))){
	entire_host_flag <- 0
}else{
	entire_host_flag <- 0
}

###############################################################
# DATE_LOOP
###############################################################
repeat {
	rho <- rho + rho_update

	##########################################
	# Graph Density
	##########################################
	# Connect 'density_realtime' and existing 'density_old'
	RT_graph_density <- read.table(paste(result_filespace,"density/RT_density_",rho,".txt",sep=""))
	RT_graph_density_nrow <- nrow(RT_graph_density)

	if(file.exists(paste(filespace, density_old_dir, "/alert_",rho,".txt",sep=""))){
		graph_density_old_alert <- read.table(paste(filespace, density_old_dir, "/alert_",rho,".txt",sep=""))
	}else{
		write.table(matrix(c(0,0), 1, 2),paste(filespace, density_old_dir, "/alert_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)
		graph_density_old_alert <- read.table(paste(filespace, density_old_dir, "/alert_",rho,".txt",sep=""))
	}

	if(file.exists(paste(filespace, density_old_dir, "/density_",rho,".txt",sep=""))){
		graph_density_old <- read.table(paste(filespace, density_old_dir, "/density_",rho,".txt",sep=""))
	}else{
		write.table(matrix(c(0,0,0,0), 1, 4),paste(filespace, density_old_dir, "/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)
		graph_density_old <- read.table(paste(filespace, density_old_dir, "/density_",rho,".txt",sep=""))
	}

	if(RT_rmalert_flag == 1){		# If you want to delete alerts previously obtained in real time
		if(graph_density_old_alert[1,1] != 0){		# If there is an alert
			graph_density_old_order <- graph_density_old[order(-graph_density_old[,2]),]
			graph_density_old_order <- graph_density_old_order[-1:-nrow(graph_density_old_alert),]	# Delete the ones with DENSITY larger than the number of ALERTs
			graph_density_old <- graph_density_old_order[order(graph_density_old_order[,1]),]		# density reordering

			graph_density_sum_line <- nrow(graph_density_old) + nrow(RT_graph_density) - alert_window_size
			if(graph_density_sum_line > 0){
				graph_density_old <- graph_density_old[-1:-graph_density_sum_line,]
			}

			try(rm(graph_density_old_order), silent=TRUE)
		}else{						# If tehre is no alert
			graph_density_sum_line <- nrow(graph_density_old) + nrow(RT_graph_density) - alert_window_size
			if(graph_density_sum_line > 0){
				graph_density_old <- graph_density_old[-1:-graph_density_sum_line,]
			}
		}
	}else{
		graph_density_sum_line <- nrow(graph_density_old) + nrow(RT_graph_density) - alert_window_size
		if(graph_density_sum_line > 0){
			graph_density_old <- graph_density_old[-1:-graph_density_sum_line,]
		}
	}

	# Connect 'density_old' and 'RT_density' to record
	RT_graph_density <- rbind(graph_density_old, RT_graph_density)
	if(RT_graph_density[1,1] == 0){
		RT_graph_density <- RT_graph_density[-1,]
	}
	write.table(RT_graph_density, paste(result_filespace, density_dir, "/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)
	graph_density <- read.table(paste(result_filespace, density_dir, "/density_",rho,".txt",sep=""))

	try(rm(RT_graph_density,RT_graph_density_nrow, graph_density_old, graph_density_old_alert, graph_density_sum_line), silent=TRUE)

	# When the number of graph densities exceeds (alert_window_size-1), alert judgment and Portinfo are executed.
	if(nrow(graph_density) >= (alert_window_size-1) ){

		##########################
		# Alert judgment
		##########################
		density_order <- graph_density[order(-graph_density[,2]),]		# graph_densityの2列目(密度)を昇順でdensity_orderに代入
		density_nrow <- nrow(density_order)								# density_order(密度)の行数をdensity_nrowに代入
		density_min <- density_nrow										# density_nrowを同じくdensity_minに代入し、退避
		graph_density <- graph_density[,2]								# graph_densityの2列目(密度)をgraph_densityに代入、ここで1列目(タイムスタンプ)は消滅

		repeat {
			graph_density_tmp <- graph_density[-which.max(graph_density)]				# graph_density(密度)の一番大きい要素(max)を除いたデータをgraph_density_tmpに代入

			if (variance(graph_density) == 0 || is.na(variance(graph_density_tmp)) || variance(graph_density_tmp)/variance(graph_density) > theta){	# graph_density_tmpの標本分散割るgraph_densityの標本分散が0.95より大きいとき、またはvariance(graph_density_tmp)がNAのとき
				break										# break.  結局graph_densityはmaxの要素何個か(alertの対象データ)が抜けたデータになる．
			} else {									# 0.95より大きくないときは
				density_min <- density_min - 1				# density_min(graph_densityの行数)-1をdensity_minに代入
				graph_density <- graph_density_tmp			# graph_density_tmpをgraph_densityに代入する
			}
		}

		density_alert <- density_nrow - density_min			# density_nrow-density_min(alertの対象データの個数)をdensity_alertに代入

		if(density_alert == 0){
			density_order <- matrix(c(0,0), 1, 2)
		}else{
			density_order <- density_order[1:density_alert,]	# alertの対象データだけをdensity_orderに代入(1列：タイムスタンプ、2列：密度)
		}

		# alert_rho.txtにalertの対象データを書き込む(1列：タイムスタンプ、2列：密度)
		write.table(density_order,paste(result_filespace, density_dir, "/alert_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)

		log_rho <- paste("rho  = ", rho, sep="")
		log_number_of_alert <- paste(log_rho, density_alert, sep="		")
		log_alert <- rbind(log_alert, log_number_of_alert)

		try(rm(graph_density,density_order,density_nrow,density_min,graph_density_tmp,density_alert), silent=TRUE)



		#####################################
		# Density EPS graph
		#####################################

		filedate_eps <- paste(year,month,day,hour,minute, "_",rho,"_d.eps",sep="")
		eps_filespace <- paste(result_filespace, eps_dir, "/",filedate_eps,sep="")

		graph_density <- read.table(paste(result_filespace, density_dir, "/density_",rho,".txt",sep=""))
		density_alert <- read.table(paste(result_filespace, density_dir, "/alert_",rho,".txt",sep=""))
		density_alert_nrow <- nrow(density_alert)

		postscript(eps_filespace, horizontal = FALSE, onefile = FALSE, paper = "special", height = 6, width = 15)	# ADOBE ポストスクリプト作図デバイス horizontal = FALSE, onfile = FALSE, paper="special" を指定しないとTeX に取り込んだ際,横にひっくり返る
		par(pch=19)
		par(ps=20)				# 文字とシンボルの大きさを指定
		par(mar=c(5,5,2,1))		# 下、左、上、右の順で余白指定
		plot(graph_density[,1],graph_density[,2], type="b",main=paste(sensor,", r = ",rho,sep=""),lwd=3,col="blue",ylab="Density",xlab="UNIX Time")									# グラフを描く type : b, hがいい
		if(density_alert[density_alert_nrow, 2] != 0){
			abline(h=density_alert[density_alert_nrow, 2], col='red', lwd=3, lty=1)
		}
		legend("topleft",legend = c("Density","Alert"),col = c("blue","red"),pch=19,lty=1,lwd=3, bty="n")			# グラフに凡例を付ける
		#axis(4)								# 右に座標を描く
		#mtext("Density",side = 4,line = 3)		# 右の3行目にDensity文字列を書き込む
		#par(new=T)		# 上書き指定
		dev.off()	# 必要な出力がすべて終ったらすぐにデバイスを閉じると不具合が起こりにくい

		try(rm(graph_density,density_alert,density_alert_nrow), silent=TRUE)


		#####################################
		# portinfo:
		# Arrange the hosts and destination ports for the time period when you get an alert in an easy-to-read format.
		#####################################
		if(portinfo_flag == 1){
			# アラートが出た時間帯をalertlistに代入
			alertlist <- read.table(paste(result_filespace, density_dir, "/alert_",rho,".txt",sep=""))
			alertlist <- as.matrix(alertlist)
			alertlist <- alertlist[,1]

			# アラートが一つもない場合
			if(alertlist[1] == 0){
				if(rho < rho_stop){		# まだrhoが続く場合
					next				# 次のMainループに行く
				}else{					# Mainループが終わる場合
					# 終わりの合計実行時間
					end_proc_time <- t(as.matrix(proc.time()))
					end_proc_time <- round(end_proc_time, digits = 3)
					write.table(paste("Total ", end_proc_time[1], end_proc_time[2], end_proc_time[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

					# 空白行
					write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

					write.table("num of alerts(each rho)", paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)
					write.table(log_alert, paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

					break		# 終了
				}
			}

			# アラートがある場合
			# アラート重複チェック
			entire_alertlist_length <- length(unique(entire_alertlist))
			entire_alertlist <- c(entire_alertlist, alertlist)
			if(entire_alertlist_length == length(unique(entire_alertlist))){
				if(rho < rho_stop){		# まだrhoが続く場合
					next				# 次のMainループに行く
				}else{					# Mainループが終わる場合
									# 終わりの合計実行時間
					end_proc_time <- t(as.matrix(proc.time()))
					end_proc_time <- round(end_proc_time, digits = 3)
					write.table(paste("Total ", end_proc_time[1], end_proc_time[2], end_proc_time[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

					# 空白行
					write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

					write.table("num of alerts(each rho)", paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)
					write.table(log_alert, paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

					break		# 終了
				}
			}else if(entire_alertlist_length == 0){

			}else{
				alertlist <- unique(entire_alertlist)[-1:-entire_alertlist_length]
			}


			if(!file.exists(paste(result_filespace, portinfo_dir, "/rho_", rho, "/", sep=""))){
				dir.create(paste(result_filespace, portinfo_dir, "/rho_",rho,"/", sep=""))
			}

			portinfo_count_num <- 1		# アラートの番号(アラートが複数時間帯ある場合に区別するため)

			# アラートの数の分，繰り返す
			for(alert_time in alertlist){

				# 過去の時刻のアラートの場合
				if(class(try(read.table(paste(UH_filespace, alert_time, ".txt",sep=""),sep="."), silent=TRUE)) == "try-error"){
					alert_year <- format(as.POSIXct(alert_time, origin="1970-1-1",tz="Japan"), format="%Y")
					alert_month <- format(as.POSIXct(alert_time, origin="1970-1-1",tz="Japan"), format="%m")
					alert_day <- format(as.POSIXct(alert_time, origin="1970-1-1",tz="Japan"), format="%d")
					alert_hour <- format(as.POSIXct(alert_time, origin="1970-1-1",tz="Japan"), format="%H")
					alert_minute <- format(as.POSIXct(alert_time, origin="1970-1-1",tz="Japan"), format="%M")

					date_alert_time <- paste(alert_year, alert_month, alert_day, alert_hour, alert_minute, sep="")
					UH_filespace_alert_time <- paste(data_filespace, sensor, "/", alert_year, alert_month, "/", alert_year, alert_month, alert_day, "/", date_alert_time, "/", name_result, "unique_host/", sep="")
					if(class(try(read.table(paste(UH_filespace_alert_time, alert_time, ".txt",sep=""),sep="."), silent=TRUE)) == "try-error"){
						next
					}else{
						file.copy(paste(UH_filespace_alert_time, alert_time, ".txt", sep=""), paste(UH_filespace, alert_time, ".txt", sep=""))
						data_tmp <- read.table(paste(data_filespace, sensor, "/", alert_year, alert_month, "/", alert_year, alert_month, alert_day, "/", date_alert_time, "/input/", date_alert_time, "_ver", ver, ".txt", sep=""), sep=" ")
					}
					pre <- paste("1(", filedate, ")", sep="")
				}else{
					# 現在の時刻のアラートの場合
					data_tmp <- read.table(paste(input_filespace, filedate_txt, sep=""))			# " "を区切りにdata_tmpにtmp.txtを読み込む
					pre <- 0
				}

				if(is.vector(data_tmp)){
					data_tmp <- t(as.matrix(data_tmp))
				}

				# 時間Unix Time
				data_time <- data_tmp[,1]							# data_tmpの1列目（時刻)をdata_timeにnumericとして代入, data_tmp[1]だとlist
				data_time <- floor(data_time)						# data_time以上でない最大の整数を返す．(小数点以下切り捨て), data_time更新
				# 送信元ホスト・ポート
				source_host_tmp <- data_tmp[,3]						# data_tmpの3列目（送信元）をsource_host_tmpに代入, data_tmp[,3]はnumericなため
				source_host_tmp <-as.character(source_host_tmp)		# 文字列として代入
				# 送信先ホスト・ポート
				dst_host_tmp <- data_tmp[,5]						# data_tmpの5列目(送信先darknet IP)をdst_host_tmpに代入, data_tmp[,5]はnumericなため
				dst_host_tmp <- as.character(dst_host_tmp)			# 文字列として代入

				source_sepa <- t(matrix(unlist(strsplit(source_host_tmp, "\\.")), nrow=5))
				dst_sepa <- t(matrix(unlist(strsplit(dst_host_tmp, "\\.")), nrow=5))

				# octetごと
				if(octet == 2){
					source_host <- paste(source_sepa[,1], ".", source_sepa[,2], sep="")		# 送信元IPアドレスをsource_hostに代入 ex) 133.5
				}else if(octet == 3){
					source_host <- paste(source_sepa[,1], ".", source_sepa[,2], ".", source_sepa[,3], sep="")		# 送信元IPアドレスをsource_hostに代入 ex) 133.5.16
				}else if(octet == 4){
					source_host <- paste(source_sepa[,1], ".", source_sepa[,2], ".", source_sepa[,3], ".", source_sepa[,4], sep="")		# 送信元IPアドレスをsource_hostに代入 ex) 133.5.16.11
				}
				dst_host <- paste(dst_sepa[,1],dst_sepa[,2],dst_sepa[,3],dst_sepa[,4],sep=".")										# 送信先IPアドレスをdst_hostに代入(darknet IP)

				if(octet == 2){
					data_portinfo <- cbind(data_time, source_sepa[,1], source_sepa[,2], dst_sepa[,5])
				}else if(octet == 3){
					data_portinfo <- cbind(data_time, source_sepa[,1], source_sepa[,2], source_sepa[,3], dst_sepa[,5])
				}else if(octet == 4){
					data_portinfo <- cbind(data_time, source_sepa[,1], source_sepa[,2], source_sepa[,3], source_sepa[,4], dst_sepa[,5])
				}

				data_portinfo_dst <- cbind(data_time,source_host,dst_host,dst_sepa[,5])									# 列ベクトル単位で結合

				try(rm(data_tmp, data_time, source_host_tmp, dst_host_tmp, source_sepa, dst_sepa, source_host, dst_host), silent=TRUE)


				data_alert_time <- which(data_portinfo[,1] >= alert_time & data_portinfo[,1] < alert_time + interval)
				data_portinfo_alert_time <- data_portinfo[data_alert_time,]
				if(octet == 2){
					data_portinfo_alert_time <- matrix(as.numeric(data_portinfo_alert_time),,4)		# characterなのでnumericに変換
				}else if(octet == 3){
					data_portinfo_alert_time <- matrix(as.numeric(data_portinfo_alert_time),,5)		# characterなのでnumericに変換
				}else if(octet == 4){
					data_portinfo_alert_time <- matrix(as.numeric(data_portinfo_alert_time),,6)		# characterなのでnumericに変換
				}


				unique_host_portinfo <- read.table(paste(UH_filespace, alert_time, ".txt",sep=""),sep=".")
				unique_host_portinfo <- as.matrix(unique_host_portinfo)
				unique_host_portinfo_nrow <- nrow(unique_host_portinfo)

				if(octet == 2){
					# アラートを得た時間帯の送信元ホストのパケット数データ("."なしのIPアドレス(12.3の場合12003となる))
					data_packet_count_by_porthost_alert_time <- c(NULL, as.numeric(paste(formatC(data_portinfo_alert_time[,2], width=3, flag="0"), formatC(data_portinfo_alert_time[,3], width=3, flag="0"),sep="")))
					# そのアラートを得た時間帯の一意なホストIPアドレス
					data_unique_host_alert_time <- c(NULL, as.numeric(paste(formatC(unique_host_portinfo[,1], width=3, flag="0"), formatC(unique_host_portinfo[,2], width=3, flag="0"), sep="")))
				}else if(octet == 3){
					# アラートを得た時間帯の送信元ホストのパケット数データ("."なしのIPアドレス(12.3.45の場合12003045となる))
					data_packet_count_by_porthost_alert_time <- c(NULL, as.numeric(paste(formatC(data_portinfo_alert_time[,2], width=3, flag="0"), formatC(data_portinfo_alert_time[,3], width=3, flag="0"), formatC(data_portinfo_alert_time[,4], width=3, flag="0"), sep="")))
					# そのアラートを得た時間帯の一意なホストIPアドレス
					data_unique_host_alert_time <- c(NULL, as.numeric(paste(formatC(unique_host_portinfo[,1], width=3, flag="0"), formatC(unique_host_portinfo[,2], width=3, flag="0"), formatC(unique_host_portinfo[,3], width=3, flag="0"), sep="")))
				}else if(octet == 4){
					# アラートを得た時間帯の送信元ホストのパケット数データ("."なしのIPアドレス(12.3.45.678の場合12003045678となる))
					data_packet_count_by_porthost_alert_time <- c(NULL, as.numeric(paste(formatC(data_portinfo_alert_time[,2], width=3, flag="0"), formatC(data_portinfo_alert_time[,3], width=3, flag="0"), formatC(data_portinfo_alert_time[,4], width=3, flag="0"), formatC(data_portinfo_alert_time[,5], width=3, flag="0"),sep="")))
					# そのアラートを得た時間帯の一意なホストIPアドレス
					data_unique_host_alert_time <- c(NULL, as.numeric(paste(formatC(unique_host_portinfo[,1], width=3, flag="0"), formatC(unique_host_portinfo[,2], width=3, flag="0"), formatC(unique_host_portinfo[,3], width=3, flag="0"), formatC(unique_host_portinfo[,4], width=3, flag="0"), sep="")))
				}

				# claster_packetにunique_hostに一致するpacket_count_by_porthostを判別し、data_packet_count_by_porthostを代入(通常、全てが一致しdata_packet_count_by_porthostそのままが代入される)
				data_element <- is.element(data_packet_count_by_porthost_alert_time, data_unique_host_alert_time)
				claster_packet <- data_portinfo_alert_time[data_element,]

				if(is.vector(claster_packet)){
					claster_packet <- t(as.matrix(claster_packet))
				}

				# ホストIPアドレスと宛先ポートが一意なdataをclaster_uniqueに代入
				if(octet == 2){
					claster_unique <- unique(claster_packet[,2:4])
				}else if(octet == 3){
					claster_unique <- unique(claster_packet[,2:5])
				}else if(octet == 4){
					claster_unique <- unique(claster_packet[,2:6])
				}

				if(is.vector(claster_unique)){
					claster_unique <- t(as.matrix(claster_unique))
				}

				if(octet == 2){
					unique_port <- sort(unique(claster_packet[,4]))				# 昇順の一意な宛先ポート
				}else if(octet == 3){
					unique_port <- sort(unique(claster_packet[,5]))				# 昇順の一意な宛先ポート
				}else if(octet == 4){
					unique_port <- sort(unique(claster_packet[,6]))				# 昇順の一意な宛先ポート
				}
				packet_count_by_port <- numeric(length(unique_port))		# ポート別パケット数(unique_portの個数分の0で初期化)
				host_count_by_port <- numeric(length(unique_port))			# ポート別ホスト数

				if(octet == 2){
					for(j in 1:length(unique_port)){
						packet_count_by_port[j] <- length(which(claster_packet[,4] == unique_port[j]))		# ポート別パケット数
						host_count_by_port[j] <- length(which(claster_unique[,3] == unique_port[j]))		# ポート別ホスト数
					}
				}else if(octet == 3){
					for(j in 1:length(unique_port)){
						packet_count_by_port[j] <- length(which(claster_packet[,5] == unique_port[j]))		# ポート別パケット数
						host_count_by_port[j] <- length(which(claster_unique[,4] == unique_port[j]))		# ポート別ホスト数
					}
				}else if(octet == 4){
					for(j in 1:length(unique_port)){
						packet_count_by_port[j] <- length(which(claster_packet[,6] == unique_port[j]))		# ポート別パケット数
						host_count_by_port[j] <- length(which(claster_unique[,5] == unique_port[j]))		# ポート別ホスト数
					}
				}


				packet_sum <- sum(packet_count_by_port)													# パケット総数
				packet_percent_by_port <- packet_count_by_port/packet_sum * 100							# ポート別パケット割合
				host_percent_by_port <- host_count_by_port/unique_host_portinfo_nrow * 100				# ポート別ホスト割合

				packets_by_port <- cbind(unique_port, packet_count_by_port, packet_percent_by_port)
				if(is.vector(packets_by_port)){
					packets_by_port <- t(as.matrix(packets_by_port))
				}
				packets_by_port <- packets_by_port[order(-packets_by_port[,2]),]
				hosts_by_port <- cbind(unique_port, host_count_by_port, host_percent_by_port)
				if(is.vector(hosts_by_port)){
					hosts_by_port <- t(as.matrix(hosts_by_port))
				}
				hosts_by_port <- hosts_by_port[order(-hosts_by_port[,2]),]

				portinfo_filename <- paste(result_filespace,portinfo_dir, "/rho_",rho,"/",portinfo_count_num,"_",alert_time,"_portinfo_hosts_and_packets_count_by_port.csv",sep="")
				write.table(paste("Packets Total", packet_sum, sep=","), portinfo_filename, quote=F, col.names=F, row.names=F, append=F)
				write.table(paste("Hosts Total", unique_host_portinfo_nrow, sep=","), portinfo_filename, quote=F, col.names=F, row.names=F, append=T)
				write.table(" ", portinfo_filename, quote=F,col.names=F,row.names=F,append=T)
				write.table(rbind(c("port","packet","percent"), packets_by_port), portinfo_filename, sep=",", quote=F, col.names=F, row.names=F, append=T)
				write.table(" ", portinfo_filename, quote=F,col.names=F,row.names=F,append=T)
				write.table(rbind(c("port","host","percent"), hosts_by_port), portinfo_filename, sep=",", quote=F, col.names=F, row.names=F, append=T)

				# アラートのホストの数が3より小さい時はskipする
				#if(hosts_by_port[1,2] < 3){
				#	portinfo_count_num <- portinfo_count_num + 1
				#	next
				#}


				count_alert_upper <- 1	# アラートとされた1つ時間帯の中にアラートレベルを判定し，アラート対象になるポートの数．必ず1つのポートはアラートとして上げる
				alert_level <- NULL			# 1:attack 2:survey measurement 3:weak survey measurement
				repeat{
					if(hosts_by_port[count_alert_upper,3] >= alert_percent_upper_bound && hosts_by_port[count_alert_upper,2] >= alert_num_upper_bound){
						alert_level <- c(alert_level, 1)
					}else if(hosts_by_port[count_alert_upper,3] >= alert_percent_upper_bound && hosts_by_port[count_alert_upper,2] < alert_num_upper_bound){
						alert_level <- c(alert_level, 2)
					}else{
						alert_level <- c(alert_level, 3)
						if(hosts_by_port[count_alert_upper,2] != hosts_by_port[count_alert_upper+1,2]){
							break
						}
					}
					count_alert_upper <- count_alert_upper + 1
				}




				# アラート対象になるポートの数分繰り返す
				for(hoge in 1:count_alert_upper){

					port <- hosts_by_port[hoge, 1]		# ポート別ホスト数の割合が大きいポート順にportに代入


					if(octet == 2){
						port_host_info <- claster_packet[which(claster_packet[,4] == port), 2:3]		# そのポートのパケット数分のホストIPアドレスだけを取ってくる
					}else if(octet == 3){
						port_host_info <- claster_packet[which(claster_packet[,5] == port), 2:4]		# そのポートのパケット数分のホストIPアドレスだけを取ってくる
					}else if(octet == 4){
						port_host_info <- claster_packet[which(claster_packet[,6] == port), 2:5]		# そのポートのパケット数分のホストIPアドレスだけを取ってくる
					}

					if(is.vector(port_host_info)){
						port_host_info <- t(as.matrix(port_host_info))
					}
					port_host_info_nrow <- nrow(port_host_info)

					if(octet == 2){
						host_format <- c(NULL,as.numeric(paste(formatC(port_host_info[,1], width=3, flag="0"),formatC(port_host_info[,2], width=3, flag="0"),sep="")))		# ex) 123.45 -> 123045
					}else if(octet == 3){
						host_format <- c(NULL,as.numeric(paste(formatC(port_host_info[,1], width=3, flag="0"),formatC(port_host_info[,2], width=3, flag="0"),formatC(port_host_info[,3], width=3, flag="0"),sep="")))		# ex) 123.45.6 -> 123045006
					}else if(octet == 4){
						host_format <- c(NULL,as.numeric(paste(formatC(port_host_info[,1], width=3, flag="0"),formatC(port_host_info[,2], width=3, flag="0"),formatC(port_host_info[,3], width=3, flag="0"),formatC(port_host_info[,4], width=3, flag="0"),sep="")))		# ex) 123.45.6.789 -> 123045006789
					}

					if(nrow(port_host_info) == 1){

					}else if(octet == 2){
						port_host_info <- port_host_info[order(port_host_info[,2]),]
						port_host_info <- port_host_info[order(port_host_info[,1]),]
					}else if(octet == 3){
						port_host_info <- port_host_info[order(port_host_info[,3]),]
						port_host_info <- port_host_info[order(port_host_info[,2]),]
						port_host_info <- port_host_info[order(port_host_info[,1]),]
					}else if(octet == 4){
						port_host_info <- port_host_info[order(port_host_info[,4]),]
						port_host_info <- port_host_info[order(port_host_info[,3]),]
						port_host_info <- port_host_info[order(port_host_info[,2]),]
						port_host_info <- port_host_info[order(port_host_info[,1]),]
					}

					port_host_info <- unique(port_host_info)
					if(is.vector(port_host_info)){
						port_host_info <- t(as.matrix(port_host_info))
					}

					if(octet == 2){
						unique_order_host <- c(NULL,as.numeric(paste(formatC(port_host_info[,1], width=3, flag="0"),formatC(port_host_info[,2], width=3, flag="0"),sep="")))		# 一意に昇順にホスト
					}else if(octet == 3){
						unique_order_host <- c(NULL,as.numeric(paste(formatC(port_host_info[,1], width=3, flag="0"),formatC(port_host_info[,2], width=3, flag="0"),formatC(port_host_info[,3], width=3, flag="0"),sep="")))		# 一意に昇順にホスト
					}else if(octet == 4){
						unique_order_host <- c(NULL,as.numeric(paste(formatC(port_host_info[,1], width=3, flag="0"),formatC(port_host_info[,2], width=3, flag="0"),formatC(port_host_info[,3], width=3, flag="0"),formatC(port_host_info[,4], width=3, flag="0"),sep="")))		# 一意に昇順にホスト
					}

					# ホスト割合が大きかったポートに対しパケットを送ったホスト別のパケット数を求める
					each_host_packets <- numeric(length(unique_order_host))
					if(port_host_info_nrow != 0){
						for(j in 1:length(unique_order_host)){
							tmp_an_unique_order_host <- unique_order_host[j]
							for(k in 1:port_host_info_nrow){
								if(host_format[k] == tmp_an_unique_order_host){
									each_host_packets[j] <- each_host_packets[j] + 1
								}
							}
						}
					}else{
						each_host_packets <- c(0)
					}
					each_host_packets <- as.matrix(each_host_packets, ncol=1)

					# ホストIPアドレス
					if(octet == 2){
						t1 <- port_host_info[,1]
						t2 <- port_host_info[,2]
						if(port_host_info_nrow != 0){
							port_host_IP <- paste(t1, ".", t2, sep="")
						}else{
							port_host_IP <- c(0)
						}
					}else if(octet == 3){
						t1 <- port_host_info[,1]
						t2 <- port_host_info[,2]
						t3 <- port_host_info[,3]
						if(port_host_info_nrow != 0){
							port_host_IP <- paste(t1, ".", t2, ".", t3, sep="")
						}else{
							port_host_IP <- c(0)
						}
					}else if(octet == 4){
						t1 <- port_host_info[,1]
						t2 <- port_host_info[,2]
						t3 <- port_host_info[,3]
						t4 <- port_host_info[,4]
						if(port_host_info_nrow != 0){
							port_host_IP <- paste(t1, ".", t2, ".", t3, ".", t4, sep="")
						}else{
							port_host_IP <- c(0)
						}
					}



					# PtH: ホスト割合が大きかったポートに対しパケットを送ったホストIPアドレスとそのパケット数
					port_host_packets <- cbind(port_host_IP, each_host_packets)


					# StD: ホスト割合が大きかったポートに対しパケットを送ったホストIPアドレスと宛先ダークネットIP
					data_portinfo_dst_tmp <- data_portinfo_dst[data_alert_time,]
					data_portinfo_dst_tmp <- data_portinfo_dst_tmp[data_element,]
					data_portinfo_dst_tmp <- data_portinfo_dst_tmp[which(data_portinfo_dst_tmp[,4] == port),]
					if(is.vector(data_portinfo_dst_tmp)){
						data_portinfo_dst_tmp <- t(as.matrix(data_portinfo_dst_tmp))
					}
					data_portinfo_dst_tmp <- data_portinfo_dst_tmp[,1:3]
					if(is.vector(data_portinfo_dst_tmp)){
						data_portinfo_dst_tmp <- t(as.matrix(data_portinfo_dst_tmp))
					}


					# 一点集中型判定条件は以下の通り
					# (1) 一番多いdst IP宛のパケット数と全パケット数の割合
					# (2) 一番多いdst IP宛にパケット投げたユニークホスト数と全ユニークホスト数の割合
					# ｢(1)>70% AND (2)>70%｣の時，一点集中型だと判定する．
					src_dst_ip <- as.data.frame(data_portinfo_dst_tmp[,2:3])
					colnames(src_dst_ip) <- c("src_ip", "dst_ip")
					uniq_src_dst_ip <- src_dst_ip[!duplicated(paste(src_dst_ip$src_ip, src_dst_ip$dst_ip, sep = "")), ]

					# (1) パケット数
					num_packet_src_dst_ip <- nrow(src_dst_ip)
					num_most_packet_dst_ip <- sort(table(src_dst_ip$dst_ip), decreasing=T)[][[1]]
					packet_dst_percent <- num_most_packet_dst_ip / num_packet_src_dst_ip * 100

					# (2) ホスト数
					num_host_uniq_src_dst_ip <- nrow(uniq_src_dst_ip)
					num_most_host_dst_ip <- sort(table(uniq_src_dst_ip$dst_ip), decreasing=T)[][[1]]
					host_dst_percent <- num_most_host_dst_ip / num_host_uniq_src_dst_ip * 100

					if(host_dst_percent > dst_percent_upper_bound && packet_dst_percent > dst_percent_upper_bound){
						alert_level[hoge] <- 4
					}


					# portinfo filespace
					UH_filename <- paste(result_filespace,portinfo_dir, "/rho_",rho,"/",portinfo_count_num,"_level",alert_level[hoge],"_port",port,"_",alert_time,"/", sep="")
					if(!file.exists(UH_filename)){
						dir.create(UH_filename)
					}

					PtH_filename <- paste(UH_filename,"Packet_to_Host_", alert_time, "_", port, ".txt", sep="")
					write.table(port_host_packets,PtH_filename,quote=F,col.names=F,row.names=F,append=F)

					StD_filename<-paste(UH_filename,"SourceHost_to_DstHost",  alert_time, "_", port, ".txt", sep="")
					write.table(data_portinfo_dst_tmp, StD_filename,quote=F,col.names=F,row.names=F,append=F)


					# write alert!
					format_data_time <- format(as.POSIXct(alert_time, origin="1970-1-1",tz="Japan"), format="%Y/%m/%d %H:%M:%S")
					hosts_by_port[hoge, 3] <- format(as.numeric(hosts_by_port[hoge, 3]), digits=2)
					packet_dst_percent <- format(as.numeric(packet_dst_percent), digits=2)
					host_dst_percent <- format(as.numeric(host_dst_percent), digits=2)

					alert_result_json <- paste('{"Time":"', format_data_time, '","Port":', port, ',"Lv":', alert_level[hoge], ',"Num(src)":', hosts_by_port[hoge, 2], ',"Per(src)":', hosts_by_port[hoge, 3], ',"Per(dst[packet])":', packet_dst_percent, ',"Per(dst[host])":', host_dst_percent, ',"pre":"', pre, '","ID":"', sensor, '","rho":', rho, '}', sep="")
					write.table(alert_result_json, alert_resultspace, quote=F, col.names=F, row.names=F, append=T)

					if(octet == 4 && pre == 0 && (alert_level[hoge] == 2 || alert_level[hoge] == 3)){
						scanner_result_json <- paste('{"Time":"', format_data_time, '","Port":', port, ',"Num(src)":', hosts_by_port[hoge, 2], ',"Per(src)":', hosts_by_port[hoge, 3], ',"ID":"', sensor, '","rho":', rho, '}', sep="")
						write.table(scanner_result_json, paste(data_filespace, "scanner/alert.json", sep=""), quote=F, col.names=F, row.names=F, append=T)
					}


				}

				portinfo_count_num <- portinfo_count_num + 1
			}
			#try(rm(portinfo_count_num, data_alert_time, data_portinfo_alert_time, unique_host_portinfo, data_element, unique_host_portinfo_nrow, data_packet_count_by_porthost_alert_time, data_unique_host_alert_time, claster_packet, claster_unique, unique_port, packet_count_by_port, host_count_by_port, packet_sum, packet_percent_by_port, host_percent_by_port, count_alert_upper, hoge, port, port_host_info, port_host_info_nrow, host_format, unique_order_host, each_host_packets, j, tmp_an_unique_order_host, k, t1, t2, t3, t4, port_host_IP, port_host_packets, data_portinfo_dst_tmp, list_port, list_PtH, list_SHtD, data_time, result_json, num_specific_info, PbP, HbP), silent=TRUE)

		}
	}


	################################
	# r adjustment
	#
	# However, if rho exceeds the maximum setting value, break
	################################
	if(rho < rho_stop){
		next		# 最初のLoop(repeat)に戻る
	}else{
		# 終わりの合計実行時間
		end_proc_time <- t(as.matrix(proc.time()))
		end_proc_time <- round(end_proc_time, digits = 3)
		write.table(paste("Total ", end_proc_time[1], end_proc_time[2], end_proc_time[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

		# 空白行
		write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

		write.table("num of alerts(each rho)", paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)
		write.table(log_alert, paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

		break		# 終了
	}

}
