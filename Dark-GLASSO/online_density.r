####################################
# initialization
####################################
rm(list=ls()) 							# clear object list
options(digits=16)						# set display digits to 16
options(scipen=25)						# avoid exponential representation of large numbers
library(glasso)							#
#library(igraph)						#
old.op <- options(max.print=999999)		# workaround for error [exceeded getOption("max.print")
argv <- commandArgs(TRUE)				#
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
rho_stop <- as.numeric(argv[10])	# value of r to stop

octet <- as.numeric(argv[11])						# 2~4: up to 2nd~4th octet of source host
host_upper_bound <- as.numeric(argv[12])			# limit number of hosts
set_interval <- as.numeric(argv[13])				# time interval for one sample
num_of_sample <- as.numeric(argv[14])				# number of sample

protocol <- as.character(argv[15])
name_result <- as.character(argv[16])

RT_flag <- as.numeric(argv[17])
gml_flag <- as.numeric(argv[18])			# calculate gml graph:1 don't calculate:0 (need igraph library to use)
ver <- as.numeric(argv[19])

year <- as.character(year)
month <- formatC(month, width=2, flag="0")	# when converting a number to a string, prepend "0"
month <- as.character(month)
day <- formatC(day, width=2, flag="0")
day <- as.character(day)
hour <- formatC(hour, width=2, flag="0")
hour <- as.character(hour)
minute <- formatC(minute, width=2, flag="0")
minute <- as.character(minute)

onetime_flag <- 1		# do this once in the first main loop (this flag is always set to 1)

###############################################################
# FILE
###############################################################
filespace <- paste(data_filespace, sensor, "/", year, month, "/", year, month, day, "/", year, month, day, hour, minute, "/", sep="")
result_filespace <- paste(filespace, name_result, sep="")

# create directories
if(!file.exists(result_filespace)){
	dir.create(result_filespace)
	dir.create(paste(result_filespace, "unique_host/", sep=""))
	dir.create(paste(result_filespace, "density/", sep=""))
#	dir.create(paste(result_filespace, "eps/", sep=""))
	dir.create(paste(result_filespace, "sample_covariance_matrix/", sep=""))
	if(gml_flag == 1){
		dir.create(paste(result_filespace, "graph_glasso/", sep=""))
	}
}

input_filespace <- paste(filespace, "input/", sep="")
UH_filespace <- paste(result_filespace,"unique_host/",sep="")
SCM_filespace <- paste(result_filespace,"sample_covariance_matrix/",sep="")

filedate <- paste(year, month, day, hour, minute, sep="")
if(octet == 2){
	filedate_txt <- paste(year, month, day, hour, minute, "_ver", ver, ".txt", sep="")
}else if(octet == 4){
	filedate_txt <- paste(year, month, day, hour, minute, "_ver", ver, "_octet4.txt", sep="")
}

write.table(NULL, paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=F)
write.table(paste("時間帯", "選ばれたホストの数", "ホスト全体の数", sep= "		"), paste(result_filespace, "num_of_host.txt", sep=""), quote=F,col.names=F,row.names=F,append=F)

#################################
# MAKE_RANDOM_SAMPLE_FUNCTION (parameter: INPUT_DATA)
#################################
 make_random_sample <- function(INPUT_DATA){
	if(length(which(unique_host == INPUT_DATA[2])) != 0){							# If there is a match between INPUT_DATA[2] and unique_host
		if(length(which(INPUT_DATA[2] == colnames(random_sample))) == 0){				# If there is no match between INPUT_DATA[2] and random_sample column name
			random_sample <<- cbind(random_sample, numeric(num_of_sample))					# Add column 0 to random_sample
			random_sample[count_make_RS, ncol(random_sample)] <<- 1							# Assign 1 to random_sample's count_make_RS row, last column
			colnames(random_sample)[ncol(random_sample)] <<- as.character(INPUT_DATA[2])	# Assign "INPUT_DATA[2]" to the end of random_sample's column name
		}else{																			# If INPUT_DATA[2] and random_sample's column name match
			random_sample[count_make_RS, which(INPUT_DATA[2] == colnames(random_sample))] <<- random_sample[count_make_RS, which(INPUT_DATA[2] == colnames(random_sample))] + 1		# Add 1 to random_sample's count_make_RS row (matched by INPUT_DATA[2] and random_sample's column name) column
		}
	}
}

# Redefine the function for finding the sample variance, since var() is a function for finding the unbiased sample variance
variance <- function(x) var(x)*(length(x)-1)/length(x)
interval <- set_interval * num_of_sample


###############################################################
# Log
###############################################################
variable_name <- c("sensor <-", "data_filespace <-", "year <-", "month <-", "day <-", "hour <-", "minute <-", "rho <-", "rho_update <-", "rho_stop <-", "octet <-", "host_upper_bound <-", "set_interval <-", "num_of_sample <-", "protocol <-", "name_result <-", "RT_flag <- ", "gml_flag <-")
variable_value <- c(sensor, data_filespace, year, month, day, hour, minute, rho, rho_update, rho_stop, octet, host_upper_bound, set_interval, num_of_sample, protocol, name_result, RT_flag, gml_flag)
(variable <- data.frame(NAME = variable_name, VALUE = variable_value))

write.table(variable, paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)			# parameters log
write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)
write.table(paste("Moment","User ", "System", "Progress", sep="		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)		# run-time record

# initial run time
first_proc_time <- t(as.matrix(proc.time()))
first_proc_time <- round(first_proc_time, digits = 3)
write.table(paste("Start ", first_proc_time[1], first_proc_time[2], first_proc_time[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)



###############################################################
# DATA_READING
###############################################################
data_tmp <- read.table(paste(input_filespace, filedate_txt, sep=""))

if(is.vector(data_tmp)){
	data_tmp <- t(as.matrix(data_tmp))
}



###############################################################
# data preprocessing
###############################################################
# 時間Unix Time
data_time <- data_tmp[,1]							# data_tmpの1列目（時刻)をdata_timeにnumericとして代入, data_tmp[1]だとlist
data_time <- floor(data_time)						# data_time以上でない最大の整数を返す．(小数点以下切り捨て), data_time更新
# 送信元ホスト・ポート
source_host_tmp <- data_tmp[,3]						# data_tmpの3列目（送信元）をsource_host_tmpに代入, data_tmp[,3]はnumericなため
source_host_tmp <-as.character(source_host_tmp)		# 文字列として代入
# 送信先ホスト・ポート
dst_host_tmp <- data_tmp[,5]						# data_tmpの5列目(送信先darknet IP)をdst_host_tmpに代入, data_tmp[,5]はnumericなため
dst_host_tmp <- as.character(dst_host_tmp)			# 文字列として代入

############################################################################
# source_host_tmpを文字ピリオッド "." で分割する(strsplit)
# リストの要素を端からベクトルとして結合して 1 つのベクトルとしてまとめる関数(unlist)
# 5行の行列を作成,左の列から順に (上から下へ) 埋められる．(matrix)
# 行列を転置する．(t)
#  ex)	[,1] [,2] [,3] [,4]   [,5]
#  [1,]	"133" "5" "16" "11" "TCP port番号"
source_sepa <- t(matrix(unlist(strsplit(source_host_tmp, "\\.")), nrow=5))
dst_sepa <- t(matrix(unlist(strsplit(dst_host_tmp, "\\.")), nrow=5))
############################################################################

if(octet == 2){
	source_host <- paste(source_sepa[,1], ".", source_sepa[,2], sep="")		# 送信元IPアドレスをsource_hostに代入 ex) 133.5


	source_host_4 <- paste(source_sepa[,1], ".", source_sepa[,2], ".", source_sepa[,3], ".", source_sepa[,4], sep="")
	source_host_4 <- unique(source_host_4)
	write.table(source_host_4, paste(UH_filespace,"another_octet_UH.txt", sep=""),quote=F,col.names=F,row.names=F,append=F)
	other_num_of_host <- length(source_host_4)
	octet_other <- 4

	try(rm(source_host_4), silent=TRUE)

}else if(octet == 3){
	source_host <- paste(source_sepa[,1], ".", source_sepa[,2], ".", source_sepa[,3], sep="")		# 送信元IPアドレスをsource_hostに代入 ex) 133.5.16
}else if(octet == 4){
	source_host <- paste(source_sepa[,1], ".", source_sepa[,2], ".", source_sepa[,3], ".", source_sepa[,4], sep="")		# 送信元IPアドレスをsource_hostに代入 ex) 133.5.16.11

	source_host_2 <- paste(source_sepa[,1], ".", source_sepa[,2], sep="")
	source_host_2 <- unique(source_host_2)
	write.table(source_host_2, paste(UH_filespace,"another_octet_UH.txt", sep=""),quote=F,col.names=F,row.names=F,append=F)
	other_num_of_host <- length(source_host_2)
	octet_other <- 2

	try(rm(source_host_2), silent=TRUE)
}

data_time_source_dstport <- cbind(data_time,source_host,dst_sepa[,5])					# 列ベクトル単位で結合

count_rho_loop <- 0			# rごとのループ回数
num_of_host <- NULL
count_main_loop <- -1		# mainループ回数を初期に-1を入れる

# Data Readingの実行時間
data_reading_proc_time <- t(as.matrix(proc.time()))
data_reading_proc_time_sub <- data_reading_proc_time - first_proc_time
data_reading_proc_time_sub <- round(data_reading_proc_time_sub, digits = 3)
write.table(paste("Data", data_reading_proc_time_sub[1], data_reading_proc_time_sub[2], data_reading_proc_time_sub[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

try(rm(data_tmp, source_host_tmp, dst_host_tmp, source_sepa, dst_sepa, source_host, data_time, variable, variable_name, variable_value), silent=TRUE)



###############################################################
# DATE_LOOP
###############################################################
repeat {
	rho <- rho + rho_update		# 使用するrの値の間隔を決定
	count_rho_loop <- count_rho_loop + 1

# 	write.table(NULL, paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)
	if(RT_flag == 1){
		write.table(NULL, paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)
	}


	###############################################################
	# MAIN_LOOP
	###############################################################
	while(length(data_time_source_dstport) != 0 && count_main_loop != length(num_of_host[,1])){			# dataの長さが0かmainループが時間帯の数分回らない限り繰り返す
		if(count_rho_loop == 1){		# rhoループ一回目だけホストを計算する
			#root_time <- as.numeric(data_time_source_dstport[1,1])%/%set_interval		# dataの(1,1)要素 一番上に有る時刻を100で割った整数商を計算しroot_timeに代入
			root_time <- as.numeric(as.POSIXlt(paste(year, "-", month, "-", day, " ", hour, ":", minute, ":", "00 JST",sep=""), tz="Japan"))%/%set_interval
			if(is.matrix(data_time_source_dstport)){
				###############################################################
				# SUB_DATA_READING
				# sub_data_time_source_dstport:set_interval*num_of_sample (period data)
				# data_time_source_dstport: data excluding 'sub_data_time_source_dstport'
				###############################################################
				sub_data_time_source_dstport <- data_time_source_dstport[as.numeric(data_time_source_dstport[,1])%/%set_interval - root_time < num_of_sample,]

			}else if(is.vector(data_time_source_dstport)){
				data_time_source_dstport <- NULL

				if(RT_flag == 0){
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}else{
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}
				num_of_host<-matrix(c(root_time*set_interval,1),1,2)

				next
			}

			if(is.matrix(sub_data_time_source_dstport)){
				data_time_source_dstport <- data_time_source_dstport[-(1:nrow(sub_data_time_source_dstport)),]
			}else if(is.vector(sub_data_time_source_dstport)){
				data_time_source_dstport <- NULL

				if(RT_flag == 0){
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}else{
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}
				num_of_host<-matrix(c(root_time*set_interval,1),1,2)

				next
			}

			if(is.vector(sub_data_time_source_dstport)){
				sub_data_time_source_dstport <- t(as.matrix(sub_data_time_source_dstport))
			}

			###############################################################
			# HOST_MAKING
			# entire_unique_host : IP addresses in 'sub_data_time_source_dstport' are assigned by removing duplicates
			# unique_host : If the number of hosts exceeds host_upper_bound, randomly select 'host_upper_bound.'
			###############################################################
			entire_unique_host <- unique(sub_data_time_source_dstport[,2])

			# If the number of hosts exceeds the threshold (host_upper_bound), randomly select 'host_upper_bound'
			if(length(entire_unique_host) > host_upper_bound){
				write.table(entire_unique_host, paste(UH_filespace,"entire_unique_host_", root_time*set_interval,".txt",sep=""),quote=F,col.names=F,row.names=F,append=F)	# write entire host

				choice <- sample(1:length(entire_unique_host), host_upper_bound)
				choice <- sort(choice)
				unique_host <- entire_unique_host[choice]

				try(rm(choice), silent=TRUE)
			}else{
				unique_host <- entire_unique_host
			}

			write.table(unique_host, paste(UH_filespace, root_time*set_interval,".txt", sep=""),quote=F,col.names=F,row.names=F,append=F)		# 各時間帯のホストアドレス記録

			# 各時間帯の選ばれたホストの数とホスト全体の数を記録
			num_of_unique_host <- length(unique_host)
			num_of_entire_host <- length(entire_unique_host)
			# 時間帯ごとのホスト数と全体ホスト数を記録
			time_num_of_host <- t(as.matrix(c(root_time*set_interval, num_of_unique_host, num_of_entire_host)))
			num_of_host <- rbind(num_of_host, time_num_of_host)


			###############################################################
			# Count the number of packets of 'unique_host' from 'make_random_sample' and assign it to 'random_sample'
			###############################################################
			count_make_RS <- 1
			random_sample <- matrix(0, num_of_sample, 1)		# random_sampleにnum_of_sample行1列の全要素に0を代入
			tmp_root_time <- root_time
			name_root_time <- NULL

			while((tmp_root_time - root_time) != num_of_sample){		# tmp_root_time - root_timeがnum_of_sampleと等しくなるまで繰り返す（num_of_sample回繰り返す）
				same_time_data <- sub_data_time_source_dstport[as.numeric(sub_data_time_source_dstport[,1])%/%set_interval == tmp_root_time,]		# sub_dataでtmp_root_timeからset_interval分同じ時間のデータをsame_time_dataに代入
				if(is.matrix(same_time_data) == FALSE){		# same_time_dataが行列ではない場合
					same_time_data <- t(as.matrix(same_time_data))	# 行列にする
				}

				if(length(entire_unique_host) > host_upper_bound){
					same_time_data <- same_time_data[is.element(same_time_data[,2], unique_host),]		# same_time_data[,2]中の各要素は集合unique_hostに含まれるか否か, 含まれたらそのsame_time_dataをsame_time_dataに代入（ランダムに選んだunique_hostだけのデータを取り出す）
				}

				random_sample[count_make_RS, ] <- 0

				# same_time_dataが1行だけの時vectorになってしまうためもう一度matrixにする
				if(is.matrix(same_time_data) == FALSE){		# same_time_dataが行列ではない場合
					same_time_data <- t(as.matrix(same_time_data))	# 行列にする
				}

				if(nrow(same_time_data) != 0){						# same_time_dataの行数が0でなければ
					apply(same_time_data, 1, make_random_sample)	# same_time_dataの行に関してmake_random_sample関数に適用する
				}

				if(count_make_RS == num_of_sample){
					count_make_RS = 0
				}
				name_root_time <- c(name_root_time, tmp_root_time*set_interval)
				tmp_root_time = tmp_root_time + 1
				count_make_RS = count_make_RS + 1
			}

			rownames(random_sample) <- name_root_time
			random_sample <- random_sample[,-1]						# 1列目のXはNAのため削除

			write.table(random_sample, paste(result_filespace, "count_data.csv", sep=""), sep=",", quote=F,col.names=F,row.names=T,append=F)
			#write.table(colnames(random_sample), paste(result_filespace, "data_srcIP.csv", sep=""), sep=",", quote=F,col.names=F,row.names=F,append=F)


			##########################
			# Sample covariance matrix calculation:
			# Since the number of packets is always positive and does not follow a normal distribution, the logarithm is added to make the distribution look like a normal distribution.
			##########################
			random_sample[random_sample == 0] <- 0.1			# random_sampleの要素で0のものは全て0.1にする
			random_sample <- log(random_sample)					# random_sampleの全要素にlog計算する(log0.1 = -2.3, log1 = 0, 底は自然対数e)
			sample_covariance_matrix <- var(random_sample) * (num_of_sample-1)/num_of_sample	# random_sampleの不偏標本分散を求めsample_covariance_matrixに代入
			########################


			write.table(sample_covariance_matrix, paste(SCM_filespace, root_time*set_interval,".txt", sep=""), quote=F,col.names=F,row.names=F,append=F)		# 各時間帯の標本共分散行列記録

			if(length(sample_covariance_matrix) == 1){
				# Realtime flag
				if(RT_flag == 0){
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}else{
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}
				next
			}

			try(rm(entire_unique_host, same_time_data, tmp_root_time, sub_data_time_source_dstport, count_make_RS, random_sample), silent=TRUE)


		}else{		# rhoループ1回目以降
			count_main_loop <- count_main_loop + 1

			if(class(try(read.table(paste(UH_filespace, root_time*set_interval,".txt",sep="")), silent=TRUE)) == "try-error"){
				if(RT_flag == 0){
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}else{
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}
				next
			}
			# read unique host
			unique_host <- read.table(paste(UH_filespace, root_time*set_interval,".txt",sep=""))
			unique_host <- as.matrix(unique_host)

			# read SCM
			sample_covariance_matrix <- read.table(paste(SCM_filespace, root_time*set_interval,".txt",sep=""))
			sample_covariance_matrix <- as.matrix(sample_covariance_matrix)
			colnames(sample_covariance_matrix) <- unique_host
			rownames(sample_covariance_matrix) <- unique_host

			if(nrow(sample_covariance_matrix) == 1){
				# Realtime flag
				if(RT_flag == 0){
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}else{
					write.table(paste(root_time*set_interval, 0, 1, 0, sep=" "),paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
				}
				next
			}
		}


		#########GLASSO_CALCULATE###################
		estimated_precision_matrix <- glasso(sample_covariance_matrix, rho)[[2]] 	#[[1]]:covariance  [[2]]:precision
		##########################################

		#########edge_calculate_glasso###################
		rownames(estimated_precision_matrix) <- rownames(sample_covariance_matrix)
		colnames(estimated_precision_matrix) <- colnames(sample_covariance_matrix)

		diag(estimated_precision_matrix) <- 0						# Diagonal component of 'estimated_precision_matrix' is set to 0

		node <- nrow(estimated_precision_matrix) 			# number of nodes
		edge <- sum(estimated_precision_matrix != 0)/2  	# number of edges
		graph_density <- edge/(node*(node-1)) 				# garph density

		#write.table(estimated_precision_matrix, paste(result_filespace,"estimated_precision_matrix/rho_", rho, "/", root_time*set_interval, ".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)

		# Realtime flag
		if(RT_flag == 0){
			write.table(paste(root_time*set_interval, graph_density, node, edge, sep=" "),paste(result_filespace,"density/density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
		}else{
			write.table(paste(root_time*set_interval, graph_density, node, edge, sep=" "),paste(result_filespace,"density/RT_density_",rho,".txt",sep=""),quote=F,col.names=F,row.names=F,append=T)
		}

		##########################################
		# Write GML Glasso Graph
		##########################################
		if(gml_flag == 1){
			y <- which(estimated_precision_matrix != 0, arr.ind=TRUE)
			estimated_precision_matrix[y] <- 1

			graph_gml <- graph.adjacency(estimated_precision_matrix)
			graph_gml <- as.undirected(graph_gml)

			write.graph(graph_gml, paste(result_filespace, "/graph_glasso/", root_time*set_interval,"_", rho, ".gml",sep=""), "gml")
		}
	}

	# Do it only once at the end of the first main loop
	if(onetime_flag == 1){
		write.table(paste("Time", "unique_host", "Entire host", sep="	"), paste(result_filespace, "num_of_host.txt", sep=""), quote=F,col.names=F,row.names=F,append=F)
		write.table(num_of_host, paste(result_filespace, "num_of_host.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)		# 各時間帯のホスト数記録

		data_time_source_dstport <- 1		# data_time_source_dstportを1にすることでmainループを1回目以降も回すことができる

		try(rm(num_of_unique_host, num_of_entire_host, time_num_of_host), silent=TRUE)
	}

	onetime_flag <- 0
	count_main_loop <- 0		# Initialized to 0 after the main loop ends

	gc()
	gc()
	invisible(replicate(20, gc()))


	# rhoごとの実行時間
	rho_proc_time <- t(as.matrix(proc.time()))
	if(count_rho_loop == 1){
		rho_proc_time_sub <- rho_proc_time - data_reading_proc_time
		pre_rho_proc_time <- rho_proc_time
	}else{
		rho_proc_time_sub <- rho_proc_time - pre_rho_proc_time
		pre_rho_proc_time <- rho_proc_time
	}
	rho_log <- paste("r= ", rho, sep="")
	rho_proc_time_sub <- round(rho_proc_time_sub, digits = 3)
	write.table(paste(rho_log, rho_proc_time_sub[1], rho_proc_time_sub[2], rho_proc_time_sub[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)


	if(rho < rho_stop){		# まだrhoが続く場合
		next				# 次のMainループに行く
	}else{					# Mainループが終わる場合
		# 終わりの合計実行時間
		end_proc_time <- t(as.matrix(proc.time()))
		end_proc_time <- round(end_proc_time, digits = 3)
		write.table(paste("Total ", end_proc_time[1], end_proc_time[2], end_proc_time[3], sep= " 		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

		# LOG
		if(octet == 2){
			LOG_file <- paste(data_filespace, "LOG/", year, month, "_", sensor, ".log", sep="")
		}else if(octet == 4){
			LOG_file <- paste(data_filespace, "LOG/", year, month, "_octet4_", sensor, ".log", sep="")
		}
		if(octet == 2){
			host_percent <- format(num_of_host[,3]/other_num_of_host, digits=3)
		}else if(octet == 4){
			host_percent <- format(other_num_of_host/num_of_host[,3], digits=3)
		}
		LOG_column <- paste("date,runtime(density),host_upper_bound,octet(host,entire host),octet_other(host, percent)", sep="")
		LOG_info <- paste(filedate,",",end_proc_time[3],",",host_upper_bound,",",octet,"(",num_of_host[,2],",",num_of_host[,3],"),", octet_other, "(",other_num_of_host,",",host_percent,")", sep="")

		if(!file.exists(paste(data_filespace, "LOG/", sep=""))){
			dir.create(paste(data_filespace, "LOG/", sep=""))
		}
		if(file.exists(LOG_file)){
			write.table(LOG_info, LOG_file, quote=F,col.names=F,row.names=F,append=T)
		}else{
			write.table(LOG_column, LOG_file, quote=F,col.names=F,row.names=F,append=T)
			write.table(LOG_info, LOG_file, quote=F,col.names=F,row.names=F,append=T)
		}


		# 空白行
		write.table(paste(" ", " ", sep="\n"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)

		# 時間帯の数とホストの平均数記録
		write.table(paste("num of periods", nrow(num_of_host) - 1, sep="		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)
		write.table(paste("mean of hosts", mean(num_of_host[,2]), sep="		"), paste(result_filespace, "Log.txt", sep=""), quote=F,col.names=F,row.names=F,append=T)


		break		# 終了
	}

}
