#!/bin/bash

START="2019-09-01 00:00:00" # !!!
END="2019-09-01 01:00:00"   # !!!


engine="NMF"     # !!! GLASSO, NMF, NTD
ID="XXX" # !!!
ver=1   # !!! Only GLASSO needs to specify ver.
nextwindow_interval=600   # !!! GLASSO is fixed at 600; NMF and NTD can be set freely (e.g., 900 at 15-minute intervals, 1800 at 30-minute intervals, etc.)
NMF_nonport=1  # !!! For NMF, 0 if port spatial feature synchronization is computed, 1 if not computed; no setting is required except for NMF

dmp_filespace="XXX" # !!!
data_filespace="XXX"
program_filespace="XXX"
dmp_interval=600
dmp_check=50

START=$(TZ=Asia/Tokyo date --date "${START}" +"%s")
END=$(TZ=Asia/Tokyo date --date "${END}" +"%s")

# input_interval
if test ${engine} = "GLASSO"
then
    input_interval=600
elif test ${engine} = "NMF" -o ${engine} = "NTD"
then
    input_interval=1800
else
    echo "input_interval: ${engine} error"
fi

# move to next window func.
nextwindow() {
    START=$(($1+$2))
    if test ${START} -gt ${END}
    then
        check_nextwindow=1
    else
        check_nextwindow=0
    fi
}

while true
do
    Before_UNIX=$(TZ=Asia/Tokyo date +"%s")

    START_UNIXTIME=${START}
    END_UNIXTIME=$((${START_UNIXTIME}+${input_interval}))

    START_YEAR=$(TZ=Asia/Tokyo date --date "@${START_UNIXTIME}" +"%Y")
    START_MONTH=$(TZ=Asia/Tokyo date --date "@${START_UNIXTIME}" +"%m")
    START_DAY=$(TZ=Asia/Tokyo date --date "@${START_UNIXTIME}" +"%d")
    START_HOUR=$(TZ=Asia/Tokyo date --date "@${START_UNIXTIME}" +"%H")
    START_MINUTE=$(TZ=Asia/Tokyo date --date "@${START_UNIXTIME}" +"%M")

    START_TIME=${START_YEAR}${START_MONTH}${START_DAY}${START_HOUR}${START_MINUTE}

    # input/output file space
    output_filespace="${data_filespace}sensor${ID}/${START_YEAR}${START_MONTH}/${START_YEAR}${START_MONTH}${START_DAY}/${START_TIME}/"
    input_filespace="${output_filespace}input/"
    if test -e ${input_filespace}
    then
       :
    else
       mkdir -p ${input_filespace}
    fi
    inputfile_txt="${input_filespace}${START_TIME}_ver${ver}.txt"


    # inputfile_dmp (mergecap)
    if test ${engine} = "GLASSO"
    then

        inputfile_dmp="${dmp_filespace}${START_YEAR}${START_MONTH}/sensor${ID}_${START_TIME}.dmp"

        # dmpfile existence check
        if test -e ${inputfile_dmp}
        then
            :
        else
            echo "${START_TIME}: dmpfile doen't exist."
            rm -rf ${input_filespace}

            nextwindow ${START_UNIXTIME} ${nextwindow_interval}
            if test ${check_nextwindow} -eq 1
            then
                break
            fi
            continue
        fi

        # input file length check
        inputfile_dmp_check=$(TZ=Asia/Tokyo capinfos -u ${inputfile_dmp} | sed -e '1d' | awk '{print $3}' | awk -F. '{print $1}')
        if test ${inputfile_dmp_check} = "n/a"
        then
            inputfile_dmp_check=0
        fi

        if test ${inputfile_dmp_check} -lt $((${input_interval}-${dmp_check}))
        then
            echo "${START_TIME}: the length is less than $((${input_interval}-${dmp_check})) sec or n/a."
            rm -rf ${input_filespace}
            nextwindow ${START_UNIXTIME} ${nextwindow_interval}
            if test ${check_nextwindow} -eq 1
            then
                break
            fi
            continue
        fi

        # tcpdump -> inputfile_txt
        tcpdump -nnttq -r ${inputfile_dmp} | egrep  ^[0-9.]*\ \I\P\ [0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+\ \>\ [0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9.]+ | awk -F ' ' '{print $1,$2,$3,$4,$5}' > ${inputfile_txt}
        echo "finish making ${inputfile_txt}"


    elif test ${engine} = "NMF" -o ${engine} = "NTD"
    then
        dmpfile2_YYMM=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}+${dmp_interval}))" +"%Y%m")
        dmpfile2_TIME=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}+${dmp_interval}))" +"%Y%m%d%H%M")
        dmpfile3_YYMM=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}+${dmp_interval}*2))" +"%Y%m")
        dmpfile3_TIME=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}+${dmp_interval}*2))" +"%Y%m%d%H%M")

        dmpfile_1="${dmp_filespace}${START_YEAR}${START_MONTH}/sensor${ID}_${START_TIME}.dmp"
        dmpfile_2="${dmp_filespace}${dmpfile2_YYMM}/sensor${ID}_${dmpfile2_TIME}.dmp"
        dmpfile_3="${dmp_filespace}${dmpfile3_YYMM}/sensor${ID}_${dmpfile3_TIME}.dmp"
        inputfile_dmp="${input_filespace}${START_TIME}_ver${ver}.dmp"

        # dmpfile existence check
        if test -e ${dmpfile_1} -a -e ${dmpfile_2} -a -e ${dmpfile_3}
        then
            :
        else
            echo "${START_TIME}: dmpfile doen't exist."
            rm -rf ${input_filespace}
            nextwindow ${START_UNIXTIME} ${nextwindow_interval}
            if test ${check_nextwindow} -eq 1
            then
                break
            fi
            continue
        fi

        # mergecap
        mergecap ${dmpfile_1} ${dmpfile_2} ${dmpfile_3} -w ${inputfile_dmp}

        # input file length check
        inputfile_dmp_check=$(TZ=Asia/Tokyo capinfos -u ${inputfile_dmp} | sed -e '1d' | awk '{print $3}' | awk -F. '{print $1}')
        if test ${inputfile_dmp_check} = "n/a"
        then
            inputfile_dmp_check=0
        fi

        if test ${inputfile_dmp_check} -lt $((${input_interval}-${dmp_check}*3))
        then
            echo "${START_TIME}: the length is less than $((${input_interval}-${dmp_check}*3)) sec or n/a."
            rm -rf ${input_filespace}
            nextwindow ${START_UNIXTIME} ${nextwindow_interval}
            if test ${check_nextwindow} -eq 1
            then
                break
            fi
            continue
        fi

        if test ${engine} = "NMF"
        then
            # tcpdump -> inputfile_txt
            tcpdump -nnttq -r ${inputfile_dmp} | egrep  ^[0-9.]*\ \I\P\ [0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+\ \>\ [0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9]+[\.][0-9.]+ | awk -F ' ' '{print $1,$2,$3,$4,$5}' > ${inputfile_txt}
            rm ${inputfile_dmp}
            echo "finish making ${inputfile_txt}"
        elif test ${engine} = "NTD"
        then
            mv ${inputfile_dmp} "${input_filespace}sensor${ID}_${START_TIME}.dmp"
            echo "finish making ${inputfile_dmp}"
        fi

    else
        echo "inputfile_dmp: ${engine} error"
    fi



	###################
	# engine script
	###################
	if test ${engine} = "GLASSO"   # GLASSO
    then
        ###################
    	# Dark-GLASSO
    	###################
        # パラメータ
        theta=0.98
        # density file num (rhoの範囲*2で決まる)
        file_num=6


        # Read the time when the glasso program was last executed
        if test -e ${data_filespace}sensor${ID}/glasso_latest_lasttime_${input_interval}_ver${ver}.log
        then
            glasso_latest_lasttime=$(cat ${data_filespace}sensor${ID}/glasso_latest_lasttime_${input_interval}_ver${ver}.log)

            # copy density_old
            Lasttime_YEAR=$(TZ=Asia/Tokyo date --date "@${glasso_latest_lasttime}" +"%Y")
            Lasttime_MONTH=$(TZ=Asia/Tokyo date --date "@${glasso_latest_lasttime}" +"%m")
            Lasttime_DAY=$(TZ=Asia/Tokyo date --date "@${glasso_latest_lasttime}" +"%d")
            Lasttime_HOUR=$(TZ=Asia/Tokyo date --date "@${glasso_latest_lasttime}" +"%H")
            Lasttime_MINUTE=$(TZ=Asia/Tokyo date --date "@${glasso_latest_lasttime}" +"%M")
            Lasttime_TIME=${Lasttime_YEAR}${Lasttime_MONTH}${Lasttime_DAY}${Lasttime_HOUR}${Lasttime_MINUTE}
            if test -e ${output_filespace}density_old_${theta}/
            then
               :
            else
               mkdir -p ${output_filespace}density_old_${theta}/
            fi
            cp -r ${data_filespace}sensor${ID}/${Lasttime_YEAR}${Lasttime_MONTH}/${Lasttime_YEAR}${Lasttime_MONTH}${Lasttime_DAY}/${Lasttime_TIME}/result_M12/density_${theta}/* ${output_filespace}density_old_${theta}/
        else
            # Update last processing time (UNIX time)
            echo ${START_UNIXTIME} > ${data_filespace}sensor${ID}/glasso_latest_lasttime_${input_interval}_ver${ver}.log
        fi

        # Delete colon(:) in inputfile
        cat ${inputfile_txt} | tr -d ":" > ${inputfile_txt}tmp
        mv ${inputfile_txt}tmp ${inputfile_txt}

        R --vanilla --slave --args "sensor${ID}" ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 0.3 0.1 0.8 2 1000 50 12 "TCP" "result_M12/" 1 0 ${ver} < ${program_filespace}${engine}/online_density.r

        rm -rf ${output_filespace}result_M12/sample_covariance_matrix
        # Check the number of density files
        density_num=$(ls -U ${output_filespace}result_M12/density/ | wc -l)
        # Check if there is a file with size 0 in RT_density
        zero_file=0
        for file in `find ${output_filespace}result_M12/density/RT* -maxdepth 1 -type f`
        do
        	if [ ! -s ${file} ]
        	then
        		zero_file=1
        	fi
        done

        # portinfo
        if test ${density_num} -eq ${file_num} -a ${zero_file} -eq 0
        then
        	# If no error, run portinfo
        	R --vanilla --slave --args "sensor${ID}" ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 0.3 0.1 0.8 2 10 20 70 ${theta} 50 12 "TCP" "result_M12/" 1 1 ${ver} "alert_json_sensor${ID}_T98_${START_YEAR}${START_MONTH}.txt" 432 < ${program_filespace}${engine}/online_portinfo.r

        	# Update last processing time (UNIX time)
        	echo ${START_UNIXTIME} > ${data_filespace}sensor${ID}/glasso_latest_lasttime_${input_interval}_ver${ver}.log
        else
        	# If there is an error
        	echo "${START_TIME}: density file error"
        fi

        # Delete input data 6 days old
        sixdaysago_YEAR=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}-518400))" +"%Y")
        sixdaysago_MONTH=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}-518400))" +"%m")
        sixdaysago_DAY=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}-518400))" +"%d")
        sixdaysago_HOUR=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}-518400))" +"%H")
        sixdaysago_MINUTE=$(TZ=Asia/Tokyo date --date "@$((${START_UNIXTIME}-518400))" +"%M")

        sixdaysago_TIME=${sixdaysago_YEAR}${sixdaysago_MONTH}${sixdaysago_DAY}${sixdaysago_HOUR}${sixdaysago_MINUTE}

        # input file space
        sixdaysago_input_filespace="${data_filespace}sensor${ID}/${sixdaysago_YEAR}${sixdaysago_MONTH}/${sixdaysago_YEAR}${sixdaysago_MONTH}${sixdaysago_DAY}/${sixdaysago_TIME}/input/"
        if test -e ${sixdaysago_input_filespace}
        then
            rm -rf ${sixdaysago_input_filespace}   # remove inputfile_txt
        fi
    elif test ${engine} = "NMF"    # NMF
    then
        ###################
        # Dark-NMF
        ###################
        if test ${NMF_nonport} -eq 0
        then
            # With port spatial feature synchronization calculation
            if test -e "${data_filespace}sensor${ID}/Anomaly_dstPort_list/"
            then
            :
            else
            mkdir -p "${data_filespace}sensor${ID}/Anomaly_dstPort_list/"
            fi
            portlist_file="${data_filespace}sensor${ID}/Anomaly_dstPort_list/${START_YEAR}${START_MONTH}_Anomaly_dstPort_list_ver${ver}.txt"
            echo "0" > ${portlist_file}

            # program/DarkNMF.r (host spatial feature synchronization calculation)
            R --vanilla --slave --args ${ID} ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 2 30 1 ${ver} 1 1 0 1 10 3000 < ${program_filespace}${engine}/DarkNMF.r
            R --vanilla --slave --args ${ID} ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 2 ${ver} 1 10 0 50 2 30 2 0 < ${program_filespace}${engine}/DarkNMF_alertonly.r # 16(octet2) 0 50 2

            portlist="$(cat ${portlist_file})"

            if test ${portlist} -eq 0 -o -z ${portlist}
            then
                echo -e "Dark-NMF: anomaly dstPort list = NULL"

                rm -rf ${output_filespace}result*/NMF_csv/
                rm -rf ${output_filespace}result*/prepro_${START_TIME}.txt
                rm -rf ${input_filespace}   # remove inputfile_txt

                # check Processing Time
                After_UNIX=$(TZ=Asia/Tokyo date +"%s")
                GAP=$((${After_UNIX}-${Before_UNIX}))
                echo -e "${START_TIME}: done"
                echo -e "Processing Time: ${GAP} sec \n\n"

                nextwindow ${START_UNIXTIME} ${nextwindow_interval}
                if test ${check_nextwindow} -eq 1
                then
                    break
                fi
                continue
            else
                echo "Dark-NMF: anomaly dstPort list = ${portlist}"
                # program/DarkNMF-port.r (port spatial feature synchronization calculation)
                R --vanilla --slave --args ${ID} ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 4 30 1 ${ver} 1 1 1 0 1 10 3000 < ${program_filespace}${engine}/DarkNMF-port.r
                R --vanilla --slave --args ${ID} ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 4 ${ver} 1 1 10 1 98 2 ${portlist} 0 < ${program_filespace}${engine}/DarkNMF-port_alertonly.r # 98
            fi

            rm -rf ${output_filespace}result*/NMF_csv/
            rm -rf ${output_filespace}result*/prepro_${START_TIME}.txt
            rm -rf ${output_filespace}port_result*/NMF_csv/
            rm -rf ${output_filespace}port_result*/prepro_${START_TIME}.txt
            rm -rf ${input_filespace}   # remove inputfile_txt
        elif test ${NMF_nonport} -eq 1
        then
            # Without port spatial feature synchronization calculation

            # program/DarkNMF.r (host spatial feature synchronization calculation)
            R --vanilla --slave --args ${ID} ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 2 30 1 ${ver} 1 1 0 1 10 3000 < ${program_filespace}${engine}/DarkNMF.r
            R --vanilla --slave --args ${ID} ${data_filespace} ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} 2 ${ver} 1 10 1 50 2 30 2 0 < ${program_filespace}${engine}/DarkNMF_alertonly.r # 16(octet2) 0 50 2

            rm -rf ${output_filespace}result*/NMF_csv/
            rm -rf ${output_filespace}result*/prepro_${START_TIME}.txt
            rm -rf ${input_filespace}   # remove inputfile_txt
        fi
    elif test ${engine} = "NTD"    # NTD
    then
        ###################
    	# Dark-NTD
    	###################
    	ITERATION_NUM=4
    	for ITERATION in $(seq 1 ${ITERATION_NUM})
    	do
             R --vanilla --slave --args ${START_YEAR} ${START_MONTH} ${START_DAY} ${START_HOUR} ${START_MINUTE} "sensor${ID}" "${program_filespace}${engine}/" ${input_filespace} "${data_filespace}output/" "${data_filespace}alert/" 5 5 ${ITERATION} < ${program_filespace}${engine}/online_script.R
        done
        rm -rf ${input_filespace}   # remove inputfile_txt
        rmdir ${output_filespace}
    fi

    # check Processing Time
    After_UNIX=$(TZ=Asia/Tokyo date +"%s")
    GAP=$((${After_UNIX}-${Before_UNIX}))
    echo -e "${START_TIME}: done"
    echo -e "Processing Time: ${GAP} sec \n\n"

    nextwindow ${START_UNIXTIME} ${nextwindow_interval}
    if test ${check_nextwindow} -eq 1
    then
        break
    fi

done
