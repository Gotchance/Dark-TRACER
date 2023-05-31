
# Overview

This repository contains the R implementation of Dark-TRACER, a framework for early anomaly detection of malware activities. It was presented in the following paper. Please refer to the PDF and slides for details. Also, the dataset used in the paper is publicly available.

> [**C. Han**](https://hackmd.io/@gotchance/BJ7ACLZ0E), J. Takeuchi, T. Takahashi, and D. Inoue, ''*Dark-TRACER*: Early Detection Framework for Malware Activity Based on Anomalous Spatiotemporal Patterns,'' *IEEE ACCESS*, 2022. [\[DOI\]](https://doi.org/10.1109/ACCESS.2022.3145966) [\[PDF\]](https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=9690867) [\[Related Slides\]](https://drive.google.com/file/d/1IlOar4ARu-olK4k-b5g6WWC4Id2abfnB/view?usp=sharing) [\[Datasets\]](https://csdataset.nict.go.jp/darknet-2022/) [\[Codes\]](https://github.com/Gotchance/Dark-TRACER)

<br>

# What is Dark-TRACER?
Dark-TRACER is a framework for early anomaly detection of malware activities by estimating the synchronization of spatiotemporal patterns observed in darknet traffic leveraging three machine learning methods. It consists of the following three modules
- Dark-GLASSO: utilizes the Graphical Lasso, a sparse structure learning algorithm
- Dark-NMF: utilizes the non-negative matrix factorization (NMF)
- Dark-NTD: utilizes the non-negative Tucker decomposition (NTD)

![](https://hackmd.io/_uploads/HJ2x9N7L3.png)

The darknet is the unused IP address space of the Internet, and is an observation network where most observed traffic are malicious communications. It is useful for understanding global cyber attack trends. The darknet is also known as a network telescope and should not be confused with the dark web, such as Tor.

<br>
<br>

---

# Modules

## Input data format

| Engine | Input data format |
| -------- | -------------- |
| Dark-GLASSO   | text data |
| Dark-NMF      | text data |
| Dark-NTD      | pcap data   |
| ChangeFinder | text data |

> ChangeFinder is a conventional method and was used in the paper for comparative evaluation.

<br>

---

## Source code (R)

| Engine | Source code                                                       |
| -------- | ------------------------------------------------------------ |
| Dark-GLASSO   | online_portinfo.r / online_portinfo.r                        |
| Dark-NMF      | DarkNMF.r / DarkNMF_alertonly.r / DarkNMF-port.r / DarkNMF-port_alertonly.r |
| Dark-NTD      | online_script.R                                              |
| ChangeFinder | 2021_cpd.ipynb |


<br>

---

## Processing Procedures for Each Module

### DarkTRACER_online.sh
- Dark-GLASSO, Dark-NMF, and Dark-NTD can all be processed together in this shell script.
  - The period is specified by START and END, and calculations are performed sequentially at intervals of nextwindow_interval.
- Up to line 180, the process of preparing the input data for each module from the darknet PCAP data.
  - Sometimes the input data is empty or not long enough, so it is checked.
  - Dark-GLASSO uses 10 minutes of input data, while Dark-NMF and Dark-NTD use 30 minutes.
- If you have processed CSV data instead of PCAP, please create the input data format from CSV according to the module.
    - For Dark-GLASSO, tcpdump at line 113, and for Dark-NMF, tcpdump at line 169 will produce text data. This is the input data.
    - For Dark-NTD, PCAP is the input data (line 174). The input data format must be created from CSV by looking at the Dark-NTD source code.
- The execution part of each module starts at line 180.


<br>

---

### Dark-GLASSO

```
1. If you have the result of the previous run, do the following. If not, do 2.
  1.1 Create ${output_filespace}density_old_${theta}
  1.2 Copy the previous results into
    $ cp -r ${data_filespace}sensor${ID}/${Lasttime_YEAR}${Lasttime_MONTH}/${Lasttime_YEAR}${Lasttime_MONTH}${Lasttime_DAY}/${Lasttime_TIME}/result_M12/density_${theta}/* ${output_filespace}density_old_${theta}/

2. run online_density.r
3. (number of density files) == 6 and RT_density file has no 0 bytes
  3.1 Run online_portinfo.r
4. delete input data from 6 days ago
5. when execution is finished, delete unnecessary files such as input data
```


### Dark-NMF (without computing synchronization of port spatial features)

```
1 Run DarkNMF.r
2 Run DarkNMF_alertonly.r
3 When execution is finished, delete unnecessary files such as input data.
```

### Dark-NMF (with synchronization computation of port spatial features)

```
1 Create ${data_filespace}sensor${ID}/Anomaly_dstPort_list
2 portlist_file="${data_filespace}sensor${ID}/Anomaly_dstPort_list/${START_YEAR}${START_MONTH}_Anomaly_dstPort_list_ver${ver}.txt
3 Write 0 to portlist_file
4 Execute DarkNMF.r
5 Execution of DarkNMF_alertonly.r
6 When portlist_file is non-zero
  6.1 Execution of DarkNMF-port.r
  6.2 Execution of DarkNMF-port_alertonly.r
7 When execution is finished, delete unnecessary files such as input data.
```

### Dark-NTD

```
1 Execution of online_script.
2 When execution is finished, delete unnecessary files such as input data.
```


### ChangeFinder

```
It can be run from 2021_cpd.ipynb. (includes sample data)
```


<br>

---


## Notes

- As a preprocessing step, destination port numbers with a large number of unique hosts and packets are identified and excluded.
  - We leave the period setting and judgment method to you.
  - Also, port numbers frequently obtained by alerts can be considered as port numbers that are no longer of interest, so they can be excluded.
  - Reasons for preprocessing to exclude port numbers:
    - By excluding packets with uninteresting port numbers, it is expected that other port numbers that have not attracted attention will be highlighted.
- Make sure that there is no time discrepancy. The TimeZone on the server you are experimenting with may need to be "Asia/Tokyo."
