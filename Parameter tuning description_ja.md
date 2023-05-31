# 良くチューニングするパラメータの説明
## Dark-GLASSO
1. 異常検知のしきい値(theta=0.98)
    - copy_old_data.rとonline_portinfo.rに存在
	- 値を大きくするとアラートが出やすい (theta=0.995など)
	- 値を小さくするとアラートが出にくい
        - 上限，下限は特に意見なし
	- 通常はtheta=0.98で運用中
2. 異常検知時に参照するデータサイズ(alert_window_size=432)
    - online_portinfo.rに存在
    - 通常はalert_window_size=432で運用中 (3日分のデータを参照)
        - GLASSOのプロセスを初めて実行する場合，アラートを得るには3日分のデータが貯まるまで待つ必要がある．
        - 値を小さくするとアラートを早く得ることができる
    - 値の1は10分のデータ1個分を指す (6だと1時間分，6x24=144だと1日分，6x24x3=432だと3日分の意味)
        - 正常に運用したいのであれば，下限として144は欲しい．また，適宜thetaの値も連動して変化させないと精度が落ちる可能性あり
3. 送信元ホスト数の制限 (host_upper_bound=1000)
    - online_density.rに存在
    - ホスト数が多いと処理時間が指数的に増加するため制限
        - 計算時間に余裕があれば値を増やす方が望ましい
    - 下限として500は欲しい
        - 500より少ないと精度が極端に悪くなり，さらにアラートが逆に出づらくなる可能性がある．
4. アラートの対象ポートを判定する際のしきい値 (alert_percent_upper_bound=10とalert_num_upper_bound=20)
    - online_portinfo.rに存在
    - alert_percent_upper_boundはアラートの対象ポートを判定する際のしきい値 (ホストの割合)
        - 値10とは，10%以上のホストが対象ポートにパケットを送信しているか確認することを指す
    - alert_num_upper_boundもアラートの対象ポートを判定する際のしきい値 (ホストの数)
        - 値20とは，20個以上のホストが対象ポートにパケットを送信しているか確認することを指す
    - あまりチューニングする必要はないと考えられるが，
        - アラートが出過ぎてるときは両方とも値を大きくすると良い
        - アラートが少ないときは，両方とも値を小さくすると良い
- 他のパラメータはあまりチューニングしなくて良い

### Dark-GLASSOにおける注意点
- Dark-GLASSOの注意点として，上記のパラメータを一度チューニングすると，alert_window_sizeで指定した期間分データが貯まるまで待つ必要がある．
    - 他のエンジンはパラメータをチューニングしてもデータが貯まることを待たなくても良い．

<br>
<br>
<br>

---

## Dark-NMF
1. Choose_Portinfo_ActiveHost
    - DarkNMF_alertonly.rに存在
    - 異常検知を行う際に，異常アクティブホストだけで行うなら1，全アクティブホストで行うなら0
    - Choose_Portinfo_ActiveHost = 0のとき
        - 誤検知がほぼないため，運用に用いることが望ましい．
        - ただ正解の検知も少なくなるため，DarkNMF-port.rを追加で運用する．
        - 現状，DarkNMF_alertonly.rのテキスト処理時間が非常にかかってしまっている．
    - Choose_Portinfo_ActiveHost = 1のとき
        - 現状，DarkNMF_alertonly.rのテキスト処理時間が短くなる
        - しかし，誤検知が多い．
        - DarkNMF-port.rを追加で運用する必要はない
    - 今後の方針
        - Choose_Portinfo_ActiveHost = 1で運用 (このときDarkNMF-port.rは処理しなくて良い)
        - テキスト処理が重たいことを解消して，Choose_Portinfo_ActiveHost = 0で運用 (このときDarkNMF-port.rを追加で運用する)
2. 異常アクティブホストの判定しきい値 (AnomalyActiveHost_packet_threshold=50と AnomalyActiveHost_threshold=2)
    - DarkNMF_alertonly.rに存在
    - AnomalyActiveHost_packet_thresholdは異常アクティブホストの判定する際のしきい値 (最大パケット数の割合)
        - 値50とは，アクティブホストの中で最も大きいパケット数をホストの50%をしきい値に，最大パケットの50%以上を示すアクティブホストを異常アクティブホストとする
    - AnomalyActiveHost_thresholdは異常アクティブホストの判定する際のしきい値 (ホストの数)
        - 値2とは，上記のAnomalyActiveHost_packet_thresholdによって判定された異常アクティブホストが2個以上であることを確認している．
    - これらはChoose_Portinfo_ActiveHost = 1のときだけ有効なしきい値
        - Choose_Portinfo_ActiveHost = 1のとき，あまりアラートが得られなくなった場合は，AnomalyActiveHost_packet_thresholdの値を小さくすれば良い
        - 逆にアラートが出過ぎている場合は，AnomalyActiveHost_packet_thresholdの値をより大きく，またはAnomalyActiveHost_thresholdの値を大きくすると良い
3. アラートの対象ポートを判定する際のしきい値 (alert_percent_upper_bound=30と alert_num_upper_bound=2)
    - DarkNMF_alertonly.rに存在
    - alert_percent_upper_boundはアラートの対象ポートを判定する際のしきい値 (ホストの割合)
    - alert_num_upper_boundはアラートの対象ポートを判定する際のしきい値 (ホストの数)
    - Dark-GLASSOにおけるalert_percent_upper_bound，alert_num_upper_boundと同じ意味．そのため，同様にあまりチューニングする必要はないと考えられるが，
        - アラートが出過ぎてるときは両方とも値を大きくすると良い
        - アラートが少ないときは，両方とも値を小さくすると良い
- DarkNMF-port.rとDarkNMF-port_alertonly.rについて
    - DarkNMF_alertonly.rのChoose_Portinfo_ActiveHost=0のとき，port.rを運用する
    - DarkNMF-port_alertonly.rにもChoose_Portinfo_ActivedstPortやAnomalyActivedstPort_packet_threshold, AnomalyActivedstPort_thresholdが存在する．意味はDarkNMF-port.r上のパラメータと同様である．
        - Choose_Portinfo_ActivedstPortは1固定で良い
        - AnomalyActivedstPort_packet_thresholdは通常98にしているが，アラートが得られない場合はこの値を小さく(90~98の間ぐらい)，アラートが出過ぎる場合は値を大きくすると良い．
        - AnomalyActivedstPort_thresholdは2固定で良い


<br>
<br>

---
## Dark-NTD
- Dark-NTDにおける主にチューニング可能なパラメータは3つのみで，チューニングする必要はほとんどないと考えられる
    - NTDアルゴリズムの基底数 (CORE_SIZE=5)
    - FSTDの選択fiber数 (FSTD_NUM=5)
        - CORE_SIZEとFSTD_NUMは両方とも5固定で良い
    - 反復回数 (ITERATION_NUM=4)
        - アラートが得られない場合は反復回数を増やすと良い
        - 通常は4で良い
