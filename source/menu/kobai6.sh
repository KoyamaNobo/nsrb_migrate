/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　購買年間処理              JIP
01 BY50  JS年間仕入先別　仕入高明細表   *  CHAINN  A   job
02 ******                                  CHAINN  ANO
03 BY60  JS年間品目区分別集計表　１・２    CHAINN  A   job
04 BY70  JS年間製品仕入明細表              CHAINN  A   job
05 ******                                  CHAINN  ANO
06 BY80  JS仕入先材料別仕入明細表          CHAINN  A   job
07 BY82  JS材料仕入先別仕入明細表          CHAINN  A   job
08 ******                                  CHAINN  ANO
09 ******  ******************************  CHAINN  ANO
10 KBM410LM材料統計マスタ　前残修正入力    CHAINN  ANO exec
11 KBY050LM材料棚卸入力ファイル　クリア    CHAINN  ANO exec
12 ******  ･･････････････････････････････  CHAINN  ANO
13 KBY060LM材料棚卸　入力                  CHAINN  ANO exec
14 BY15  JS材料棚卸明細表                  CHAINN  A   job
15 ******  ･･････････････････････････････  CHAINN  ANO
16 KBY070LM材料棚卸 セット (４月クリア後)  CHAINN  ANO exec
17 ******                                  CHAINN  ANO
18 BY20  JS部門別材料棚卸差額表            CHAINN  A   job
19 ******                                  CHAINN  ANO
20 BY90  JS材料棚卸　更新                  CHAINN  A   job
/>
