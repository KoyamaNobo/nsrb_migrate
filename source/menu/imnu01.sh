/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 一般日次処理(入力・送信他）     JIP
01 JT010ILM預り・受注他入力                CHAINN  ANO exec
02 ******                                  CHAINN  ANO
03 JJ540UJS預り・受注他日報                CHAINN  A   job
04 ******                                  CHAINN  ANO
05 ******                                  CHAINN  ANO
06 JTN31ILM出荷指図入力　                  CHAINN  ANO exec
07 ******                                  CHAINN  ANO
08 JJN110JS　　 〃 　　リスト              CHAINN  A   job
09 ******                                  CHAINN  ANO
10 ******                                  CHAINN  ANO
11 JJN310JS返品伝票発行                    CHAINN  A   job
12 ******                                  CHAINN  ANO
13 ******                                  CHAINN  ANO
14 JJ031UJS送り状データ自動生成【一　般】  CHAINN  A   job
15 ******                                  CHAINN  ANO
16 ******  ------------------------------  CHAINN  ANO
17 HJO10NJS送信データ生成（玉島）          CHAINN  A   job
18 ******                                  CHAINN  ANO
19 ******  ------------------------------  CHAINN  ANO
20 JT012ILM受注ファイル　摘要修正入力      CHAINN  ANO exec
21 HJO15NJS再送用送信データ生成（玉島）    CHAINN  A   job
22 ******                                  CHAINN  ANO
23 JK900MLMＯ／Ｌ状況Ｆ　クリア            CHAINN  ANO exec
24 ******  ------------------------------  CHAINN  ANO
25 JJN289JS有効在庫抽出 <全件>    (SYZDF)  CHAINN  A   job
26 HD28  JS倉庫間移動入力・更新            CHAINN  A   job
27 ******                                  CHAINN  ANO
28 JJ2811JS取引先用有効在庫表              CHAINN  A   job
29 JTN90ILM預り（サイズ無）入力            CHAINN  ANO exec
30 ******  ------------------------------  CHAINN  ANO
31 JHS005JSトラスコ他ＦＡＸ分　入力        CHAINN  A   job
32 JJN050JS    〃　　自動指図変換・リスト  CHAINN  A   job
33 ******  ------------------------------  CHAINN  ANO
34 JHS010JSワークマン受注　更新･作表 (EDI  CHAINN  A   job
35 JHS110JS　　〃　　自動指図変換    (EDI  CHAINN  A   job
36 JHS280JS    〃    受信品名別集計表      CHAINN  A   job
37 JHS320JSナ　フ　コ受注　更新･作表 (EDI  CHAINN  A   job
38 ******  　　　　　○エクセル修正○      CHAINN  ANO
39 JHS350JS　　〃　　自動指図変換    (EDI  CHAINN  A   job
40 JHS281JS    〃    受信品名別集計表      CHAINN  A   job
/>
