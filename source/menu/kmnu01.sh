/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 教育日次処理(入力・送信他)      JIP
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
14 JJ030UJS送り状データ自動生成【教　育】  CHAINN  A   job
15 ******                                  CHAINN  ANO
16 ******  ------------------------------  CHAINN  ANO
17 HJO11NJS送信データ生成（両備）          CHAINN  A   job
18 HJO12NJS　　　〃　　　（玉島）          CHAINN  A   job
19 ******                                  CHAINN  ANO
20 ******                                  CHAINN  ANO
21 HJO16NJS再送用送信データ生成（両備）    CHAINN  A   job
22 HJO15NJS　　　　 〃 　　　　（玉島）    CHAINN  A   job
23 JK900MLMＯ／Ｌ状況Ｆ　クリア            CHAINN  ANO exec
24 ******  ------------------------------  CHAINN  ANO
25 JJN289JS有効在庫抽出 <全件>    (SYZDF)  CHAINN  A   job
26 HD28  JS倉庫間移動入力・更新            CHAINN  A   job
27 ******                                  CHAINN  ANO
28 ******                                  CHAINN  ANO
29 ******                                  CHAINN  ANO
30 ******                                  CHAINN  ANO
31 ******                                  CHAINN  ANO
32 ******                                  CHAINN  ANO
33 HENTODJS受注Ｅｘｃｅｌ変換  (JMSTD-K)   CHAINN  A  job
34 ******                                  CHAINN  ANO
35 JHS570JS請求データ作成　(AKATYA.CSV)    CHAINN  A  job
36 ******                                  CHAINN  ANO
37 ******                                  CHAINN  ANO
38 ******                                  CHAINN  ANO
39 ******                                  CHAINN  ANO
40 ******                                  CHAINN  ANO
/>
