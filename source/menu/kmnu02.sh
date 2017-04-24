/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 教育日次処理(伝票発行他)        JIP
01 ******  ＊＊＊　　伝票発行　　＊＊＊    CHAINN  ANO
02 ******                                  CHAINN  ANO
03 JJN300JS出荷指図書発行                  CHAINN  A   job
04 ******                                  CHAINN  ANO
05 JJ0540JS送り状発行；福山通運            CHAINN  A   job
06 JJ0580JS　　　　　；佐川急便            CHAINN  A   job
07 JJ0590JS　　　　　；西濃運輸            CHAINN  A   job
08 ******                                  CHAINN  ANO
09 JJ0500JS荷札・入日記                    CHAINN  A   job
10 ******                                  CHAINN  ANO
11 ******                                  CHAINN  ANO
12 ******                                  CHAINN  ANO
13 ******                                  CHAINN  ANO
14 ******  ･･････････････････････････････  CHAINN  ANO
15 JT052ULM送り状入力                      CHAINN  ANO exec
16 ******  ＊＊＊　そ　の　他　＊＊＊      CHAINN  ANO
17 ******                                  CHAINN  ANO
18 ******                                  CHAINN  ANO
19 ******                                  CHAINN  ANO
20 SYUKKAPM荷札入日記入力・作票            CHAINN  ANO menu
21 ******  ＊＊＊　　問　合　せ　　＊＊＊  CHAINN  ANO
22 ******                                  CHAINN  ANO
23 JJ040RJS未処理データ問合せ              CHAINN  A   job
24 ******                                  CHAINN  ANO
25 NJJ051JS出荷指図№問合せ                CHAINN  A   job
26 ******                                  CHAINN  ANO
27 NJJ053JS送り状問合せ                    CHAINN  A   job
28 ******                                  CHAINN  ANO
29 NJJ110JS荷札・入日記問合せ              CHAINN  A   job
30 ******                                  CHAINN  ANO
31 JJ251RJS倉別在庫問合せ                  CHAINN  A   job
32 ******                                  CHAINN  ANO
33 ******                                  CHAINN  ANO
34 ******                                  CHAINN  ANO
35 ******                                  CHAINN  ANO
36 ******  ＊＊＊　通信異常時　　＊＊＊    CHAINN  ANO
37 ******                                  CHAINN  ANO
38 ******                                  CHAINN  ANO
39 ******                                  CHAINN  ANO
40 GMEN10PM外部倉庫分　伝票発行            CHAINN  ANO menu
/>
