/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 ＊ 早島倉庫出荷処理(Ⅰ)　＊     JIP
01 RJJ020JS受信更新                        CHAINN  A   job
02 ******  ------------------------------  CHAINN  ANO
03 RJJ030JS出荷指図書発行                  CHAINN  A   job
04 ******                                  CHAINN  ANO
05 ******                                  CHAINN  ANO
06 RJJ010JS福山通運ＣＳＶ作成              CHAINN  A   job
07 ******                                  CHAINN  ANO
08 RJJ058JS送り状発行；佐川急便            CHAINN  A   job
09 RJJ059JS　　　　　；西濃運輸            CHAINN  A   job
10 ******                                  CHAINN  ANO
11 ******                                  CHAINN  ANO
12 RJJ054JS（福通送り状再発行）            CHAINN  A   job
13 ******                                  CHAINN  ANO
14 ******  ------------------------------  CHAINN  ANO
15 RJT520JS送り状入力                      CHAINN  A   job
16 ******  ------------------------------  CHAINN  ANO
17 RJK690JS出荷指図書訂正・取消            CHAINN  A   job
18 ******                                  CHAINN  ANO
19 RJK720JS送信実績データ生成              CHAINN  A   job
20 ******                                  CHAINN  ANO
21 RJJ090JS◎　日次繰越　◎                CHAINN  A   job
22 ******  ------------------------------  CHAINN  ANO
23 RJJ040JS未処理データ問合せ              CHAINN  A   job
24 ******                                  CHAINN  ANO
25 RJJ051JS出荷指図№問合せ                CHAINN  A   job
26 RJJ053JS送り状問合せ                    CHAINN  A   job
27 ******                                  CHAINN  ANO
28 ******                                  CHAINN  ANO
29 ******                                  CHAINN  ANO
30 RJ251RJS倉別在庫問合せ                  CHAINN  A   job
31 ******                                  CHAINN  ANO
32 ******                                  CHAINN  ANO
33 ******                                  CHAINN  ANO
34 ******                                  CHAINN  ANO
35 HMM240LM直送先コード問合せ              CHAINN  ANO exec
36 HMM060LM品名コード問合せ                CHAINN  ANO exec
37 ******                                  CHAINN  ANO
38 ******  ＊＊＊　異　常　時　＊＊＊      CHAINN  ANO
39 ******                                  CHAINN  ANO
40 RYOB00PM他の端末処理                    RUN  N  ANO menu
/>
