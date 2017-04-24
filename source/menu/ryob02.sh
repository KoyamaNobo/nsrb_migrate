/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 ＊ 早島倉庫出荷処理(Ⅱ)         JIP
01 RJJ050JS荷札・入日記発行                CHAINN  A   job
02 ******                                  CHAINN  ANO
03 ******                                  CHAINN  ANO
04 ******                                  CHAINN  ANO
05 RJJ040JS未処理データ問合せ              CHAINN  A   job
06 ******                                  CHAINN  ANO
07 RJJ110JS荷札・入日記問合せ              CHAINN  A   job
08 ******                                  CHAINN  ANO
09 ******  ------------------------------  CHAINN  ANO
10 ******                                  CHAINN  ANO
11 RHM010JS荷札・入日記入力                CHAINN  A   job
12 RHM020JS荷札・入日記入力（数量）        CHAINN  A   job
13 ******                                  CHAINN  ANO
14 RHM110JS荷札・入日記入力分発行          CHAINN  A   job
15 ******                                  CHAINN  ANO
16 RHM900JS荷札・入日記入力分クリア        CHAINN  A   job
17 ******                                  CHAINN  ANO
18 ******  ＊＊＊　異　常　時　＊＊＊      CHAINN  ANO
19 ******                                  CHAINN  ANO
20 RYOB00PMその他端末処理                  RUN  N  ANO menu
/>
