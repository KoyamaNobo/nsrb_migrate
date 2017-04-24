/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　日次処理                  JIP
01 ASM010LM日付入力                        CHAINN  ANO exec
02 ******                                  CHAINN  ANO
03 ******                                  CHAINN  ANO
04 ******                                  CHAINN  ANO
05 TD01  JS手形･領収書･買掛支払伝票　入力  CHAINN  A   job
06 TD10  JS手形・購買変換                  CHAINN  A   job
07 ******                                  CHAINN  ANO
08 TD15  JS受手 異動伝票入力               CHAINN  A   job
09 TSD250LM異動データ更新・クリア          CHAINN  ANO exec
10 ******                                  CHAINN  ANO
11 ******  ******************************  CHAINN  ANO
12 TR10  JS領収書変換生成・チェックリスト  CHAINN  A   job
13 TSR050LM領収書　追加入力                CHAINN  ANO exec
14 TR20  JS領収書　チェックリスト          CHAINN  A   job
15 TR30  JS領収書　発行                 #  CHAINN  A   job
16 ******                                  CHAINN  ANO
17 ******                                  CHAINN  ANO
18 TR35  JS領収書 再発行 (再発行表示無) #  CHAINN  A   job
19 ******                                  CHAINN  ANO
20 ******                                  CHAINN  ANO
/>
