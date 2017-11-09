/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　　　日次処理              JIP
01 ******  □□　当月分入力処理　□□      CHAINN  ANO
02 HD12  JS売上・値引入力                  CHAINN  A   job
03 ******                                  CHAINN  ANO
04 HD14  JS売上・値引伝票発行              CHAINN  A   job
05 ******                                  CHAINN  ANO
06 ******                                  CHAINN  ANO
07 ******                                  CHAINN  ANO
08 ******                                  CHAINN  ANO
09 ******                                  CHAINN  ANO
10 HD16  JSSTRANｾｯﾄ(通常に),後[日次更新]   CHAINN  A   job
11 ******  □□　次月分入力処理　□□      CHAINN  ANO
12 HD13  JS売上・値引入力                  CHAINN  A   job
13 ******                                  CHAINN  ANO
14 HD14  JS売上・値引伝票発行              CHAINN  A   job
15 ******                                  CHAINN  ANO
16 ******                                  CHAINN  ANO
17 HD90  JS       〃     再発行            CHAINN  A   job
18 ******                                  CHAINN  ANO
19 ******  ******************************  CHAINN  ANO
20 HD16  JSSTRANｾｯﾄ(通常に),後[日次更新]   CHAINN  A   job
/>
