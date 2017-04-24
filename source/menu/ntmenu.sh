/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 【退職給与引当金管理業務】      JIP
01 ******  ----マスタメンテナンス処理----  CHAINN  ANO
02 NT010MLM人事マスタ（２）メンテナンス    CHAINN  ANO NRLML3
03 NT020MLM退職金支給率マスタメンテナンス  CHAINN  ANO NRLML3
04 ******                                  CHAINN  ANO
05 ******                                  CHAINN  ANO
06 ******                                  CHAINN  ANO
07 ******                                  CHAINN  ANO
08 ******                                  CHAINN  ANO
09 NTSOF SM*総括表ファイルメンテナンス     CHAINN  ANO exec
10 NTHIF SM*日付ファイルメンテナンス       CHAINN  ANO exec
11 ******  -----------入力処理-----------  CHAINN  ANO
12 JJM910LM退職金共済掛金入力              CHAINN  ANO NRLML3
13 ******                                  CHAINN  ANO
14 ******                                  CHAINN  ANO
15 ******                                  CHAINN  ANO
16 ******  -----------問合せ処理---------  CHAINN  ANO
17 NTT100JS退職金　問合せ                  CHAINN  A   job
18 ******                                  CHAINN  ANO
19 ******                                  CHAINN  ANO
20 JJE010LM社員ｺｰﾄﾞ　問合せ                CHAINN  ANO NRLML3
21 ******  -----------作表処理-----------  CHAINN  ANO
22 NT050ULM作表ワーク生成                  CHAINN  ANO NRLML3
23 ******                                  CHAINN  ANO
24 ******                                  CHAINN  ANO
25 NTL060JS退職給与引当金内訳表            CHAINN  A   job
26 NTL075JS退職金共済掛金引当金額表        CHAINN  A   job
27 NT080LLM退職給与引当金総括表            CHAINN  ANO NRLML3
28 ******                                  CHAINN  ANO
29 ******                                  CHAINN  ANO
30 ******                                  CHAINN  ANO
31 ******  -----------期末処理-----------  CHAINN  ANO
32 ******                                  CHAINN  ANO
33 ******                                  CHAINN  ANO
34 ******                                  CHAINN  ANO
35 ******                                  CHAINN  ANO
36 ******                                  CHAINN  ANO
37 ******                                  CHAINN  ANO
38 ******                                  CHAINN  ANO
39 ******                                  CHAINN  ANO
40 NT090ULM期末繰越                        CHAINN  ANO NRLML3
/>
