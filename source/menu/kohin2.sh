/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 工品　日次処理                  JIP
01 ASM010LM日付入力                        CHAINN  ANO exec
02 KHD010LM売上・値引入力                  CHAINN  ANO exec
03 KD11  JS売上・値引伝票発行 (マット他)   CHAINN  A   job
04 KD20  JS加硫伝票入力                    CHAINN  A   job
05 KHD210LM廃却不良別伝票入力              CHAINN  ANO exec
06 ******                                  CHAINN  ANO
07 ******                                  CHAINN  ANO
08 KD25  JS(工品製品仕入　変換)            CHAINN  A   job
09 ******                                  CHAINN  ANO
10 KD30  JS工品他日計更新累積クリア        CHAINN  A   job
11 KHD610LM工品売上集計表(日計)            CHAINN  ANO exec
12 ******                                  CHAINN  ANO
13 ******                                  CHAINN  ANO
14 ******                                  CHAINN  ANO
15 ******                                  CHAINN  ANO
16 KY71  JS工品販売計画・実績問合せ        CHAINN  A   job
17 ******  ･･････････････････････････････  CHAINN  ANO
18 ******                                  CHAINN  ANO
19 KD15  JS売上値引伝票　再発行            CHAINN  A   job
20 BD95  JS(工品製品仕入  未変換チェック)  CHAINN  A   job
/>
