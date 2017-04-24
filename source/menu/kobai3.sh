/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　月次処理　（作表）        JIP
01 BD80  JS購買支払未変換チェックリスト    CHAINN  A   job
02 BD90  JS購買未印字・工品未変換チェック  CHAINN  A   job
03 BD75  JS製品仕入未変換チェック          CHAINN  A   job
04 ******  ------------------------------  CHAINN  ANO
05 BG10  JS  仕入 各日付別集計表           CHAINN  A   job
06 BG05  JS  日付別買掛残支払明細表        CHAINN  A   job
07 ******                                  CHAINN  ANO
08 KBG210LM仕入先別 仕入金額明細表      *  CHAINN  ANO exec
09 ******                                  CHAINN  ANO
10 BG20  JS買掛金台帳                   #  CHAINN  A   job
11 ******                                  CHAINN  ANO
12 BG55  JS製品仕入　明細表                CHAINN  A   job
13 BG60  JS仕入品材料　明細表他            CHAINN  A   job
14 BG45  JS材料区分別 仕入・棚卸表         CHAINN  A   job
15 BG43  JS部門材料別仕入明細表            CHAINN  A   job
16 ******                                  CHAINN  ANO
17 BG25  JS材料・製品品目区分別　受払表    CHAINN  A   job
18 ******                                  CHAINN  ANO
19 ******                                  CHAINN  ANO
20 BG35  JS材料受払表                      CHAINN  A   job
21 BG40  JS材料日付別　受払表              CHAINN  A   job
22 ******                                  CHAINN  ANO
23 ******                                  CHAINN  ANO
24 ******                                  CHAINN  ANO
25 ******                                  CHAINN  ANO
26 ******                                  CHAINN  ANO
27 ******                                  CHAINN  ANO
28 ******                                  CHAINN  ANO
29 ******                                  CHAINN  ANO
30 ******  ------------------------------  CHAINN  ANO
31 BG65  JS材料在庫明細表                  CHAINN  A   job
32 ******                                  CHAINN  ANO
33 ******                                  CHAINN  ANO
34 ******                                  CHAINN  ANO
35 ******                                  CHAINN  ANO
36 ******                                  CHAINN  ANO
37 ******                                  CHAINN  ANO
38 ******                                  CHAINN  ANO
39 ******                                  CHAINN  ANO
40 ******                                  CHAINN  ANO
/>
