/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　手形問合せ                  JIP
01 TT10  JS割引手形　問合せ                CHAINN  A   job
02 ******                                  CHAINN  ANO
03 TT05  JS取引先別受取手形　問合せ        CHAINN  A   job
04 ******                                  CHAINN  ANO
05 TT15  JS取引先別支払手形　問合せ        CHAINN  A   job
06 ******                                  CHAINN  ANO
07 TT20  JS割引手形　決済予定一覧　問合せ  CHAINN  A   job
08 ******  ------------------------------  CHAINN  ANO
09 HKM520LM得意先コード　問合せ            CHAINN  ANO exec
10 KBM050LM仕入先コード　問合せ            CHAINN  ANO exec
11 HT15  JS得意先売掛データ　変換          CHAINN  A   job
12 HKT165LM得意先別売掛残高　問合せ        CHAINN  ANO exec
13 HKT210LM          〃                    CHAINN  ANO exec
14 HKT160LM                                CHAINN  ANO exec
15 BT10  JS買掛金台帳　問合せ              CHAINN  A   job
16 ******                                  CHAINN  ANO
17 ******                                  CHAINN  ANO
18 HT35  JS得意先別　請求明細　問合せ      CHAINN  A   job
19 HT45  JS　 〃 　　入金明細　　〃        CHAINN  A   job
20 ******                                  CHAINN  ANO
/>
