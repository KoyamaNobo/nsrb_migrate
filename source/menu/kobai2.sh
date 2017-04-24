/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　日次処理　（入力･累積･更新）  JIP
01 ASM010LM日付入力                        CHAINN  ANO exec
02 ******  ･･････････････････････････････  CHAINN  ANO
03 KBD010LM仕入伝票入力                    CHAINN  ANO exec
04 ******                                  CHAINN  ANO
05 KBD050LM工品製品仕入　変換可能リスト    CHAINN  ANO exec
06 ******                                  CHAINN  ANO
07 ******                                  CHAINN  ANO
08 KBD020LM出庫伝票入力                    CHAINN  ANO exec
09 ******                                  CHAINN  ANO
10 KBD900LM（支払修正入力）                CHAINN  ANO exec
11 BD10  JS日次更新　累積　クリア          CHAINN  A   job
12 ******                                  CHAINN  ANO
13 ******                                  CHAINN  ANO
14 ******                                  CHAINN  ANO
15 ******  ------------------------------  CHAINN  ANO
16 BD20  JS製品仕入　入力                  CHAINN  A   job
17 ******                                  CHAINN  ANO
18 KBD150LM営業変換・販売変換可能リスト    CHAINN  ANO exec
19 ******  ･･････････････････････････････  CHAINN  ANO
20 BD25  JS購買・販売変換未処理リスト      CHAINN  A   job
/>
