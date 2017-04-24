/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 Ｆ・Ｂ総合振込　処理            JIP
01 FBN010LM総合振込　金額　入力            CHAINN  ANO exec
02 ******                                  CHAINN  ANO
03 FBP050LM総合振込　明細表                CHAINN  ANO exec
04 ******                                  CHAINN  ANO
05 FBP010LM総合振込　データ作成・リスト    CHAINN  ANO exec
06 ******  ******************************  CHAINN  ANO
07 JJM510LM振込銀行マスター　メンテナンス  CHAINN  ANO exec
08 JJM520LM　　　〃　　　　　問合せ        CHAINN  ANO exec
09 FBM010LM振込先マスター　メンテナンス    CHAINN  ANO exec
10 FBN050LM      〃        問合せ          CHAINN  ANO exec
11 ******  ------  中国銀行  ------------  CHAINN  ANO
12 FB10  JS総合振込データ　送信   №１     CHAINN  A   job
13 FB11  JS                       №２     CHAINN  A   job
14 FB12  JS 　　　　　　　　　　　№３     CHAINN  A   job
15 ******  ------------------------------  CHAINN  ANO
16 ******                                  CHAINN  ANO
17 ******                                  CHAINN  ANO
18 ******                                  CHAINN  ANO
19 ******                                  CHAINN  ANO
20 ******                                  CHAINN  ANO
/>
