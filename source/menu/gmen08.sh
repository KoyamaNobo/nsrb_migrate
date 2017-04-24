/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 ＊＊　教育発注入庫　処理　＊＊
01 KBD310LM製品仕入　発注伝票　入力        CHAINN  ANO exec
02 ******                                  CHAINN  ANO
03 ******                                  CHAINN  ANO
04 JJ252RJS倉別在庫　問合せ                CHAINN  A   job
05 JJ255RJS実在庫　問合せ                  CHAINN  A   job
06 ******                                  CHAINN  ANO
07 KBT510LM品名別　発注入庫残　問合        CHAINN  ANO exec
08 KBT520LM仕入先別　発注入庫残　問合せ    CHAINN  ANO exec
09 BT61  JS納期別　入庫残　問合せ          CHAINN  A   job
10 KBT550LMロット別　入庫明細　問合せ      CHAINN  ANO exec
11 ******                                  CHAINN  ANO
12 ******                                  CHAINN  ANO
13 BD51  JS発注日別　発注入庫明細表        CHAINN  A   job
14 BD55  JS納期別 発注・入庫明細表         CHAINN  A   job
15 BD61  JS品名別　発注入庫残明細表        CHAINN  A   job
16 BD85  JS　　　　　〃　　　　(日付指定)  CHAINN  A   job
17 BD65  JS仕入先別　発注入庫残明細表      CHAINN  A   job
18 ******                                  CHAINN  ANO
19 ******                                  CHAINN  ANO
20 ******                                  CHAINN  ANO
21 HMM060LM品名コード問合せ                CHAINN  ANO exec
22 ******                                  CHAINN  ANO
23 ******                                  CHAINN  ANO
24 ******                                  CHAINN  ANO
25 ******                                  CHAINN  ANO
26 HY15S JS年間品名サイズ別出荷明細表 ﾉﾐ   CHAINN  A   job
27 HY15P JS　　　　〃　　　返品明細表      CHAINN  A   job
28 ******                                  CHAINN  ANO
29 ******                                  CHAINN  ANO
30 ******  ***** ＰＣ エクセル 変換 *****  CHAINN  ANO
31 ******                                  CHAINN  ANO
32 ******  事前納品連絡データ抽出 (SGNRF)  CHAINN  A   NRJSL
33 ******                                  CHAINN  ANO
34 ******  出荷指図データ　抽出   (SJSTF)  CHAINN  A   NRJSL
35 ******                                  CHAINN  ANO
36 ******  出荷予定 変換  PC→AVX (SSYF)   CHAINN  A   NRJSL
37 ******                                  CHAINN  ANO
38 ******                                  CHAINN  ANO
39 HG47  JS品名受払抽出 色有      (HHUHF)  CHAINN  A   job
40 HG49  JS     〃      色無      (HHUHF)  CHAINN  A   job
/>
