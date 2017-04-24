/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　　　日次処理              JIP
01 ASM010LM日付入力                        CHAINN  ANO exec
02 ******  ------------------------------  CHAINN  ANO
03 HD11  JS売上・値引入力                  CHAINN  A   job
04 ******                                  CHAINN  ANO
05 HD15  JS売上・値引伝票発行　更新　累積  CHAINN  A   job
06 HD17  JS　　　　〃　　Ａ４　　　〃      CHAINN  A   job
07 ******                                  CHAINN  ANO
08 HD25  JS仕上受入入力                    CHAINN  A   job
09 HD27  JS製品仕入変換 (購買)             CHAINN  A   job
10 ******                                  CHAINN  ANO
11 PRD35 JS入金変換（財務より）            CHAINN  A   job
12 HKD010LM入金伝票入力                    CHAINN  ANO exec
13 HD40  JS入金票作成（マスター更新）      CHAINN  A   job
14 ******                                  CHAINN  ANO
15 HD30  JS在庫更新　日計更新　クリア      CHAINN  A   job
16 ******  ------------------------------  CHAINN  ANO
17 HD05  JSワークマン売上変換リスト        CHAINN  A   job
18 HD06  JSナ　フ　コ　　　〃              CHAINN  A   job
19 HMD150LMワークマン･ナフコ指図修正入力   CHAINN  ANO exec
20 HD10  JSワークマン･ナフコ売上自動変換   CHAINN  A   job
21 HMD710LM履物売上集計表                  CHAINN  ANO exec
22 ******  ------------------------------  CHAINN  ANO
23 HD81  JS出荷指図　売上未計上リスト      CHAINN  A   job
24 HG35  JS得意先品名別　預り受払表        CHAINN  A   job
25 HG32  JS分類品名別不良返品・格外合計表  CHAINN  A   job
26 HG90  JS月末  在庫一括振替  (伝票発行)  CHAINN  A   job
27 HT02  JS在庫明細問合せ(－分)            CHAINN  A   job
28 HD48  JS在庫明細表（親子関係のみ）      CHAINN  A   job
29 ******  ------------------------------  CHAINN  ANO
30 HD99  JS売上値引エクセル変換(VIV･ﾄﾞｯﾄ)  CHAINN  A   job
31 HMD221LM売上・値引伝票データ　復旧      CHAINN  ANO exec
32 ******  ------------------------------  CHAINN  ANO
33 ******  　  ☆　日計更新後　☆          CHAINN  ANO
34 HD91  JS（売上・値引伝票　再発行）      CHAINN  A   job
35 HD92  JS                〃　　　　Ａ４  CHAINN  A   job
36 HD97  JS（入金票　再発行）              CHAINN  A   job
37 HT15  JS得意先別売掛データ  変換        CHAINN  A   job
38 ******  ------------------------------  CHAINN  ANO
39 HKD110LM入金累積ファイル　修正入力      CHAINN  ANO exec
40 MNYUF SM入金ファイル　強制修正入力      CHAINN  ANO exec
/>
