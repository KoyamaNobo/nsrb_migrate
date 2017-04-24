/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 その他メンテナンス              JIP
01 MHHTF SM履物品名統計ファイル(HHTF)      CHAINN  ANO exec
02 MHUHM SM履物品名受払ファイル(HUHM)      CHAINN  ANO exec
03 ******                                  CHAINN  ANO
04 MTSKF SM得意先請求ファイル(TSKF)        CHAINN  ANO exec
05 MTTM  SM得意先統計ファイル(TTM)         CHAINN  ANO exec
06 MTZNTMSM得意先年間販売ファイル(TZNTM)   CHAINN  ANO exec
07 ******                                  CHAINN  ANO
08 HKE110LM担当・分類セット　年間累積Ｆ    CHAINN  ANO exec
09 HMY150LM売上他得意先月別対比　修正入力  CHAINN  ANO exec
10 MDATEMSM日付マスター(DATEM)             CHAINN  ANO exec
11 ******                                  CHAINN  ANO
12 ******                                  CHAINN  ANO
13 ******                                  CHAINN  ANO
14 HMM112LM品名マスタ　予定原価クリア      CHAINN  ANO exec
15 ******  ******************************  CHAINN  ANO
16 ******                                  CHAINN  ANO
17 ******  =====  ＥＸＣＬＥへ変換  =====  CHAINN  ANO
18 HM14  JS得意先品名単価変換 (THTW)       CHAINN  A   job
19 ******  ====　ＥＸＣＥＬより変換　====  CHAINN  ANO
20 HM13  JS得意先品名単価一括登録 (THTW)   CHAINN  A   job
/>
