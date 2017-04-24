/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 日次処理
01 PRD20 JS振替伝票入力(手形･売掛･買掛他)  CHAINN  A   job
02 ******                                  CHAINN  ANO
03 PRD300LM財務変換  ( 01 → 11 )          CHAINN  ANO exec
04 ******                                  CHAINN  ANO
05 TD05  JS手形決済更新・財務変換          CHAINN  A   job
06 ******  ------------------------------  CHAINN  ANO
07 HKM520LM得意先コード　問合せ            CHAINN  ANO exec
08 PR070RLM取引先コード　問合せ            CHAINN  ANO exec
09 PR010RLM勘定科目コード　問合せ          CHAINN  ANO exec
10 PR085RLM摘要コード　問合せ              CHAINN  ANO exec
11 PR201ILM振替伝票入力                    CHAINN  ANO exec
12 ******                                  CHAINN  ANO
13 JR205LJS仕訳伝票チェックリスト          CHAINN  A   job
14 ******                                  CHAINN  ANO
15 ******                                  CHAINN  ANO
16 JR290UJS◎　マスタ更新　◎              CHAINN  A   job
17 ******                                  CHAINN  ANO
18 JR280UJS◎　日次繰越　◎                CHAINN  A   job
19 ******  ******************************  CHAINN  ANO
20 MSIWIWSM仕訳インプットワーク　修正入力  CHAINN  ANO exec
/>
