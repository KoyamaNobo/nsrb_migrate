/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 月次処理
01 PR260LLM日計表                          CHAINN  ANO exec
02 PRG210LM勘定科目別日計表                CHAINN  ANO exec
03 ------  ------------------------------  CHAINN  ANO
04 PR400LLM合計残高試算表                  CHAINN  ANO exec
05 PR405LLM総勘定内訳表                    CHAINN  ANO exec
06 JR410UJS貸借対照表                      CHAINN  A   job
07 JR420UJS損益計算書（月次）              CHAINN  A   job
08 JR430UJS　　〃　　（期末）              CHAINN  A   job
09 JR510UJS製造原価報告書（月次）          CHAINN  A   job
10 JR520UJS　　　〃　　　（期末）          CHAINN  A   job
11 PR441LLM総勘定元帳                      CHAINN  ANO exec
12 PR445LLM補助元帳                        CHAINN  ANO exec
13 JR440UJS総勘定元帳・補助元帳（月次用）  CHAINN  A   job
14 ------  ------------------------------  CHAINN  ANO
15 ******                                  CHAINN  ANO
16 JR610UJS消費税振替明細表                CHAINN  A   job
17 ------                                  CHAINN  ANO
18 ------                                  CHAINN  ANO
19 JR620UJS◎　消費税振替伝票発行　◎      RUN  N  A   job
20 PR627LLM○　消費税振替伝票再発行　○    CHAINN  ANO exec
21 JR450UJS部門別損益計算書（月次）        CHAINN  A   job
22 JR460UJS 　　　〃 　　　（期末）        CHAINN  A   job
23 JR530UJS部門別製造原価報告書（月次）    CHAINN  A   job
24 JR540UJS 　　　　〃　　     （期末）    CHAINN  A   job
25 ------  ------------------------------  CHAINN  ANO
26 JR480UJS部門別損益管理表（月次）        CHAINN  A   job
27 JR490UJS       〃       （期末）        CHAINN  A   job
28 JR550UJS部門別製造原価管理表（月次）    CHAINN  A   job
29 JR560UJS         〃         （期末）    CHAINN  A   job
30 PR500LLM部門別経費管理表（月次）        CHAINN  ANO exec
31 PR505LLM       〃       （期末）        CHAINN  ANO exec
32 ------  ------------------------------  CHAINN  ANO
33 JR400UJS☆貸借･損益･製造原価　一括作表  CHAINN  A   job
34 ------                                  CHAINN  ANO
35 PR20  JS経費相手科目　日計表・内訳表    CHAINN  A   job
36 ------                                  CHAINN  ANO
37 PRG050LM財務関係　残高明細表            CHAINN  ANO exec
38 PR10  JS※　残高　明細表                CHAINN  A   job
39 ------  ------------------------------  CHAINN  ANO
40 PR40  JS科目月別　消費税内訳表          CHAINN  A   job
/>
