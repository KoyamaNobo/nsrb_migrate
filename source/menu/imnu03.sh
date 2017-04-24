/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 一般日次処理(受信・更新他)      JIP
01 HJO30NJS出荷実績データ受信（玉島）      CHAINN  A   job
02 ******                                  CHAINN  ANO
03 ******                                  CHAINN  ANO
04 JTO33LLM受信分出荷指図書　（実績）      CHAINN  ANO exec
05 JJO34LJS　　　  〃    　白紙 ﾅﾌｺ･ﾜｰｸﾏﾝ  CHAINN  A   job
06 ******                                  CHAINN  ANO
07 JT590ILM出荷確定入力（訂正・取消し）    CHAINN  ANO exec
08 ******                                  CHAINN  ANO
09 JJ580IJS 　　〃 　　リスト              CHAINN  A   job
10 ******                                  CHAINN  ANO
11 ******                                  CHAINN  ANO
12 ******                                  CHAINN  ANO
13 JJ600UJS出荷確定データ更新              CHAINN  A   job
14 ******                                  CHAINN  ANO
15 ******                                  CHAINN  ANO
16 ******                                  CHAINN  ANO
17 JJ550UJS確定未処理・生成未処理リスト    CHAINN  A   job
18 ******                                  CHAINN  ANO
19 ******                                  CHAINN  ANO
20 JJ8000JS出荷指図残リスト                CHAINN  A   job
21 JJ200 JS　◎日次更新◎                  CHAINN  A   job
22 ******                                  CHAINN  ANO
23 JJN510JS玉島出荷　品名別出荷数合計表    CHAINN  A   job
24 ******                                  CHAINN  ANO
25 JJN550JS玉島入荷　品名別入荷数合計表    CHAINN  A   job
26 ******                                  CHAINN  ANO
27 ******                                  CHAINN  ANO
28 JJN400JS得意先別・品名別　出荷日報      CHAINN  A   job
29 ******                                  CHAINN  ANO
30 JJN460JS日付倉庫品名別　出荷数明細表    CHAINN  A   job
31 ******                                  CHAINN  ANO
32 ******                                  CHAINN  ANO
33 ******                                  CHAINN  ANO
34 ******  ------------------------------  CHAINN  ANO
35 JTO35LLM受信分出荷指図再発行（当日以前  CHAINN  ANO exec
36 JJO36LJS　　 　　　〃             白紙  CHAINN  A   job
37 ******  ------------------------------  CHAINN  ANO
38 ******                                  CHAINN  ANO
39 ******  ******************************  CHAINN  ANO
40 ******                                  CHAINN  ANO
/>
