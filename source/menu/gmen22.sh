/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 ＊＊　受注出荷作表他　＊＊      JIP
01 JJ2810JS倉別在庫表（実，有効在庫他）    CHAINN  A   job
02 ******                                  CHAINN  ANO
03 JJ3230JS品名倉別  在庫受払表            CHAINN  A   job
04 ******  ------------------------------  CHAINN  ANO
05 JJ690UJS受注日得意先別　受注残リスト    CHAINN  A   job
06 ******                                  CHAINN  ANO
07 JJ2350JS品名受注日別　受注残リスト      CHAINN  A   job
08 ******                                  CHAINN  ANO
09 JT297LLM受注残帳(品名別)                CHAINN  ANO exec
10 JJ2910JS受注他残帳(品名得意先別)　　    CHAINN  A   job
11 JJ2911JS　　〃　　(　　〃　  　)指図含  CHAINN  A   job
12 JJ2915JS　　〃（品名日付得意先別）〃    CHAINN  A   job
13 JJ2920JS受注他残帳(得意先・品名別)      CHAINN  A   job
14 JJ287UJS   〃   (品名担当得意先別)      CHAINN  A   job
15 ******                                  CHAINN  ANO
16 JJ2912JS受注数帳(品名・得意先別)　　    CHAINN  A   job
17 ******                                  CHAINN  ANO
18 JJ750UJS受注数合計表（得意先品名別）    CHAINN  A   job
19 ******                                  CHAINN  ANO
20 JJ790UJS受注他台帳                      CHAINN  A   job
21 JT052ULM送り状入力                      CHAINN  ANO exec
22 ******                                  CHAINN  ANO
23 JJ0540JS送り状発行　：　福山通運        CHAINN  A   job
24 JJ0580JS　　　　　　：　佐川急便        CHAINN  A   job
25 JJ0590JS　　　　　　：　西濃運輸        CHAINN  A   job
26 ******  ------------------------------  CHAINN  ANO
27 SYUKKAPM荷札・入日記入力・発行          CHAINN  ANO menu
28 ******  ------------------------------  CHAINN  ANO
29 HM30  JS担当別得意先名簿                CHAINN  A   job
30 ******                                  CHAINN  ANO
31 ******                                  CHAINN  ANO
32 HMM220LM直送先名簿(ｺｰﾄﾞ)                CHAINN  ANO exec
33 HM22  JS　　〃　　(ｱｲｳｴｵ)               CHAINN  A   job
34 ******                                  CHAINN  ANO
35 HMM046LM履物品名コード表                CHAINN  ANO exec
36 ******                                  CHAINN  ANO
37 HM15  JS担当得意先品名別単価リスト      CHAINN  A   job
38 ******                                  CHAINN  ANO
39 ******  ------------------------------  CHAINN  ANO
40 JJ2811JS取引先用有効在庫表              CHAINN  A   job
/>
