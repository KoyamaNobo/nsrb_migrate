/RUN #MENUP,TYP=LM,FIL=SYS@LML;
00 　　　月次処理                  JIP
01 TSG980LM手形未変換チェックリスト        CHAINN  ANO exec
02 ******  ------------------------------  CHAINN  ANO
03 TSG510LM手形日計表                   !  CHAINN  ANO exec
04 ******                                  CHAINN  ANO
05 TG05  JS銀行別　割引手形帳           !  CHAINN  A   job
06 ******                                  CHAINN  ANO
07 TG10  JS受取･支払･保有手形　明細表   !  CHAINN  A   job
08 ******                                  CHAINN  ANO
09 TG20  JS割引手形次月分　決済予定表   !  CHAINN  A   job
10 TG25  JS期日別・相手科目別　支払手形 !  CHAINN  A   job
11 ******  ------------------------------  CHAINN  ANO
12 TG50  JS月次  データセーブ・更新        CHAINN  A   job
13 ******  ------------------------------  CHAINN  ANO
14 ******                                  CHAINN  ANO
15 TG55  JS割手・支手落ち込み　振替伝票 #  CHAINN  A   job
16 ******                                  CHAINN  ANO
17 TG90  JS取引先別　支手サイト一覧表   !  CHAINN  A   job
18 ******  ******************************  CHAINN  ANO
19 TSR200LM領収書（控）　作成           !  CHAINN  ANO exec
/>
