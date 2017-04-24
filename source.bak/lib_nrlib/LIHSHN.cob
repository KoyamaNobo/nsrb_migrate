000010********************************************
000020*****     履物製品発注入庫ファイル     *****
000030*****    (  HSHNF 256/1 KEY 1-18  )    *****
000040********************************************
000050 FD  HSHNF
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HSHNF".
000090 01  HSHN-R.
000100     02  HSHN-KEY.
000110       03  HSHN-RNO     PIC  9(008).
000120       03  HSHN-RNOD  REDEFINES HSHN-RNO.
000130         04  HSHN-RSN   PIC  9(002).
000140         04  HSHN-RNG   PIC  9(004).
000150         04  HSHN-RND   PIC  9(002).
000160       03  HSHN-DATE    PIC  9(008).
000170       03  HSHN-NGP   REDEFINES HSHN-DATE.
000180         04  HSHN-NEN   PIC  9(004).
000190         04  HSHN-GP    PIC  9(004).
000200       03  HSHN-SNO     PIC  9(002).
000210     02  HSHN-SCD       PIC  9(004).
000220     02  HSHN-HCD       PIC  9(006).
000230     02  HSHN-ASUD.
000240       03  HSHN-SUD  OCCURS   4.                                   ｻｲｽﾞ
000250         04  HSHN-ASU  OCCURS  10.
000260           05  HSHN-SU  PIC S9(004).                               数量
000270     02  HSHN-KBNO      PIC  9(006).
000280     02  HSHN-HPC       PIC  9(001).
000290     02  HSHN-KRC       PIC  9(001).
000300     02  HSHN-UNO       PIC  9(006).
000310     02  F              PIC  X(054).
