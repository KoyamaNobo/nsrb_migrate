000010***************************************************
000020*****     成型・検査データファイル            *****
000030*****     (  HSKDF 170/3 , KEY 1-21  )        *****
000040***************************************************
000050 FD  HSKDF
000060     BLOCK  3 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HSKDF".
000090 01  HSKD-R.
000100     02  HSKD-KEY.
000110       03  HSKD-RNO.
000120         04  HSKD-NEN   PIC  9(004).                              年
000130         04  HSKD-DNO.
000140           05  HSKD-GET PIC  9(002).                              月
000150           05  HSKD-NO  PIC  9(003).                              №
000160           05  HSKD-SUB PIC  9(001).                              枝番
000170       03  HSKD-DATE    PIC  9(008).                              日付
000180       03  HSKD-NGPD  REDEFINES HSKD-DATE.
000190         04  HSKD-NGD   PIC  9(006).
000200         04  HSKD-NGDD  REDEFINES HSKD-NGD.
000210           05  HSKD-NEND PIC  9(004).
000220           05  HSKD-GETD PIC  9(002).
000230         04  HSKD-PEYD  PIC  9(002).
000240       03  HSKD-SEQ     PIC  9(002).                              SEQ №
000250       03  HSKD-SKC     PIC  9(001).                              成型検査
000260     02  HSKD-HCD       PIC  9(006).                              ｺｰﾄﾞ
000270     02  HSKD-ASUD.                                               実績数
000280       03  HSKD-ASU  OCCURS   4.
000290         04  HSKD-SUD   OCCURS  10.
000300           05  HSKD-SU  PIC S9(003).
000310     02  HSKD-END       PIC  9(001).                              完了区分
000320     02  HSKD-UNO       PIC  9(006).                              受入№
000330*****02  F              PIC  X(011).                              D.020318
000340     02  F              PIC  X(007).                              I.020318
000350     02  HSKD-PRI       PIC  9(003).                              印字区分
000360     02  HSKD-IPD       PIC  9(002).                              I.020318
000370     02  HSKD-HHD       PIC  9(002).                              I.020318
000380     02  HSKD-GHD       PIC  9(002).                              I.020318
000390*****02  HSKD-HHC       PIC  9(001).                              D.020318
000400*****02  HSKD-GHC       PIC  9(001).                              D.020318
