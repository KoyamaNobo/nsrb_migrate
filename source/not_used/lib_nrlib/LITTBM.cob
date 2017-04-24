000010********************************************************
000020*****     昇給・賞与　考課配分テーブルマスター     *****
000030*****               TTBM   ( 512/1 )               *****
000040********************************************************
000050 FD  TTB-M
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "TTBM".
000090 01  TTB-R.
000100     02  TT-KEY         PIC  X(002).                              01
000110*
000120     02  TT-NTT.                                                  年齢配分
000130*****  03  TT-NT    OCCURS 90  PIC  9(004).                       D.951207
000140       03  TT-NT    OCCURS 45  PIC  9(004).                       I.951207
000150*****02  TT-NTTD  REDEFINES TT-NTT.                               D.951207
000160*****  03  TT-NMD.                                                D.951207
000170*****    04  TT-NMT   OCCURS 45  PIC  9(004).                     D.951207
000180*****  03  TT-NWD.                                                D.951207
000190*****    04  TT-NWT   OCCURS 45  PIC  9(004).                     D.951207
000200*
000210     02  TT-KTT.                                                  考課配分
000220       03  TT-KT    OCCURS 50  PIC  9(006).                       I.970702
000230*****  03  TT-KT    OCCURS 60  PIC  9(005).                       D.951207
000240*****  03  TT-KT    OCCURS 50  PIC  9(005).                       D.970702
000250     02  TT-KTTD  REDEFINES TT-KTT.
000260       03  TT-KMD.
000270         04  TT-KMT   OCCURS  5  PIC  X(030).                     I.970702
000280*****    04  TT-KMT   OCCURS  6  PIC  X(025).                     D.951207
000290*****    04  TT-KMT   OCCURS  5  PIC  X(025).                     D.970702
000300       03  TT-KWD.
000310         04  TT-KWT   OCCURS  5  PIC  X(030).                     I.970702
000320*****    04  TT-KWT   OCCURS  6  PIC  X(025).                     D.951207
000330*****    04  TT-KWT   OCCURS  5  PIC  X(025).                     D.970702
000340     02  TT-KTTA  REDEFINES TT-KTT.
000350       03  TT-KTA   OCCURS 10  PIC  X(030).                       I.970702
000360*****  03  TT-KTA   OCCURS 12  PIC  X(025).                       D.951207
000370*****  03  TT-KTA   OCCURS 10  PIC  X(025).                       D.970702
000380*
000390*****02  F              PIC  X(088).                              D.951207
000400*****02  F              PIC  X(062).                              D.970702
000410     02  F              PIC  X(012).                              I.970702
000420*
000430     02  TT-SYD.                                                  賞与
000440       03  TT-SKT       PIC  9(001)V9(03).                        賞与箇月
000450       03  TT-KKT       PIC  9(001)V9(03).                        考課箇月
000460     02  TT-SKD.                                                  昇給
000470       03  TT-HBR       PIC  9(001)V9(03).                        基本給BU
000480       03  TT-HBK       PIC  9(001).                              切上区分
000490       03  TT-KBR       PIC  9(001)V9(03).                        勤務給BU
000500       03  TT-KBK       PIC  9(001).                              切上区分
