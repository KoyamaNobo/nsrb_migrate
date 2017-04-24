000010*    ¼Ü¹ ²ÝÌß¯Ä         *     (170/3)
000020 FD  SDI
000030     BLOCK      CONTAINS     3      RECORDS                       H 90.12
000040     LABEL      RECORD       STANDARD
000050     VALUE      OF           IDENTIFICATION      "SIWAKE-I".
000060 01  SDI-REC.
000070     02  SDI-KEY.
000080*****  03  SDIYMD        PIC 9(6).                                D.971111
000090       03  SDIYMD        PIC 9(8).                                I.971111
000100       03  SDIJNO        PIC 9(6).
000110       03  SDILNO        PIC 9(2).
000120     02  SDIKARI.
000130       03  KRCD.
000140         04  KRCDM       PIC 9(4).
000150         04  KRCDS       PIC 9(4).
000160       03  KRSECT        PIC 9(4).
000170       03  KRSKN         PIC 9(3).
000180       03  KRTAX         PIC X(1).
000190       03  KRKIN         PIC S9(10).
000200       03  KR-TB         PIC 9(2).
000210*****  03  KRTNO         PIC X(8).                                D.980224
000220     02  SDIKASI.
000230       03  KSCD.
000240         04  KSCDM       PIC 9(4).
000250         04  KSCDS       PIC 9(4).
000260       03  KSSECT        PIC 9(4).
000270       03  KSSKN         PIC 9(3).
000280       03  KSTAX         PIC X(1).
000290       03  KSKIN         PIC S9(10).
000300       03  KS-TB         PIC 9(2).
000310*****  03  KSTNO         PIC X(8).                                D.980224
000320     02  SDICUST         PIC 9(5).
000330     02  SDITEKICD       PIC 9(3).
000340     02  SDITEKI         PIC N(20).
000350     02  SDISIN          PIC X(1).
000360*****02  FILLER          PIC X(34).                               D.970729
000370*****02  FILLER          PIC X(20).                               D.980217
000380     02  SDINAMEN        PIC N(10).                               I.980217
000390     02  F               PIC X(10).                               I.980217
000400     02  SDIETAX         PIC X(1).                                I.970729
000410*****02  FILLER          PIC X(13).                               D.971111
000420*****02  FILLER          PIC X(11).                               D.980217
000430     02  FILLER          PIC X(17).                               D.980217
000440     02  SDIDEL          PIC X(1).
