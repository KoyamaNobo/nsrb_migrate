000010*********************************************
000020*****     ¨»iI΅όΝ}X^[      *****
000030*****      ( HTIM )   85/3              *****
000040*********************************************
000050 FD  HTI-M
000060*****BLOCK  4 RECORDS                                             D.990121
000070     BLOCK  3 RECORDS                                             I.990121
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "HTIM".
000100 01  HTI-R.
000110     02  HTI-KEY.
000120       03  HTI-DNO.                                               `[
000130         04  HTI-DNO1   PIC  9(005).                              `[
000140         04  HTI-DNO2   PIC  X(001).
000150       03  HTI-GNO      PIC  9(001).                              s
000160     02  HTI-SNO        PIC  9(001).                              I.990121
000170     02  HTI-HCD        PIC  9(006).                              iΌΊ°Δή
000180     02  HTI-SIZ        PIC  9(001).                              I.990121
000190     02  HTI-SUD.                                                 I΅
000200       03  HTI-SU       PIC S9(006)  OCCURS  10.                  I.990121
000210*****  03  HTI-SU       PIC S9(005)  OCCURS  10.                  D.990121
000220     02  HTI-ASU   REDEFINES  HTI-SUD.
000230       03  HTI-SU01     PIC S9(006).                              I.990121
000240       03  HTI-SU02     PIC S9(006).                              I.990121
000250       03  HTI-SU03     PIC S9(006).                              I.990121
000260       03  HTI-SU04     PIC S9(006).                              I.990121
000270       03  HTI-SU05     PIC S9(006).                              I.990121
000280       03  HTI-SU06     PIC S9(006).                              I.990121
000290       03  HTI-SU07     PIC S9(006).                              I.990121
000300       03  HTI-SU08     PIC S9(006).                              I.990121
000310       03  HTI-SU09     PIC S9(006).                              I.990121
000320       03  HTI-SU10     PIC S9(006).                              I.990121
000330     02  HTI-BC.                                                  I.990121
000340       03  HTI-BC1      PIC  9(002).                              I.990121
000350       03  HTI-BC2      PIC  9(002).                              I.990121
000360       03  HTI-BC3      PIC  9(002).                              I.990121
000370     02  HTI-ISU        PIC  9(003).                              I.000420
000380     02  HTI-NC         PIC  9(001).                              I.110404
000390*****02  F              PIC  X(001).                              D.110404
000400*****02  F              PIC  X(004).                              D.000420
000410*****  03  HTI-SU01     PIC S9(005).                              D.990121
000420*****  03  HTI-SU02     PIC S9(005).                              D.990121
000430*****  03  HTI-SU03     PIC S9(005).                              D.990121
000440*****  03  HTI-SU04     PIC S9(005).                              D.990121
000450*****  03  HTI-SU05     PIC S9(005).                              D.990121
000460*****  03  HTI-SU06     PIC S9(005).                              D.990121
000470*****  03  HTI-SU07     PIC S9(005).                              D.990121
000480*****  03  HTI-SU08     PIC S9(005).                              D.990121
000490*****  03  HTI-SU09     PIC S9(005).                              D.990121
000500*****  03  HTI-SU10     PIC S9(005).                              D.990121
000510*****02  HTI-SNO        PIC  9(001).                              D.990121
