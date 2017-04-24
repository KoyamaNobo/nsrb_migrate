000010*********************************************
000020*****     óöï®êªïiéÛï•É}ÉXÉ^Å[Å@Å@      *****
000030*****      ( HUHM )    102/5            *****
000040*********************************************
000050 FD  HUH-M
000060     BLOCK  5 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HUHM".
000090 01  HUH-R.
000100     02  HUH-KEY.
000110       03  HUH-HCD      PIC  9(006).                              ïiñº∫∞ƒﬁ
000120       03  HUH-HCDD  REDEFINES HUH-HCD.                           I.930209
000130         04  HUH-HCD1   PIC  9(004).                              I.930209
000140         04  HUH-HCD2   PIC  9(002).                              I.930209
000150*
000160     02  HUH-NGD.
000170*****  03  HUH-NEN      PIC  9(002).                              D.970709
000180       03  HUH-NEN      PIC  9(004).                              I.970709
000190       03  HUH-GET      PIC  9(002).
000200     02  HUH-NG    REDEFINES HUH-NGD  PIC 9(006).                 I.970709
000210*****02  HUH-NG    REDEFINES HUH-NGD  PIC 9(004).                 D.970709
000220*
000230     02  HUH-D.
000240       03  HUH-ZS       PIC S9(006).                              ëOåJêî
000250       03  HUH-ZK       PIC S9(009).                              ëOåJäz
000260       03  HUH-NS       PIC S9(007).                              ì¸å…êî
000270       03  HUH-NK       PIC S9(010).                              ì¸å…äz
000280       03  HUH-SS       PIC S9(008).                              èoâ◊êî
000290       03  HUH-SK       PIC S9(010).                              èoâ◊äz
000300       03  HUH-YS       PIC S9(006).                              óÇåJêî
000310       03  HUH-YK       PIC S9(009).                              óÇåJäz
000320       03  HUH-UG       PIC S9(010).                              îÑè„å¥âø
000330*
000340     02  HUH-BCD12.                                               I.030514
000350       03  HUH-BCD1     PIC  9(003).                              I.030514
000360       03  HUH-BCW1  REDEFINES HUH-BCD1.                          I.030514
000370         04  HUH-BC1    PIC  9(002).                              I.030514
000380         04  HUH-BC21   PIC  9(001).                              I.030514
000390*****02  HUH-BC1        PIC  9(002).                              D.030514
000400*****02  HUH-BCD1  REDEFINES HUH-BC1.                             D.940622
000410*****  03  HUH-BC11     PIC  9(001).                              D.940622
000420*****  03  HUH-BC12     PIC  9(001).                              D.940622
000430*****02  HUH-BC2        PIC  9(002).                              D.030514
000440*****02  HUH-BCD2  REDEFINES HUH-BC2.                             D.030514
000450*****  03  HUH-BC21     PIC  9(001).                              D.030514
000460       03  HUH-BC22     PIC  9(001).
000470     02  HUH-BCW12 REDEFINES HUH-BCD12.                           I.030514
000480       03  F            PIC  9(002).                              I.030514
000490       03  HUH-BC2      PIC  9(002).                              I.030514
000500     02  HUH-BC3        PIC  9(002).                              ï™óﬁCD3
000510     02  HUH-BCD3  REDEFINES HUH-BC3.
000520       03  HUH-BC31     PIC  9(001).
000530       03  HUH-BC32     PIC  9(001).
000540     02  HUH-BMC        PIC  9(002).                              I.020314
000550     02  HUH-BMNO       PIC  9(001).                              I.020425
000560     02  HUH-BC4        PIC  9(001).                              I.090121
000570*
000580     02  F              PIC  X(005).                              I.090121
000590*****02  F              PIC  X(006).                              D.090121
000600*****02  F              PIC  X(007).                              D.020425
000610*****02  F              PIC  X(009).                              D.020314
000620*****02  HUH-PBC        PIC  9(001).                              D.960905
000630*****02  F              PIC  X(001).                              D.990122
000640*****02  HUH-SOC        PIC  9(001).                              D.990122
000650*
000660*****02  F              PIC  X(007).                              D.990122
000670*****02  F              PIC  X(009).                              D.970709
