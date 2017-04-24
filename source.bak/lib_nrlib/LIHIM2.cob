000010***************************************************
000020*****     óöï®Å@ÇhÇmÇcÇdÇwÇdÇcÅ@É}ÉXÉ^Å[Å@    *****
000030*****     (  HIMD  256/1)                     *****
000040*****     (  HIM2  1-12 )                     *****
000050***************************************************
000060 FD  HI2-M
000070     BLOCK  1 RECORDS
000080     LABEL RECORD IS STANDARD
000090     VALUE OF IDENTIFICATION "HIM2".
000100 01  HI-R.
000110*    * * *   å≈íËçÄñ⁄   * * *
000120     02  HI-KEY2.
000130       03  HI-MHCD      PIC  9(006).
000140       03  HI-MHCDD REDEFINES HI-MHCD.
000150         04  HI-MHCD1   PIC  9(004).
000160         04  HI-MHCD2   PIC  9(002).
000170       03  HI-HCD       PIC  9(006).
000180       03  HI-KEY   REDEFINES HI-HCD.
000190         04  HI-HCD1    PIC  9(004).
000200         04  HI-HCD2    PIC  9(002).
000210     02  HI-NAME        PIC  N(024).                              ïiñº
000220     02  HI-BC.
000230       03  HI-BCD12.                                              I.030514
000240         04  HI-BCD1    PIC  9(003).                              I.030514
000250         04  HI-BCW1  REDEFINES HI-BCD1.                          I.030514
000260           05  HI-BC1   PIC  9(002).                              I.030514
000270           05  HI-BC21  PIC  9(001).                              I.030514
000280*****  03  HI-BC1       PIC  9(002).                              D.030514
000290*****  03  HI-BCD2.                                               D.030514
000300*****    04  HI-BC21    PIC  9(001).                              D.030514
000310         04  HI-BC22    PIC  9(001).
000320       03  HI-BCW12 REDEFINES HI-BCD12.                           I.030514
000330         04  F          PIC  9(002).                              I.030514
000340         04  HI-BC2     PIC  9(002).                              I.030514
000350*****  03  HI-BC2   REDEFINES HI-BCD2  PIC  9(002).               D.030514
000360       03  HI-BC3       PIC  9(002).
000370*    [   ª≤ΩﬁãÊï™ ì‡ñÛ   ]
000380     02  HI-ASSD.
000390       03  HI-SSD   OCCURS  4.
000400         04  HI-SS      PIC  9(010).
000410     02  HI-ASKD  REDEFINES HI-ASSD.
000420       03  HI-SKD   OCCURS  4.
000430         04  HI-SK    OCCURS 10.
000440           05  HI-S     PIC  9(001).
000450     02  HI-AHSD  REDEFINES HI-ASSD.
000460       03  HI-HSD.
000470         04  HI-SS1     PIC  9(010).                              ª≤Ωﬁ1
000480         04  HI-SD1   REDEFINES HI-SS1.
000490           05  HI-S1    OCCURS  10  PIC  9(001).
000500         04  HI-SS2     PIC  9(010).                              ª≤Ωﬁ2
000510         04  HI-SD2    REDEFINES HI-SS2.
000520           05  HI-S2    OCCURS  10  PIC  9(001).
000530         04  HI-SS3     PIC  9(010).                              ª≤Ωﬁ3
000540         04  HI-SD3    REDEFINES HI-SS3.
000550           05  HI-S3    OCCURS  10  PIC  9(001).
000560         04  HI-SS4     PIC  9(010).                              ª≤Ωﬁ4
000570         04  HI-SD4    REDEFINES HI-SS4.
000580           05  HI-S4    OCCURS  10  PIC  9(001).
000590     02  HI-SB          PIC  9(005).
000600     02  HI-FT          PIC  9(005).
000610*    [   êªë¢å¥âø   ]
000620*****02  HI-ZRG         PIC  9(005).                              D.090121
000630*****02  HI-SKG         PIC  9(005).                              D.090121
000640*****02  HI-GKG         PIC  9(005).                              D.090121
000650*****02  HI-KNG         PIC  9(004).                              D.090121
000660     02  F              PIC  X(019).                              I.090121
000670     02  HI-KT          PIC  9(005).
000680*
000690     02  HI-TCD         PIC  9(004).
000700     02  HI-ISU         PIC  9(003).
000710     02  HI-KRC         PIC  9(001).                              I.011030
000720     02  HI-SCC         PIC  9(001).                              I.011030
000730     02  HI-BMC         PIC  9(002).                              I.020314
000740     02  HI-BMNO        PIC  9(001).                              I.020425
000750     02  HI-YG          PIC  9(005).                              I.030805
000760     02  HI-HKB         PIC  9(001).                              *A040311
000770     02  HI-HPV         PIC  9(001).                              I.090121
000780     02  HI-BC4         PIC  9(001).                              I.090121
000790     02  HI-SSC         PIC  9(001).                              I.110224
000800     02  F              PIC  X(010).                              I.110224
000810*****02  F              PIC  X(011).                              D.110224
000820*****02  F              PIC  X(057).                              D.090121
000830*
000840     02  HI-SMS         PIC  N(016).                              I.090121
000850     02  HI-UNG         PIC  9(006).                              I.090121
000860     02  HI-NNG         PIC  9(006).                              I.090121
000870     02  HI-OL          PIC  X(001).                              I.020729
000880     02  HI-CS          PIC  N(010).
000890     02  F              PIC  X(006).
000900     02  HI-DNG         PIC  9(006).
000910     02  HI-SNG         PIC  9(004).                              ìoò^ì˙ït
000920     02  HI-SNGD    REDEFINES HI-SNG.
000930       03  HI-SNEN      PIC  9(002).
000940       03  HI-SGET      PIC  9(002).
000950     02  HI-ENG         PIC  9(004).                              îpé~ì˙ït
000960     02  HI-ENGD    REDEFINES HI-ENG.
000970       03  HI-ENEN      PIC  9(002).
000980       03  HI-EGET      PIC  9(002).
