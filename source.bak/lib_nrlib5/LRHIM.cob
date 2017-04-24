000010***************************************************
000020*****     óöï®Å@ÇhÇmÇcÇdÇwÇdÇcÅ@ÇqÇcÇa  Å@    *****
000030*****     (  HIMD  256/1)                     *****
000040***************************************************
000050 FD  HI-M
000060     BLOCK  1 RECORDS
000070     LABEL RECORD IS STANDARD
000080     VALUE OF IDENTIFICATION "HIM-DB".
000090 01  HI-R.
000100     02  HI-KEY2.
000110       03  HI-MHCD      PIC  9(006).
000120       03  HI-MHCDD REDEFINES HI-MHCD.
000130         04  HI-MHCD1   PIC  9(004).
000140         04  HI-MHCD2   PIC  9(002).
000150       03  HI-HCD       PIC  9(006).
000160       03  HI-KEY   REDEFINES HI-HCD.
000170         04  HI-HCD1    PIC  9(004).
000180         04  HI-HCD2    PIC  9(002).
000190     02  HI-NAME        PIC  N(024).
000200     02  HI-BC.
000210       03  HI-BCD12.
000220         04  HI-BCD1    PIC  9(003).
000230         04  HI-BCW1 REDEFINES HI-BCD1.
000240           05  HI-BC1   PIC  9(002).
000250           05  HI-BC21  PIC  9(001).
000260         04  HI-BC22    PIC  9(001).
000270       03  HI-BCW12 REDEFINES HI-BCD12.
000280         04  F          PIC  9(002).
000290         04  HI-BC2     PIC  9(002).
000300       03  HI-BC3       PIC  9(002).
000310     02  HI-ASSD.
000320       03  HI-SSD   OCCURS  4.
000330         04  HI-SS      PIC  9(010).
000340     02  HI-ASKD  REDEFINES HI-ASSD.
000350       03  HI-SKD   OCCURS  4.
000360         04  HI-SK    OCCURS 10.
000370           05  HI-S     PIC  9(001).
000380     02  HI-AHSD  REDEFINES HI-ASSD.
000390       03  HI-HSD.
000400         04  HI-SS1     PIC  9(010).                              ª≤Ωﬁ1
000410         04  HI-SD1   REDEFINES HI-SS1.
000420           05  HI-S1    OCCURS  10  PIC  9(001).
000430         04  HI-SS2     PIC  9(010).                              ª≤Ωﬁ2
000440         04  HI-SD2    REDEFINES HI-SS2.
000450           05  HI-S2    OCCURS  10  PIC  9(001).
000460         04  HI-SS3     PIC  9(010).                              ª≤Ωﬁ3
000470         04  HI-SD3    REDEFINES HI-SS3.
000480           05  HI-S3    OCCURS  10  PIC  9(001).
000490         04  HI-SS4     PIC  9(010).                              ª≤Ωﬁ4
000500         04  HI-SD4    REDEFINES HI-SS4.
000510           05  HI-S4    OCCURS  10  PIC  9(001).
000520     02  HI-SB          PIC  9(005).
000530     02  HI-FT          PIC  9(005).
000540*    [   êªë¢å¥âø   ]
000550     02  HI-ZRG         PIC  9(005).
000560     02  HI-SKG         PIC  9(005).
000570     02  HI-GKG         PIC  9(005).
000580     02  HI-KNG         PIC  9(004).
000590     02  HI-KT          PIC  9(005).
000600*
000610     02  HI-TCD         PIC  9(004).
000620     02  HI-ISU         PIC  9(003).
000630     02  HI-KRC         PIC  9(001).
000640     02  HI-SCC         PIC  9(001).
000650     02  HI-BMC         PIC  9(002).
000660     02  HI-BMNO        PIC  9(001).
000670     02  HI-YG          PIC  9(005).
000680     02  HI-HKB         PIC  9(001).
000690     02  F              PIC  X(045).
000700*
000710     02  HI-UNG         PIC  9(006).
000720     02  HI-NNG         PIC  9(006).
000730     02  HI-OL          PIC  X(001).
000740     02  HI-CS          PIC  N(010).
000750     02  F              PIC  X(006).
000760     02  HI-DNG         PIC  9(006).
000770     02  HI-SNG         PIC  9(004).                              ìoò^ì˙ït
000780     02  HI-SNGD    REDEFINES HI-SNG.
000790       03  HI-SNEN      PIC  9(002).
000800       03  HI-SGET      PIC  9(002).
000810     02  HI-ENG         PIC  9(004).                              îpé~ì˙ït
000820     02  HI-ENGD    REDEFINES HI-ENG.
000830       03  HI-ENEN      PIC  9(002).
000840       03  HI-EGET      PIC  9(002).
