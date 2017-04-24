      ***************************************************
      *****     óöï®Å@ÇhÇmÇcÇdÇwÇdÇcÅ@ÇqÇcÇa  Å@    *****
      *****     (  HIMD  256/1)                     *****
      ***************************************************
       01  HI-M.
           02  HI-M_PNAME1    PIC  X(006) VALUE "HIM-DB".
           02  F              PIC  X(001).
           02  HI-M_LNAME     PIC  X(004) VALUE "HI-M".
           02  F              PIC  X(001).
           02  HI-M_KEY1      PIC  X(100) VALUE SPACE.
           02  HI-M_KEY2      PIC  X(100) VALUE SPACE.
           02  HI-M_SORT      PIC  X(100) VALUE SPACE.
           02  HI-M_IDLST     PIC  X(100) VALUE SPACE.
           02  HI-M_RES       USAGE  POINTER.
       01  HI-R.
           02  HI-KEY2.
             03  HI-MHCD      PIC  9(006).
             03  HI-MHCDD REDEFINES HI-MHCD.
               04  HI-MHCD1   PIC  9(004).
               04  HI-MHCD2   PIC  9(002).
             03  HI-HCD       PIC  9(006).
             03  HI-KEY   REDEFINES HI-HCD.
               04  HI-HCD1    PIC  9(004).
               04  HI-HCD2    PIC  9(002).
           02  HI-NAME        PIC  N(024).
           02  HI-BC.
             03  HI-BCD12.
               04  HI-BCD1    PIC  9(003).
               04  HI-BCW1 REDEFINES HI-BCD1.
                 05  HI-BC1   PIC  9(002).
                 05  HI-BC21  PIC  9(001).
               04  HI-BC22    PIC  9(001).
             03  HI-BCW12 REDEFINES HI-BCD12.
               04  F          PIC  9(002).
               04  HI-BC2     PIC  9(002).
             03  HI-BC3       PIC  9(002).
           02  HI-ASSD.
             03  HI-SSD   OCCURS  4.
               04  HI-SS      PIC  9(010).
           02  HI-ASKD  REDEFINES HI-ASSD.
             03  HI-SKD   OCCURS  4.
               04  HI-SK    OCCURS 10.
                 05  HI-S     PIC  9(001).
           02  HI-AHSD  REDEFINES HI-ASSD.
             03  HI-HSD.
               04  HI-SS1     PIC  9(010).                              ª≤Ωﬁ1
               04  HI-SD1   REDEFINES HI-SS1.
                 05  HI-S1    OCCURS  10  PIC  9(001).
               04  HI-SS2     PIC  9(010).                              ª≤Ωﬁ2
               04  HI-SD2    REDEFINES HI-SS2.
                 05  HI-S2    OCCURS  10  PIC  9(001).
               04  HI-SS3     PIC  9(010).                              ª≤Ωﬁ3
               04  HI-SD3    REDEFINES HI-SS3.
                 05  HI-S3    OCCURS  10  PIC  9(001).
               04  HI-SS4     PIC  9(010).                              ª≤Ωﬁ4
               04  HI-SD4    REDEFINES HI-SS4.
                 05  HI-S4    OCCURS  10  PIC  9(001).
           02  HI-SB          PIC  9(005).
           02  HI-FT          PIC  9(005).
      *    [   êªë¢å¥âø   ]
           02  HI-ZRG         PIC  9(005).
           02  HI-SKG         PIC  9(005).
           02  HI-GKG         PIC  9(005).
           02  HI-KNG         PIC  9(004).
           02  HI-KT          PIC  9(005).
      *
           02  HI-TCD         PIC  9(004).
           02  HI-ISU         PIC  9(003).
           02  HI-KRC         PIC  9(001).
           02  HI-SCC         PIC  9(001).
           02  HI-BMC         PIC  9(002).
           02  HI-BMNO        PIC  9(001).
           02  HI-YG          PIC  9(005).
           02  HI-HKB         PIC  9(001).
           02  F              PIC  X(045).
      *
           02  HI-UNG         PIC  9(006).
           02  HI-NNG         PIC  9(006).
           02  HI-OL          PIC  X(001).
           02  HI-CS          PIC  N(010).
           02  F              PIC  X(006).
           02  HI-DNG         PIC  9(006).
           02  HI-SNG         PIC  9(004).                              ìoò^ì˙ït
           02  HI-SNGD    REDEFINES HI-SNG.
             03  HI-SNEN      PIC  9(002).
             03  HI-SGET      PIC  9(002).
           02  HI-ENG         PIC  9(004).                              îpé~ì˙ït
           02  HI-ENGD    REDEFINES HI-ENG.
             03  HI-ENEN      PIC  9(002).
             03  HI-EGET      PIC  9(002).
       77  F                  PIC  X(001).
