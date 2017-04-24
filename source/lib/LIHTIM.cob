      *********************************************
      *****     óöï®êªïiíIâµì¸óÕÉ}ÉXÉ^Å[      *****
      *****      ( HTIM )   85/3              *****
      *********************************************
       01  HTI-M.
           02  HTI-M_PNAME1   PIC  X(004) VALUE "HTIM".
           02  F              PIC  X(001).
           02  HTI-M_LNAME    PIC  X(005) VALUE "HTI-M".
           02  F              PIC  X(001).
           02  HTI-M_KEY1     PIC  X(100) VALUE SPACE.
           02  HTI-M_KEY2     PIC  X(100) VALUE SPACE.
           02  HTI-M_SORT     PIC  X(100) VALUE SPACE.
           02  HTI-M_IDLST    PIC  X(100) VALUE SPACE.
           02  HTI-M_RES      USAGE  POINTER.
       01  HTI-R.
           02  HTI-KEY.
             03  HTI-DNO.                                               ì`ï[áÇ
               04  HTI-DNO1   PIC  9(005).                              ì`ï[áÇ
               04  HTI-DNO2   PIC  X(001).
             03  HTI-GNO      PIC  9(001).                              çsáÇ
           02  HTI-SNO        PIC  9(001).
           02  HTI-HCD        PIC  9(006).                              ïiñº∫∞ƒﬁ
           02  HTI-SIZ        PIC  9(001).
           02  HTI-SUD.                                                 íIâµêî
             03  HTI-SU       PIC S9(006)  OCCURS  10.
           02  HTI-ASU   REDEFINES  HTI-SUD.
             03  HTI-SU01     PIC S9(006).
             03  HTI-SU02     PIC S9(006).
             03  HTI-SU03     PIC S9(006).
             03  HTI-SU04     PIC S9(006).
             03  HTI-SU05     PIC S9(006).
             03  HTI-SU06     PIC S9(006).
             03  HTI-SU07     PIC S9(006).
             03  HTI-SU08     PIC S9(006).
             03  HTI-SU09     PIC S9(006).
             03  HTI-SU10     PIC S9(006).
           02  HTI-BC.
             03  HTI-BC1      PIC  9(002).
             03  HTI-BC2      PIC  9(002).
             03  HTI-BC3      PIC  9(002).
           02  HTI-ISU        PIC  9(003).
           02  HTI-NC         PIC  9(001).
       77  F                  PIC  X(001).
