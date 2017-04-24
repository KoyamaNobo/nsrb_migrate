      *********************************************
      *****     óöï®êªïiéÛï•É}ÉXÉ^Å[Å@Å@      *****
      *****      ( HUHM )    102/5            *****
      *********************************************
       01  HUH-M.
           02  HUH-M_PNAME1   PIC  X(004) VALUE "HUHM".
           02  F              PIC  X(001).
           02  HUH-M_LNAME    PIC  X(005) VALUE "HUH-M".
           02  F              PIC  X(001).
           02  HUH-M_KEY1     PIC  X(100) VALUE SPACE.
           02  HUH-M_SORT     PIC  X(100) VALUE SPACE.
           02  HUH-M_IDLST    PIC  X(100) VALUE SPACE.
           02  HUH-M_RES      USAGE  POINTER.
       01  HUH-R.
           02  HUH-KEY.
             03  HUH-HCD      PIC  9(006).                              ïiñº∫∞ƒﬁ
             03  HUH-HCDD  REDEFINES HUH-HCD.
               04  HUH-HCD1   PIC  9(004).
               04  HUH-HCD2   PIC  9(002).
      *
           02  HUH-NGD.
             03  HUH-NEN      PIC  9(004).
             03  HUH-GET      PIC  9(002).
           02  HUH-NG    REDEFINES HUH-NGD  PIC 9(006).
      *
           02  HUH-D.
             03  HUH-ZS       PIC S9(006).                              ëOåJêî
             03  HUH-ZK       PIC S9(009).                              ëOåJäz
             03  HUH-NS       PIC S9(007).                              ì¸å…êî
             03  HUH-NK       PIC S9(010).                              ì¸å…äz
             03  HUH-SS       PIC S9(008).                              èoâ◊êî
             03  HUH-SK       PIC S9(010).                              èoâ◊äz
             03  HUH-YS       PIC S9(006).                              óÇåJêî
             03  HUH-YK       PIC S9(009).                              óÇåJäz
             03  HUH-UG       PIC S9(010).                              îÑè„å¥âø
      *
           02  HUH-BCD12.
             03  HUH-BCD1     PIC  9(003).
             03  HUH-BCW1  REDEFINES HUH-BCD1.
               04  HUH-BC1    PIC  9(002).
               04  HUH-BC21   PIC  9(001).
             03  HUH-BC22     PIC  9(001).
           02  HUH-BCW12 REDEFINES HUH-BCD12.
             03  F            PIC  9(002).
             03  HUH-BC2      PIC  9(002).
           02  HUH-BC3        PIC  9(002).                              ï™óﬁCD3
           02  HUH-BCD3  REDEFINES HUH-BC3.
             03  HUH-BC31     PIC  9(001).
             03  HUH-BC32     PIC  9(001).
           02  HUH-BMC        PIC  9(002).
           02  HUH-BMNO       PIC  9(001).
           02  HUH-BC4        PIC  9(001).
      *
           02  F              PIC  X(005).
       77  F                  PIC  X(001).
