      *    Ã·Ö³ Ï½À°        *
       01  TKI.
           02  TKI_PNAME1      PIC  X(008) VALUE "TEKIYO-K".
           02  F               PIC  X(001).
           02  TKI_LNAME       PIC  X(003) VALUE "TKI".
           02  F               PIC  X(001).
           02  TKI_KEY1        PIC  X(100) VALUE SPACE.
           02  TKI_SORT        PIC  X(100) VALUE SPACE.
           02  TKI_IDLST       PIC  X(100) VALUE SPACE.
           02  TKI_RES         USAGE  POINTER.
       01  TKI-R.
           02  TKI-KEY.
               03  TKI-01      PIC 9(3).                                “E—v‚b‚c
           02  TKI-02          PIC N(20).                               “E—v–¼
           02  FILLER          PIC X(8).
       77  F                   PIC X(1).
