      ***  仕訳ワーク　     (256/1)
       01  SDW.
           02  SDW_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  SDW_LNAME        PIC  X(003) VALUE "SDW".
           02  F                PIC  X(001).
           02  SDW_KEY1         PIC  X(100) VALUE SPACE.
           02  SDW_SORT         PIC  X(100) VALUE SPACE.
           02  SDW_IDLST        PIC  X(100) VALUE SPACE.
           02  SDW_RES          USAGE  POINTER.
      *
       01  SW-REC.
           02  WHKACD1.
             03  WHACCNTCD      PIC 9(4).
             03  WHHOACCNT      PIC 9(4).
           02  WHTRDATE         PIC 9(8).
           02  WHJUNLNO         PIC 9(6).
           02  WHLINENO         PIC 9(2).
           02  WHDR-CR          PIC 9(1).
           02  WHSECTCD         PIC 9(4).
           02  WHSKINCD         PIC 9(3).
           02  WHTAXKB          PIC X(1).
           02  WHAMOUNT         PIC S9(10).
           02  WHTEG-BAN        PIC 9(2).
           02  WHKACD2.
             03  WHOPPCD        PIC 9(4).
             03  WHHOOPPCD      PIC 9(4).
           02  WHCUSTCD         PIC 9(5).
           02  WHTEKICD         PIC 9(3).
           02  WHTEKIYO         PIC N(20).
           02  WHKEIHIKB        PIC 9(1).
           02  WHNAMEN          PIC N(10).
           02  WHACCNTCD2       PIC 9(4).
           02  WHTRDATE2        PIC 9(8).
           02  WHJUNLNO2        PIC 9(6).
           02  WHLINENO2        PIC 9(2).
           02  WHDR-CR2         PIC 9(1).
           02  F                PIC X(26).
           02  WHCOM            PIC 9(1).
           02  FILLER           PIC X(86).
       77  F                    PIC X(01).
