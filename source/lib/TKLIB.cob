      *    ÄØË·»· Ï½À°        *
       01  TK.
           02  TK_PNAME1       PIC  X(008) VALUE "NFTORI-M".
           02  F               PIC  X(001).
           02  TK_LNAME        PIC  X(002) VALUE "TK".
           02  F               PIC  X(001).
           02  TK_KEY1         PIC  X(100) VALUE SPACE.
           02  TK_SORT         PIC  X(100) VALUE SPACE.
           02  TK_IDLST        PIC  X(100) VALUE SPACE.
           02  TK_RES          USAGE  POINTER.
       01  TK-REC.
           02  TK-KEY.
               03  TK-CD       PIC 9(5).
               03  TK-CDD  REDEFINES  TK-CD.
                 04  TK-CD1    PIC 9(01).
                 04  TK-CD2    PIC 9(04).
           02  TK-NAME         PIC X(20).
           02  TK-NAMEN  REDEFINES  TK-NAME      PIC N(10).
           02  TK-PRC          PIC 9(2).
           02  TK-BKC          PIC 9(2).
           02  TK-TCD          PIC 9(4).
           02  TK-WNK          PIC 9(1).
           02  TK-SS           PIC 9(2).
           02  FILLER          PIC X(2).
           02  TK-NG           PIC 9(4).
       77  F                   PIC X(1).
