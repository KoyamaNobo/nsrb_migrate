       01  BM.
           02  BM_PNAME1       PIC  X(006) VALUE "BANK-K".
           02  F               PIC  X(001).
           02  BM_LNAME        PIC  X(002) VALUE "BM".
           02  F               PIC  X(001).
           02  BM_KEY1         PIC  X(100) VALUE SPACE.
           02  BM_SORT         PIC  X(100) VALUE SPACE.
           02  BM_IDLST        PIC  X(100) VALUE SPACE.
           02  BM_RES          USAGE  POINTER.
       01  BM-REC.
           02  BM-KEY.
             03  BANKCD        PIC 9(4).
           02  BANKNM          PIC X(20).
           02  BANKNMN  REDEFINES  BANKNM  PIC N(10).
           02  KOMOK       OCCURS  12.
             03  RATE          PIC 9(4).
             03  DBFOR         PIC S9(10).
             03  DCTERM        PIC S9(10).
             03  MCTZN         PIC S9(12).
           02  WARIRATE        PIC 9(4).
           02  WARIKIN         PIC S9(10).
           02  WARIKINH        PIC S9(10).
           02  WARIKINS        PIC S9(12).
           02  FILLER          PIC X(20).
       77  F                   PIC X(1).
