      ************************************************************
      *    部門別製造配列マスタ         16 REC  / 16 BLK         *
      ************************************************************
       01  BGNHAI.
           02  BGNHAI_PNAME1     PIC  X(008) VALUE "BGNHAI-K".
           02  F                 PIC  X(001).
           02  BGNHAI_LNAME      PIC  X(006) VALUE "BGNHAI".
           02  F                 PIC  X(001).
           02  BGNHAI_KEY1       PIC  X(100) VALUE SPACE.
           02  BGNHAI_KEY2       PIC  X(100) VALUE SPACE.
           02  BGNHAI_SORT       PIC  X(100) VALUE SPACE.
           02  BGNHAI_IDLST      PIC  X(100) VALUE SPACE.
           02  BGNHAI_RES        USAGE  POINTER.
       01  BGNHAI-REC.
           02  BGNHAI-KEY.
             03  BGNHAI-PG       PIC 9(02).
             03  BGNHAI-LN       PIC 9.
           02  BGNHAI-BUCD       PIC 9(04).
           02  FILLER            PIC X(09).
       77  F                     PIC X(1).
