      ************************************************************
      *    部門別経費配列マスタ         16 REC  / 16 BLK         *
      ************************************************************
       01  BKHHAI-K.
           02  BKHHAI-K_PNAME1   PIC  X(008) VALUE "BKHHAI-K".
           02  F                 PIC  X(001).
           02  BKHHAI-K_LNAME    PIC  X(008) VALUE "BKHHAI-K".
           02  F                 PIC  X(001).
           02  BKHHAI-K_KEY1     PIC  X(100) VALUE SPACE.
           02  BKHHAI-K_KEY2     PIC  X(100) VALUE SPACE.
           02  BKHHAI-K_SORT     PIC  X(100) VALUE SPACE.
           02  BKHHAI-K_IDLST    PIC  X(100) VALUE SPACE.
           02  BKHHAI-K_RES      USAGE  POINTER.
       01  BKHHAI-REC.
           02  BKHHAI-KEY.
             03  BKHHAI-PG       PIC 9(02).
             03  BKHHAI-LN       PIC 9.
           02  BKHHAI-BUCD       PIC 9(04).
           02  FILLER            PIC X(09).
       77  F                     PIC X(1).
