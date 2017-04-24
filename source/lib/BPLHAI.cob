      ****************************************************************
      *                                                              *
      *      < ÌÞÓÝÍÞÂ¿Ý´·Ê²ÚÂ Ì§²Ù >           * 16 REC / 16 B *    *
      *                                         * INDEXED *          *
      ****************************************************************
       01  BPLHAI.
           02  BPLHAI_PNAME1       PIC  X(008) VALUE "BPLHAI-K".
           02  F                   PIC  X(001).
           02  BPLHAI_LNAME        PIC  X(006) VALUE "BPLHAI".
           02  F                   PIC  X(001).
           02  BPLHAI_KEY1         PIC  X(100) VALUE SPACE.
           02  BPLHAI_KEY2         PIC  X(100) VALUE SPACE.
           02  BPLHAI_SORT         PIC  X(100) VALUE SPACE.
           02  BPLHAI_IDLST        PIC  X(100) VALUE SPACE.
           02  BPLHAI_RES          USAGE  POINTER.
       01  BPLHAI-REC.
           02  BPLHAI-KEY.
               03  BPLHAI-PG       PIC 9(02).
               03  BPLHAI-LN       PIC 9(01).
           02  BPLHAI-BUCD         PIC 9(04).
           02  FILLER              PIC X(09).
       77  F                       PIC X(1).
