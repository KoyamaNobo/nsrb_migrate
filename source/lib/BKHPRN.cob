      ****************************************************************
      *                                                              *
      *      < ÌÞÓÝÍÞÂ¹²Ë »¸Ë®³Ì§²Ù >           *102 REC / 05 B *    *
      *                                         * INDEXED *          *
      ****************************************************************
       01  BKH-PRN.
           02  BKH-PRN_PNAME1      PIC  X(007) VALUE "BKH-PRN".
           02  F                   PIC  X(001).
           02  BKH-PRN_LNAME       PIC  X(007) VALUE "BKH-PRN".
           02  F                   PIC  X(001).
           02  BKH-PRN_KEY1        PIC  X(100) VALUE SPACE.
           02  BKH-PRN_KEY2        PIC  X(100) VALUE SPACE.
           02  BKH-PRN_KEY3        PIC  X(100) VALUE SPACE.
           02  BKH-PRN_SORT        PIC  X(100) VALUE SPACE.
           02  BKH-PRN_IDLST       PIC  X(100) VALUE SPACE.
           02  BKH-PRN_RES         USAGE  POINTER.
       01  BKHPRN-REC.
           02  BKHPRN-KEY.
               03  BKHPRN-PG       PIC 9(02).
               03  BKHPRN-ACCD.
                 04  BKHPRN-ACM    PIC 9(04).
                 04  BKHPRN-ACS    PIC 9(04).
           02  BKHPRN-GCD          PIC 9(03).
           02  BKHPRN-GYO          PIC 9(01).
           02  BKHPRN-ITEM.
               03  BKHPRN-IT  OCCURS   7   TIMES.
                   04  BKHPRN-GI   PIC S9(11).
           02  FILLER              PIC X(11).
       77  F                       PIC X(1).
