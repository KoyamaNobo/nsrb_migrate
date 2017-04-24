      ****************************************************************
      *                                                              *
      *      < ÌÞÓÝÍÞÂ¾²¿Þ³¹ÞÝ¶»¸Ë®³Ì§²Ù >      *102 REC / 05 B *    *
      *                                         * INDEXED *          *
      ****************************************************************
       01  BGNPRN.
           02  BGNPRN_PNAME1       PIC  X(007) VALUE "BGN-PRN".
           02  F                   PIC  X(001).
           02  BGNPRN_LNAME        PIC  X(006) VALUE "BGNPRN".
           02  F                   PIC  X(001).
           02  BGNPRN_KEY1         PIC  X(100) VALUE SPACE.
           02  BGNPRN_KEY2         PIC  X(100) VALUE SPACE.
           02  BGNPRN_SORT         PIC  X(100) VALUE SPACE.
           02  BGNPRN_IDLST        PIC  X(100) VALUE SPACE.
           02  BGNPRN_RES          USAGE  POINTER.
       01  BPLPRN-REC.
           02  BPLPRN-KEY.
               03  BPLPRN-PG       PIC 9(02).
               03  BPLPRN-LNO      PIC 9(03).
           02  BPLPRN-GYO          PIC 9(01).
           02  BPLPRN-GKB          PIC 9(01).
           02  BPLPRN-NM           PIC N(10).
           02  BPLPRN-UKB          PIC X(01).
           02  BPLPRN-IKB          PIC 9(01).
           02  BPLPRN-ITEM.
             03  BPLPRN-AM         PIC S9(11)
                 OCCURS            5   TIMES.
           02  FILLER              PIC X(18).
       77  F                       PIC X(1).
