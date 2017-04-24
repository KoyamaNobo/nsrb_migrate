      *    ÌÞÓÝÒ² Ì§²Ù        *
       01  BNM.
           02  BNM_PNAME1       PIC  X(008) VALUE "BUMON-K ".
           02  F                PIC  X(001).
           02  BNM_LNAME        PIC  X(003) VALUE "BNM".
           02  F                PIC  X(001).
           02  BNM_KEY1         PIC  X(100) VALUE SPACE.
           02  BNM_SORT         PIC  X(100) VALUE SPACE.
           02  BNM_IDLST        PIC  X(100) VALUE SPACE.
           02  BNM_RES          USAGE  POINTER.
       01  BNM-REC.
           02  BNM-KEY.
               03  BNM-BU       PIC 9(02).
               03  BNM-KA       PIC 9(02).
           02  BNMNM            PIC X(20).
           02  BNMNMN  REDEFINES  BNMNM  PIC N(10).
           02  BNM-PL.
               03  BNMPLLB  OCCURS 3   TIMES.
                   04  BNM-PLPG PIC 9(02).
                   04  BNM-PLLN PIC 9(01).
           02  BNM-KH.
               03  BNM-KHLB OCCURS 6   TIMES.
                   04  BNM-KHPG PIC 9(02).
                   04  BNM-KHLN PIC 9(01).
           02  BNM-BUMONKBN     PIC 9(01).
           02  BNM-GN.
               03  BNM-GNLB OCCURS 3  TIMES.
                   04  BNM-GNPG PIC 9(2).
                   04  BNM-GNLN PIC 9.
           02  FILLER           PIC X(03).
       77  F                    PIC X(1).
