       01  BS.
           02  BS_PNAME1       PIC  X(004) VALUE "BS-K".
           02  F               PIC  X(001).
           02  BS_LNAME        PIC  X(002) VALUE "BS".
           02  F               PIC  X(001).
           02  BS_KEY1         PIC  X(100) VALUE SPACE.
           02  BS_SORT         PIC  X(100) VALUE SPACE.
           02  BS_IDLST        PIC  X(100) VALUE SPACE.
           02  BS_RES          USAGE  POINTER.
       01  BS-REC.
           02  BS-KEY          PIC X(3).
           02  BS-LIN          PIC 9.
           02  BS-DR.
             03  BS-GKBDR      PIC 9.
             03  BS-PKBDR      PIC 9.
             03  BS-NAMDR      PIC X(20).
             03  BS-NAMDRR     REDEFINES    BS-NAMDR.
               04  BS-NAMDR1   PIC N(10).
             03  BS-KINDR      PIC S9(11).
           02  BS-CR.
             03  BS-GKBCR      PIC 9.
             03  BS-PKBCR      PIC 9.
             03  BS-NAMCR      PIC X(20).
             03  BS-NAMCRR     REDEFINES    BS-NAMCR.
               04  BS-NAMCR1   PIC N(10).
             03  BS-KINCR      PIC S9(11).
           02  BS-RKB          PIC 9.
           02  FILLER          PIC X(14).
       77  F                   PIC X(1).
