      ***  ���������}�X�^ (85/3)
       01  GEN.
           02  GEN_PNAME1      PIC  X(007) VALUE "GENKA-F".
           02  F               PIC  X(001).
           02  GEN_LNAME       PIC  X(003) VALUE "GEN".
           02  F               PIC  X(001).
           02  GEN_KEY1        PIC  X(100) VALUE SPACE.
           02  GEN_SORT        PIC  X(100) VALUE SPACE.
           02  GEN_IDLST       PIC  X(100) VALUE SPACE.
           02  GEN_RES         USAGE  POINTER.
       01  PL-REC.
           02  PL-KEY          PIC X(3).
           02  PL-LIN          PIC 9.
           02  PL-GKB          PIC 9.
           02  PL-NAM          PIC X(20).
           02  PL-NAMN     REDEFINES   PL-NAM   PIC N(10).
           02  PL-YY.
             03  PL-ZENKI      PIC S9(11).
             03  PL-TOUKI      PIC S9(11).
           02  PL-MM.
             03  PL-ZENMM      PIC S9(11).
             03  PL-TOUMM      PIC S9(11).
           02  PL-URIKB        PIC X.
           02  PL-PKB          PIC 9.
           02  PL-TANA         PIC 9.
           02  PL-YM.
             03  PL-YYWK       PIC 99.
             03  PL-MMWK       PIC 99.
           02  FILLER          PIC X(9).
       77  F                   PIC X(1).
