      *****************************************************************
      *    ワークファイル          (  64/4 )  SEQUENTIAL              *
      *****************************************************************
       01  NS-W01.
           02  NS-W01_PNAME1     PIC  X(009) VALUE SPACE.
           02  F                 PIC  X(001).
           02  NS-W01_LNAME      PIC  X(006) VALUE "NS-W01".
           02  F                 PIC  X(001).
           02  NS-W01_KEY1       PIC  X(100) VALUE SPACE.
           02  NS-W01_SORT       PIC  X(100) VALUE SPACE.
           02  NS-W01_IDLST      PIC  X(100) VALUE SPACE.
           02  NS-W01_RES        USAGE  POINTER.
       01  W01-R.
           02  W01-01            PIC 9(01).
           02  W01-02            PIC 9(08).
           02  W01-02R  REDEFINES  W01-02.
             03  W01-021         PIC 9(04).
             03  W01-022         PIC 9(02).
             03  W01-023         PIC 9(02).
           02  W01-03            PIC 9(04).
           02  W01-04            PIC 9(04).
           02  W01-05            PIC 9(04).
           02  W01-06            PIC 9(02).
           02  W01-07            PIC 9(02).
           02  W01-08            PIC S9(10).
           02  W01-09            PIC 9(08).
           02  W01-09R  REDEFINES  W01-09.
             03  W01-091         PIC 9(04).
             03  W01-092         PIC 9(02).
             03  W01-093         PIC 9(02).
           02  FILLER            PIC X(21).
       77  F                     PIC X(01).
