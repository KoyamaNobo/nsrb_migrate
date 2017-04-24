      *****************************************************************
      *    åàçœèÓïÒÉtÉ@ÉCÉã        ( 51/5 )  SEQUENTIAL               *
      *****************************************************************
       01  NS-KES.
           02  NS-KES_PNAME1     PIC  X(006) VALUE "NS-KES".
           02  F                 PIC  X(001).
           02  NS-KES_LNAME      PIC  X(006) VALUE "NS-KES".
           02  F                 PIC  X(001).
           02  NS-KES_KEY1       PIC  X(100) VALUE SPACE.
           02  NS-KES_SORT       PIC  X(100) VALUE SPACE.
           02  NS-KES_IDLST      PIC  X(100) VALUE SPACE.
           02  NS-KES_RES        USAGE  POINTER.
       01  KES-R.
           02  KES-01            PIC 9(01).
           02  KES-02            PIC 9(08).
           02  KES-02R  REDEFINES  KES-02.
             03  KES-021         PIC 9(04).
             03  KES-022         PIC 9(02).
             03  KES-023         PIC 9(02).
           02  KES-03            PIC 9(04).
           02  KES-04            PIC 9(04).
           02  KES-05            PIC 9(04).
           02  KES-06            PIC 9(02).
           02  KES-07            PIC 9(02).
           02  KES-08            PIC S9(10).
           02  KES-09            PIC 9(08).
           02  KES-09R  REDEFINES  KES-09.
             03  KES-091         PIC 9(04).
             03  KES-092         PIC 9(02).
             03  KES-093         PIC 9(02).
           02  FILLER            PIC X(08).
       77  F                     PIC X(01).
