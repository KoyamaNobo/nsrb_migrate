      *****************************************************************
      *    仕訳接続ワーク　        ( 170/3 )  SEQUENTIAL              *
      *****************************************************************
       01  NS-SIW.
           02  NS-SIW_PNAME1     PIC  X(006) VALUE "NS-SIW".
           02  F                 PIC  X(001).
           02  NS-SIW_LNAME      PIC  X(006) VALUE "NS-SIW".
           02  F                 PIC  X(001).
           02  NS-SIW_KEY1       PIC  X(100) VALUE SPACE.
           02  NS-SIW_SORT       PIC  X(100) VALUE SPACE.
           02  NS-SIW_IDLST      PIC  X(100) VALUE SPACE.
           02  NS-SIW_RES        USAGE  POINTER.
       01  SIW-R.
           02  SIW-01            PIC 9(08).
           02  SIW-02            PIC 9(06).
           02  SIW-03            PIC 9(02).
           02  SIW-04.
             03  SIW-041.
               04  SIW-0411      PIC 9(04).
               04  SIW-0412      PIC 9(04).
             03  SIW-042         PIC 9(04).
             03  SIW-043         PIC 9(03).
             03  SIW-044         PIC X(01).
             03  SIW-045         PIC S9(10).
             03  SIW-046         PIC 9(02).
           02  SIW-05.
             03  SIW-051.
               04  SIW-0511      PIC 9(04).
               04  SIW-0512      PIC 9(04).
             03  SIW-052         PIC 9(04).
             03  SIW-053         PIC 9(03).
             03  SIW-054         PIC X(01).
             03  SIW-055         PIC S9(10).
             03  SIW-056         PIC 9(02).
           02  SIW-06            PIC 9(05).
           02  SIW-07            PIC 9(03).
           02  SIW-08            PIC N(20).
           02  SIW-09            PIC X(01).
           02  SIW-10            PIC N(10).
           02  F                 PIC X(10).
           02  SIW-90            PIC 9(01).
           02  SIW-97.
             03  SIW-971         PIC 9(08).
             03  SIW-971R REDEFINES  SIW-971.
               04  SIW-9711      PIC 9(04).
               04  SIW-9712      PIC 9(02).
               04  SIW-9713      PIC 9(02).
             03  SIW-972         PIC 9(08).
             03  SIW-972R REDEFINES  SIW-972.
               04  SIW-9721      PIC 9(04).
               04  SIW-9722      PIC 9(02).
               04  SIW-9723      PIC 9(02).
           02  SIW-98            PIC 9(01).
           02  SIW-99            PIC X(01).
       77  F                     PIC X(01).
