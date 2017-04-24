      *****************************************************************
      *    “`•[‡‚ƒtƒ@ƒCƒ‹          ( 256/1 )  INDEXED                 *
      *****************************************************************
       01  NS-DNO.
           02  NS-DNO_PNAME1        PIC  X(006) VALUE "NS-DNO".
           02  F                    PIC  X(001).
           02  NS-DNO_LNAME         PIC  X(006) VALUE "NS-DNO".
           02  F                    PIC  X(001).
           02  NS-DNO_KEY1          PIC  X(100) VALUE SPACE.
           02  NS-DNO_SORT          PIC  X(100) VALUE SPACE.
           02  NS-DNO_IDLST         PIC  X(100) VALUE SPACE.
           02  NS-DNO_RES           USAGE  POINTER.
       01  DNO-R.
           02  DNO1-R.
               03  DNO1-KEY.
                 04  DNO1-01          PIC X(02).
               03  DNO1-02.
                 04  DNO1-021         PIC 9(06).
                 04  DNO1-022         PIC 9(06).
                 04  DNO1-023         PIC 9(06).
               03  FILLER             PIC X(236).
      *    
           02  DNO2-R.
               03  DNO2-KEY.
                 04  DNO2-01          PIC X(02).
               03  DNO2-02.
                 04  DNO2-021     OCCURS 10.
                   05  DNO2-0211      PIC 9(04).
                   05  DNO2-0212      PIC 9(04).
               03  DNO2-03.
                 04  DNO2-031         PIC 9(04).
                 04  DNO2-032         PIC 9(04).
               03  DNO2-04.
                 04  DNO2-041.
                   05  DNO2-0411     OCCURS 2.
                     06  DNO2-04111   PIC 9(04).
                     06  DNO2-04112   PIC 9(04).
                 04  DNO2-042.
                   05  DNO2-0421     OCCURS 2.
                     06  DNO2-04211   PIC 9(04).
                     06  DNO2-04212   PIC 9(04).
               03  DNO2-05.
                 04  DNO2-051     OCCURS 3.
                   05  DNO2-0511     OCCURS 2.
                     06  DNO2-05111   PIC 9(04).
                     06  DNO2-05112   PIC 9(04).
               03  DNO2-06.
                 04  DNO2-061     OCCURS 3.
                   05  DNO2-0611      PIC 9(03).
                   05  DNO2-0612      PIC 9(03).
               03  DNO2-07.
                 04  DNO2-071.
                   05  DNO2-0711      PIC 9(03)     OCCURS 2.
                 04  DNO2-072.
                   05  DNO2-0721      PIC 9(03)     OCCURS 2.
               03  FILLER             PIC X(56).
       77  F                          PIC  X(001).
