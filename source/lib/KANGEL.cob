       01  KNG.
           02  KNG_PNAME1        PIC  X(010) VALUE "KAMOKU-KNG".
           02  F                 PIC  X(001).
           02  KNG_LNAME         PIC  X(003) VALUE "KNG".
           02  F                 PIC  X(001).
           02  KNG_KEY1          PIC  X(100) VALUE SPACE.
           02  KNG_KEY2          PIC  X(100) VALUE SPACE.
           02  KNG_SORT          PIC  X(100) VALUE SPACE.
           02  KNG_IDLST         PIC  X(100) VALUE SPACE.
           02  KNG_RES           USAGE  POINTER.
       01  KNG-R.
           02  KNG-KEY.
             03  K-ACCD          PIC 9(4).
             03  K-HOCD          PIC 9(4).
           02  KNGNM             PIC X(20).
           02  KNGNMN  REDEFINES  KNGNM  PIC N(10).
           02  KNGTAX            PIC X(01).                             �ېŋ敪
           02  FILLER            PIC X(3).
       77  F                     PIC X(1).
