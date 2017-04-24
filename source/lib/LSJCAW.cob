      **************************************************
      *****    ＪＣＡ手順ファイル（ワークマン）    *****
      *****           ( JCAWF )  256/1             *****
      **************************************************
       01  JCAWF.
           02  JCAWF_PNAME1    PIC  X(005) VALUE "JCAWF".
           02  F               PIC  X(001).
           02  JCAWF_LNAME     PIC  X(005) VALUE "JCAWF".
           02  F               PIC  X(001).
           02  JCAWF_KEY1      PIC  X(100) VALUE SPACE.
           02  JCAWF_SORT      PIC  X(100) VALUE SPACE.
           02  JCAWF_IDLST     PIC  X(100) VALUE SPACE.
           02  JCAWF_RES       USAGE  POINTER.
       01  JCAW-R.
           02  JCAW-R1         PIC  X(128).
           02  JCAW-R2         PIC  X(128).
       77  F                   PIC  X(001).
