      ***********************************************
      **   　　受注ダミーファイル（化成用）　　　　**
      *****         ( G D M M  )  16/16         *****
      ***********************************************
       01  GDMM.
           02  GDMM_PNAME1  PIC  X(004) VALUE "GDMM".
           02  F            PIC  X(001).
           02  GDMM_LNAME   PIC  X(004) VALUE "GDMM".
           02  F            PIC  X(001).
           02  GDMM_KEY1    PIC  X(100) VALUE SPACE.
           02  GDMM_SORT    PIC  X(100) VALUE SPACE.
           02  GDMM_IDLST   PIC  X(100) VALUE SPACE.
           02  GDMM_RES     USAGE  POINTER.
       01  GDMM-R.
           02  GDMM-KEY.                                                KEY
               03  GDMM-01  PIC 9(01).
           02  F            PIC X(15).
       77  F                PIC X(1).
