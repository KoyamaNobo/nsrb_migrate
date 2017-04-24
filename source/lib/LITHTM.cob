      ********************************************
      *****    得意先品名別　単価ファイル    *****
      *****    MIX  DATE=THTD (42/8)         *****
      *****         KEY1=THTM1     (1-11)    *****
      *****         KEY2=THTM2     (5-11)    *****
      ********************************************
       01  THTM.
           02  THTM_PNAME1    PIC  X(005) VALUE "THTM1".
           02  F              PIC  X(001).
           02  THTM_PNAME2    PIC  X(005) VALUE "THTM2".
           02  F              PIC  X(001).
           02  THTM_LNAME     PIC  X(004) VALUE "THTM".
           02  F              PIC  X(001).
           02  THTM_KEY1      PIC  X(100) VALUE SPACE.
           02  THTM_KEY2      PIC  X(100) VALUE SPACE.
           02  THTM_KEY3      PIC  X(100) VALUE SPACE.
           02  THTM_SORT      PIC  X(100) VALUE SPACE.
           02  THTM_IDLST     PIC  X(100) VALUE SPACE.
           02  THTM_RES       USAGE  POINTER.
       01  THT-R.
           02  THT-KD.
             03  THT-KEY.
               04  THT-TCD    PIC  9(004).
               04  THT-HCD.
                 05  THT-HCDF PIC  9(004).
                 05  THT-HCDR PIC  9(002).
               04  THT-SIZ    PIC  9(001).
             03  THT-TCD1     PIC  9(004).
           02  THT-KDD   REDEFINES THT-KD.
             03  THT-TCD3     PIC  9(004).
             03  THT-KEY2.
               04  THT-HCD2   PIC  9(006).
               04  THT-SIZ2   PIC  9(001).
               04  THT-TCD2   PIC  9(004).
           02  THT-T          PIC  9(005).
      *           02  F              PIC  X(014).
           02  THT-TT         PIC  9(005).
           02  F              PIC  X(009).
           02  THT-TNC        PIC  9(002).
           02  THT-NG         PIC  9(006).
           02  THT-NGL   REDEFINES THT-NG.
             03  F            PIC  9(002).
             03  THT-NGS      PIC  9(004).
       77  F                  PIC  X(001).
