      ********************************************
      ***** 旧 得意先品名別　単価ファイル    *****
      *****    MIX  DATE=OTHTD (42/8)        *****
      *****         KEY1=OTHTM1    (1-11)    *****
      *****         KEY2=OTHTM2    (5-11)    *****
      ********************************************
       01  OTHTM.
           02  OTHTM_PNAME1    PIC  X(006) VALUE "OTHTM1".
           02  F               PIC  X(001).
           02  OTHTM_PNAME2    PIC  X(006) VALUE "OTHTM2".
           02  F               PIC  X(001).
           02  OTHTM_LNAME     PIC  X(005) VALUE "OTHTM".
           02  F               PIC  X(001).
           02  OTHTM_KEY1      PIC  X(100) VALUE SPACE.
           02  OTHTM_KEY2      PIC  X(100) VALUE SPACE.
           02  OTHTM_KEY3      PIC  X(100) VALUE SPACE.
           02  OTHTM_SORT      PIC  X(100) VALUE SPACE.
           02  OTHTM_IDLST     PIC  X(100) VALUE SPACE.
           02  OTHTM_RES       USAGE  POINTER.
       01  OTHT-R.
           02  OTHT-KD.
             03  OTHT-KEY.
               04  OTHT-TCD    PIC  9(004).
               04  OTHT-HCD.
                 05  OTHT-HCDF PIC  9(004).
                 05  OTHT-HCDR PIC  9(002).
               04  OTHT-SIZ    PIC  9(001).
             03  OTHT-TCD1     PIC  9(004).
           02  OTHT-KDD   REDEFINES OTHT-KD.
             03  OTHT-TCD3     PIC  9(004).
             03  OTHT-KEY2.
               04  OTHT-HCD2   PIC  9(006).
               04  OTHT-SIZ2   PIC  9(001).
               04  OTHT-TCD2   PIC  9(004).
           02  OTHT-T          PIC  9(005).
           02  F               PIC  X(014).
           02  OTHT-TNC        PIC  9(002).
           02  OTHT-NG         PIC  9(006).
           02  OTHT-NGL   REDEFINES OTHT-NG.
             03  F             PIC  9(002).
             03  OTHT-NGS      PIC  9(004).
       77  F                   PIC  X(001).
