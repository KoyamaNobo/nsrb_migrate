      ************************************************
      *****    赤ちゃん本舗納品先ファイル        *****
      *****           ( AHNHF )   64/4           *****
      ************************************************
       01  AHNHF.
           02  AHNHF_PNAME1   PIC  X(005) VALUE "AHNHF".
           02  F              PIC  X(001).
           02  AHNHF_LNAME    PIC  X(005) VALUE "AHNHF".
           02  F              PIC  X(001).
           02  AHNHF_KEY1     PIC  X(100) VALUE SPACE.
           02  AHNHF_SORT     PIC  X(100) VALUE SPACE.
           02  AHNHF_IDLST    PIC  X(100) VALUE SPACE.
           02  AHNHF_RES      USAGE  POINTER.
       01  AHNH-R.
           02  AHNH-KEY.
             03  AHNH-STC     PIC  9(007).
           02  AHNH-NHSN      PIC  N(016).
           02  AHNH-CTC       PIC  9(007).
           02  AHNH-CSC.
             03  AHNH-TCD     PIC  9(004).
             03  AHNH-CCD     PIC  9(003).
           02  AHNH-HP        PIC  X(001).
           02  F              PIC  X(010).
       77  F                  PIC X(1).
