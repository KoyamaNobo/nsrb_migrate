      ********************************************
      *****     履物製品発注入庫ファイル     *****
      *****    (  HSHNF 256/1 KEY 1-18  )    *****
      ********************************************
       01  HSHNF.
           02  HSHNF_PNAME1   PIC  X(005) VALUE "HSHNF".
           02  F              PIC  X(001).
           02  HSHNF_LNAME    PIC  X(005) VALUE "HSHNF".
           02  F              PIC  X(001).
           02  HSHNF_KEY1     PIC  X(100) VALUE SPACE.
           02  HSHNF_KEY2     PIC  X(100) VALUE SPACE.
           02  HSHNF_SORT     PIC  X(100) VALUE SPACE.
           02  HSHNF_IDLST    PIC  X(100) VALUE SPACE.
           02  HSHNF_RES      USAGE  POINTER.
       01  HSHN-R.
           02  HSHN-KEY.
             03  HSHN-RNO     PIC  9(008).
             03  HSHN-RNOD  REDEFINES HSHN-RNO.
               04  HSHN-RSN   PIC  9(002).
               04  HSHN-RNG   PIC  9(004).
               04  HSHN-RND   PIC  9(002).
             03  HSHN-DATE    PIC  9(008).
             03  HSHN-NGP   REDEFINES HSHN-DATE.
               04  HSHN-NEN   PIC  9(004).
               04  HSHN-GP    PIC  9(004).
             03  HSHN-SNO     PIC  9(002).
           02  HSHN-SCD       PIC  9(004).
           02  HSHN-HCD       PIC  9(006).
           02  HSHN-ASUD.
             03  HSHN-SUD  OCCURS   4.                                   ｻｲｽﾞ
               04  HSHN-ASU  OCCURS  10.
                 05  HSHN-SU  PIC S9(004).                               数量
           02  HSHN-KBNO      PIC  9(006).
           02  HSHN-HPC       PIC  9(001).
           02  HSHN-KRC       PIC  9(001).
           02  HSHN-UNO       PIC  9(006).
           02  F              PIC  X(054).
       77  F                  PIC  X(001).
