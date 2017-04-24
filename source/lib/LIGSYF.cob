      *****************************************
      *****     生協出荷予定ファイル      *****
      *****      ( GSYF )  128/2          *****
      *****************************************
       01  GSYF.
           02  GSYF_PNAME1    PIC  X(008) VALUE "GSYF-RDB".
           02  F              PIC  X(001).
           02  GSYF_LNAME     PIC  X(004) VALUE "GSYF".
           02  F              PIC  X(001).
           02  GSYF_KEY1      PIC  X(100) VALUE SPACE.
           02  GSYF_SORT      PIC  X(100) VALUE SPACE.
           02  GSYF_IDLST     PIC  X(100) VALUE SPACE.
           02  GSYF_RES       USAGE  POINTER.
       01  GSY-R.
           02  GSY-KEY.
             03  GSY-HCD      PIC  9(006).
             03  GSY-TCD      PIC  9(004).
             03  GSY-ND       PIC  9(008).
             03  GSY-SIZ      PIC  9(001).
           02  GSY-ASU.
             03  GSY-SUD   OCCURS  10.
               04  GSY-SU     PIC S9(006).
           02  F              PIC  9(002).
           02  GSY-BCD1       PIC  9(003).
           02  GSY-TEK        PIC  N(016).
           02  F              PIC  X(005).
           02  GSY-DC         PIC  9(001).
           02  GSY-ID         PIC  9(006).
       77  F                  PIC X(1).
