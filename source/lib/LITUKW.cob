      ********************************************
      *****     得意先別売掛管理ファイル     *****
      *****         (  TUKFW 64/4  )         *****
      ********************************************
       01  TUKF.
           02  TUKF_PNAME1    PIC  X(005) VALUE "TUKFW".
           02  F              PIC  X(001).
           02  TUKF_LNAME     PIC  X(004) VALUE "TUKF".
           02  F              PIC  X(001).
           02  TUKF_KEY1      PIC  X(100) VALUE SPACE.
           02  TUKF_SORT      PIC  X(100) VALUE SPACE.
           02  TUKF_IDLST     PIC  X(100) VALUE SPACE.
           02  TUKF_RES       USAGE  POINTER.
       01  TUK-R.
           02  TUK-KEY.                                                 KEY
             03  TUK-TCD      PIC  9(004).                              得意先C
             03  TUK-DAI.                                               台帳№
               04  TUK-DN1    PIC  X(003).
               04  TUK-DN2    PIC  9(006).
               04  TUK-DN3    PIC  9(001).
           02  F              PIC  9(004).
           02  TUK-DATE       PIC  9(008).                              日付
           02  TUK-NGP   REDEFINES TUK-DATE.
             03  TUK-NG.
               04  TUK-NEN    PIC  9(004).
               04  TUK-GET    PIC  9(002).
             03  TUK-PEY      PIC  9(002).
           02  TUK-DC         PIC  9(001).                              区分
           02  TUK-DNO        PIC  X(006).
           02  TUK-GNO        PIC  9(001).
           02  TUK-KIN        PIC S9(009).                              金額
           02  TUK-SHZ        PIC S9(007).                              消費税
           02  TUK-SKD        PIC  9(008).
           02  TUK-SKDD  REDEFINES TUK-SKD.
             03  TUK-SNEN     PIC  9(004).
             03  TUK-SGET     PIC  9(002).
             03  TUK-SPEY     PIC  9(002).
           02  TUK-DCC        PIC  9(001).
           02  TUK-TNC        PIC  9(002).
           02  TUK-BMC        PIC  9(001).                              部門C
           02  F              PIC  X(002).
       77  F                  PIC  X(001).
