      ********************************************
      *****     ìæà”êÊï îÑä|ä«óùÉtÉ@ÉCÉã     *****
      *****         (  TUKF  64/4  )         *****
      ********************************************
       01  TUKF.
           02  TUKF_PNAME1    PIC  X(005) VALUE "TUKF1".
           02  F              PIC  X(001).
           02  TUKF_PNAME2    PIC  X(005) VALUE "TUKF2".
           02  F              PIC  X(001).
           02  TUKF_LNAME     PIC  X(004) VALUE "TUKF".
           02  F              PIC  X(001).
           02  TUKF_KEY1      PIC  X(100) VALUE SPACE.
           02  TUKF_KEY2      PIC  X(100) VALUE SPACE.
           02  TUKF_KEY3      PIC  X(100) VALUE SPACE.
           02  TUKF_KEY4      PIC  X(100) VALUE SPACE.
           02  TUKF_SORT      PIC  X(100) VALUE SPACE.
           02  TUKF_IDLST     PIC  X(100) VALUE SPACE.
           02  TUKF_RES       USAGE  POINTER.
       01  TUK-R.
           02  TUK-KEY.                                                 KEY
             03  TUK-TCD      PIC  9(004).                              ìæà”êÊC
             03  TUK-DAI.                                               ë‰í†áÇ
               04  TUK-DN1    PIC  X(003).
               04  TUK-DN2    PIC  9(006).
               04  TUK-DN3    PIC  9(001).
           02  TUK-KEY2.
             03  TUK-TCD2     PIC  9(004).
             03  TUK-DATE     PIC  9(008).
             03  TUK-NGP   REDEFINES TUK-DATE.
               04  TUK-NG.
                 05  TUK-NEN  PIC  9(004).
                 05  TUK-GET  PIC  9(002).
               04  TUK-PEY    PIC  9(002).
             03  TUK-DC       PIC  9(001).
             03  TUK-DNO      PIC  X(006).
             03  TUK-GNO      PIC  9(001).
           02  TUK-KIN        PIC S9(009).                              ã‡äz
           02  TUK-SHZ        PIC S9(007).                              è¡îÔê≈
           02  TUK-SKD        PIC  9(008).
           02  TUK-SKDD  REDEFINES TUK-SKD.
             03  TUK-SNEN     PIC  9(004).
             03  TUK-SGET     PIC  9(002).
             03  TUK-SPEY     PIC  9(002).
           02  TUK-DCC        PIC  9(001).
           02  TUK-TNC        PIC  9(002).
           02  TUK-BMC        PIC  9(001).                              ïîñÂC
           02  F              PIC  X(002).
       77  F                  PIC  X(001).
