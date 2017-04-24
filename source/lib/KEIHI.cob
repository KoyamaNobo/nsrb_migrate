       01  HH-F.
           02  HH-F_PNAME1    PIC  X(007) VALUE "KEIHI-K".
           02  F              PIC  X(001).
           02  HH-F_LNAME     PIC  X(004) VALUE "HH-F".
           02  F              PIC  X(001).
           02  HH-F_KEY1      PIC  X(100) VALUE SPACE.
           02  HH-F_KEY2      PIC  X(100) VALUE SPACE.
           02  HH-F_KEY3      PIC  X(100) VALUE SPACE.
           02  HH-F_SORT      PIC  X(100) VALUE SPACE.
           02  HH-F_IDLST     PIC  X(100) VALUE SPACE.
           02  HH-F_RES       USAGE  POINTER.
      *****¹²Ë  Ï½À°***************  (128/2)
       01  HH-R.
           02  HH-KEY.
               03  HH-BUCD.
                   04  HH-BU1  PIC 99.
                   04  HH-BU2  PIC 99.
               03  HH-KACD     PIC 9(4).
               03  HH-HOCD     PIC 9(4).
           02  HH-GOCD         PIC 9(3).
           02  HH-GYO          PIC 9.
           02  HH-TOUKI.
               03  HH-GEL      PIC S9(11)     COMP-3  OCCURS 15.
           02  FILLER          PIC X(22).
       77  F                   PIC X(1).
