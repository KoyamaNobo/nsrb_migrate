      ******************************************************************
      *                  補助残高マスタ                                *
      *                   <  256 REC  1 BLOCK  >                       *
      *                   <       INDEXED      >                       *
      ******************************************************************
      *
       01  HZM-F.
           02  HZM-F_PNAME1         PIC  X(007) VALUE "HOZAN-K".
           02  F                    PIC  X(001).
           02  HZM-F_LNAME          PIC  X(005) VALUE "HZM-F".
           02  F                    PIC  X(001).
           02  HZM-F_KEY1           PIC  X(100) VALUE SPACE.
           02  HZM-F_KEY2           PIC  X(100) VALUE SPACE.
           02  HZM-F_SORT           PIC  X(100) VALUE SPACE.
           02  HZM-F_IDLST          PIC  X(100) VALUE SPACE.
           02  HZM-F_RES            USAGE  POINTER.
      *
       01  HZM-R.
           02  HZM-KEY.
               03  HZM-KMCD         PIC   9(04).
               03  HZM-HOCD         PIC   9(04).
           02  HZM-ZAN              PIC  S9(11)  COMP-3.
           02  HZM-TJ.
               03  HZM-TJIS      OCCURS  15.
                   04  HZM-TJKR     PIC  S9(11)  COMP-3.
                   04  HZM-TJKS     PIC  S9(11)  COMP-3.
           02  FILLER               PIC   X(62).
       77  F                        PIC X(1).
