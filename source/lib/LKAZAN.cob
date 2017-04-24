      ******************************************************************
      *                  科目残高マスタ                                *
      *                   <  341 REC  3 BLOCK  >                       *
      *                   <       INDEXED      >                       *
      ******************************************************************
      *
       01  KZM-F.
           02  KZM-F_PNAME1         PIC  X(007) VALUE "KAZAN-K".
           02  F                    PIC  X(001).
           02  KZM-F_LNAME          PIC  X(005) VALUE "KZM-F".
           02  F                    PIC  X(001).
           02  KZM-F_KEY1           PIC  X(100) VALUE SPACE.
           02  KZM-F_SORT           PIC  X(100) VALUE SPACE.
           02  KZM-F_IDLST          PIC  X(100) VALUE SPACE.
           02  KZM-F_RES            USAGE  POINTER.
      *
       01  KZM-R.
           02  KZM-KEY.
               03  KZM-KMCD         PIC   9(04).
           02  KZM-ZAN              PIC  S9(11)  COMP-3.
           02  KZM-TJ.
               03  KZM-TJIS      OCCURS  15.
                   04  KZM-TJKR     PIC  S9(11)  COMP-3.
                   04  KZM-TJKS     PIC  S9(11)  COMP-3.
           02  KZM-ZJ.
               03  KZM-ZJIS      OCCURS  12.
                   04  KZM-ZJKR     PIC  S9(11)  COMP-3.
                   04  KZM-ZJKS     PIC  S9(11)  COMP-3.
           02  FILLER               PIC   X(07).
       77  F                        PIC  X(001).
