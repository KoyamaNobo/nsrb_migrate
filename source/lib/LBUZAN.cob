      ******************************************************************
      *                   < лч ╩ч щ   о  ╫  ю  >                       *
      *                   <  341 REC  3 BLOCK  >                       *
      *                   <       INDEXED      >                       *
      ******************************************************************
      *
       01  BZM-F.
           02  BZM-F_PNAME1         PIC  X(007) VALUE "BUZAN-K".
           02  F                    PIC  X(001).
           02  BZM-F_LNAME          PIC  X(005) VALUE "BZM-F".
           02  F                    PIC  X(001).
           02  BZM-F_KEY1           PIC  X(100) VALUE SPACE.
           02  BZM-F_KEY2           PIC  X(100) VALUE SPACE.
           02  BZM-F_KEY3           PIC  X(100) VALUE SPACE.
           02  BZM-F_SORT           PIC  X(100) VALUE SPACE.
           02  BZM-F_IDLST          PIC  X(100) VALUE SPACE.
           02  BZM-F_RES            USAGE  POINTER.
      *
       01  BZM-REC.
           02  BZM-KEY.
               03  BZM-BMON         PIC   9(04).
               03  BZM-BMONR        REDEFINES    BZM-BMON.
                   04  BZM-BMCD     PIC   9(02).
                   04  BZM-YOBI     PIC   9(02).
               03  BZM-KMCD         PIC   9(04).
           02  BZM-TJ.
               03  BZM-TJIS      OCCURS  15.
                   04  BZM-TJKR     PIC  S9(11)  COMP-3.
                   04  BZM-TJKS     PIC  S9(11)  COMP-3.
           02  BZM-ZJ.
               03  BZM-ZJIS      OCCURS  12.
                   04  BZM-ZJKR     PIC  S9(11)  COMP-3.
                   04  BZM-ZJKS     PIC  S9(11)  COMP-3.
           02  FILLER               PIC   X(09).
       77  F                        PIC X(1).
