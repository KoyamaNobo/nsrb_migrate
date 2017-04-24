      ***********************************
      *    購買コントロールマスター     *
      *        KBNOM   (64/4)           *
      *        IS     KEY:1-5           *
      ***********************************
       01  KBNO-M.
           02  KBNO-M_PNAME1  PIC  X(005) VALUE "KBNOM".
           02  F              PIC  X(001).
           02  KBNO-M_LNAME   PIC  X(006) VALUE "KBNO-M".
           02  F              PIC  X(001).
           02  KBNO-M_KEY1    PIC  X(100) VALUE SPACE.
           02  KBNO-M_SORT    PIC  X(100) VALUE SPACE.
           02  KBNO-M_IDLST   PIC  X(100) VALUE SPACE.
           02  KBNO-M_RES     USAGE  POINTER.
       01  KBNO-R.
           02  BNO-KEY.                                                 KEY
             03  BNO-KEYD     PIC  X(002).
             03  F            PIC  X(003).
           02  BNO-RD1.                                                 01
             03  BNO-DATE     PIC  9(002).                              台帳日付
             03  BNO-SNG      PIC  9(006).
             03  BNO-ENG      PIC  9(006).
             03  F            PIC  X(045).
           02  BNO-RD2   REDEFINES BNO-RD1.                             02
             03  BNO-DNO1     PIC  9(006).                              仕入伝票
             03  F            PIC  X(053).
           02  BNO-RD3   REDEFINES BNO-RD1.
             03  BNO-DNO2     PIC  9(006).                              出庫伝票
             03  F            PIC  X(053).
       77  F                  PIC  X(001).
