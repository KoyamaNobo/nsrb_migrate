      ***********************************************
      *****     工品品名材料使用マスター　      *****
      *****         (  KHJM  32/8  )            *****
      ***********************************************
       01  KHJ-M.
           02  KHJ-M_PNAME1   PIC  X(004) VALUE "KHJM".
           02  F              PIC  X(001).
           02  KHJ-M_LNAME    PIC  X(005) VALUE "KHJ-M".
           02  F              PIC  X(001).
           02  KHJ-M_KEY1     PIC  X(100) VALUE SPACE.
           02  KHJ-M_SORT     PIC  X(100) VALUE SPACE.
           02  KHJ-M_IDLST    PIC  X(100) VALUE SPACE.
           02  KHJ-M_RES      USAGE  POINTER.
       01  KHJ-R.
           02  KHJ-KEY.                                                 ｺｰﾄﾞ
             03  KHJ-HCD      PIC  X(005).                              ﾋﾝﾒｲC
             03  KHJ-JCD      PIC  X(007).                              ｻﾞｲﾘｮｳC
      *
           02  KHJ-SGR        PIC  9(004)V9(01).                        ｺﾞﾑｼﾖｳG
           02  KHJ-SGRD  REDEFINES KHJ-SGR  PIC  9(001)V9(04).          ｺﾞﾑｼﾖｳG
      *
           02  KHJ-SU         PIC  9(001).                              ｶﾅｸﾞｺｽｳ
           02  KHJ-KSC        PIC  9(001).                              ｶﾅｸﾞｼﾖｳC
      *
           02  F              PIC  X(013).
       77  F                  PIC  X(001).
