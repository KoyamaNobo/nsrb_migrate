      **************************************
      *****     工品　区分マスター     *****
      *****      (  KKBM  64/4  )       *****
      **************************************
       01  KKB-M.
           02  KKB-M_PNAME1   PIC  X(004) VALUE "KKBM".
           02  F              PIC  X(001).
           02  KKB-M_LNAME    PIC  X(005) VALUE "KKB-M".
           02  F              PIC  X(001).
           02  KKB-M_KEY1     PIC  X(100) VALUE SPACE.
           02  KKB-M_KEY2     PIC  X(100) VALUE SPACE.
           02  KKB-M_SORT     PIC  X(100) VALUE SPACE.
           02  KKB-M_IDLST    PIC  X(100) VALUE SPACE.
           02  KKB-M_RES      USAGE  POINTER.
       01  KKB-R.
      *    * * *   K E Y  ｺ ｳ ﾓ ｸ   * * *
           02  KKB-KEY.                                                 ｺｰﾄﾞ
             03  KKB-NO       PIC  9(002).                              №
             03  KKB-BC       PIC  X(005).
             03  KKB-BC1  REDEFINES KKB-BC.
               04  KKB-YC     PIC  9(002).                              用途
               04  F          PIC  X(003).
             03  KKB-BC4  REDEFINES KKB-BC.
               04  KKB-KS2    PIC  9(002).                              機種２
               04  F          PIC  X(003).
             03  KKB-BC5  REDEFINES KKB-BC.
               04  KKB-FRC    PIC  9(002).                              廃却不良
               04  F          PIC  X(003).
             03  KKB-BC7  REDEFINES KKB-BC.
               04  KKB-JSC    PIC  9(001).                              仕入先№
               04  F          PIC  X(004).
             03  KKB-BC9  REDEFINES KKB-BC.
               04  F          PIC  X(005).
      *    * * *   N A M E  ｺ ｳ ﾓ ｸ   * * *
           02  KKB-NAME1.
             03  KKB-YCN      PIC  N(016).                              用途区分
             03  F            PIC  X(025).
           02  KKB-NAME4  REDEFINES KKB-NAME1.
             03  KKB-KSN2     PIC  X(006).                              機種２
             03  F            PIC  X(051).
           02  KKB-NAME5  REDEFINES KKB-NAME1.
             03  KKB-FRN      PIC  N(006).                              廃却不良
             03  F            PIC  X(045).
           02  KKB-NAME7  REDEFINES KKB-NAME1.
             03  KKB-JSN      PIC  N(008).                              仕入先名
             03  KKB-SCO      PIC  9(004).
             03  F            PIC  X(037).
           02  KKB-NAME9  REDEFINES KKB-NAME1.
             03  KKB-JRCA.
               04  KKB-JRCD  OCCURS  4.
                 05  KKB-JRC  PIC  9(006).
             03  F            PIC  X(033).
           02  KKB-NAME90 REDEFINES KKB-NAME1.                          作業区分
             03  KKB-SC.
               04  KKB-SC01   PIC  9(001).                              売上
               04  KKB-SC02   PIC  9(001).                              売上変換
               04  KKB-SC03   PIC  9(001).                              値引
               04  KKB-SC04   PIC  9(001).                              入金
               04  KKB-SC05   PIC  9(001).                              加硫
               04  KKB-SC06   PIC  9(001).                              廃却
               04  KKB-SC07   PIC  9(001).                              払出回収
               04  KKB-SC08   PIC  9(001).                              東海仕入
               04  KKB-SC09   PIC  9(001).                              --------
               04  KKB-SC10   PIC  9(001).                              --------
               04  KKB-SC11   PIC  9(001).                              --------
               04  KKB-SC12   PIC  9(001).                              --------
               04  KKB-SC13   PIC  9(001).                              日次更新
               04  KKB-SC14   PIC  9(001).                              予定変換
               04  KKB-SC15   PIC  9(001).                              月次更新
             03  F            PIC  X(042).
           02  KKB-NAME95 REDEFINES KKB-NAME1.
             03  KKB-DNO      PIC  9(006).
             03  F            PIC  X(051).
       77  F                  PIC  X(001).
