      ***************************************
      *****     材　料　マ　ス　タ　ー  *****
      *****        (  JM  128/2  )      *****
      ***************************************
       01  J-M.
           02  J-M_PNAME1     PIC  X(002) VALUE "JM".
           02  F              PIC  X(001).
           02  J-M_LNAME      PIC  X(003) VALUE "J-M".
           02  F              PIC  X(001).
           02  J-M_KEY1       PIC  X(100) VALUE SPACE.
           02  J-M_SORT       PIC  X(100) VALUE SPACE.
           02  J-M_IDLST      PIC  X(100) VALUE SPACE.
           02  J-M_RES        USAGE  POINTER.
       01  J-R.
           02  J-KEY.                                                   ＫＥＹ
             03  J-KEYD.
               04  J-BC       PIC  9(001).                              　　部門
               04  J-RC       PIC  9(002).                                  品目
             03  J-JC         PIC  9(003).
           02  J-JCD   REDEFINES J-KEY  PIC 9(006).                     材料ｺｰﾄﾞ
           02  J-NAME         PIC  N(024).                              材料名
           02  J-YC           PIC  9(001).                              用途区分
           02  J-ZC           PIC  9(001).                              在庫区分
           02  J-SC           PIC  9(001).                              製品区分
           02  J-TC1          PIC  9(001).                              単位区分
           02  J-ST           PIC S9(006)V9(02).                           最終単価
           02  J-TC2          PIC  9(001).                              単位区分
           02  J-YT           PIC S9(006)V9(02).                           予定単価
           02  J-MCD          PIC  9(006).                              加工前CD
           02  J-KT           PIC  9(006)V9(02).                           加工単価
           02  FILLER         PIC  X(022).                              FILLER
           02  J-BKC          PIC  9(002).                              部門管理
           02  J-BKNO         PIC  9(002).
           02  F              PIC  X(007).
           02  J-ED           PIC  9(006).                              最終日付
           02  J-EDR   REDEFINES J-ED.
               03  J-EDY      PIC  9(002).                              　年
               03  J-EDM      PIC  9(002).                              　月
               03  J-EDD      PIC  9(002).                              　日
       77  F                  PIC  X(001).
