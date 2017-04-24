      ********************************************
      *****    仕　入　先　マ　ス　タ　ー    *****
      *****         (  S M  )  256/1         *****
      ********************************************
       01  S-M.
           02  S-M_PNAME1     PIC  X(002) VALUE "SM".
           02  F              PIC  X(001).
           02  S-M_LNAME      PIC  X(003) VALUE "S-M".
           02  F              PIC  X(001).
           02  S-M_KEY1       PIC  X(100) VALUE SPACE.
           02  S-M_SORT       PIC  X(100) VALUE SPACE.
           02  S-M_IDLST      PIC  X(100) VALUE SPACE.
           02  S-M_RES        USAGE  POINTER.
       01  S-R.
           02  S-KEY.                                                   ＫＥＹ
             03  S-TCD        PIC  9(004).                              仕入先CD
             03  S-TCDD  REDEFINES S-TCD.
               04  S-TCD1     PIC  9(001).
               04  S-TCD2     PIC  9(003).
           02  S-NAME         PIC  N(024).                              仕入先名
           02  S-JSU          PIC  N(024).                              住所(上)
           02  S-JSS          PIC  N(012).                              住所(下)
           02  S-UNO          PIC  X(008).
           02  S-TEL          PIC  X(014).
           02  S-FAX          PIC  X(014).
           02  S-FKC          PIC  9(002).                              府県ｺｰﾄﾞ
           02  S-SZC          PIC  9(001).                              消費税C
           02  S-STS          PIC  9(003).
           02  F              PIC  9(002).
           02  S-SKR          PIC  9(001).                              送金料C
           02  S-PC           PIC  9(002).
           02  S-BKC          PIC  9(002).
           02  S-SHH          PIC  9(001).
           02  F              PIC  X(029).
           02  S-TNG          PIC  9(004).
           02  S-KANA         PIC  X(036).
           02  F              PIC  X(006).
           02  S-ENG          PIC  9(004).
           02  S-KKC          PIC  9(001).                              買掛区分
           02  S-SFC          PIC  9(001).                              振込区分
           02  S-TGC          PIC  9(001).                              手形区分
       77  F                  PIC  X(001).
