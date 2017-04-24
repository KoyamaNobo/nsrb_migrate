      ***********************************************
      *****　　直　送　先　マ　ス　タ　ー       *****
      *****         (  T C M  )  192/4          *****
      ***********************************************
       01  TC-M.
           02  TC-M_PNAME1    PIC  X(003) VALUE "TCM".
           02  F              PIC  X(001).
           02  TC-M_LNAME     PIC  X(004) VALUE "TC-M".
           02  F              PIC  X(001).
           02  TC-M_KEY1      PIC  X(100) VALUE SPACE.
           02  TC-M_KEY2      PIC  X(100) VALUE SPACE.
           02  TC-M_SORT      PIC  X(100) VALUE SPACE.
           02  TC-M_IDLST     PIC  X(100) VALUE SPACE.
           02  TC-M_RES       USAGE  POINTER.
       01  TC-R.
           02  TC-KEY.                                                  ｺｰﾄﾞ
             03  TC-TCD       PIC  9(004).                              得意先CD
             03  TC-CCD       PIC  9(003).                              直送先CD
           02  TC-NAME        PIC  N(026).
           02  TC-JSU         PIC  N(020).
           02  TC-JSS         PIC  N(020).
           02  TC-UNO         PIC  X(008).
           02  TC-TEL         PIC  X(014).
           02  TC-FKC         PIC  9(002).                              府県ｺｰﾄﾞ
           02  TC-UCD         PIC  9(001).                              運送ｺｰﾄﾞ
           02  TC-SSC         PIC  9(001).
           02  TC-BIK         PIC  9(001).
           02  TC-DHC         PIC  9(001).
           02  TC-SEK         PIC  9(001).
           02  TC-STC         PIC  9(009).
           02  TC-2N          PIC  9(004).
           02  TC-MIC         PIC  9(001).
           02  TC-MZC         PIC  9(001).
           02  TC-NFN         PIC  9(001).
           02  TC-PNO         PIC  9(002).
           02  TC-DATE        PIC  9(006).
           02  TC-YMD   REDEFINES TC-DATE.                              年月日
               03  TC-YY      PIC  9(002).                                年
               03  TC-MM      PIC  9(002).                                月
               03  TC-DD      PIC  9(002).                                日
       77  F                  PIC  X(001).
