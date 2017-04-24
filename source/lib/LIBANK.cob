      ******************************************
      *****                                *****
      *****     銀　行　マ　ス　タ　ー     *****
      *****      ( B A N K M )   85/3      *****
      ******************************************
       01  BANK-M.
           02  BANK-M_PNAME1  PIC  X(005) VALUE "BANKM".
           02  F              PIC  X(001).
           02  BANK-M_LNAME   PIC  X(006) VALUE "BANK-M".
           02  F              PIC  X(001).
           02  BANK-M_KEY1    PIC  X(100) VALUE SPACE.
           02  BANK-M_SORT    PIC  X(100) VALUE SPACE.
           02  BANK-M_IDLST   PIC  X(100) VALUE SPACE.
           02  BANK-M_RES     USAGE  POINTER.
       01  BANK-R.
           02  B-KEY          PIC  X(004).                              ＫＥＹ
           02  B-BNA          PIC  N(008).                              銀行名
           02  B-SNA          PIC  N(008).                              本支店名
           02  B-YBW          PIC  9(010).                              割引枠
           02  B-ZYZ          PIC  9(010).                              前割引残
           02  B-YBZ          PIC  9(010).                              割引残高
           02  B-YBC          PIC  9(001).                              割引区分
           02  F              PIC  X(011).
           02  B-SC           PIC  9(001).                              使用区分
           02  B-NG           PIC  9(004).                              最終年月
           02  B-PRC          PIC  9(002).                              作表区分
       77  F                  PIC X(1).
