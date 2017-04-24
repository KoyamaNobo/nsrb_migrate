      ******************************************************
      *****     仕　入　先　統　計　マ　ス　タ　ー     *****
      *****             ( S T M )     85/3             *****
      ******************************************************
       01  ST-M.
           02  ST-M_PNAME1    PIC  X(003) VALUE "STM".
           02  F              PIC  X(001).
           02  ST-M_LNAME     PIC  X(004) VALUE "ST-M".
           02  F              PIC  X(001).
           02  ST-M_KEY1      PIC  X(100) VALUE SPACE.
           02  ST-M_SORT      PIC  X(100) VALUE SPACE.
           02  ST-M_IDLST     PIC  X(100) VALUE SPACE.
           02  ST-M_RES       USAGE  POINTER.
       01  ST-R.
           02  ST-KEY.                                                  ＫＥＹ
             03  ST-KEY1      PIC  9(001).
             03  ST-KEY2      PIC  9(003).
           02  ST-ZKZ         PIC S9(009).
           02  ST-ZKZZ        PIC S9(008).
           02  ST-KZ          PIC S9(009).
           02  ST-KZZ         PIC S9(008).
           02  ST-TSK         PIC S9(009).
           02  ST-TSKZ        PIC S9(008).
           02  ST-THK         PIC S9(009).
           02  ST-THKZ        PIC S9(008).
           02  ST-PC          PIC  9(004).                              台帳頁
           02  F              PIC  X(009).
       77  F                  PIC  X(001).
