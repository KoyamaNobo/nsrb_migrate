      *******************************************************************
      *    　　　　　　　　　類似品マスタ                               *
      *******************************************************************
       01  JT-RUIJ.
           02  JT-RUIJ_PNAME1          PIC  X(007) VALUE "JT-RUIJ".
           02  F                       PIC  X(001).
           02  JT-RUIJ_LNAME           PIC  X(007) VALUE "JT-RUIJ".
           02  F                       PIC  X(001).
           02  JT-RUIJ_KEY1            PIC  X(100) VALUE SPACE.
           02  JT-RUIJ_KEY2            PIC  X(100) VALUE SPACE.
           02  JT-RUIJ_KEY3            PIC  X(100) VALUE SPACE.
           02  JT-RUIJ_SORT            PIC  X(100) VALUE SPACE.
           02  JT-RUIJ_IDLST           PIC  X(100) VALUE SPACE.
           02  JT-RUIJ_RES             USAGE  POINTER.
      *
       01  RUIJ-R.
           02   RUIJ-KEY.                                               KEY
                03   RUIJ-01           PIC 9(2).                        ｸﾌﾞﾝ　
                03   RUIJ-02           PIC 9(6).                        ﾋﾝｺｰﾄﾞ1
                03   RUIJ-03           PIC 9(6).                        ﾋﾝｺｰﾄﾞ2
           02   FILLER                 PIC X(2).
       77  F                           PIC X(1).
