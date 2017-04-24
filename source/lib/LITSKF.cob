      **************************************
      *****     得意先請求ファイル     *****
      *****      (  TSKF 256/1  )      *****
      **************************************
       01  TSKF.
           02  TSKF_PNAME1    PIC  X(004) VALUE "TSKF".
           02  F              PIC  X(001).
           02  TSKF_LNAME     PIC  X(004) VALUE "TSKF".
           02  F              PIC  X(001).
           02  TSKF_KEY1      PIC  X(100) VALUE SPACE.
           02  TSKF_SORT      PIC  X(100) VALUE SPACE.
           02  TSKF_IDLST     PIC  X(100) VALUE SPACE.
           02  TSKF_RES       USAGE  POINTER.
       01  TSK-R.
           02  TSK-KEY.                                                 KEY
             03  TSK-TCD      PIC  9(004).                              得意先C
           02  TSK-ZSDD.                                                前回請求
             03  TSK-ZSD   OCCURS   5.
               04  TSK-HTS    PIC S9(009).
               04  TSK-SZS    PIC S9(007).
               04  TSK-ZNGP   PIC  9(008).
           02  TSK-KKD.                                                 今回請求
             03  TSK-HTN      PIC S9(009).
             03  TSK-SZN      PIC S9(007).
             03  TSK-HTC      PIC S9(007).
             03  TSK-SZC      PIC S9(005).
             03  TSK-HTU      PIC S9(009).
             03  TSK-SZU      PIC S9(007).
             03  TSK-KNGP     PIC  9(008).
           02  F              PIC  X(008).
           02  TSK-TNC        PIC  9(002).                              担当Ｃ
           02  TSK-BMC        PIC  9(001).                              部門C
           02  TSK-DCC        PIC  9(001).
           02  F              PIC  X(068).
       77  F                  PIC  X(001).
