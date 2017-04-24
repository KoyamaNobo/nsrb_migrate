      ****************************************
      *****     非請求データファイル     *****
      *****       (  SKDKF 192/4  )      *****
      ****************************************
       01  SKDKF.
           02  SKDKF_PNAME1   PIC  X(005) VALUE "SKDKF".
           02  F              PIC  X(001).
           02  SKDKF_LNAME    PIC  X(005) VALUE "SKDKF".
           02  F              PIC  X(001).
           02  SKDKF_KEY1     PIC  X(100) VALUE SPACE.
           02  SKDKF_SORT     PIC  X(100) VALUE SPACE.
           02  SKDKF_IDLST    PIC  X(100) VALUE SPACE.
           02  SKDKF_RES      USAGE  POINTER.
       01  SKDK-R.
           02  SKDK-KEY.                                                KEY
             03  SKDK-TCD     PIC  9(004).                              得意先C
             03  SKDK-DATE    PIC  9(008).                              日付
             03  SKDK-NGP   REDEFINES SKDK-DATE.
               04  SKDK-NG.
                 05  SKDK-NEN PIC  9(004).
                 05  SKDK-GET PIC  9(002).
               04  SKDK-PEY   PIC  9(002).
             03  SKDK-NGPL  REDEFINES SKDK-DATE.
               04  F          PIC  9(002).
               04  SKDK-NGPS  PIC  9(006).
             03  SKDK-NGPD  REDEFINES SKDK-DATE.
               04  F          PIC  9(004).
               04  SKDK-GP    PIC  9(004).
             03  SKDK-DTC     PIC  9(001).                              区分
             03  SKDK-DNO     PIC  9(006).                              伝票№
             03  SKDK-GNO     PIC  9(001).                              　行№
           02  SKDK-HCD       PIC  9(006).                              品名Ｃ
           02  SKDK-HCDD  REDEFINES SKDK-HCD.
             03  SKDK-KCD     PIC  X(005).
             03  F            PIC  X(001).
           02  SKDK-SU        PIC S9(006)V9(02).                           数量
           02  SKDK-T         PIC S9(006)V9(02).                           単価
           02  SKDK-KIN       PIC S9(009).                              金額
           02  SKDK-DC        PIC  9(001).                              伝区
           02  SKDK-CSC       PIC  9(001).
           02  SKDK-SKD       PIC  9(008).                              請求日
           02  SKDK-SKDD  REDEFINES SKDK-SKD.
             03  SKDK-SNEN    PIC  9(004).
             03  SKDK-SGET    PIC  9(002).
             03  SKDK-SPEY    PIC  9(002).
           02  SKDK-TNC       PIC  9(002).                              担当Ｃ
           02  SKDK-BMC       PIC  9(001).                              部門C
           02  SKDK-DCC       PIC  9(001).
           02  F              PIC  X(002).
           02  SKDK-TCD2      PIC  9(004).
           02  SKDK-CCD       PIC  9(003).                              直送№
           02  SKDK-BI        PIC  N(024).                              備考
           02  SKDK-HNO       PIC  9(006).
           02  F              PIC  X(030).
           02  SKDK-SHZ       PIC S9(007).                              消費税
           02  SKDK-KSU       PIC  9(003).                              個数
           02  SKDK-JCD       PIC  9(006).
           02  F              PIC  X(012).
           02  SKDK-SNO       PIC  9(006).
       77  F                  PIC  X(001).
