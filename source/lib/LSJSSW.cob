      **********************************************
      *****     材料仕入・支払累積ファイル     *****
      **********************************************
       01  JSSR-F.
           02  JSSR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JSSR-F_LNAME   PIC  X(006) VALUE "JSSR-F".
           02  F              PIC  X(001).
           02  JSSR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  JSSR-F_SORT    PIC  X(100) VALUE SPACE.
           02  JSSR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  JSSR-F_RES     USAGE  POINTER.
       01  JSSR-R.
           02  JR-DC.                                                   伝区
             03  JR-DC1       PIC  9(001).
             03  JR-DC2       PIC  9(001).
           02  JR-DATE        PIC  9(008).
           02  JR-NGP   REDEFINES JR-DATE.                              日付
             03  JR-NG.
               04  JR-NEN     PIC  9(004).
               04  JR-NENL  REDEFINES JR-NEN.
                 05  JR-NEN1  PIC  9(002).
                 05  JR-NEN2  PIC  9(002).
               04  JR-GET     PIC  9(002).
             03  JR-PEY       PIC  9(002).
           02  JR-NGPD   REDEFINES JR-DATE.
             03  JR-NEND      PIC  9(004).
             03  JR-GP        PIC  9(004).
             03  JR-GPD  REDEFINES JR-GP.
               04  JR-GETD    PIC  9(002).
               04  JR-PEYD    PIC  9(002).
           02  JR-NGPL  REDEFINES JR-DATE.
             03  F            PIC  9(002).
             03  JR-NGPS      PIC  9(006).
           02  JR-SCDD.                                                 仕入先C
             03  JR-SCD1      PIC  9(001).
             03  JR-SCD2      PIC  9(003).
           02  JR-SCD    REDEFINES JR-SCDD  PIC  9(004).
           02  JR-JCDD.                                                 材料C
             03  JR-JCD12.
               04  JR-JCD1    PIC  9(001).
               04  JR-JCD2    PIC  9(002).
             03  JR-JCD3      PIC  9(003).
           02  JR-JCD    REDEFINES JR-JCDD  PIC  9(006).
           02  JR-SU          PIC S9(007)V9(02).                          数量
           02  JR-SUD    REDEFINES JR-SU    PIC S9(009).
           02  JR-T           PIC S9(006)V9(02).                           単価
           02  JR-TD     REDEFINES JR-T     PIC S9(008).
           02  JR-KIN         PIC S9(008).
           02  JR-SHZ         PIC S9(007).
           02  JR-SNGP.                                                 修正日
             03  JR-SNG.
               04  JR-SNEN    PIC  9(002).
               04  JR-SGET    PIC  9(002).
             03  JR-SPEY      PIC  9(002).
           02  JR-SNGPD  REDEFINES JR-SNGP.
             03  JR-SNEND     PIC  9(002).
             03  JR-SGPD      PIC  9(004).
             03  JR-SGP  REDEFINES JR-SGPD.
               04  JR-SGETD   PIC  9(002).
               04  JR-SPEYD   PIC  9(002).
           02  JR-SDAT   REDEFINES JR-SNGP  PIC  9(006).
           02  JR-SJCD        PIC  9(006).
           02  JR-NHN         PIC  9(006).
           02  JR-FC          PIC  9(001).
           02  JR-YC          PIC  9(001).                              用途C
           02  JR-TC          PIC  9(001).                              単位C
           02  JR-HC          PIC  9(001).                              製品C
           02  JR-SC          PIC  9(001).                              支払C
           02  JR-BSC         PIC  9(001).
           02  JR-BKC         PIC  9(002).
           02  F              PIC  X(016).
           02  JR-KEY         PIC  X(007).
           02  JR-KEYD  REDEFINES JR-KEY.
             03  JR-DNO       PIC  9(006).
             03  JR-GNO       PIC  9(001).
           02  JR-CR          PIC  9(001).                              ﾁｪﾂｸﾘｽﾄC
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
