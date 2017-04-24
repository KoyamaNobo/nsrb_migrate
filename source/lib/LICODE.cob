      ************************************
      *****    コード変換ファイル    *****
      *****      ( CODEF ) 102/5     *****
      ************************************
       01  CODEF.
           02  CODEF_PNAME1     PIC  X(005) VALUE "CODEF".
           02  F                PIC  X(001).
           02  CODEF_PNAME2     PIC  X(006) VALUE "CODEF2".
           02  F                PIC  X(001).
           02  CODEF_LNAME      PIC  X(005) VALUE "CODEF".
           02  F                PIC  X(001).
           02  CODEF_KEY1       PIC  X(100) VALUE SPACE.
           02  CODEF_KEY2       PIC  X(100) VALUE SPACE.
           02  CODEF_KEY3       PIC  X(100) VALUE SPACE.
           02  CODEF_KEY4       PIC  X(100) VALUE SPACE.
           02  CODEF_KEY5       PIC  X(100) VALUE SPACE.
           02  CODEF_SORT       PIC  X(100) VALUE SPACE.
           02  CODEF_IDLST      PIC  X(100) VALUE SPACE.
           02  CODEF_RES        USAGE  POINTER.
       01  CODE-R.
           02  CODE-D1.
             03  CODE-KEY.
               04  CODE-TCD     PIC  9(004).
               04  CODE-JAN     PIC  X(013).
               04  CODE-CO    REDEFINES CODE-JAN.
                 05  CODE-WCO   PIC  9(007).
                 05  F          PIC  X(006).
               04  CODE-HCD     PIC  9(006).
               04  CODE-HCDD  REDEFINES CODE-HCD.
                 05  CODE-HCD1  PIC  9(004).
                 05  CODE-HCD2  PIC  9(002).
               04  CODE-SIZ     PIC  9(001).
               04  CODE-SNO     PIC  9(002).
             03  F              PIC  X(013).
           02  CODE-D2    REDEFINES CODE-D1.
             03  CODE-TCD2      PIC  9(004).
             03  CODE-JAN3      PIC  X(013).
             03  CODE-CO2   REDEFINES CODE-JAN3.
               04  CODE-WCO2    PIC  9(007).
               04  F            PIC  X(006).
             03  CODE-KEY2.
               04  CODE-HCD20   PIC  9(006).
               04  CODE-HCDD2 REDEFINES CODE-HCD20.
                 05  CODE-HCD21 PIC  9(004).
                 05  CODE-HCD22 PIC  9(002).
               04  CODE-SIZ2    PIC  9(001).
               04  CODE-SNO2    PIC  9(002).
               04  CODE-JAN2    PIC  X(013).
           02  CODE-ITF         PIC  X(016).
           02  CODE-ITFD    REDEFINES CODE-ITF.
             03  CODE-ISU       PIC  9(003).
             03  CODE-JAND      PIC  X(013).
           02  CODE-NAME        PIC  X(020).
           02  F                PIC  X(027).
       77  F                    PIC X(1).
