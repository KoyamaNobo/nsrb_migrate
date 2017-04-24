      ********************************************
      *****     出荷・値引トラン　ワーク     *****
      *****       WK0064___       64/4       *****
      ********************************************
       01  SNTRF.
           02  SNTRF_PNAME1     PIC  X(009) VALUE SPACE.
           02  F                PIC  X(001).
           02  SNTRF_LNAME      PIC  X(005) VALUE "SNTRF".
           02  F                PIC  X(001).
           02  SNTRF_KEY1       PIC  X(100) VALUE SPACE.
           02  SNTRF_SORT       PIC  X(100) VALUE SPACE.
           02  SNTRF_IDLST      PIC  X(100) VALUE SPACE.
           02  SNTRF_RES        USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DATE        PIC  9(008).
           02  SNTR-NGP   REDEFINES SNTR-DATE.
             03  SNTR-NG.
               04  SNTR-NEN     PIC  9(004).
               04  SNTR-NEND  REDEFINES SNTR-NEN.
                 05  SNTR-NEN1  PIC  9(002).
                 05  SNTR-NEN2  PIC  9(002).
               04  SNTR-GET     PIC  9(002).
             03  SNTR-PEY       PIC  9(002).
           02  SNTR-NGPD  REDEFINES SNTR-DATE.
             03  F              PIC  9(002).
             03  SNTR-NGPS.
               04  SNTR-NENS    PIC  9(002).
               04  SNTR-GP      PIC  9(004).
           02  SNTR-TCD         PIC  9(004).
           02  SNTR-HCD         PIC  9(006).
           02  SNTR-HCDD  REDEFINES SNTR-HCD.
             03  SNTR-HCD1      PIC  9(004).
             03  SNTR-HCD2      PIC  9(002).
           02  SNTR-SU          PIC S9(005).
           02  SNTR-T           PIC  9(005).
           02  SNTR-KIN         PIC S9(008).
           02  SNTR-CSC         PIC  9(001).
           02  SNTR-DC          PIC  9(001).
           02  SNTR-FT          PIC  9(005).
           02  SNTR-BC.
             03  SNTR-BC1       PIC  9(002).
             03  SNTR-BC2.
               04  SNTR-BC21    PIC  9(001).
               04  SNTR-BC22    PIC  9(001).
             03  SNTR-BC3       PIC  9(002).
           02  SNTR-BCD   REDEFINES SNTR-BC.
             03  SNTR-BCD1      PIC  9(003).
             03  F              PIC  9(003).
           02  SNTR-TNC.
             03  SNTR-TNC1      PIC  9(001).
             03  SNTR-TNC2      PIC  9(001).
           02  SNTR-FKC         PIC  9(002).
           02  SNTR-SZC         PIC  9(001).
           02  SNTR-SNC         PIC  9(001).
           02  SNTR-BMC         PIC  9(002).
           02  SNTR-BMNO        PIC  9(001).
           02  SNTR-HPV         PIC  9(001).
           02  SNTR-FTC         PIC  9(001).
           02  F                PIC  X(003).
           02  SNTR-SIZ         PIC  9(001).
       77  F                    PIC  X(001).
