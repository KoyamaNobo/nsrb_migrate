      ************************************************
      *****    統一伝票ファイル（ワークマン）    *****
      *****           ( TDNWF )  256/1           *****
      ************************************************
       01  TDNWF.
           02  TDNWF_PNAME1   PIC  X(005) VALUE "TDNWF".
           02  F              PIC  X(001).
           02  TDNWF_LNAME    PIC  X(005) VALUE "TDNWF".
           02  F              PIC  X(001).
           02  TDNWF_KEY1     PIC  X(100) VALUE SPACE.
           02  TDNWF_KEY2     PIC  X(100) VALUE SPACE.
           02  TDNWF_KEY3     PIC  X(100) VALUE SPACE.
           02  TDNWF_SORT     PIC  X(100) VALUE SPACE.
           02  TDNWF_IDLST    PIC  X(100) VALUE SPACE.
           02  TDNWF_RES      USAGE  POINTER.
       01  TDNW-R.
           02  TDNW-R1.
               03  TDNW1-KEY.
                 04  TDNW1-STC.
                   05  TDNW1-STC1 PIC  9(004).
                   05  TDNW1-SCDD REDEFINES TDNW1-STC1.
                     06  F          PIC  9(002).
                     06  TDNW1-SCD  PIC  9(002).
                   05  TDNW1-STC2 PIC  9(005).
                   05  TDNW1-TCDD REDEFINES TDNW1-STC2.
                     06  F          PIC  9(001).
                     06  TDNW1-TCD  PIC  9(004).
                 04  TDNW1-DNO.
                   05  F          PIC  9(002).
                   05  TDNW1-DNOD PIC  9(007).
                 04  TDNW1-DGN    PIC  9(002).
               03  TDNW1-BC.
                 04  TDNW1-BCD    PIC  9(002).
                 04  F            PIC  9(001).
               03  TDNW1-SHC      PIC  9(001).
               03  TDNW1-DPC      PIC  X(002).
               03  TDNW1-HNGP     PIC  9(006).
               03  TDNW1-HNGPD REDEFINES TDNW1-HNGP.
                 04  TDNW1-HNEN   PIC  9(002).
                 04  TDNW1-HGET   PIC  9(002).
                 04  TDNW1-HPEY   PIC  9(002).
               03  TDNW1-NNGP     PIC  9(006).
               03  TDNW1-NNGPD REDEFINES TDNW1-NNGP.
                 04  TDNW1-NNEN   PIC  9(002).
                 04  TDNW1-NGET   PIC  9(002).
                 04  TDNW1-NPEY   PIC  9(002).
               03  TDNW1-THC      PIC  9(006).
               03  TDNW1-MHC      PIC  X(001).
               03  F              PIC  X(001).
               03  TDNW1-SNA      PIC  X(020).
               03  TDNW1-TNA      PIC  X(020).
               03  TDNW1-HCC      PIC  9(001).
               03  TDNW1-HSP      PIC  X(001).
               03  TDNW1-DHC      PIC  X(001).
               03  TDNW1-KHC      PIC  X(001).
               03  TDNW1-KCC      PIC  X(001).
               03  TDNW1-UBC      PIC  X(001).
               03  TDNW1-NCC      PIC  9(001).
               03  TDNW1-EDI      PIC  9(001).
               03  TDNW1-NKC      PIC  9(001).
               03  TDNW1-ZAC      PIC  9(001).
               03  F              PIC  X(150).
               03  TDNW1-HC       PIC  9(001).
               03  F              PIC  X(008).
               03  TDNW1-PC       PIC  9(001).
           02  TDNW-R2    REDEFINES  TDNW-R1.
               03  TDNW2-KEY.
                 04  TDNW2-STC.
                   05  TDNW2-STC1 PIC  9(004).
                   05  TDNW2-SCDD REDEFINES TDNW2-STC1.
                     06  F          PIC  9(002).
                     06  TDNW2-SCD  PIC  9(002).
                   05  TDNW2-STC2 PIC  9(005).
                   05  TDNW2-TCDD REDEFINES TDNW2-STC2.
                     06  F          PIC  9(001).
                     06  TDNW2-TCD  PIC  9(004).
                 04  TDNW2-DNO.
                   05  F          PIC  9(002).
                   05  TDNW2-DNOD PIC  9(007).
                 04  TDNW2-DGN    PIC  9(002).
               03  TDNW2-HCD      PIC  X(013).
               03  TDNW2-HCDD  REDEFINES  TDNW2-HCD.
                 04  TDNW2-WCO    PIC  9(007).
                 04  F            PIC  X(006).
               03  TDNW2-ISU      PIC  9(003)V9(01).
               03  TDNW2-KSU      PIC  9(004).
               03  TDNW2-HTC      PIC  X(002).
               03  TDNW2-SU       PIC  9(005)V9(01).
               03  TDNW2-GTN      PIC  9(007)V9(02).
               03  TDNW2-UTN      PIC  9(007).
               03  TDNW2-GKIN     PIC  9(010).
               03  TDNW2-UKIN     PIC  9(010).
               03  TDNW2-GCN      PIC  9(006).
               03  TDNW2-CCD      PIC  X(003).
               03  TDNW2-SHN      PIC  X(025).
               03  TDNW2-JAN      PIC  X(013).
               03  F              PIC  X(004).
               03  TDNW2-TSH      PIC  9(005).
               03  TDNW2-TKC      PIC  X(001).
               03  F              PIC  X(001).
               03  F              PIC  X(090).
               03  TDNW2-TSU      PIC  9(005)V9(01).
               03  TDNW2-TSC      PIC  9(001).
               03  TDNW2-HCO      PIC  9(006).
               03  TDNW2-HC       PIC  9(001).
               03  F              PIC  X(008).
               03  TDNW2-PC       PIC  9(001).
       77  F                  PIC  X(001).
