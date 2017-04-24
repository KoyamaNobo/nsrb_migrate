      ************************************************
      *****    統一伝票ファイル（ナ　フ　コ）    *****
      *****           ( TDNNF )  256/1           *****
      ************************************************
       01  TDNNF.
           02  TDNNF_PNAME1   PIC  X(009) VALUE "TDNNF-RYO".
           02  F              PIC  X(001).
           02  TDNNF_LNAME    PIC  X(005) VALUE "TDNNF".
           02  F              PIC  X(001).
           02  TDNNF_KEY1     PIC  X(100) VALUE SPACE.
           02  TDNNF_KEY2     PIC  X(100) VALUE SPACE.
           02  TDNNF_KEY3     PIC  X(100) VALUE SPACE.
           02  TDNNF_SORT     PIC  X(100) VALUE SPACE.
           02  TDNNF_IDLST    PIC  X(100) VALUE SPACE.
           02  TDNNF_RES      USAGE  POINTER.
       01  TDNN-R.
           02  TDNN-R1.                                                     ﾍｯﾀﾞｰ
               03  TDNN1-KEY.
                 04  TDNN1-STC.
                   05  TDNN1-SCD  PIC  9(002).
                   05  TDNN1-TCD  PIC  9(003).
                   05  F          PIC  X(004).
                 04  TDNN1-DNO.
                   05  F          PIC  X(002).
                   05  TDNN1-DNOD PIC  9(007).
                 04  TDNN1-DGN    PIC  9(002).
               03  F              PIC  X(002).
               03  TDNN1-BCD      PIC  9(002).
               03  TDNN1-DPC      PIC  9(002).
               03  TDNN1-HNGP     PIC  9(006).
               03  TDNN1-HNGPD REDEFINES TDNN1-HNGP.
                 04  TDNN1-HNEN   PIC  9(002).
                 04  TDNN1-HGET   PIC  9(002).
                 04  TDNN1-HPEY   PIC  9(002).
               03  TDNN1-NNGP     PIC  9(006).
               03  TDNN1-NNGPD REDEFINES TDNN1-NNGP.
                 04  TDNN1-NNEN   PIC  9(002).
                 04  TDNN1-NGET   PIC  9(002).
                 04  TDNN1-NPEY   PIC  9(002).
               03  TDNN1-THC      PIC  9(006).
               03  TDNN1-STA      PIC  X(002).
               03  TDNN1-SNA      PIC  X(015).
               03  TDNN1-TNA      PIC  X(015).
               03  TDNN1-TSN      PIC  X(015).
               03  TDNN1-TST      PIC  X(012).
               03  TDNN1-HCC      PIC  9(001).
               03  TDNN1-F1.
                 04  TDNN1-NHB    PIC  X(001).
                 04  F            PIC  X(021).
               03  F              PIC  X(002).
               03  TDNN1-AR       PIC  X(007).
               03  TDNN1-DUR      PIC  X(026).
               03  TDNN1-DSHR     PIC  X(014).
               03  TDNN1-DSMR     PIC  X(007).
               03  TDNN1-ER       PIC  X(005).
               03  TDNN1-FSR      PIC  X(015).
               03  TDNN1-FUR      PIC  X(007).
               03  TDNN1-LCR      PIC  X(016).
               03  TDNN1-LUR      PIC  X(020).
               03  TDNN1-LSR      PIC  X(007).
               03  F              PIC  X(002).
               03  TDNN1-HC       PIC  9(001).
               03  TDNN1-PC       PIC  9(001).
           02  TDNN-R2    REDEFINES  TDNN-R1.                                                     ﾒｲｻｲ
               03  TDNN2-KEY.
                 04  TDNN2-STC.
                   05  TDNN2-SCD  PIC  9(002).
                   05  TDNN2-TCD  PIC  9(003).
                   05  F          PIC  X(004).
                 04  TDNN2-DNO.
                   05  F          PIC  X(002).
                   05  TDNN2-DNOD PIC  9(007).
                 04  TDNN2-DGN    PIC  9(002).
               03  TDNN2-JAN      PIC  X(013).
               03  TDNN2-GAR      PIC  X(006).
               03  F              PIC  X(001).
               03  TDNN2-TNI      PIC  X(003).
               03  TDNN2-SU       PIC  9(005).
               03  F              PIC  X(001).
               03  TDNN2-GTN      PIC  9(007).
               03  TDNN2-GTND  REDEFINES TDNN2-GTN.
                 04  TDNN2-GTN1   PIC  9(001).
                 04  TDNN2-GTN2   PIC  9(006).
               03  F              PIC  X(002).
               03  TDNN2-UTN      PIC  9(007).
               03  TDNN2-UTND  REDEFINES TDNN2-UTN.
                 04  TDNN2-UTN1   PIC  9(001).
                 04  TDNN2-UTN2   PIC  9(006).
               03  TDNN2-GKIN     PIC  9(010).
               03  TDNN2-UKIN     PIC  9(010).
               03  F              PIC  X(009).
               03  TDNN2-SHN      PIC  X(025).
               03  TDNN2-HSC      PIC  X(008).
               03  TDNN2-COR      PIC  X(006).
               03  TDNN2-SIZ      PIC  X(005).
               03  F              PIC  X(004).
               03  TDNN2-KKK      PIC  X(025).
               03  TDNN2-PCH      PIC  X(001).
               03  TDNN2-PSI      PIC  X(001).
               03  TDNN2-PBM      PIC  9(002).
               03  TDNN2-PJAN     PIC  X(013).
               03  TDNN2-PSHN     PIC  X(020).
               03  TDNN2-PKKK     PIC  X(020).
               03  TDNN2-PUTN     PIC  9(007).
               03  TDNN2-PMS      PIC  9(005).
               03  F              PIC  X(006).
               03  TDNN2-TSU      PIC  9(005).
               03  TDNN2-TSC      PIC  9(001).
               03  TDNN2-HCO      PIC  9(006).
               03  TDNN2-HC       PIC  9(001).
               03  TDNN2-PC       PIC  9(001).
       77  F                  PIC  X(001).
