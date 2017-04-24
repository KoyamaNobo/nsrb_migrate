      ************************************************
      *****    統一伝票ファイル（赤ちゃん本舗）  *****
      *****           ( TDNAF )  256/1           *****
      ************************************************
       01  TDNAF.
           02  TDNAF_PNAME1     PIC  X(009) VALUE "TDNAF-RYO".
           02  F                PIC  X(001).
           02  TDNAF_LNAME      PIC  X(005) VALUE "TDNAF".
           02  F                PIC  X(001).
           02  TDNAF_KEY1       PIC  X(100) VALUE SPACE.
           02  TDNAF_KEY2       PIC  X(100) VALUE SPACE.
           02  TDNAF_SORT       PIC  X(100) VALUE SPACE.
           02  TDNAF_IDLST      PIC  X(100) VALUE SPACE.
           02  TDNAF_RES        USAGE  POINTER.
       01  TDNA-R.
           02  TDNA-KEY.
             03  TDNA-STC       PIC  9(007).
             03  TDNA-STCD  REDEFINES TDNA-STC.
               04  TDNA-STC12.
                 05  TDNA-STC1  PIC  9(002).
                 05  TDNA-STC2  PIC  9(003).
               04  TDNA-STC3    PIC  9(002).
             03  TDNA-DNO       PIC  9(007).
             03  TDNA-DGN       PIC  9(002).
           02  TDNA-JAN         PIC  X(013).
           02  TDNA-SU          PIC  9(006).
           02  TDNA-GTN         PIC  9(007).
           02  TDNA-UTN         PIC  9(007).
           02  TDNA-GKIN        PIC  9(010).
           02  TDNA-UKIN        PIC  9(010).
           02  TDNA-DPM         PIC  X(002).
           02  TDNA-CLS         PIC  X(003).
           02  TDNA-SHM         PIC  X(013).
           02  TDNA-MKH         PIC  X(010).
           02  TDNA-MSB         PIC  X(010).
           02  TDNA-TY          PIC  X(002).
           02  TDNA-HCD         PIC  9(006).
           02  TDNA-COR         PIC  N(004).
           02  TDNA-SIZ         PIC  X(004).
           02  TDNA-NSU         PIC  9(006).
           02  TDNA-TSC         PIC  9(001).
           02  F                PIC  X(008).
           02  TDNA-CCD         PIC  9(003).
           02  TDNA-TNA         PIC  N(014).
           02  TDNA-HNO         PIC  9(009).
           02  TDNA-HNGP        PIC  9(006).
           02  TDNA-NNGP        PIC  9(006).
           02  TDNA-THC         PIC  9(006).
           02  TDNA-BI          PIC  X(010).
           02  TDNA-SNGP        PIC  9(008).
           02  TDNA-SNGPD REDEFINES TDNA-SNGP.
             03  F              PIC  9(002).
             03  TDNA-NGPS      PIC  9(006).
           02  TDNA-HNA         PIC  X(006).
           02  TDNA-ZON         PIC  X(004).
           02  TDNA-DC          PIC  9(002).
           02  F                PIC  X(007).
           02  TDNA-DNGP        PIC  9(008).
           02  TDNA-DNGPD REDEFINES TDNA-DNGP.
             03  F              PIC  9(002).
             03  TDNA-DNGPS     PIC  9(006).
           02  TDNA-NRC         PIC  9(001).
           02  F                PIC  X(008).
           02  TDNA-PC          PIC  9(001).
           02  TDNA-RC          PIC  9(001).
       77  F                    PIC  X(001).
