      *****************************************
      *****     統一伝票入力ファイル　    *****
      *****      (  TDIF  170/3  )        *****
      *****************************************
       01  TDIF.
           02  TDIF_PNAME1    PIC  X(008) VALUE "TDIF-RYO".
           02  F              PIC  X(001).
           02  TDIF_LNAME     PIC  X(004) VALUE "TDIF".
           02  F              PIC  X(001).
           02  TDIF_KEY1      PIC  X(100) VALUE SPACE.
           02  TDIF_SORT      PIC  X(100) VALUE SPACE.
           02  TDIF_IDLST     PIC  X(100) VALUE SPACE.
           02  TDIF_RES       USAGE  POINTER.
       01  TDI-R.
           02  TDI-KEY.
             03  TDI-DNO      PIC  9(006).
             03  TDI-GNO      PIC  9(001).
           02  TDI-DATE       PIC  9(006).
           02  TDI-NGP   REDEFINES TDI-DATE.
             03  TDI-NEN      PIC  9(002).
             03  TDI-GET      PIC  9(002).
             03  TDI-PEY      PIC  9(002).
           02  TDI-TCD        PIC  9(004).
           02  TDI-CCD        PIC  9(003).
           02  TDI-TPC        PIC  9(004).
           02  TDI-HCD        PIC  9(006).
           02  TDI-SIZ        PIC  X(003).
           02  TDI-SKB        PIC  9(001).
           02  TDI-SNO        PIC  9(002).
           02  TDI-SU         PIC S9(005).
           02  TDI-GT         PIC  9(007).
           02  TDI-UT         PIC  9(007).
           02  TDI-GKIN       PIC S9(008).
           02  TDI-UKIN       PIC S9(008).
           02  TDI-JNOD.
             03  TDI-JNO      PIC  9(006).
             03  TDI-JGN      PIC  9(001).
           02  TDI-SOK        PIC  9(001).
           02  TDI-UNS        PIC  9(001).
           02  TDI-ISU        PIC  9(003).
           02  TDI-HNO        PIC  X(010).
           02  TDI-TEKI       PIC  N(028).
           02  TDI-TED   REDEFINES TDI-TEKI.
             03  TDI-THT      PIC  N(009).
             03  TDI-TTE      PIC  N(019).
           02  TDI-TRN        PIC  X(020).
           02  TDI-JAN        PIC  X(013).
           02  F              PIC  X(052).
           02  TDI-NNGP       PIC  9(006).
           02  TDI-NHMS       PIC  9(006).
           02  F              PIC  X(008).
           02  TDI-PRC        PIC  9(001).
           02  TDI-UPC        PIC  9(001).
       77  F                  PIC  X(001).
