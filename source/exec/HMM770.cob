       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      HMM750.
      *********************************************************
      *    PROGRAM         :  履物振替単価修正更新(月中評価替え
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    DATA WRITTN     :  00/06/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-KIN          PIC S9(009).
           02  W-KIND         PIC S9(009).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL  REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-SM           PIC  N(004).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LITTM.
           COPY LIHUHM.
      *FD  HFTSF
       01  HFTSF_HMM770.
           02  HFTSF_PNAME1   PIC  X(005) VALUE "HFTSF".
           02  F              PIC  X(001).
           02  HFTSF_LNAME    PIC  X(012) VALUE "HFTSF_HMM770".
           02  F              PIC  X(001).
           02  HFTSF_KEY1     PIC  X(100) VALUE SPACE.
           02  HFTSF_SORT     PIC  X(100) VALUE SPACE.
           02  HFTSF_IDLST    PIC  X(100) VALUE SPACE.
           02  HFTSF_RES      USAGE  POINTER.
       01  HFTS-R.
           02  HFTS-KEY.
             03  HFTS-NC      PIC  9(001).
             03  HFTS-HCD     PIC  9(006).
             03  HFTS-HCDD  REDEFINES HFTS-HCD.
               04  HFTS-HCD1  PIC  9(004).
               04  HFTS-HCD2  PIC  9(002).
           02  HFTS-OLD.
             03  HFTS-FTO     PIC  9(005).
             03  HFTS-ZRGO    PIC  9(005).
             03  HFTS-SKGO    PIC  9(005).
             03  HFTS-GKGO    PIC  9(005).
             03  HFTS-KNGO    PIC  9(004).
           02  HFTS-NEW.
             03  HFTS-FT      PIC  9(005).
             03  HFTS-ZRG     PIC  9(005).
             03  HFTS-SKG     PIC  9(005).
             03  HFTS-GKG     PIC  9(005).
             03  HFTS-KNG     PIC  9(004).
           02 HFTS-BC.
             03  HFTS-BC1     PIC  9(002).
             03  HFTS-BC2.
               04  HFTS-BC21  PIC  9(001).
               04  HFTS-BC22  PIC  9(001).
             03  HFTS-BC3     PIC  9(002).
           02  F              PIC  X(003).
       77  F                  PIC  X(001).
      *FD  STRANYR
       01  STRANYR_HMM770.
           02  STRANYR_PNAME1 PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  STRANYR_LNAME  PIC  X(014) VALUE "STRANYR_HMM770".
           02  F              PIC  X(001).
           02  STRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  STRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  STRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  STRANYR_RES    USAGE  POINTER.
       01  STRANY-R.
           02  STRANY-DNO     PIC  9(006).
           02  STRANY-GNO     PIC  9(001).
           02  STRANY-DATE    PIC  9(008).
           02  STRANY-NGP   REDEFINES STRANY-DATE.
             03  STRANY-NG    PIC  9(006).
             03  F            PIC  9(002).
           02  STRANY-TCD     PIC  9(004).
           02  STRANY-HCD     PIC  9(006).
           02  F              PIC  X(031).
           02  STRANY-SU      PIC S9(005).
           02  F              PIC  X(014).
           02  STRANY-DC      PIC  9(001).
           02  STRANY-FT      PIC  9(005).
           02  F              PIC  X(046).
           02  STRANY-UNC     PIC  9(001).
       77  F                  PIC  X(001).
      *FD  RSTRANYR
       01  RSTRANYR_HMM770.
           02  RSTRANYR_PNAME1 PIC  X(011) VALUE "STRANYR-RDB".
           02  F               PIC  X(001).
           02  RSTRANYR_LNAME  PIC  X(015) VALUE "RSTRANYR_HMM770".
           02  F               PIC  X(001).
           02  RSTRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  RSTRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  RSTRANYR_RES    USAGE  POINTER.
       01  RSTRANY-R.
           02  RSTRANY-DNO    PIC  9(006).
           02  RSTRANY-GNO    PIC  9(001).
           02  RSTRANY-DATE   PIC  9(008).
           02  RSTRANY-NGP   REDEFINES RSTRANY-DATE.
             03  RSTRANY-NG   PIC  9(006).
             03  F            PIC  9(002).
           02  RSTRANY-TCD    PIC  9(004).
           02  RSTRANY-HCD    PIC  9(006).
           02  F              PIC  X(031).
           02  RSTRANY-SU     PIC S9(005).
           02  F              PIC  X(014).
           02  RSTRANY-DC     PIC  9(001).
           02  RSTRANY-FT     PIC  9(005).
           02  F              PIC  X(046).
           02  RSTRANY-UNC    PIC  9(001).
       77  F                  PIC  X(001).
      *FD  SNTRF
       01  SNTRF_HMM770.
           02  SNTRF_PNAME1   PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTRF_LNAME    PIC  X(012) VALUE "SNTRF_HMM770".
           02  F              PIC  X(001).
           02  SNTRF_KEY1     PIC  X(100) VALUE SPACE.
           02  SNTRF_SORT     PIC  X(100) VALUE SPACE.
           02  SNTRF_IDLST    PIC  X(100) VALUE SPACE.
           02  SNTRF_RES      USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  SNTR-DATE      PIC  9(008).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-HCD       PIC  9(006).
           02  F              PIC  X(031).
           02  SNTR-SU        PIC S9(005).
           02  F              PIC  X(014).
           02  SNTR-DC        PIC  9(001).
           02  SNTR-FT        PIC  9(005).
           02  F              PIC  X(046).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
      *FD  UTRF
       01  UTRF_HMM770.
           02  UTRF_PNAME1    PIC  X(004) VALUE "UTRF".
           02  F              PIC  X(001).
           02  UTRF_LNAME     PIC  X(011) VALUE "UTRF_HMM770".
           02  F              PIC  X(001).
           02  UTRF_KEY1      PIC  X(100) VALUE SPACE.
           02  UTRF_SORT      PIC  X(100) VALUE SPACE.
           02  UTRF_IDLST     PIC  X(100) VALUE SPACE.
           02  UTRF_RES       USAGE  POINTER.
       01  UTR-R.
           02  UTR-DNO        PIC  9(007).
           02  UTR-DATE       PIC  9(008).
           02  UTR-HCD        PIC  9(006).
           02  F              PIC  X(041).
           02  UTR-SU         PIC S9(005).
           02  F              PIC  X(008).
           02  UTR-KIN        PIC S9(008).
           02  UTR-NC         PIC  9(001).
           02  UTR-SC         PIC  9(001).
           02  F              PIC  X(043).
       77  F                  PIC  X(001).
      *FD  UTRYR
       01  UTRYR_HMM770.
           02  UTRYR_PNAME1   PIC  X(009) VALUE "UTRYR-RDB".
           02  F              PIC  X(001).
           02  UTRYR_LNAME    PIC  X(012) VALUE "UTRYR_HMM770".
           02  F              PIC  X(001).
           02  UTRYR_KEY1     PIC  X(100) VALUE SPACE.
           02  UTRYR_SORT     PIC  X(100) VALUE SPACE.
           02  UTRYR_IDLST    PIC  X(100) VALUE SPACE.
           02  UTRYR_RES      USAGE  POINTER.
       01  UTRY-R.
           02  UTRY-DNO       PIC  9(007).
           02  UTRY-DATE      PIC  9(008).
           02  UTRY-NGP   REDEFINES UTRY-DATE.
             03  UTRY-NG      PIC  9(006).
             03  F            PIC  9(002).
           02  UTRY-HCD       PIC  9(006).
           02  F              PIC  X(041).
           02  UTRY-SU        PIC S9(005).
           02  F              PIC  X(008).
           02  UTRY-KIN       PIC S9(008).
           02  UTRY-NC        PIC  9(001).
           02  UTRY-SC        PIC  9(001).
           02  F              PIC  X(043).
       77  F                  PIC  X(001).
      *FD  STRAN
       01  STRAN_HMM770.
           02  STRAN_PNAME1   PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  STRAN_LNAME    PIC  X(012) VALUE "STRAN_HMM770".
           02  F              PIC  X(001).
           02  STRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  STRAN_SORT     PIC  X(100) VALUE SPACE.
           02  STRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  STRAN_RES      USAGE  POINTER.
       01  STRAN-R            PIC  X(128).
       77  F                  PIC  X(001).
      *FD  UTRAN
       01  UTRAN_HMM770.
           02  UTRAN_PNAME1   PIC  X(005) VALUE "UTRAN".
           02  F              PIC  X(001).
           02  UTRAN_LNAME    PIC  X(012) VALUE "UTRAN_HMM770".
           02  F              PIC  X(001).
           02  UTRAN_KEY1     PIC  X(100) VALUE SPACE.
           02  UTRAN_SORT     PIC  X(100) VALUE SPACE.
           02  UTRAN_IDLST    PIC  X(100) VALUE SPACE.
           02  UTRAN_RES      USAGE  POINTER.
       01  UTRAN-R            PIC  X(128).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物　振替単価　更新　　＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SM    PIC  N(004).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  TTM ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  HUHM ﾅｼ  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME5   PIC  X(023) VALUE
                  "***  ﾐｺｳｼﾝ ﾃﾞｰﾀ ｱﾘ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(025) VALUE
                  "***  HIM REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(025) VALUE
                  "***  TTM REWRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(034) VALUE
                  "***  HUHM REWRITE ｴﾗｰ (SNRAN)  ***".
             03  E-ME13  PIC  X(033) VALUE
                  "***  HUHM REWRITE ｴﾗｰ (UTRF)  ***".
             03  E-ME21  PIC  X(028) VALUE
                  "***  HUHM ｷﾝｶﾞｸ ｴﾗｰ (+)  ***".
             03  E-ME22  PIC  X(028) VALUE
                  "***  HUHM ｷﾝｶﾞｸ ｴﾗｰ (X)  ***".
             03  E-HCD   PIC  9(006).
             03  E-TCD   PIC  9(004).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "66" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "12" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "20" "36" "22" "01C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "53" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SM" "N" "12" "30" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SM" BY REFERENCE W-SM "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "290" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "290" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "16" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "23" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "25" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "25" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "34" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "33" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "28" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "28" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "9" "24" "50" "6" "E-ME22" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "50" "4" "E-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE SNTR-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE "評価替え" TO W-SM.
           CALL "SD_Output" USING "D-SM" D-SM "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HFTSF_PNAME1 " " BY REFERENCE HFTSF_IDLST "1"
            "HFTS-KEY" BY REFERENCE HFTS-KEY.
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 " " BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           MOVE SPACE TO HFTS-KEY.
           MOVE 1 TO HFTS-NC.
      *           START HFTSF KEY NOT < HFTS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HFTSF_PNAME1 "HFTS-KEY" " NOT < " HFTS-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-15.
      *           READ HFTSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HFTS-NC NOT = 1
               GO TO M-25
           END-IF
           MOVE HFTS-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE HFTS-FT TO HI-FT.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           MOVE HFTS-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-95
           END-IF
           COMPUTE HUH-ZK = HUH-ZS * HFTS-FT.
           COMPUTE HUH-YK = HUH-YS * HFTS-FT.
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HFTSF_IDLST HFTSF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-15.
       M-25.
           MOVE ZERO TO W-NG.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           CALL "DB_F_Open" USING
            "I-O" STRANYR_PNAME1 " " BY REFERENCE STRANYR_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" RSTRANYR_PNAME1 " " BY REFERENCE RSTRANYR_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" UTRYR_PNAME1 " " BY REFERENCE UTRYR_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" UTRF_PNAME1 " " BY REFERENCE UTRF_IDLST "0".
           MOVE 0 TO W-DC.
       M-30.
      *           READ SNTRF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  SNTR-GNO = 9
               GO TO M-30
           END-IF
           IF  SNTR-SNC = 1
               GO TO M-30
           END-IF
           IF  SNTR-DC = 2 OR 4
               GO TO M-30
           END-IF
      *
           MOVE 1 TO HFTS-NC.
           MOVE SNTR-HCD TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
      *
           MOVE HFTS-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE SNTR-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SNTR-DC = 1 OR 5
               COMPUTE TT-TUG = TT-TUG - (SNTR-SU * SNTR-FT * -1)
                                       + (SNTR-SU * HFTS-FT * -1)
           ELSE
               COMPUTE TT-TUG = TT-TUG - (SNTR-SU * SNTR-FT)
                                       + (SNTR-SU * HFTS-FT)
           END-IF
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           IF  SNTR-DC = 6 OR 8
               GO TO M-30
           END-IF
           MOVE HFTS-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SNTR-DC = 1 OR 5
               COMPUTE HUH-UG = HUH-UG - (SNTR-SU * SNTR-FT * -1)
                                       + (SNTR-SU * HFTS-FT * -1)
           ELSE
               COMPUTE HUH-UG = HUH-UG - (SNTR-SU * SNTR-FT)
                                       + (SNTR-SU * HFTS-FT)
           END-IF
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE HFTS-FT TO SNTR-FT.
      *           REWRITE SNTR-R.
      *///////////////
           CALL "DB_Update" USING
            SNTRF_PNAME1 SNTRF_LNAME SNTR-R RETURNING RET.
           GO TO M-30.
       M-35.
           IF  W-DC = 0
               GO TO M-45
           END-IF.
       M-40.
      *           READ STRANYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" STRANYR_PNAME1 BY REFERENCE STRANY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-41
           END-IF
           IF  STRANY-GNO = 9
               GO TO M-40
           END-IF
           IF  STRANY-UNC = 1
               GO TO M-40
           END-IF
           IF  STRANY-NG < W-NG
               GO TO M-40
           END-IF
           MOVE 1 TO HFTS-NC.
           MOVE STRANY-HCD TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           MOVE HFTS-FT TO STRANY-FT.
      *           REWRITE STRANY-R.
      *///////////////
           CALL "DB_Update" USING
            STRANYR_PNAME1 STRANYR_LNAME STRANY-R RETURNING RET.
           GO TO M-40.
       M-41.
      *           SELECT RSTRANYR WHERE RSTRANY-NG = W-NG AND
      *                                 RSTRANY-UNC NOT = 1.
      *///////////////
           CALL "DB_Select" USING RSTRANYR_PNAME1 "WHERE"
            "RSTRANY-NG" "=" W-NG "AND" "RSTRANY-UNC" "NOT =" "1"
            RETURNING RET.
       M-42.
      *           READ RSTRANYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RSTRANYR_PNAME1 BY REFERENCE RSTRANY-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO W-DC
               GO TO M-45
           END-IF
           MOVE 1 TO HFTS-NC.
           MOVE RSTRANY-HCD TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-42
           END-IF
           MOVE HFTS-FT TO RSTRANY-FT.
      *           REWRITE RSTRANY-R.
      *///////////////
           CALL "DB_Update" USING
            RSTRANYR_PNAME1 RSTRANYR_LNAME RSTRANY-R RETURNING RET.
           GO TO M-42.
       M-45.
      *           READ UTRF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UTRF_PNAME1 BY REFERENCE UTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           MOVE 1 TO HFTS-NC.
           MOVE UTR-HCD TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  W-DC = 0
               MOVE 1 TO W-DC
           END-IF
      *
           MOVE HFTS-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE HFTS-HCD TO HUH-KEY.
      *           READ HUH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  UTR-NC = 5
               COMPUTE HUH-NK = HUH-NK + UTR-KIN - (UTR-SU * HFTS-FT)
           ELSE
               COMPUTE HUH-NK = HUH-NK - UTR-KIN + (UTR-SU * HFTS-FT)
           END-IF
      *           REWRITE HUH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HUH-M_PNAME1 HUH-M_LNAME HUH-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           COMPUTE UTR-KIN = UTR-SU * HFTS-FT.
      *           REWRITE UTR-R.
      *///////////////
           CALL "DB_Update" USING
            UTRF_PNAME1 UTRF_LNAME UTR-R RETURNING RET.
           GO TO M-45.
       M-50.
           IF  W-DC = 0
               GO TO M-60
           END-IF
      *           SELECT UTRYR WHERE UTRY-NG = W-NG.
      *///////////////
           CALL "DB_Select" USING UTRYR_PNAME1 "WHERE"
            "UTRY-NG" "=" W-NG RETURNING RET.
       M-55.
      *           READ UTRYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" UTRYR_PNAME1 BY REFERENCE UTRY-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING UTRYR_PNAME1
               GO TO M-60
           END-IF
           MOVE 1 TO HFTS-NC.
           MOVE UTRY-HCD TO HFTS-HCD.
      *           READ HFTSF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HFTSF_PNAME1 BY REFERENCE HFTS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           COMPUTE UTRY-KIN = UTRY-SU * HFTS-FT.
      *           REWRITE UTRY-R.
      *///////////////
           CALL "DB_Update" USING
            UTRYR_PNAME1 UTRYR_LNAME UTRY-R RETURNING RET.
           GO TO M-55.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE HFTSF_IDLST HFTSF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE RSTRANYR_IDLST RSTRANYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRYR_IDLST UTRYR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE UTRF_IDLST UTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 " " BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
       M-75.
      *           READ HUH-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE HUH-HCD TO HI-KEY.
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           COMPUTE W-KIN = HUH-ZK + HUH-NK - HUH-UG.
           IF  HUH-YK NOT = W-KIN
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           COMPUTE W-KIN = HUH-YS * HI-FT.
           IF  HUH-YK NOT = W-KIN
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-75.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
