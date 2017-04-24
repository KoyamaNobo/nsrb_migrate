       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG000.
      *********************************************************
      *    PROGRAM         :  履物在庫・非請求　各ファイル変換*
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=HUHM(在庫ｱﾘ , 1=HUHM(在庫ﾅｼ   *
      *                    :  2=SNTRF       , 3=TTM           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-HUH.
           02  WHUH-KEY.
             03  WHUH-HCD     PIC  9(006).
           02  WHUH-NG        PIC  9(006).
           02  WHUH-D.
             03  WHUH-ZS      PIC S9(006).
             03  WHUH-ZK      PIC S9(009).
             03  WHUH-NS      PIC S9(007).
             03  WHUH-NK      PIC S9(010).
             03  WHUH-SS      PIC S9(008).
             03  WHUH-SK      PIC S9(010).
             03  WHUH-YS      PIC S9(006).
             03  WHUH-YK      PIC S9(009).
             03  WHUH-UG      PIC S9(010).
      *
           02  WHUH-BC.
             03  WHUH-BCD1    PIC  9(003).
             03  F            PIC  9(001).
             03  WHUH-BC3     PIC  9(002).
           02  WHUH-BMC       PIC  9(002).
           02  WHUH-BMNO      PIC  9(001).
           02  F              PIC  X(032).
       01  W-SNTR.
           02  WSNTR-DNO      PIC  9(006).
           02  WSNTR-GNO      PIC  9(001).
           02  F              PIC  X(012).
           02  WSNTR-HCD      PIC  9(006).
           02  F              PIC  X(050).
           02  WSNTR-DC       PIC  9(001).
           02  WSNTR-FT       PIC  9(005).
           02  F              PIC  X(046).
           02  WSNTR-SNC      PIC  9(001).
       01  W-DATA.
           02  W-NG           PIC  9(006) VALUE ZERO.
           02  W-FNM          PIC  N(008).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHIM.
           COPY LIHUHM.
           COPY LITTM.
      *FD  SNTRF
       01  SNTRF_HMG000.
           02  SNTRF_PNAME1   PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTRF_LNAME    PIC  X(012) VALUE "SNTRF_HMG000".
           02  F              PIC  X(001).
           02  SNTRF_KEY1     PIC  X(100) VALUE SPACE.
           02  SNTRF_SORT     PIC  X(100) VALUE SPACE.
           02  SNTRF_IDLST    PIC  X(100) VALUE SPACE.
           02  SNTRF_RES      USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  F              PIC  X(008).
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
      *FD  HENF
       01  HENF_HMG000.
           02  HENF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HENF_LNAME     PIC  X(011) VALUE "HENF_HMG000".
           02  F              PIC  X(001).
           02  HENF_KEY1      PIC  X(100) VALUE SPACE.
           02  HENF_SORT      PIC  X(100) VALUE SPACE.
           02  HENF_IDLST     PIC  X(100) VALUE SPACE.
           02  HENF_RES       USAGE  POINTER.
       01  HEN-R              PIC  X(128).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　履物非請求他　各ファイル変換　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-FNM   PIC  N(008).
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  FILLER  PIC  9(006).
             03  E-ME2.
               04  FILLER  PIC  X(016) VALUE
                    "***  TTM ﾅｼ  ***".
               04  FILLER  PIC  9(004).
             03  E-ME3.
               04  FILLER  PIC  X(025) VALUE
                    "***  TTM REWRITE ｴﾗ-  ***".
               04  FILLER  PIC  9(004).
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FNM" "N" "15" "26" "16" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-FNM" BY REFERENCE W-FNM "16" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "71" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "71" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" " " "24" "0" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME1" "X" "24" "15" "16" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME1" "9" "24" "35" "6" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME1" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" " " "24" "0" "20" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME2" "X" "24" "15" "16" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME2" "9" "24" "35" "4" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME2" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" " " "24" "0" "29" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME3" "X" "24" "15" "25" " " "E-ME3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME3" "9" "24" "44" "4" "01E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME3" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN = 0 OR > 3
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               MOVE "　（ＨＵＨＭ）　" TO W-FNM
               CALL "DB_F_Open" USING
                "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST
                "1" "HUH-KEY" BY REFERENCE HUH-KEY
           END-IF
           IF  JS-SIGN = 2
               MOVE "（ＳＮＴＲＦ）　" TO W-FNM
               CALL "DB_F_Open" USING
                "INPUT" SNTRF_PNAME1 "SHARED" BY REFERENCE
                SNTRF_IDLST "0"
           END-IF
           IF  JS-SIGN = 3
               MOVE "　（ＴＴＭ）　　" TO W-FNM
               CALL "DB_F_Open" USING
                "I-O" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
                "TT-KEY" BY REFERENCE TT-KEY
               CALL "DB_F_Open" USING
                "INPUT" SNTRF_PNAME1 "SHARED" BY REFERENCE
                SNTRF_IDLST "0"
           END-IF
           CALL "SD_Output" USING "D-FNM" D-FNM "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  JS-SIGN = 3
               GO TO M-50
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HENF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HENF_PNAME1 " " BY REFERENCE HENF_IDLST "0".
           IF  JS-SIGN = 1
               GO TO M-10
           END-IF
           IF  JS-SIGN = 2
               GO TO M-40
           END-IF.
       M-10.
      *           READ HUH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HUH-M_PNAME1 BY REFERENCE HUH-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  W-NG = ZERO
               MOVE HUH-NG TO W-NG
           END-IF
           IF  ZERO = HUH-ZS AND HUH-ZK AND HUH-NS AND HUH-NK AND
                      HUH-SS AND HUH-SK AND HUH-YS AND HUH-YS AND HUH-UG
               GO TO M-10
           END-IF
           MOVE ZERO TO W-HUH.
           MOVE HUH-R TO W-HUH.
           IF  ZERO = HUH-ZS AND HUH-NS AND HUH-SS AND HUH-YS
               GO TO M-15
           END-IF
           MOVE HUH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HI-YG NOT = ZERO
               COMPUTE WHUH-ZK = WHUH-ZS * HI-YG
               COMPUTE WHUH-NK = WHUH-NS * HI-YG
               COMPUTE WHUH-UG = WHUH-SS * HI-YG
               COMPUTE WHUH-YK = WHUH-YS * HI-YG
           END-IF.
       M-15.
           MOVE W-HUH TO HEN-R.
      *           WRITE HEN-R.
      *//////////////
           CALL "DB_Insert" USING
            HENF_PNAME1 HENF_LNAME HEN-R RETURNING RET.
           GO TO M-10.
       M-20.
           GO TO M-90.
       M-40.
      *           READ SNTRF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           INITIALIZE W-SNTR.
           MOVE SNTR-R TO W-SNTR.
           IF  WSNTR-SNC NOT = 0
               GO TO M-45
           END-IF
           IF  WSNTR-GNO > 6
               GO TO M-45
           END-IF
           IF  WSNTR-DC = 2 OR 4 OR 8
               GO TO M-45
           END-IF
           MOVE WSNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HI-YG NOT = ZERO
               MOVE HI-YG TO WSNTR-FT
           END-IF.
       M-45.
           MOVE W-SNTR TO HEN-R.
      *           WRITE HEN-R.
      *//////////////
           CALL "DB_Insert" USING
            HENF_PNAME1 HENF_LNAME HEN-R RETURNING RET.
           GO TO M-40.
       M-50.
      *           READ TT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  TT-BC NOT = 0
               GO TO M-50
           END-IF
           IF  TT-YUG = ZERO
               GO TO M-50
           END-IF
           MOVE ZERO TO TT-YUG.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-50.
       M-55.
      *           READ SNTRF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-SNC = 1
               GO TO M-55
           END-IF
           IF  SNTR-GNO > 6
               GO TO M-55
           END-IF
           IF  SNTR-DC NOT = 0 AND 1 AND 2 AND 3 AND 5 AND 7
               GO TO M-55
           END-IF
           MOVE SNTR-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-CL" E-CL "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HI-YG = ZERO
               IF  SNTR-DC = 0 OR 3 OR 7
                   COMPUTE TT-YUG = TT-YUG + (SNTR-SU * SNTR-FT)
               ELSE
                   COMPUTE TT-YUG = TT-YUG - (SNTR-SU * SNTR-FT)
               END-IF
           END-IF
           IF  HI-YG NOT = ZERO
               IF  SNTR-DC = 0 OR 3 OR 7
                   COMPUTE TT-YUG = TT-YUG + (SNTR-SU * HI-YG)
               ELSE
                   COMPUTE TT-YUG = TT-YUG - (SNTR-SU * HI-YG)
               END-IF
           END-IF
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-55.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HENF_IDLST HENF_PNAME1
           END-IF
           IF  JS-SIGN = 2
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HENF_IDLST HENF_PNAME1
           END-IF
           IF  JS-SIGN = 3
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
