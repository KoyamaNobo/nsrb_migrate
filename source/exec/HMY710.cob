       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY710.
      **************************************************************
      *    PROGRAM         :  地下足袋・Ｃ長靴・Ｃ作業履           *
      *                    :  得意先順位品種別　売上集計表         *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *        変更　　　  :  62/05/19                             *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIN             PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  HEAD.
           02  W-HNG.
             03  F            PIC  N(005) VALUE "＊＊＊　　".
             03  H-SN         PIC 99.
             03  F            PIC  N(001) VALUE "年".
             03  H-SG         PIC Z9.
             03  F            PIC  N(004) VALUE "月　～　".
             03  H-EN         PIC 99.
             03  F            PIC  N(001) VALUE "年".
             03  H-EG         PIC Z9.
             03  F            PIC  N(002) VALUE "月　".
           02  W-HMD.
             03  F            PIC  N(019) VALUE
                  "　得意先順位品種別　年間売上粗利集計表".
             03  F            PIC  N(005) VALUE "　　＊＊＊".
           02  W-HDP.
             03  F            PIC  X(005) VALUE "DATE ".
             03  H-DATE       PIC 99B99B99.
             03  F            PIC  X(007) VALUE "     P.".
             03  H-PAGE       PIC ZZ9.
       01  HEAD11.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(013) VALUE SPACE.
           02  H-NG1          PIC  X(034).
           02  F              PIC  N(004) VALUE "地下足袋".
           02  H-MD1          PIC  N(024).
           02  F              PIC  X(006) VALUE SPACE.
           02  H-DP1          PIC  X(023).
       01  HEAD12.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(014) VALUE SPACE.
           02  H-NG2          PIC  X(034).
           02  F              PIC  N(003) VALUE "Ｃ長靴".
           02  H-MD2          PIC  N(024).
           02  F              PIC  X(007) VALUE SPACE.
           02  H-DP2          PIC  X(023).
       01  HEAD13.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(013) VALUE SPACE.
           02  H-NG3          PIC  X(034).
           02  F              PIC  N(004) VALUE "Ｃ作業履".
           02  H-MD3          PIC  N(024).
           02  F              PIC  X(006) VALUE SPACE.
           02  H-DP3          PIC  X(023).
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "　№".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(106) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上数量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　足単価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　売上粗利益".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-NO           PIC ZZ9.
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(082).
       01  W-P2.
           02  F              PIC  X(015).
           02  P-HCD.
             03  F            PIC  X(002).
             03  P-HCDD       PIC  9(004).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UK           PIC --,---,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-GK           PIC --,---,---,--9.
           02  P-GT           PIC ---,--9.
           02  P-AR           PIC -----,---,--9.
           02  P-RR           PIC ----9.9.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-HCD.
             03  W-HCD1       PIC  9(004).
             03  F            PIC  X(002).
           02  W-AR           PIC S9(009).
           02  W-UT           PIC S9(005).
           02  W-GT           PIC S9(005).
           02  W-RR           PIC S9(003)V9(01).
           02  W-KIN          PIC S9(010).
           02  W-NO           PIC  9(003) VALUE ZERO.
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CNT            PIC  9(003).
           02  W-TNA          PIC  N(026).
           02  W-AHNA.
             03  W-HNA   OCCURS  24  PIC  N(001).
           02  W-HNAD  REDEFINES W-AHNA  PIC  N(024).
           02  W-ANA.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-NAD   REDEFINES W-ANA   PIC  N(024).
           02  W-C            PIC  9(002).
           02  CHK            PIC  9(001).
       01  WS-D.
           02  WS-SU          PIC S9(008).
           02  WS-UK          PIC S9(010).
           02  WS-GK          PIC S9(010).
           02  WS-AR          PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(008).
           02  WA-UK          PIC S9(010).
           02  WA-GK          PIC S9(010).
           02  WA-AR          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
       01  SSR-YF_HMY710.
           02  SSR-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  SSR-YF_LNAME    PIC  X(013)  VALUE "SSR-YF_HMY710".
           02  F               PIC  X(001).
           02  SSR-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  SSR-YF_RES      USAGE  POINTER.
       01  SSR-YR.
           02  Y-TCD          PIC  9(004).
           02  Y-HCD          PIC  9(006).
           02  Y-SU           PIC S9(007).
           02  Y-UK           PIC S9(010).
           02  Y-GK           PIC S9(010).
           02  Y-TC1          PIC  9(002).
           02  Y-TC2          PIC  9(002).
           02  Y-BC1          PIC  9(002).
           02  Y-BC2          PIC  9(002).
           02  Y-BC3          PIC  9(002).
           02  Y-FKC          PIC  9(002).
           02  Y-NG           PIC  9(006).
           02  F              PIC  X(073).
       77  F                  PIC  X(001).
       01  TNO-F_HMY710.
           02  TNO-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  TNO-F_LNAME    PIC  X(012)  VALUE "TNO-F_HMY710".
           02  F              PIC  X(001).
           02  TNO-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  TNO-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  TNO-F_SORT     PIC  X(100)  VALUE SPACE.
           02  TNO-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  TNO-F_RES      USAGE  POINTER.
       01  TNO-R.
           02  NO-TCD         PIC  9(004).
           02  NO-SU          PIC S9(007).
           02  NO-KIN         PIC S9(010).
           02  NO-GKIN        PIC S9(010).
           02  F              PIC  X(225).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　得意先順位品種別　売上集計表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(024) VALUE
                "'  年   月 ～ '  年   月".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-MID1  PIC  N(008) VALUE
                  "（　地下足袋　）".
             03  D-MID2  PIC  N(008) VALUE
                  "（　Ｃ長靴　）　".
             03  D-MID3  PIC  N(008) VALUE
                  "（　Ｃ作業履　）".
           02  D-NG.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-TCD     PIC  9(004).
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "430" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "2" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "3" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "4" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "5" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "6" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "7" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "8" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "N" "9" "10" "48" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "14" "22" "24" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID" "X" "20" "23" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "5" "0" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID1" "N" "5" "26" "16" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID2" "N" "5" "26" "16" "D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-MID3" "N" "5" "26" "16" "D-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "14" "0" "8" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "Z9" "14" "23" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "14" "28" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "Z9" "14" "37" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "14" "42" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "35" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-TCD" "9" "24" "40" "4" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE NO-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIN FROM ARGUMENT-VALUE.
           IF  JS-SIN > 2
               GO TO M-05
           END-IF.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           MOVE D-SPNEN TO W-SNEN H-SN.
           MOVE D-SPGET TO W-SGET H-SG.
           MOVE D-EPNEN TO W-ENEN H-EN.
           MOVE D-EPGET TO W-EGET H-EG.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
           MOVE DATE-02R TO H-DATE.
           IF  JS-SIN = 0
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" 
                                         RETURNING RESU
               MOVE W-HNG TO H-NG1
               MOVE W-HMD TO H-MD1
           END-IF.
           IF  JS-SIN = 1
               CALL "SD_Output" USING "D-MID2" D-MID2 "p" 
                                         RETURNING RESU
               MOVE W-HNG TO H-NG2
               MOVE W-HMD TO H-MD2
           END-IF.
           IF  JS-SIN = 2
               CALL "SD_Output" USING "D-MID3" D-MID3 "p" 
                                         RETURNING RESU
               MOVE W-HNG TO H-NG3
               MOVE W-HMD TO H-MD3
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0128ID TO SSR-YF_PNAME1.
           MOVE WK0256ID TO TNO-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TNO-F_PNAME1 " " BY REFERENCE TNO-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
      *           READ TNO-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TNO-F_PNAME1 BY REFERENCE TNO-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           IF  ZERO = NO-SU AND NO-KIN AND NO-GKIN
               GO TO M-15
           END-IF.
           MOVE ZERO TO WS-D CNT.
           ADD 1 TO W-NO.
       M-20.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-20
           END-IF.
       M-25.
           IF  NO-TCD NOT = Y-TCD
               GO TO M-20
           END-IF.
       M-30.
           PERFORM S-65 THRU S-75.
       M-35.
           MOVE ZERO TO W-C CHK.
           MOVE SPACE TO W-P2 W-HNAD W-NAD.
           MOVE Y-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＨＩ－Ｍ　無し　＊＊　　　" TO HI-NAME
           END-IF.
           IF  JS-SIN NOT = 2
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO W-HNAD
               GO TO M-45
           END-IF.
           MOVE W-HCD1 TO P-HCDD.
           MOVE HI-NAME TO W-NAD.
       M-40.
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO M-45
           END-IF.
           MOVE W-NA(W-C) TO W-HNA(W-C).
           IF  W-NA(W-C) NOT = SPACE
               GO TO M-40
           END-IF.
           ADD 1 TO W-C.
           IF  W-C = 25
               GO TO M-45
           END-IF.
           MOVE W-NA(W-C) TO W-HNA(W-C).
           IF  W-NA(W-C) NOT = SPACE
               GO TO M-40
           END-IF.
       M-45.
           MOVE W-HNAD TO P-HNA.
           MOVE Y-SU TO P-SU.
           MOVE Y-UK TO P-UK.
           MOVE Y-GK TO P-GK.
           COMPUTE W-AR = Y-UK - Y-GK.
           MOVE W-AR TO P-AR.
           MOVE ZERO TO W-UT W-GT.
           IF  Y-SU NOT = ZERO
               IF  Y-UK NOT = ZERO
                   COMPUTE W-UT ROUNDED = Y-UK / Y-SU
               END-IF
           END-IF.
           IF  Y-SU NOT = ZERO
               IF  Y-GK NOT = ZERO
                   COMPUTE W-GT ROUNDED = Y-GK / Y-SU
               END-IF
           END-IF.
           MOVE W-UT TO P-UT.
           MOVE W-GT TO P-GT.
           MOVE ZERO TO W-RR.
           MOVE Y-UK TO W-KIN.
           IF  W-AR = ZERO
               GO TO M-55
           END-IF.
           IF  Y-UK = ZERO
               GO TO M-50
           END-IF.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF.
           COMPUTE W-RR ROUNDED = (W-AR * 100) / W-KIN.
           GO TO M-55.
       M-50.
           IF  W-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR
           END-IF.
       M-55.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-65 THRU S-75
           END-IF.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD Y-SU TO WS-SU.
           ADD Y-UK TO WS-UK.
           ADD Y-GK TO WS-GK.
           ADD W-AR TO WS-AR.
           ADD 1 TO CNT.
       M-60.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-65
           END-IF.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-60
           END-IF.
           IF  Y-TCD = NO-TCD
               GO TO M-35
           END-IF.
       M-65.
           PERFORM S-20 THRU S-40.
       M-70.
      *           READ TNO-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TNO-F_PNAME1 BY REFERENCE TNO-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
           IF  ZERO = NO-SU AND NO-KIN AND NO-GKIN
               GO TO M-70
           END-IF.
           MOVE ZERO  TO WS-D CNT.
           ADD 1 TO W-NO.
           IF  Y-TCD > NO-TCD
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0"
               GO TO M-20
           END-IF.
           GO TO M-25.
       M-75.
           IF  NO-KIN NOT = ZERO
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-TCD" E-TCD "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               GO TO M-95
           END-IF.
      *           READ TNO-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TNO-F_PNAME1 BY REFERENCE TNO-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF.
           MOVE ZERO  TO WS-D CNT.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0".
           GO TO M-20.
       M-80.
           MOVE SPACE TO W-P2.
           MOVE ALL "　" TO P-HNA.
           MOVE "　【　　総　合　計　　】　　　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-UK TO P-UK.
           MOVE WA-GK TO P-GK.
           MOVE WA-AR TO P-AR.
           MOVE ZERO TO W-RR.
           MOVE WA-UK TO W-KIN.
           IF  WA-AR = ZERO
               GO TO M-90
           END-IF.
           IF  WA-UK = ZERO
               GO TO M-85
           END-IF.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF.
           COMPUTE W-RR ROUNDED = (WA-AR * 100) / W-KIN.
           GO TO M-90.
       M-85.
           IF  WA-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR
           END-IF.
       M-90.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           IF  JS-SIN = 0
               MOVE W-HDP TO H-DP1
               MOVE HEAD11 TO SP-R
           END-IF.
           IF  JS-SIN = 1
               MOVE W-HDP TO H-DP2
               MOVE HEAD12 TO SP-R
           END-IF.
           IF  JS-SIN = 2
               MOVE W-HDP TO H-DP3
               MOVE HEAD13 TO SP-R
           END-IF.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1 W-P2.
           MOVE ALL "　" TO P-HNA.
           IF  CNT = 1
               GO TO S-35
           END-IF.
           MOVE "　　　　　　　　　　［　合　計　］　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-UK TO P-UK.
           MOVE WS-GK TO P-GK.
           MOVE WS-AR TO P-AR.
           MOVE ZERO TO W-RR.
           MOVE WS-UK TO W-KIN.
           IF  WS-AR = ZERO
               GO TO S-30
           END-IF.
           IF  WS-UK = ZERO
               GO TO S-25
           END-IF.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF.
           COMPUTE W-RR ROUNDED = (WS-AR * 100) / W-KIN.
           GO TO S-30.
       S-25.
           IF  WS-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR
           END-IF.
       S-30.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-65 THRU S-75
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-35.
           MOVE SPACE TO SP-R.
           ADD WS-SU TO WA-SU.
           ADD WS-UK TO WA-UK.
           ADD WS-GK TO WA-GK.
           ADD WS-AR TO WA-AR.
       S-40.
           EXIT.
       S-65.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO W-P1.
           MOVE ALL "　" TO P-TNA.
           MOVE W-NO TO P-NO.
           MOVE NO-TCD TO P-TCD.
           MOVE NO-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　得意先マスター　無し　＊＊" TO T-NAME
           END-IF.
           MOVE T-NAME TO W-TNA.
           MOVE W-TNA TO P-TNA.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.
