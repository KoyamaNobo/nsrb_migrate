       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY550.
       AUTHOR. S-NAKAO.
      *********************************************************
      *    PROGRAM         :  得意先順位別売上実績表          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  /20                             *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  順位順=0 , ｺｰﾄﾞ順=1             *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD11.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SY1          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SM1          PIC ZZ.
           02  F              PIC  X(005) VALUE "月 - ".
           02  H-EY1          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EM1          PIC ZZ.
           02  F              PIC  N(019) VALUE
                "月　得意先順位別　売上実績表　　＊＊＊".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE1        PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE1        PIC Z9.
       01  HEAD12.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SY2          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SM2          PIC ZZ.
           02  F              PIC  X(005) VALUE "月 - ".
           02  H-EY2          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EM2          PIC ZZ.
           02  F              PIC  N(021) VALUE
                "月　履物得意先順位別　売上実績表　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE2        PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE2        PIC Z9.
       01  HEAD13.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SY3          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SM3          PIC ZZ.
           02  F              PIC  X(005) VALUE "月 - ".
           02  H-EY3          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EM3          PIC ZZ.
           02  F              PIC  N(022) VALUE
                "月　工品他得意先順位別　売上実績表　　＊＊＊".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE3        PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE3        PIC Z9.
       01  HEAD15.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(012) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SY5          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SM5          PIC ZZ.
           02  F              PIC  X(005) VALUE "月 - ".
           02  H-EY5          PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EM5          PIC ZZ.
           02  F              PIC  N(022) VALUE
                "月　履物得意先コード別　売上実績表　　＊＊＊".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE5        PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE5        PIC Z9.
       01  HEAD2.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(010) VALUE "    ｺｰﾄﾞ  ".
           02  F              PIC  N(007) VALUE "得　意　先　名".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(006) VALUE "売上累計金額".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(012) VALUE
                "％　　粗利累計額　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  P-NO           PIC ZZZ.
           02  F              PIC  X(003).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NA           PIC  N(026).
           02  P-U            PIC ---,---,---,--9.
           02  F              PIC  X(002).
           02  P-URR          PIC ---9.99.
           02  P-UAR          PIC --,---,---,--9.
           02  P-ARR          PIC -----9.99.
       01  W-D.
           02  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-NO           PIC  9(003).
           02  W-URR          PIC S9(003)V9(02).
           02  W-ARR          PIC S9(003)V9(02).
           02  W-DMM          PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-PAGE         PIC  9(002).
       01  WS-D.
           02  WS-U           PIC S9(010).
           02  WS-UAR         PIC S9(010).
       01  WA-D.
           02  WA-U           PIC S9(010).
           02  WA-UAR         PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
       01  TM-GF_HMY550.
           02  TM-GF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  TM-GF_LNAME    PIC  X(012)  VALUE "TM-GF_HMY550".
           02  F              PIC  X(001).
           02  TM-GF_KEY1     PIC  X(100)  VALUE SPACE.
           02  TM-GF_KEY2     PIC  X(100)  VALUE SPACE.
           02  TM-GF_SORT     PIC  X(100)  VALUE SPACE.
           02  TM-GF_IDLST    PIC  X(100)  VALUE SPACE.
           02  TM-GF_RES      USAGE  POINTER.
       01  G-R.
           02  G-TCD          PIC  9(004).
           02  G-KNCD         PIC  9(002).
           02  G-TKC          PIC  9(002).
           02  G-TNC          PIC  9(002).
           02  G-U            PIC S9(010).
           02  G-UAR          PIC S9(010).
           02  F              PIC  X(034).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　得意先　年間順位別　売上実績表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'  年   月  ～　'  年   月".
       01  C-DSP.
           02  D-SIGN    PIC  N(004) VALUE
                "コード順".
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
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
            "C-MID" " " "0" "0" "376" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID" "X" "15" "22" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SIGN" "N" "7" "30" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" "D-SIGN" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "15" "23" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "15" "28" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "15" "39" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "15" "44" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           COPY LIBCPR.
           MOVE D-SPNG TO W-SNG.
           MOVE D-EPNG TO W-ENG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                              RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-SIGN" D-SIGN "p" 
                                              RETURNING RESU
           END-IF.
           IF  JS-SIGN = 0
               MOVE W-SNEN TO H-SY1 H-SY2 H-SY3
               MOVE W-SGET TO H-SM1 H-SM2 H-SM3
               MOVE W-ENEN TO H-EY1 H-EY2 H-EY3
               MOVE W-EGET TO H-EM1 H-EM2 H-EM3
           ELSE
               MOVE W-SNEN TO H-SY5
               MOVE W-SGET TO H-SM5
               MOVE W-ENEN TO H-EY5
               MOVE W-EGET TO H-EM5
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TM-GF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           IF  JS-SIGN = 1
               MOVE 1 TO CNT
           ELSE
               MOVE ZERO TO CNT
           END-IF.
       M-10.
           ADD 1 TO CNT.
           IF  CNT = 4
               GO TO M-95
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" TM-GF_PNAME1 " " BY REFERENCE TM-GF_IDLST "0".
           MOVE ZERO TO WA-D W-NO.
       M-15.
      *           READ TM-GF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-GF_PNAME1 BY REFERENCE G-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
           IF  CNT = 1
               GO TO M-20
           END-IF.
           IF  CNT = 2
               IF  G-TNC < 90
                   GO TO M-20
               END-IF
           END-IF.
           IF  CNT = 3
               IF  G-TNC > 89
                   GO TO M-20
               END-IF
           END-IF.
           GO TO M-15.
       M-20.
           ADD G-U TO WA-U.
           ADD G-UAR TO WA-UAR.
           GO TO M-15.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-GF_IDLST TM-GF_PNAME1.
           IF  WA-U = ZERO
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" TM-GF_PNAME1 " " BY REFERENCE TM-GF_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           IF  JS-SIGN = 1
               MOVE DATE-02R TO H-DATE5
           ELSE
               MOVE DATE-02R TO H-DATE1 H-DATE2 H-DATE3
           END-IF.
           MOVE ZERO TO WS-D W-PAGE.
           PERFORM S-10 THRU S-15.
       M-30.
      *           READ TM-GF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-GF_PNAME1 BY REFERENCE G-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF.
           IF  ZERO = G-U AND G-UAR
               GO TO M-30
           END-IF.
           IF  CNT = 1
               GO TO M-45
           END-IF.
           IF  CNT = 2
               IF  G-TNC < 90
                   GO TO M-45
               END-IF
           END-IF.
           IF  CNT = 3
               IF  G-TNC > 89
                   GO TO M-45
               END-IF
           END-IF.
           GO TO M-30.
       M-45.
           MOVE ZERO TO W-URR.
           IF  G-U NOT = ZERO
               COMPUTE W-URR ROUNDED = (G-U * 100) / WA-U
           END-IF.
           MOVE ZERO TO W-ARR.
           IF  G-UAR = ZERO
               GO TO M-55
           END-IF.
           IF  G-U = ZERO
               GO TO M-50
           END-IF.
           IF  G-U < ZERO
               COMPUTE W-ARR ROUNDED = (G-UAR * -100) / G-U
           ELSE
               COMPUTE W-ARR ROUNDED = (G-UAR * 100) / G-U
           END-IF.
           GO TO M-55.
       M-50.
           IF  G-UAR > ZERO
               MOVE 100 TO W-ARR
           ELSE
               MOVE -100 TO W-ARR
           END-IF.
       M-55.
           ADD 1 TO W-NO.
           MOVE SPACE TO W-P.
           MOVE W-NO TO P-NO.
           MOVE G-TCD TO P-TCD.
           MOVE G-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　得意先マスター　無し　＊＊"  TO T-NAME
           END-IF.
           MOVE T-NAME TO P-NA.
           MOVE G-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE G-UAR TO P-UAR.
           MOVE W-ARR TO P-ARR.
           IF  JS-SIGN = 1
               CALL "PR_Get_Linage" RETURNING LINAGECOUNTER
               IF  LINAGECOUNTER > 63
                   PERFORM S-05 THRU S-15
               END-IF
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE W-P TO SP-R.
           IF  JS-SIGN = 1
               GO TO M-30.
           ADD G-U TO WS-U.
           ADD G-UAR TO WS-UAR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT = 54
               GO TO M-30
           END-IF.
           PERFORM S-20 THRU S-40.
           PERFORM S-05 THRU S-15.
           GO TO M-30.
       M-60.
           IF  JS-SIGN = 0
               PERFORM S-20 THRU S-40
           END-IF.
           PERFORM S-45 THRU S-60.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-GF_IDLST TM-GF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  JS-SIGN = 0
               GO TO M-10
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
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
           MOVE SPACE TO SP-R.
           IF  JS-SIGN = 0
               IF  CNT = 1
                   MOVE W-PAGE TO H-PAGE1
                   MOVE HEAD11 TO SP-R
               ELSE
                   IF  CNT = 2
                       MOVE W-PAGE TO H-PAGE2
                       MOVE HEAD12 TO SP-R
                   ELSE
                       IF  CNT = 3
                           MOVE W-PAGE TO H-PAGE3
                           MOVE HEAD13 TO SP-R
                       END-IF
                   END-IF
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               MOVE W-PAGE TO H-PAGE5
               MOVE HEAD15 TO SP-R
           END-IF.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO W-URR.
           IF  WS-U NOT = ZERO
               COMPUTE W-URR ROUNDED = (WS-U * 100) / WA-U
           END-IF.
       S-25.
           MOVE ZERO TO W-ARR.
           IF  WS-UAR = ZERO
               GO TO S-35
           END-IF.
           IF  WS-U = ZERO
               GO TO S-30
           END-IF.
           IF  WS-U < ZERO
               COMPUTE W-ARR ROUNDED = (WS-UAR * -100) / WS-U
           ELSE
               COMPUTE W-ARR ROUNDED = (WS-UAR * 100) / WS-U
           END-IF.
           GO TO S-35.
       S-30.
           IF  WS-UAR > ZERO
               MOVE 100 TO W-ARR
           ELSE
               MOVE -100 TO W-ARR
           END-IF.
       S-35.
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　　　　　　［　小　計　］　" TO P-NA.
           MOVE WS-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE WS-UAR TO P-UAR.
           MOVE W-ARR TO P-ARR.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO WS-D.
       S-40.
           EXIT.
       S-45.
           MOVE 100 TO W-URR.
           MOVE ZERO TO W-ARR.
           IF  WA-UAR = ZERO
               GO TO S-55
           END-IF.
           IF  WA-U = ZERO
               GO TO S-50
           END-IF.
           IF  WA-U < ZERO
               COMPUTE W-ARR ROUNDED = (WA-UAR * -100) / WA-U
           ELSE
               COMPUTE W-ARR ROUNDED = (WA-UAR * 100) / WA-U
           END-IF.
           GO TO S-55.
       S-50.
           IF  WA-UAR > ZERO
               MOVE 100 TO W-ARR
           ELSE
               MOVE -100 TO W-ARR
           END-IF.
       S-55.
           MOVE SPACE TO W-P.
           MOVE "　【　　総　合　計　　】　　" TO P-NA.
           MOVE WA-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE WA-UAR TO P-UAR.
           MOVE W-ARR TO P-ARR.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-60.
           EXIT.
