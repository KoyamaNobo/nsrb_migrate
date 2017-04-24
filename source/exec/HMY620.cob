       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY620.
       AUTHOR.  S-NAKAO.
      *************************************************************
      *    PROGRAM         :  担当者　得意先別　売上実績表        *
      *    PRINTER TYPE    :  JIPS                                *
      *    SCREEN          :  ******                              *
      *        変更　　　  :  62/05/20                            *
      *    COMPILE TYPE    :  COBOL                               *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIN             PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SY           PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SM           PIC ZZ.
           02  F              PIC  X(005) VALUE "月 - ".
           02  H-EY           PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EM           PIC ZZ.
           02  F              PIC  N(002) VALUE "月　".
           02  H-CT2          PIC  N(002) VALUE "担当".
           02  F              PIC  N(015) VALUE
                "得意先別　売上実績表　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(006) VALUE "売上累計金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "比率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(006) VALUE "粗利累計金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　粗利率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TN           PIC  N(026).
           02  P-U            PIC ---,---,---,--9.
           02  P-URR          PIC -----9.99.
           02  P-UA           PIC --,---,---,--9.
           02  P-ARR          PIC -----9.99.
           02  F              PIC  X(027).
       01  W-DATA.
           02  W-URR          PIC S9(003)V9(02).
           02  W-ARR          PIC S9(003)V9(02).
           02  W-TC.
             03  W-TC1        PIC  9(001).
             03  W-TC2        PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(001).
           02  W-D.
             03  W-U          PIC S9(010).
             03  W-UA         PIC S9(010).
           02  W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
       01  WT-D.
           02  WT-U           PIC S9(010).
           02  WT-UA          PIC S9(010).
       01  WS-D.
           02  WS-U           PIC S9(010).
           02  WS-UA          PIC S9(010).
       01  WA-D.
           02  WA-U           PIC S9(010).
           02  WA-UA          PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
       01  TM-GF_HMY620.
           02  TM-GF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  TM-GF_LNAME    PIC  X(012)  VALUE "TM-GF_HMY620".
           02  F              PIC  X(001).
           02  TM-GF_KEY1     PIC  X(100)  VALUE SPACE.
           02  TM-GF_KEY2     PIC  X(100)  VALUE SPACE.
           02  TM-GF_SORT     PIC  X(100)  VALUE SPACE.
           02  TM-GF_IDLST    PIC  X(100)  VALUE SPACE.
           02  TM-GF_RES      USAGE  POINTER.
       01  TM-R.
           02  TM-TCD         PIC  9(004).
           02  F              PIC  9(004).
           02  TM-TC.
             03  TM-TC1       PIC  9(001).
             03  TM-TC2       PIC  9(001).
           02  TM-D.
             03  TM-U         PIC S9(010).
             03  TM-UA        PIC S9(010).
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
                "＊＊＊　　年間　　　得意先別　販売実績表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'  年   月  ～  '  年   月".
       01  C-DSP.
           02  FILLER.
             03  D-CT2     PIC N(02).
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
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
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "6" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "D-CT2" "N" "6" "26" "4" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-CT2" BY REFERENCE H-CT2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" "01C-DSP" " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "10" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE D-SPNG TO W-SNG.
           MOVE D-EPNG TO W-ENG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-CT2" D-CT2 "p" 
                                              RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                              RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO TM-GF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TM-GF_PNAME1 " " BY REFERENCE TM-GF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           MOVE ZERO TO WA-D.
       M-10.
      *           READ TM-GF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-GF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF.
           MOVE TM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                MOVE 1 TO T-BC
           END-IF.
           ADD TM-U TO WA-U.
           IF  T-BC = 0
               ADD TM-UA TO WA-UA
           END-IF.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-GF_IDLST TM-GF_PNAME1.
           IF  WA-U = ZERO
               CALL "DB_F_Close" USING
                BY REFERENCE T-M_IDLST T-M_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                              RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE W-SNEN TO H-SY.
           MOVE W-SGET TO H-SM.
           MOVE W-ENEN TO H-EY.
           MOVE W-EGET TO H-EM.
           CALL "DB_F_Open" USING
            "INPUT" TM-GF_PNAME1 " " BY REFERENCE TM-GF_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-20.
      *           READ TM-GF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-GF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
           MOVE TM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＴＭ　なし　＊＊"  TO T-NAME
               MOVE 1 TO T-BC
           END-IF.
           IF  T-BC NOT = 0
               MOVE ZERO TO TM-UA
           END-IF.
           IF  ZERO = TM-U AND TM-UA
               GO TO M-20
           END-IF.
       M-25.
           MOVE ZERO TO WS-D.
           MOVE TM-TC1 TO W-TC1.
       M-30.
           MOVE ZERO TO WT-D W-DMM.
           MOVE TM-TC2 TO W-TC2.
       M-35.
           MOVE ZERO TO W-D.
           MOVE TM-D TO W-D.
           PERFORM S-55 THRU S-65.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TN.
           IF  W-DMM = ZERO
               MOVE 5 TO W-DMM
               MOVE W-TC TO P-TC
           END-IF.
           MOVE TM-TCD TO P-TCD.
           MOVE T-NAME TO P-TN.
           MOVE TM-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE TM-UA TO P-UA.
           MOVE W-ARR TO P-ARR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD TM-U TO WT-U.
           ADD TM-UA TO WT-UA.
       M-45.
      *           READ TM-GF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TM-GF_PNAME1 BY REFERENCE TM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           MOVE TM-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＴＭ　なし　＊＊"  TO T-NAME
               MOVE 1 TO T-BC
           END-IF.
           IF  T-BC NOT = 0
               MOVE ZERO TO TM-UA
           END-IF.
           IF  ZERO = TM-U AND TM-UA
               GO TO M-45
           END-IF.
           IF  W-TC1 NOT = TM-TC1
               GO TO M-55
           END-IF.
           IF  W-TC2 NOT = TM-TC2
               GO TO M-50
           END-IF.
           GO TO M-35.
       M-50.
           PERFORM S-20 THRU S-30.
           GO TO M-30.
       M-55.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-25.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE TM-GF_IDLST TM-GF_PNAME1.
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
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO W-D.
           MOVE WT-D TO W-D.
           PERFORM S-55 THRU S-65.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TN.
           MOVE "　　　　　　　　　（　　ＴＯＴＡＬ　　）　" TO P-TN.
           MOVE WT-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE WT-UA TO P-UA.
           MOVE W-ARR TO P-ARR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-U TO WS-U.
           ADD WT-UA TO WS-UA.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO W-D.
           MOVE WS-D TO W-D.
           PERFORM S-55 THRU S-65.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TN.
           MOVE "　　　　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-TN.
           MOVE WS-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE WS-UA TO P-UA.
           MOVE W-ARR TO P-ARR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-40.
           EXIT.
       S-45.
           MOVE ZERO TO W-D.
           MOVE WA-D TO W-D.
           PERFORM S-55 THRU S-65.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TN.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】　　" TO P-TN.
           MOVE WA-U TO P-U.
           MOVE W-URR TO P-URR.
           MOVE WA-UA TO P-UA.
           MOVE W-ARR TO P-ARR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-50.
           EXIT.
       S-55.
           MOVE ZERO TO W-URR.
           IF  W-U NOT = ZERO
               COMPUTE W-URR ROUNDED = (W-U * 100) / WA-U
           END-IF.
           MOVE ZERO TO W-ARR.
           IF  W-UA = ZERO
               GO TO S-65
           END-IF.
           IF  W-U = ZERO
               GO TO S-60
           END-IF.
           IF  W-U < ZERO
               COMPUTE W-ARR ROUNDED = (W-UA * -100) / W-U
           ELSE
               COMPUTE W-ARR ROUNDED = (W-UA * 100) / W-U
           END-IF.
           GO TO S-65.
       S-60.
           IF  W-UA > ZERO
               MOVE 100 TO W-ARR
           ELSE
               MOVE -100 TO W-ARR
           END-IF.
       S-65.
           EXIT.
