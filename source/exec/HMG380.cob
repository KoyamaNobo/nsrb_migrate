       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMG380.
      *********************************************************
      *    PROGRAM         :  品名別返品・不良返品合計表　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN    1    :  返品                            *
      *               2    :  不良返品                        *
      *               3    :  不良返品(原価)                  *
      *               5    :  入庫格外品(原価)                *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001) VALUE 0.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(009) VALUE SPACE.
           02  H-MID          PIC  N(024).
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005)  VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007)  VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(008) VALUE " ｺｰﾄﾞ   ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　単　価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  H-KGM          PIC  N(004) VALUE SPACE.
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC -----,--9.
           02  P-T            PIC ----,--9.
           02  P-KIN          PIC ---,---,--9.
           02  P-KKIN         PIC ---,---,--9.
       01  W-DATA.
           02  W-BC3          PIC  9(002).
           02  W-BC1          PIC  9(002).
           02  W-BC1D REDEFINES W-BC1.
             03  W-BC11       PIC  9(001).
             03  W-BC12       PIC  9(001).
           02  W-FTC          PIC  9(001).
           02  W-T            PIC S9(005).
           02  W-KIN          PIC S9(008).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-MID1         PIC  N(024) VALUE
                "　　＊＊＊　　品名別　返品合計表　　＊＊＊　　　".
           02  W-MID2         PIC  N(024) VALUE
                "　＊＊＊　　品名別　不良返品合計表　　＊＊＊　　".
           02  W-MID3         PIC  N(024) VALUE
                "＊＊＊　　品名別　不良返品合計表（原価）　＊＊＊".
           02  W-MID5         PIC  N(024) VALUE
                "＊＊＊　　品名別　格外品合計表（入庫）　＊＊＊　".
       01  WN-D.
           02  WN-SU          PIC S9(007).
           02  WN-KIN         PIC S9(008).
           02  WN-KKIN        PIC S9(008).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-KIN         PIC S9(008).
           02  WT-KKIN        PIC S9(008).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(008).
           02  WS-KKIN        PIC S9(008).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(008).
           02  WA-KKIN        PIC S9(008).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LSSNTW.
           COPY LSPF.
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物返品・不良返品合計表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID5.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物　入庫　格外品合計表　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-MID5
       CALL "SD_Init" USING
           "C-MID5" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID5" "N" "6" "10" "44" " " "C-MID5" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN NOT = 1 AND 2 AND 3 AND 5
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 5
               CALL "SD_Output" USING
                "C-MID5" C-MID5 "p" RETURNING RESU
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-10.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JS-SIGN = 3 OR 5
               IF  SNTR-SU = ZERO
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1 OR 2
               IF  ZERO = SNTR-SU OR SNTR-KIN
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE W-MID1 TO H-MID
           END-IF
           IF  JS-SIGN = 2
               MOVE W-MID2 TO H-MID
           END-IF
           IF  JS-SIGN = 3
               MOVE "格外金額" TO H-KGM
               MOVE W-MID3 TO H-MID
           END-IF
           IF  JS-SIGN = 5
               MOVE "格外金額" TO H-KGM
               MOVE W-MID5 TO H-MID
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D.
           MOVE SNTR-BC3 TO W-BC3.
           IF  JS-SIGN = 1 OR 2
               GO TO M-30
           END-IF.
       M-20.
           MOVE ZERO TO WT-D W-BC1.
           IF  W-BC3 NOT = 10
               MOVE SNTR-BC1 TO W-BC1
               MOVE 0 TO W-FTC
               GO TO M-30
           ELSE
               MOVE SNTR-BMNO TO W-BC11
           END-IF.
       M-25.
           MOVE ZERO TO WN-D.
           MOVE SNTR-FTC TO W-FTC.
       M-30.
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO HI-NAME
           END-IF
           IF  JS-SIGN = 3 OR 5
               COMPUTE W-KIN = HI-FT * SNTR-SU
               MOVE HI-FT TO W-T
           ELSE
               MOVE SNTR-KIN TO W-KIN
               COMPUTE W-T ROUNDED = SNTR-KIN / SNTR-SU
           END-IF
           MOVE SPACE TO W-P.
           MOVE SNTR-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE SNTR-SU TO P-SU.
           MOVE W-T TO P-T.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE ALL "　" TO P-HNA.
           IF  JS-SIGN = 1 OR 2
               ADD SNTR-SU TO WS-SU
               ADD W-KIN TO WS-KIN
           END-IF
           IF  JS-SIGN = 3 OR 5
               IF  W-BC3 NOT = 10
                   ADD SNTR-SU TO WT-SU
                   ADD W-KIN TO WT-KIN
               ELSE
                   ADD SNTR-SU TO WN-SU
                   ADD W-KIN TO WN-KIN
               END-IF
           END-IF.
       M-45.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  JS-SIGN = 3 OR 5
               IF  SNTR-SU = ZERO
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 1 OR 2
               IF  ZERO = SNTR-SU OR SNTR-KIN
                   GO TO M-45
               END-IF
           END-IF
           IF  SNTR-BC3 NOT = W-BC3
               IF  JS-SIGN = 1 OR 2
                   GO TO M-55
               ELSE
                   GO TO M-50
               END-IF
           END-IF
           IF  JS-SIGN = 1 OR 2
               GO TO M-20
           END-IF
           IF  W-BC3 = 10
               IF  SNTR-BMNO = W-BC11
                   IF  SNTR-FTC = W-FTC
                       GO TO M-30
                   ELSE
                       PERFORM S-20 THRU S-25
                       GO TO M-25
                   END-IF
               END-IF
           END-IF
           IF  W-BC3 NOT = 10
               IF  SNTR-BC1 = W-BC1
                   GO TO M-30
               END-IF
           END-IF
           IF  W-BC3 = 10
               PERFORM S-20 THRU S-25
           END-IF
           PERFORM S-30 THRU S-35.
           GO TO M-20.
       M-50.
           IF  W-BC3 = 10
               PERFORM S-20 THRU S-25
           END-IF
           PERFORM S-30 THRU S-35.
       M-55.
           PERFORM S-40 THRU S-45.
           GO TO M-15.
       M-80.
           IF  JS-SIGN = 1 OR 2
               GO TO M-85
           END-IF
           IF  W-BC3 = 10
               PERFORM S-20 THRU S-25
           END-IF
           PERFORM S-30 THRU S-35.
       M-85.
           PERFORM S-40 THRU S-45.
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-HNA.
           MOVE "　　　【　総　合　計　】　　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           IF  JS-SIGN = 3 OR 5
               MOVE WA-KKIN TO P-KKIN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
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
           IF  W-FTC = 1
               COMPUTE WN-KKIN = WN-SU * 1500
           ELSE
               COMPUTE WN-KKIN = WN-SU * 2500
           END-IF
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-HNA.
           MOVE "　　　　　　　　　　　　　　　＜　計　＞" TO P-HNA.
           MOVE WN-SU TO P-SU.
           MOVE WN-KIN TO P-KIN.
           MOVE WN-KKIN TO P-KKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WN-SU TO WT-SU.
           ADD WN-KIN TO WT-KIN.
           ADD WN-KKIN TO WT-KKIN.
       S-25.
           EXIT.
       S-30.
           IF  JS-SIGN = 3 OR 5
               IF  W-BC3 = 20
                   COMPUTE WT-KKIN = WT-SU * 700
               ELSE
                     IF  W-BC3 = 30
                         IF  W-BC1 = 72
                             COMPUTE WT-KKIN = WT-SU * 1000
                         ELSE
                             COMPUTE WT-KKIN = WT-SU * 500
                         END-IF
                     END-IF
               END-IF
           END-IF
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-HNA.
           MOVE "　　　　　　　　　　（　小　計　）" TO P-HNA.
           MOVE WT-SU TO P-SU.
           MOVE WT-KIN TO P-KIN.
           IF  JS-SIGN = 3 OR 5
               MOVE WT-KKIN TO P-KKIN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WT-SU TO WS-SU.
           ADD WT-KIN TO WS-KIN.
           ADD WT-KKIN TO WS-KKIN.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE ALL "　" TO P-HNA.
           MOVE "　　　　　　　　［　合　計　］" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           IF  JS-SIGN = 3 OR 5
               MOVE WS-KKIN TO P-KKIN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
           ADD WS-KKIN TO WA-KKIN.
       S-45.
           EXIT.
