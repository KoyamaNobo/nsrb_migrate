       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG060.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-27.
      *********************************************************
      *    PROGRAM         : 　担当者品種別売上粗利集計表     *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/15                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=通常  1=合計                  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  15K                PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(029) VALUE SPACE.
           02  H-MID          PIC  N(026) VALUE SPACE.
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(003) VALUE "担当者".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(005) VALUE "品　　　名".
           02  F              PIC  X(045) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(002) VALUE "原価".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(002) VALUE "粗利".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  F              PIC  X(002).
           02  P-TNC          PIC  9(002).
           02  P-TNCD  REDEFINES P-TNC.
             03  F            PIC  X(001).
             03  P-TNC1       PIC  9(001).
           02  F              PIC  X(003).
           02  P-HCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UKI          PIC --,---,---,--9.
           02  P-SKI          PIC --,---,---,--9.
           02  P-AR           PIC -----,---,--9.
           02  P-RR           PIC -----9.9.
       01  W-D.
           02  W-KEY.
             03  W-TNC.
               04  W-TNC1     PIC  9(001).
               04  W-TNC2     PIC  9(001).
             03  W-HCD1       PIC  9(004).
           02  W-AR           PIC S9(009).
           02  W-RR           PIC S9(003)V9(01).
           02  W-KIN          PIC S9(009).
           02  W-RD.
             03  W-SU         PIC S9(005).
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
       01  WN-D.
           02  WN-SU          PIC S9(007).
           02  WN-UKI         PIC S9(009).
           02  WN-SKI         PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-UKI         PIC S9(009).
           02  WT-SKI         PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-UKI         PIC S9(009).
           02  WS-SKI         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-UKI         PIC S9(009).
           02  WA-SKI         PIC S9(009).
       01  W-DATA.
           02  W-ALL          PIC S9(006)V9(05).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  CHK            PIC  9(003).
           02  CNT            PIC  9(002).
           02  W-NAAD.
             03  W-NA    OCCURS  24  PIC  N(001).
           02  W-NAD   REDEFINES W-NAAD  PIC  N(024).
           02  W-NMAD.
             03  W-NM    OCCURS  24  PIC  N(001).
           02  W-NMD   REDEFINES W-NMAD  PIC  N(024).
           02  W-MID0         PIC  N(026) VALUE
               "　＊＊＊　　担当者品種別　売上粗利集計表　　＊＊＊　".
           02  W-MID1         PIC  N(026) VALUE
               "＊＊＊　　担当者合計品種別　売上粗利集計表　　＊＊＊".
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
                "＊＊　　　　　　　　　　　　　　　　　　＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊　　担当者品種別売上粗利集計表　　　＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊　　　　　　　　　　　　　　　　　　＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-MID   PIC  N(004) VALUE
                "（合計）".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "*** DATA ﾅｼ ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4A05".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
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
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID" "N" "7" "28" "8" " " "C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "20" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               MOVE W-MID0 TO H-MID
           ELSE
               MOVE W-MID1 TO H-MID
               CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU
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
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D.
           MOVE SNTR-TNC1 TO W-TNC1.
       M-20.
           IF  JS-SIGN = 0
               MOVE ZERO TO WT-D
               MOVE SNTR-TNC2 TO W-TNC2
           END-IF
           MOVE ZERO TO CHK.
       M-25.
           MOVE ZERO TO WN-D.
           MOVE SNTR-HCD1 TO W-HCD1.
       M-30.
           ADD W-SU TO WN-SU.
           ADD W-UKIN TO WN-UKI.
           ADD W-GKIN TO WN-SKI.
       M-35.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-35
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN AND W-GKIN
               GO TO M-35
           END-IF
           IF  W-TNC1 NOT = SNTR-TNC1
               GO TO M-50
           END-IF
           IF  JS-SIGN = 0
               IF  W-TNC2 NOT = SNTR-TNC2
                   GO TO M-45
               END-IF
           END-IF
           IF  W-HCD1 NOT = SNTR-HCD1
               GO TO M-40
           END-IF
           GO TO M-30.
       M-40.
           PERFORM S-20 THRU S-40.
           GO TO M-25.
       M-45.
           PERFORM S-20 THRU S-40.
           PERFORM S-45 THRU S-55.
           GO TO M-20.
       M-50.
           PERFORM S-20 THRU S-40.
           IF  JS-SIGN = 0
               PERFORM S-45 THRU S-55
           END-IF
           PERFORM S-60 THRU S-70.
           GO TO M-15.
       M-85.
           PERFORM S-20 THRU S-40.
           IF  JS-SIGN = 0
               PERFORM S-45 THRU S-55
           END-IF
           PERFORM S-60 THRU S-70.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R W-P.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-UKI TO P-UKI.
           MOVE WA-SKI TO P-SKI.
           COMPUTE W-AR = WA-UKI - WA-SKI.
           MOVE W-AR TO P-AR.
           IF  WA-UKI = ZERO
               GO TO M-90
           END-IF
           MOVE WA-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           MOVE W-RR TO P-RR.
       M-90.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DST-RTN.
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       DST-EX.
           EXIT.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO CHK.
       S-15.
           EXIT.
       S-20.
           IF ZERO = WN-SU AND WN-UKI AND WN-SKI
               GO TO S-40
           END-IF
           MOVE SPACE TO W-NAD W-NMD.
           MOVE ZERO TO HI-KEY CNT.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" " NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO W-NAD
               GO TO S-25
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO W-NAD
               GO TO S-25
           END-IF
           IF  W-HCD1 NOT = HI-HCD1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO W-NAD
           ELSE
               MOVE HI-NAME TO W-NAD
           END-IF.
       S-25.
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-30
           END-IF
           MOVE W-NA(CNT) TO W-NM(CNT).
           IF  W-NA(CNT) NOT = SPACE
               GO TO S-25
           END-IF
           ADD 1 TO CNT.
           IF  CNT = 25
               GO TO S-30
           END-IF
           MOVE W-NA(CNT) TO W-NM(CNT).
           IF  W-NA(CNT) NOT = SPACE
               GO TO S-25
           END-IF.
       S-30.
           IF  HI-OL = 1
               GO TO S-37
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P SP-R.
           IF  CHK = ZERO
               IF  JS-SIGN = 0
                   MOVE W-TNC TO P-TNC
               ELSE
                   MOVE W-TNC1 TO P-TNC1
               END-IF
           END-IF
           MOVE W-HCD1 TO P-HCD.
           MOVE W-NMD TO P-HNA.
           MOVE WN-SU TO P-SU.
           MOVE WN-UKI TO P-UKI.
           MOVE WN-SKI TO P-SKI.
           COMPUTE W-AR = WN-UKI - WN-SKI.
           MOVE W-AR TO P-AR.
           IF  WN-UKI = ZERO
               GO TO S-35
           END-IF
           MOVE WN-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           MOVE W-RR TO P-RR.
       S-35.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-37.
           IF  JS-SIGN = 0
               ADD WN-SU TO WT-SU
               ADD WN-UKI TO WT-UKI
               ADD WN-SKI TO WT-SKI
           ELSE
               ADD WN-SU TO WS-SU
               ADD WN-UKI TO WS-UKI
               ADD WN-SKI TO WS-SKI
           END-IF
           IF  HI-OL NOT = 1
               ADD 1 TO CHK
           END-IF.
       S-40.
           EXIT.
       S-45.
           IF  ZERO = WT-SU AND WT-UKI AND WT-SKI
               GO TO S-55
           END-IF
           MOVE SPACE TO W-P.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE "　　　　　　　（　　ＴＯＴＡＬ　　）　" TO P-HNA.
           MOVE WT-SU TO P-SU.
           MOVE WT-UKI TO P-UKI.
           MOVE WT-SKI TO P-SKI.
           COMPUTE W-AR = WT-UKI - WT-SKI.
           MOVE W-AR TO P-AR.
           IF  WT-UKI = ZERO
               GO TO S-50
           END-IF
           MOVE WT-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           MOVE W-RR TO P-RR.
       S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-SU TO WS-SU.
           ADD WT-UKI TO WS-UKI.
           ADD WT-SKI TO WS-SKI.
       S-55.
           EXIT.
       S-60.
           IF  ZERO = WS-SU AND WS-UKI AND WS-SKI
               GO TO S-70
           END-IF
           MOVE SPACE TO W-P.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
               IF  JS-SIGN = 0
                   MOVE W-TNC TO P-TNC
               ELSE
                   MOVE W-TNC1 TO P-TNC1
               END-IF
           END-IF
           MOVE "　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-UKI TO P-UKI.
           MOVE WS-SKI TO P-SKI.
           COMPUTE W-AR = WS-UKI - WS-SKI.
           MOVE W-AR TO P-AR.
           IF  WS-UKI = ZERO
               GO TO S-65
           END-IF
           MOVE WS-UKI TO W-KIN.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1
           END-IF
           COMPUTE W-ALL ROUNDED = W-AR / W-KIN.
           COMPUTE W-RR ROUNDED = W-ALL * 100.
           MOVE W-RR TO P-RR.
       S-65.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-UKI TO WA-UKI.
           ADD WS-SKI TO WA-SKI.
       S-70.
           EXIT.
