       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG160.
      *********************************************************
      *    PROGRAM         :  履物担当得意先別売上粗利集計表  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *        変更　　　  :  62/05/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  H-SAN          PIC  N(001) VALUE SPACE.
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　履物担当得意先別　売上粗利集計表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(025) VALUE
                "担当 ｺｰﾄﾞ  得　意　先　名".
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(041) VALUE
                "売上金額     売上原価    売上粗利 利益率%".
           02  F              PIC  X(025) VALUE
                " 教育売上金額    教育粗利".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNAME        PIC  N(026).
           02  P-U            PIC ------,---,--9.
           02  P-UG           PIC -----,---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC -----9.9.
           02  P-KU           PIC -----,---,--9.
           02  P-KA           PIC ----,---,--9.
       01  W-DATA.
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-WU           PIC S9(009).
           02  W-NAME         PIC  N(026).
           02  W-ALL          PIC S9(009)V9(05).
           02  W-PAGE         PIC  9(002).
           02  CNT            PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-KIN.
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
       01  WN-D.
           02  W-U            PIC S9(009).
           02  W-UG           PIC S9(009).
           02  W-AR           PIC S9(009).
           02  W-RR           PIC -999V9.
           02  W-KU           PIC S9(009).
           02  W-KG           PIC S9(009).
           02  W-KA           PIC S9(009).
       01  WT-D.
           02  WT-U           PIC S9(009).
           02  WT-UG          PIC S9(009).
           02  WT-AR          PIC S9(009).
           02  WT-KU          PIC S9(009).
           02  WT-KA          PIC S9(009).
       01  WS-D.
           02  WS-U           PIC S9(009).
           02  WS-UG          PIC S9(009).
           02  WS-AR          PIC S9(009).
           02  WS-KU          PIC S9(009).
           02  WS-KA          PIC S9(009).
       01  WA-D.
           02  WA-U           PIC S9(009).
           02  WA-UG          PIC S9(009).
           02  WA-AR          PIC S9(009).
           02  WA-KU          PIC S9(009).
           02  WA-KA          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　担当　得意先別　売上粗利集計表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
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
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
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
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
           MOVE ZERO TO W-KIN.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF  ZERO = W-UKIN AND W-GKIN
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D W-PAGE.
           IF  COMPLETION_CODE = 010
               MOVE "参" TO H-SAN
           END-IF
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE SNTR-TNC1 TO W-TNC1.
           MOVE ZERO TO WS-D.
       M-20.
           MOVE SNTR-TNC2 TO W-TNC2.
           MOVE ZERO TO WT-D CHK CNT.
       M-25.
           MOVE SNTR-TCD TO W-TCD.
           MOVE ZERO TO WN-D.
           MOVE SPACE TO W-NAME.
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO T-NAME
           END-IF
           MOVE T-NAME TO W-NAME.
       M-30.
           ADD W-UKIN TO W-U.
           ADD W-GKIN TO W-UG.
           IF  SNTR-BC3 = 30
               ADD W-UKIN TO W-KU
               ADD W-GKIN TO W-KG
           END-IF.
       M-35.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-35
           END-IF
           MOVE ZERO TO W-KIN.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF  ZERO = W-UKIN AND W-GKIN
               GO TO M-35
           END-IF
           IF  SNTR-TNC1 NOT = W-TNC1
               GO TO M-50
           END-IF
           IF  SNTR-TNC2 NOT = W-TNC2
               GO TO M-45
           END-IF
           IF  SNTR-TCD = W-TCD
               GO TO M-30
           END-IF.
       M-40.
           PERFORM S-20 THRU S-30.
           GO TO M-25.
       M-45.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-50.
           GO TO M-20.
       M-50.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-50.
           PERFORM S-55 THRU S-65.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-50.
           PERFORM S-55 THRU S-65.
           PERFORM S-70 THRU S-80.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
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
           IF  ZERO = W-U AND W-UG AND W-KU AND W-KG
               GO TO S-30
           END-IF
           IF  W-TNC > 89
               MOVE W-U TO W-UG
           END-IF
           COMPUTE W-AR = W-U - W-UG.
           COMPUTE W-KA = W-KU - W-KG.
           MOVE ZERO TO W-RR.
           IF  ZERO = W-U OR W-AR
               GO TO S-25
           END-IF
           MOVE W-U TO W-WU.
           IF  W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (W-AR / W-WU) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       S-25.
           MOVE SPACE TO W-P.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               MOVE W-TNC TO P-TNC
           END-IF
           MOVE W-TCD TO P-TCD.
           MOVE W-NAME TO P-TNAME.
           MOVE W-U TO P-U.
           MOVE W-UG TO P-UG.
           MOVE W-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF  ZERO NOT = W-KU AND W-KA
               MOVE W-KU TO P-KU
               MOVE W-KA TO P-KA
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-U TO WT-U.
           ADD W-UG TO WT-UG.
           ADD W-AR TO WT-AR.
           ADD W-KU TO WT-KU.
           ADD W-KA TO WT-KA.
           ADD 1 TO CNT.
       S-30.
           EXIT.
       S-35.
           IF  CNT = 1
               GO TO S-45
           END-IF
           MOVE ZERO TO W-RR.
           IF  ZERO = WT-U OR WT-AR
               GO TO S-40
           END-IF
           MOVE WT-U TO W-WU.
           IF  W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WT-AR / W-WU) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       S-40.
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　　（　　合　計　　）　" TO P-TNAME.
           MOVE WT-U TO P-U.
           MOVE WT-UG TO P-UG.
           MOVE WT-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF  ZERO NOT = WT-KU AND WT-KA
               MOVE WT-KU TO P-KU
               MOVE WT-KA TO P-KA
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-U TO WS-U.
           ADD WT-UG TO WS-UG.
           ADD WT-AR TO WS-AR.
           ADD WT-KU TO WS-KU.
           ADD WT-KA TO WS-KA.
       S-50.
           EXIT.
       S-55.
           MOVE ZERO TO W-RR.
           IF  ZERO = WS-U OR WS-AR
               GO TO S-60
           END-IF
           MOVE WS-U TO W-WU.
           IF  W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WS-AR / W-WU) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       S-60.
           MOVE SPACE TO W-P.
           MOVE "　　　　　［　　小　計　　］　　　　　" TO P-TNAME.
           MOVE WS-U TO P-U.
           MOVE WS-UG TO P-UG.
           MOVE WS-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF  ZERO NOT = WS-KU AND WS-KA
               MOVE WS-KU TO P-KU
               MOVE WS-KA TO P-KA
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-U TO WA-U.
           ADD WS-UG TO WA-UG.
           ADD WS-AR TO WA-AR.
           ADD WS-KU TO WA-KU.
           ADD WS-KA TO WA-KA.
       S-65.
           EXIT.
       S-70.
           MOVE ZERO TO W-RR.
           IF  ZERO = WA-U OR WA-AR
               GO TO S-75
           END-IF
           MOVE WA-U TO W-WU.
           IF  W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WA-AR / W-WU) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       S-75.
           MOVE SPACE TO W-P.
           MOVE "　【　　総　合　計　　】　　　　　　" TO P-TNAME.
           MOVE WA-U TO P-U.
           MOVE WA-UG TO P-UG.
           MOVE WA-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF  ZERO NOT = WA-KU AND WA-KA
               MOVE WA-KU TO P-KU
               MOVE WA-KA TO P-KA
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-80.
           EXIT.
