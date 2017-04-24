       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG450.
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-17.
      *********************************************************
      *    PROGRAM         :  担当得意先別　売上粗利集計表    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *        変更　　　  :  62/05/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=全体 , 2=工品他               *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　担当者得意先別　売上粗利集計表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(003) VALUE "担当者".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "利益率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
       01  W-P.
           02  F              PIC  X(003).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNAME        PIC  N(026).
           02  P-U            PIC ------,---,--9.
           02  P-UG           PIC ------,---,--9.
           02  P-AR           PIC -----,---,--9.
           02  P-RR           PIC -----9.9.
           02  P-SHZ          PIC ---,---,--9.
       01  W-D.
           02  W-TC           PIC  9(002).
           02  W-U            PIC S9(009).
           02  W-WU           PIC S9(009).
           02  W-UG           PIC S9(009).
           02  W-AR           PIC S9(009).
           02  W-RR           PIC S9(003)V9(01).
           02  W-SHZ          PIC S9(008).
       01  WS-D.
           02  WS-U           PIC S9(009).
           02  WS-UG          PIC S9(009).
           02  WS-AR          PIC S9(009).
           02  WS-SHZ         PIC S9(008).
       01  WA-D.
           02  WA-U           PIC S9(009).
           02  WA-UG          PIC S9(009).
           02  WA-AR          PIC S9(009).
           02  WA-SHZ         PIC S9(008).
       01  W-ALL              PIC S9(003)V9(04).
       01  W-PAGE             PIC  9(002).
       01  CHK                PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSTTM.
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
                "＊＊＊　　担当者得意先別　売上粗利集計表　　＊＊＊".
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF JS-SIGN NOT = 0 AND 2
               GO TO M-05
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0".
       M-10.
      *           READ TT-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  JS-SIGN = 2
               IF  TT-BC = 0
                   GO TO M-10
               END-IF
           END-IF
           COMPUTE W-U = TT-TUA - TT-TNB.
           COMPUTE W-SHZ = TT-TUAZ - TT-TNBZ.
           IF  ZERO = W-U AND TT-TUG AND W-SHZ
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D W-PAGE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE TT-TNC TO W-TC.
           MOVE ZERO TO WS-D CHK.
       M-20.
           MOVE TT-TUG TO W-UG.
           COMPUTE W-AR = W-U - W-UG.
           MOVE ZERO TO W-RR.
           IF ZERO = W-U OR W-AR
               GO TO M-25
           END-IF
           MOVE W-U TO W-WU.
           IF W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-RR ROUNDED = (W-AR / W-WU) * 100.
       M-25.
           MOVE TT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＴＭ　なし　＊＊" TO T-NAME
           END-IF
           MOVE SPACE TO W-P.
           IF  CHK = ZERO
               MOVE 5 TO CHK
               MOVE TT-TNC TO P-TC
           END-IF
           MOVE TT-TCD TO P-TCD.
           MOVE T-NAME TO P-TNAME.
           MOVE W-U TO P-U.
           MOVE W-UG TO P-UG.
           MOVE W-AR TO P-AR.
           MOVE W-RR TO P-RR.
           MOVE W-SHZ TO P-SHZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-U TO WS-U.
           ADD W-UG TO WS-UG.
           ADD W-AR TO WS-AR.
           ADD W-SHZ TO WS-SHZ.
       M-30.
      *           READ TT-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 2
               IF  TT-BC = 0
                   GO TO M-30
               END-IF
           END-IF
           COMPUTE W-U = TT-TUA - TT-TNB.
           COMPUTE W-SHZ = TT-TUAZ - TT-TNBZ.
           IF  ZERO = W-U AND TT-TUG AND W-SHZ
               GO TO M-30
           END-IF
           IF  W-TC = TT-TNC
               GO TO M-20
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE ZERO TO W-RR.
           IF  ZERO = WS-U OR WS-AR
               GO TO S-25
           END-IF
           MOVE WS-U TO W-WU.
           IF  W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-RR ROUNDED = (WS-AR / W-WU) * 100.
       S-25.
           MOVE SPACE TO W-P.
           MOVE "　　　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-TNAME.
           MOVE WS-U TO P-U.
           MOVE WS-UG TO P-UG.
           MOVE WS-AR TO P-AR.
           MOVE W-RR TO P-RR.
           MOVE WS-SHZ TO P-SHZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-U TO WA-U.
           ADD WS-UG TO WA-UG.
           ADD WS-AR TO WA-AR.
           ADD WS-SHZ TO WA-SHZ.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO W-RR.
           IF  ZERO = WA-U OR WA-AR
               GO TO S-40
           END-IF
           MOVE WA-U TO W-WU.
           IF  W-WU < ZERO
               COMPUTE W-WU = W-WU * -1
           END-IF
           COMPUTE W-RR ROUNDED = (WA-AR / W-WU) * 100.
       S-40.
           MOVE SPACE TO W-P.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】　　" TO P-TNAME.
           MOVE WA-U TO P-U.
           MOVE WA-UG TO P-UG.
           MOVE WA-AR TO P-AR.
           MOVE W-RR TO P-RR.
           MOVE WA-SHZ TO P-SHZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           EXIT.
