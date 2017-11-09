       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG030.
      *********************************************************
      *    PROGRAM         :  工品　販売実績・製品受払表 (参  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIN             PIC  9(001).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(006) VALUE "（参考資料）".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　工品品種別　販売実績・受払表　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(004) VALUE "品　　名".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  X(009) VALUE " I------ ".
           02  F              PIC  N(004) VALUE "前月繰越".
           02  F              PIC  X(017) VALUE " ------I I------ ".
           02  F              PIC  N(004) VALUE "当月入庫".
           02  F              PIC  X(017) VALUE " ------I I------ ".
           02  F              PIC  N(004) VALUE "当月売上".
           02  F              PIC  X(017) VALUE " ------I I------ ".
           02  F              PIC  N(004) VALUE "翌月繰越".
           02  F              PIC  X(009) VALUE " ------I(".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "平均売価".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(005) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(004) VALUE "予定原価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-YC           PIC  9(002).
           02  F              PIC  X(001).
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(001).
           02  P-NAME         PIC  X(020).
           02  F              PIC  X(089).
           02  P-F            PIC  X(001).
           02  P-BT           PIC ----,---.--.
           02  P-R            PIC  X(001).
           02  F              PIC  X(005).
       01  W-P2.
           02  P-TM           PIC  N(010).
           02  P-GT           PIC ----,---.--.
           02  P-ZS           PIC -----,---.--.
           02  P-ZK           PIC ---,---,---.
           02  P-SS           PIC -----,---.--.
           02  P-SK           PIC ---,---,---.
           02  P-US           PIC -----,---.--.
           02  P-UK           PIC ---,---,---.
           02  P-YS           PIC -----,---.--.
           02  P-YK           PIC ---,---,---.
           02  P-AR           PIC ---,---,---.
           02  P-RR           PIC ----9.9.
       01  WS-D.
           02  WS-ZK          PIC S9(008).
           02  WS-SK          PIC S9(008).
           02  WS-UG          PIC S9(008).
           02  WS-UK          PIC S9(008).
           02  WS-YK          PIC S9(008).
           02  WS-AR          PIC S9(008).
       01  WT-D.
           02  WT-ZK          PIC S9(008).
           02  WT-SK          PIC S9(008).
           02  WT-UG          PIC S9(008).
           02  WT-UK          PIC S9(008).
           02  WT-YK          PIC S9(008).
           02  WT-AR          PIC S9(008).
       01  WA-D.
           02  WA-ZK          PIC S9(008).
           02  WA-SK          PIC S9(008).
           02  WA-UG          PIC S9(008).
           02  WA-UK          PIC S9(008).
           02  WA-YK          PIC S9(008).
           02  WA-AR          PIC S9(008).
       01  W-DATA.
           02  W-D.
             03  W-GT         PIC  9(006)V9(02).
             03  W-BT         PIC S9(006)V9(02).
             03  W-SK         PIC S9(008).
             03  W-ZK         PIC S9(008).
             03  W-SS         PIC S9(006)V9(02).
             03  W-UK         PIC S9(008).
             03  W-GK         PIC S9(008).
             03  W-YS         PIC S9(006)V9(02).
             03  W-YK         PIC S9(008).
             03  W-AR         PIC S9(008).
             03  W-RR         PIC S9(003)V9(01).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-YC           PIC  9(002).
           02  W-YCC          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHT2.
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　工品品種別　販売実績・受払表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
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
            "C-MID" " " "0" "0" "336" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "48" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "48" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "48" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "48" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "48" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "48" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "48" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "31" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "35" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-10
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-KSU AND KHT-ISU AND KHT-HSU
                 AND KHT-SSU
                 AND KHT-UKIN AND KHT-NKIN
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D.
           MOVE DATE-03R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE KHT-YC TO W-YC.
           MOVE ZERO TO WT-D W-YCC.
       M-20.
           MOVE KHT-NC TO W-NC.
           MOVE ZERO TO WS-D CHK.
       M-25.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE ZERO TO W-D.
           IF  KH-YGT1 = ZERO
               MOVE KH-GT1 TO W-GT
           ELSE
               MOVE KH-YGT1 TO W-GT
           END-IF
           COMPUTE W-ZK = KHT-ZSU * W-GT.
           COMPUTE W-SS = KHT-KSU - KHT-HSU + KHT-ISU.
           COMPUTE W-SK = W-SS * W-GT.
           COMPUTE W-UK = KHT-UKIN - KHT-NKIN.
           IF  KHT-SSU NOT = ZERO
               COMPUTE W-BT ROUNDED = W-UK / KHT-SSU
           END-IF
           IF  W-GT = ZERO
               MOVE ZERO TO W-YS
           ELSE
               COMPUTE W-YS = KHT-ZSU + W-SS - KHT-SSU
           END-IF
           COMPUTE W-GK = KHT-SSU * W-GT.
           COMPUTE W-YK = W-YS * W-GT.
           COMPUTE W-AR = W-UK - W-GK.
           IF  W-UK = ZERO
               GO TO M-30
           END-IF
           IF  W-AR NOT = ZERO
               IF  W-UK > ZERO
                   COMPUTE W-RR ROUNDED = (W-AR * 100) / W-UK
               ELSE
                   COMPUTE W-RR ROUNDED = (W-AR * -100) / W-UK
               END-IF
           END-IF.
       M-30.
           MOVE SPACE TO W-P1 W-P2.
           MOVE SPACE TO P-TM.
           IF  W-YCC = 0
               MOVE 1 TO W-YCC
               MOVE W-YC TO P-YC
           END-IF
           MOVE KHT-KEY TO P-HCD.
           MOVE KH-NAME TO P-NAME.
           IF  W-BT NOT = ZERO
               MOVE "(" TO P-F
               MOVE ")" TO P-R
               MOVE W-BT TO P-BT
           END-IF
           MOVE W-GT TO P-GT.
           MOVE KHT-ZSU TO P-ZS.
           MOVE W-ZK TO P-ZK.
           MOVE W-SS TO P-SS.
           MOVE W-SK TO P-SK
           MOVE KHT-SSU TO P-US.
           MOVE W-UK TO P-UK.
           MOVE W-YS TO P-YS.
           MOVE W-YK TO P-YK.
           MOVE W-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
      *
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-YC TO P-YC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-ZK TO WS-ZK.
           ADD W-SK TO WS-SK.
           ADD W-GK TO WS-UG.
           ADD W-UK TO WS-UK.
           ADD W-YK TO WS-YK.
           ADD W-AR TO WS-AR.
           IF  CHK = 1
               MOVE 2 TO CHK
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF.
       M-35.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-35
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-KSU AND KHT-ISU AND KHT-HSU
                 AND KHT-SSU
                 AND KHT-UKIN AND KHT-NKIN
               GO TO M-35
           END-IF
           IF  KHT-YC NOT = W-YC
               GO TO M-40
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-35.
           GO TO M-20.
       M-40.
           PERFORM S-20 THRU S-35.
           PERFORM S-45 THRU S-55.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-35.
           PERFORM S-45 THRU S-55.
           PERFORM S-60 THRU S-70.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE HEAD1 TO SP-R.
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
           MOVE ZERO TO W-RR.
           IF  WS-UK = ZERO
               GO TO S-25
           END-IF
           IF  WS-AR NOT = ZERO
               IF  WS-UK > ZERO
                   COMPUTE W-RR ROUNDED = (WS-AR * 100) / WS-UK
               ELSE
                   COMPUTE W-RR ROUNDED = (WS-AR * -100) / WS-UK
               END-IF
           END-IF.
       S-25.
           IF  CHK NOT = 2
               MOVE SPACE TO SP-R
               GO TO S-30
           END-IF
           MOVE SPACE TO W-P2.
           MOVE "　　　（　小　計　）" TO P-TM.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-SK TO P-SK.
           MOVE WS-UK TO P-UK.
           MOVE WS-YK TO P-YK.
           MOVE WS-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
       S-30.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-ZK TO WT-ZK.
           ADD WS-SK TO WT-SK.
           ADD WS-UG TO WT-UG.
           ADD WS-UK TO WT-UK.
           ADD WS-YK TO WT-YK.
           ADD WS-AR TO WT-AR.
       S-35.
           EXIT.
       S-40.
           MOVE ZERO TO W-RR.
           IF  WT-UK = ZERO
               GO TO S-45
           END-IF
           IF  WT-AR NOT = ZERO
               IF  WT-UK > ZERO
                   COMPUTE W-RR ROUNDED = (WT-AR * 100) / WT-UK
               ELSE
                   COMPUTE W-RR ROUNDED = (WT-AR * -100) / WT-UK
               END-IF
           END-IF.
       S-45.
           MOVE SPACE TO W-P2.
           MOVE "　［　合　計　］　　" TO P-TM.
           MOVE WT-ZK TO P-ZK.
           MOVE WT-SK TO P-SK.
           MOVE WT-UK TO P-UK.
           MOVE WT-YK TO P-YK.
           MOVE WT-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-ZK TO WA-ZK.
           ADD WT-SK TO WA-SK.
           ADD WT-UG TO WA-UG.
           ADD WT-UK TO WA-UK.
           ADD WT-YK TO WA-YK.
           ADD WT-AR TO WA-AR.
       S-55.
           EXIT.
       S-60.
           MOVE ZERO TO W-RR.
           IF  WA-UK = ZERO
               GO TO S-65
           END-IF
           IF  WA-AR NOT = ZERO
               IF  WA-UK > ZERO
                   COMPUTE W-RR ROUNDED = (WA-AR * 100) / WA-UK
               ELSE
                   COMPUTE W-RR ROUNDED = (WA-AR * -100) / WA-UK
               END-IF
           END-IF.
       S-65.
           MOVE SPACE TO W-P2.
           MOVE "【　総　合　計　】　" TO P-TM.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-SK TO P-SK.
           MOVE WA-UK TO P-UK.
           MOVE WA-YK TO P-YK.
           MOVE WA-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-70.
           EXIT.
