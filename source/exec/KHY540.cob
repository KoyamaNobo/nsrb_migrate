       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY540.
      *********************************************************
      *    PROGRAM         :  年間用途区分別販売実績表　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　＊＊＊　　".
           02  P-SNEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  P-SGET         PIC Z9.
           02  F              PIC  X(003) VALUE " - ".
           02  P-ENEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  P-EGET         PIC Z9.
           02  F              PIC  N(022) VALUE
                "　年間　工品用途区分別　販売実績表　　＊＊＊".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "用　　途　　区　　分".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上数量".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-YC           PIC  9(002).
           02  F              PIC  X(001).
           02  P-YCN          PIC  N(016).
           02  F              PIC  X(065).
       01  W-P2.
           02  F              PIC  X(003).
           02  P-M            PIC  N(016).
           02  P-SS           PIC ----,---,--9.99.
           02  P-SK           PIC --,---,---,--9.
           02  P-UG           PIC --,---,---,--9.
           02  P-AR           PIC --,---,---,--9.
           02  P-RR           PIC -----9.9.
           02  P-20K          PIC  X(005).
       01  W-D.
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-RR           PIC S9(003)V9(02).
       01  W-ST.
           02  WS-SS          PIC S9(008)V9(02).
           02  WS-SK          PIC S9(010).
           02  WS-UG          PIC S9(010).
           02  WS-AR          PIC S9(010).
       01  W-TT.
           02  WT-SS          PIC S9(008)V9(02).
           02  WT-SK          PIC S9(010).
           02  WT-UG          PIC S9(010).
           02  WT-AR          PIC S9(010).
       01  W-GT.
           02  W-GTD   OCCURS   2.
             03  WG-SK        PIC S9(010).
             03  WG-UG        PIC S9(010).
             03  WG-AR        PIC S9(010).
       01  W-AT.
           02  WA-SK          PIC S9(010).
           02  WA-UG          PIC S9(010).
           02  WA-AR          PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LIBFDD.
           COPY LIKKBM.
           COPY LSPF.
       01  KH-YF_KHY540.
           02  KH-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  KH-YF_LNAME    PIC  X(012)  VALUE "KH-YF_KHY540".
           02  F              PIC  X(001).
           02  KH-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  KH-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  KH-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  KH-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  KH-YF_RES      USAGE  POINTER.
       01  KHY-R.
           02  Y-HCD          PIC  X(005).
           02  Y-YC           PIC  9(002).
           02  Y-NC           PIC  9(001).
           02  Y-ZS           PIC S9(006)V9(02).
           02  Y-ZK           PIC S9(008).
           02  Y-NS           PIC S9(006)V9(02).
           02  Y-NK           PIC S9(008).
           02  Y-SS           PIC S9(006)V9(02).
           02  Y-SK           PIC S9(008).
           02  Y-YS           PIC S9(006)V9(02).
           02  Y-YK           PIC S9(008).
           02  Y-UG           PIC S9(008).
           02  Y-NG           PIC  9(006).
           02  Y-SNG.
             03  F            PIC  9(002).
             03  Y-SNEN       PIC  9(002).
             03  Y-SGET       PIC  9(002).
           02  Y-ENG.
             03  F            PIC  9(002).
             03  Y-ENEN       PIC  9(002).
             03  Y-EGET       PIC  9(002).
           02  F              PIC  X(030).
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
                "＊＊＊　　年間工品用途区分別販売実績表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
      *
       M-05.
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO KH-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KH-YF_PNAME1 " " BY REFERENCE KH-YF_IDLST "0".
       M-15.
      *           READ KH-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KH-YF_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KH-YF_IDLST KH-YF_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  Y-YC = ZERO
               GO TO M-15
           END-IF.
           IF  ZERO = Y-SS AND Y-SK AND Y-UG
               GO TO M-15
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE Y-SNEN TO P-SNEN.
           MOVE Y-SGET TO P-SGET.
           MOVE Y-ENEN TO P-ENEN.
           MOVE Y-EGET TO P-EGET.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AT W-GT.
       M-20.
           MOVE ZERO TO W-TT CHK.
           MOVE Y-YC TO W-YC.
           MOVE SPACE TO KKB-KEY.
           MOVE 01 TO KKB-NO.
           MOVE W-YC TO KKB-YC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KKB-YCN
           END-IF.
       M-25.
           MOVE ZERO TO W-ST.
           MOVE Y-NC TO W-NC.
       M-30.
           ADD Y-SS TO WS-SS.
           ADD Y-SK TO WS-SK.
           ADD Y-UG TO WS-UG.
       M-35.
      *           READ KH-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KH-YF_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  ZERO = Y-SS AND Y-SK AND Y-UG
               GO TO M-35
           END-IF.
           IF  Y-YC NOT = W-YC
               GO TO M-40
           END-IF.
           IF  Y-NC = W-NC
               GO TO M-30
           END-IF.
           PERFORM S-30 THRU S-40.
           GO TO M-25.
       M-40.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-60.
           GO TO M-20.
       M-90.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-60.
           PERFORM S-65 THRU S-85.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KH-YF_IDLST KH-YF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
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
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-YCN.
           MOVE W-YC TO P-YC.
           MOVE KKB-YCN TO P-YCN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-10 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = WS-SS AND WS-SK AND WS-UG
               GO TO S-40
           END-IF.
           ADD 1 TO CHK.
           IF  CHK = 1
               PERFORM S-20 THRU S-25
           END-IF.
           COMPUTE WS-AR = WS-SK - WS-UG.
           MOVE ZERO TO W-RR.
           IF  WS-SK NOT = ZERO
               IF  WS-AR NOT = ZERO
                   COMPUTE W-RR = (WS-AR / WS-SK) * 100
                   GO TO S-35
               END-IF
           END-IF.
           IF  WS-SK = ZERO
               IF  WS-AR > ZERO
                   MOVE 100 TO W-RR
               ELSE
                   IF  WS-AR < ZERO
                       MOVE -100 TO W-RR
                   END-IF
               END-IF
           END-IF.
       S-35.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           IF  W-NC = 0
               MOVE "　　　　　　　　　　　　内　作　" TO P-M
           END-IF.
           IF  W-NC = 1
               MOVE "　　　　　　　　　　　　仕　入　" TO P-M
           END-IF.
           MOVE WS-SS TO P-SS.
           MOVE WS-SK TO P-SK.
           MOVE WS-UG TO P-UG.
           MOVE WS-AR TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-SS TO WT-SS.
           ADD WS-SK TO WT-SK.
           ADD WS-UG TO WT-UG.
           ADD WS-AR TO WT-AR.
           IF  W-NC = 0
               ADD WS-SK TO WG-SK(1)
               ADD WS-UG TO WG-UG(1)
               ADD WS-AR TO WG-AR(1)
           END-IF.
           IF  W-NC = 1
               ADD WS-SK TO WG-SK(2)
               ADD WS-UG TO WG-UG(2)
               ADD WS-AR TO WG-AR(2)
           END-IF.
       S-40.
           EXIT.
       S-45.
           IF  CHK < 2
               GO TO S-55
           END-IF.
           MOVE ZERO TO W-RR.
           IF  WT-SK NOT = ZERO
               IF  WT-AR NOT = ZERO
                   COMPUTE W-RR = (WT-AR / WT-SK) * 100
                   GO TO S-50
               END-IF
           END-IF.
           IF  WT-SK = ZERO
               IF  WT-AR > ZERO
                   MOVE 100 TO W-RR
               ELSE
                   IF  WT-AR < ZERO
                       MOVE -100 TO W-RR
                   END-IF
               END-IF
           END-IF.
       S-50.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           MOVE "　　　　　　　　（　小　計　）　" TO P-M.
           MOVE WT-SS TO P-SS.
           MOVE WT-SK TO P-SK.
           MOVE WT-UG TO P-UG.
           MOVE WT-AR TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-55.
           ADD WT-SK TO WA-SK.
           ADD WT-UG TO WA-UG.
           ADD WT-AR TO WA-AR.
       S-60.	
           EXIT.
       S-65.
           MOVE ZERO TO W-RR.
           IF  WA-SK NOT = ZERO
               IF  WA-AR NOT = ZERO
                   COMPUTE W-RR = (WA-AR / WA-SK) * 100
                   GO TO S-70
               END-IF
           END-IF.
           IF  WA-SK = ZERO
               IF  WA-AR > ZERO
                   MOVE 100 TO W-RR
               ELSE
                   IF  WA-AR < ZERO
                       MOVE -100 TO W-RR
                   END-IF
               END-IF
           END-IF.
       S-70.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           MOVE "　　　　【　総　合　計　】　　　" TO P-M.
           MOVE WA-SK TO P-SK.
           MOVE WA-UG TO P-UG.
           MOVE WA-AR TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 0 TO CHK.
       S-75.
           ADD 1 TO CHK.
           IF  CHK > 2
               GO TO S-85
           END-IF.
           MOVE ZERO TO W-RR.
           IF  WG-SK(CHK) NOT = ZERO
               IF  WG-AR(CHK) NOT = ZERO
                   COMPUTE W-RR = (WG-AR(CHK) / WG-SK(CHK)) * 100
                   GO TO S-80
               END-IF
           END-IF.
           IF  WG-SK(CHK) = ZERO
               IF  WG-AR(CHK) > ZERO
                   MOVE 100 TO W-RR
               ELSE
                   IF  WG-AR(CHK) < ZERO
                       MOVE -100 TO W-RR
                   END-IF
               END-IF
           END-IF.
       S-80.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           IF  CHK = 1
               MOVE "　　　　　　　　　　　　内　作　" TO P-M
           END-IF.
           IF  CHK = 2
               MOVE "　　　　　　　　　　　　仕　入　" TO P-M
           END-IF.
           MOVE WG-SK(CHK) TO P-SK.
           MOVE WG-UG(CHK) TO P-UG.
           MOVE WG-AR(CHK) TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO S-75.
       S-85.
           EXIT.
