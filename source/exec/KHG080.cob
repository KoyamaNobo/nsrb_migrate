       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHG080.
      *********************************************************
      *    PROGRAM         :  用途区分別販売実績表            *
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
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　工品　用途区分別　販売実績表　".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(010) VALUE
                "用　途　区　分　名　".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上数量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-YCN          PIC  N(016).
           02  F              PIC  X(058).
       01  W-P2.
           02  P-M            PIC  N(016).
           02  P-MD    REDEFINES P-M.
             03  F            PIC  N(012).
             03  P-NM         PIC  N(004).
           02  P-SS           PIC ---,---,--9.99.
           02  P-SK           PIC ----,---,--9.
           02  P-UG           PIC ----,---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC -----9.9.
       01  W-D.
           02  W-BCD          PIC  9(001).
           02  W-BC           PIC  9(001).
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-SK           PIC S9(008).
           02  W-AR           PIC S9(008).
           02  W-RR           PIC S9(003)V9(04).
       01  W-NT.
           02  WN-SS          PIC S9(007)V9(02).
           02  WN-SK          PIC S9(009).
           02  WN-UG          PIC S9(009).
           02  WN-AR          PIC S9(009).
       01  W-ST.
           02  WS-SS          PIC S9(007)V9(02).
           02  WS-SK          PIC S9(009).
           02  WS-UG          PIC S9(009).
           02  WS-AR          PIC S9(009).
       01  W-TT.
           02  WT-SK          PIC S9(009).
           02  WT-UG          PIC S9(009).
           02  WT-AR          PIC S9(009).
       01  W-GT.
           02  W-GTD   OCCURS   2.
             03  WG-SK        PIC S9(009).
             03  WG-UG        PIC S9(009).
             03  WG-AR        PIC S9(009).
       01  W-AT.
           02  WA-SK          PIC S9(009).
           02  WA-UG          PIC S9(009).
           02  WA-AR          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKKBM.
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　工品用途区分別販売実績表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(030) VALUE
                  "***  DATA ﾅｼ  ***             ".
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
            "C-MID" " " "0" "0" "308" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "45" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "45" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "30" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "5" "E-ME1" " "  RETURNING RESU.
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
           MOVE DATE-03R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
           CALL "PR_Open" RETURNING RESP.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-10
           END-IF
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AT W-GT.
           IF  KHT-YC < 20
               MOVE 0 TO W-BCD
           ELSE
               MOVE 1 TO W-BCD
           END-IF.
       M-15.
           MOVE W-BCD TO W-BC.
           MOVE ZERO TO W-TT.
       M-20.
           MOVE ZERO TO W-ST W-C.
           MOVE KHT-YC TO W-YC.
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
           MOVE ZERO TO W-NT.
           MOVE KHT-NC TO W-NC.
       M-30.
           COMPUTE W-SK = KHT-UKIN - KHT-NKIN.
           MOVE ZERO TO W-AR.
           COMPUTE W-AR = W-SK - KHT-GKIN.
           ADD KHT-SSU TO WN-SS.
           ADD W-SK TO WN-SK.
           ADD KHT-GKIN TO WN-UG.
           ADD W-AR TO WN-AR.
       M-35.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF
           IF  KHT-YC < 20
               MOVE 0 TO W-BCD
           ELSE
               MOVE 1 TO W-BCD
           END-IF
           IF  W-BCD NOT = W-BC
               GO TO M-45
           END-IF
           IF  KHT-YC NOT = W-YC
               GO TO M-40
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-30
           END-IF
           PERFORM S-30 THRU S-40.
           GO TO M-25.
       M-40.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-60.
           GO TO M-20.
       M-45.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-60.
           PERFORM S-65 THRU S-75.
           GO TO M-15.
       M-75.
           PERFORM S-30 THRU S-40.
           PERFORM S-45 THRU S-60.
           PERFORM S-65 THRU S-75.
           MOVE ZERO TO W-RR.
           IF  WA-AR = ZERO
               GO TO M-80
           END-IF
           IF  WA-SK = ZERO
               IF  WA-AR < ZERO
                   MOVE -100 TO W-RR
                   GO TO M-80
               END-IF
           END-IF
           IF  WA-SK = ZERO
               IF  WA-AR > ZERO
                   MOVE 100 TO W-RR
                   GO TO M-80
               END-IF
           END-IF
           COMPUTE W-RR ROUNDED = (WA-AR / WA-SK) * 100.
           IF ((WA-AR < ZERO) AND (W-RR > ZERO)) OR
              ((WA-AR > ZERO) AND (W-RR < ZERO))
                   COMPUTE W-RR = W-RR * -1
           END-IF.
       M-80.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           MOVE "　　　【　　総　合　計　　】　　" TO P-M.
           MOVE WA-SK TO P-SK
           MOVE WA-UG TO P-UG
           MOVE WA-AR TO P-AR
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           MOVE ZERO TO W-C.
       M-85.
           ADD 1 TO W-C.
           IF  W-C > 2
               GO TO M-95
           END-IF
           MOVE ZERO TO W-RR.
           IF  WG-AR(W-C) = ZERO
               GO TO M-90
           END-IF
           IF  WG-SK(W-C) = ZERO
               IF  WG-AR(W-C) < ZERO
                   MOVE -100 TO W-RR
                   GO TO M-90
               END-IF
           END-IF
           IF  WG-SK(W-C) = ZERO
               IF  WG-AR(W-C) > ZERO
                   MOVE 100 TO W-RR
                   GO TO M-90
               END-IF
           END-IF
           COMPUTE W-RR ROUNDED = (WG-AR(W-C) / WG-SK(W-C)) * 100.
           IF ((WG-AR(W-C) < ZERO) AND (W-RR > ZERO)) OR
              ((WG-AR(W-C) > ZERO) AND (W-RR < ZERO))
                   COMPUTE W-RR = W-RR * -1
           END-IF.
       M-90.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           IF  W-C = 1
               MOVE "内　作　" TO P-NM
           END-IF
           IF  W-C = 2
               MOVE "仕　入　" TO P-NM
           END-IF
           MOVE WG-SK(W-C) TO P-SK.
           MOVE WG-UG(W-C) TO P-UG.
           MOVE WG-AR(W-C) TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-85.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
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
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-YCN.
           MOVE KKB-YCN TO P-YCN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = WN-SS AND WN-SK AND WN-UG AND WN-AR
               GO TO S-40
           END-IF
           ADD 1 TO W-C.
           IF  W-C = 1
               PERFORM S-20 THRU S-25
           END-IF
           MOVE ZERO TO W-RR.
           IF  WN-AR = ZERO
               GO TO S-35
           END-IF
           IF  WN-SK = ZERO
               IF  WN-AR < ZERO
                   MOVE -100 TO W-RR
                   GO TO S-35
               END-IF
           END-IF
           IF  WN-SK = ZERO
               IF  WN-AR > ZERO
                   MOVE 100 TO W-RR
                   GO TO S-35
               END-IF
           END-IF
           COMPUTE W-RR ROUNDED = (WN-AR / WN-SK) * 100.
           IF ((WN-AR < ZERO) AND (W-RR > ZERO)) OR
              ((WN-AR > ZERO) AND (W-RR < ZERO))
                   COMPUTE W-RR = W-RR * -1
           END-IF.
       S-35.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           IF  W-NC = 0
               MOVE "内　作　" TO P-NM
           END-IF
           IF  W-NC = 1
               MOVE "仕　入　" TO P-NM
           END-IF
           MOVE WN-SS TO P-SS.
           MOVE WN-SK TO P-SK.
           MOVE WN-UG TO P-UG.
           MOVE WN-AR TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WN-SS TO WS-SS.
           ADD WN-SK TO WS-SK.
           ADD WN-UG TO WS-UG.
           ADD WN-AR TO WS-AR.
           IF  W-NC = 0
               ADD WN-SK TO WG-SK(1)
               ADD WN-UG TO WG-UG(1)
               ADD WN-AR TO WG-AR(1)
           END-IF
           IF  W-NC = 1
               ADD WN-SK TO WG-SK(2)
               ADD WN-UG TO WG-UG(2)
               ADD WN-AR TO WG-AR(2)
           END-IF.
       S-40.
           EXIT.
       S-45.
           IF  W-C < 2
               GO TO S-55
           END-IF
           MOVE ZERO TO W-RR.
           IF  WS-AR = ZERO
               GO TO S-50
           END-IF
           IF  WS-SK = ZERO
               IF  WS-AR < ZERO
                   MOVE -100 TO W-RR
                   GO TO S-50
               END-IF
           END-IF
           IF  WS-SK = ZERO
               IF  WS-AR > ZERO
                   MOVE 100 TO W-RR
                   GO TO S-50
               END-IF
           END-IF
           COMPUTE W-RR ROUNDED = (WS-AR / WS-SK) * 100.
           IF ((WS-AR < ZERO) AND (W-RR > ZERO)) OR
              ((WS-AR > ZERO) AND (W-RR < ZERO))
                   COMPUTE W-RR = W-RR * -1
           END-IF.
       S-50.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           MOVE "　　　　　　　　（　小　計　）　" TO P-M.
           MOVE WS-SS TO P-SS.
           MOVE WS-SK TO P-SK.
           MOVE WS-UG TO P-UG.
           MOVE WS-AR TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-55.
           ADD WS-SK TO WT-SK.
           ADD WS-UG TO WT-UG.
           ADD WS-AR TO WT-AR.
       S-60.
           EXIT.
       S-65.
           MOVE ZERO TO W-RR.
           IF  WT-AR = ZERO
               GO TO S-70
           END-IF
           IF  WT-SK = ZERO
               IF  WT-AR < ZERO
                   MOVE -100 TO W-RR
                   GO TO S-70
               END-IF
           END-IF
           IF  WT-SK = ZERO
               IF  WT-AR > ZERO
                   MOVE 100 TO W-RR
                   GO TO S-70
               END-IF
           END-IF
           COMPUTE W-RR ROUNDED = (WT-AR / WT-SK) * 100.
           IF ((WT-AR < ZERO) AND (W-RR > ZERO)) OR
              ((WT-AR > ZERO) AND (W-RR < ZERO))
                   COMPUTE W-RR = W-RR * -1
           END-IF.
       S-70.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           MOVE "　　　　　［　合　計　］　　　　" TO P-M.
           MOVE WT-SK TO P-SK
           MOVE WT-UG TO P-UG
           MOVE WT-AR TO P-AR
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-SK TO WA-SK.
           ADD WT-UG TO WA-UG.
           ADD WT-AR TO WA-AR.
      *
       S-75.
           EXIT.
