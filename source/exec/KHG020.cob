       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG020.
      *********************************************************
      *    PROGRAM         :  品種別製品受払表(原価）         *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/06                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                "＊＊＊　　工業用品　製品受払表　".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  X(008) VALUE "------  ".
           02  F              PIC  N(004) VALUE "前月繰越".
           02  F              PIC  X(018) VALUE "  ------ -------  ".
           02  F              PIC  N(004) VALUE "当月受入".
           02  F              PIC  X(019) VALUE "  ------- -------  ".
           02  F              PIC  N(004) VALUE "当月売上".
           02  F              PIC  X(018) VALUE "  ------- ------  ".
           02  F              PIC  N(004) VALUE "翌月繰越".
           02  F              PIC  X(008) VALUE "  ------".
       01  HEAD3.
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(004) VALUE "品　　名".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　原　価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  P-YC           PIC  9(002).
           02  F              PIC  X(001).
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(001).
           02  P-NAME         PIC  X(020).
           02  P-T            PIC ZZZZ,ZZ9.99.
           02  P-ZS           PIC -----,---.--.
           02  P-ZK           PIC ---,---,---.
           02  P-KS           PIC --,---,---.--.
           02  P-KK           PIC ----,---,---.
           02  P-US           PIC --,---,---.--.
           02  P-UK           PIC ----,---,---.
           02  P-YS           PIC -----,---.--.
           02  P-YK           PIC ---,---,---.
       01  WS-D.
           02  WS-ZS          PIC S9(007)V9(02).
           02  WS-ZK          PIC S9(008).
           02  WS-KS          PIC S9(007)V9(02).
           02  WS-KK          PIC S9(009).
           02  WS-US          PIC S9(007)V9(02).
           02  WS-UK          PIC S9(009).
           02  WS-YS          PIC S9(007)V9(02).
           02  WS-YK          PIC S9(008).
       01  WT-D.
           02  WT-ZK          PIC S9(008).
           02  WT-KK          PIC S9(009).
           02  WT-UK          PIC S9(009).
           02  WT-YK          PIC S9(008).
       01  WA-D.
           02  WA-ZK          PIC S9(008).
           02  WA-KK          PIC S9(009).
           02  WA-UK          PIC S9(009).
           02  WA-YK          PIC S9(008).
       01  W-DATA.
           02  W-D.
             03  W-KS         PIC S9(007)V9(02).
             03  W-YS         PIC S9(007)V9(02).
             03  W-YK         PIC S9(008).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
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
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　工品製品受払表　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "238" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "34" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "34" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "34" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "34" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "34" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "34" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "34" "06C-MID" " "  RETURNING RESU.
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
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-10
           END-IF
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
           IF  KHT-YC NOT = 10 AND 11
               IF  KH-GT1 = ZERO
                   MOVE ZERO TO KHT-SSU
               END-IF
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-ZKIN AND KHT-KSU AND KHT-HSU
                 AND KHT-ISU AND KHT-KKIN AND KHT-SSU AND KHT-GKIN
               GO TO M-10
           END-IF
           MOVE DATE-03R TO H-DATE.
           PERFORM MID-010 THRU MID-EX.
       M-15.
           MOVE KHT-YC TO W-YC.
           MOVE ZERO TO WT-D CHK.
       M-20.
           MOVE KHT-NC TO W-NC.
           MOVE ZERO TO WS-D CNT.
       M-25.
           COMPUTE W-KS = KHT-KSU - KHT-HSU + KHT-ISU.
           COMPUTE W-YS = KHT-ZSU - KHT-SSU + W-KS.
           COMPUTE W-YK = W-YS * KH-GT1.
      *
           MOVE SPACE TO W-P.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-YC TO P-YC
           END-IF
           MOVE KHT-KEY TO P-HCD.
           MOVE KH-NAME TO P-NAME.
           MOVE KH-GT1 TO P-T.
           MOVE KHT-ZSU TO P-ZS.
           MOVE KHT-ZKIN TO P-ZK.
           MOVE W-KS TO P-KS.
           MOVE KHT-KKIN TO P-KK.
           MOVE KHT-SSU TO P-US.
           MOVE KHT-GKIN TO P-UK.
           MOVE W-YS TO P-YS.
           MOVE W-YK TO P-YK.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-YC TO P-YC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD KHT-ZSU TO WS-ZS.
           ADD KHT-ZKIN TO WS-ZK.
           ADD W-KS TO WS-KS.
           ADD KHT-KKIN TO WS-KK.
           ADD KHT-SSU TO WS-US.
           ADD KHT-GKIN TO WS-UK.
           ADD W-YS TO WS-YS.
           ADD W-YK TO WS-YK.
           IF  CNT = 1
               MOVE 2 TO CNT
           END-IF
           IF  CNT = 0
               MOVE 1 TO CNT
           END-IF.
       M-40.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-40
           END-IF
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
           IF  KHT-YC NOT = 10 AND 11
               IF  KH-GT1 = ZERO
                   MOVE ZERO TO KHT-SSU
               END-IF
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-ZKIN AND KHT-KSU AND KHT-HSU
                     AND KHT-ISU AND KHT-KKIN AND KHT-SSU AND KHT-GKIN
               GO TO M-40
           END-IF
           IF  KHT-YC NOT = W-YC
               GO TO M-45
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-25
           END-IF
           PERFORM TPR-RTN THRU TPR-EX.
           GO TO M-20.
       M-45.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           GO TO M-15.
       M-90.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           PERFORM APR-RTN THRU APR-EX.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
       MID-EX.
           EXIT.
       TPR-RTN.
           IF  CNT NOT = 2
               MOVE SPACE TO SP-R
               GO TO TPR-010
           END-IF
           MOVE SPACE TO W-P.
           MOVE "        （ 小 計 ） " TO P-NAME.
           MOVE WS-ZS TO P-ZS.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-KS TO P-KS.
           MOVE WS-KK TO P-KK.
           MOVE WS-US TO P-US.
           MOVE WS-UK TO P-UK.
           MOVE WS-YS TO P-YS.
           MOVE WS-YK TO P-YK.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-YC TO P-YC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
       TPR-010.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-ZK TO WT-ZK.
           ADD WS-KK TO WT-KK.
           ADD WS-UK TO WT-UK.
           ADD WS-YK TO WT-YK.
       TPR-EX.
           EXIT.
       SPR-RTN.
           MOVE SPACE TO W-P.
           MOVE "    ［　合　計　］  " TO P-NAME.
           MOVE WT-ZK TO P-ZK.
           MOVE WT-KK TO P-KK.
           MOVE WT-UK TO P-UK.
           MOVE WT-YK TO P-YK.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-YC TO P-YC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-ZK TO WA-ZK.
           ADD WT-KK TO WA-KK.
           ADD WT-UK TO WA-UK.
           ADD WT-YK TO WA-YK.
       SPR-EX.
           EXIT.
       APR-RTN.
           MOVE SPACE TO W-P.
           MOVE " 【　総　合　計　】 " TO P-NAME.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-KK TO P-KK.
           MOVE WA-UK TO P-UK.
           MOVE WA-YK TO P-YK.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       APR-EX.
           EXIT.
