       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY060.
      *********************************************************
      *    PROGRAM         :  用途区分別　棚卸差額明細表　　  *
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
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-CHK          PIC  N(001).
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　工品用途区分別　棚卸差額明細表　　＊＊＊".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  N(012) VALUE
                "用　途　区　分　名　　　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(011) VALUE
                "　　棚卸数　　棚卸金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(011) VALUE
                "　　帳簿数　　帳簿金額".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(011) VALUE
                "　　差額数　　差額金額".
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
       01  W-P1.
           02  P-YCN          PIC  N(016).
           02  F              PIC  X(076).
       01  W-P2.
           02  P-M            PIC  N(016).
           02  P-MD    REDEFINES P-M.
             03  F            PIC  N(012).
             03  P-NM         PIC  N(004).
           02  F              PIC  X(001).
           02  P-TSU          PIC --,---,--9.99.
           02  P-TKIN         PIC ----,---,--9.
           02  P-ZSU          PIC --,---,--9.99.
           02  P-ZKIN         PIC ----,---,--9.
           02  P-SSU          PIC --,---,--9.99.
           02  P-SKIN         PIC ----,---,--9.
       01  WN-D.
           02  WN-ZSU         PIC S9(007)V9(02).
           02  WN-ZKIN        PIC S9(009).
           02  WN-TSU         PIC S9(007)V9(02).
           02  WN-TKIN        PIC S9(009).
           02  WN-SSU         PIC S9(007)V9(02).
           02  WN-SKIN        PIC S9(009).
       01  WS-D.
           02  WS-ZSU         PIC S9(007)V9(02).
           02  WS-ZKIN        PIC S9(009).
           02  WS-TSU         PIC S9(007)V9(02).
           02  WS-TKIN        PIC S9(009).
           02  WS-SSU         PIC S9(007)V9(02).
           02  WS-SKIN        PIC S9(009).
       01  WA-D.
           02  WA-ZKIN        PIC S9(009).
           02  WA-TKIN        PIC S9(009).
           02  WA-SKIN        PIC S9(009).
       01  W-DATA.
           02  W-DC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-TKIN         PIC S9(008).
           02  W-ZKIN         PIC S9(008).
           02  W-SSU          PIC S9(007)V9(02).
           02  W-SKIN         PIC S9(008).
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-TTS          PIC S9(006)V9(02).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　工品用途区分別　棚卸差額表　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(028) VALUE
                "月次繰越　前=1  後=5    ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DC    PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
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
            "C-MID" " " "0" "0" "372" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "19" "28" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "22" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DC" "9" "14" "42" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DC" BY REFERENCE W-DC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "A-DC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "31" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR"  RETURNING RESU.
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
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DC "A-DC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DC NOT = 1 AND 5
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
       M-20.
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
               GO TO M-20
           END-IF
           IF  W-DC = 1
               COMPUTE W-TTS =
                       KHT-ZSU + KHT-KSU - KHT-HSU + KHT-ISU - KHT-SSU
           ELSE
               MOVE KHT-TTS TO W-TTS
           END-IF
           IF  ZERO = KHT-JTS AND W-TTS
               GO TO M-20
           END-IF
           MOVE SPACE TO H-CHK.
           IF  W-DC = 1
               MOVE "Ｃ" TO H-CHK
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D.
           MOVE DATE-03R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-25.
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
           END-IF
           MOVE ZERO TO WS-D W-C.
       M-30.
           MOVE KHT-NC TO W-NC.
           MOVE ZERO TO WN-D.
       M-40.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO KH-GT1
           END-IF
           COMPUTE W-TKIN = KHT-JTS * KH-GT1.
           COMPUTE W-ZKIN = W-TTS * KH-GT1.
           COMPUTE W-SSU = KHT-JTS - W-TTS.
           COMPUTE W-SKIN = W-TKIN - W-ZKIN.
      *
           ADD W-TTS TO WN-ZSU.
           ADD W-ZKIN TO WN-ZKIN.
           ADD KHT-JTS TO WN-TSU.
           ADD W-TKIN TO WN-TKIN.
           ADD W-SSU TO WN-SSU.
           ADD W-SKIN TO WN-SKIN.
       M-45.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-45
           END-IF
           IF  W-DC = 1
               COMPUTE W-TTS =
                       KHT-ZSU + KHT-KSU - KHT-HSU + KHT-ISU - KHT-SSU
           ELSE
               MOVE KHT-TTS TO W-TTS
           END-IF
           IF  ZERO = KHT-JTS AND W-TTS
               GO TO M-45
           END-IF
           IF  KHT-YC NOT = W-YC
               GO TO M-50
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-40
           END-IF
           PERFORM S-30 THRU S-35.
           GO TO M-30.
       M-50.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           GO TO M-25.
       M-90.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           PERFORM S-60 THRU S-65.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE KKB-YCN TO P-YCN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           IF  W-C = 0
               PERFORM S-20 THRU S-25
           END-IF
           ADD 1 TO W-C.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-M.
           IF  W-NC = 0
               MOVE "内　作　" TO P-NM
           END-IF
           IF  W-NC = 1
               MOVE "仕　入　" TO P-NM
           END-IF
           MOVE WN-ZSU TO P-ZSU.
           MOVE WN-ZKIN TO P-ZKIN.
           MOVE WN-TSU TO P-TSU.
           MOVE WN-TKIN TO P-TKIN.
           MOVE WN-SSU TO P-SSU.
           MOVE WN-SKIN TO P-SKIN.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WN-ZSU TO WS-ZSU.
           ADD WN-ZKIN TO WS-ZKIN.
           ADD WN-TSU TO WS-TSU.
           ADD WN-TKIN TO WS-TKIN.
           ADD WN-SSU TO WS-SSU.
           ADD WN-SKIN TO WS-SKIN.
       S-35.
           EXIT.
       S-40.
           IF  W-C < 2
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P2.
           MOVE "　　　　　　　　（　小　計　）　" TO P-M.
           MOVE WS-ZSU TO P-ZSU.
           MOVE WS-ZKIN TO P-ZKIN.
           MOVE WS-TSU TO P-TSU.
           MOVE WS-TKIN TO P-TKIN.
           MOVE WS-SSU TO P-SSU.
           MOVE WS-SKIN TO P-SKIN.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-ZKIN TO WA-ZKIN.
           ADD WS-TKIN TO WA-TKIN.
           ADD WS-SKIN TO WA-SKIN.
       S-50.
           EXIT.
       S-60.
           MOVE SPACE TO W-P2.
           MOVE "　　　　［　総　合　計　］　　　" TO P-M.
           MOVE WA-ZKIN TO P-ZKIN.
           MOVE WA-TKIN TO P-TKIN.
           MOVE WA-SKIN TO P-SKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-65.
           EXIT.
