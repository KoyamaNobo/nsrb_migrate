       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHY030.
      ******************************************
      *****     工品品種別　棚卸差額表     *****
      *****     JS-SIGN  :  0=棚卸 , 1=廃棄*****
      ******************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  H-CHK          PIC  N(001) VALUE SPACE.
           02  F              PIC  X(027) VALUE SPACE.
           02  H-MID          PIC  N(021) VALUE SPACE.
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(004) VALUE "品　　名".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(003) VALUE "単　価".
           02  F              PIC  X(003) VALUE SPACE.
           02  H-MID1         PIC  N(011) VALUE SPACE.
           02  F              PIC  X(003) VALUE SPACE.
           02  H-MID2         PIC  N(011) VALUE SPACE.
           02  F              PIC  X(003) VALUE SPACE.
           02  H-MID3         PIC  N(011) VALUE SPACE.
       01  W-P.
           02  P-HCD          PIC  9(005).
           02  F              PIC  X(002).
           02  P-NAME         PIC  X(020).
           02  P-T            PIC ZZZZZ,ZZZ.ZZ.
           02  P-TSU          PIC --,---,---.--.
           02  P-TKIN         PIC ----,---,---.
           02  P-ZSU          PIC --,---,---.--.
           02  P-ZKIN         PIC ----,---,---.
           02  P-SSU          PIC --,---,---.--.
           02  P-SKIN         PIC ----,---,---.
       01  WS-D.
           02  WS-ZSU         PIC S9(007)V9(02).
           02  WS-ZKIN        PIC S9(009).
           02  WS-TSU         PIC S9(007)V9(02).
           02  WS-TKIN        PIC S9(009).
           02  WS-SSU         PIC S9(007)V9(02).
           02  WS-SKIN        PIC S9(009).
       01  WT-D.
           02  WT-ZSU         PIC S9(007)V9(02).
           02  WT-ZKIN        PIC S9(009).
           02  WT-TSU         PIC S9(007)V9(02).
           02  WT-TKIN        PIC S9(009).
           02  WT-SSU         PIC S9(007)V9(02).
           02  WT-SKIN        PIC S9(009).
       01  WA-D.
           02  WA-ZKIN        PIC S9(009).
           02  WA-TKIN        PIC S9(009).
           02  WA-SKIN        PIC S9(009).
       01  W-DATA.
           02  W-TKIN         PIC S9(008).
           02  W-ZKIN         PIC S9(008).
           02  W-SSU          PIC S9(007)V9(02).
           02  W-SKIN         PIC S9(008).
           02  W-TTS          PIC S9(006)V9(02).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-DMM          PIC  9(001).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DC    PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID1.
             03  FILLER  PIC  N(013) VALUE
                  "工品　製品棚卸差額　明細表".
             03  FILLER  PIC  X(028) VALUE
                  "月次繰越  前=1  後=5    ﾘﾀｰﾝ".
           02  D-MID2  PIC  N(013) VALUE
                "　工品仕掛品　廃棄明細表　".
       01  C-ERR.
           02  FILLER.
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
            "C-MID" " " "0" "0" "344" " " " "  RETURNING RESU.
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
            "08C-MID" "X" "20" "22" "22" "07C-MID" " " RETURNING RESU.
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
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "80" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID1" " " "0" "0" "54" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID1" "N" "6" "20" "26" " " "D-MID1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID1" "X" "14" "19" "28" "01D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID2" "N" "6" "20" "26" "D-MID1" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "10" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
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
           MOVE ZERO TO W-DATA.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU
               MOVE "＊＊＊　　工品仕掛品　棚卸差額表　　＊＊＊"
                                                              TO H-MID
               MOVE "　　棚卸数　　棚卸金額" TO H-MID1
               MOVE "　　帳簿数　　帳簿金額" TO H-MID2
               MOVE "　　差額数　　差額金額" TO H-MID3
           ELSE
               CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU
               MOVE "＊＊＊　　工品仕掛品　廃棄明細表　　＊＊＊"
                                                              TO H-MID
               MOVE "　　在庫数　　在庫金額" TO H-MID1
               MOVE "　　廃棄数　　廃棄金額" TO H-MID2
               MOVE "　　　残数　　　残金額" TO H-MID3
               GO TO M-15
           END-IF.
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
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT = BTB
               IF  JS-SIGN = 0
                   GO TO M-10
               END-IF
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
           IF  KHT-NC = 9
               GO TO M-20
           END-IF
           IF  JS-SIGN = 0
               IF  W-DC = 1
                   COMPUTE W-TTS =
                       KHT-ZSU + KHT-KSU - KHT-HSU + KHT-ISU - KHT-SSU
               ELSE
                   MOVE KHT-TTS TO W-TTS
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE KHT-HKS TO W-TTS
           END-IF
           IF  JS-SIGN = 0
               IF  ZERO = W-TTS AND KHT-JTS
                   GO TO M-20
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  ZERO = W-TTS AND KHT-ZSU
                   GO TO M-20
               END-IF
           END-IF
           IF  JS-SIGN = 0
               IF  W-DC = 1
                   MOVE "Ｃ" TO H-CHK
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WA-D.
           MOVE DATE-03R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-25.
           MOVE KHT-YC TO W-YC.
           MOVE ZERO TO WT-D.
       M-30.
           MOVE KHT-NC TO W-NC.
           MOVE ZERO TO WS-D W-C.
       M-35.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "    **  KHM ﾅｼ  **  " TO KH-NAME
               MOVE ZERO TO KH-GT1
           END-IF
           IF  JS-SIGN = 1
               MOVE KHT-ZKIN TO W-TKIN
           ELSE
               COMPUTE W-TKIN = KHT-JTS * KH-GT1
           END-IF
           COMPUTE W-ZKIN = W-TTS * KH-GT1.
           IF  JS-SIGN = 1
               COMPUTE W-SSU = KHT-ZSU - W-TTS
           ELSE
               COMPUTE W-SSU = KHT-JTS - W-TTS
           END-IF
           COMPUTE W-SKIN = W-TKIN - W-ZKIN.
           MOVE SPACE TO W-P.
           MOVE KHT-KEY TO P-HCD.
           MOVE KH-NAME TO P-NAME.
           MOVE KH-GT1 TO P-T.
           MOVE W-TTS TO P-ZSU.
           MOVE W-ZKIN TO P-ZKIN.
           IF  JS-SIGN = 1
               MOVE KHT-ZSU TO P-TSU
           ELSE
               MOVE KHT-JTS TO P-TSU
           END-IF
           MOVE W-TKIN TO P-TKIN.
           MOVE W-SSU TO P-SSU.
           MOVE W-SKIN TO P-SKIN.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-C = 1
               MOVE 2 TO W-C
           END-IF
           IF  W-C = 0
               MOVE 1 TO W-C
           END-IF
      *
           ADD W-TTS TO WS-ZSU.
           ADD W-ZKIN TO WS-ZKIN.
           IF  JS-SIGN = 1
               ADD KHT-ZSU TO WS-TSU
           ELSE
               ADD KHT-JTS TO WS-TSU
           END-IF
           ADD W-TKIN TO WS-TKIN.
           ADD W-SSU TO WS-SSU.
           ADD W-SKIN TO WS-SKIN.
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
           IF  KHT-NC = 9
               GO TO M-40
           END-IF
           IF  W-DC = 1
               COMPUTE W-TTS =
                       KHT-ZSU + KHT-KSU - KHT-HSU + KHT-ISU - KHT-SSU
           ELSE
               MOVE KHT-TTS TO W-TTS
           END-IF
           IF  JS-SIGN = 1
               MOVE KHT-HKS TO W-TTS
           END-IF
           IF  JS-SIGN = 0
               IF  ZERO = W-TTS AND KHT-JTS
                   GO TO M-40
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  ZERO = W-TTS AND KHT-ZSU
                   GO TO M-40
               END-IF
           END-IF
           IF  KHT-YC NOT = W-YC
               GO TO M-45
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-35
           END-IF
           PERFORM S-20 THRU S-30.
           GO TO M-30.
       M-45.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-25.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           PERFORM S-50 THRU S-55.
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
       S-15.
           EXIT.
       S-20.
           IF  W-C < 2
               MOVE SPACE TO SP-R
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE "    　　（　計　）  " TO P-NAME.
           MOVE WS-ZSU TO P-ZSU.
           MOVE WS-ZKIN TO P-ZKIN.
           MOVE WS-TSU TO P-TSU.
           MOVE WS-TKIN TO P-TKIN.
           MOVE WS-SSU TO P-SSU.
           MOVE WS-SKIN TO P-SKIN.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P TO SP-R.
       S-25.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-ZSU TO WT-ZSU.
           ADD WS-ZKIN TO WT-ZKIN.
           ADD WS-TSU TO WT-TSU.
           ADD WS-TKIN TO WT-TKIN.
           ADD WS-SSU TO WT-SSU.
           ADD WS-SKIN TO WT-SKIN.
       S-30.
           EXIT.
       S-35.
           MOVE SPACE TO W-P.
           MOVE "    ［　小　計　］  " TO P-NAME.
           MOVE WT-ZSU TO P-ZSU.
           MOVE WT-ZKIN TO P-ZKIN.
           MOVE WT-TSU TO P-TSU.
           MOVE WT-TKIN TO P-TKIN.
           MOVE WT-SSU TO P-SSU.
           MOVE WT-SKIN TO P-SKIN.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-ZKIN TO WA-ZKIN.
           ADD WT-TKIN TO WA-TKIN.
           ADD WT-SKIN TO WA-SKIN.
       S-40.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE "  【　総　合　計　】" TO P-NAME.
           MOVE WA-ZKIN TO P-ZKIN.
           MOVE WA-TKIN TO P-TKIN.
           MOVE WA-SKIN TO P-SKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-55.
           EXIT.
