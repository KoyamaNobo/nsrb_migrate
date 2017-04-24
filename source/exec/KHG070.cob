       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHG070.
      *********************************************************
      *    PROGRAM         :  防振ゴム　材料費　明細表        *
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
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　防振ゴム　材料費　明細表　　＊＊＊".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(040) VALUE
                " 製番　　売　　価　　加硫数　　 加硫金額".
           02  F              PIC  X(033) VALUE
                "　　重　量 　　 ｺﾞﾑ金額　　金具数".
           02  F              PIC  X(034) VALUE
                "     金具金額　 　材料経費　比率％".
       01  W-P.
           02  P-TM           PIC  X(017).
           02  P-D     REDEFINES P-TM.
             03  P-HCD        PIC  X(005).
             03  P-T          PIC ZZZZZ,ZZ9.99.
           02  P-KS           PIC --,---,--9.
           02  P-SK           PIC -----,---,--9.
           02  P-GG           PIC --,---,--9.
           02  P-GK           PIC -----,---,--9.
           02  P-SU           PIC --,---,--9.
           02  P-KK           PIC -----,---,--9.
           02  P-ZR           PIC -----,---,--9.
           02  P-RT           PIC -----9.9.
       01  WS-D.
           02  WS-KS          PIC S9(007).
           02  WS-SK          PIC S9(009).
           02  WS-GG          PIC S9(007).
           02  WS-GK          PIC S9(009).
           02  WS-SU          PIC S9(007).
           02  WS-KK          PIC S9(009).
           02  WS-ZR          PIC S9(009).
       01  WA-D.
           02  WA-KS          PIC S9(007).
           02  WA-SK          PIC S9(009).
           02  WA-GG          PIC S9(007).
           02  WA-GK          PIC S9(009).
           02  WA-SU          PIC S9(007).
           02  WA-KK          PIC S9(009).
           02  WA-ZR          PIC S9(009).
       01  W-D.
           02  W-YC           PIC  9(002).
           02  W-GG           PIC S9(007).
           02  W-KKIN         PIC S9(008).
           02  W-GK           PIC S9(009).
           02  W-KK           PIC S9(009).
           02  W-SU           PIC S9(007).
           02  W-ZR           PIC S9(009).
           02  W-RT           PIC S9(003)V9(01).
           02  W-SGR          PIC  9(002)V9(04).
           02  W-SSU          PIC  9(002).
           02  W-KSU          PIC  9(002).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHJM.
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
                "＊＊＊　　防振ゴム　材料費　明細表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO WA-D.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KHT-KSU = ZERO
               GO TO M-10
           END-IF
           IF  KHT-YC NOT = 10 AND 11
               GO TO M-10
           END-IF.
       M-15.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHJ-M_PNAME1 BY REFERENCE KHJ-M_IDLST "1"
            "KHJ-KEY" BY REFERENCE KHJ-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-20.
           MOVE ZERO TO WS-D.
           MOVE KHT-YC TO W-YC.
       M-25.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           COMPUTE W-KKIN ROUNDED = KHT-KSU * KH-T1.
           PERFORM S-30 THRU S-40.
           COMPUTE W-GG ROUNDED = KHT-KSU * W-SGR.
           COMPUTE W-GK ROUNDED = KHT-KSU * KH-TGM.
           COMPUTE W-SU = (KHT-KSU * W-KSU) + (KHT-SSU * W-SSU).
           COMPUTE W-KK ROUNDED = KHT-KSU * KH-TKN.
           COMPUTE W-ZR = W-GK + W-KK.
      *
           MOVE SPACE TO W-P.
           MOVE KHT-KEY TO P-HCD.
           MOVE KH-T1 TO P-T.
           MOVE KHT-KSU TO P-KS.
           MOVE W-KKIN TO P-SK.
           MOVE W-GG TO P-GG.
           MOVE W-GK TO P-GK.
           MOVE W-SU TO P-SU.
           MOVE W-KK TO P-KK.
           MOVE W-ZR TO P-ZR.
           IF  KHT-KKIN NOT = ZERO
               COMPUTE W-RT ROUNDED = (W-ZR * 100) / KHT-KKIN
               MOVE W-RT TO P-RT
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD KHT-KSU TO WS-KS.
           ADD W-KKIN TO WS-SK.
           ADD W-GG TO WS-GG.
           ADD W-GK TO WS-GK.
           ADD W-KK TO WS-KK.
           ADD W-SU TO WS-SU.
           ADD W-ZR TO WS-ZR.
       M-30.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-KSU = ZERO
               GO TO M-30
           END-IF
           IF  KHT-YC NOT = 10 AND 11
               GO TO M-30
           END-IF.
       M-35.
           IF  KHT-YC = W-YC
               GO TO M-25
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-20.
       M-90.
           PERFORM S-20 THRU S-25.
           MOVE SPACE TO W-P.
           MOVE "[  ALL TOTAL  ]  " TO P-TM.
           MOVE WA-KS TO P-KS.
           MOVE WA-SK TO P-SK.
           MOVE WA-GG TO P-GG.
           MOVE WA-GK TO P-GK.
           MOVE WA-SU TO P-SU.
           MOVE WA-KK TO P-KK.
           MOVE WA-ZR TO P-ZR.
           IF  WA-SK NOT = ZERO
               COMPUTE W-RT ROUNDED = (WA-ZR * 100) / WA-SK
               MOVE W-RT TO P-RT
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHJ-M_IDLST KHJ-M_PNAME1.
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
           MOVE SPACE TO W-P.
           MOVE "  <  SUB TOTAL  >" TO P-TM.
           MOVE WS-KS TO P-KS.
           MOVE WS-SK TO P-SK.
           MOVE WS-GG TO P-GG.
           MOVE WS-GK TO P-GK.
           MOVE WS-SU TO P-SU.
           MOVE WS-KK TO P-KK.
           MOVE WS-ZR TO P-ZR.
           IF  WS-SK NOT = ZERO
               COMPUTE W-RT ROUNDED = (WS-ZR * 100) / WS-SK
               MOVE W-RT TO P-RT
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-KS TO WA-KS.
           ADD WS-SK TO WA-SK.
           ADD WS-GG TO WA-GG.
           ADD WS-GK TO WA-GK.
           ADD WS-KK TO WA-KK.
           ADD WS-SU TO WA-SU.
           ADD WS-ZR TO WA-ZR.
       S-25.
           EXIT.
       S-30.
           MOVE ZERO TO W-SGR W-KSU W-SSU.
           MOVE SPACE TO KHJ-KEY.
           MOVE KH-KEY TO KHJ-HCD.
      *           START KHJ-M KEY NOT < KHJ-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            KHJ-M_PNAME1 "KHJ-KEY" " NOT < " KHJ-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF.
       S-35.
      *           READ KHJ-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHJ-M_PNAME1 BY REFERENCE KHJ-R
            RETURNING RET.
           IF  RET = 1
               GO TO S-40
           END-IF
           IF  KH-KEY NOT = KHJ-HCD
               GO TO S-40
           END-IF
           IF  KHJ-KSC = 0
               ADD KHJ-SGRD TO W-SGR
           END-IF
           IF  KHJ-KSC = 1
               ADD KHJ-SU TO W-KSU
           END-IF
           IF  KHJ-KSC = 2
               ADD KHJ-SU TO W-SSU
           END-IF
           GO TO S-35.
       S-40.
           EXIT.
