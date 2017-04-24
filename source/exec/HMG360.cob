       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMG360.
      *********************************************************
      *    PROGRAM         :  担当得意先品名別不良返品明細表　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-IKV          PIC  N(009).
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　履物担当者別　不良返品明細表　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005)  VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007)  VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(003) VALUE "担当　".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(009) VALUE "コード　品　　　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "数　量　単　価　　　金　　額".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(002).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(002).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC -----,--9.
           02  P-T            PIC ----,--9.
           02  P-KIN          PIC --,---,---,--9.
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-D.
             03  W-TCD        PIC  9(004).
             03  W-SU         PIC S9(006).
             03  W-T          PIC S9(005).
             03  W-KIN        PIC S9(008).
           02  W-TNA          PIC  N(026).
           02  CNT            PIC  9(003).
           02  W-IKV          PIC  9(001) VALUE 0.
           02  W-POC          PIC  9(001) VALUE 0.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  WS-D.
           02  W-TC           PIC  9(002).
           02  WS-SU          PIC S9(006).
           02  WS-KIN         PIC S9(008).
       01  WA-D.
           02  WA-SU          PIC S9(006).
           02  WA-KIN         PIC S9(008).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　履物不良返品　明細表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-MID" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           ADD 1 TO W-IKV.
           IF  W-IKV > 3
               GO TO M-95
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-15.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = SNTR-SU OR SNTR-KIN
               GO TO M-15
           END-IF
           IF  W-IKV = 1
               IF (SNTR-BCD1 = 322) OR (SNTR-BC3 = 30)
                   GO TO M-15
               END-IF
           END-IF
           IF  W-IKV = 2
               IF  SNTR-BC3 NOT = 30
                   GO TO M-15
               END-IF
           END-IF
           IF  W-IKV = 3
               IF  SNTR-BCD1 NOT = 322
                   GO TO M-15
               END-IF
           END-IF
           IF  W-IKV = 1
               MOVE "【　一　般　】　　" TO H-IKV
           END-IF
           IF  W-IKV = 2
               MOVE "【　教　育　】　　" TO H-IKV
           END-IF
           IF  W-IKV = 3
               MOVE "【ヴィヴェンディ】" TO H-IKV
           END-IF
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO WA-D.
       M-20.
           MOVE ZERO TO WS-D.
           MOVE SNTR-TNC TO W-TC.
           MOVE SPACE TO SP-R.
           MOVE ALL "　"  TO  P-TNA P-HNA.
           MOVE W-TC TO P-TC.
       M-25.
           MOVE ZERO TO W-D.
           MOVE SNTR-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
       M-30.
           MOVE W-TCD TO P-TCD.
           MOVE W-TNA TO P-TNA.
           MOVE ZERO TO CNT.
       M-35.
           MOVE SNTR-HCD TO P-HCD.
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO HI-NAME
           END-IF
           MOVE HI-NAME TO P-HNA.
       M-40.
           COMPUTE W-T ROUNDED = SNTR-KIN / SNTR-SU.
           MOVE W-15K TO P-15K.
           MOVE SNTR-SU TO P-SU.
           MOVE W-T TO P-T.
           MOVE SNTR-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               MOVE W-TCD TO P-TCD
               MOVE W-TNA TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-20K TO P-20K.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE ALL "　"  TO  P-TNA P-HNA.
           ADD SNTR-SU TO W-SU.
           ADD SNTR-KIN TO W-KIN.
           ADD 1 TO CNT.
       M-45.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  ZERO = SNTR-SU OR SNTR-KIN
               GO TO M-45
           END-IF
           IF  W-IKV = 1
               IF (SNTR-BCD1 = 322) OR (SNTR-BC3 = 30)
                   GO TO M-45
               END-IF
           END-IF
           IF  W-IKV = 2
               IF  SNTR-BC3 NOT = 30
                   GO TO M-45
               END-IF
           END-IF
           IF  W-IKV = 3
               IF  SNTR-BCD1 NOT = 322
                   GO TO M-45
               END-IF
           END-IF
           IF  SNTR-TNC NOT = W-TC
               GO TO M-55
           END-IF
           IF  SNTR-TCD NOT = W-TCD
               GO TO M-50
           END-IF
           GO TO M-35.
       M-50.
           PERFORM S-20 THRU S-30.
           GO TO M-25.
       M-55.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-20.
       M-90.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           MOVE SPACE TO W-P.
           MOVE ALL "　"  TO  P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】　　" TO P-TNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-20K TO P-20K.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
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
           IF  CNT = 1
               GO TO S-25
           END-IF
           MOVE W-15K TO P-15K.
           MOVE ALL "　"  TO  P-TNA P-HNA.
           MOVE "　　　　　　　　　　　　（　ＴＯＴＡＬ　）" TO P-HNA.
           MOVE W-SU TO P-SU.
           MOVE W-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               MOVE W-TCD TO P-TCD
               MOVE W-TNA TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-20K TO P-20K.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           MOVE SPACE TO SP-R W-P.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-SU TO WS-SU.
           ADD W-KIN TO WS-KIN.
       S-30.
           EXIT.
       S-35.
           MOVE ALL "　"  TO  P-TNA P-HNA.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-TNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE W-20K TO P-20K.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-40.
           EXIT.
