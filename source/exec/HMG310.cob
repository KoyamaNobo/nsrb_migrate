       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMG310.
      *********************************************************
      *    PROGRAM         :  担当得意先品名別 売上･返品明細表*
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
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　担当得意先品名別　売上・返品明細表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005)  VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007)  VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  X(008) VALUE " I----  ".
           02  F              PIC  N(004) VALUE "売　　上".
           02  F              PIC  X(006) VALUE "  ---I".
           02  F              PIC  X(016) VALUE " I------------  ".
           02  F              PIC  N(004) VALUE "返　　品".
           02  F              PIC  X(014) VALUE "  -----------I".
           02  F              PIC  X(008) VALUE " I----  ".
           02  F              PIC  N(004) VALUE "合　　計".
           02  F              PIC  X(006) VALUE "  ---I".
       01  HEAD3.
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　返品数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　不良数".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数量計".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(002) VALUE "金額".
       01  W-P1.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(088).
       01  W-P2.
           02  F              PIC  X(017).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-USU          PIC ----,--9.
           02  P-UKIN         PIC ----,---,--9.
           02  P-PSU          PIC ----,--9.
           02  P-FSU          PIC ----,--9.
           02  P-HSU          PIC ----,--9.
           02  P-HKIN         PIC ----,---,--9.
           02  P-TSU          PIC ----,--9.
           02  P-TKIN         PIC ----,---,--9.
       01  W-DATA.
           02  W-SED.
             03  W-STNC       PIC  9(002).
             03  W-ETNC       PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
           02  W-TNC          PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-D.
             03  W-USU        PIC S9(006).
             03  W-UKIN       PIC S9(008).
             03  W-PSU        PIC S9(006).
             03  W-FSU        PIC S9(006).
             03  W-HSU        PIC S9(006).
             03  W-HKIN       PIC S9(008).
             03  W-TSU        PIC S9(006).
             03  W-TKIN       PIC S9(008).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(003).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  WT-D.
           02  WT-USU         PIC S9(006).
           02  WT-UKIN        PIC S9(009).
           02  WT-PSU         PIC S9(006).
           02  WT-FSU         PIC S9(006).
           02  WT-HSU         PIC S9(006).
           02  WT-HKIN        PIC S9(009).
           02  WT-TSU         PIC S9(006).
           02  WT-TKIN        PIC S9(009).
       01  WS-D.
           02  WS-USU         PIC S9(006).
           02  WS-UKIN        PIC S9(009).
           02  WS-PSU         PIC S9(006).
           02  WS-FSU         PIC S9(006).
           02  WS-HSU         PIC S9(006).
           02  WS-HKIN        PIC S9(009).
           02  WS-TSU         PIC S9(006).
           02  WS-TKIN        PIC S9(009).
       01  WA-D.
           02  WA-USU         PIC S9(006).
           02  WA-UKIN        PIC S9(009).
           02  WA-PSU         PIC S9(006).
           02  WA-FSU         PIC S9(006).
           02  WA-HSU         PIC S9(006).
           02  WA-HKIN        PIC S9(009).
           02  WA-TSU         PIC S9(006).
           02  WA-TKIN        PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　得意先品名別　売上・返品明細表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(025) VALUE
                "担当ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STNC  PIC  9(002).
             03  A-ETNC  PIC  9(002).
           02  A-DMM   PIC  9(001).
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
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "397" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
           "08C-MID" "X" "14" "22" "25" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "24" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "14" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STNC" "9" "14" "32" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETNC" "9" "14" "38" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "41" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-STNC > W-ETNC
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-25.
      *           READ SNTRF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               GO TO M-95
           END-IF
           IF  SNTR-TNC < W-STNC
               GO TO M-25
           END-IF
           IF  SNTR-TNC > W-ETNC
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               GO TO M-95
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-25
           END-IF
           IF  ZERO = SNTR-SU AND SNTR-KIN
               GO TO M-25
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-30.
           MOVE ZERO TO WS-D CHK1.
           MOVE SNTR-TNC TO W-TNC.
       M-35.
           MOVE ZERO TO WT-D CNT CHK2.
           MOVE SNTR-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO T-NAME
           END-IF.
       M-40.
           MOVE ZERO TO W-D.
           MOVE SNTR-HCD TO W-HCD.
       M-45.
           IF  SNTR-SNC = 1
               SUBTRACT SNTR-KIN FROM W-UKIN W-TKIN
               GO TO M-50
           END-IF
           IF  SNTR-HCD < 999900
               IF  SNTR-DC = 1
                   ADD SNTR-SU TO W-PSU W-HSU
                   ADD SNTR-KIN TO W-HKIN
                   SUBTRACT SNTR-SU FROM W-TSU
                   SUBTRACT SNTR-KIN FROM W-TKIN
               ELSE
                   IF  SNTR-DC = 2
                       ADD SNTR-SU TO W-FSU W-HSU
                       ADD SNTR-KIN TO W-HKIN
                       SUBTRACT SNTR-KIN FROM W-TKIN
                   ELSE
                       IF  SNTR-DC = 5 OR 6
                           SUBTRACT SNTR-SU FROM W-USU W-TSU
                           SUBTRACT SNTR-KIN FROM W-UKIN W-TKIN
                       ELSE
                           ADD SNTR-SU TO W-USU W-TSU
                           ADD SNTR-KIN TO W-UKIN W-TKIN
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SNTR-HCD > 999899
               IF  SNTR-DC = 1 OR 2
                   ADD SNTR-KIN TO W-HKIN
                   SUBTRACT SNTR-KIN FROM W-TKIN
               ELSE
                   IF  SNTR-DC = 5 OR 6
                       SUBTRACT SNTR-KIN FROM W-UKIN W-TKIN
                   ELSE
                       ADD SNTR-KIN TO W-UKIN W-TKIN
                   END-IF
               END-IF
           END-IF.
       M-50.
      *           READ SNTRF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-TNC > W-ETNC
               GO TO M-90
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-50
           END-IF
           IF  ZERO = SNTR-SU AND SNTR-KIN
               GO TO M-50
           END-IF
           IF  SNTR-TNC NOT = W-TNC
               GO TO M-65
           END-IF
           IF  SNTR-TCD NOT = W-TCD
               GO TO M-60
           END-IF
           IF  SNTR-HCD NOT = W-HCD
               GO TO M-55
           END-IF
           GO TO M-45.
       M-55.
           PERFORM S-30 THRU S-35.
           GO TO M-40.
       M-60.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           GO TO M-35.
       M-65.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           PERFORM S-60 THRU S-65.
           GO TO M-30.
       M-90.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           PERFORM S-60 THRU S-65.
           PERFORM S-70 THRU S-75.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNA.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-TNC TO P-TNC
           END-IF
           MOVE 1 TO CHK2.
           MOVE W-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = W-USU AND W-UKIN AND W-PSU AND W-FSU AND
                     W-HSU AND W-HKIN AND W-TSU AND W-TKIN
               GO TO S-35
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO HI-NAME
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE W-USU TO P-USU.
           MOVE W-UKIN TO P-UKIN.
           MOVE W-PSU TO P-PSU.
           MOVE W-FSU TO P-FSU.
           MOVE W-HSU TO P-HSU.
           MOVE W-HKIN TO P-HKIN.
           MOVE W-TSU TO P-TSU.
           MOVE W-TKIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE ZERO TO CHK
               PERFORM S-05 THRU S-15
           END-IF
           IF  0 = CHK1 OR CHK2
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-USU TO WT-USU.
           ADD W-UKIN TO WT-UKIN.
           ADD W-PSU TO WT-PSU.
           ADD W-FSU TO WT-FSU.
           ADD W-HSU TO WT-HSU.
           ADD W-HKIN TO WT-HKIN.
           ADD W-TSU TO WT-TSU.
           ADD W-TKIN TO WT-TKIN.
           ADD 1 TO CNT.
       S-35.
           EXIT.
       S-40.
           IF  CNT = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　　　　　　　（　ＴＯＴＡＬ　）" TO P-HNA.
           MOVE WT-USU TO P-USU.
           MOVE WT-UKIN TO P-UKIN.
           MOVE WT-PSU TO P-PSU.
           MOVE WT-FSU TO P-FSU.
           MOVE WT-HSU TO P-HSU.
           MOVE WT-HKIN TO P-HKIN.
           MOVE WT-TSU TO P-TSU.
           MOVE WT-TKIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
               MOVE ZERO TO CHK
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-45.
           ADD WT-USU TO WS-USU.
           ADD WT-UKIN TO WS-UKIN.
           ADD WT-PSU TO WS-PSU.
           ADD WT-FSU TO WS-FSU.
           ADD WT-HSU TO WS-HSU.
           ADD WT-HKIN TO WS-HKIN.
           ADD WT-TSU TO WS-TSU.
           ADD WT-TKIN TO WS-TKIN.
       S-50.
           EXIT.
       S-60.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE "　　　　　　［　ＳＵＢ　ＴＯＴＡＬ　］" TO P-HNA.
           MOVE WS-USU TO P-USU.
           MOVE WS-UKIN TO P-UKIN.
           MOVE WS-PSU TO P-PSU.
           MOVE WS-FSU TO P-FSU.
           MOVE WS-HSU TO P-HSU.
           MOVE WS-HKIN TO P-HKIN.
           MOVE WS-TSU TO P-TSU.
           MOVE WS-TKIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-USU TO WA-USU.
           ADD WS-UKIN TO WA-UKIN.
           ADD WS-PSU TO WA-PSU.
           ADD WS-FSU TO WA-FSU.
           ADD WS-HSU TO WA-HSU.
           ADD WS-HKIN TO WA-HKIN.
           ADD WS-TSU TO WA-TSU.
           ADD WS-TKIN TO WA-TKIN.
       S-65.
           EXIT.
       S-70.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           MOVE "　　【　ＡＬＬ　ＴＯＴＡＬ　】" TO P-HNA.
           MOVE WA-USU TO P-USU.
           MOVE WA-UKIN TO P-UKIN.
           MOVE WA-PSU TO P-PSU.
           MOVE WA-FSU TO P-FSU.
           MOVE WA-HSU TO P-HSU.
           MOVE WA-HKIN TO P-HKIN.
           MOVE WA-TSU TO P-TSU.
           MOVE WA-TKIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.
