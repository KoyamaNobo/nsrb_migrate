       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG020.
      *******************************************************************
      *    担当得意先品種別売上集計表（ハイパーＶ）                     *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(007) VALUE "【ハイパーＶ】".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(024) VALUE
               "＊＊＊　　担当得意先品種別　売上集計表　　＊＊＊".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　数　量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "平均売価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-HCD1         PIC  9(004).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ----,--9.
           02  P-UT           PIC ----,--9.
           02  P-KIN          PIC ----,---,--9.
       01  W-D.
           02  W-KEY.
             03  W-TCD        PIC  9(004).
             03  W-HCD1       PIC  9(004).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-UT           PIC S9(005).
           02  W-RD.
             03  W-SU         PIC S9(005).
             03  W-UKIN       PIC S9(008).
       01  WN-D.
           02  WN-SU          PIC S9(006).
           02  WN-UKI         PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(006).
           02  WT-UKI         PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(006).
           02  WS-UKI         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(006).
           02  WA-UKI         PIC S9(009).
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-DC           PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SETC.
             03  W-STC        PIC  9(002).
             03  W-ETC        PIC  9(002) VALUE 99.
           02  CNT            PIC  9(002).
           02  W-NAME         PIC  N(024).
           02  W-ANAD  REDEFINES W-NAME.
             03  W-NAD   OCCURS  24.
               04  W-NA       PIC  N(001).
           02  W-HNA          PIC  N(024).
           02  W-AHND  REDEFINES W-HNA.
             03  W-AHNA  OCCURS  24.
               04  W-HNAD     PIC  N(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　担当得意先品種別　売上集計表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　（ハイパーＶ）　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(020) VALUE
                "担当者ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STC   PIC  9(002).
             03  A-ETC   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-KEY   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-MID" " " "0" "0" "378" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "24" "20" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "23" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STC" "9" "15" "36" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STC" BY REFERENCE W-STC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETC" "9" "15" "42" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETC" BY REFERENCE W-ETC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "40" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "50" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "50" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "35" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-07.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-07
           END-IF.
       M-08.
           CALL "SD_Accept" USING BY REFERENCE A-ETC "A-ETC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-07
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-08
           END-IF
           IF  W-STC > W-ETC
               GO TO M-08
           END-IF.
       M-12.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-08
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-12
           END-IF
           IF  W-DMM = 9
               GO TO M-07
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-12
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-15.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-15
           END-IF
           IF  SNTR-CSC NOT = 0
               GO TO M-15
           END-IF
           IF  SNTR-TNC < W-STC OR > W-ETC
               GO TO M-15
           END-IF
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 0 TO HI-HPV
           END-IF
           IF  HI-HPV NOT = 1
               GO TO M-15
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN
               GO TO M-15
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-PAGE.
           PERFORM HED-010 THRU HED-EX.
           MOVE ZERO TO WA-D.
       M-20.
           MOVE ZERO TO WS-D.
           MOVE SNTR-TNC1 TO W-TNC1.
       M-25.
           MOVE 0 TO W-C.
           MOVE SNTR-TNC2 TO W-TNC2.
       M-30.
           MOVE ZERO TO WT-D CHK W-DC.
           MOVE SNTR-TCD TO W-TCD.
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　マスター　なし　＊　　　　　" TO T-NAME
           END-IF.
       M-35.
           MOVE ZERO TO WN-D.
           MOVE SNTR-HCD1 TO W-HCD1.
       M-40.
           ADD W-SU TO WN-SU.
           ADD W-UKIN TO WN-UKI.
       M-45.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-45
           END-IF
           IF  SNTR-CSC NOT = 0
               GO TO M-45
           END-IF
           IF  SNTR-TNC < W-STC OR > W-ETC
               GO TO M-45
           END-IF
           MOVE SNTR-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 0 TO HI-HPV
           END-IF
           IF  HI-HPV NOT = 1
               GO TO M-45
           END-IF
           PERFORM DST-RTN THRU DST-EX.
           IF  ZERO = W-SU AND W-UKIN
               GO TO M-45
           END-IF
      *
           IF  W-TNC NOT = SNTR-TNC
               GO TO M-60
           END-IF
           IF  W-TCD NOT = SNTR-TCD
               GO TO M-55
           END-IF
           IF  W-HCD1 NOT = SNTR-HCD1
               GO TO M-50
           END-IF
           GO TO M-40.
       M-50.
           PERFORM MPR-RTN THRU MPR-EX.
           GO TO M-35.
       M-55.
           PERFORM MPR-RTN THRU MPR-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           GO TO M-30.
       M-60.
           PERFORM MPR-RTN THRU MPR-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           IF  W-TNC1 = SNTR-TNC1
               GO TO M-25
           END-IF
           PERFORM SPR-RTN THRU SPR-EX.
           GO TO M-20.
       M-80.
           PERFORM MPR-RTN THRU MPR-EX.
           PERFORM KPR-RTN THRU KPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE "　【　　総　合　計　　】　　　　" TO P-HNA.
           MOVE WA-SU TO P-SU.
           MOVE WA-UKI TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DST-RTN.
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       DST-EX.
           EXIT.
       HED-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HED-010.
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
       HED-EX.
           EXIT.
       MPR-RTN.
           IF  ZERO = WN-SU AND WN-UKI
               GO TO MPR-EX
           END-IF
           PERFORM HNA-RTN THRU HNA-EX.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           IF  W-C = ZERO
               MOVE 5 TO W-C
               MOVE 0 TO CHK
               MOVE W-TNC TO P-TNC
           END-IF
           IF  CHK = ZERO
               MOVE 5 TO CHK
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           MOVE W-HCD1 TO P-HCD1.
           MOVE W-NAME TO P-HNA.
           MOVE WN-SU TO P-SU.
           MOVE WN-UKI TO P-KIN.
           MOVE ZERO TO W-UT.
           IF  WN-SU = ZERO
               GO TO MPR-020
           END-IF
           IF  WN-UKI NOT = ZERO
               COMPUTE W-UT ROUNDED = WN-UKI / WN-SU
           END-IF
           MOVE W-UT TO P-UT.
       MPR-020.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WN-SU TO WT-SU.
           ADD WN-UKI TO WT-UKI.
           ADD 1 TO W-DC.
       MPR-EX.
           EXIT.
       HNA-RTN.
           MOVE SPACE TO W-NAME W-HNA.
           MOVE SPACE TO HI-KEY.
           MOVE W-HCD1 TO HI-HCD1.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" "NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO W-NAME
               GO TO HNA-EX
           END-IF
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO W-NAME
               GO TO HNA-EX
           END-IF
           IF  W-HCD1 NOT = HI-HCD1
               MOVE "　＊＊　マスター　なし　＊＊" TO W-NAME
               GO TO HNA-EX
           END-IF
           MOVE HI-NAME TO W-HNA.
           MOVE ZERO TO CNT.
       HNA-020.
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO HNA-EX
           END-IF
           MOVE W-HNAD(CNT) TO W-NA(CNT).
           IF  W-HNAD(CNT) NOT = SPACE
               GO TO HNA-020
           END-IF
           ADD 1 TO CNT.
           IF  CNT > 24
               GO TO HNA-EX
           END-IF
           MOVE W-HNAD(CNT) TO W-NA(CNT).
           IF  W-HNAD(CNT) NOT = SPACE
               GO TO HNA-020
           END-IF.
       HNA-EX.
           EXIT.
       KPR-RTN.
           IF  ZERO = WT-SU AND WT-UKI
               GO TO KPR-EX
           END-IF
           IF  W-DC < 2
               GO TO KPR-040
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE "　　　　　　　　　　　（　小　計　）　" TO P-HNA.
           MOVE WT-SU TO P-SU.
           MOVE WT-UKI TO P-KIN.
       KPR-020.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KPR-040.
           ADD WT-SU TO WS-SU.
           ADD WT-UKI TO WS-UKI.
       KPR-EX.
           EXIT.
       SPR-RTN.
           IF  ZERO = WS-SU AND WS-UKI
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO SPR-EX
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA P-HNA.
           MOVE "　　　　［　　合　計　　］　　　　　" TO P-HNA.
           MOVE WS-SU TO P-SU.
           MOVE WS-UKI TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM HED-RTN THRU HED-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-SU TO WA-SU.
           ADD WS-UKI TO WA-UKI.
       SPR-EX.
           EXIT.
