       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG180.
      ***********************************************************
      *    PROGRAM         :  履物担当得意先別販売実績表        *
      *    PRINTER TYPE    :  JIPS                              *
      *    SCREEN          :  ______                            *
      *    COMPILE TYPE    :  COBOL                             *
      *    JS-SIGN         :  0=作表 , 1=ＥＸＣＥＬ , 2=作表(参 *
      ***********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  JS-SIGN            PIC  9(001).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-SAN          PIC  N(001) VALUE SPACE.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　履物担当得意先別　販売実績表　　＊＊＊".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "一般売上金額".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  N(008) VALUE "　ワーク売上金額".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(006) VALUE "教育売上金額".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNAME        PIC  N(026).
           02  P-U            PIC ----,---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC ----9.9.
           02  P-IU           PIC ----,---,--9.
           02  P-IRR          PIC ----9.9.
           02  P-WU           PIC ----,---,--9.
           02  P-WRR          PIC ----9.9.
           02  P-KU           PIC ----,---,--9.
           02  P-KRR          PIC ----9.9.
       01  W-DATA.
           02  W-STNC         PIC  9(002).
           02  W-ETNC         PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-UD           PIC S9(009).
           02  W-ALL          PIC S9(009)V9(05).
           02  W-PAGE         PIC  9(002).
           02  CNT            PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-KIN.
             03  W-UKIN       PIC S9(008).
             03  W-GKIN       PIC S9(008).
           02  W-RR           PIC S9(003)V9(01).
           02  W-IRR          PIC S9(003)V9(01).
           02  W-WRR          PIC S9(003)V9(01).
           02  W-KRR          PIC S9(003)V9(01).
       01  WN-D.
           02  W-U            PIC S9(009).
           02  W-UG           PIC S9(009).
           02  W-AR           PIC S9(009).
           02  W-IU           PIC S9(009).
           02  W-IUG          PIC S9(009).
           02  W-IAR          PIC S9(009).
           02  W-WU           PIC S9(009).
           02  W-WUG          PIC S9(009).
           02  W-WAR          PIC S9(009).
           02  W-KU           PIC S9(009).
           02  W-KUG          PIC S9(009).
           02  W-KAR          PIC S9(009).
       01  WT-D.
           02  WT-U           PIC S9(009).
           02  WT-UG          PIC S9(009).
           02  WT-AR          PIC S9(009).
           02  WT-IU          PIC S9(009).
           02  WT-IUG         PIC S9(009).
           02  WT-IAR         PIC S9(009).
           02  WT-WU          PIC S9(009).
           02  WT-WUG         PIC S9(009).
           02  WT-WAR         PIC S9(009).
           02  WT-KU          PIC S9(009).
           02  WT-KUG         PIC S9(009).
           02  WT-KAR         PIC S9(009).
       01  WS-D.
           02  WS-U           PIC S9(009).
           02  WS-UG          PIC S9(009).
           02  WS-AR          PIC S9(009).
           02  WS-IU          PIC S9(009).
           02  WS-IUG         PIC S9(009).
           02  WS-IAR         PIC S9(009).
           02  WS-WU          PIC S9(009).
           02  WS-WUG         PIC S9(009).
           02  WS-WAR         PIC S9(009).
           02  WS-KU          PIC S9(009).
           02  WS-KUG         PIC S9(009).
           02  WS-KAR         PIC S9(009).
       01  WA-D.
           02  WA-U           PIC S9(009).
           02  WA-UG          PIC S9(009).
           02  WA-AR          PIC S9(009).
           02  WA-IU          PIC S9(009).
           02  WA-IUG         PIC S9(009).
           02  WA-IAR         PIC S9(009).
           02  WA-WU          PIC S9(009).
           02  WA-WUG         PIC S9(009).
           02  WA-WAR         PIC S9(009).
           02  WA-KU          PIC S9(009).
           02  WA-KUG         PIC S9(009).
           02  WA-KAR         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSSNTW.
           COPY LSPF.
      *FD  EXL-F
       01  EXL-F_HMG180.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_HMG180".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-TNC        PIC  9(002).
           02  EXL-TCD        PIC  9(004).
           02  EXL-TNAME      PIC  N(026).
           02  EXL-U          PIC S9(009).
           02  EXL-UG         PIC S9(009).
           02  EXL-AR         PIC S9(009).
           02  EXL-RR         PIC S9(003)V9(01).
           02  EXL-IU         PIC S9(009).
           02  EXL-IUG        PIC S9(009).
           02  EXL-IAR        PIC S9(009).
           02  EXL-IRR        PIC S9(003)V9(01).
           02  EXL-WU         PIC S9(009).
           02  EXL-WUG        PIC S9(009).
           02  EXL-WAR        PIC S9(009).
           02  EXL-WRR        PIC S9(003)V9(01).
           02  EXL-KU         PIC S9(009).
           02  EXL-KUG        PIC S9(009).
           02  EXL-KAR        PIC S9(009).
           02  EXL-KRR        PIC S9(003)V9(01).
           02  F              PIC  X(074).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　担当得意先別　売上集計表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(018) VALUE
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
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
           "C-MID" " " "0" "0" "348" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "12" "23" "18" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "40" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "12" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STNC" "9" "12" "33" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETNC" "9" "12" "39" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "57" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           IF  JS-SIGN = 1
               MOVE 00 TO W-STNC
               MOVE 99 TO W-ETNC
               GO TO M-25
           END-IF.
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
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF.
       M-25.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-30.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
                CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SNTR-TNC < W-STNC
               GO TO M-30
           END-IF
           IF  SNTR-TNC > W-ETNC
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-30
           END-IF
           MOVE ZERO TO W-KIN.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF  ZERO = W-UKIN AND W-GKIN
               GO TO M-30
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           MOVE ZERO TO WA-D W-PAGE.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
               GO TO M-35
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           IF  JS-SIGN = 2
               MOVE "参" TO H-SAN
           END-IF
           PERFORM MID-010 THRU MID-EX.
       M-35.
           MOVE SNTR-TNC1 TO W-TNC1.
           MOVE ZERO TO WS-D.
       M-40.
           MOVE SNTR-TNC2 TO W-TNC2.
           MOVE ZERO TO WT-D CHK CNT.
       M-45.
           MOVE SNTR-TCD TO W-TCD.
           MOVE ZERO TO WN-D.
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　マスター　なし　＊＊　　" TO T-NAME
           END-IF.
       M-50.
           ADD W-UKIN TO W-U.
           ADD W-GKIN TO W-UG.
           IF  SNTR-BC3 = 10
               ADD W-UKIN TO W-IU
               ADD W-GKIN TO W-IUG
           ELSE
               IF  SNTR-BC3 = 20
                   ADD W-UKIN TO W-WU
                   ADD W-GKIN TO W-WUG
               ELSE
                   IF  SNTR-BC3 = 30
                       ADD W-UKIN TO W-KU
                       ADD W-GKIN TO W-KUG
                   END-IF
               END-IF
           END-IF.
       M-55.
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
               GO TO M-55
           END-IF
           MOVE ZERO TO W-KIN.
           IF (SNTR-SNC = 0) OR (SNTR-DC NOT = 2)
               COMPUTE W-GKIN = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-GKIN = W-GKIN * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF  ZERO = W-UKIN AND W-GKIN
               GO TO M-55
           END-IF
           IF  SNTR-TNC1 NOT = W-TNC1
               GO TO M-70
           END-IF
           IF  SNTR-TNC2 NOT = W-TNC2
               GO TO M-65
           END-IF
           IF  SNTR-TCD = W-TCD
               GO TO M-50
           END-IF
      *
           PERFORM MEI-RTN THRU MEI-EX.
           GO TO M-45.
       M-65.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-40.
       M-70.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-35.
       M-90.
           PERFORM MEI-RTN THRU MEI-EX.
           PERFORM TOT-RTN THRU TOT-EX.
           PERFORM SUB-RTN THRU SUB-EX.
           PERFORM ALL-RTN THRU ALL-EX.
      *
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
       MID-EX.
           EXIT.
       MEI-RTN.
           IF  ZERO = W-U AND W-UG AND W-IU AND W-IUG AND
                             W-WU AND W-WUG AND W-KU AND W-KUG
               GO TO MEI-EX
           END-IF
           IF  W-TNC > 89
               MOVE W-U TO W-UG
           END-IF
           COMPUTE W-AR = W-U - W-UG.
           COMPUTE W-IAR = W-IU - W-IUG.
           COMPUTE W-WAR = W-WU - W-WUG.
           COMPUTE W-KAR = W-KU - W-KUG.
      *
           MOVE ZERO TO W-RR.
           IF  ZERO = W-U OR W-AR
               GO TO MEI-010
           END-IF
           MOVE W-U TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (W-AR / W-UD) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       MEI-010.
           MOVE ZERO TO W-IRR.
           IF  ZERO = W-IU OR W-IAR
               GO TO MEI-020
           END-IF
           MOVE W-IU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (W-IAR / W-UD) * 100.
           COMPUTE W-IRR ROUNDED = W-ALL * 1.
       MEI-020.
           MOVE ZERO TO W-WRR.
           IF  ZERO = W-WU OR W-WAR
               GO TO MEI-030
           END-IF
           MOVE W-WU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (W-WAR / W-UD) * 100.
           COMPUTE W-WRR ROUNDED = W-ALL * 1.
       MEI-030.
           MOVE ZERO TO W-KRR.
           IF  ZERO = W-KU OR W-KAR
               GO TO MEI-040
           END-IF
           MOVE W-KU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (W-KAR / W-UD) * 100.
           COMPUTE W-KRR ROUNDED = W-ALL * 1.
       MEI-040.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNAME
           ELSE
               MOVE SPACE TO W-P
           END-IF
           IF  CHK = ZERO
               MOVE 5 TO CHK
               IF  JS-SIGN = 1
                   MOVE W-TNC TO EXL-TNC
               ELSE
                   MOVE W-TNC TO P-TNC
               END-IF
           END-IF
           IF  JS-SIGN = 1
               MOVE W-TCD TO EXL-TCD
               MOVE T-NAME TO EXL-TNAME
               MOVE W-U TO EXL-U
               MOVE W-UG TO EXL-UG
               MOVE W-AR TO EXL-AR
               MOVE W-RR TO EXL-RR
               MOVE W-IU TO EXL-IU
               MOVE W-IUG TO EXL-IUG
               MOVE W-IAR TO EXL-IAR
               MOVE W-IRR TO EXL-IRR
               MOVE W-WU TO EXL-WU
               MOVE W-WUG TO EXL-WUG
               MOVE W-WAR TO EXL-WAR
               MOVE W-WRR TO EXL-WRR
               MOVE W-KU TO EXL-KU
               MOVE W-KUG TO EXL-KUG
               MOVE W-KAR TO EXL-KAR
               MOVE W-KRR TO EXL-KRR
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO MEI-050
           END-IF
           MOVE W-TCD TO P-TCD.
           MOVE T-NAME TO P-TNAME.
           MOVE W-U TO P-U.
           MOVE W-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF (W-IU NOT = ZERO) OR (W-IRR NOT = ZERO)
               MOVE W-IU TO P-IU
               MOVE W-IRR TO P-IRR
           END-IF
           IF (W-WU NOT = ZERO) OR (W-WRR NOT = ZERO)
               MOVE W-WU TO P-WU
               MOVE W-WRR TO P-WRR
           END-IF
           IF (W-KU NOT = ZERO) OR (W-KRR NOT = ZERO)
               MOVE W-KU TO P-KU
               MOVE W-KRR TO P-KRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-050.
           ADD W-U TO WT-U.
           ADD W-UG TO WT-UG.
           ADD W-AR TO WT-AR.
           ADD W-IU TO WT-IU.
           ADD W-IUG TO WT-IUG.
           ADD W-IAR TO WT-IAR.
           ADD W-WU TO WT-WU.
           ADD W-WUG TO WT-WUG.
           ADD W-WAR TO WT-WAR.
           ADD W-KU TO WT-KU.
           ADD W-KUG TO WT-KUG.
           ADD W-KAR TO WT-KAR.
           ADD 1 TO CNT.
       MEI-EX.
           EXIT.
       TOT-RTN.
           IF  JS-SIGN NOT = 1
               IF  CNT = 1
                   MOVE SPACE TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   GO TO TOT-050
               END-IF
           END-IF
           MOVE ZERO TO W-RR.
           IF  ZERO = WT-U OR WT-AR
               GO TO TOT-010
           END-IF
           MOVE WT-U TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WT-AR / W-UD) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       TOT-010.
           MOVE ZERO TO W-IRR.
           IF  ZERO = WT-IU OR WT-IAR
               GO TO TOT-020
           END-IF
           MOVE WT-IU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WT-IAR / W-UD) * 100.
           COMPUTE W-IRR ROUNDED = W-ALL * 1.
       TOT-020.
           MOVE ZERO TO W-WRR.
           IF  ZERO = WT-WU OR WT-WAR
               GO TO TOT-030
           END-IF
           MOVE WT-WU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WT-WAR / W-UD) * 100.
           COMPUTE W-WRR ROUNDED = W-ALL * 1.
       TOT-030.
           MOVE ZERO TO W-KRR.
           IF  ZERO = WT-KU OR WT-KAR
               GO TO TOT-040
           END-IF
           MOVE WT-KU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WT-KAR / W-UD) * 100.
           COMPUTE W-KRR ROUNDED = W-ALL * 1.
       TOT-040.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNAME
               MOVE "　　　　　　　　（　　合　計　　）" TO EXL-TNAME
               MOVE WT-U TO EXL-U
               MOVE WT-UG TO EXL-UG
               MOVE WT-AR TO EXL-AR
               MOVE W-RR TO EXL-RR
               MOVE WT-IU TO EXL-IU
               MOVE WT-IUG TO EXL-IUG
               MOVE WT-IAR TO EXL-IAR
               MOVE W-IRR TO EXL-IRR
               MOVE WT-WU TO EXL-WU
               MOVE WT-WUG TO EXL-WUG
               MOVE WT-WAR TO EXL-WAR
               MOVE W-WRR TO EXL-WRR
               MOVE WT-KU TO EXL-KU
               MOVE WT-KUG TO EXL-KUG
               MOVE WT-KAR TO EXL-KAR
               MOVE W-KRR TO EXL-KRR
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO TOT-050
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　　（　　合　計　　）　" TO P-TNAME.
           MOVE WT-U TO P-U.
           MOVE WT-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF (WT-IU NOT = ZERO) OR (W-IRR NOT = ZERO)
               MOVE WT-IU TO P-IU
               MOVE W-IRR TO P-IRR
           END-IF
           IF (WT-WU NOT = ZERO) OR (W-WRR NOT = ZERO)
               MOVE WT-WU TO P-WU
               MOVE W-WRR TO P-WRR
           END-IF
           IF (WT-KU NOT = ZERO) OR (W-KRR NOT = ZERO)
               MOVE WT-KU TO P-KU
               MOVE W-KRR TO P-KRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       TOT-050.
           ADD WT-U TO WS-U.
           ADD WT-UG TO WS-UG.
           ADD WT-AR TO WS-AR.
           ADD WT-IU TO WS-IU.
           ADD WT-IUG TO WS-IUG.
           ADD WT-IAR TO WS-IAR.
           ADD WT-WU TO WS-WU.
           ADD WT-WUG TO WS-WUG.
           ADD WT-WAR TO WS-WAR.
           ADD WT-KU TO WS-KU.
           ADD WT-KUG TO WS-KUG.
           ADD WT-KAR TO WS-KAR.
       TOT-EX.
           EXIT.
       SUB-RTN.
           MOVE ZERO TO W-RR.
           IF  ZERO = WS-U OR WS-AR
               GO TO SUB-010
           END-IF
           MOVE WS-U TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WS-AR / W-UD) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       SUB-010.
           MOVE ZERO TO W-IRR.
           IF  ZERO = WS-IU OR WS-IAR
               GO TO SUB-020
           END-IF
           MOVE WS-IU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WS-IAR / W-UD) * 100.
           COMPUTE W-IRR ROUNDED = W-ALL * 1.
       SUB-020.
           MOVE ZERO TO W-WRR.
           IF  ZERO = WS-WU OR WS-WAR
               GO TO SUB-030
           END-IF
           MOVE WS-WU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WS-WAR / W-UD) * 100.
           COMPUTE W-WRR ROUNDED = W-ALL * 1.
       SUB-030.
           MOVE ZERO TO W-KRR.
           IF  ZERO = WS-KU OR WS-KAR
               GO TO SUB-040
           END-IF
           MOVE WS-KU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WS-KAR / W-UD) * 100.
           COMPUTE W-KRR ROUNDED = W-ALL * 1.
       SUB-040.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNAME
               MOVE "　　　　　［　　小　計　　］　" TO EXL-TNAME
               MOVE WS-U TO EXL-U
               MOVE WS-UG TO EXL-UG
               MOVE WS-AR TO EXL-AR
               MOVE W-RR TO EXL-RR
               MOVE WS-IU TO EXL-IU
               MOVE WS-IUG TO EXL-IUG
               MOVE WS-IAR TO EXL-IAR
               MOVE W-IRR TO EXL-IRR
               MOVE WS-WU TO EXL-WU
               MOVE WS-WUG TO EXL-WUG
               MOVE WS-WAR TO EXL-WAR
               MOVE W-WRR TO EXL-WRR
               MOVE WS-KU TO EXL-KU
               MOVE WS-KUG TO EXL-KUG
               MOVE WS-KAR TO EXL-KAR
               MOVE W-KRR TO EXL-KRR
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO SUB-050
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　　　　　［　　小　計　　］　　　　　" TO P-TNAME.
           MOVE WS-U TO P-U.
           MOVE WS-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF (WS-IU NOT = ZERO) OR (W-IRR NOT = ZERO)
               MOVE WS-IU TO P-IU
               MOVE W-IRR TO P-IRR
           END-IF
           IF (WS-WU NOT = ZERO) OR (W-WRR NOT = ZERO)
               MOVE WS-WU TO P-WU
               MOVE W-WRR TO P-WRR
           END-IF
           IF (WS-KU NOT = ZERO) OR (W-KRR NOT = ZERO)
               MOVE WS-KU TO P-KU
               MOVE W-KRR TO P-KRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TNC TO P-TNC
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       SUB-050.
           ADD WS-U TO WA-U.
           ADD WS-UG TO WA-UG.
           ADD WS-AR TO WA-AR.
           ADD WS-IU TO WA-IU.
           ADD WS-IUG TO WA-IUG.
           ADD WS-IAR TO WA-IAR.
           ADD WS-WU TO WA-WU.
           ADD WS-WUG TO WA-WUG.
           ADD WS-WAR TO WA-WAR.
           ADD WS-KU TO WA-KU.
           ADD WS-KUG TO WA-KUG.
           ADD WS-KAR TO WA-KAR.
       SUB-EX.
           EXIT.
       ALL-RTN.
           MOVE ZERO TO W-RR.
           IF  ZERO = WA-U OR WA-AR
               GO TO ALL-010
           END-IF
           MOVE WA-U TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WA-AR / W-UD) * 100.
           COMPUTE W-RR ROUNDED = W-ALL * 1.
       ALL-010.
           MOVE ZERO TO W-IRR.
           IF  ZERO = WA-IU OR WA-IAR
               GO TO ALL-020
           END-IF
           MOVE WA-IU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WA-IAR / W-UD) * 100.
           COMPUTE W-IRR ROUNDED = W-ALL * 1.
       ALL-020.
           MOVE ZERO TO W-WRR.
           IF  ZERO = WA-WU OR WA-WAR
               GO TO ALL-030
           END-IF
           MOVE WA-WU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WA-WAR / W-UD) * 100.
           COMPUTE W-WRR ROUNDED = W-ALL * 1.
       ALL-030.
           MOVE ZERO TO W-KRR.
           IF  ZERO = WA-KU OR WA-KAR
               GO TO ALL-040
           END-IF
           MOVE WA-KU TO W-UD.
           IF  W-UD < ZERO
               COMPUTE W-UD = W-UD * -1
           END-IF
           COMPUTE W-ALL ROUNDED = (WA-KAR / W-UD) * 100.
           COMPUTE W-KRR ROUNDED = W-ALL * 1.
       ALL-040.
           IF  JS-SIGN = 1
               INITIALIZE EXL-R
               MOVE SPACE TO EXL-TNAME
               MOVE "　【　　総　合　計　　】" TO EXL-TNAME
               MOVE WA-U TO EXL-U
               MOVE WA-UG TO EXL-UG
               MOVE WA-AR TO EXL-AR
               MOVE W-RR TO EXL-RR
               MOVE WA-IU TO EXL-IU
               MOVE WA-IUG TO EXL-IUG
               MOVE WA-IAR TO EXL-IAR
               MOVE W-IRR TO EXL-IRR
               MOVE WA-WU TO EXL-WU
               MOVE WA-WUG TO EXL-WUG
               MOVE WA-WAR TO EXL-WAR
               MOVE W-WRR TO EXL-WRR
               MOVE WA-KU TO EXL-KU
               MOVE WA-KUG TO EXL-KUG
               MOVE WA-KAR TO EXL-KAR
               MOVE W-KRR TO EXL-KRR
      *               WRITE EXL-R
      *//////////////
               CALL "DB_Insert" USING
                EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET
               GO TO ALL-EX
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　【　　総　合　計　　】　　　　　　" TO P-TNAME.
           MOVE WA-U TO P-U.
           MOVE WA-AR TO P-AR.
           MOVE W-RR TO P-RR.
           IF (WA-IU NOT = ZERO) OR (W-IRR NOT = ZERO)
               MOVE WA-IU TO P-IU
               MOVE W-IRR TO P-IRR
           END-IF
           IF (WA-WU NOT = ZERO) OR (W-WRR NOT = ZERO)
               MOVE WA-WU TO P-WU
               MOVE W-WRR TO P-WRR
           END-IF
           IF (WA-KU NOT = ZERO) OR (W-KRR NOT = ZERO)
               MOVE WA-KU TO P-KU
               MOVE W-KRR TO P-KRR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       ALL-EX.
           EXIT.
