       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY380.
      **************************************************************
      *    PROGRAM         :  教育得意先品種別年間売上粗利集計表   *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  ******                               *
      *        変更　　　  :  62/05/19                             *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
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
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-SG           PIC Z9.
           02  F              PIC  N(004) VALUE "月　～　".
           02  H-EN           PIC 99.
           02  F              PIC  N(001) VALUE "年".
           02  H-EG           PIC Z9.
           02  F              PIC  N(021) VALUE
                "月　教育シューズ　得意先別　売上粗利集計表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(112) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上数量".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上原価".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上粗利".
           02  F              PIC  N(004) VALUE "　利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P1.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(088).
       01  W-P2.
           02  F              PIC  X(016).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  P-SU           PIC ---,---,--9.
           02  P-UT           PIC ---,--9.
           02  P-UK           PIC --,---,---,--9.
           02  P-GT           PIC ---,--9.
           02  P-GK           PIC --,---,---,--9.
           02  P-AR           PIC -----,---,--9.
           02  P-RR           PIC ----9.9.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-HCD1         PIC  9(004).
           02  W-AR           PIC S9(009).
           02  W-UT           PIC S9(005).
           02  W-GT           PIC S9(005).
           02  W-RR           PIC S9(003)V9(01).
           02  W-KIN          PIC S9(010).
           02  CNT.
             03  CNT1         PIC  9(003).
             03  CNT2         PIC  9(003).
           02  CHK            PIC  9(001).
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  W-SNG          PIC  9(004).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-ENG          PIC  9(004).
           02  W-ENGD  REDEFINES W-ENG.
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-DMM          PIC  9(001).
       01  W-D.
           02  WD-SU          PIC S9(008).
           02  WD-UK          PIC S9(010).
           02  WD-GK          PIC S9(010).
           02  WD-AR          PIC S9(009).
       01  WN-D.
           02  WN-SU          PIC S9(008).
           02  WN-UK          PIC S9(010).
           02  WN-GK          PIC S9(010).
           02  WN-AR          PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(008).
           02  WT-UK          PIC S9(010).
           02  WT-GK          PIC S9(010).
           02  WT-AR          PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(008).
           02  WA-UK          PIC S9(010).
           02  WA-GK          PIC S9(010).
           02  WA-AR          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *       FD  SSR-YF
       01  SSR-YF_HMY380.
           02  SSR-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  SSR-YF_LNAME    PIC  X(013)  VALUE "SSR-YF_HMY380".
           02  F               PIC  X(001).
           02  SSR-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  SSR-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  SSR-YF_RES      USAGE  POINTER.
       01  SSR-YR.
           02  Y-TCD          PIC  9(004).
           02  Y-HCD.
             03  Y-HCD1       PIC  9(004).
             03  Y-HCD2       PIC  9(002).
           02  Y-SU           PIC S9(007).
           02  Y-UK           PIC S9(010).
           02  Y-GK           PIC S9(010).
           02  Y-TC1.
             03  Y-TC11       PIC  9(001).
             03  Y-TC12       PIC  9(001).
           02  Y-TC2          PIC  9(002).
           02  Y-BC1          PIC  9(002).
           02  Y-BC2          PIC  9(002).
           02  Y-BC3          PIC  9(002).
           02  Y-TFK          PIC  9(002).
           02  Y-NG           PIC  9(006).
           02  F              PIC  X(073).
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
               "＊＊＊　教育得意先品種別　年間売上集計表　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
               "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
               "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
               "'  年   月  ～  '  年   月".
           02  FILLER  PIC  X(022) VALUE
               "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
             03  FILLER  PIC 99.
             03  FILLER  PIC Z9.
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
            "C-MID" " " "0" "0" "384" " " " " RETURNING RESU.
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
            "08C-MID" "X" "15" "21" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID" "X" "20" "23" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-NG" " " "15" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "01D-NG" "99" "15" "22" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-NG" "Z9" "15" "27" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-NG" "99" "15" "38" "2" "02D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NG" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-NG" "Z9" "15" "43" "2" "03D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-NG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           MOVE D-SPNG TO W-SNG.
           MOVE D-EPNG TO W-ENG.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" 
                                         RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10.
           IF  W-DMM = 9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN.
           IF  W-DMM NOT = 1
               GO TO M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SSR-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SSR-YF_PNAME1 " " BY REFERENCE SSR-YF_IDLST "0".
       M-15.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-15.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE W-SNEN TO H-SN.
           MOVE W-SGET TO H-SG.
           MOVE W-ENEN TO H-EN.
           MOVE W-EGET TO H-EG.
           MOVE DATE-02R TO H-DATE.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-25.
           MOVE Y-TCD TO W-TCD.
           MOVE ZERO TO WT-D CNT1.
           MOVE SPACE TO W-P1.
           MOVE W-TCD TO P-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　得意先マスター　無し　＊＊" TO T-NAME.
           MOVE T-NAME TO P-TNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-30.
           MOVE Y-HCD1 TO W-HCD1.
           MOVE ZERO TO WN-D CNT2.
       M-35.
           MOVE SPACE TO W-P2.
           MOVE Y-HCD TO P-HCD.
           MOVE Y-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＨＩ－Ｍ　無し　＊＊　" TO HI-NAME.
           MOVE HI-NAME TO P-HNA.
           MOVE ZERO TO W-UT W-GT.
           IF  Y-SU NOT = ZERO
               IF  Y-UK NOT = ZERO
                   COMPUTE W-UT ROUNDED = Y-UK / Y-SU.
           IF  Y-SU NOT = ZERO
               IF  Y-GK NOT = ZERO
                   COMPUTE W-GT ROUNDED = Y-GK / Y-SU.
           MOVE W-UT TO P-UT.
           MOVE W-GT TO P-GT.
           COMPUTE W-AR = Y-UK - Y-GK.
           MOVE ZERO TO W-D CHK.
           MOVE Y-SU TO WD-SU.
           MOVE Y-UK TO WD-UK.
           MOVE Y-GK TO WD-GK.
           MOVE W-AR TO WD-AR.
           PERFORM S-60 THRU S-80.
           ADD Y-SU TO WN-SU.
           ADD Y-UK TO WN-UK.
           ADD Y-GK TO WN-GK.
           ADD W-AR TO WN-AR.
           ADD 1 TO CNT1 CNT2.
       M-40.
      *           READ SSR-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSR-YF_PNAME1 BY REFERENCE SSR-YR " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80.
           IF  ZERO = Y-SU AND Y-UK AND Y-GK
               GO TO M-40.
           IF  Y-TCD NOT = W-TCD
               GO TO M-45.
           IF  Y-HCD1 = W-HCD1
               GO TO M-35.
           PERFORM S-20 THRU S-30.
           GO TO M-30.
       M-45.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-45.
           GO TO M-25.
       M-80.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-45.
           MOVE SPACE TO W-P2.
           MOVE "　【　　総　合　計　　】　　　　　　　　" TO P-HNA.
           MOVE ZERO TO W-D.
           MOVE WA-D TO W-D.
           MOVE 5 TO CHK.
           PERFORM S-60 THRU S-80.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSR-YF_IDLST SSR-YF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
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
           IF  CNT2 = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-25.
           MOVE SPACE TO W-P2.
           MOVE "　　　　　　　　　　　　（　計　）　　　" TO P-HNA.
           MOVE ZERO TO W-D CHK.
           MOVE WN-D TO W-D.
           PERFORM S-60 THRU S-80.
       S-25.
           ADD WN-SU TO WT-SU.
           ADD WN-UK TO WT-UK.
           ADD WN-GK TO WT-GK.
           ADD WN-AR TO WT-AR.
       S-30.
           EXIT.
       S-35.
           IF  CNT1 = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-40.
           MOVE SPACE TO W-P2.
           MOVE "　　　［　　合　　計　　］　　　　　　　" TO P-HNA.
           MOVE ZERO TO W-D CHK.
           MOVE WT-D TO W-D.
           PERFORM S-60 THRU S-80.
       S-40.
           ADD WT-SU TO WA-SU.
           ADD WT-UK TO WA-UK.
           ADD WT-GK TO WA-GK.
           ADD WT-AR TO WA-AR.
       S-45.
           EXIT.
       S-60.
           MOVE WD-SU TO P-SU.
           MOVE WD-UK TO P-UK.
           MOVE WD-GK TO P-GK.
           MOVE WD-AR TO P-AR.
           MOVE ZERO TO W-RR.
           MOVE WD-UK TO W-KIN.
           IF  WD-AR = ZERO
               GO TO S-70.
           IF  WD-UK = ZERO
               GO TO S-65.
           IF  W-KIN < ZERO
               COMPUTE W-KIN = W-KIN * -1.
           COMPUTE W-RR ROUNDED = (WD-AR * 100) / W-KIN.
           GO TO S-70.
       S-65.
           IF  WD-AR > ZERO
               MOVE 100 TO W-RR
           ELSE
               MOVE -100 TO W-RR.
       S-70.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               GO TO S-75.
           PERFORM S-05 THRU S-15.
           IF  CHK = ZERO
               MOVE SPACE TO SP-R
               MOVE W-P1 TO SP-R
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-75.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-80.
           EXIT.
