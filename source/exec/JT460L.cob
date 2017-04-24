       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT460L.
      *********************************************************
      *    PROGRAM         :  倉庫品名別　出荷数明細表        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  95/08/09                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM7200.
       OBJECT-COMPUTER. SYSTEM7200.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  N(002) VALUE "【　".
           02  F              PIC  X(001) VALUE "'".
           02  H-SNEN         PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-SGET         PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  H-SPEY         PIC Z9.
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "～".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(001) VALUE "'".
           02  H-ENEN         PIC  9(002).
           02  F              PIC  N(001) VALUE "年".
           02  H-EGET         PIC Z9.
           02  F              PIC  N(001) VALUE "月".
           02  H-EPEY         PIC Z9.
           02  F              PIC  N(001) VALUE "日".
           02  F              PIC  N(002) VALUE "　】".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　倉庫品名別　出荷数明細表　　＊＊＊".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "倉庫名　".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(002) VALUE "SS".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(001) VALUE "S".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(001) VALUE "M".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(001) VALUE "L".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(002) VALUE "LL".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(054) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(008) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(054) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(015) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(054) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
       01  W-P.
           02  P-SOK          PIC  N(006).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ---,--9  OCCURS  10 TIMES.
           02  P-SUT          PIC ----,--9.
       01  W-DATA.
           02  W-SNGP.
             03  F            PIC  9(002).
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-ENGP.
             03  F            PIC  9(002).
             03  W-ENEN       PIC  9(002).
             03  W-EGET       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-SOK          PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-L            PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  CNTD           PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-GC           PIC  9(002).
           02  W-GCD          PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-NC           PIC  9(001).
           02  W-TC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC    OCCURS   4  PIC  9(001).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SU    OCCURS  10  PIC S9(005).
             03  W-SUT        PIC S9(006).
           02  WT-ZCD.
             03  WT-ZC   OCCURS   4  PIC  9(001).
           02  WT-ASUD.
             03  WT-ASU  OCCURS   4.
               04  WT-SU   OCCURS  10  PIC S9(005).
             03  WT-SUT       PIC S9(006).
           02  W-SSD.
             03  W-SS    OCCURS  10  PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY L-JCON.
           COPY LIHIM2.
           COPY LTWK03.
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
                "＊＊＊　　倉庫品名別　出荷数明細表　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "10" "44" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "97" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "97" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK03_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK03_PNAME1 " " BY REFERENCE JT-WK03_IDLST "0".
           MOVE ZERO TO W-SNGP W-ENGP.
       M-10.
      *           READ JT-WK03 AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" JT-WK03_PNAME1 BY REFERENCE W03-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  W-SNGP = ZERO
               MOVE W03-05 TO W-SNGP
           ELSE
               IF  W03-05 < W-SNGP
                   MOVE W03-05 TO W-SNGP
               END-IF
           END-IF
           IF  W03-05 > W-ENGP
               MOVE W03-05 TO W-ENGP
           END-IF
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
           IF  ZERO = W-SNGP AND W-ENGP
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JT-WK03_PNAME1 " " BY REFERENCE JT-WK03_IDLST "0".
       M-20.
      *           READ JT-WK03 AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" JT-WK03_PNAME1 BY REFERENCE W03-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           ACCEPT H-DATE FROM DATE.
           MOVE W-SNEN TO H-SNEN.
           MOVE W-SGET TO H-SGET.
           MOVE W-SPEY TO H-SPEY.
           MOVE W-ENEN TO H-ENEN.
           MOVE W-EGET TO H-EGET.
           MOVE W-EPEY TO H-EPEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-25.
           MOVE W03-07 TO W-SOK.
           MOVE 3 TO JCON3-01.
           MOVE W-SOK TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON3-03
           END-IF
           MOVE ZERO TO WT-ZCD WT-ASUD CHK.
       M-30.
           MOVE W03-09 TO W-HCD.
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "　マスター　なし　" TO HI-NAME
           END-IF
           MOVE ZERO TO W-ZCD W-ASUD.
       M-35.
           PERFORM S-20 THRU S-30.
       M-40.
      *           READ JT-WK03 AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" JT-WK03_PNAME1 BY REFERENCE W03-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W03-07 NOT = W-SOK
               GO TO M-45
           END-IF
           IF  W03-09 = W-HCD
               GO TO M-35
           END-IF
      *
           PERFORM S-35 THRU S-70.
           GO TO M-30.
       M-45.
           PERFORM S-35 THRU S-70.
           PERFORM S-75 THRU S-95.
           GO TO M-25.
       M-90.
           PERFORM S-35 THRU S-70.
           PERFORM S-75 THRU S-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
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
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           IF  W03-10 = 1
               MOVE 4 TO W-SC
           END-IF
           IF  W03-10 = 2
               MOVE 1 TO W-SC
           END-IF
           IF  W03-10 = 3
               MOVE 2 TO W-SC
           END-IF
           IF  W03-10 = 4
               MOVE 3 TO W-SC
           END-IF
           MOVE ZERO TO CNTD.
       S-25.
           ADD 1 TO CNTD.
           IF  CNTD NOT = 11
               ADD W03-1211(CNTD) TO W-SU(W-SC,CNTD)
               ADD W03-1211(CNTD) TO WT-SU(W-SC,CNTD)
               GO TO S-25
           END-IF
           ADD W03-122 TO W-SUT WT-SUT.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO W-SC.
       S-40.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               GO TO S-50
           END-IF
           IF  ZERO = W-SU(W-SC,01) AND W-SU(W-SC,02) AND W-SU(W-SC,03)
                 AND W-SU(W-SC,04) AND W-SU(W-SC,05) AND W-SU(W-SC,06)
                 AND W-SU(W-SC,07) AND W-SU(W-SC,08) AND W-SU(W-SC,09)
                 AND W-SU(W-SC,10)
               GO TO S-40
           END-IF
           MOVE 1 TO W-ZC(W-SC) WT-ZC(W-SC).
           GO TO S-40.
       S-50.
           IF  W-ZCD = ZERO
               GO TO S-70
           END-IF
           MOVE ZERO TO W-SC W-NC.
       S-55.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-70
           END-IF
           IF  W-ZC(W-SC) = 0
               GO TO S-55
           END-IF
           MOVE ZERO TO W-SSD.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SOK P-NAME.
           IF  CHK = 0
               MOVE 9 TO CHK
               MOVE JCON3-03 TO P-SOK
           END-IF
           IF  W-NC = 0
               MOVE 9 TO W-NC
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-NAME
           END-IF
           IF  W-SC = 1
               MOVE HI-SS2 TO W-SSD
               MOVE 2 TO P-SIZ
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF
           IF  W-SC = 2
               MOVE HI-SS3 TO W-SSD
               MOVE 3 TO P-SIZ
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF
           IF  W-SC = 3
               MOVE HI-SS4 TO W-SSD
               MOVE 0 TO W-SS(10)
               MOVE 4 TO P-SIZ
               IF  0 = W-ZC(4)
                   MOVE W-SUT TO P-SUT
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE HI-SS1 TO W-SSD
               MOVE 1 TO P-SIZ
               MOVE W-SUT TO P-SUT
           END-IF
           MOVE ZERO TO CNTD.
       S-60.
           ADD 1 TO CNTD.
           IF  CNTD = 11
               GO TO S-65
           END-IF
           IF  W-SS(CNTD) NOT = 0
               MOVE W-SU(W-SC,CNTD) TO P-SU(CNTD)
           END-IF
           GO TO S-60.
       S-65.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE JCON3-03 TO P-SOK
               MOVE W-HCD TO P-HCD
               MOVE HI-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-55.
       S-70.
           EXIT.
       S-75.
           MOVE ZERO TO W-SC W-NC.
       S-80.
           ADD 1 TO W-SC.
           IF  W-SC = 5
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO S-95
           END-IF
           IF  WT-ZC(W-SC) = 0
               GO TO S-80
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-SOK P-NAME.
           IF  W-NC = 0
               MOVE 9 TO W-NC
               MOVE "　　　　　（　合　計　）　" TO P-NAME
           END-IF
           IF  W-SC = 1
               MOVE 2 TO P-SIZ
               IF  0 = WT-ZC(2) AND WT-ZC(3) AND WT-ZC(4)
                   MOVE WT-SUT TO P-SUT
               END-IF
           END-IF
           IF  W-SC = 2
               MOVE 3 TO P-SIZ
               IF  0 = WT-ZC(3) AND WT-ZC(4)
                   MOVE WT-SUT TO P-SUT
               END-IF
           END-IF
           IF  W-SC = 3
               MOVE 4 TO P-SIZ
               IF  0 = WT-ZC(4)
                   MOVE WT-SUT TO P-SUT
               END-IF
           END-IF
           IF  W-SC = 4
               MOVE 1 TO P-SIZ
               MOVE WT-SUT TO P-SUT
           END-IF
           MOVE ZERO TO CNTD.
       S-85.
           ADD 1 TO CNTD.
           IF  CNTD = 11
               GO TO S-90
           END-IF
           IF  W-SC = 2
               IF  CNTD > 9
                   GO TO S-85
               END-IF
           END-IF
           IF  W-SC = 3
               IF  CNTD > 8
                   GO TO S-85
               END-IF
           END-IF
           IF  WT-SU(W-SC,CNTD) NOT = ZERO
               MOVE WT-SU(W-SC,CNTD) TO P-SU(CNTD)
           END-IF
           GO TO S-85.
       S-90.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE JCON3-03 TO P-SOK
               MOVE "　　　　　（　合　計　）　" TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-80.
       S-95.
           EXIT.
