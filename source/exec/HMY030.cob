       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMY030.
      *********************************************************
      *    PROGRAM         :  担当得意先別売上値引伝票合計表  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
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
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-SNG          PIC 99/99.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(001) VALUE "～".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-SNG          PIC 99/99.
           02  F              PIC  N(023) VALUE
                "　担当得意先別　売上値引伝票　合計表　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "伝区".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(006) VALUE "(5%)  ".
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(004) VALUE "(3%)".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TNC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-DNO          PIC  9(006).
           02  F              PIC  X(002).
           02  P-DC           PIC  9(001).
           02  P-KIN          PIC ------,---,--9.
           02  P-SHZ5         PIC ----,---,--9.
           02  P-SHZ3         PIC ----,---,--9.
       01  W-DATA.
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-TCDC         PIC  9(001).
           02  W-TNCC         PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-SENG.
             03  W-SNG        PIC  9(004).
             03  W-SNGD  REDEFINES W-SNG.
               04  W-SNEN     PIC  9(002).
               04  W-SGET     PIC  9(002).
             03  W-ENG        PIC  9(004).
             03  W-ENGD  REDEFINES W-ENG.
               04  W-ENEN     PIC  9(002).
               04  W-EGET     PIC  9(002).
           02  W-SED.
             03  W-STNC       PIC  9(002).
             03  W-ETNC       PIC  9(002) VALUE 99.
           02  W-DMM          PIC  9(001).
       01  WN-D.
           02  WN-KIN         PIC S9(009).
           02  WN-SHZ5        PIC S9(008).
           02  WN-SHZ3        PIC S9(008).
       01  WT-D.
           02  WT-KIN         PIC S9(009).
           02  WT-SHZ5        PIC S9(008).
           02  WT-SHZ3        PIC S9(008).
       01  WS-D.
           02  WS-KIN         PIC S9(009).
           02  WS-SHZ5        PIC S9(008).
           02  WS-SHZ3        PIC S9(008).
       01  WA-D.
           02  WA-KIN         PIC S9(009).
           02  WA-SHZ5        PIC S9(008).
           02  WA-SHZ3        PIC S9(008).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
       01  HUND-F_HMY030.
           02  HUND-F_PNAME1  PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  HUND-F_LNAME   PIC  X(013)  VALUE "HUND-F_HMY030".
           02  F              PIC  X(001).
           02  HUND-F_KEY1    PIC  X(100)  VALUE SPACE.
           02  HUND-F_KEY2    PIC  X(100)  VALUE SPACE.
           02  HUND-F_SORT    PIC  X(100)  VALUE SPACE.
           02  HUND-F_IDLST   PIC  X(100)  VALUE SPACE.
           02  HUND-F_RES     USAGE  POINTER.
       01  HUND-R.
           02  F              PIC  9(002).
           02  HUND-DATE      PIC  9(006).
           02  HUND-DNO       PIC  9(006).
           02  HUND-TCD       PIC  9(004).
           02  HUND-SHZ       PIC S9(006).
           02  HUND-KIN       PIC S9(008).
           02  F              PIC  X(001).
           02  HUND-DC        PIC  9(001).
           02  HUND-ZC        PIC  9(001).
           02  HUND-TNC.
             03  HUND-TNC1    PIC  9(001).
             03  HUND-TNC2    PIC  9(001).
           02  HUND-UNC       PIC  9(001).
           02  F              PIC  X(026).
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　担当得意先別売上値引伝票合計表　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(026) VALUE
                "'  年   月  ～  '  年   月".
           02  FILLER  PIC  X(020) VALUE
                "担当者ｺｰﾄﾞ  00 ～ 99".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STNC     PIC  9(002).
             03  A-ETNC     PIC  9(002).
           02  A-DMM      PIC  9(001).
       01  C-DSP.
           02  D-SENG.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC Z9.
       01  C-ERR.
           02  FILLER.
             03  E-ME1      PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98     PIC  X(005) VALUE X"1B4A05".
             03  E-ME99     PIC  X(005) VALUE X"1B4205".
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
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "418" " " " " RETURNING RESU.
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
            "08C-MID" "X" "13" "22" "26" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "16" "25" "20" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "24" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "16" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STNC" "9" "16" "37" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETNC" "9" "16" "43" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "41" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SENG" " " "13" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SENG" "9" "13" "23" "2" " " "D-SENG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SENG" BY REFERENCE W-SNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SENG" "Z9" "13" "28" "2" "01D-SENG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SENG" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SENG" "9" "13" "39" "2" "02D-SENG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SENG" BY REFERENCE W-ENEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SENG" "Z9" "13" "44" "2" "03D-SENG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-SENG" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
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
      *
       M-05.
           COPY LIBCPR.
           MOVE D-SSNG TO W-SNG.
           MOVE D-ESNG TO W-ENG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SENG" D-SENG "p" 
                                  RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
           IF  W-STNC > W-ETNC
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
           IF  W-DMM = 9
               GO TO M-10
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HUND-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HUND-F_PNAME1 " " BY REFERENCE HUND-F_IDLST "0".
       M-25.
      *           READ HUND-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HUND-F_PNAME1 BY REFERENCE HUND-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HUND-F_IDLST HUND-F_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  HUND-TNC < W-STNC
               GO TO M-25
           END-IF.
           IF  HUND-TNC > W-ETNC
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HUND-F_IDLST HUND-F_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D W-PAGE.
           PERFORM S-10 THRU S-15.
       M-30.
           MOVE HUND-TNC1 TO W-TNC1.
           MOVE ZERO TO WS-D.
       M-35.
           MOVE HUND-TNC2 TO W-TNC2.
           MOVE ZERO TO WT-D W-TNCC.
       M-40.
           MOVE HUND-TCD TO W-TCD.
           MOVE HUND-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO T-NAME
           END-IF.
           MOVE ZERO TO WN-D W-TCDC.
       M-45.
           PERFORM S-20 THRU S-25.
       M-50.
      *           READ HUND-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HUND-F_PNAME1 BY REFERENCE HUND-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  HUND-TNC > W-ETNC
               GO TO M-90
           END-IF.
           IF  HUND-TNC1 NOT = W-TNC1
               GO TO M-60
           END-IF.
           IF  HUND-TNC2 NOT = W-TNC2
               GO TO M-55
           END-IF.
           IF  HUND-TCD = W-TCD
               GO TO M-45
           END-IF.
           PERFORM S-30 THRU S-35.
           GO TO M-40.
       M-55.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-35.
       M-60.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           GO TO M-30.
       M-90.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           PERFORM S-60 THRU S-65.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUND-F_IDLST HUND-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
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
           MOVE SPACE TO P-NAME.
           IF  W-TNCC = 0
               MOVE 1 TO W-TNCC
               MOVE W-TNC TO P-TNC
           END-IF.
           IF  W-TCDC = 0
               MOVE 1 TO W-TCDC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NAME
           END-IF.
           MOVE HUND-DATE TO P-DATE.
           MOVE HUND-DNO TO P-DNO.
           IF  HUND-UNC = 0
               MOVE HUND-DC TO P-DC
           ELSE
               MOVE 9 TO P-DC
           END-IF.
           MOVE HUND-KIN TO P-KIN.
           IF  HUND-ZC = 0
               MOVE HUND-SHZ TO P-SHZ5
           END-IF.
           IF  HUND-ZC = 3
               MOVE HUND-SHZ TO P-SHZ3
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TNC TO P-TNC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD HUND-KIN TO WN-KIN.
           IF  HUND-ZC = 0
               ADD HUND-SHZ TO WN-SHZ5
           END-IF.
           IF  HUND-ZC = 3
               ADD HUND-SHZ TO WN-SHZ3
           END-IF.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　　　　　　　（　計　）　" TO P-NAME.
           MOVE WN-KIN TO P-KIN.
           MOVE WN-SHZ5 TO P-SHZ5.
           MOVE WN-SHZ3 TO P-SHZ3.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WN-KIN TO WT-KIN.
           ADD WN-SHZ5 TO WT-SHZ5.
           ADD WN-SHZ3 TO WT-SHZ3.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　　　　＜　小　計　＞　" TO P-NAME.
           MOVE WT-KIN TO P-KIN.
           MOVE WT-SHZ5 TO P-SHZ5.
           MOVE WT-SHZ3 TO P-SHZ3.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-KIN TO WS-KIN.
           ADD WT-SHZ5 TO WS-SHZ5.
           ADD WT-SHZ3 TO WS-SHZ3.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　［　合　計　］　　　" TO P-NAME.
           MOVE WS-KIN TO P-KIN.
           MOVE WS-SHZ5 TO P-SHZ5.
           MOVE WS-SHZ3 TO P-SHZ3.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TNC TO P-TNC
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-KIN TO WA-KIN.
           ADD WS-SHZ5 TO WA-SHZ5.
           ADD WS-SHZ3 TO WA-SHZ3.
       S-55.
           EXIT.
       S-60.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　【　総　合　計　】　　　" TO P-NAME.
           MOVE WA-KIN TO P-KIN.
           MOVE WA-SHZ5 TO P-SHZ5.
           MOVE WA-SHZ3 TO P-SHZ3.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-65.
           EXIT.
