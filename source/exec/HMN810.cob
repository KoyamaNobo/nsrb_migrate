       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN810.
      *********************************************************
      *    PROGRAM         :  棚卸表　（決算単価入力用）      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/16                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
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
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　履物棚卸表　（決算単価入力用）　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "コード　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "振替単価".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "決算単価".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(024).
           02  P-NAD   REDEFINES P-NAME.
             03  F            PIC  N(015).
             03  P-BRN        PIC  N(003).
             03  F            PIC  N(006).
           02  P-SU           PIC ----,---,--9.
           02  P-FT           PIC ----,--9.
           02  P-KIN          PIC ----,---,---,--9.
           02  F              PIC  X(008).
       01  W-D.
           02  W-BC1          PIC  9(002).
           02  W-BMNO         PIC  9(001).
           02  W-BC3          PIC  9(002).
           02  W-HCD          PIC  9(006).
           02  CNT            PIC  9(001).
           02  W-SU           PIC S9(006).
       01  WH-D.
           02  WH-SU          PIC S9(006).
           02  WH-KIN         PIC S9(008).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-KIN         PIC S9(010).
       01  WG-D.
           02  WG-SU          PIC S9(007).
           02  WG-KIN         PIC S9(010).
       01  WS-D.
           02  WS-SU          PIC S9(007).
           02  WS-KIN         PIC S9(010).
       01  WA-D.
           02  WA-SU          PIC S9(007).
           02  WA-KIN         PIC S9(010).
       01  WN-D.
             02  WN-AD    OCCURS   5.
               03  WN-SU      PIC S9(007).
               03  WN-KIN     PIC S9(010).
       01  W-PAGE             PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  HHTW-F
       01  HHTW-F_HMN810.
           02  HHTW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HHTW-F_LNAME   PIC  X(013) VALUE "HHTW-F_HMN810".
           02  F              PIC  X(001).
           02  HHTW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HHTW-F_SORT    PIC  X(100) VALUE SPACE.
           02  HHTW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HHTW-F_RES     USAGE  POINTER.
       01  HHTW-R.
           02  F              PIC  X(006).
           02  HHTW-HCD       PIC  9(006).
           02  F              PIC  X(191).
           02  HHTW-ATSU.
             03  HHTW-TSUD  OCCURS  10.
               04  HHTW-TSU   PIC S9(006)  COMP-3.
           02  HHTW-BC1       PIC  9(002).
           02  HHTW-BC2.
             03  HHTW-BC21    PIC  9(001).
             03  HHTW-BC22    PIC  9(001).
           02  HHTW-BC3       PIC  9(002).
           02  HHTW-BMNO      PIC  9(001).
           02  F              PIC  X(006).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　履物棚卸表　（決算単価入力用）　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  02E-ME2 PIC  X(007).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  9(006).
             03  E-STAT  PIC  X(002).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "69" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "69" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" " " "24" "0" "34" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME2" "X" "24" "15" "27" " " "E-ME2" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME2" "X" "24" "29" "7" "01E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME2" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "45" "6" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HHTW-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO HHTW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTW-F_PNAME1 " " BY REFERENCE HHTW-F_IDLST "0".
       M-10.
      *           READ HHTW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTW-F_PNAME1 BY REFERENCE HHTW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HHTW-F_IDLST HHTW-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           COMPUTE W-SU = HHTW-TSU(01) + HHTW-TSU(02) + HHTW-TSU(03)
                        + HHTW-TSU(04) + HHTW-TSU(05) + HHTW-TSU(06)
                        + HHTW-TSU(07) + HHTW-TSU(08) + HHTW-TSU(09)
                        + HHTW-TSU(10).
           IF  W-SU = ZERO
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D WN-D.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE HHTW-BC3 TO W-BC3.
           MOVE ZERO TO WS-D.
       M-20.
           MOVE HHTW-BMNO TO W-BMNO.
           MOVE ZERO TO WG-D.
       M-25.
           MOVE HHTW-BC1 TO W-BC1.
           MOVE ZERO TO WT-D.
       M-30.
           MOVE HHTW-HCD TO W-HCD.
           MOVE ZERO TO WH-D.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　マスター　なし　　" TO HI-NAME
           END-IF.
       M-35.
           ADD W-SU TO WH-SU.
       M-40.
      *           READ HHTW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HHTW-F_PNAME1 BY REFERENCE HHTW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           COMPUTE W-SU = HHTW-TSU(01) + HHTW-TSU(02) + HHTW-TSU(03)
                        + HHTW-TSU(04) + HHTW-TSU(05) + HHTW-TSU(06)
                        + HHTW-TSU(07) + HHTW-TSU(08) + HHTW-TSU(09)
                        + HHTW-TSU(10).
           IF  W-SU = ZERO
               GO TO M-40
           END-IF
           IF  W-BC3 NOT = HHTW-BC3
               GO TO M-55
           END-IF
           IF  W-BMNO NOT = HHTW-BMNO
               GO TO M-50
           END-IF
           IF  W-BC1 NOT = HHTW-BC1
               GO TO M-45
           END-IF
           IF  W-HCD = HHTW-HCD
               GO TO M-35
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-30.
       M-45.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           GO TO M-25.
       M-50.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           GO TO M-20.
       M-55.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           GO TO M-15.
       M-60.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-45.
           PERFORM S-50 THRU S-55.
           PERFORM S-60 THRU S-65.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HHTW-F_IDLST HHTW-F_PNAME1.
      *
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 53
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE ZERO TO CNT.
       M-65.
           ADD 1 TO CNT.
           IF  CNT = 6
               GO TO M-95
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE SPACE TO HKB-KEY.
           MOVE "16" TO HKB-NO.
           IF  CNT = 1
               MOVE 22 TO HKB-BMC
           END-IF
           IF  CNT = 2
               MOVE 26 TO HKB-BMC
           END-IF
           IF  CNT = 3
               MOVE 29 TO HKB-BMC
           END-IF
           IF  CNT = 4
               MOVE 23 TO HKB-BMC
           END-IF
           IF  CNT = 5
               MOVE 24 TO HKB-BMC
           END-IF
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-BMN
           END-IF
           MOVE HKB-BMN TO P-BRN.
           MOVE WN-SU(CNT) TO P-SU.
           MOVE WN-KIN(CNT) TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-65.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
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
           COMPUTE WH-KIN = WH-SU * HI-FT.
      *
           MOVE SPACE TO W-P.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE WH-SU TO P-SU.
           MOVE HI-FT TO P-FT.
           MOVE WH-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WH-SU TO WT-SU WN-SU(W-BMNO).
           ADD WH-KIN TO WT-KIN WN-KIN(W-BMNO).
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　　　　　　　（　計　）" TO P-NAME.
           MOVE WT-SU TO P-SU.
           MOVE WT-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-SU TO WG-SU.
           ADD WT-KIN TO WG-KIN.
       S-35.
           EXIT.
       S-40.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　　　　　［　小　計　］" TO P-NAME.
           MOVE WG-SU TO P-SU.
           MOVE WG-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WG-SU TO WS-SU.
           ADD WG-KIN TO WS-KIN.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　　　　　［　合　計　］" TO P-NAME.
           MOVE WS-SU TO P-SU.
           MOVE WS-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-55.
           EXIT.
       S-60.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE "　【　総　合　計　】　　" TO P-NAME.
           MOVE WA-SU TO P-SU.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-65.
           EXIT.
