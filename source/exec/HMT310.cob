       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT310.
      ******************************************************
      *****     担当得意先別　売上粗利集計　問合せ     *****
      ******************************************************
       AUTHOR. T-FUJII.
       DATE-WRITTEN. 1974-07-17.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
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
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(019) VALUE
                "＊＊＊　　担当者得意先別　売上粗利集計".
           02  F              PIC  N(012) VALUE
                "プルーフリスト　　＊＊＊".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(003) VALUE "担当者".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "売上原価　　売上粗利".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "利益率".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  F              PIC  X(002).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(004).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(026).
           02  P-UK           PIC -----,---,--9.
           02  P-UG           PIC -----,---,--9.
           02  P-AR           PIC ----,---,--9.
           02  P-RR           PIC -----9.9.
       01  W-D.
           02  W-TC           PIC  9(002).
           02  W-UK           PIC S9(009).
           02  W-UKD          PIC S9(009).
           02  W-AR           PIC S9(008).
           02  W-RR           PIC S9(003)V9(01).
           02  W-DMM          PIC  9(001).
           02  W-CHK.
             03  W-CHK1       PIC  9(001).
             03  W-CHK2       PIC  9(001).
           02  W-DATE.
             03  F            PIC  X(004).
             03  W-PEY        PIC  9(002).
           02  W-PAGE         PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-NA           PIC  N(026).
           02  W-NAME  REDEFINES W-NA.
             03  W-NAME1      PIC  N(013).
             03  W-NAME2      PIC  N(013).
       01  WT-D.
           02  WT-UK          PIC S9(009).
           02  WT-UG          PIC S9(009).
           02  WT-AR          PIC S9(008).
           02  WT-RR          PIC S9(003)V9(01).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSTTM.
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
       01  C-ACP.
           02  A-TC    PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DATE  PIC  Z(002).
           02  D-DATA.
             03  FILLER.
               04  FILLER  PIC  N(013).
             03  FILLER.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(013).
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ----,---,--9 .
               04  FILLER  PIC ---,---,--9 .
               04  FILLER  PIC ---9.9 .
           02  D-TOTAL.
             03  FILLER  PIC  N(013) VALUE
                  "　　　　　［　合　計　］　".
             03  FILLER  PIC ----,---,--9 .
             03  FILLER  PIC ----,---,--9 .
             03  FILLER  PIC ---,---,--9 .
             03  FILLER  PIC ---9.9 .
           02  FILLER.
             03  D-NM    PIC  X(022) VALUE
                  "<  NEXT PAGE   ﾘﾀｰﾝ  >".
             03  D-PM    PIC  X(030) VALUE
                  "<  ｳﾁﾀﾞｼ  ｽﾙ=5 ｼﾅｲ=0   ﾘﾀｰﾝ  >".
             03  D-S     PIC  X(050) VALUE
                  "                                                  ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "**  DATA ﾅｼ  **".
             03  E-ME3   PIC  X(013) VALUE
                  "**  TM ﾅｼ  **".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "3" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TC" "9" "3" "13" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TC" BY REFERENCE W-TC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "53" "1" "A-TC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "268" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATE" "Z" "3" "70" "2" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-DATE" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "0" "0" "97" "D-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" " " "W-L" "0" "26" " " "D-DATA" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-DATA" "N" "W-L" "7" "26" " " "01D-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-DATA" BY REFERENCE W-NAME1 "26" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" " " "W-L PLUS 1" "0" "71" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-DATA" "9" "W-L PLUS 1" "2" "4" " " "02D-DATA"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-DATA" BY REFERENCE TT-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-DATA" "N" "W-L PLUS 1" "7" "26" "0102D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-DATA" BY REFERENCE W-NAME2 "26" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-DATA" "----,---,--9" "W-L PLUS 1" "34" "12"
            "0202D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-DATA" BY REFERENCE W-UK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-DATA" "----,---,--9" "W-L PLUS 1" "47" "12"
            "0302D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0402D-DATA" BY REFERENCE TT-TUG "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-DATA" "---,---,--9" "W-L PLUS 1" "60" "11"
            "0402D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0502D-DATA" BY REFERENCE W-AR "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0602D-DATA" "---9.9" "W-L PLUS 1" "72" "6" "0502D-DATA"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "0602D-DATA" BY REFERENCE W-RR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TOTAL" " " "W-L" "0" "67" "D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TOTAL" "N" "W-L" "7" "26" " " "D-TOTAL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TOTAL" "----,---,--9" "W-L" "34" "12" "01D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TOTAL" BY REFERENCE WT-UK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TOTAL" "----,---,--9" "W-L" "47" "12" "02D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TOTAL" BY REFERENCE WT-UG "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TOTAL" "---,---,--9" "W-L" "60" "11" "03D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-TOTAL" BY REFERENCE WT-AR "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-TOTAL" "---9.9" "W-L" "72" "6" "04D-TOTAL" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-TOTAL" BY REFERENCE WT-RR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "23" "0" "102" "D-TOTAL" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "39" "22" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "23" "31" "30" "D-NM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S" "X" "23" "20" "50" "D-PM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "38" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "38" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "13" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE D-HSD TO W-DATE.
           MOVE DATE-02R TO H-DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0".
           MOVE ZERO TO W-PAGE W-CHK2.
       M-15.
           CALL "SD_Screen_Output" USING "SCHT31" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TC "A-TC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE ZERO TO WT-D.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-CHK1 T-TCD.
       M-20.
      *           READ TT-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           COMPUTE W-UK = TT-TUA - TT-TNB.
           IF  ZERO = W-UK AND TT-TUG
               GO TO M-20
           END-IF
           IF  W-TC = 99
               GO TO M-30
           END-IF
           IF  TT-TNC NOT = W-TC
               GO TO M-20
           END-IF
           MOVE TT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-30.
           MOVE 5 TO W-CHK1.
           PERFORM S-20 THRU S-25.
           IF  W-TC = 99
               GO TO M-45
           END-IF
           PERFORM S-30 THRU S-35.
           MOVE T-NAME TO W-NA.
       M-40.
           ADD 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-DATA" D-DATA "p" RETURNING RESU.
       M-45.
           ADD W-UK TO WT-UK.
           ADD TT-TUG TO WT-UG.
           ADD W-AR TO WT-AR.
           GO TO M-20.
       M-50.
           IF  W-CHK1 = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TT-M_IDLST TT-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0"
               GO TO M-15
           END-IF
           MOVE ZERO TO WT-RR.
           IF  WT-AR = ZERO
               GO TO M-55
           END-IF
           IF  WT-UK = ZERO
               GO TO M-55
           END-IF
           MOVE WT-UK TO W-UKD.
           IF  W-UKD < ZERO
               COMPUTE W-UKD = W-UKD * -1
           END-IF
           COMPUTE WT-RR ROUNDED = (WT-AR * 100) / W-UKD.
       M-55.
           PERFORM S-30 THRU S-35.
           ADD 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Output" USING "D-TOTAL" D-TOTAL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0".
       M-60.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-DMM = 5
               GO TO M-65
           END-IF
           IF  W-DMM NOT = ZERO
               GO TO M-60
           END-IF
           GO TO M-15.
       M-65.
           MOVE ZERO TO W-CHK1.
           IF  W-CHK2 NOT = ZERO
               GO TO M-70
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE 5 TO W-CHK2.
       M-70.
           IF  W-TC = 99
               GO TO M-85
           END-IF.
       M-75.
      *           READ TT-M  AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TT-TNC NOT = W-TC
               GO TO M-75
           END-IF
           COMPUTE W-UK = TT-TUA - TT-TNB.
           IF  ZERO = W-UK AND TT-TUG
               GO TO M-75
           END-IF
           MOVE TT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF.
       M-80.
           PERFORM S-20 THRU S-25.
           MOVE SPACE TO SP-R W-P.
           IF  W-CHK1 = ZERO
               MOVE W-TC TO P-TC
           END-IF
           MOVE TT-KEY TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE W-UK TO P-UK.
           MOVE TT-TUG TO P-UG.
           MOVE W-AR TO P-AR.
           MOVE W-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE 5 TO W-CHK1.
           GO TO M-75.
       M-85.
           MOVE SPACE TO W-P P-NAME.
           MOVE "　　　　　　　　　　［　　合　　計　　］" TO P-NAME.
           MOVE WT-UK TO P-UK.
           MOVE WT-UG TO P-UG.
           MOVE WT-AR TO P-AR.
           MOVE WT-RR TO P-RR.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0".
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           IF  W-CHK2 NOT = ZERO
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
           COMPUTE W-AR = W-UK - TT-TUG.
           MOVE ZERO TO W-RR.
           IF  W-AR = ZERO
               GO TO S-25
           END-IF
           IF  W-UK = ZERO
               GO TO S-25
           END-IF
           MOVE W-UK TO W-UKD.
           IF  W-UKD < ZERO
               COMPUTE W-UKD = W-UKD * -1
           END-IF
           COMPUTE W-RR ROUNDED = (W-AR * 100) / W-UKD.
       S-25.
           EXIT.
       S-30.
           IF  W-L NOT = 21
               GO TO S-35
           END-IF
           CALL "SD_Output" USING "D-S" D-S "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-30
           END-IF
           CALL "SD_Screen_Output" USING "SCHT31" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TC" A-TC "p" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-35.
           EXIT.
