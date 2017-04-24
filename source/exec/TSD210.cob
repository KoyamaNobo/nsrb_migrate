       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD210.
      ****************************************
      *****     受手　異動入力リスト     *****
      ****************************************
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
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　受取手形　異動データ入力　プルーフリスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(040) VALUE
                " 異動日  区分 ｺｰﾄﾞ 処理銀行名     振出日".
           02  F              PIC  X(035) VALUE
                "　　引受日　手形NO　 取  引  先　名".
           02  F              PIC  X(050) VALUE
                "                            満期日          金　額".
       01  W-P.
           02  P-K15          PIC  X(005).
           02  P-IDO          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-TSC          PIC  9(002).
           02  F              PIC  X(002).
           02  P-YBC          PIC  9(004).
           02  F              PIC  X(001).
           02  P-BNA          PIC  N(008).
           02  F              PIC  X(002).
           02  P-FDD          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-HKD          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-KEY          PIC  9(004).
           02  F              PIC  X(002).
           02  P-URC          PIC  X(001).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(002).
           02  P-UTI.
             03  P-MND        PIC 99/99/99.
             03  P-KIN        PIC ---,---,---,--9.
           02  P-K20          PIC  X(005).
       01  W-DATA.
           02  W-CHK          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-IDO          PIC  9(006).
           02  W-TSC          PIC  9(002).
           02  W-YBC          PIC  9(004).
           02  W-C.
             03  W-C1         PIC  9(001).
             03  W-C2         PIC  9(001).
             03  W-C3         PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIBANK.
           COPY LIUKET.
           COPY LSPF.
      *FD  IDOU-F
       01  IDOU-F_TSD210.
           02  IDOU-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  IDOU-F_LNAME   PIC  X(013) VALUE "IDOU-F_TSD210".
           02  F              PIC  X(001).
           02  IDOU-F_KEY1    PIC  X(100) VALUE SPACE.
           02  IDOU-F_SORT    PIC  X(100) VALUE SPACE.
           02  IDOU-F_IDLST   PIC  X(100) VALUE SPACE.
           02  IDOU-F_RES     USAGE  POINTER.
       01  IDOU-R.
           02  IW-KEY         PIC  X(004).
           02  IW-TSC         PIC  9(002).
           02  IW-IDO         PIC  9(006).
           02  IW-FDD         PIC  9(006).
           02  IW-HKD         PIC  9(006).
           02  IW-YBC         PIC  9(004).
           02  IW-SNI         PIC  9(004).
           02  F              PIC  X(009).
           02  IW-PC          PIC  9(001).
           02  F              PIC  X(022).
       77  F                  PIC  X(001).
      *FD  TIDM
       01  TIDM_TSD210.
           02  TIDM_PNAME1    PIC  X(004) VALUE "TIDM".
           02  F              PIC  X(001).
           02  TIDM_LNAME     PIC  X(011) VALUE "TIDM_TSD210".
           02  F              PIC  X(001).
           02  TIDM_KEY1      PIC  X(100) VALUE SPACE.
           02  TIDM_SORT      PIC  X(100) VALUE SPACE.
           02  TIDM_IDLST     PIC  X(100) VALUE SPACE.
           02  TIDM_RES       USAGE  POINTER.
       01  TID-R.
           02  TID-KEY        PIC  X(004).
           02  TID-TSC        PIC  9(002).
           02  TID-IDO        PIC  9(006).
           02  TID-FDD        PIC  9(006).
           02  TID-HKD        PIC  9(006).
           02  TID-YBC        PIC  9(004).
           02  TID-SNI        PIC  9(004).
           02  F              PIC  X(009).
           02  TID-PC         PIC  9(001).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　受取手形異動データ　入力リスト　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(048) VALUE
                "全件作表=1  追加･修正分作表=5  作表しない=9 ... ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-CHK   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "**  TIDM ﾅｼ  **".
             03  E-ME2   PIC  X(024) VALUE
                  "**  TIDM REWRITE ｴﾗｰ  **".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
            "C-MID" " " "0" "0" "420" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "50" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "16" "48" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "30" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "15" "63" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "47" "1" "A-CHK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "135" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "135" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "10" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "10" "24" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           MOVE ZERO TO W-DATA.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-CHK NOT = 1 AND 5 AND 9
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
           IF  W-CHK = 9
               GO TO M-95
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO IDOU-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" IDOU-F_PNAME1 " " BY REFERENCE IDOU-F_IDLST "0".
       M-20.
      *           READ IDOU-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" IDOU-F_PNAME1 BY REFERENCE IDOU-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE IDOU-F_IDLST IDOU-F_PNAME1
               GO TO M-95
           END-IF
           IF  W-CHK = 5
               IF  IW-PC NOT = 0
                   GO TO M-20
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" UKET-M_PNAME1 "SHARED" BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "I-O" TIDM_PNAME1 " " BY REFERENCE TIDM_IDLST "1"
            "TID-KEY" BY REFERENCE TID-KEY.
           CALL "PR_Open" RETURNING RESP.
           COPY LIBCPR.
           MOVE DATE-04R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-25.
           MOVE IW-IDO TO W-IDO.
           MOVE ZERO TO W-C.
       M-30.
           MOVE IW-TSC TO W-TSC.
           MOVE ZERO TO W-C2 W-C3.
       M-35.
           MOVE IW-YBC TO W-YBC.
           MOVE ZERO TO W-C3.
           IF  W-YBC = ZERO
               GO TO M-40
           END-IF
           MOVE W-YBC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
           END-IF.
       M-40.
           MOVE IW-KEY TO UT-KEY.
      *           READ UKET-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO UKET-R
           END-IF
           MOVE UT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　　得意先　無し　　＊＊" TO T-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-K15.
           MOVE SPACE TO P-BNA P-TNA.
           MOVE W-20K TO P-K20.
           IF  W-C1 = ZERO
               MOVE 1 TO W-C1
               MOVE W-IDO TO P-IDO
           END-IF
           IF  W-C2 = ZERO
               MOVE 1 TO W-C2
               MOVE W-TSC TO P-TSC
           END-IF
           IF  W-C3 = ZERO
               MOVE 1 TO W-C3
               IF  W-YBC NOT = ZERO
                   MOVE W-YBC TO P-YBC
                   MOVE B-BNA TO P-BNA
               END-IF
           END-IF
           MOVE IW-KEY TO P-KEY.
           IF  IW-FDD NOT = ZERO
               MOVE IW-FDD TO P-FDD
           END-IF
           IF  IW-HKD NOT = ZERO
               MOVE IW-HKD TO P-HKD
           END-IF
           IF  UT-KIN NOT = ZERO
               IF  UT-FDM NOT = SPACE
                   MOVE "*" TO P-URC
               END-IF
           END-IF
           MOVE T-NAME TO P-TNA.
           IF  UT-KIN NOT = ZERO
               MOVE UT-MKD TO P-MND
               MOVE UT-KIN TO P-KIN
           ELSE
               MOVE " **  UKETM ﾅｼ  **      " TO P-UTI
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO M-45
           END-IF
           PERFORM S-05 THRU S-15.
           MOVE W-IDO TO P-IDO.
           MOVE W-TSC TO P-TSC.
           IF  W-YBC NOT = ZERO
               MOVE W-YBC TO P-YBC
               MOVE B-BNA TO P-BNA.
       M-45.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           IF  IW-PC = 1
               GO TO M-50
           END-IF
           MOVE IW-KEY TO TID-KEY.
      *           READ TIDM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TIDM_PNAME1 BY REFERENCE TID-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE 1 TO TID-PC.
      *           REWRITE TID-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TIDM_PNAME1 TIDM_LNAME TID-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-50.
      *           READ IDOU-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" IDOU-F_PNAME1 BY REFERENCE IDOU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-CHK = 5
               IF  IW-PC NOT = 0
                   GO TO M-50
               END-IF
           END-IF
           IF  IW-IDO NOT = W-IDO
               GO TO M-25
           END-IF
           IF  IW-TSC NOT = W-TSC
               GO TO M-30
           END-IF
           IF  IW-YBC NOT = W-YBC
               GO TO M-35
           END-IF
           GO TO M-40.
       M-90.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TIDM_IDLST TIDM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE IDOU-F_IDLST IDOU-F_PNAME1.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
