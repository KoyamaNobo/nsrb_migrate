       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD050.
      ******************************************************
      *****     受取・割引・支払手形　落込みリスト     *****
      ******************************************************
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
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　受手・割引・支払手形　落込みリスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "　項目　".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "落込み日".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "№　".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(018) VALUE
                "取　引　先　名　（　振　出　人　）　".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "銀　行　名　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(008) VALUE "本　支　店　名　".
           02  F              PIC  N(006) VALUE "　受取振出日".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "満期日　".
       01  W-P.
           02  P-TGN          PIC  N(004).
           02  F              PIC  X(001).
           02  P-OKD          PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-NO           PIC  9(004).
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  P-F            PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-R            PIC  X(001).
           02  P-KIN          PIC Z,ZZZ,ZZZ,ZZZ.
           02  P-X            PIC  X(001).
           02  P-KBN          PIC  N(002).
           02  F              PIC  X(001).
           02  P-SBC          PIC  9(004).
           02  F              PIC  X(001).
           02  P-BNA          PIC  N(008).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(008).
           02  F              PIC  X(001).
           02  P-UFD          PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-MAN          PIC 99/99/99.
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-DTC          PIC  9(001).
           02  W-TGN          PIC  N(004).
           02  W-OKD          PIC  9(006).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-SKIN         PIC  9(010).
           02  W-AKIN         PIC  9(010).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LISM.
           COPY LITM.
           COPY LIBANK.
           COPY LSPF.
      *FD  TGO-F
       01  TGO-F_TSD050.
           02  TGO-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TGO-F_LNAME    PIC  X(012) VALUE "TGO-F_TSD050".
           02  F              PIC  X(001).
           02  TGO-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TGO-F_SORT     PIC  X(100) VALUE SPACE.
           02  TGO-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TGO-F_RES      USAGE  POINTER.
       01  TGO-R.
           02  TGO-DTC        PIC  9(001).
           02  TGO-TSC        PIC  9(002).
           02  TGO-OKD        PIC  9(006).
           02  TGO-NO         PIC  9(004).
           02  TGO-BCD        PIC  9(004).
           02  TGO-TCD        PIC  9(004).
           02  TGO-KIN        PIC  9(010).
           02  TGO-UFD        PIC  9(006).
           02  TGO-MKD        PIC  9(006).
           02  TGO-SBC        PIC  9(004).
           02  TGO-FDM        PIC  N(024).
           02  F              PIC  X(025).
           02  TGO-SNU        PIC  9(004).
           02  TGO-SNM        PIC  9(004).
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
                "＊＊＊　受手・割引・支払手形　落込みリスト　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-ERR" " " "0" "0" "79" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "79" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT H-DATE FROM DATE.
       M-10.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TGO-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TGO-F_PNAME1 " " BY REFERENCE TGO-F_IDLST "0".
      *
      *           READ TGO-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TGO-F_PNAME1 BY REFERENCE TGO-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TGO-F_IDLST TGO-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE 0 TO W-PAGE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE TGO-DTC TO W-DTC.
           MOVE SPACE TO W-TGN.
           IF  W-DTC = 1
               MOVE "受取手形" TO W-TGN
           ELSE
               IF  W-DTC = 2
                   MOVE "割引手形" TO W-TGN
               ELSE
                   IF  W-DTC = 3
                       MOVE "支払手形" TO W-TGN
                   END-IF
               END-IF
           END-IF
           MOVE ZERO TO CHK W-AKIN W-SKIN.
       M-20.
           MOVE TGO-OKD TO W-OKD.
           MOVE ZERO TO CHK2 W-SKIN CNT.
       M-25.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TGN P-NAME P-BNA P-SNA P-KBN.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE W-TGN TO P-TGN
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-OKD TO P-OKD
           END-IF
           MOVE TGO-NO TO P-NO.
           MOVE TGO-TCD TO P-TCD.
           MOVE TGO-KIN TO P-KIN.
           IF  TGO-TSC = 11 OR 21
               MOVE "約手" TO P-KBN
           ELSE
               IF  TGO-TSC = 12 OR 22
                   MOVE "為手" TO P-KBN
               END-IF
           END-IF
           MOVE TGO-UFD TO P-UFD.
           MOVE TGO-MKD TO P-MAN.
      *
           IF  TGO-DTC = 3
               GO TO M-30
           END-IF
           MOVE TGO-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　得意先　なし　　＊＊" TO T-NAME
           END-IF
           MOVE T-NAME TO P-NAME
           GO TO M-35.
       M-30.
           MOVE TGO-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　仕入先　なし　　＊＊" TO S-NAME
           END-IF
           MOVE S-NAME TO P-NAME.
       M-35.
           IF  TGO-DTC = 1 OR 2
               IF  TGO-SBC = ZERO
                   GO TO M-40
               END-IF
           END-IF
           IF  TGO-DTC = 3
               IF  TGO-BCD = ZERO
                   GO TO M-40
               END-IF
           END-IF
           IF  TGO-DTC = 1 OR 2
               MOVE TGO-SBC TO B-KEY
           END-IF
           IF  TGO-DTC = 3
               MOVE TGO-BCD TO B-KEY
           END-IF
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-KEY TO P-SBC.
           MOVE B-BNA TO P-BNA.
           MOVE B-SNA TO P-SNA.
       M-40.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TGN TO P-TGN
               MOVE W-OKD TO P-OKD
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  TGO-FDM NOT = SPACE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-TGN P-NAME P-BNA P-SNA P-KBN
               MOVE TGO-FDM TO P-NAME
               MOVE "(" TO P-F
               MOVE ")" TO P-R
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           ADD TGO-KIN TO W-SKIN.
           IF  CNT < 2
               ADD 1 TO CNT
           END-IF.
       M-45.
      *           READ TGO-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TGO-F_PNAME1 BY REFERENCE TGO-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TGO-DTC NOT = W-DTC
               GO TO M-55
           END-IF
           IF  TGO-OKD NOT = W-OKD
               GO TO M-50
           END-IF
           GO TO M-25.
       M-50.
           PERFORM S-20 THRU S-30.
           GO TO M-20.
       M-55.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
           GO TO M-15.
       M-85.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-40.
       M-90.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TGO-F_IDLST TGO-F_PNAME1.
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
       S-15.
           EXIT.
       S-20.
           IF  CNT = 1
               MOVE SPACE TO SP-R
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TGN P-NAME P-BNA P-SNA P-KBN.
           MOVE "　　　　　　　　　　　　　　（　小計　）" TO P-NAME.
           MOVE W-SKIN TO P-KIN.
           MOVE "*" TO P-X.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TGN TO P-TGN
               MOVE W-OKD TO P-OKD
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
       S-25.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD W-SKIN TO W-AKIN.
       S-30.
           EXIT.
       S-35.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TGN P-NAME P-BNA P-SNA P-KBN.
           MOVE "　　　　　　　　［　合　計　］" TO P-NAME.
           MOVE W-AKIN TO P-KIN.
           MOVE "*" TO P-X.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-TGN TO P-TGN
               MOVE W-OKD TO P-OKD
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-40.
           EXIT.
