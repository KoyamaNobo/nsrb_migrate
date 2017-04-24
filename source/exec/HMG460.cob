       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG460.
      *********************************************************
      *    PROGRAM         :  預り受払表　　　　　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/14                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
       01  ERR-STAT           PIC  X(002).
       01  15K                PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(013) VALUE
                "＊＊＊　　得　意　先　別　".
           02  F              PIC  N(014) VALUE
                "預　り　受　払　表　　＊＊＊".
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE.".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(019) VALUE
                "ｺｰﾄﾞ 得　意　先　名".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(016) VALUE "ｺｰﾄﾞ  品　　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(011) VALUE "   日付    ".
           02  F              PIC  N(003) VALUE "預り数".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "出荷数".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "預り残".
       01  W-P.
           02  K-CD1          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  K-CD2          PIC  X(005).
           02  F              PIC  X(001).
           02  P-GP           PIC B99/99B.
           02  P-KM    REDEFINES P-GP    PIC  X(007).
           02  F              PIC  X(001).
           02  P-AS           PIC ----,---.
           02  F              PIC  X(001).
           02  P-SS           PIC ----,---.
           02  F              PIC  X(001).
           02  P-AZ           PIC ----,--9.
       01  W-D.
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-AZ           PIC S9(006).
           02  W-PAGE         PIC  9(002).
           02  W-TNA          PIC  N(026).
           02  W-HNA          PIC  N(024).
           02  CNT            PIC  9(003).
           02  W-SETCD.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004) VALUE 9999.
           02  W-DMM          PIC  9(001).
       01  WS-D.
           02  WS-AS          PIC S9(006).
           02  WS-SS          PIC S9(006).
           02  WS-AZ          PIC S9(006).
       01  WA-D.
           02  WA-AS          PIC S9(006).
           02  WA-SS          PIC S9(006).
           02  WA-AZ          PIC S9(006).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LSPF.
      *FD  AUH-F
       01  AUH-F_HMG460.
           02  AUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  AUH-F_LNAME    PIC  X(012) VALUE "AUH-F_HMG460".
           02  F              PIC  X(001).
           02  AUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  AUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  AUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  AUH-F_RES      USAGE  POINTER.
       01  AUH-R.
           02  UH-TCD         PIC  9(004).
           02  UH-HCD         PIC  9(006).
           02  UH-DATE.
             03  UH-N         PIC  9(002).
             03  UH-GP        PIC  9(004).
           02  UH-AS          PIC S9(005).
           02  UH-SS          PIC S9(005).
           02  UH-AZ          PIC S9(005).
           02  F              PIC  X(033).
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　得意先別　預り受払表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(044) VALUE
                "<   得意先ｺｰﾄﾞ 0000 より 9999 まで打出し   >".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4A05".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "346" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "12" "8" "44" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "15" "19" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "12" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-STCD" "9" "12" "23" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-ETCD" "9" "12" "33" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "15" "36" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
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
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-STCD > W-ETCD
               GO TO M-10
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
           MOVE WK0064ID TO AUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" AUH-F_PNAME1 "EXCLUSIVE" BY REFERENCE
            AUH-F_IDLST "0".
       M-25.
      *           READ AUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" AUH-F_PNAME1 BY REFERENCE AUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE AUH-F_IDLST AUH-F_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  UH-TCD < W-STCD
               GO TO M-25
           END-IF
           IF  UH-TCD > W-ETCD
               CALL "DB_F_Close" USING
                BY REFERENCE AUH-F_IDLST AUH-F_PNAME1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO WA-D W-PAGE.
           PERFORM S-10 THRU S-15.
       M-30.
           MOVE UH-TCD TO W-TCD.
           MOVE ZERO TO WS-D CNT.
           MOVE SPACE TO W-P.
           MOVE ALL "　"  TO P-HNA  P-TNA.
           MOVE 15K   TO K-CD1.
           MOVE W-TCD TO P-TCD.
           MOVE UH-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　マスター　なし　＊　　　" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA P-TNA.
       M-35.
           MOVE UH-HCD TO W-HCD.
           MOVE ZERO TO W-AZ.
           MOVE W-HCD TO P-HCD.
           MOVE UH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊　マスター　なし　＊　　　" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-HNA P-HNA.
           MOVE ZERO TO W-AZ.
       M-45.
           ADD 1 TO CNT.
           IF  UH-DATE = ZERO
               MOVE UH-AZ TO W-AZ
               MOVE "<繰 越>" TO P-KM
           ELSE
               MOVE UH-GP TO P-GP
               MOVE UH-AS TO P-AS
               MOVE UH-SS TO P-SS
           END-IF
           COMPUTE W-AZ = W-AZ + UH-AS - UH-SS.
           MOVE W-AZ TO P-AZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               GO TO M-50
           ELSE
               GO TO M-55
           END-IF.
       M-50.
           MOVE W-TCD TO P-TCD.
           MOVE W-TNA TO P-TNA.
           MOVE W-HCD TO P-HCD.
           MOVE W-HNA TO P-HNA.
           PERFORM S-05 THRU S-15.
       M-55.
           MOVE SPACE TO SP-R.
           MOVE 20K TO K-CD2.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P SP-R.
           MOVE 15K   TO K-CD1.
           MOVE ALL "　"  TO P-HNA  P-TNA.
           ADD UH-AS TO WS-AS.
           ADD UH-SS TO WS-SS.
       M-60.
      *           READ AUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" AUH-F_PNAME1 BY REFERENCE AUH-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  UH-TCD > W-ETCD
               GO TO M-85
           END-IF
           IF  W-TCD NOT = UH-TCD
               GO TO M-65
           END-IF
           IF  W-HCD = UH-HCD
               GO TO M-45
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD W-AZ TO WS-AZ.
           GO TO M-35.
       M-65.
           PERFORM S-20 THRU S-30.
           GO TO M-30.
       M-85.
           PERFORM S-20 THRU S-30.
           MOVE SPACE TO W-P.
           MOVE ALL "　"  TO P-HNA  P-TNA.
           MOVE 15K   TO K-CD1.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】　" TO P-HNA.
           MOVE WA-AS TO P-AS.
           MOVE WA-SS TO P-SS.
           MOVE WA-AZ TO P-AZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE 20K TO K-CD2.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AUH-F_IDLST AUH-F_PNAME1.
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
       S-15.
           EXIT.
       S-20.
           ADD W-AZ TO WS-AZ.
           MOVE SPACE TO W-P.
           MOVE ALL "　"  TO P-HNA  P-TNA.
           MOVE 15K   TO K-CD1.
           IF  CNT = 1
               GO TO S-25
           END-IF
           MOVE "　　　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-HNA.
           MOVE WS-AS TO P-AS.
           MOVE WS-SS TO P-SS.
           MOVE WS-AZ TO P-AZ.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TCD TO P-TCD
               MOVE W-TNA TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE 20K TO K-CD2.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-AS TO WA-AS.
           ADD WS-SS TO WA-SS.
           ADD WS-AZ TO WA-AZ.
       S-30.
           EXIT.
