       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM230.
      **********************************************
      *****     直送先マスター　Ｗチェック     *****
      **********************************************
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
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　直送先マスター　Ｗチェック　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  15K            PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(114) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "直　送　先　名　".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "住　　所　　（上）　".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(004) VALUE "（下）　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "運送".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(006) VALUE "Ｔ　Ｅ　Ｌ　".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終日付".
           02  F              PIC  X(001) VALUE SPACE.
       01  W-P.
           02  W-P1.
             03  P-TCD        PIC  9(004).
             03  F            PIC  X(001).
             03  P-TNA        PIC  N(026).
             03  F            PIC  X(087).
           02  W-P2.
             03  F            PIC  X(001).
             03  P-CCD        PIC  9(003).
             03  F            PIC  X(001).
             03  P-NAME       PIC  N(026).
             03  P-JSU        PIC  N(020).
             03  P-JSS        PIC  N(020).
             03  F            PIC  X(002).
             03  P-UCD        PIC  9(001).
             03  F            PIC  X(001).
             03  P-TEL        PIC  X(014).
             03  F            PIC  X(001).
             03  P-YMD        PIC 99/99/99.
       01  W-R.
           02  W-KEY.
               03  W-TCD      PIC  9(004).
               03  W-CCD      PIC  9(003).
           02  W-NAME         PIC  N(026).
           02  W-JSU          PIC  N(020).
           02  W-JSS          PIC  N(020).
           02  W-UNO          PIC  X(008).
           02  W-TEL          PIC  X(014).
           02  W-FKC          PIC  9(002).
           02  W-UCD          PIC  9(001).
           02  F              PIC  X(022).
           02  W-YMD          PIC  9(006).
           02  F              PIC  X(064).
       01  W-D.
           02  CNT            PIC  9(002).
           02  W-PAGE         PIC  9(003).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LSPF.
      *FD  TC-M
       01  TC-M_HMM230.
           02  TC-M_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TC-M_LNAME     PIC  X(011) VALUE "TC-M_HMM230".
           02  F              PIC  X(001).
           02  TC-M_KEY1      PIC  X(100) VALUE SPACE.
           02  TC-M_SORT      PIC  X(100) VALUE SPACE.
           02  TC-M_IDLST     PIC  X(100) VALUE SPACE.
           02  TC-M_RES       USAGE  POINTER.
       01  TC-R.
           02  TC-KEY.
             03  TC-TCD       PIC  9(004).
             03  TC-CCD       PIC  9(003).
           02  TC-NAME        PIC  N(026).
           02  TC-JSU         PIC  N(020).
           02  TC-JSS         PIC  N(020).
           02  TC-UNO         PIC  X(008).
           02  TC-TEL         PIC  X(014).
           02  TC-FKC         PIC  9(002).
           02  TC-UCD         PIC  9(001).
           02  F              PIC  X(022).
           02  TC-YMD.
               03  TC-YY      PIC  9(002).
               03  TC-MM      PIC  9(002).
               03  TC-DD      PIC  9(002).
           02  F              PIC  X(064).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　直送先マスター　Ｗチェック　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
            "C-MID" " " "0" "0" "46" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "10" "10" "46" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO TC-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 " " BY REFERENCE TC-M_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO W-PAGE.
           PERFORM S-10 THRU S-15.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
      *           READ TC-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TC-M_PNAME1 BY REFERENCE TC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  TC-TEL = ZERO OR SPACE
               GO TO M-10
           END-IF.
       M-15.
           MOVE TC-R TO W-R.
           MOVE ZERO TO CNT.
       M-20.
      *           READ TC-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TC-M_PNAME1 BY REFERENCE TC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  TC-TEL = ZERO OR SPACE
               GO TO M-20
           END-IF
           IF  TC-TCD NOT = W-TCD
               GO TO M-15
           END-IF
           IF  TC-TEL NOT = W-TEL
               GO TO M-15
           END-IF
           IF  CNT NOT = ZERO
               GO TO M-25
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　得意先マスター　なし　＊＊　" TO T-NAME
           END-IF
           MOVE SPACE TO W-P1.
           MOVE W-TCD TO P-TCD.
           MOVE T-NAME TO P-TNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           PERFORM S-20 THRU S-30.
       M-25.
           MOVE TC-R TO W-R.
           PERFORM S-20 THRU S-30.
           GO TO M-20.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
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
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P2.
           MOVE W-CCD TO P-CCD.
           MOVE W-NAME TO P-NAME.
           MOVE W-JSU TO P-JSU.
           MOVE W-JSS TO P-JSS.
           IF  W-UCD NOT = ZERO
               MOVE W-UCD TO P-UCD
           END-IF
           MOVE W-TEL TO P-TEL.
           IF  W-YMD NOT = ZERO
               MOVE W-YMD TO P-YMD
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 63
               GO TO S-25
           END-IF
           PERFORM S-05 THRU S-15.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO CNT.
       S-30.
           EXIT.
