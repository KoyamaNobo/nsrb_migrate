       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TSM020.
      *******************************************************************
      *       PROGRAM               :　銀行マスターコード表             *
      *       DATE                  :  03/10/17                         *
      *       COMPILE TYPE          :  COBOL                            *
      *       FORM                  :  FTM020                           *
      *******************************************************************
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
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  N(017) VALUE
                  "【　　銀行マスター　コード表　　】".
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE1      PIC 99/99/99.
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  X(002) VALUE "P.".
           02  H-PAGE1      PIC Z9.
           02  F            PIC  X(009) VALUE SPACE.
           02  F            PIC  N(017) VALUE
                  "【　　銀行マスター　コード表　　】".
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE2      PIC 99/99/99.
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  X(002) VALUE "P.".
           02  H-PAGE2      PIC Z9.
       01  HEAD2.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "銀行名".
           02  F            PIC  N(001) VALUE SPACE.
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "本支店名".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "割引".
           02  F            PIC  X(002) VALUE SPACE.
           02  F            PIC  X(004) VALUE "ｺ-ﾄﾞ".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "銀行名".
           02  F            PIC  N(001) VALUE SPACE.
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "本支店名".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "割引".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "銀行名".
           02  F            PIC  N(001) VALUE SPACE.
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "本支店名".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "割引".
           02  F            PIC  X(002) VALUE SPACE.
           02  F            PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "銀行名".
           02  F            PIC  N(001) VALUE SPACE.
           02  F            PIC  X(005) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "本支店名".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "割引".
       01  W-P.
           02  W-P1    OCCURS  60.
             03  P-HCD1     PIC  X(004).
             03  P-BNA1     PIC  N(008).
             03  P-SNA1     PIC  N(008).
             03  F          PIC  X(001).
             03  P-X1       PIC  X(001).
             03  F          PIC  X(003).
             03  P-HCD2     PIC  X(004).
             03  P-BNA2     PIC  N(008).
             03  P-SNA2     PIC  N(008).
             03  F          PIC  X(001).
             03  P-X2       PIC  X(001).
             03  F          PIC  X(001).
             03  F          PIC  X(004).
             03  P-HCD3     PIC  X(004).
             03  P-BNA3     PIC  N(008).
             03  P-SNA3     PIC  N(008).
             03  F          PIC  X(001).
             03  P-X3       PIC  X(001).
             03  F          PIC  X(001).
             03  F          PIC  X(002).
             03  P-HCD4     PIC  X(004).
             03  P-BNA4     PIC  N(008).
             03  P-SNA4     PIC  N(008).
             03  F          PIC  X(001).
             03  P-X4       PIC  X(001).
       01  W-DATA.
           02  W-PAGE       PIC  9(002).
           02  W-PC         PIC  9(001).
           02  W-LD         PIC  9(002).
           02  W-CD         PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-BNA        PIC  N(008).
           02  W-DATE       PIC  9(006).
       01  ERR-STAT         PIC  X(002).
           COPY LSTAT.
      *
           COPY LSPF.
      *FD  BANK-M
       01  BANK-M_TSM020.
           02  BANK-M_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  BANK-M_LNAME   PIC  X(013) VALUE "BANK-M_TSM020".
           02  F              PIC  X(001).
           02  BANK-M_KEY1    PIC  X(100) VALUE SPACE.
           02  BANK-M_KEY2    PIC  X(100) VALUE SPACE.
           02  BANK-M_SORT    PIC  X(100) VALUE SPACE.
           02  BANK-M_IDLST   PIC  X(100) VALUE SPACE.
           02  BANK-M_RES     USAGE  POINTER.
       01  BANK-R.
           02  B-KEY          PIC  X(004).
           02  B-BNA          PIC  N(008).
           02  B-SNA          PIC  N(008).
           02  B-YBW          PIC  9(010).
           02  B-ZYZ          PIC  9(010).
           02  B-YBZ          PIC  9(010).
           02  B-YBC          PIC  9(001).
           02  F              PIC  X(016).
           02  B-PRC          PIC  9(002).
           02  F              PIC  X(043).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　銀行マスター　コード表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTM020" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "316" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "20" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "37" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT W-DATE FROM DATE.
           MOVE W-DATE TO H-DATE1 H-DATE2.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO BANK-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 " " BY REFERENCE BANK-M_IDLST "0".
      *
      *           READ BANK-M AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           PERFORM S-70 THRU S-80.
           MOVE ZERO TO W-PAGE W-PC W-LD W-CD CHK.
           MOVE SPACE TO W-BNA.
       M-20.
           PERFORM S-20 THRU S-30.
      *
           IF  W-CD NOT = 0
               GO TO M-25
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE B-BNA TO P-BNA1(W-LD)
           END-IF
           MOVE B-KEY TO P-HCD1(W-LD).
           MOVE B-SNA TO P-SNA1(W-LD).
           IF  B-YBC = 1
               MOVE "*" TO P-X1(W-LD)
           END-IF
           GO TO M-50.
       M-25.
           IF  W-CD NOT = 1
               GO TO M-30
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE B-BNA TO P-BNA2(W-LD)
           END-IF
           MOVE B-KEY TO P-HCD2(W-LD).
           MOVE B-SNA TO P-SNA2(W-LD).
           IF  B-YBC = 1
               MOVE "*" TO P-X2(W-LD)
           END-IF
           GO TO M-50.
       M-30.
           IF  W-CD NOT = 2
               GO TO M-35
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE B-BNA TO P-BNA3(W-LD)
           END-IF
           MOVE B-KEY TO P-HCD3(W-LD).
           MOVE B-SNA TO P-SNA3(W-LD).
           IF  B-YBC = 1
               MOVE "*" TO P-X3(W-LD)
           END-IF
           GO TO M-50.
       M-35.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE B-BNA TO P-BNA4(W-LD)
           END-IF
           MOVE B-KEY TO P-HCD4(W-LD).
           MOVE B-SNA TO P-SNA4(W-LD).
           IF  B-YBC = 1
               MOVE "*" TO P-X4(W-LD)
           END-IF.
       M-50.
           MOVE B-BNA TO W-BNA.
       M-60.
      *           READ BANK-M AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  B-BNA = W-BNA
               GO TO M-20
           END-IF
           MOVE 0 TO CHK.
           PERFORM S-20 THRU S-30.
           PERFORM S-20 THRU S-30.
           PERFORM S-20 THRU S-30.
           GO TO M-20.
       M-90.
           PERFORM S-50 THRU S-60.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           IF  W-PC NOT = 0
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
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE1.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE2.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 61
               GO TO S-30
           END-IF
           ADD 1 TO W-CD.
           IF  W-CD NOT = 04
               MOVE ZERO TO W-LD CHK
               GO TO S-20
           END-IF
           PERFORM S-50 THRU S-60.
           PERFORM S-70 THRU S-80.
           MOVE ZERO TO W-LD W-CD CHK.
           GO TO S-20.
       S-30.
           EXIT.
       S-50.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-55.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 61
               MOVE SPACE TO SP-R
               MOVE W-P1(W-LD) TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO S-55
           END-IF.
       S-60.
           EXIT.
       S-70.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-75.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 61
               MOVE SPACE TO P-BNA1(W-LD) P-BNA2(W-LD)
                             P-BNA3(W-LD) P-BNA4(W-LD)
                             P-SNA1(W-LD) P-SNA2(W-LD)
                             P-SNA3(W-LD) P-SNA4(W-LD)
               GO TO S-75
           END-IF.
       S-80.
           EXIT.
