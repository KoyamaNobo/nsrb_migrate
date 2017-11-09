       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS28L.
      *********************************************************
      *    PROGRAM         :  受注ＥＯＳ受信集計表      　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  H-MID          PIC  N(008) VALUE SPACE.
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　受注ＥＯＳ受信集計リスト　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ＳＳ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　Ｓ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　Ｍ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　Ｌ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ＬＬ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(044) VALUE SPACE.
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
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(044) VALUE SPACE.
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
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(044) VALUE SPACE.
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
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合　　計".
       01  W-P.
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  Z(001).
           02  P-ASU.
             03  P-SU         PIC  ---,---  OCCURS  10 TIMES.
           02  P-KEI          PIC --,---,--9.
       01  W-DATA.
           02  W-HCHK         PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-SC           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHIM2.
           COPY LSPF.
      *FD  JKEIF
       01  JKEIF_JHS28L.
           02  JKEIF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JKEIF_LNAME    PIC  X(012) VALUE "JKEIF_JHS28L".
           02  F              PIC  X(001).
           02  JKEIF_KEY1     PIC  X(100) VALUE SPACE.
           02  JKEIF_SORT     PIC  X(100) VALUE SPACE.
           02  JKEIF_IDLST    PIC  X(100) VALUE SPACE.
           02  JKEIF_RES      USAGE  POINTER.
       01  JKEI-R.
           02  JKEI-HCD       PIC  9(006).
           02  JKEI-ASUD.
             03  JKEI-ASU   OCCURS   4.
               04  JKEI-SUD   OCCURS  10.
                 05  JKEI-SU  PIC  9(005).
           02  JKEI-KEI       PIC  9(006).
           02  JKEI-AZCD.
             03  JKEI-AZC   OCCURS  4.
               04  JKEI-ZC    PIC  9(001).
           02  F              PIC  X(039).
           02  JKEI-SIGN      PIC  9(001).
           02  F              PIC  X(256).
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
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　受注ＥＯＳ受信集計リスト　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "15" "44" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "28" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO JKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JKEIF_PNAME1 " " BY REFERENCE JKEIF_IDLST "0".
       M-15.
      *           READ JKEIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JKEIF_PNAME1 BY REFERENCE JKEI-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JKEIF_IDLST JKEIF_PNAME1
               GO TO M-95
           END-IF
           IF  JKEI-AZCD = ZERO
               GO TO M-15
           END-IF
           IF  JKEI-SIGN = 0
               MOVE "【ワークマン】　" TO H-MID
           ELSE
               IF  JKEI-SIGN = 1
                   MOVE "【ナフコ】　　　" TO H-MID
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-10 THRU S-15.
       M-20.
           MOVE JKEI-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊品名なし＊　" TO HI-NAME
           END-IF
           MOVE 0 TO CHK W-HCHK.
       M-25.
           ADD 1 TO CHK.
           IF  CHK > 4
               GO TO M-35
           END-IF
           COMPUTE W-SC = CHK + 1.
           IF  W-SC = 5
               MOVE 1 TO W-SC
           END-IF
           IF  JKEI-ZC(W-SC) = 0
               GO TO M-25
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE W-SC TO P-SIZ.
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT < 11
               MOVE JKEI-SU(W-SC,CNT) TO P-SU(CNT)
               GO TO M-30
           END-IF
           PERFORM S-20 THRU S-35.
           GO TO M-25.
       M-35.
      *           READ JKEIF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JKEIF_PNAME1 BY REFERENCE JKEI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JKEI-AZCD = ZERO
               GO TO M-35
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JKEIF_IDLST JKEIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
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
           IF  W-HCHK = 1
               GO TO S-25
           END-IF
           IF  W-SC = 1
               IF  0 = JKEI-ZC(2) AND JKEI-ZC(3) AND JKEI-ZC(4)
                   MOVE JKEI-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
                   MOVE 1 TO W-HCHK
                   GO TO S-25
               END-IF
           END-IF
           IF  W-SC = 4
               IF  0 = JKEI-ZC(2) AND JKEI-ZC(3)
                   MOVE JKEI-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
                   MOVE 1 TO W-HCHK
                   GO TO S-25
               END-IF
           END-IF
           IF  W-SC = 3
               IF  0 = JKEI-ZC(2)
                   MOVE JKEI-HCD TO P-HCD
                   MOVE HI-NAME TO P-NAME
                   MOVE 1 TO W-HCHK
                   GO TO S-25
               END-IF
           END-IF
           IF  W-SC = 2
               MOVE JKEI-HCD TO P-HCD
               MOVE HI-NAME TO P-NAME
               MOVE 1 TO W-HCHK
           END-IF.
       S-25.
           IF  W-SC = 2
               IF  0 = JKEI-ZC(3) AND JKEI-ZC(4) AND JKEI-ZC(1)
                   MOVE JKEI-KEI TO P-KEI
                   GO TO S-30
               END-IF
           END-IF
           IF  W-SC = 3
               IF  0 = JKEI-ZC(4) AND JKEI-ZC(1)
                   MOVE JKEI-KEI TO P-KEI
                   GO TO S-30
               END-IF
           END-IF
           IF  W-SC = 4
               IF  0 = JKEI-ZC(1)
                   MOVE JKEI-KEI TO P-KEI
                   GO TO S-30
               END-IF
           END-IF
           IF  W-SC = 1
               MOVE JKEI-KEI TO P-KEI
           END-IF.
       S-30.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE JKEI-HCD TO P-HCD
               MOVE HI-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-35.
           EXIT.
