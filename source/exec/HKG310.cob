       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG310.
      *********************************************************
      *    PROGRAM         :  担当者別　入金明細表(集金予定Ｃ)*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/29                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
           02  F              PIC  X(018) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  F              PIC  N(023) VALUE
                "担当日付別　入金明細表　（集金予定チェック用）".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "担当".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　入金日".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入金区分".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形期日".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　金　額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　請求月".
       01  W-P.
           02  F              PIC  X(001).
           02  P-TC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-GP           PIC 99/99.
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-KBN          PIC  N(006).
           02  P-SHZ          PIC  N(004).
           02  P-TD           PIC 99/99/99.
           02  P-KIN          PIC ----,---,--9.
           02  P-TKIN         PIC ----,---,--9.
           02  F              PIC  X(002).
           02  P-SD           PIC 99/99.
       01  W-DATA.
           02  W-PEY.
             03  W-SPEY       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-TC.
             03  W-TC1        PIC  9(001).
             03  W-TC2        PIC  9(001).
           02  W-GP           PIC  9(004).
           02  W-TCD          PIC  9(004).
           02  W-C            PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
             03  CHK3         PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  WN-D.
           02  WN-KIN         PIC S9(010).
       01  WS-D.
           02  WS-KIN         PIC S9(010).
       01  WA-D.
           02  WA-KIN         PIC S9(010).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  NYUR-F
       01  NYUR-F_HKG310.
           02  NYUR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYUR-F_LNAME   PIC  X(013) VALUE "NYUR-F_HKG310".
           02  F              PIC  X(001).
           02  NYUR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUR-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUR-F_RES     USAGE  POINTER.
       01  NYUR-R.
           02  N-DATE.
             03  N-NEN        PIC  9(004).
             03  N-GP.
               04  N-GET      PIC  9(002).
               04  N-PEY      PIC  9(002).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC.
             03  N-NC1        PIC  9(001).
             03  N-NC2        PIC  9(001).
           02  F              PIC  9(003).
           02  N-TD           PIC  9(006).
           02  F              PIC  9(002).
           02  N-SD           PIC  9(004).
           02  N-BC           PIC  9(001).
           02  N-TC.
             03  N-TC1        PIC  9(001).
             03  N-TC2        PIC  9(001).
           02  F              PIC  X(088).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　担当日付別　入金明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(020) VALUE
                "【    日 ～   日  】".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SPEY  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  A-DMM   PIC  9(001).
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
           "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
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
           "08C-MID" "X" "15" "21" "20" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "X" "20" "20" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "15" "0" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SPEY" "9" "15" "25" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EPEY" "9" "15" "33" "2" "A-SPEY" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "37" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "61" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "61" " " "C-ERR" RETURNING RESU.
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
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SPEY > W-EPEY
               GO TO M-15
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
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
       M-25.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  N-PEY < W-SPEY OR > W-EPEY
               GO TO M-25
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-30.
           MOVE ZERO TO WS-D CHK.
           MOVE N-TC TO W-TC.
       M-35.
           MOVE ZERO TO CHK2 CHK3.
           MOVE N-GP TO W-GP.
       M-40.
           MOVE ZERO TO WN-D CHK3.
           MOVE N-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　ＴＭ　なし　＊＊　　" TO T-NAME
           END-IF.
       M-45.
           PERFORM S-20 THRU S-30.
       M-50.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  N-PEY < W-SPEY OR > W-EPEY
               GO TO M-50
           END-IF
           IF  N-TC NOT = W-TC
               GO TO M-65
           END-IF
           IF  N-GP NOT = W-GP
               GO TO M-60
           END-IF
           IF  N-TCD NOT = W-TCD
               GO TO M-55
           END-IF
           MOVE 0 TO W-C.
           PERFORM S-35 THRU S-40.
           GO TO M-45.
       M-55.
           MOVE 1 TO W-C.
           PERFORM S-35 THRU S-40.
           GO TO M-40.
       M-60.
           MOVE 1 TO W-C.
           PERFORM S-35 THRU S-40.
           GO TO M-35.
       M-65.
           MOVE 1 TO W-C.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
           GO TO M-30.
       M-90.
           MOVE 1 TO W-C.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN P-SHZ.
           MOVE "　　　【　　総　合　計　　】　" TO P-NAME.
           MOVE WA-KIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1.
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
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN P-SHZ.
           IF  CHK1 = 0
               MOVE 5 TO CHK1
               MOVE W-TC TO P-TC
           END-IF
           IF  CHK2 = 0
               MOVE 5 TO CHK2
               MOVE W-GP TO P-GP
           END-IF
           IF  CHK3 = 0
               MOVE 5 TO CHK3
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NAME
           END-IF
           IF  N-NC1 > 6
               MOVE "そ　の　他　" TO P-KBN
               GO TO S-25
           END-IF
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE N-NC1 TO HKB-NKC1.
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
               MOVE SPACE TO HKB-NKNA
           END-IF
           MOVE HKB-NKNA TO P-KBN.
       S-25.
           IF  N-NC2 > 7
               MOVE "消費税　" TO P-SHZ
           END-IF
           IF  N-TD NOT = ZERO
               MOVE N-TD TO P-TD
           END-IF
           MOVE N-KIN TO P-KIN.
           IF  N-SD NOT = ZERO
               MOVE N-SD TO P-SD
           END-IF
           ADD N-KIN TO WN-KIN.
       S-30.
           EXIT.
       S-35.
           IF  W-C NOT = 0
               MOVE WN-KIN TO P-TKIN
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               MOVE W-GP TO P-GP
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W-C NOT = 0
               ADD WN-KIN TO WS-KIN
           END-IF.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-KBN P-SHZ.
           MOVE "　　　　　［　　小　計　　］　" TO P-NAME.
           MOVE WS-KIN TO P-TKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TC TO P-TC
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-KIN TO WA-KIN.
       S-50.
           EXIT.
