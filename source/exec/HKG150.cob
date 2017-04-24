       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HKG150.
      *********************************************************
      *    PROGRAM         :  売掛残高明細表　ﾁｪｯｸ用　　      *
      *                    :  売上・消費税明細表　ﾁｪｯｸ用　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/05/26                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(017) VALUE SPACE.
           02  H-MID          PIC  N(019).
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(044) VALUE SPACE.
           02  F              PIC  X(016) VALUE "I-------------  ".
           02  F              PIC  N(004) VALUE "売掛残高".
           02  F              PIC  X(015) VALUE "  ------------I".
       01  HEAD3.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(007) VALUE "得　意　先　名".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  N(003) VALUE "売　上".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　合　計".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-KIN          PIC --,---,---,--9.
           02  P-SHZ          PIC ---,---,--9.
           02  P-KEI          PIC --,---,---,--9.
           02  P-20K          PIC  X(005).
       01  W-DATA.
           02  W-KEI          PIC S9(010).
           02  W-PAGE         PIC  9(002).
           02  CHK            PIC  9(001) VALUE 0.
           02  W-TNC          PIC  9(002).
           02  W-KIN          PIC S9(010).
           02  W-SHZ          PIC S9(008).
       01  WT-D.
           02  WT-KIN         PIC S9(010).
           02  WT-SHZ         PIC S9(008).
           02  WT-KEI         PIC S9(010).
       01  ERR-STAT           PIC  X(002).
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
       01  C-MID1.
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　売掛残高　明細表　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-MID2.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　売上・消費税　明細表　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "252" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "N" "3" "10" "36" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" "N" "4" "10" "36" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID1" "N" "5" "10" "36" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID1" "N" "6" "10" "36" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID1" "N" "7" "10" "36" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID1" "N" "8" "10" "36" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID1" "N" "9" "10" "36" "06C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING
           "C-MID2" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID2" "N" "3" "10" "40" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID2" "N" "4" "10" "40" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID2" "N" "5" "10" "40" "02C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID2" "N" "6" "10" "40" "03C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID2" "N" "7" "10" "40" "04C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID2" "N" "8" "10" "40" "05C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID2" "N" "9" "10" "40" "06C-MID2" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-05
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               MOVE "＊＊＊　　売掛残高　明細表　　＊＊＊　" TO H-MID
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 1
               MOVE "＊＊＊　　売上・消費税明細表　　＊＊＊" TO H-MID
               CALL "SD_Output" USING
                "C-MID2" C-MID2 "p" RETURNING RESU
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO TT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WT-D W-PAGE.
           MOVE DATE-02R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-10.
      *           READ TT-M AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TT-M_PNAME1 BY REFERENCE TT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE ZERO TO W-KIN W-SHZ.
           IF  JS-SIGN = 0
               MOVE TT-TUZ TO W-KIN
               MOVE TT-TUZZ TO W-SHZ
           END-IF
           IF  JS-SIGN = 1
               COMPUTE W-KIN = TT-TUA - TT-TNB
               COMPUTE W-SHZ = TT-TUAZ - TT-TNBZ
           END-IF
           IF  ZERO = W-KIN AND W-SHZ
               GO TO M-10
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE TT-TNC TO W-TNC
           END-IF
           IF  TT-TNC NOT = W-TNC
               MOVE TT-TNC TO W-TNC
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
      *
           COMPUTE W-KEI = W-KIN + W-SHZ.
           MOVE TT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊" TO T-NAME
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE TT-TCD TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE W-KIN TO P-KIN.
           MOVE W-SHZ TO P-SHZ.
           MOVE W-KEI TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-KIN TO WT-KIN.
           ADD W-SHZ TO WT-SHZ.
           ADD W-KEI TO WT-KEI.
           GO TO M-10.
       M-90.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　［　ＡＬＬ　ＴＯＴＡＬ　］" TO P-NAME.
           MOVE WT-KIN TO P-KIN.
           MOVE WT-SHZ TO P-SHZ.
           MOVE WT-KEI TO P-KEI.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
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
           IF  JS-SIGN = 0
               MOVE HEAD2 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF
           MOVE HEAD3 TO SP-R.
           IF  JS-SIGN = 0
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           IF  JS-SIGN = 1
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
