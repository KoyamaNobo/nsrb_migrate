       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG330.
      *********************************************************
      *    PROGRAM         :  得意先別　入金明細表 (経理用)   *
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
           02  20K            PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　得意先別　入金明細表　（経理用）".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(020) VALUE
                "ｺｰﾄﾞ　得　意　先　名".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(003) VALUE "入金日".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(007) VALUE "入金額　種　類".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(003) VALUE "期　日".
           02  F              PIC  X(001) VALUE SPACE.
       01  W-P.
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NA           PIC  N(026).
           02  F              PIC  X(001).
           02  P-DATE         PIC 99/99/99.
           02  P-KIN          PIC --,---,---,--9.
           02  F              PIC  X(002).
           02  P-NC           PIC  N(006).
           02  F              PIC  X(001).
           02  P-TD           PIC 99/99/99.
       01  W-D.
           02  W-TCD          PIC  9(004).
           02  W-NKIN         PIC S9(009).
           02  W-AKIN         PIC S9(009).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY LSPF.
      *FD  NYUR-F
       01  NYUR-F_HKG330.
           02  NYUR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYUR-F_LNAME   PIC  X(013) VALUE "NYUR-F_HKG330".
           02  F              PIC  X(001).
           02  NYUR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUR-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUR-F_RES     USAGE  POINTER.
       01  NYUR-R.
           02  F              PIC  9(002).
           02  N-DATE         PIC  9(006).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　得意先別　入金明細表　（経理用）　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(027) VALUE
                    "***  HKBM ﾅｼ (       )  ***".
               04  02E-ME1 PIC  X(007).
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
           "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "44" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" " " "24" "0" "34" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "01E-ME1" "X" "24" "15" "27" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-ME1" "X" "24" "29" "7" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "02E-ME1" BY REFERENCE HKB-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
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
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUR-F_PNAME1 " " BY REFERENCE NYUR-F_IDLST "0".
      *
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUR-F_IDLST NYUR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
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
           MOVE ZERO TO W-AKIN.
       M-35.
           MOVE ZERO TO W-NKIN CHK.
           MOVE N-TCD TO W-TCD.
       M-40.
           PERFORM S-20 THRU S-35.
       M-45.
      *           READ NYUR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUR-F_PNAME1 BY REFERENCE NYUR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  N-TCD NOT = W-TCD
               GO TO M-50
           END-IF
           GO TO M-40.
       M-50.
           PERFORM S-40 THRU S-45.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO M-35.
       M-95.
           PERFORM S-40 THRU S-45.
           PERFORM S-70 THRU S-75.
      *
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO CHK1.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           IF  CHK1 NOT = ZERO
               GO TO S-30
           END-IF
           MOVE N-TCD TO P-TCD.
           MOVE N-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　" TO T-NAME
           END-IF
           MOVE T-NAME TO P-NA.
           MOVE 5 TO CHK1.
       S-30.
           MOVE N-DATE TO P-DATE.
           MOVE N-KIN TO P-KIN.
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
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE SPACE TO HKB-NKNA
           END-IF
           MOVE HKB-NKNA TO P-NC.
           IF  N-NC2 > 7
               MOVE "消　費　税　" TO P-NC
           END-IF
           IF  N-TD NOT = ZERO
               MOVE N-TD TO P-TD
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD N-KIN TO W-NKIN.
           IF  CHK2 = 3
               MOVE 5 TO CHK2
           END-IF
           IF  CHK2 = ZERO
               MOVE 3 TO CHK2
           END-IF.
       S-35.
           EXIT.
       S-40.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF
           ADD W-NKIN TO W-AKIN.
           IF  CHK2 = 3
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　（　小　計　）　　　" TO P-NA.
           MOVE W-NKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-45.
           EXIT.
       S-70.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO W-P.
           MOVE "　【　　総　合　計　　】　　　　　　　" TO P-NA.
           MOVE W-AKIN TO P-KIN.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.
