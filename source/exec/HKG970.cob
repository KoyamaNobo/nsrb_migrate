       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG970.
      *********************************************************
      *    入金未変換リスト                 　　　　　　　　  *
      *    JS-SIGN : 0=履物 , 1=工品        　　　　　　　　  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-PAGE             PIC  9(002) VALUE ZERO.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  H-MID          PIC  N(002) VALUE SPACE.
           02  F              PIC  N(014) VALUE
                "入金　未変換リスト　　＊＊＊".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(008) VALUE "   ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "相殺".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形期日".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　請求日".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　消費税入金".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上入金".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(002).
           02  P-NC           PIC  9(002).
           02  F              PIC  X(002).
           02  P-NSC          PIC  Z(001).
           02  F              PIC  X(002).
           02  P-TD           PIC 99/99/99.
           02  P-KIN          PIC ----,---,--9.
           02  P-SS           PIC BB99/99.
           02  P-SHZ          PIC ---,---,--9.
           02  P-TKIN         PIC ----,---,--9.
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-DATE         PIC  9(008).
           02  W-BC           PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-AD.
             03  W-GKIN       PIC S9(009).
             03  W-TKIN       PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  W-PC           PIC  9(001).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
      *
           COPY LSTAT.
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
      *FD  NYUW-F
       01  NYUW-F_HKG970.
           02  NYUW-F_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F               PIC  X(001).
           02  NYUW-F_LNAME    PIC  X(013)  VALUE "NYUW-F_HKG970".
           02  F               PIC  X(001).
           02  NYUW-F_KEY1     PIC  X(100)  VALUE SPACE.
           02  NYUW-F_KEY2     PIC  X(100)  VALUE SPACE.
           02  NYUW-F_SORT     PIC  X(100)  VALUE SPACE.
           02  NYUW-F_IDLST    PIC  X(100)  VALUE SPACE.
           02  NYUW-F_RES      USAGE  POINTER.
       01  NYUW-R.
           02  N-DATE.
             03  F            PIC  9(002).
             03  N-DATES      PIC  9(006).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC.
             03  N-NC1        PIC  9(001).
             03  N-NC2        PIC  9(001).
           02  N-NSC          PIC  9(001).
           02  N-TD.
             03  F            PIC  9(002).
             03  N-TNGPS      PIC  9(006).
           02  N-SS.
             03  F            PIC  9(002).
             03  N-SNGS       PIC  9(004).
           02  N-BC           PIC  9(001).
           02  N-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  N-KEY.
             03  N-NO         PIC  9(006).
             03  N-GNO        PIC  9(001).
           02  N-FDNO.
             03  N-FNO        PIC  9(006).
             03  N-FGNO       PIC  9(002).
           02  N-SKD          PIC  9(008).
           02  F              PIC  X(017).
           02  N-ACT          PIC  9(001).
           02  N-PRC          PIC  9(001).
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
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　入金未変換リスト　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  FILLER.
             03  C-MID0    PIC  N(002) VALUE "履物".
             03  C-MID1    PIC  N(002) VALUE "工品".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
            "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
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
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-DSP" " " "6" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "C-MID0" "N" "6" "20" "4" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
            "C-MID1" "N" "6" "20" "4" "C-MID0" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "18" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO M-05
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           IF  JS-SIGN = 0
               MOVE "履物" TO H-MID
               CALL "SD_Output" USING "C-MID0" C-MID0 "p" 
                                         RETURNING RESU
           ELSE
               MOVE "工品" TO H-MID
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" 
                                         RETURNING RESU
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO NYUW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NYUW-F_PNAME1 " " BY REFERENCE NYUW-F_IDLST "0".
       M-10.
      *           READ NYUW-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NYUW-F_PNAME1 BY REFERENCE NYUW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1
               GO TO M-95
           END-IF
           IF  JS-SIGN = 0
               IF  N-BC NOT = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  N-BC = 0
                   GO TO M-10
               END-IF
           END-IF
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-65.
           MOVE N-DATE TO W-DATE.
           MOVE ZERO TO CHK.
       M-70.
           MOVE ZERO TO W-AD CHK2.
           MOVE N-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF.
       M-75.
           IF  W-PC = 0
               MOVE 9 TO W-PC
               CALL "PR_Open" RETURNING RESP
               MOVE DATE-02R TO H-DATE
               PERFORM MID-020 THRU MID-EX
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-TNA.
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               MOVE N-DATES TO P-DATE
           END-IF
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           MOVE N-NC TO P-NC.
           MOVE N-NSC TO P-NSC.
           IF  N-TD NOT = ZERO
               MOVE N-TNGPS TO P-TD
           END-IF
           MOVE N-KIN TO P-KIN.
           IF  N-SS NOT = ZERO
               MOVE N-SNGS TO P-SS
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE N-DATES TO P-DATE
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD N-KIN TO W-GKIN.
           IF  N-NC2 > 7
               ADD N-KIN TO W-SHZ
           ELSE
               ADD N-KIN TO W-TKIN
           END-IF.
       M-80.
      *           READ NYUW-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NYUW-F_PNAME1 BY REFERENCE NYUW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               IF  N-BC NOT = 0
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  N-BC = 0
                   GO TO M-80
               END-IF
           END-IF
           IF (N-DATE = W-DATE) AND (N-TCD = W-TCD)
               GO TO M-75
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           IF  N-DATE NOT = W-DATE
               GO TO M-65
           END-IF
           GO TO M-70.
       M-90.
           PERFORM KEI-RTN THRU KEI-EX.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1.
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *-------------  合計　印字　----------------------------------------------
       KEI-RTN.
           MOVE SPACE TO W-P.
           MOVE "　　　　　　　　　　　　＜　　合　計　　＞" TO P-TNA.
           MOVE W-GKIN TO P-KIN.
           MOVE W-SHZ TO P-SHZ.
           MOVE W-TKIN TO P-TKIN.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
      *-------------  見出し　印字　--------------------------------------------
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE  TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
