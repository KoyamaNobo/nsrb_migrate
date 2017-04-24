       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG360.
      *********************************************************
      *    PROGRAM         :  工品材料仕入先明細表            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  作表=0 , エクセル=1             *
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
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　工品材料　仕入明細表　　＊＊＊".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(004) VALUE "部門　　".
           02  F              PIC  N(004) VALUE "コード　".
           02  F              PIC  N(007) VALUE "材　　料　　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(009) VALUE "単　価　　　金　額".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(003) VALUE "コード".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(008) VALUE "仕　入　先　名　".
           02  F              PIC  X(020) VALUE SPACE.
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-BMN          PIC  N(004).
           02  F              PIC  X(002).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(002).
           02  P-JNA          PIC  N(024).
           02  P-SSU          PIC --,---,--9.99.
           02  P-T            PIC ----,--9.99.
           02  P-SIK          PIC ----,---,--9.
           02  F              PIC  X(003).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-SNA          PIC  N(024).
       01  W-DATA.
           02  W-PAGE         PIC  9(002).
           02  W-BKC          PIC  9(002).
           02  W-JSCD         PIC  9(002).
           02  W-JSC          PIC  9(002).
           02  W-SCD          PIC  9(004).
           02  W-JCD          PIC  9(006).
           02  WA-SIK         PIC S9(009).
           02  WS-SIK         PIC S9(009).
           02  WT-SIK         PIC S9(009).
           02  W-MD.
             03  W-SU         PIC S9(007)V9(02).
             03  W-T          PIC S9(006)V9(02).
             03  W-SIK        PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LISM.
           COPY LSJSSW.
           COPY LSPF.
      *FD  EXL-F
       01  EXL-F_KBG360.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0256000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_KBG360".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-BMN        PIC  N(004).
           02  EXL-JCD        PIC  X(006).
           02  EXL-JNA        PIC  N(024).
           02  EXL-SSU        PIC S9(007)V9(02).
           02  EXL-T          PIC S9(006)V9(02).
           02  EXL-SIK        PIC S9(009).
           02  EXL-SCD        PIC  X(004).
           02  EXL-SNA        PIC  N(024).
           02  F              PIC  X(116).
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
                "＊＊＊　　工品材料　仕入先明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "27" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-10.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               GO TO M-95
           END-IF
           IF  JR-BKC NOT = 32 AND 33
               GO TO M-10
           END-IF
           IF  ZERO = JR-SU AND JR-KIN
               GO TO M-10
           END-IF
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0"
               GO TO M-15
           END-IF
           CALL "PR_Open" RETURNING RESP.
           MOVE DATE-05R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE ZERO TO WS-SIK.
           MOVE JR-BKC TO W-BKC.
       M-20.
           MOVE ZERO TO WT-SIK.
           IF  JR-JCD2 = 90
               MOVE 1 TO W-JSC
           ELSE
               MOVE 0 TO W-JSC
           END-IF.
       M-25.
           MOVE ZERO TO W-MD.
           MOVE JR-JCD TO W-JCD.
           MOVE JR-T TO W-T.
           MOVE JR-SCD TO W-SCD.
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "（材料なし）" TO J-NAME
           END-IF
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "（仕入先なし）" TO S-NAME
           END-IF.
       M-30.
           IF  JR-DC = 10 OR 11 OR 13
               ADD JR-SU TO W-SU
           END-IF
           IF  JR-DC = 10 OR 12 OR 13
               ADD JR-KIN TO W-SIK
           END-IF.
       M-35.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JR-BKC NOT = 32 AND 33
               GO TO M-35
           END-IF
           IF  ZERO = JR-SU AND JR-KIN
               GO TO M-35
           END-IF
           IF  JR-BKC NOT = W-BKC
               GO TO M-45
           END-IF
           IF  JR-JCD2 = 90
               MOVE 1 TO W-JSCD
           ELSE
               MOVE 0 TO W-JSCD
           END-IF
           IF  W-JSC NOT = W-JSCD
               GO TO M-40
           END-IF
           IF (JR-JCD = W-JCD) AND (JR-T = W-T) AND
              (JR-SCD = W-SCD)
               GO TO M-30
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
           GO TO M-25.
       M-40.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-20.
       M-45.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM KEI-RTN THRU KEI-EX.
           PERFORM SUB-RTN THRU SUB-EX.
           GO TO M-15.
       M-90.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM KEI-RTN THRU KEI-EX.
           PERFORM SUB-RTN THRU SUB-EX.
           PERFORM ALL-RTN THRU ALL-EX.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           IF  JS-SIGN = 1
               CALL "DB_F_Close" USING
                BY REFERENCE EXL-F_IDLST EXL-F_PNAME1
           ELSE
               IF  JS-SIGN = 0
                   CALL "PR_Close" RETURNING RESP
               END-IF
           END-IF.
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
       PRI-RTN.
           IF  ZERO = W-SU AND W-SIK
               GO TO PRI-EX
           END-IF
           IF  JS-SIGN = 1
               GO TO PRI-010
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA P-SNA.
           MOVE W-15K TO P-15K.
           IF  W-BKC = 32
               MOVE "防　振　" TO P-BMN
           ELSE
               IF  W-BKC = 33
                   MOVE "その他　" TO P-BMN
               END-IF
           END-IF
           MOVE W-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           IF  W-SU NOT = ZERO
               MOVE W-SU TO P-SSU
           END-IF
           IF  W-T NOT = ZERO
               MOVE W-T TO P-T
           END-IF
           MOVE W-SIK TO P-SIK.
           MOVE W-SCD TO P-SCD.
           MOVE S-NAME TO P-SNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PRI-900.
       PRI-010.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-BMN EXL-JNA EXL-SNA.
           IF  W-BKC = 32
               MOVE "防　振　" TO EXL-BMN
           ELSE
               IF  W-BKC = 33
                   MOVE "その他　" TO EXL-BMN
               END-IF
           END-IF
           MOVE W-JCD TO EXL-JCD.
           MOVE J-NAME TO EXL-JNA.
           MOVE W-SU TO EXL-SSU.
           MOVE W-T TO EXL-T.
           MOVE W-SIK TO EXL-SIK.
           MOVE W-SCD TO EXL-SCD.
           MOVE S-NAME TO EXL-SNA.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       PRI-900.
           ADD W-SIK TO WT-SIK.
       PRI-EX.
           EXIT.
       KEI-RTN.
           IF  JS-SIGN = 1
               GO TO KEI-010
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA P-SNA.
           MOVE W-15K TO P-15K.
           IF  W-BKC = 32
               MOVE "防　振　" TO P-BMN
           ELSE
               IF  W-BKC = 33
                   MOVE "その他　" TO P-BMN
               END-IF
           END-IF
           IF  W-JSC = 0
               MOVE "　　　　　　　（材料計）　　" TO P-JNA
           ELSE
               IF  W-JSC = 1
                   MOVE "　　　　　　　（商品仕入計）" TO P-JNA
               END-IF
           END-IF
           MOVE WT-SIK TO P-SIK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO KEI-900.
       KEI-010.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-BMN EXL-JNA EXL-SNA.
           IF  W-BKC = 32
               MOVE "防　振　" TO EXL-BMN
           ELSE
               IF  W-BKC = 33
                   MOVE "その他　" TO EXL-BMN
               END-IF
           END-IF
           IF  W-JSC = 0
               MOVE "　　　　　　　（材料計）　　" TO EXL-JNA
           ELSE
               IF  W-JSC = 1
                   MOVE "　　　　　　　（商品仕入計）" TO EXL-JNA
               END-IF
           END-IF
           MOVE WT-SIK TO EXL-SIK.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       KEI-900.
           ADD WT-SIK TO WS-SIK.
       KEI-EX.
           EXIT.
       SUB-RTN.
           IF  JS-SIGN = 1
               GO TO SUB-010
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA P-SNA.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　　　［　合　計　］" TO P-JNA.
           MOVE WS-SIK TO P-SIK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           GO TO SUB-900.
       SUB-010.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-BMN EXL-JNA EXL-SNA.
           MOVE "　　　　　　　　［　合　計　］" TO EXL-JNA.
           MOVE WS-SIK TO EXL-SIK.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       SUB-900.
           ADD WS-SIK TO WA-SIK.
       SUB-EX.
           EXIT.
       ALL-RTN.
           IF  JS-SIGN = 1
               GO TO ALL-010
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-BMN P-JNA P-SNA.
           MOVE W-15K TO P-15K.
           MOVE "　　　【　総　合　計　】" TO P-JNA.
           MOVE WA-SIK TO P-SIK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO ALL-EX.
       ALL-010.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-BMN EXL-JNA EXL-SNA.
           MOVE "　　　【　総　合　計　】" TO EXL-JNA.
           MOVE WA-SIK TO EXL-SIK.
      *           WRITE EXL-R.
      *//////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       ALL-EX.
           EXIT.
