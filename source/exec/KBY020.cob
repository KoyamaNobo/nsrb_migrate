       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBY020.
      *********************************************************
      *    PROGRAM         :  材料棚卸差額明細表　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/11                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K            PIC  X(005) VALUE X"1A24212078".
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(039) VALUE SPACE.
           02  F            PIC  N(021) VALUE
                "＊＊＊　　材料　棚卸差額　明細表　　＊＊＊".
           02  F            PIC  X(032) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  F            PIC  X(055) VALUE SPACE.
           02  F            PIC  X(009) VALUE "I------  ".
           02  F            PIC  N(004) VALUE "帳簿在庫".
           02  F            PIC  X(019) VALUE "  ------I I------  ".
           02  F            PIC  N(004) VALUE "棚卸在庫".
           02  F            PIC  X(019) VALUE "  ------I I------  ".
           02  F            PIC  N(004) VALUE "棚卸差額".
           02  F            PIC  X(009) VALUE "  ------I".
       01  HEAD3.
           02  F            PIC  N(003) VALUE "コード".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(007) VALUE "材　　料　　名".
           02  F            PIC  X(028) VALUE SPACE.
           02  F            PIC  N(016) VALUE
                "単　価　　　　数　量　　　金　額".
           02  F            PIC  X(009) VALUE SPACE.
           02  F            PIC  N(009) VALUE "数　量　　　金　額".
           02  F            PIC  X(009) VALUE SPACE.
           02  F            PIC  N(009) VALUE "数　量　　　金　額".
       01  W-P.
           02  P-15K        PIC  X(005).
           02  P-KEY        PIC  X(006).
           02  F            PIC  X(001).
           02  P-NA         PIC  N(024).
           02  P-NAD   REDEFINES P-NA.
             03  P-NA1      PIC  N(013).
             03  P-NA2      PIC  N(011).
           02  P-T          PIC -----,--9.99.
           02  P-ZS         PIC ---,---,--9.99.
           02  P-ZK         PIC ----,---,--9.
           02  P-ZAI        PIC ----,---,--9.99.
           02  P-ZAK        PIC ----,---,--9.
           02  P-SS         PIC ----,---,--9.99.
           02  P-SK         PIC ----,---,--9.
           02  P-20K        PIC  X(005).
       01  W-D.
           02  W-MD.
             03  W-ZAK      PIC S9(009).
             03  W-ZK       PIC S9(009).
             03  W-SS       PIC S9(007)V9(02).
             03  W-SK       PIC S9(009).
           02  W-SD.
             03  WS-ZK      PIC S9(009).
             03  WS-ZAK     PIC S9(009).
             03  WS-SK      PIC S9(009).
           02  W-GD.
             03  WG-ZK      PIC S9(009).
             03  WG-ZAK     PIC S9(009).
             03  WG-SK      PIC S9(009).
           02  W-AD.
             03  WA-ZK      PIC S9(009).
             03  WA-ZAK     PIC S9(009).
             03  WA-SK      PIC S9(009).
           02  W-PC         PIC  9(001).
           02  W-PAGE       PIC  9(002).
           02  W-BKC        PIC  9(002).
           02  W-BKNO       PIC  9(002).
           02  W-JCD1.
             03  W-JCD11    PIC  9(001).
             03  W-JCD12    PIC  9(002).
           02  W-JCDD12     PIC  9(002).
       01  ERR-STAT         PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIJM.
           COPY LSJTM.
           COPY BUMONF.
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　材料　棚卸　差額明細表　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ER.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  ﾌﾞﾓﾝ ﾅｼ  ***".
             03  E-BKC   PIC  9(002).
             03  E-STAT  PIC  X(002).
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
      *C-ER
       CALL "SD_Init" USING 
            "C-ER" " " "0" "0" "31" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ER" " " "24" "0" "31" " " "C-ER" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ER" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-BKC" "9" "24" "35" "2" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-BKC" BY REFERENCE W-BKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-BKC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-F_PNAME1 " " BY REFERENCE JT-F_IDLST "0".
           MOVE ZERO TO W-PC.
       M-10.
      *           READ JT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-F_PNAME1 BY REFERENCE JT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  JT-ZC = 1
               GO TO M-10
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE 5 TO W-PC.
           MOVE ZERO TO W-PAGE W-AD.
           MOVE DATE-05R TO H-DATE.
           PERFORM S-10 THRU S-15.
       M-12.
           MOVE JT-BKC TO W-BKC.
           MOVE JT-BKNO TO W-BKNO.
           MOVE ZERO TO W-GD.
           MOVE ZERO TO BNM-KEY.
           MOVE W-BKC TO BNM-BU.
      *           READ BNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO BNMNMN
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-BKC" E-BKC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-15.
           MOVE JT-KEYD TO W-JCD1.
           MOVE W-JCD12 TO W-JCDD12.
           PERFORM S-45 THRU S-50.
           MOVE W-JCDD12 TO W-JCD12.
           MOVE ZERO TO W-SD.
       M-20.
           MOVE JT-KEY TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　ＪＭ　なし　＊＊　" TO J-NAME
               MOVE ZERO TO J-ST
           END-IF
           MOVE ZERO TO W-MD.
           COMPUTE W-ZK = JT-CSU * J-ST.
           COMPUTE W-ZAK = JT-TSU * J-ST.
           COMPUTE W-SS = JT-CSU - JT-TSU.
           COMPUTE W-SK = W-ZK - W-ZAK.
           IF  ZERO = JT-TSU AND W-ZAK AND JT-CSU AND W-ZK AND W-SS
                                                            AND W-SK
               GO TO M-30
           END-IF.
       M-25.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE JT-KEY TO P-KEY.
           MOVE J-NAME TO P-NA.
           MOVE J-ST TO P-T.
           MOVE JT-TSU TO P-ZAI.
           MOVE W-ZAK TO P-ZAK.
           MOVE JT-CSU TO P-ZS.
           MOVE W-ZK TO P-ZK.
           MOVE W-SS TO P-SS.
           MOVE W-SK TO P-SK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-ZK TO WS-ZK.
           ADD W-ZAK TO WS-ZAK.
           ADD W-SK TO WS-SK.
       M-30.
      *           READ JT-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-F_PNAME1 BY REFERENCE JT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JT-ZC = 1
               GO TO M-30
           END-IF
           MOVE JT-RC TO W-JCDD12.
           PERFORM S-45 THRU S-50.
           IF  JT-BKNO NOT = W-BKNO
               GO TO M-35
           END-IF
           IF  JT-BC = W-JCD11
               IF  W-JCDD12 = W-JCD12
                   GO TO M-20
               END-IF
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-15.
       M-35.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-32.
           GO TO M-12.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-32.
           PERFORM S-35 THRU S-40.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE JT-F_IDLST JT-F_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1
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
           IF  ZERO = WS-ZAK AND WS-ZK AND WS-SK
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　　　　　　　　　　（　　小　計　　）" TO P-NA.
           MOVE WS-ZAK TO P-ZAK.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-SK TO P-SK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-ZK TO WG-ZK.
           ADD WS-ZAK TO WG-ZAK.
           ADD WS-SK TO WG-SK.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = WG-ZAK AND WG-ZK AND WG-SK
               GO TO S-32
           END-IF
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　　　［　　合　計　　］　" TO P-NA1.
           MOVE BNMNMN TO P-NA2.
           MOVE WG-ZAK TO P-ZAK.
           MOVE WG-ZK TO P-ZK.
           MOVE WG-SK TO P-SK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WG-ZK TO WA-ZK.
           ADD WG-ZAK TO WA-ZAK.
           ADD WG-SK TO WA-SK.
       S-32.
           EXIT.
       S-35.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE "　【　　総　合　計　　】　" TO P-NA.
           MOVE WA-ZAK TO P-ZAK.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-SK TO P-SK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-10
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-20K TO P-20K.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-40.
           EXIT.
       S-45.
           IF  W-JCDD12 < 05
               MOVE 00 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 10
               MOVE 05 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 15
               MOVE 10 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 20
               MOVE 05 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 22
               MOVE 20 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 23
               MOVE 22 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 24
               MOVE 23 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 25
               MOVE 24 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 30
               MOVE 25 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 35
               MOVE 30 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 40
               MOVE 35 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 45
               MOVE 40 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 60
               MOVE 45 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 62
               MOVE 60 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 64
               MOVE 62 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 68
               MOVE 64 TO W-JCDD12
               GO TO S-50
           END-IF
           IF  W-JCDD12 < 80
               MOVE 68 TO W-JCDD12
           END-IF.
       S-50.
           EXIT.
