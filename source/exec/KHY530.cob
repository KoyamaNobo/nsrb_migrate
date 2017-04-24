       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY530.
      *********************************************************
      *    PROGRAM         :  年間品種別製品受払表　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/13                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  P-HN           PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  P-HG           PIC Z9.
           02  F              PIC  X(003) VALUE " - ".
           02  P-ON           PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  P-OG           PIC Z9.
           02  F              PIC  N(015) VALUE
                "　年間工品製品受払表　　＊＊＊".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(025) VALUE
                "I-----  前期繰越  -----I ".
           02  F              PIC  X(029) VALUE
                "I--------  当期入庫  -------I".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(029) VALUE
                "I--------  当期出庫  -------I".
           02  F              PIC  X(025) VALUE
                " I-----  次期繰越  -----I".
       01  HEAD3.
           02  F              PIC  X(033) VALUE
                " ｺｰﾄﾞ 品　　名　　　　　　　     ".
           02  F              PIC  X(051) VALUE
                "数　量　　　金　額　       数　量　　　　 金　額   ".
           02  F              PIC  X(052) VALUE
                "      数　量　　　　 金　額       数　量　　　金　額".
       01  W-P.
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(001).
           02  P-NAME         PIC  X(020).
           02  P-ZS           PIC --,---,--9.99.
           02  P-ZK           PIC ----,---,--9.
           02  P-NS           PIC ----,---,--9.99.
           02  P-NK           PIC ---,---,---,--9.
           02  P-SS           PIC ----,---,--9.99.
           02  P-SK           PIC ---,---,---,--9.
           02  P-YS           PIC --,---,--9.99.
           02  P-YK           PIC ----,---,--9.
       01  W-D.
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  W-HCD          PIC  X(005).
           02  CHK            PIC  9(001).
       01  W-ST.
           02  WS-ZS          PIC S9(007)V9(02).
           02  WS-ZK          PIC S9(009).
           02  WS-NS          PIC S9(008)V9(02).
           02  WS-NK          PIC S9(011).
           02  WS-SS          PIC S9(008)V9(02).
           02  WS-SK          PIC S9(011).
           02  WS-YS          PIC S9(007)V9(02).
           02  WS-YK          PIC S9(009).
       01  W-TT.
           02  WT-ZS          PIC S9(007)V9(02).
           02  WT-ZK          PIC S9(009).
           02  WT-NS          PIC S9(008)V9(02).
           02  WT-NK          PIC S9(011).
           02  WT-SS          PIC S9(008)V9(02).
           02  WT-SK          PIC S9(011).
           02  WT-YS          PIC S9(007)V9(02).
           02  WT-YK          PIC S9(009).
       01  W-AT.
           02  WA-ZK          PIC S9(009).
           02  WA-NK          PIC S9(011).
           02  WA-SK          PIC S9(011).
           02  WA-YK          PIC S9(009).
       01  W-PC               PIC  9(002) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
           COPY LIBFDD.
           COPY LIKHM.
           COPY LSPF.
       01  KH-YF_KHY530.
           02  KH-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  KH-YF_LNAME    PIC  X(012)  VALUE "KH-YF_KHY530".
           02  F              PIC  X(001).
           02  KH-YF_KEY1     PIC  X(100)  VALUE SPACE.
           02  KH-YF_KEY2     PIC  X(100)  VALUE SPACE.
           02  KH-YF_SORT     PIC  X(100)  VALUE SPACE.
           02  KH-YF_IDLST    PIC  X(100)  VALUE SPACE.
           02  KH-YF_RES      USAGE  POINTER.
       01  KHY-R.
           02  Y-HCD          PIC  X(005).
           02  Y-YC           PIC  9(002).
           02  Y-NC           PIC  9(001).
           02  Y-ZS           PIC S9(007)V9(02).
           02  Y-ZK           PIC S9(009).
           02  Y-NS           PIC S9(008)V9(02).
           02  Y-NK           PIC S9(011).
           02  Y-SS           PIC S9(008)V9(02).
           02  Y-SK           PIC S9(011).
           02  Y-YS           PIC S9(007)V9(02).
           02  Y-YK           PIC S9(009).
           02  Y-NG.
             03  Y-SNEN       PIC  9(002).
             03  Y-SGET       PIC  9(002).
             03  Y-ENEN       PIC  9(002).
             03  Y-EGET       PIC  9(002).
           02  F              PIC  X(162).
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
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　年間工品製品受払表　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2     PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME98    PIC  X(005) VALUE X"1B4A05".
             03  E-ME99    PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "3" "10" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID" "N" "4" "10" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID" "N" "5" "10" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID" "N" "6" "10" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID" "N" "7" "10" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID" "N" "8" "10" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID" "N" "9" "10" "38" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "43" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "43" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO KH-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KH-YF_PNAME1 " " BY REFERENCE KH-YF_IDLST "0".
       M-10.
      *           READ KH-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KH-YF_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KH-YF_IDLST KH-YF_PNAME1
               CALL "SD_Output" USING "E-ME1" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                         RETURNING RESU
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  Y-YC = ZERO
               GO TO M-10
           END-IF.
           IF  ZERO = Y-ZS AND Y-ZK AND Y-NS AND Y-NK
                 AND Y-SS AND Y-SK AND Y-YS AND Y-YK
               GO TO M-10
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE Y-SNEN TO P-HN.
           MOVE Y-SGET TO P-HG.
           MOVE Y-ENEN TO P-ON.
           MOVE Y-EGET TO P-OG.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AT.
       M-20.
           MOVE ZERO TO W-TT.
           MOVE Y-YC TO W-YC.
       M-25.
           MOVE ZERO TO W-ST CHK.
           MOVE Y-NC TO W-NC.
       M-45.
           PERFORM S-20 THRU S-35.
       M-55.
      *           READ KH-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KH-YF_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  ZERO = Y-ZS AND Y-ZK AND Y-NS AND Y-NK
                 AND Y-SS AND Y-SK AND Y-YS AND Y-YK
               GO TO M-55
           END-IF.
           IF  Y-YC NOT = W-YC
               GO TO M-65
           END-IF.
           IF  Y-NC = W-NC
               GO TO M-45
           END-IF.
           PERFORM S-40 THRU S-50.
           GO TO M-25.
       M-65.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           GO TO M-20.
       M-90.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KH-YF_IDLST KH-YF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PC.
           MOVE W-PC TO H-PAGE.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P.
           MOVE Y-HCD TO P-HCD.
           MOVE Y-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " ***  KH-M ﾅｼ  ***  " TO KH-NAME
           END-IF.
           MOVE KH-NAME TO P-NAME.
           MOVE Y-ZS TO P-ZS.
           MOVE Y-ZK TO P-ZK.
           MOVE Y-NS TO P-NS.
           MOVE Y-NK TO P-NK.
           MOVE Y-SS TO P-SS.
           MOVE Y-SK TO P-SK.
           MOVE Y-YS TO P-YS.
           MOVE Y-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD Y-ZS TO WS-ZS.
           ADD Y-ZK TO WS-ZK.
           ADD Y-NS TO WS-NS.
           ADD Y-NK TO WS-NK.
           ADD Y-SS TO WS-SS.
           ADD Y-SK TO WS-SK.
           ADD Y-YS TO WS-YS.
           ADD Y-YK TO WS-YK.
           IF  CHK = 1
               MOVE 2 TO CHK
           END-IF.
           IF  CHK = 0
               MOVE 1 TO CHK
           END-IF.
       S-35.
           EXIT.
       S-40.
           IF  CHK < 2
               MOVE SPACE TO SP-R
               GO TO S-45
           END-IF.
           MOVE SPACE TO W-P.
           MOVE "        （ 小 計 ） " TO P-NAME.
           MOVE WS-ZS TO P-ZS.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-NS TO P-NS.
           MOVE WS-NK TO P-NK.
           MOVE WS-SS TO P-SS.
           MOVE WS-SK TO P-SK.
           MOVE WS-YS TO P-YS.
           MOVE WS-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
       S-45.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-ZS TO WT-ZS.
           ADD WS-ZK TO WT-ZK.
           ADD WS-NS TO WT-NS.
           ADD WS-NK TO WT-NK.
           ADD WS-SS TO WT-SS.
           ADD WS-SK TO WT-SK.
           ADD WS-YS TO WT-YS.
           ADD WS-YK TO WT-YK.
       S-50.
           EXIT.
       S-55.
           MOVE SPACE TO W-P.
           MOVE "    ［　合　計　］  " TO P-NAME.
           MOVE WT-ZS TO P-ZS.
           MOVE WT-ZK TO P-ZK.
           MOVE WT-NS TO P-NS.
           MOVE WT-NK TO P-NK.
           MOVE WT-SS TO P-SS.
           MOVE WT-SK TO P-SK.
           MOVE WT-YS TO P-YS.
           MOVE WT-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WT-ZK TO WA-ZK.
           ADD WT-NK TO WA-NK.
           ADD WT-SK TO WA-SK.
           ADD WT-YK TO WA-YK.
       S-60.
           EXIT.
       S-65.
           MOVE SPACE TO W-P.
           MOVE " 【　総　合　計　】 " TO P-NAME.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-NK TO P-NK.
           MOVE WA-SK TO P-SK.
           MOVE WA-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-70.
           EXIT.
