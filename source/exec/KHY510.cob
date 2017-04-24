       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY510.
      *********************************************************
      *    PROGRAM         :  年間用途区分別製品受払表　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(032) VALUE SPACE.
           02  F              PIC  N(005) VALUE "＊＊＊　　".
           02  P-SNEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  P-SGET         PIC Z9.
           02  F              PIC  X(003) VALUE " - ".
           02  P-ENEN         PIC  9(002).
           02  F              PIC  X(001) VALUE "/".
           02  P-EGET         PIC Z9.
           02  F              PIC  N(022) VALUE
                "　年間　工品用途区分別　製品受払表　　＊＊＊".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(026) VALUE
                "I-----  前期繰越   -----I ".
           02  F              PIC  X(029) VALUE
                "I--------  当期入庫  -------I".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(029) VALUE
                "I--------  当期出庫  -------I".
           02  F              PIC  X(026) VALUE
                " I------  次期繰越  -----I".
       01  HEAD3.
           02  F              PIC  N(015) VALUE
                "用　途　区　分　名　　　　　　".
           02  F              PIC  X(050) VALUE
                "  数　量　　　金　額         数　量　　　 　金　額".
           02  F              PIC  X(030) VALUE
                "         数　量　　　 　金　額".
           02  F              PIC  X(026) VALUE
                "        数　量　　　金　額".
       01  W-P1.
           02  P-15K1         PIC  X(005).
           02  P-YCN          PIC  N(016).
           02  F              PIC  X(112).
       01  W-P2.
           02  P-15K2         PIC  X(005).
           02  P-M            PIC  N(016).
           02  P-ZS           PIC ---,---,--9.99.
           02  P-ZK           PIC ----,---,--9.
           02  P-NS           PIC ----,---,--9.99.
           02  P-NK           PIC ---,---,---,--9.
           02  P-SS           PIC ----,---,--9.99.
           02  P-UG           PIC ---,---,---,--9.
           02  P-YS           PIC ---,---,--9.99.
           02  P-YK           PIC ----,---,--9.
           02  P-20K          PIC  X(005).
       01  W-D.
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  CHK            PIC  9(001).
       01  W-ST.
           02  WS-ZS          PIC S9(007)V9(02).
           02  WS-ZK          PIC S9(009).
           02  WS-NS          PIC S9(008)V9(02).
           02  WS-NK          PIC S9(011).
           02  WS-SS          PIC S9(008)V9(02).
           02  WS-UG          PIC S9(011).
           02  WS-YS          PIC S9(007)V9(02).
           02  WS-YK          PIC S9(009).
       01  W-TT.
           02  WT-ZS          PIC S9(007)V9(02).
           02  WT-ZK          PIC S9(009).
           02  WT-NS          PIC S9(008)V9(02).
           02  WT-NK          PIC S9(011).
           02  WT-SS          PIC S9(008)V9(02).
           02  WT-UG          PIC S9(011).
           02  WT-YS          PIC S9(007)V9(02).
           02  WT-YK          PIC S9(009).
       01  W-GT.
           02  W-GTD   OCCURS   2.
             03  WG-ZK        PIC S9(009).
             03  WG-NK        PIC S9(011).
             03  WG-UG        PIC S9(011).
             03  WG-YK        PIC S9(009).
       01  W-AT.
           02  WA-ZK          PIC S9(009).
           02  WA-NK          PIC S9(011).
           02  WA-UG          PIC S9(011).
           02  WA-YK          PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LIBFDD.
           COPY LIKKBM.
           COPY LSPF.
       01  KH-YF_KHY510.
           02  KH-YF_PNAME1   PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  KH-YF_LNAME    PIC  X(012)  VALUE "KH-YF_KHY510".
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
           02  Y-ZS           PIC S9(006)V9(02).
           02  Y-ZK           PIC S9(008).
           02  Y-NS           PIC S9(006)V9(02).
           02  Y-NK           PIC S9(008).
           02  Y-SS           PIC S9(006)V9(02).
           02  Y-SK           PIC S9(008).
           02  Y-YS           PIC S9(006)V9(02).
           02  Y-YK           PIC S9(008).
           02  Y-UG           PIC S9(008).
           02  Y-NG           PIC  9(006).
           02  Y-SNG.
             03  F            PIC  9(002).
             03  Y-SNEN       PIC  9(002).
             03  Y-SGET       PIC  9(002).
           02  Y-ENG.
             03  F            PIC  9(002).
             03  Y-ENEN       PIC  9(002).
             03  Y-EGET       PIC  9(002).
           02  F              PIC  X(030).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　年間工品用途区分別製品受払表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" 
                                         RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO KH-YF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KH-YF_PNAME1 " " BY REFERENCE KH-YF_IDLST "0".
       M-35.
      *           READ KH-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KH-YF_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KH-YF_IDLST KH-YF_PNAME1
               CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                         RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           IF  Y-YC = ZERO
               GO TO M-35
           END-IF.
           IF  ZERO = Y-ZS AND Y-ZK AND Y-YS AND Y-YK AND
                     Y-NS AND Y-NK AND Y-SS AND Y-UG
               GO TO M-35
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE Y-SNEN TO P-SNEN.
           MOVE Y-SGET TO P-SGET.
           MOVE Y-ENEN TO P-ENEN.
           MOVE Y-EGET TO P-EGET.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AT W-GT.
       M-40.
           MOVE ZERO TO W-TT CHK.
           MOVE Y-YC TO W-YC.
           MOVE SPACE TO KKB-KEY.
           MOVE 01 TO KKB-NO.
           MOVE W-YC TO KKB-YC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KKB-YCN
           END-IF.
       M-45.
           MOVE ZERO TO W-ST.
           MOVE Y-NC TO W-NC.
       M-50.
           IF  Y-SNG = Y-NG
               ADD Y-ZS TO WS-ZS
               ADD Y-ZK TO WS-ZK
           END-IF.
           IF  Y-ENG = Y-NG
               ADD Y-YS TO WS-YS
               ADD Y-YK TO WS-YK
           END-IF.
           ADD Y-NS TO WS-NS.
           ADD Y-NK TO WS-NK.
           ADD Y-SS TO WS-SS.
           ADD Y-UG TO WS-UG.
       M-55.
      *           READ KH-YF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" KH-YF_PNAME1 BY REFERENCE KHY-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
           IF  ZERO = Y-ZS AND Y-ZK AND Y-YS AND Y-YK AND
                     Y-NS AND Y-NK AND Y-SS AND Y-UG
               GO TO M-55
           END-IF.
           IF  Y-YC NOT = W-YC
               GO TO M-60
           END-IF.
           IF  Y-NC = W-NC
               GO TO M-50
           END-IF.
           PERFORM S-30 THRU S-35.
           GO TO M-45.
       M-60.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           GO TO M-40.
       M-90.
           PERFORM S-30 THRU S-35.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-65.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KH-YF_IDLST KH-YF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
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
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           MOVE SPACE TO P-YCN.
           MOVE KKB-YCN TO P-YCN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           IF  ZERO = WS-ZS AND WS-ZK AND WS-NS AND WS-NK AND
                     WS-SS AND WS-UG AND WS-YS AND WS-YK
               GO TO S-35
           END-IF.
           ADD 1 TO CHK.
           IF  CHK = 1
               PERFORM S-20 THRU S-25
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-M.
           IF  W-NC = 0
               MOVE "　　　　　　　　　　　　内　作　" TO P-M
           END-IF.
           IF  W-NC = 1
               MOVE "　　　　　　　　　　　　仕　入　" TO P-M
           END-IF.
           MOVE WS-ZS TO P-ZS.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-NS TO P-NS.
           MOVE WS-NK TO P-NK.
           MOVE WS-SS TO P-SS.
           MOVE WS-UG TO P-UG.
           MOVE WS-YS TO P-YS.
           MOVE WS-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD WS-ZS TO WT-ZS.
           ADD WS-ZK TO WT-ZK.
           ADD WS-NS TO WT-NS.
           ADD WS-NK TO WT-NK.
           ADD WS-SS TO WT-SS.
           ADD WS-UG TO WT-UG.
           ADD WS-YS TO WT-YS.
           ADD WS-YK TO WT-YK.
           IF  W-NC = 0
               ADD WS-ZK TO WG-ZK(1)
               ADD WS-NK TO WG-NK(1)
               ADD WS-UG TO WG-UG(1)
               ADD WS-YK TO WG-YK(1)
           END-IF.
           IF  W-NC = 1
               ADD WS-ZK TO WG-ZK(2)
               ADD WS-NK TO WG-NK(2)
               ADD WS-UG TO WG-UG(2)
               ADD WS-YK TO WG-YK(2)
           END-IF.
       S-35.
           EXIT.
       S-40.
           IF  CHK < 2
               GO TO S-45
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-M.
           MOVE "　　　　　　　　（　小　計　）　" TO P-M.
           MOVE WT-ZS TO P-ZS.
           MOVE WT-ZK TO P-ZK.
           MOVE WT-NS TO P-NS.
           MOVE WT-NK TO P-NK.
           MOVE WT-SS TO P-SS.
           MOVE WT-UG TO P-UG.
           MOVE WT-YS TO P-YS.
           MOVE WT-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-45.
           ADD WT-ZK TO WA-ZK.
           ADD WT-NK TO WA-NK.
           ADD WT-UG TO WA-UG.
           ADD WT-YK TO WA-YK.
       S-50.
           EXIT.
       S-55.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K.
           MOVE "　　　　【　総　合　計　】　　　" TO P-M.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-NK TO P-NK.
           MOVE WA-UG TO P-UG.
           MOVE WA-YK TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE 0 TO CHK.
       S-60.
           ADD 1 TO CHK.
           IF  CHK > 2
               GO TO S-65
           END-IF.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-M.
           IF  CHK = 1
               MOVE "　　　　　　　　　　　　内　作　" TO P-M
           END-IF.
           IF  CHK = 2
               MOVE "　　　　　　　　　　　　仕　入　" TO P-M
           END-IF.
           MOVE WG-ZK(CHK) TO P-ZK.
           MOVE WG-NK(CHK) TO P-NK.
           MOVE WG-UG(CHK) TO P-UG.
           MOVE WG-YK(CHK) TO P-YK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM S-05 THRU S-15
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-60.
       S-65.
           EXIT.
