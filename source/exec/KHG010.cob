       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHG010.
      *********************************************************
      *    PROGRAM         :  用途区分別製品受払表(原価)      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/06                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　工品　用途区分別　製品受払表　".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(046) VALUE
                "I---  前月繰越　--I I------  当月入庫  -----I ".
           02  F              PIC  X(046) VALUE
                "I------  当月出庫  -----I I---  次月繰越  --I ".
           02  F              PIC  X(019) VALUE "I----  増 減  ----I".
       01  HEAD3.
           02  F              PIC  X(044) VALUE
                "用　途　区　分　名         数　量　 金　　額".
           02  F              PIC  X(052) VALUE
                "        数　量　　金　　額　　    数　量　　金　　額".
           02  F              PIC  X(040) VALUE
                "   数　量　 金　　額　 数　量　 金　　額".
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
       01  W-P.
           02  P-YCN          PIC  N(012).
           02  P-NM           PIC  N(004).
           02  P-ZS           PIC -----,--9.
           02  P-ZK           PIC ---,---,--9.
           02  P-NS           PIC ---,---,--9.99.
           02  P-NK           PIC ----,---,--9.
           02  P-SS           PIC ---,---,--9.99.
           02  P-SK           PIC ----,---,--9.
           02  P-YS           PIC -----,--9.
           02  P-YK           PIC ---,---,--9.
           02  P-HS           PIC -----,--9.
           02  P-HK           PIC ---,---,--9.
       01  W-DATA.
           02  W-BCD          PIC  9(001).
           02  W-BC           PIC  9(001).
           02  W-YC.
             03  W-YC1        PIC  9(001).
             03  W-YC2        PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-C            PIC  9(001).
           02  W-CD           PIC  9(001).
           02  W-NS           PIC S9(006)V9(02).
           02  W-YS           PIC S9(006)V9(02).
           02  W-YK           PIC S9(008).
           02  W-HS           PIC S9(006)V9(02).
           02  W-HK           PIC S9(008).
       01  W-NT.
           02  WN-ZS          PIC S9(006)V9(02).
           02  WN-ZK          PIC S9(008).
           02  WN-NS          PIC S9(007)V9(02).
           02  WN-NK          PIC S9(009).
           02  WN-SS          PIC S9(007)V9(02).
           02  WN-SK          PIC S9(009).
           02  WN-YS          PIC S9(006)V9(02).
           02  WN-YK          PIC S9(008).
           02  WN-HS          PIC S9(006)V9(02).
           02  WN-HK          PIC S9(008).
       01  W-ST.
           02  WS-ZS          PIC S9(006)V9(02).
           02  WS-ZK          PIC S9(008).
           02  WS-NS          PIC S9(007)V9(02).
           02  WS-NK          PIC S9(009).
           02  WS-SS          PIC S9(007)V9(02).
           02  WS-SK          PIC S9(009).
           02  WS-YS          PIC S9(006)V9(02).
           02  WS-YK          PIC S9(008).
           02  WS-HS          PIC S9(006)V9(02).
           02  WS-HK          PIC S9(008).
       01  W-KT.
           02  WK-ZK          PIC S9(008).
           02  WK-NK          PIC S9(009).
           02  WK-SK          PIC S9(009).
           02  WK-YK          PIC S9(008).
           02  WK-HK          PIC S9(008).
       01  W-TT.
           02  WT-ZK          PIC S9(008).
           02  WT-NK          PIC S9(009).
           02  WT-SK          PIC S9(009).
           02  WT-YK          PIC S9(008).
           02  WT-HK          PIC S9(008).
       01  W-GT.
           02  W-GTD   OCCURS  2.
             03  WG-ZK        PIC S9(008).
             03  WG-NK        PIC S9(009).
             03  WG-SK        PIC S9(009).
             03  WG-YK        PIC S9(008).
             03  WG-HK        PIC S9(008).
       01  W-AT.
           02  WA-ZK          PIC S9(008).
           02  WA-NK          PIC S9(009).
           02  WA-SK          PIC S9(009).
           02  WA-YK          PIC S9(008).
           02  WA-HK          PIC S9(008).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKKBM.
           COPY LIKHT2.
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
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　工品用途区分別製品受払表　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(030) VALUE
                  "***  DATA ﾅｼ  ***             ".
             03  E-ME2   PIC  X(030) VALUE
                  "***  KHM ﾅｼ  ***              ".
             03  E-KEY   PIC  X(005).
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
            "C-MID" " " "0" "0" "308" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "75" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "75" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "30" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "30" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           MOVE DATE-03R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
           CALL "PR_Open" RETURNING RESP.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-10
           END-IF
           IF  KHT-YC < 20
               MOVE 0 TO W-BCD
           ELSE
               MOVE 1 TO W-BCD
           END-IF
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO W-AT W-GT.
       M-15.
           MOVE W-BCD TO W-BC.
           MOVE ZERO TO W-TT.
       M-20.
           MOVE ZERO TO W-KT W-CD.
           MOVE KHT-YC TO W-YC.
       M-25.
           MOVE ZERO TO W-ST W-C.
           MOVE KHT-YC2 TO W-YC2.
      *
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
       M-30.
           MOVE ZERO TO W-NT.
           MOVE KHT-NC TO W-NC.
       M-35.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  KHT-YC NOT = 10 AND 11
               IF  KH-GT1 = ZERO
                   MOVE ZERO TO KHT-SSU
               END-IF
           END-IF
      *
           COMPUTE W-NS = KHT-KSU - KHT-HSU + KHT-ISU.
           COMPUTE W-YS = KHT-ZSU + W-NS - KHT-SSU.
           COMPUTE W-YK = W-YS * KH-GT1.
           COMPUTE W-HS = W-YS - KHT-ZSU.
           COMPUTE W-HK = W-YK - KHT-ZKIN.
      *
           ADD KHT-ZSU TO WN-ZS
           ADD KHT-ZKIN TO WN-ZK.
           ADD W-NS TO WN-NS.
           ADD KHT-KKIN TO WN-NK.
           ADD KHT-SSU TO WN-SS.
           ADD KHT-GKIN TO WN-SK.
           ADD W-YS TO WN-YS.
           ADD W-YK TO WN-YK.
           ADD W-HS TO WN-HS.
           ADD W-HK TO WN-HK.
       M-40.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC < 20
               MOVE 0 TO W-BCD
           ELSE
               MOVE 1 TO W-BCD
           END-IF
           IF  W-BCD NOT = W-BC
               GO TO M-55
           END-IF
           IF  KHT-YC1 NOT = W-YC1
               GO TO M-50
           END-IF
           IF  KHT-YC2 NOT = W-YC2
               GO TO M-45
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-35
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-30.
       M-45.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
           GO TO M-25.
       M-50.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           GO TO M-20.
       M-55.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
           GO TO M-15.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
           PERFORM S-55 THRU S-60.
           PERFORM S-65 THRU S-70.
           PERFORM S-75 THRU S-80.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
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
           IF  ZERO = WN-ZS AND WN-ZK AND WN-NS AND WN-NK AND
                     WN-SS AND WN-SK AND WN-YS AND WN-YK
               GO TO S-25
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE 1 TO W-CD.
           ADD 1 TO W-C.
           IF  W-C = 1
               MOVE KKB-YCN TO P-YCN
           END-IF
           IF  W-NC = 0
               MOVE "内　作　" TO P-NM
           END-IF
           IF  W-NC = 1
               MOVE "仕　入　" TO P-NM
           END-IF
           MOVE WN-ZS TO P-ZS.
           MOVE WN-ZK TO P-ZK.
           MOVE WN-NS TO P-NS.
           MOVE WN-NK TO P-NK.
           MOVE WN-SS TO P-SS.
           MOVE WN-SK TO P-SK.
           MOVE WN-YS TO P-YS.
           MOVE WN-YK TO P-YK.
           MOVE WN-HS TO P-HS.
           MOVE WN-HK TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE KKB-YCN TO P-YCN
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WN-ZS TO WS-ZS.
           ADD WN-ZK TO WS-ZK.
           ADD WN-NS TO WS-NS.
           ADD WN-NK TO WS-NK.
           ADD WN-SS TO WS-SS.
           ADD WN-SK TO WS-SK.
           ADD WN-YS TO WS-YS.
           ADD WN-YK TO WS-YK.
           ADD WN-HS TO WS-HS.
           ADD WN-HK TO WS-HK.
           IF  W-NC = 0
               ADD WN-ZK TO WG-ZK(1)
               ADD WN-NK TO WG-NK(1)
               ADD WN-SK TO WG-SK(1)
               ADD WN-YK TO WG-YK(1)
               ADD WN-HK TO WG-HK(1)
           END-IF
           IF  W-NC = 1
               ADD WN-ZK TO WG-ZK(2)
               ADD WN-NK TO WG-NK(2)
               ADD WN-SK TO WG-SK(2)
               ADD WN-YK TO WG-YK(2)
               ADD WN-HK TO WG-HK(2)
           END-IF.
       S-25.
           EXIT.
       S-40.
           IF  W-C < 2
               GO TO S-45
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE "　　　　　　　（　計　）" TO P-YCN.
           MOVE WS-ZS TO P-ZS.
           MOVE WS-ZK TO P-ZK.
           MOVE WS-NS TO P-NS.
           MOVE WS-NK TO P-NK.
           MOVE WS-SS TO P-SS.
           MOVE WS-SK TO P-SK.
           MOVE WS-YS TO P-YS.
           MOVE WS-YK TO P-YK.
           MOVE WS-HS TO P-HS.
           MOVE WS-HK TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-45.
           ADD WS-ZK TO WK-ZK.
           ADD WS-NK TO WK-NK.
           ADD WS-SK TO WK-SK.
           ADD WS-YK TO WK-YK.
           ADD WS-HK TO WK-HK.
       S-50.
           EXIT.
       S-55.
           IF  W-CD = 0
               GO TO S-60
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE "　　　　＜　小　計　＞　" TO P-YCN.
           MOVE WK-ZK TO P-ZK.
           MOVE WK-NK TO P-NK.
           MOVE WK-SK TO P-SK.
           MOVE WK-YK TO P-YK.
           MOVE WK-HK TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WK-ZK TO WT-ZK.
           ADD WK-NK TO WT-NK.
           ADD WK-SK TO WT-SK.
           ADD WK-YK TO WT-YK.
           ADD WK-HK TO WT-HK.
       S-60.
           EXIT.
       S-65.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE "　　［　合　計　］　　　" TO P-YCN.
           MOVE WT-ZK TO P-ZK.
           MOVE WT-NK TO P-NK.
           MOVE WT-SK TO P-SK.
           MOVE WT-YK TO P-YK.
           MOVE WT-HK TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WT-ZK TO WA-ZK.
           ADD WT-NK TO WA-NK.
           ADD WT-SK TO WA-SK.
           ADD WT-YK TO WA-YK.
           ADD WT-HK TO WA-HK.
       S-70.
           EXIT.
       S-75.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE "【　　総　合　計　　】　" TO P-YCN.
           MOVE WA-ZK TO P-ZK.
           MOVE WA-NK TO P-NK.
           MOVE WA-SK TO P-SK.
           MOVE WA-YK TO P-YK.
           MOVE WA-HK TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE "内　作　" TO P-NM.
           MOVE WG-ZK(1) TO P-ZK.
           MOVE WG-NK(1) TO P-NK.
           MOVE WG-SK(1) TO P-SK.
           MOVE WG-YK(1) TO P-YK.
           MOVE WG-HK(1) TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-YCN P-NM.
           MOVE "仕　入　" TO P-NM.
           MOVE WG-ZK(2) TO P-ZK.
           MOVE WG-NK(2) TO P-NK.
           MOVE WG-SK(2) TO P-SK.
           MOVE WG-YK(2) TO P-YK.
           MOVE WG-HK(2) TO P-HK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-80.
           EXIT.
