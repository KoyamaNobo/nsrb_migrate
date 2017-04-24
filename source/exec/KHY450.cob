       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHY450.
      *************************************************************
      *    PROGRAM         :  年間工品・材料得意先品種別売上集計表*
      *    PRINTER TYPE    :  JIPS                                *
      *    SCREEN          :  ******                              *
      *    COMPILE TYPE    :  COBOL                               *
      *************************************************************
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
           02  F              PIC  X(003) VALUE "( '".
           02  H-SNG          PIC 99/99.
           02  F              PIC  X(005) VALUE " ～ '".
           02  H-ENG          PIC 99/99.
           02  F              PIC  X(002) VALUE " )".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　年間　工品他　得意先品名別　売上集計表".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "粗　　利".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "率　".
           02  F              PIC  X(001) VALUE "%".
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  P-20K          PIC  X(005).
           02  F              PIC  X(001).
           02  P-HCD          PIC  X(005).
           02  F              PIC  X(001).
           02  P-HNA          PIC  X(020).
           02  P-TMD   REDEFINES P-HNA.
             03  P-TM         PIC  N(010).
           02  P-SU           PIC ----,---,--9.99.
           02  P-T            PIC ----,--9.99.
           02  P-KIN          PIC --,---,---,--9.
           02  P-AR           PIC ----,---,---.
           02  P-RR           PIC ----9.9.
       01  W-PJ.
           02  P-15KJ         PIC  X(005).
           02  F              PIC  X(050).
           02  P-F            PIC  X(001).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  P-R            PIC  X(001).
           02  F              PIC  X(033).
       01  W-DATA.
           02  W-BC           PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  X(005).
           02  W-TC           PIC  9(001).
           02  W-JC           PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-D.
             03  W-SU         PIC S9(007)V9(02).
             03  W-T          PIC S9(006)V9(02).
             03  W-KIN        PIC S9(010).
             03  W-UG         PIC S9(010).
             03  W-AR         PIC S9(009).
             03  W-RR         PIC S9(003)V9(02).
           02  WN-D.
             03  WN-KIN       PIC S9(010).
             03  WN-UG        PIC S9(010).
             03  WN-AR        PIC S9(009).
           02  WS-D.
             03  WS-KIN       PIC S9(010).
             03  WS-UG        PIC S9(010).
             03  WS-AR        PIC S9(009).
           02  WA-D.
             03  WA-KIN       PIC S9(010).
           02  W-PAGE         PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIKHM.
           COPY LIJM.
           COPY LSPF.
      *FD  URIR-F
       01  URIR-F_KHY450.
           02  URIR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  URIR-F_LNAME   PIC  X(013) VALUE "URIR-F_KHY450".
           02  F              PIC  X(001).
           02  URIR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  URIR-F_SORT    PIC  X(100) VALUE SPACE.
           02  URIR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  URIR-F_RES     USAGE  POINTER.
       01  URIR-R.
           02  URIR-DC        PIC  9(001).
           02  F              PIC  X(008).
           02  URIR-TCD       PIC  9(004).
           02  URIR-HCD       PIC  X(005).
           02  URIR-SU        PIC S9(006)V9(02).
           02  URIR-T         PIC S9(006)V9(02).
           02  URIR-KIN       PIC S9(008).
           02  F              PIC  X(021).
           02  URIR-SNG       PIC  9(004).
           02  F              PIC  X(002).
           02  URIR-ENG       PIC  9(004).
           02  URIR-JCD       PIC  9(006).
           02  URIR-GT        PIC  9(006)V9(02).
           02  F              PIC  X(036).
           02  URIR-BC        PIC  9(001).
           02  F              PIC  X(004).
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
                "＊＊＊　工品他　得意先品名別　売上集計表　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "336" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "60" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "60" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO URIR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" URIR-F_PNAME1 " " BY REFERENCE URIR-F_IDLST "0".
       M-10.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  URIR-DC = 4 OR 5 OR 9
               GO TO M-10
           END-IF
           MOVE URIR-SNG TO H-SNG.
           MOVE URIR-ENG TO H-ENG.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D.
           MOVE URIR-BC TO W-BC.
       M-20.
           MOVE ZERO TO WN-D W-TC W-PC.
           MOVE URIR-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF.
       M-25.
           MOVE ZERO TO W-D W-JC.
           MOVE URIR-HCD TO W-HCD.
           MOVE W-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KH-NAME
           END-IF
           IF  URIR-JCD = ZERO
               GO TO M-30
           END-IF
           MOVE 1 TO W-JC.
           MOVE URIR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
           END-IF.
       M-30.
           IF  URIR-DC NOT = 8
               ADD URIR-SU TO W-SU
               ADD URIR-KIN TO W-KIN
           ELSE
               SUBTRACT URIR-KIN FROM W-KIN
               GO TO M-35
           END-IF
           IF  W-TCD = 4745 OR 4758
               ADD URIR-KIN TO W-UG
           ELSE
               IF  URIR-JCD = ZERO
                   COMPUTE W-UG = W-UG + (URIR-SU * URIR-GT)
               ELSE
                   ADD URIR-KIN TO W-UG
               END-IF
           END-IF.
       M-35.
      *           READ URIR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URIR-F_PNAME1 BY REFERENCE URIR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  URIR-DC = 4 OR 5 OR 9
               GO TO M-35
           END-IF
           IF  URIR-BC NOT = W-BC
               GO TO M-45
           END-IF
           IF  URIR-TCD NOT = W-TCD
               GO TO M-40
           END-IF
           IF  URIR-HCD = W-HCD
               IF  W-JC = 0
                   GO TO M-30
               END-IF
           END-IF
      *
           PERFORM S-20 THRU S-30.
           GO TO M-25.
       M-40.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-50.
           GO TO M-20.
       M-45.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-50.
           PERFORM S-55 THRU S-65.
           GO TO M-15.
       M-50.
           PERFORM S-20 THRU S-30.
           PERFORM S-35 THRU S-50.
           PERFORM S-55 THRU S-65.
           PERFORM S-70 THRU S-75.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE URIR-F_IDLST URIR-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
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
           IF  W-SU NOT = ZERO
               IF  W-KIN NOT = ZERO
                   COMPUTE W-T ROUNDED = W-KIN / W-SU
               END-IF
           END-IF
           COMPUTE W-AR = W-KIN - W-UG.
           IF  W-KIN NOT = ZERO
               IF  W-AR NOT = ZERO
                   COMPUTE W-RR ROUNDED = (W-AR / W-KIN) * 100
               END-IF
           END-IF
           IF  W-AR NOT = ZERO
               IF  W-KIN = ZERO
                   MOVE 100 TO W-RR
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-TNA.
           IF  W-TC = 0
               MOVE 1 TO W-TC
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
           END-IF
           MOVE W-HCD TO P-HCD.
           MOVE KH-NAME TO P-HNA.
           IF (W-SU NOT = ZERO) OR (W-T NOT = ZERO)
               MOVE W-SU TO P-SU
               MOVE W-T TO P-T
           END-IF
           MOVE W-KIN TO P-KIN.
           MOVE W-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-KIN TO WN-KIN.
           ADD W-UG TO WN-UG.
           ADD W-AR TO WN-AR.
           IF  W-PC = 1
               MOVE 2 TO W-PC
           END-IF
           IF  W-PC = 0
               MOVE 1 TO W-PC
           END-IF
      *
           IF  W-JC = 0
               GO TO S-30
           END-IF
           MOVE SPACE TO W-PJ.
           MOVE W-15K TO P-15KJ.
           MOVE SPACE TO P-JNA.
           MOVE "(" TO P-F.
           MOVE J-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           MOVE ")" TO P-R.
           MOVE SPACE TO SP-R.
           MOVE W-PJ TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-30.
           EXIT.
       S-35.
           IF  W-PC NOT = 2
               GO TO S-45
           END-IF
           MOVE ZERO TO W-RR.
           IF  WN-KIN NOT = ZERO
               IF  WN-AR NOT = ZERO
                   COMPUTE W-RR ROUNDED = (WN-AR / WN-KIN) * 100
               END-IF
           END-IF
           IF  WN-AR NOT = ZERO
               IF  WN-KIN = ZERO
                   MOVE 100 TO W-RR
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE SPACE TO P-TNA.
           MOVE "　（　ＴＯＴＡＬ　）" TO P-TM.
           MOVE WN-KIN TO P-KIN.
           MOVE WN-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE W-TCD TO P-TCD
               MOVE T-NAME TO P-TNA
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-45.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WN-KIN TO WS-KIN.
           ADD WN-UG TO WS-UG.
           ADD WN-AR TO WS-AR.
       S-50.
           EXIT.
       S-55.
           MOVE ZERO TO W-RR.
           IF  WS-KIN NOT = ZERO
               IF  WS-AR NOT = ZERO
                   COMPUTE W-RR ROUNDED = (WS-AR / WS-KIN) * 100
               END-IF
           END-IF
           IF  WS-AR NOT = ZERO
               IF  WS-KIN = ZERO
                   MOVE 100 TO W-RR
               END-IF
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　　　　　　　［　ＳＵＢ　ＴＯＴＡＬ　］" TO P-TNA.
           MOVE WS-KIN TO P-KIN.
           MOVE WS-AR TO P-AR.
           IF  W-RR NOT = ZERO
               MOVE W-RR TO P-RR
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           ADD WS-KIN TO WA-KIN.
       S-65.
           EXIT.
       S-70.
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE W-20K TO P-20K.
           MOVE "　【　　ＡＬＬ　ＴＯＴＡＬ　　】" TO P-TNA.
           MOVE WA-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
           EXIT.
