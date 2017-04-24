       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT277L.
      ***************************************************
      *    PROGRAM        : 得意先別受注残金額明細表    *
      *    DATA WRITTEN   : 05/01/12                    *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : JIPS                        *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  HEAD1.
           02  K-1                   PIC X(05)  VALUE  X"1A24212078".
           02  H-SEN                 PIC N(06).
           02  FILLER                PIC X(02)  VALUE   SPACE.
           02  F                     PIC N(04)  VALUE   "得意先別".
           02  H-PM                  PIC N(04)  VALUE   SPACE.
           02  F                     PIC N(08)  VALUE
                 "残　金額明細表　".
           02  FILLER                PIC X(02)  VALUE  SPACE.
           02  H-CHK                 PIC N(06)  VALUE  SPACE.
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(05)  VALUE  "DATE ".
           02  H-NGP                 PIC 99/99/99.
           02  FILLER                PIC X(05)  VALUE  "   P.".
           02  H-PAGE                PIC Z9.
       01  HEAD2.
           02  FILLER                PIC X(05)  VALUE  "ｺｰﾄﾞ ".
           02  FILLER                PIC N(08)  VALUE
                 "得　意　先　名　".
           02  FILLER                PIC X(33)  VALUE  SPACE.
           02  FILLER                PIC N(04)  VALUE  "　数　量".
           02  FILLER                PIC X(07)  VALUE  SPACE.
           02  FILLER                PIC N(04)  VALUE  "　金　額".
       01  W-P.
           02  P-TCD                 PIC 9(04).
           02  F                     PIC X(01).
           02  P-NAME                PIC N(26).
           02  P-SU                  PIC ----,---,--9.
           02  P-KIN                 PIC -----,---,--9.
       01  W-DATA.
           02  W-POC                 PIC 9(01).
           02  W-CHK                 PIC 9(01).
           02  W-PC                  PIC 9(01).
           02  W-SEN                 PIC 9(01).
           02  CNT                   PIC 9(02).
           02  W-PAGE                PIC 9(02).
           02  W-TCD                 PIC 9(04).
           02  W-SUD                 PIC S9(07).
           02  W-KIND                PIC S9(09).
           02  W-SU                  PIC S9(07).
           02  W-KIN                 PIC S9(09).
           02  WT-SU                 PIC S9(07).
           02  WT-KIN                PIC S9(09).
       COPY    LWMSG.
      *
           COPY  LTWK04.
           COPY  LITCM.
           COPY  LSPF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(28) VALUE
                     "                            ".
               03  FILLER  PIC  X(26) VALUE
                    "得意先別受注残　金額明細表".
       COPY    LSMSG.
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "54" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "23" "28" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "24" "26" "01DSP-01" " "
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           MOVE     ZERO    TO   W-DATA.
           ACCEPT         H-NGP       FROM     DATE.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
      *
      *           READ      JT-WK04           AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "SD_Output" USING
                "ERR-02" ERR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO  M-95
           END-IF
           MOVE  WK04-88    TO  W-CHK.
           MOVE  WK04-89    TO  W-PC,
           MOVE  WK04-90    TO  W-SEN.
      *
           PERFORM     SET-RTN     THRU     SET-EX.
       M-10.
           MOVE  WK04-04    TO  W-TCD.
           MOVE  ZERO       TO  W-SU  W-KIN.
       M-15.
           ADD   W-SUD      TO  W-SU.
           ADD   W-KIND     TO  W-KIN.
      *
      *           READ    JT-WK04                  AT      END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-80
           END-IF
           PERFORM     SET-RTN     THRU     SET-EX.
           IF  WK04-04        =   W-TCD
               GO  TO  M-15
           END-IF
           PERFORM     PRI-RTN     THRU     PRI-EX.
           GO  TO  M-10.
       M-80.
           PERFORM     PRI-RTN     THRU     PRI-EX.
           PERFORM     TOT-RTN     THRU     TOT-EX.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           IF  W-POC          =   1
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       MID-RTN.
           MOVE   SPACE     TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
           ADD    1         TO  W-PAGE.
           MOVE   W-PAGE    TO  H-PAGE.
           MOVE   HEAD1     TO  SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE     TO  SP-R.
           MOVE   HEAD2     TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE     TO  SP-R.
       MID-EX.
           EXIT.
       SET-RTN.
           MOVE ZERO TO CNT W-SUD W-KIND.
       SET-010.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO SET-020
           END-IF
           ADD WK04-1111(CNT) TO W-SUD.
           SUBTRACT WK04-1211(CNT) FROM W-SUD.
           SUBTRACT WK04-141(CNT) FROM W-SUD.
           IF  W-CHK    =   1
               SUBTRACT WK04-151(CNT) FROM W-SUD
           END-IF
           GO TO SET-010.
       SET-020.
           COMPUTE  W-KIND  =  W-SUD  *  WK04-17.
       SET-EX.
           EXIT.
       PRI-RTN.
           IF  ZERO     =   W-SU  AND  W-KIN
               GO  TO  PRI-EX
           END-IF
           IF  W-POC    =   1
               GO  TO  PRI-010
           END-IF
           MOVE  1               TO  W-POC.
           IF  W-SEN      =   0
               MOVE  "【教　育】"   TO  H-SEN
           END-IF
           IF  W-SEN      =   1
               MOVE  "【一　般】"   TO  H-SEN
           END-IF
           IF  W-SEN      =   9
               MOVE  ALL  "　"    TO  H-SEN
           END-IF
           IF  W-PC     =  0
               MOVE  "　　受注"    TO  H-PM
           END-IF
           IF  W-PC     =  5
               MOVE  "　　預り"    TO  H-PM
           END-IF
           IF  W-PC     =  6
               MOVE  "　取よけ"    TO  H-PM
           END-IF
           IF  W-CHK    =   1
               MOVE  "　（指図含）"   TO  H-CHK
           ELSE
               MOVE  ALL  "　"      TO  H-CHK
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM     MID-010    THRU   MID-EX.
       PRI-010.
           MOVE   W-TCD    TO    TC-TCD.
           MOVE   001      TO    TC-CCD.
      *           READ   TC-M    UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "　"  TO    TC-NAME
           END-IF
      *
           MOVE   SPACE    TO    W-P.
           MOVE   W-TCD    TO    P-TCD.
           MOVE   TC-NAME  TO    P-NAME.
           MOVE   W-SU     TO    P-SU.
           MOVE   W-KIN    TO    P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER   >  62
               PERFORM     MID-010    THRU   MID-EX
           END-IF
           MOVE   SPACE    TO    SP-R.
           MOVE   W-P      TO    SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO    SP-R.
      *
           ADD    W-SU     TO    WT-SU.
           ADD    W-KIN    TO    WT-KIN.
       PRI-EX.
           EXIT.
       TOT-RTN.
           IF  W-POC    =   0
               GO  TO  TOT-EX
           END-IF
      *
           MOVE   SPACE    TO    W-P.
           MOVE   "　　　　　　　　　　【　合　計　】"  TO    P-NAME.
           MOVE   WT-SU    TO    P-SU.
           MOVE   WT-KIN   TO    P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER   >  62
               PERFORM     MID-010    THRU   MID-EX
           END-IF
           MOVE   SPACE    TO    SP-R.
           MOVE   W-P      TO    SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE    TO    SP-R.
       TOT-EX.
           EXIT.
