       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT750L.
      ***************************************************
      *    PROGRAM        : 受注数合計表（得意先品名別）*
      *    DATA WRITTEN   : 98/02/12                    *
      *    SCREEN USED    : UNUSED                      *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT           PIC X(02)    VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-20K          PIC X(05)  VALUE  X"1A24212474".
           02  F              PIC X(35)  VALUE   SPACE.
           02  FILLER         PIC N(05)  VALUE  "＊＊＊　　".
           02  FILLER         PIC X(01)  VALUE  "'".
           02  H-NEN          PIC 9(02).
           02  FILLER         PIC N(01)  VALUE  "年".
           02  H-GET          PIC Z9.
           02  FILLER         PIC N(02)  VALUE  "月分".
           02  FILLER         PIC N(18)  VALUE
                "　得意先品名別　受注合計表　　＊＊＊".
           02  F              PIC X(22)  VALUE   SPACE.
           02  H-YY           PIC 9(02).
           02  FILLER         PIC N(01)  VALUE  "年".
           02  H-MM           PIC Z9.
           02  FILLER         PIC N(01)  VALUE  "月".
           02  H-DD           PIC Z9.
           02  FILLER         PIC N(01)  VALUE  "日".
           02  FILLER         PIC X(07)  VALUE  "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC X(05)  VALUE  X"1A24212078".
           02  FILLER         PIC X(05)  VALUE  "ｺｰﾄﾞ ".
           02  FILLER         PIC N(08)  VALUE   "得　意　先　名　".
           02  FILLER         PIC X(119) VALUE  SPACE.
       01  HEAD3.
           02  FILLER         PIC X(08)  VALUE  "  ｺｰﾄﾞ  ".
           02  FILLER         PIC N(06)  VALUE  "品　　　名　".
           02  FILLER         PIC X(28)  VALUE  SPACE.
           02  FILLER         PIC X(01)  VALUE  "1".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "３号".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "２号".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "１号".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "０号".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "　中".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "　大".
           02  FILLER         PIC X(05)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "特大".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "28.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "29.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "30.0".
           02  FILLER         PIC X(10)  VALUE  SPACE.
       01  HEAD4.
           02  FILLER         PIC X(45)  VALUE  SPACE.
           02  FILLER         PIC X(01)  VALUE  "2".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "12.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "13.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "13.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "14.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "15.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "16.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "17.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "18.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "19.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "20.0".
           02  FILLER         PIC X(10)  VALUE  SPACE.
       01  HEAD5.
           02  FILLER         PIC X(45)  VALUE  SPACE.
           02  FILLER         PIC X(01)  VALUE  "3".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "21.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "21.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "22.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "22.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "23.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "23.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "24.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "24.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "25.0".
           02  FILLER         PIC X(18)  VALUE  SPACE.
       01  HEAD6.
           02  FILLER         PIC X(45)  VALUE  SPACE.
           02  FILLER         PIC X(01)  VALUE  "4".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "24.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "24.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "25.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "25.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "26.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "26.5".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "27.0".
           02  FILLER         PIC X(04)  VALUE  SPACE.
           02  FILLER         PIC X(04)  VALUE  "27.5".
           02  FILLER         PIC X(23)  VALUE  SPACE.
           02  FILLER         PIC N(02)  VALUE  "合計".
       01  W-P1.
           02  P1-01          PIC 9(4).
           02  F              PIC X.
           02  P1-02          PIC N(26).
       01  W-P2.
           02  F              PIC X(01).
           02  P2-01          PIC 9(06).
           02  F              PIC X.
           02  P2-02          PIC N(24).
           02  F              PIC X.
           02  P2-03          PIC 9(01).
           02  P2-04        OCCURS   10.
             03  P2-041       PIC ----,---.
           02  P2-05          PIC --,---,--9.
       01  W-DATA.
           02  W-YMD.
             03  W-YY         PIC 9(02).
             03  W-MM         PIC 9(02).
             03  W-DD         PIC 9(02).
           02  W-PAGE         PIC 9(02).
           02  W-C.
             03  W-C1         PIC 9(01).
             03  W-C2         PIC 9(01).
           02  CNT            PIC 9(02).
           02  W-KEI          PIC S9(07).
           02  W-TCD          PIC 9(04).
           02  W-HCD          PIC 9(06).
           02  W-ASUD.
             03  W-ASU       OCCURS         4.
               04  W-SU       PIC  S9(06)  OCCURS 10.
           02  CHK.
             03  CHK1         PIC 9(01).
             03  CHK2         PIC 9(01).
             03  CHK3         PIC 9(01).
           02  W-ZCD.
             03  W-ZC         PIC 9(01)  OCCURS  4.
       COPY    LWMSG.
      *
           COPY  LTWK07.
           COPY  LITCM.
           COPY  LIHIM2.
      *FD  SP-F
       77  SP-R               PIC X(256).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  DSP-01.
               03  FILLER  PIC  X(26) VALUE
                     "                          ".
               03  FILLER  PIC  X(24) VALUE
                    "得意先品名別　受注合計表".
       COPY    LSMSG.
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "50" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "50" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "23" "26" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "24" "24" "01DSP-01" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-WK07_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK07_PNAME1 " " BY REFERENCE JT-WK07_IDLST "0".
       M-10.
      *           READ JT-WK07 AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK07_PNAME1 BY REFERENCE WK07-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JT-WK07_IDLST JT-WK07_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = WK07-0511(01) AND WK07-0511(02) AND WK07-0511(03)
                  AND WK07-0511(04) AND WK07-0511(05) AND WK07-0511(06)
                  AND WK07-0511(07) AND WK07-0511(08) AND WK07-0511(09)
                  AND WK07-0511(10)
               GO TO M-10
           END-IF
           MOVE WK07-011 TO H-NEN.
           MOVE WK07-012 TO H-GET.
      *
           ACCEPT W-YMD FROM DATE.
           MOVE W-YY TO H-YY.
           MOVE W-MM TO H-MM.
           MOVE W-DD TO H-DD.
           MOVE ZERO TO W-PAGE.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE WK07-02 TO W-TCD.
           MOVE WK07-02 TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ   TC-M    UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "　"  TO    TC-NAME
           END-IF
           MOVE 0 TO CHK1.
       M-20.
           MOVE WK07-03 TO W-HCD.
           MOVE WK07-03 TO HI-MHCD HI-HCD.
      *           READ   HI2-M  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "　"  TO    HI-NAME
           END-IF
      *
           MOVE ZERO TO W-ASUD W-KEI W-ZCD.
       M-25.
           MOVE 1 TO W-ZC(WK07-04).
           MOVE ZERO TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE WK07-0511(CNT) TO W-SU(WK07-04,CNT)
               ADD WK07-0511(CNT) TO W-KEI
               GO TO M-30
           END-IF.
       M-35.
      *           READ JT-WK07 AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK07_PNAME1 BY REFERENCE WK07-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           IF (WK07-02 = W-TCD) AND (WK07-03 = W-HCD)
               GO TO M-25
           END-IF
           PERFORM S-30 THRU S-45.
           IF  WK07-02 NOT = W-TCD
               GO TO M-15
           END-IF
           GO TO M-20.
       M-60.
           PERFORM S-30 THRU S-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK07_IDLST JT-WK07_PNAME1.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE W-TCD TO P1-01.
           MOVE TC-NAME TO P1-02.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 57
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-25.
           EXIT.
       S-30.
           IF  W-ZCD = ZERO
               GO TO S-45
           END-IF
           IF  W-ZC(2) = 1
               MOVE 2 TO CHK3
           END-IF
           IF  W-ZC(3) = 1
               MOVE 3 TO CHK3
           END-IF
           IF  W-ZC(4) = 1
               MOVE 4 TO CHK3
           END-IF
           IF  W-ZC(1) = 1
               MOVE 1 TO CHK3
           END-IF
           IF  CHK1 = 0
               MOVE 1 TO CHK1
               PERFORM S-20 THRU S-25
           END-IF
           MOVE 0 TO W-C1 CHK2.
       S-35.
           ADD 1 TO W-C1.
           IF  W-C1 = 5
               GO TO S-45
           END-IF
           IF  W-C1 = 1
               MOVE 4 TO W-C2
           END-IF
           IF  W-C1 = 2
               MOVE 1 TO W-C2
           END-IF
           IF  W-C1 = 3
               MOVE 2 TO W-C2
           END-IF
           IF  W-C1 = 4
               MOVE 3 TO W-C2
           END-IF
           IF  W-ZC(W-C2) = 0
               GO TO S-35
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P2-02.
           IF  CHK2 = 0
               MOVE 1 TO CHK2
               MOVE W-HCD TO P2-01
               MOVE HI-NAME TO P2-02
           END-IF
           MOVE W-C2 TO P2-03.
           MOVE ZERO TO CNT.
       S-40.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE W-SU(W-C2,CNT) TO P2-041(CNT)
               GO TO S-40
           END-IF
           IF  CHK3 = W-C2
               MOVE W-KEI TO P2-05
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               MOVE W-HCD TO P2-01
               MOVE HI-NAME TO P2-02
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  CHK3 NOT = W-C2
               GO TO S-35
           END-IF.
       S-45.
           EXIT.
