       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHY250.
      *********************************************************
      *    PROGRAM         :  工品仕掛品　棚卸集計表　(場所)  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *        変更　　　  :  62/04/10                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　工品仕掛品　棚卸集計表　　＊＊＊".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(005) VALUE "  :  ".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(002) VALUE "場所".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P.
           02  W-PD    OCCURS  58.
             03  P-HCD1       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  X(020).
             03  F            PIC  X(001).
             03  P-KC1        PIC  9(001).
             03  P-SU1        PIC ---,---,--9.99.
             03  P-T1         PIC ----,--9.99.
             03  P-KIN1       PIC ----,---,--9.
             03  F            PIC  X(002).
             03  P-X          PIC  X(001).
             03  F            PIC  X(002).
             03  P-HCD2       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  X(020).
             03  F            PIC  X(001).
             03  P-KC2        PIC  9(001).
             03  P-SU2        PIC ---,---,--9.99.
             03  P-T2         PIC ----,--9.99.
             03  P-KIN2       PIC ----,---,--9.
       01  W-DATA.
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(002).
           02  W-HCD          PIC  X(005).
           02  CNT            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-CD           PIC  9(001).
           02  W-POC          PIC  9(001).
           02  W-T            PIC  9(006)V9(02).
       01  WT-D.
           02  WT-SU          PIC S9(006)V9(02).
           02  WT-KIN         PIC S9(009).
       01  WS-D.
           02  WS-SU          PIC S9(008)V9(02).
           02  WS-KIN         PIC S9(009).
       01  WA-D.
           02  WA-SU          PIC S9(008)V9(02).
           02  WA-KIN         PIC S9(009).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LSPF.
      *FD  KHTN-F
       01  KHTN-F_KHY250.
           02  KHTN-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KHTN-F_LNAME   PIC  X(013) VALUE "KHTN-F_KHY250".
           02  F              PIC  X(001).
           02  KHTN-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KHTN-F_SORT    PIC  X(100) VALUE SPACE.
           02  KHTN-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KHTN-F_RES     USAGE  POINTER.
       01  KHTN-R.
           02  KHTN-KC        PIC  9(001).
           02  KHTN-HCD       PIC  X(005).
           02  KHTN-SU        PIC S9(006)V9(02).
           02  KHTN-YC        PIC  9(002).
           02  KHTN-NC        PIC  9(001).
           02  F              PIC  X(003).
           02  KHTN-PC        PIC  9(001).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　工品仕掛品　棚卸集計表（場所）　　＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
           COPY LSSEM.
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "72" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "50" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "20" "29" "22" "01C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "46" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KHTN-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KHTN-F_PNAME1 " " BY REFERENCE KHTN-F_IDLST "0".
      *
      *           READ KHTN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTN-F_PNAME1 BY REFERENCE KHTN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KHTN-F_IDLST KHTN-F_PNAME1
               GO TO M-95
           END-IF
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           INITIALIZE W-DATA.
           MOVE ZERO TO WA-D.
           MOVE SPACE TO W-P.
       M-25.
           MOVE KHTN-YC TO W-YC.
           MOVE KHTN-NC TO W-NC.
           MOVE ZERO TO WS-D.
       M-30.
           MOVE KHTN-HCD TO W-HCD.
           MOVE ZERO TO WT-D CHK CNT.
       M-35.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
      *
      *           READ KHTN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KHTN-F_PNAME1 BY REFERENCE KHTN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF
           IF (KHTN-YC NOT = W-YC) OR (KHTN-NC NOT = W-NC)
               GO TO M-50
           END-IF
           IF  KHTN-HCD = W-HCD
               GO TO M-35
           END-IF
           IF  CNT = 9
               GO TO M-40
           END-IF
           PERFORM S-45 THRU S-50.
           GO TO M-45.
       M-40.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
       M-45.
           GO TO M-30.
       M-50.
           IF  CNT = 9
               GO TO M-55
           END-IF
           PERFORM S-45 THRU S-50.
           GO TO M-60.
       M-55.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
       M-60.
      *
           PERFORM S-20 THRU S-25.
           PERFORM S-55 THRU S-60.
           PERFORM S-20 THRU S-25.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           GO TO M-25.
       M-75.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHTN-F_IDLST KHTN-F_PNAME1.
           IF  CNT = 9
               GO TO M-80
           END-IF
           PERFORM S-45 THRU S-50.
           GO TO M-85.
       M-80.
           PERFORM S-20 THRU S-25.
           PERFORM S-40 THRU S-50.
       M-85.
       M-90.
           PERFORM S-20 THRU S-25.
           PERFORM S-55 THRU S-60.
           PERFORM S-20 THRU S-25.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           PERFORM S-20 THRU S-25.
           PERFORM S-65 THRU S-70.
           PERFORM S-75 THRU S-85.
           CALL "PR_Close" RETURNING RESP.
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
       S-20.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO S-25
           END-IF
           ADD 1 TO W-CD.
           IF  W-CD NOT = 2
               MOVE ZERO TO W-LD CHK
               GO TO S-20
           END-IF
           PERFORM S-75 THRU S-85.
           MOVE ZERO TO W-LD W-CD CHK.
           MOVE SPACE TO W-P.
           GO TO S-20.
       S-25.
           EXIT.
       S-30.
           MOVE KHTN-HCD TO KH-HCD.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ﾏｽﾀｰ ﾐﾄｳﾛｸ          " TO KH-NAME
               MOVE ZERO TO KH-GT1
           END-IF
           IF  W-CD = 0
               MOVE KHTN-KC TO P-KC1(W-LD)
               MOVE KHTN-SU TO P-SU1(W-LD)
               MOVE ":" TO P-X(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-HCD TO P-HCD1(W-LD)
                   MOVE KH-NAME TO P-NAME1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE KHTN-KC TO P-KC2(W-LD)
               MOVE KHTN-SU TO P-SU2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-HCD TO P-HCD2(W-LD)
                   MOVE KH-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF
           ADD KHTN-SU TO WT-SU.
           IF  CNT = 5
               MOVE 9 TO CNT
           END-IF
           IF  CNT = 0
               MOVE 5 TO CNT
           END-IF.
       S-35.
           EXIT.
       S-40.
           IF  W-CD = 0
               MOVE "        （　計　）  " TO P-NAME1(W-LD)
           ELSE
               MOVE "        （　計　）  " TO P-NAME2(W-LD)
           END-IF.
       S-45.
           MOVE KH-GT1 TO W-T.
           COMPUTE WT-KIN = WT-SU * W-T.
           IF  W-CD = 0
               MOVE WT-SU TO P-SU1(W-LD)
               MOVE W-T TO P-T1(W-LD)
               MOVE WT-KIN TO P-KIN1(W-LD)
               MOVE ":" TO P-X(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-HCD TO P-HCD1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE WT-SU TO P-SU2(W-LD)
               MOVE W-T TO P-T2(W-LD)
               MOVE WT-KIN TO P-KIN2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-HCD TO P-HCD2(W-LD)
               END-IF
           END-IF
           ADD WT-SU TO WS-SU.
           ADD WT-KIN TO WS-KIN.
       S-50.
           EXIT.
       S-55.
           IF  W-CD = 0
               MOVE "    ［　小計　］    " TO P-NAME1(W-LD)
               MOVE WS-SU TO P-SU1(W-LD)
               MOVE WS-KIN TO P-KIN1(W-LD)
               MOVE ":" TO P-X(W-LD)
           END-IF
           IF  W-CD = 1
               MOVE "    ［　小計　］    " TO P-NAME2(W-LD)
               MOVE WS-SU TO P-SU2(W-LD)
               MOVE WS-KIN TO P-KIN2(W-LD)
           END-IF
           ADD WS-SU TO WA-SU.
           ADD WS-KIN TO WA-KIN.
       S-60.
           EXIT.
       S-65.
           IF  W-CD = 0
               MOVE "【　総合計　】　    " TO P-NAME1(W-LD)
               MOVE WA-SU TO P-SU1(W-LD)
               MOVE WA-KIN TO P-KIN1(W-LD)
               MOVE ":" TO P-X(W-LD)
           END-IF
           IF  W-CD = 1
               MOVE "【　総合計　】　    " TO P-NAME2(W-LD)
               MOVE WA-SU TO P-SU2(W-LD)
               MOVE WA-KIN TO P-KIN2(W-LD)
           END-IF.
       S-70.
           EXIT.
       S-75.
           IF  W-POC = 0
               MOVE 9 TO W-POC
               CALL "PR_Open" RETURNING RESP
               MOVE DATE-03R TO H-DATE
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-80.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               IF  P-X(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO S-80
               END-IF
           END-IF.
       S-85.
           EXIT.
