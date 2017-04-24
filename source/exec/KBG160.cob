       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBG160.
      *********************************************************
      *    PROGRAM         :  購買品目区分別集計表２　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/06/09                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
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
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  N(024) VALUE
                "＊＊＊　　材料品目区分別　仕入集計表２　　＊＊＊".
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
       01  HEAD2.
           02  F              PIC  X(045) VALUE
                "         :            素　　　材　          :".
           02  F              PIC  X(033) VALUE
                "          工　　　　品　         ".
           02  F              PIC  X(029) VALUE
                " :             :             ".
       01  HEAD3.
           02  F              PIC  X(045) VALUE
                " 日　付  :    コ゛ ム   　薬　品   　合　計 :".
           02  F              PIC  X(033) VALUE
                "      防　振　　その他   　合　計".
           02  F              PIC  X(029) VALUE
                " :    製品仕入 :   総　合　計".
       01  W-P.
           02  P-DATE         PIC 99/99/99B.
           02  P-DATED REDEFINES P-DATE  PIC  X(009).
           02  P-X1           PIC  X(001).
           02  P-SG           PIC ---,---,--9.
           02  P-SY           PIC ---,---,--9.
           02  P-ST           PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-X2           PIC  X(001).
           02  P-KB           PIC ----,---,--9.
           02  P-KS           PIC --,---,--9.
           02  P-KT           PIC ---,---,--9.
           02  F              PIC  X(001).
           02  P-X3           PIC  X(001).
           02  P-SS           PIC ----,---,--9.
           02  F              PIC  X(001).
           02  P-X4           PIC  X(001).
           02  P-TOTAL        PIC -----,---,--9.
       01  W-D.
           02  W-DATE.
             03  W-NG         PIC  9(004).
             03  W-H          PIC  9(002).
           02  W-DATED REDEFINES W-DATE  PIC  9(006).
           02  WN-D.
             03  W-SG         PIC S9(009).
             03  W-SY         PIC S9(009).
             03  W-ST         PIC S9(009).
             03  W-KB         PIC S9(009).
             03  W-KS         PIC S9(008).
             03  W-KT         PIC S9(009).
             03  W-SS         PIC S9(009).
             03  W-TOTAL      PIC S9(009).
       01  WS-D.
           02  WS-SG          PIC S9(009).
           02  WS-SY          PIC S9(009).
           02  WS-ST          PIC S9(009).
           02  WS-KB          PIC S9(009).
           02  WS-KS          PIC S9(008).
           02  WS-KT          PIC S9(009).
           02  WS-SS          PIC S9(009).
           02  WS-TOTAL       PIC S9(009).
       01  WA-D.
           02  WA-SG          PIC S9(009).
           02  WA-SY          PIC S9(009).
           02  WA-ST          PIC S9(009).
           02  WA-KB          PIC S9(009).
           02  WA-KS          PIC S9(008).
           02  WA-KT          PIC S9(009).
           02  WA-SS          PIC S9(009).
           02  WA-TOTAL       PIC S9(009).
       01  W-CNT              PIC  9(001).
       01  W-CNT1             PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LSJSSW.
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　品目区分別　仕入集計表２　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
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
               GO TO M-95
           END-IF
           IF  JR-DC1 = 3
               GO TO M-10
           END-IF
           IF  JR-JCD1 = 0 OR 1 OR 5 OR 6
               GO TO M-10
           END-IF
      *
           MOVE DATE-05R TO H-DATE.
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
           MOVE ZERO TO WA-D.
       M-15.
           MOVE ZERO TO WS-D.
           IF  JR-PEY < 11
               MOVE 1 TO W-CNT
               GO TO M-20
           END-IF
           IF  JR-PEY < 21
               MOVE 2 TO W-CNT
               GO TO M-20
           END-IF
           MOVE 3 TO W-CNT.
       M-20.
           MOVE ZERO TO W-D.
           MOVE JR-NGPS TO W-DATE.
       M-25.
           PERFORM S-05 THRU S-30.
       M-30.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JR-DC1 = 3
               GO TO M-30
           END-IF
           IF  JR-JCD1 = 0 OR 1 OR 5 OR 6
               GO TO M-30
           END-IF
           IF  W-DATE = JR-NGPS
               GO TO M-25
           END-IF.
       M-35.
           PERFORM S-35 THRU S-40.
           IF  JR-PEY < 11
               MOVE 1 TO W-CNT1
               GO TO M-40
           END-IF
           IF  JR-PEY < 21
               MOVE 2 TO W-CNT1
               GO TO M-40
           END-IF
           MOVE 3 TO W-CNT1.
       M-40.
           IF  W-CNT = W-CNT1
               GO TO M-20
           END-IF
           PERFORM S-45 THRU S-50.
           MOVE ZERO TO WS-D.
           MOVE W-CNT1 TO W-CNT.
           GO TO M-20.
       M-90.
           PERFORM S-35 THRU S-40.
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R W-P.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE "  合 計 " TO P-DATED.
           MOVE ZERO TO WN-D.
           MOVE WA-D TO WN-D.
           PERFORM S-55 THRU S-60.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           ADD JR-KIN TO W-TOTAL.
           IF  JR-JCD2 > 89 AND < 95
               GO TO S-25
           END-IF
           IF  JR-JCD1 = 2
               GO TO S-10
           END-IF
           IF  JR-JCD1 = 3 OR 4
               GO TO S-15
           END-IF
           IF  JR-JCD1 = 6
               GO TO S-20
           END-IF
           IF  JR-JCD1 = 7
               GO TO S-25
           END-IF.
       S-10.
           ADD JR-KIN TO W-ST.
           IF  JR-JCD2 < 68
               ADD JR-KIN TO W-SG
               GO TO S-30
           END-IF
           IF  JR-JCD2 < 82
               ADD JR-KIN TO W-SY
               GO TO S-30
           END-IF.
       S-15.
           ADD JR-KIN TO W-KT.
           IF  JR-JCD1 = 3
               ADD JR-KIN TO W-KB
               GO TO S-30
           END-IF
           IF  JR-JCD1 = 4
               ADD JR-KIN TO W-KS
               GO TO S-30
           END-IF.
       S-20.
           IF  JR-JCD1 = 6
               GO TO S-30
           END-IF.
       S-25.
           ADD JR-KIN TO W-SS.
       S-30.
           EXIT.
       S-35.
           MOVE SPACE TO SP-R W-P.
           MOVE W-DATED TO P-DATE.
           PERFORM S-55 THRU S-60.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-SG TO WS-SG.
           ADD W-SY TO WS-SY.
           ADD W-ST TO WS-ST.
           ADD W-KB TO WS-KB.
           ADD W-KS TO WS-KS.
           ADD W-KT TO WS-KT.
           ADD W-SS TO WS-SS.
           ADD W-TOTAL TO WS-TOTAL.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO SP-R W-P.
           MOVE "  小 計 " TO P-DATED.
           MOVE ZERO TO WN-D.
           MOVE WS-D TO WN-D.
           PERFORM S-55 THRU S-60.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R W-P.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD WS-SG TO WA-SG.
           ADD WS-SY TO WA-SY.
           ADD WS-ST TO WA-ST.
           ADD WS-KB TO WA-KB.
           ADD WS-KS TO WA-KS.
           ADD WS-KT TO WA-KT.
           ADD WS-SS TO WA-SS.
           ADD WS-TOTAL TO WA-TOTAL.
       S-50.
           EXIT.
       S-55.
           MOVE W-SG TO P-SG.
           MOVE W-SY TO P-SY.
           MOVE W-ST TO P-ST.
           MOVE W-KB TO P-KB.
           MOVE W-KS TO P-KS.
           MOVE W-KT TO P-KT.
           MOVE W-SS TO P-SS.
           MOVE W-TOTAL TO P-TOTAL.
           MOVE ":" TO P-X1 P-X2 P-X3 P-X4.
           MOVE W-P TO SP-R.
       S-60.
           EXIT.
