       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHT710.
      *********************************************************
      *    PROGRAM         :  マット他　受払問合せ（金額）　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKT71                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
       01  WS-D.
           02  WS-ZK          PIC S9(008).
           02  WS-SK          PIC S9(008).
           02  WS-UG          PIC S9(008).
           02  WS-UK          PIC S9(008).
           02  WS-YK          PIC S9(008).
           02  WS-AR          PIC S9(008).
       01  WA-D.
           02  WA-ZK          PIC S9(008).
           02  WA-SK          PIC S9(008).
           02  WA-UG          PIC S9(008).
           02  WA-UK          PIC S9(008).
           02  WA-YK          PIC S9(008).
           02  WA-AR          PIC S9(008).
       01  W-DATA.
           02  W-D.
             03  W-ZK         PIC S9(008).
             03  W-SK         PIC S9(008).
             03  W-UG         PIC S9(008).
             03  W-UK         PIC S9(008).
             03  W-YK         PIC S9(008).
             03  W-AR         PIC S9(008).
           02  W-YC           PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIKHM.
           COPY LIKHT2.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MEI.
             03  01D-MEI  PIC  X(005).
             03  02D-MEI  PIC  X(020).
             03  03D-MEI  PIC ---,---,--9 .
             03  04D-MEI  PIC ---,---,--9 .
             03  05D-MEI  PIC ---,---,--9 .
             03  06D-MEI  PIC ---,---,--9 .
             03  07D-MEI  PIC ------,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "70" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "79" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "D-MEI" " " "W-L" "0" "79" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING
            "01D-MEI" "X" "W-L" "1" "5" " " "D-MEI"  RETURNING RESU.
       CALL "SD_From" USING
            "01D-MEI" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02D-MEI" "X" "W-L" "7" "20" "01D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02D-MEI" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03D-MEI" "---,---,--9" "W-L" "27" "11" "02D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03D-MEI" BY REFERENCE W-ZK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04D-MEI" "---,---,--9" "W-L" "38" "11" "03D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04D-MEI" BY REFERENCE W-SK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "05D-MEI" "---,---,--9" "W-L" "49" "11" "04D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "05D-MEI" BY REFERENCE W-UK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "06D-MEI" "---,---,--9" "W-L" "60" "11" "05D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06D-MEI" BY REFERENCE W-YK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "07D-MEI" "------,--9" "W-L" "71" "10" "06D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "07D-MEI" BY REFERENCE W-AR "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "31" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "16" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING
            "E-KEY" "X" "24" "35" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT71" RETURNING RESU.
           MOVE ZERO TO W-DATA.
      *
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "1"
            "KHT-KEYD" BY REFERENCE KHT-KEYD.
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-10.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-10
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-KSU AND KHT-HSU AND KHT-SSU
                             AND KHT-ISU  AND KHT-UKIN AND KHT-NKIN
               GO TO M-10
           END-IF
           IF  KHT-YC = 10 OR 11 OR 25
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO WA-D.
       M-15.
           MOVE KHT-YC TO W-YC.
           MOVE ZERO TO WS-D CHK.
       M-20.
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO KH-GT1
               MOVE SPACE TO KH-NAME
           END-IF
           COMPUTE W-ZK = KHT-ZSU * KH-GT1.
           COMPUTE W-SK = (KHT-KSU - KHT-HSU + KHT-ISU) * KH-GT1.
           COMPUTE W-UK = KHT-UKIN - KHT-NKIN.
           COMPUTE W-UG = KHT-SSU * KH-GT1.
           COMPUTE W-YK =
            (KHT-ZSU + KHT-KSU - KHT-HSU + KHT-ISU - KHT-SSU) * KH-GT1.
           IF  KHT-YC = 99
               MOVE ZERO TO W-AR
           ELSE
               COMPUTE W-AR = W-UK - W-UG
           END-IF
           PERFORM S-05 THRU S-10.
           IF  W-END = 1
               GO TO M-95
           END-IF
      *
           ADD W-ZK TO WS-ZK.
           ADD W-SK TO WS-SK.
           ADD W-UG TO WS-UG.
           ADD W-UK TO WS-UK.
           ADD W-YK TO WS-YK.
           ADD W-AR TO WS-AR.
           IF CHK = 0
               MOVE 1 TO CHK
           END-IF.
       M-30.
      *           READ KHT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KHT-M_PNAME1 BY REFERENCE KHT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-30
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-KSU AND KHT-HSU AND KHT-SSU
                             AND KHT-ISU  AND KHT-UKIN AND KHT-NKIN
               GO TO M-30
           END-IF
           IF  KHT-YC = 10 OR 11 OR 25
               GO TO M-30
           END-IF
           IF  KHT-YC = W-YC
               GO TO M-20
           END-IF
           IF  CHK NOT = 0
               PERFORM S-20 THRU S-35
           END-IF
           IF  W-END = 1
               GO TO M-95
           END-IF
           GO TO M-15.
       M-90.
           IF  CHK NOT = 0
               PERFORM S-20 THRU S-35
           END-IF
           IF  W-END = 1
               GO TO M-95
           END-IF
           PERFORM S-40 THRU S-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 23
               CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU
               GO TO S-10
           END-IF
      *
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO S-10
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCKT71" RETURNING RESU.
           MOVE 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO S-05.
       S-10.
           EXIT.
       S-20.
           MOVE ZERO TO W-D.
           MOVE WS-D TO W-D.
           MOVE SPACE TO KH-KEY.
           MOVE "   <  SUB TOTAL  >  " TO KH-NAME.
           PERFORM S-05 THRU S-10.
           IF  W-END = 1
               GO TO S-35
           END-IF
      *
           ADD WS-ZK TO WA-ZK.
           ADD WS-SK TO WA-SK.
           ADD WS-UG TO WA-UG.
           ADD WS-UK TO WA-UK.
           ADD WS-YK TO WA-YK.
           ADD WS-AR TO WA-AR.
       S-35.
           EXIT.
       S-40.
           MOVE ZERO TO W-D.
           MOVE WA-D TO W-D.
           MOVE SPACE TO KH-KEY.
           MOVE "  [  ALL TOTAL  ]   " TO KH-NAME.
           PERFORM S-05 THRU S-10.
       S-50.
           EXIT.
