       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT350.
      *********************************************************
      *    PROGRAM         :  履物部門担当得意先別売上問合せ  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT35                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-BC3          PIC  9(002).
           02  W-TNC          PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-DATE.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KIN.
             03  W-U          PIC S9(008).
             03  W-UG         PIC S9(008).
             03  W-UA         PIC S9(008).
           02  WN-D.
             03  WN-U         PIC S9(009).
             03  WN-UA        PIC S9(009).
           02  WA-D.
             03  WA-U         PIC S9(009).
             03  WA-UA        PIC S9(009).
           02  W-BRN3         PIC  N(003).
           02  W-NEM          PIC  X(030).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHKBM.
           COPY LSSNTW.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　部門担当得意先別　売上問合せ　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(005) VALUE "カジュアル".
             03  FILLER  PIC  X(006) VALUE "=10 , ".
             03  FILLER  PIC  N(003) VALUE "ワーク".
             03  FILLER  PIC  X(006) VALUE "=20 , ".
             03  FILLER  PIC  N(003) VALUE "教　育".
             03  FILLER  PIC  X(012) VALUE "=30 .....   ".
       01  C-ACP.
           02  A-BC3   PIC  9(002).
           02  A-TNC   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MM.
             03  FILLER  PIC  N(003).
             03  FILLER  PIC Z9.
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  9(001) VALUE 1.
             03  FILLER  PIC  N(001) VALUE "～".
             03  FILLER  PIC Z9.
           02  D-DATA.
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  N(024).
             03  FILLER  PIC ----,---,--9 .
             03  FILLER  PIC ----,---,--9 .
           02  D-TD.
             03  FILLER  PIC  N(019) VALUE
                  "　　　　　　　　　　［　　合　計　　］".
             03  FILLER  PIC ----,---,--9 .
             03  FILLER  PIC ----,---,--9 .
           02  D-NEM   PIC  X(030).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "286" " " " " RETURNING RESU.
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
           "06C-MID" " " "15" "0" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "15" "11" "10" " " "06C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "15" "21" "6" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "09C-MID" "N" "15" "27" "6" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "10C-MID" "X" "15" "33" "6" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "11C-MID" "N" "15" "39" "6" "10C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "12C-MID" "X" "15" "45" "12" "11C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-BC3" "9" "15" "56" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-BC3" BY REFERENCE W-BC3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-TNC" "9" "3" "6" "2" "A-BC3" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-TNC" BY REFERENCE W-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "62" "1" "A-TNC" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "182" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MM" " " "3" "0" "14" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MM" "RN" "3" "61" "6" " " "D-MM" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MM" BY REFERENCE W-BRN3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MM" "Z9" "3" "71" "2" "01D-MM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MM" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MM" "X" "3" "73" "1" "02D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MM" "9" "3" "75" "1" "03D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MM" "N" "3" "76" "2" "04D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-MM" "Z9" "3" "78" "2" "05D-MM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "06D-MM" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATA" " " "W-L" "0" "76" "D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATA" "9" "W-L" "1" "4" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATA" BY REFERENCE T-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATA" "N" "W-L" "6" "48" "01D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATA" BY REFERENCE T-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DATA" "----,---,--9" "W-L" "55" "12" "02D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DATA" BY REFERENCE WN-U "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-DATA" "----,---,--9" "W-L" "68" "12" "03D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-DATA" BY REFERENCE WN-UA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "W-L" "0" "62" "D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "N" "W-L" "6" "38" " " "D-TD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "----,---,--9" "W-L" "55" "12" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE WA-U "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TD" "----,---,--9" "W-L" "68" "12" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TD" BY REFERENCE WA-UA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NEM" "X" "23" "33" "30" "D-TD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NEM" BY REFERENCE W-NEM "30" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-10.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
      *
           COPY LIBCPR.
           MOVE D-HSD TO W-DATE.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-BC3 "A-BC3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE 14 TO HKB-NO.
           MOVE W-BC3 TO HKB-BR3.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE HKB-BRN3 TO W-BRN3.
       M-20.
           CALL "SD_Screen_Output" USING "SCHT35" RETURNING RESU.
           CALL "SD_Output" USING "D-MM" D-MM "p" RETURNING RESU.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-TNC "A-TNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-TNC NOT = 99
               MOVE SPACE TO HKB-KEY
               MOVE 04 TO HKB-NO
               MOVE W-TNC TO HKB-TNC
      *               READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   GO TO M-25
               END-IF
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
           MOVE ZERO TO WA-D.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-30.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-30
           END-IF
           IF  SNTR-BC3 NOT = W-BC3
               GO TO M-30
           END-IF
           IF  W-TNC NOT = 99
               IF  SNTR-TNC NOT = W-TNC
                   GO TO M-30
               END-IF
           END-IF
      *
           PERFORM COM-RTN THRU COM-EX.
           IF  ZERO = W-U AND W-UA
               GO TO M-30
           END-IF.
       M-35.
           IF  W-TNC = 99
               GO TO M-40
           END-IF
           MOVE ZERO TO WN-D.
           MOVE SNTR-TCD TO W-TCD.
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊＊　得意先なし　＊＊" TO T-NAME
           END-IF.
       M-40.
           ADD W-U TO WN-U WA-U.
           ADD W-UA TO WN-UA WA-UA.
       M-45.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-45
           END-IF
           IF  SNTR-BC3 NOT = W-BC3
               GO TO M-45
           END-IF
           PERFORM COM-RTN THRU COM-EX.
           IF  ZERO = W-U AND W-UA
               GO TO M-45
           END-IF
           IF  W-TNC = 99
               GO TO M-40
           END-IF
           IF  SNTR-TNC NOT = W-TNC
               GO TO M-50
           END-IF
           IF  SNTR-TCD = W-TCD
               GO TO M-40
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
           IF  W-L = 23
               GO TO M-20
           END-IF
           GO TO M-35.
       M-50.
           IF  W-TNC = 99
               GO TO M-55
           END-IF
           PERFORM MEI-RTN THRU MEI-EX.
           IF  W-L = 23
               GO TO M-20
           END-IF.
       M-55.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       COM-RTN.
           MOVE ZERO TO W-KIN.
           IF (SNTR-SNC = 0) AND (SNTR-DC NOT = 2)
               COMPUTE W-UG = SNTR-SU * SNTR-FT
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-UG = W-UG * -1
               COMPUTE W-U = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-U
           END-IF
           COMPUTE W-UA = W-U - W-UG.
       COM-EX.
           EXIT.
       MEI-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 23
               CALL "SD_Output" USING
                "D-DATA" D-DATA "p" RETURNING RESU
               GO TO MEI-EX
           END-IF
           MOVE SPACE TO W-NEM.
           MOVE "ＮＥＸＴ=ﾘﾀｰﾝ , 担当入力=BS   " TO W-NEM.
           CALL "SD_Output" USING "D-NEM" D-NEM "p" RETURNING RESU.
       MEI-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCHT35" RETURNING RESU
               CALL "SD_Output" USING "D-MM" D-MM "p" RETURNING RESU
               CALL "SD_Output" USING "A-TNC" A-TNC "p" RETURNING RESU
               MOVE 4 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO MEI-RTN
           END-IF.
       MEI-EX.
           EXIT.
       KEI-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               MOVE SPACE TO W-NEM
               MOVE "ＮＥＸＴ=ﾘﾀｰﾝ , 担当入力=BS   " TO W-NEM
           ELSE
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
               MOVE SPACE TO W-NEM
               MOVE "　　　　　 ＥＮＤ　ＤＡＴＡ   " TO W-NEM
           END-IF
           CALL "SD_Output" USING "D-NEM" D-NEM "p" RETURNING RESU.
       KEI-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-L = 23
               IF  ESTAT NOT = BTB
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Screen_Output" USING
                    "SCHT35" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MM" D-MM "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-TNC" A-TNC "p" RETURNING RESU
                   MOVE 4 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   GO TO KEI-RTN
               END-IF
           END-IF.
       KEI-EX.
           EXIT.
