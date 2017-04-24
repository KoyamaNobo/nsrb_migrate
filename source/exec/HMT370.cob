       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT370.
      *********************************************************
      *    PROGRAM         :  履物担当得意先部門別売上問合せ  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT37                          *
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
           02  W-TNCD         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-L.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
           02  W-DATE.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KIN          PIC S9(008).
           02  WN-D.
             03  WN-IP        PIC S9(009).
             03  WN-WK        PIC S9(009).
             03  WN-KI        PIC S9(009).
             03  WN-GU        PIC S9(009).
           02  WT-D.
             03  WT-IP        PIC S9(009).
             03  WT-WK        PIC S9(009).
             03  WT-KI        PIC S9(009).
             03  WT-GU        PIC S9(009).
           02  WS-D.
             03  WS-IP        PIC S9(009).
             03  WS-WK        PIC S9(009).
             03  WS-KI        PIC S9(009).
             03  WS-GU        PIC S9(009).
           02  WA-D.
             03  WA-IP        PIC S9(009).
             03  WA-WK        PIC S9(009).
             03  WA-KI        PIC S9(009).
             03  WA-GU        PIC S9(009).
           02  WK-D.
             03  WK-IP        PIC S9(009).
             03  WK-WK        PIC S9(009).
             03  WK-KI        PIC S9(009).
             03  WK-GU        PIC S9(009).
           02  W-TM           PIC  N(012).
           02  W-NEM          PIC  X(037).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
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
       01  C-ACP.
           02  A-TNCD  PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MM.
             03  01D-MM  PIC Z9 .
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  9(001) VALUE 1.
             03  FILLER  PIC  N(001) VALUE "～".
             03  05D-MM  PIC Z9 .
           02  D-DATA.
             03  FILLER.
               04  02D-DATA  PIC  9(002).
               04  03D-DATA  PIC  9(004).
               04  04D-DATA  PIC  N(026).
             03  FILLER.
               04  06D-DATA  PIC ----,---,--9 .
               04  07D-DATA  PIC ----,---,--9 .
               04  08D-DATA  PIC ----,---,--9 .
               04  09D-DATA  PIC ----,---,--9 .
           02  D-TD.
             03  01D-TD  PIC  N(012).
             03  02D-TD  PIC ----,---,--9 .
             03  03D-TD  PIC ----,---,--9 .
             03  04D-TD  PIC ----,---,--9 .
             03  05D-TD  PIC ----,---,--9 .
           02  D-NEM   PIC  X(037).
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
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "3" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TNCD" "9" "4" "2" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TNCD" BY REFERENCE W-TNCD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "67" "1" "A-TNCD" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "223" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MM" " " "2" "0" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MM" "Z9" "2" "60" "2" " " "D-MM" RETURNING RESU.
       CALL "SD_From" USING
           "01D-MM" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MM" "X" "2" "62" "1" "01D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03D-MM" "9" "2" "64" "1" "02D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04D-MM" "N" "2" "65" "2" "03D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05D-MM" "Z9" "2" "67" "2" "04D-MM" " " RETURNING RESU.
       CALL "SD_From" USING
           "05D-MM" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-DATA" " " "0" "0" "106" "D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DATA" " " "W-L1" "0" "58" " " "D-DATA" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-DATA" "9" "W-L1" "2" "2" " " "01D-DATA" RETURNING RESU.
       CALL "SD_From" USING
           "02D-DATA" BY REFERENCE W-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-DATA" "9" "W-L1" "6" "4" "02D-DATA" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-DATA" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-DATA" "N" "W-L1" "11" "52" "03D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-DATA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05D-DATA" " " "W-L2" "0" "48" "01D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06D-DATA" "----,---,--9" "W-L2" "30" "12" " " "05D-DATA"
            RETURNING RESU.
       CALL "SD_From" USING
           "06D-DATA" BY REFERENCE WN-GU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "07D-DATA" "----,---,--9" "W-L2" "43" "12" "06D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "07D-DATA" BY REFERENCE WN-IP "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "08D-DATA" "----,---,--9" "W-L2" "56" "12" "07D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "08D-DATA" BY REFERENCE WN-WK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "09D-DATA" "----,---,--9" "W-L2" "69" "12" "08D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "09D-DATA" BY REFERENCE WN-KI "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TD" " " "W-L2" "0" "52" "D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TD" "N" "W-L2" "6" "24" " " "D-TD" RETURNING RESU.
       CALL "SD_From" USING
           "01D-TD" BY REFERENCE W-TM "24" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TD" "----,---,--9" "W-L2" "30" "12" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TD" BY REFERENCE WK-GU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TD" "----,---,--9" "W-L2" "43" "12" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-TD" BY REFERENCE WK-IP "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-TD" "----,---,--9" "W-L2" "56" "12" "03D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-TD" BY REFERENCE WK-WK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05D-TD" "----,---,--9" "W-L2" "69" "12" "04D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "05D-TD" BY REFERENCE WK-KI "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NEM" "X" "23" "30" "37" "D-TD" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NEM" BY REFERENCE W-NEM "37" "0" RETURNING RESU.
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
           CALL "SD_Screen_Output" USING "SCHT37" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-HSD TO W-DATE.
           CALL "SD_Output" USING "D-MM" D-MM "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TNCD "A-TNCD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT37" RETURNING RESU.
           CALL "SD_Output" USING "D-MM" D-MM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TNCD" A-TNCD "p" RETURNING RESU.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 3 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-15.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-15
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-KIN
           END-IF
           IF  W-KIN = ZERO
               GO TO M-15
           END-IF
           MOVE ZERO TO WA-D.
       M-20.
           MOVE ZERO TO WS-D.
           MOVE SNTR-TNC TO W-TNC.
       M-25.
           MOVE ZERO TO WT-D.
           MOVE SNTR-TNC2 TO W-TNC2.
       M-30.
           MOVE ZERO TO WN-D.
           MOVE SNTR-TCD TO W-TCD.
           IF  SNTR-TNC < W-TNCD
               GO TO M-35
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "　＊得意先なし＊" TO T-NAME
           END-IF.
       M-35.
           IF  SNTR-BC3 = 10
               ADD W-KIN TO WN-IP WT-IP WS-IP WA-IP
           END-IF
           IF  SNTR-BC3 = 20
               ADD W-KIN TO WN-WK WT-WK WS-WK WA-WK
           END-IF
           IF  SNTR-BC3 = 30
               ADD W-KIN TO WN-KI WT-KI WS-KI WA-KI
           END-IF
           ADD W-KIN TO WN-GU WT-GU WS-GU WA-GU.
       M-40.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-40
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-KIN
           END-IF
           IF  W-KIN = ZERO
               GO TO M-40
           END-IF
           IF  SNTR-TNC1 NOT = W-TNC1
               GO TO M-50
           END-IF
           IF  SNTR-TNC2 NOT = W-TNC2
               GO TO M-45
           END-IF
           IF  SNTR-TCD = W-TCD
               GO TO M-35
           END-IF
           IF  W-TNC < W-TNCD
               GO TO M-30
           END-IF
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           GO TO M-30.
       M-45.
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 1 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           GO TO M-25.
       M-50.
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 1 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 2 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           GO TO M-20.
       M-55.
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 1 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 2 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           PERFORM ALL-RTN THRU ALL-EX.
           IF  ESTAT = ADV
               GO TO M-10
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MEI-RTN.
           IF  W-TNC < W-TNCD
               GO TO MEI-EX
           END-IF
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 = 22
               GO TO MEI-020
           END-IF
           IF  W-DC = 0
               CALL "SD_Output" USING
                "D-DATA" D-DATA "p" RETURNING RESU
               GO TO MEI-EX
           END-IF
           IF  W-DC = 1
               MOVE WT-D TO WK-D
               MOVE "　　　　（　小　計　）　" TO W-TM
           END-IF
           IF  W-DC = 2
               MOVE WS-D TO WK-D
               MOVE "　　［　合　計　］　　　" TO W-TM
           END-IF
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
           GO TO MEI-EX.
       MEI-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               GO TO MEI-EX
           END-IF
           IF  ESTAT NOT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING
                "SCHT37" RETURNING RESU
               CALL "SD_Output" USING
                "D-MM" D-MM "p" RETURNING RESU
               MOVE 2 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               MOVE 3 TO W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               GO TO MEI-RTN
           END-IF.
       MEI-EX.
           EXIT.
       ALL-RTN.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 NOT = 22
               MOVE WA-D TO WK-D
               MOVE "【　総　合　計　】　　　" TO W-TM
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
           END-IF.
       ALL-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ALL-EX
           END-IF
           IF  W-L1 = 22
               IF  ESTAT NOT = PF9
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Screen_Output" USING
                    "SCHT37" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MM" D-MM "p" RETURNING RESU
                   MOVE 2 TO W-L1
                   CALL "SD_Arg_Match_Line" USING
                    "W-L1" "2" W-L1 RETURNING RESU
                   MOVE 3 TO W-L2
                   CALL "SD_Arg_Match_Line" USING
                    "W-L2" "2" W-L2 RETURNING RESU
                   GO TO ALL-RTN
               END-IF
           END-IF.
       ALL-EX.
           EXIT.
