       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT360.
      *********************************************************
      *    PROGRAM         :  óöï®ïîñÂíSìñï îÑè„ñ‚çáÇπ        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHT36                          *
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
           02  W-DC           PIC  9(001).
           02  W-TNC.
             03  W-TNC1       PIC  9(001).
             03  W-TNC2       PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-DATE.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-KIN          PIC S9(008).
           02  WN-D.
             03  WN-NU        PIC S9(009).
             03  WN-IU        PIC S9(009).
             03  WN-KU        PIC S9(009).
             03  WN-GU        PIC S9(009).
           02  WT-D.
             03  WT-NU        PIC S9(009).
             03  WT-IU        PIC S9(009).
             03  WT-KU        PIC S9(009).
             03  WT-GU        PIC S9(009).
           02  WS-D.
             03  WS-NU        PIC S9(009).
             03  WS-IU        PIC S9(009).
             03  WS-KU        PIC S9(009).
             03  WS-GU        PIC S9(009).
           02  WA-D.
             03  WA-NU        PIC S9(009).
             03  WA-IU        PIC S9(009).
             03  WA-KU        PIC S9(009).
             03  WA-GU        PIC S9(009).
           02  W-TM           PIC  N(002).
           02  W-NEM          PIC  X(021).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
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
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MM.
             03  01D-MM  PIC Z9 .
             03  FILLER  PIC  X(001) VALUE "/".
             03  FILLER  PIC  9(001) VALUE 1.
             03  FILLER  PIC  N(001) VALUE "Å`".
             03  05D-MM  PIC Z9 .
           02  D-DATA.
             03  01D-DATA  PIC  9(002).
             03  02D-DATA  PIC ----,---,--9 .
             03  03D-DATA  PIC ----,---,--9 .
             03  04D-DATA  PIC ----,---,--9 .
             03  05D-DATA  PIC ----,---,--9 .
           02  D-TD.
             03  01D-TD  PIC  N(002).
             03  02D-TD  PIC ----,---,--9 .
             03  03D-TD  PIC ----,---,--9 .
             03  04D-TD  PIC ----,---,--9 .
             03  05D-TD  PIC ----,---,--9 .
           02  D-NEM   PIC  X(021).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
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
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "67" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "131" " " " " RETURNING RESU.
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
           "D-DATA" " " "W-L" "0" "50" "D-MM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DATA" "9" "W-L" "14" "2" " " "D-DATA" RETURNING RESU.
       CALL "SD_From" USING
           "01D-DATA" BY REFERENCE W-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-DATA" "----,---,--9" "W-L" "18" "12" "01D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-DATA" BY REFERENCE WN-NU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-DATA" "----,---,--9" "W-L" "31" "12" "02D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-DATA" BY REFERENCE WN-IU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-DATA" "----,---,--9" "W-L" "44" "12" "03D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-DATA" BY REFERENCE WN-KU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05D-DATA" "----,---,--9" "W-L" "57" "12" "04D-DATA" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "05D-DATA" BY REFERENCE WN-GU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TD" " " "W-L" "0" "52" "D-DATA" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TD" "N" "W-L" "13" "4" " " "D-TD" RETURNING RESU.
       CALL "SD_From" USING
           "01D-TD" BY REFERENCE W-TM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-TD" "----,---,--9" "W-L" "18" "12" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "02D-TD" BY REFERENCE WT-NU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-TD" "----,---,--9" "W-L" "31" "12" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "03D-TD" BY REFERENCE WT-IU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-TD" "----,---,--9" "W-L" "44" "12" "03D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "04D-TD" BY REFERENCE WT-KU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05D-TD" "----,---,--9" "W-L" "57" "12" "04D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING
           "05D-TD" BY REFERENCE WT-GU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NEM" "X" "23" "47" "21" "D-TD" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NEM" BY REFERENCE W-NEM "21" "0" RETURNING RESU.
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
           CALL "SD_Screen_Output" USING "SCHT36" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-HSD TO W-DATE.
           CALL "SD_Output" USING "D-MM" D-MM "p" RETURNING RESU.
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
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-10
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-KIN
           END-IF
           IF  W-KIN = ZERO
               GO TO M-10
           END-IF
           MOVE ZERO TO WA-D.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
           MOVE ZERO TO WS-D.
           MOVE SNTR-TNC TO W-TNC.
       M-20.
           MOVE ZERO TO WN-D.
           MOVE SNTR-TNC2 TO W-TNC2.
       M-25.
           IF  SNTR-BC3 = 10
               ADD W-KIN TO WN-NU WS-NU WA-NU
           END-IF
           IF  SNTR-BC3 = 20
               ADD W-KIN TO WN-IU WS-IU WA-IU
           END-IF
           IF  SNTR-BC3 = 30
               ADD W-KIN TO WN-KU WS-KU WA-KU
           END-IF
           ADD W-KIN TO WN-GU WS-GU WA-GU.
       M-30.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-30
           END-IF
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-KIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-KIN TO W-KIN
           END-IF
           IF  W-KIN = ZERO
               GO TO M-30
           END-IF
           IF  SNTR-TNC1 NOT = W-TNC1
               GO TO M-35
           END-IF
           IF  SNTR-TNC2 = W-TNC2
               GO TO M-25
           END-IF
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           GO TO M-20.
       M-35.
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 1 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           GO TO M-15.
       M-40.
           MOVE 0 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           MOVE 1 TO W-DC.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           PERFORM ALL-RTN THRU ALL-EX.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MEI-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 23
               IF  W-DC = 0
                   CALL "SD_Output" USING
                    "D-DATA" D-DATA "p" RETURNING RESU
                   GO TO MEI-EX
               ELSE
                   MOVE WS-D TO WT-D
                   MOVE "è¨åv" TO W-TM
                   CALL "SD_Output" USING
                    "D-TD" D-TD "p" RETURNING RESU
                   GO TO MEI-EX
               END-IF
           END-IF
           MOVE SPACE TO W-NEM.
           MOVE "ÇmÇdÇwÇsÅ@ÇcÇ`ÇsÇ`   " TO W-NEM.
           CALL "SD_Output" USING "D-NEM" D-NEM "p" RETURNING RESU.
       MEI-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING
                "SCHT36" RETURNING RESU
               CALL "SD_Output" USING "D-MM" D-MM "p" RETURNING RESU
               MOVE 3 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO MEI-RTN
           END-IF.
       MEI-EX.
           EXIT.
       ALL-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               MOVE SPACE TO W-NEM
               MOVE "ÇmÇdÇwÇsÅ@ÇcÇ`ÇsÇ`   " TO W-NEM
           ELSE
               MOVE WA-D TO WT-D
               MOVE "çáåv" TO W-TM
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
               MOVE SPACE TO W-NEM
               MOVE "Å@ÇdÇmÇcÅ@ÇcÇ`ÇsÇ`   " TO W-NEM
           END-IF
           CALL "SD_Output" USING "D-NEM" D-NEM "p" RETURNING RESU.
       ALL-020.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-L = 23
               IF  ESTAT NOT = PF9
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "SD_Screen_Output" USING
                    "SCHT36" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-MM" D-MM "p" RETURNING RESU
                   MOVE 3 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   GO TO ALL-RTN
               END-IF
           END-IF.
       ALL-EX.
           EXIT.
