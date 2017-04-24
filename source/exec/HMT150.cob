       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMT150.
      *******************************************************************
      *    PROGRAM         :  履物得意先品種別売上集計問合せ            *
      *    PRINTER TYPE    :  JIPS                                      *
      *    SCREEN          :  SCHT15                                    *
      *******************************************************************
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
           02  W-NGP.
             03  F            PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-L            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-RD.
             03  W-SU         PIC S9(006).
             03  W-UKIN       PIC S9(009).
       01  WN-D.
           02  WN-SU          PIC S9(006).
           02  WN-UKI         PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(006).
           02  WT-UKI         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
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
           02  A-TCD   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-GP.
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-MD.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
               04  FILLER  PIC ----,--- .
               04  FILLER  PIC ----,---,--- .
             03  D-TD.
               04  FILLER  PIC  N(009)       VALUE
                    "【　　合　計　　】".
               04  FILLER  PIC ----,--- .
               04  FILLER  PIC ----,---,--- .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
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
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "3" "9" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "73" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "168" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GP" " " "2" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GP" "Z9" "2" "67" "2" " " "D-GP" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-GP" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-GP" "Z9" "2" "71" "2" "01D-GP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-GP" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "3" "14" "52" "D-GP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L" "0" "112" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "W-L" "0" "74" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" "9" "W-L" "2" "6" " " "D-MD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD" "N" "W-L" "9" "48" "01D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD" "----,---" "W-L" "58" "8" "02D-MD" " " 
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD" BY REFERENCE WN-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MD" "----,---,---" "W-L" "67" "12" "03D-MD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MD" BY REFERENCE WN-UKI "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "W-L" "0" "38" "D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TD" "N" "W-L" "37" "18" " " "D-TD" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TD" "----,---" "W-L" "58" "8" "01D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TD" BY REFERENCE WT-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TD" "----,---,---" "W-L" "67" "12" "02D-TD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TD" BY REFERENCE WT-UKI "9" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "95" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "95" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
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
           COPY LIBCPR.
           MOVE D-HSD TO W-NGP.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Screen_Output" USING "SCHT15" RETURNING RESU.
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE ZERO TO WT-D W-EC.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SNTRF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-20.
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
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  SNTR-TCD > W-TCD
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  SNTR-TCD < W-TCD
               GO TO M-20
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-20
           END-IF
           PERFORM S-05 THRU S-10.
           IF  ZERO = W-SU AND W-UKIN
               GO TO M-20
           END-IF.
       M-25.
           MOVE ZERO TO WN-D.
           MOVE SNTR-HCD TO W-HCD.
       M-30.
           ADD W-SU TO WN-SU.
           ADD W-UKIN TO WN-UKI.
       M-35.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-EC
               GO TO M-45
           END-IF
           IF  SNTR-TCD > W-TCD
               MOVE 1 TO W-EC
               GO TO M-45
           END-IF
           IF  SNTR-DC = 4 OR 8
               GO TO M-35
           END-IF
           PERFORM S-05 THRU S-10.
           IF  ZERO = W-SU AND W-UKIN
               GO TO M-35
           END-IF
           IF  SNTR-TCD NOT = W-TCD
               MOVE 1 TO W-EC
               GO TO M-45
           END-IF
           IF  W-HCD NOT = SNTR-HCD
               GO TO M-40
           END-IF
           GO TO M-30.
       M-40.
           PERFORM S-15 THRU S-20.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               GO TO M-90
           END-IF
           IF  W-DMM = 9
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               GO TO M-10
           END-IF
           GO TO M-25.
       M-45.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           PERFORM S-15 THRU S-20.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           PERFORM S-25 THRU S-35.
           IF  W-DMM = 1
               CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM"
                "9" "1" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-RD.
           IF (SNTR-SNC = 1) OR (SNTR-DC = 1 OR 2 OR 5)
               COMPUTE W-SU = SNTR-SU * -1
               COMPUTE W-UKIN = SNTR-KIN * -1
           ELSE
               MOVE SNTR-SU TO W-SU
               MOVE SNTR-KIN TO W-UKIN
           END-IF
           IF (SNTR-HCD > 999899) OR (SNTR-SNC = 1) OR (SNTR-DC = 2)
               MOVE ZERO TO W-SU
           END-IF.
       S-10.
           EXIT.
       S-15.
           IF  ZERO = WN-SU AND WN-UKI
               GO TO S-20
           END-IF
           PERFORM S-25 THRU S-35.
           IF  ESTAT = PF9
               GO TO S-20
           END-IF
           IF  W-DMM = 9
               GO TO S-20
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　　　" TO HI-NAME
           END-IF
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
      *
           ADD WN-SU TO WT-SU.
           ADD WN-UKI TO WT-UKI.
       S-20.
           EXIT.
       S-25.
           MOVE 1 TO W-DMM.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 23
               GO TO S-35
           END-IF.
       S-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO S-35.
           IF  ESTAT NOT = HTB
               GO TO S-30
           END-IF
           IF  W-DMM = 9
               GO TO S-35
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-30
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHT15" RETURNING RESU.
           CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE 5 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-35.
           EXIT.
