       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT120.
      *********************************************************
      *    PROGRAM         :  材料受払明細表　　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBT12                          *
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
           02  W-JCD          PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN1       PIC  9(002).
             03  W-NEN2       PIC  9(002).
             03  W-GP         PIC  9(004).
             03  W-GPD   REDEFINES W-GP.
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-ZAI          PIC S9(007)V9(02).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-ED           PIC  9(001).
           02  W-L            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIJM.
      *FD  JUH-F
       01  JUH-F_KBT120.
           02  JUH-F_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JUH-F_LNAME    PIC  X(012) VALUE "JUH-F_KBT120".
           02  F              PIC  X(001).
           02  JUH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JUH-F_SORT     PIC  X(100) VALUE SPACE.
           02  JUH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JUH-F_RES      USAGE  POINTER.
       01  JUH-R.
           02  UH-JCD         PIC  9(006).
           02  UH-NGP.
             03  UH-N1        PIC  9(002).
             03  UH-N2        PIC  9(002).
             03  UH-GPD       PIC  9(004).
             03  UH-GP    REDEFINES UH-GPD.
               04  UH-G       PIC  9(002).
               04  UH-P       PIC  9(002).
           02  UH-ZNSC        PIC  9(001).
           02  UH-SU          PIC S9(007)V9(02).
           02  UH-T           PIC S9(006)V9(02).
           02  UH-KIN         PIC S9(008).
           02  F              PIC  X(024).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-JCD   PIC  9(006).
           02  A-NGP   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-JM.
             03  FILLER  PIC  N(024).
             03  FILLER  PIC ----,--9.99 .
             03  FILLER  PIC  9(002).
           02  FILLER.
             03  D-NGP.
               04  FILLER  PIC  9(002).
               04  FILLER  PIC ZZ .
               04  FILLER  PIC ZZ .
             03  D-NS    PIC -----,---.-- .
             03  D-T     PIC ----,---.-- .
             03  D-SS    PIC -----,---.-- .
             03  D-ZS    PIC -----,---.-- .
             03  D-ZK    PIC ----,---,--- .
             03  D-ZAI   PIC -----,---.-- .
           02  FILLER.
             03  D-NM    PIC  X(045) VALUE
                  "NEXT=ﾘﾀｰﾝ , ｺｰﾄﾞ=F10 , 年月日=F5 , 終了=F9   ".
             03  D-EM    PIC  X(012) VALUE "[ END DATA ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ｻﾞｲｺ ｴﾗｰ  ***".
             03  E-ME3   PIC  X(016) VALUE
                  "***  J-M ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "13" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD" "9" "4" "4" "6" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGP" "9" "7" "4" "6" "A-JCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGP" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "67" "1" "A-NGP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "195" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JM" " " "4" "0" "61" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-JM" "N" "4" "11" "48" " " "D-JM"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-JM" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-JM" "----,--9.99" "4" "60" "11" "01D-JM" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "02D-JM" BY REFERENCE J-ST "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-JM" "9" "4" "73" "2" "02D-JM" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "03D-JM" BY REFERENCE J-BKC "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "77" "D-JM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NGP" " " "W-L" "0" "6" " " "02C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NGP" "9" "W-L" "4" "2" " " "D-NGP"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NGP" BY REFERENCE UH-N2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NGP" "ZZ" "W-L" "6" "2" "01D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NGP" BY REFERENCE UH-G "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-NGP" "ZZ" "W-L" "8" "2" "02D-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-NGP" BY REFERENCE UH-P "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NS" "-----,---.--" "W-L" "11" "12" "D-NGP" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "D-NS" BY REFERENCE UH-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "----,---.--" "W-L" "24" "11" "D-NS" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE UH-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SS" "-----,---.--" "W-L" "36" "12" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SS" BY REFERENCE UH-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZS" "-----,---.--" "W-L" "49" "12" "D-SS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZS" BY REFERENCE UH-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZK" "----,---,---" "W-L" "62" "12" "D-ZS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZK" BY REFERENCE UH-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZAI" "-----,---.--" "W-L" "49" "12" "D-ZK" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZAI" BY REFERENCE W-ZAI "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "23" "0" "57" "02C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "23" "45" " " "03C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "10" "12" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "141" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "141" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "16" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT12" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO JUH-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-JM" D-JM "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NGP "A-NGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE 20 TO W-NEN1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JUH-F_PNAME1 " " BY REFERENCE JUH-F_IDLST "0".
       M-20.
      *           READ JUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JUH-F_PNAME1 BY REFERENCE JUH-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               GO TO M-10
           END-IF
           IF  W-JCD < UH-JCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               GO TO M-10
           END-IF
           IF  W-JCD > UH-JCD
               GO TO M-20
           END-IF
           MOVE ZERO TO W-ZAI W-DC.
           MOVE ZERO TO W-ED.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
           IF  W-NGP < UH-NGP
               GO TO M-30
           END-IF
           IF  W-NGP = UH-NGP
               MOVE 9 TO W-DC
               GO TO M-30
           END-IF
           IF  UH-ZNSC = 0
               MOVE UH-SU TO W-ZAI
           END-IF
           IF  UH-ZNSC = 1
               ADD UH-SU TO W-ZAI
           END-IF
           IF  UH-ZNSC = 2
               SUBTRACT UH-SU FROM W-ZAI
           END-IF
           MOVE 1 TO W-DC.
           GO TO M-35.
       M-30.
           IF  W-DC = 0 OR 1
               MOVE 9 TO W-DC
               IF  W-ZAI NOT = ZERO
                   ADD 1 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   CALL "SD_Output" USING
                    "D-ZAI" D-ZAI "p" RETURNING RESU
               END-IF
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO TO M-40
           END-IF
           CALL "SD_Output" USING "D-NGP" D-NGP "p" RETURNING RESU.
           IF  UH-ZNSC = 0
               MOVE UH-SU TO W-ZAI
               CALL "SD_Output" USING "D-ZS" D-ZS "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZK" D-ZK "p" RETURNING RESU
           END-IF
           IF  UH-ZNSC = 1
               ADD UH-SU TO W-ZAI
               CALL "SD_Output" USING "D-NS" D-NS "p" RETURNING RESU
               CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZAI" D-ZAI "p" RETURNING RESU
           END-IF
           IF  UH-ZNSC = 2
               SUBTRACT UH-SU FROM W-ZAI
               CALL "SD_Output" USING "D-SS" D-SS "p" RETURNING RESU
               CALL "SD_Output" USING "D-ZAI" D-ZAI "p" RETURNING RESU
           END-IF.
       M-35.
      *           READ JUH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JUH-F_PNAME1 BY REFERENCE JUH-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-ED
               GO TO M-40
           END-IF
           IF  W-JCD < UH-JCD
               MOVE 1 TO W-ED
               GO TO M-40
           END-IF
           GO TO M-25.
       M-40.
           IF  W-ED = 1
               CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU
               IF  W-DC = 0 OR 1
                   IF  W-ZAI NOT = ZERO
                       ADD 1 TO W-L
                       CALL "SD_Arg_Match_Line" USING
                        "W-L" "2" W-L RETURNING RESU
                       CALL "SD_Output" USING
                        "D-ZAI" D-ZAI "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               GO TO M-90
           END-IF
           IF  ESTAT NOT = ADV AND PF5 AND HTB
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT12" RETURNING RESU.
           IF (ESTAT = ADV) OR (ESTAT = HTB AND W-ED = 1)
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               GO TO M-10.
           CALL "SD_Output" USING "A-JCD" A-JCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-JM" D-JM "p" RETURNING RESU.
           IF  ESTAT = PF5
               CALL "DB_F_Close" USING
                BY REFERENCE JUH-F_IDLST JUH-F_PNAME1
               GO TO M-15
           ELSE
               IF  ESTAT = HTB
                   MOVE 6 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   GO TO M-30
               END-IF
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
