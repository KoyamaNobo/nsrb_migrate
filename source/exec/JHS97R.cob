       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS97R.
      ********************************************
      *****    •i–¼•Ê‚i‚`‚mƒR[ƒh@–â‡‚¹    *****
      *****      SCREEN : SJH97R             *****
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-TNA          PIC  N(026).
           02  W-SEN          PIC  9(001).
           02  W-HCD          PIC  9(006).
           02  W-HCDD.
             03  W-HCD1       PIC  9(004).
             03  F            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-L.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
           02  W-SIZN         PIC  X(004).
           02  W-ED           PIC  9(001).
       01  W-SET.
           02  W-ADSM.
             03  F            PIC  X(040) VALUE
                  " SET    SS  S   M   L   LL  XL  XXL     ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5     SET".
           02  W-ADSD.
             03  W-ADS   OCCURS   5.
               04  W-DSD   OCCURS  10.
                 05  W-DS     PIC  X(004).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LIHIM.
           COPY LRCODE.
      *FD  CODEW
       01  CODEW_JHS97R.
           02  CODEW_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  CODEW_LNAME    PIC  X(012) VALUE "CODEW_JHS97R".
           02  F              PIC  X(001).
           02  CODEW_KEY1     PIC  X(100) VALUE SPACE.
           02  CODEW_SORT     PIC  X(100) VALUE SPACE.
           02  CODEW_IDLST    PIC  X(100) VALUE SPACE.
           02  CODEW_RES      USAGE  POINTER.
       01  CODEW-R.
           02  CODEW-HCD1     PIC  9(004).
           02  F              PIC  X(060).
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
           02  A-TCD   PIC  9(004).
           02  A-SEN   PIC  9(001).
           02  A-HCD   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-TMSG  PIC  X(042) VALUE
                  "(  ‚`‚k‚k=0 , ƒgƒ‰ƒXƒR’†ŽR‚Ì‚Ý=1   ØÀ°Ý  )".
             03  D-TMSGC PIC  X(042) VALUE
                  "                                          ".
           02  D-MEI.
             03  FILLER.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
               04  FILLER  PIC  X(004).
             03  FILLER  PIC  X(020).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA Å¼  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  Ä¸²»· Å¼  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ËÝÒ² Å¼  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  »²½Þ Å¼  ***".
             03  E-ME5   PIC  N(008) VALUE
                  "‚d‚m‚c@‚c‚`‚s‚`".
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "3" "11" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "4" "47" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "7" "18" "6" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "71" "1" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "227" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "3" "16" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "4" "0" "84" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TMSG" "X" "4" "13" "42" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TMSGC" "X" "4" "13" "42" "D-TMSG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "0" "0" "91" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" " " "W-L1" "16" "71" " " "D-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
           "0101D-MEI" "X" "W-L1" "4" "13" " " "01D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MEI" BY REFERENCE CODE-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MEI" "9" "W-L1" "18" "6" "0101D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MEI" BY REFERENCE CODE-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MEI" "N" "W-L1" "25" "48" "0201D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-MEI" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-MEI" "X" "W-L1" "74" "4" "0301D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-MEI" BY REFERENCE W-SIZN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "X" "W-L2" "25" "20" "01D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE CODE-NAME "20" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "85" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "85" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "N" "24" "15" "16" "E-ME4" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH97R" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE W-ADSM TO W-ADSD.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO CODEW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE SPACE TO W-TNA.
           IF  W-TCD = ZERO
               MOVE "‚i‚`‚mƒR[ƒh" TO W-TNA
               CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU
               CALL "SD_Output" USING "D-TMSG" D-TMSG "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE T-NAME TO W-TNA.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TMSGC" D-TMSGC "p" RETURNING RESU.
           GO TO M-20.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SEN > 1
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               IF  W-TCD = ZERO
                   GO TO M-15
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           MOVE 5 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 6 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 0 TO W-ED.
      *
           IF W-SEN = 0
               CALL "DB_F_Open" USING
                "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE
                CODEF_IDLST "0"
      *               SELECT CODEF WHERE CODE-TCD = W-TCD
      *                            ORDER BY CODE-HCD CODE-SIZ CODE-SNO
      *///////////////
               CALL "DB_Select" USING
                CODEF_PNAME1 "WHERE" 
                "CODE-TCD" "=" W-TCD
                "ORDER BY" "CODE-HCD" "CODE-SIZ" "CODE-SNO"
                RETURNING RET
               GO TO M-30
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" CODEW_PNAME1 " " BY REFERENCE CODEW_IDLST "0".
       M-25.
      *           READ CODEW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEW_PNAME1 BY REFERENCE CODEW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CODEW_IDLST CODEW_PNAME1
               MOVE 1 TO W-ED
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  CODEW-HCD1 < W-HCD1
               GO TO M-25
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
      *           SELECT CODEF WHERE CODE-TCD = W-TCD
      *                          AND CODE-HCD1 = CODEW-HCD1
      *                        ORDER BY CODE-HCD CODE-SIZ CODE-SNO.
      *///////////////
           CALL "DB_Select" USING
            CODEF_PNAME1 "WHERE" 
            "CODE-TCD" "=" W-TCD "AND"
            "CODE-HCD1" "=" CODEW-HCD1
            "ORDER BY" "CODE-HCD" "CODE-SIZ" "CODE-SNO" RETURNING RET.
       M-30.
      *           READ CODEF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING CODEF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE CODEF_IDLST CODEF_PNAME1
               MOVE 1 TO W-ED
               GO TO M-40
           END-IF
           IF  CODE-HCD < W-HCD
               GO TO M-30
           END-IF
      *
           PERFORM SET-RTN THRU SET-EX.
       M-35.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 = 23
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
           GO TO M-30.
       M-40.
           IF  W-SEN = 1
               MOVE 0 TO W-ED
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-85
           END-IF
           IF  ESTAT = BTB
               IF  W-ED NOT = 0
                   GO TO M-60
               ELSE
                   IF  W-SEN = 0
                       CALL "DB_F_Close" USING
                        BY REFERENCE CODEF_IDLST CODEF_PNAME1
                       GO TO M-60
                   ELSE
                       CALL "DB_F_Close" USING
                        BY REFERENCE CODEW_IDLST CODEW_PNAME1
                       GO TO M-60
                   END-IF
               END-IF
           END-IF
           IF  ESTAT = HTB
               IF  W-ED = 0
                   GO TO M-60
               ELSE
                   GO TO M-90
               END-IF
           END-IF
           GO TO M-45.
       M-60.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH97R" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-TCD = ZERO
               CALL "SD_Output" USING "D-TMSG" D-TMSG "p" RETURNING RESU
               CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "D-TMSGC" D-TMSGC "p" RETURNING RESU
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           MOVE 5 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 6 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           GO TO M-35.
       M-85.
           IF  W-ED = 0
               IF  W-SEN = 0
                   CALL "DB_F_Close" USING
                    BY REFERENCE CODEF_IDLST CODEF_PNAME1
               ELSE
                   CALL "DB_F_Close" USING
                    BY REFERENCE CODEF_IDLST CODEF_PNAME1
                   CALL "DB_F_Close" USING
                    BY REFERENCE CODEW_IDLST CODEW_PNAME1
               END-IF
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET-RTN.
           MOVE CODE-HCD TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "––@•i–¼@‚È‚µ@––@" TO HI-NAME
           END-IF
           MOVE 0 TO HI-S(4,10).
           MOVE CODE-SIZ TO W-S.
           MOVE CODE-SNO TO CNT.
           IF  1 NOT = W-S AND CNT
               IF  HI-S(W-S,CNT) = 0
                   MOVE "????" TO W-SIZN
                   GO TO SET-EX
               END-IF
           END-IF
           IF (ZERO NOT = HI-SS(2)) OR (ZERO NOT = HI-SS(3))
                                    OR (ZERO NOT = HI-SS(4))
               ADD 1 TO W-S
           END-IF
           MOVE W-DS(W-S,CNT) TO W-SIZN.
       SET-EX.
           EXIT.
