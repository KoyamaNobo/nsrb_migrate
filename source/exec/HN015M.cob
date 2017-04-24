       IDENTIFICATION DIVISION.
       PROGRAM-ID. HN015M.
      *********************************************************
      *    PROGRAM         :  履物得意先品名単価　問合せ      *
      *    SCREEN          :  SH015M                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  CHK            PIC  9(001).
           02  W-END          PIC  9(001).
           02  W-SEN          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-HCD          PIC  9(006).
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITM.
           COPY LIHIM.
           COPY LITHTM.
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
           02  A-SEN   PIC  9(001).
      *
           02  FILLER.
             03  A-TCD1   PIC  9(004).
             03  A-HCD1   PIC  9(006).
           02  FILLER.
             03  A-TCD2   PIC  9(004).
             03  A-HCD2   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID   PIC  X(004) VALUE "    ".
           02  D-MEC.
             03  FILLER  PIC  X(006) VALUE "      ".
             03  FILLER  PIC  X(014) VALUE
                  "              ".
           02  D-ME1.
             03  FILLER.
               04  FILLER  PIC  X(002) VALUE "  ".
               04  FILLER  PIC  N(003) VALUE "得意先".
             03  FILLER  PIC  N(007) VALUE
                  "品　　　　　名".
           02  D-ME2.
             03  FILLER  PIC  N(003) VALUE "品　名".
             03  FILLER  PIC  N(007) VALUE
                  "得　意　先　名".
           02  FILLER.
             03  D-TNA    PIC  N(026).
             03  D-HNA    PIC  N(024).
           02  FILLER.
             03  D-MEIH.
               04  01D-MEIH  PIC  9(006).
               04  02D-MEIH  PIC  N(024).
               04  03D-MEIH  PIC  9(001).
               04  04D-MEIH  PIC  Z(005).
             03  D-MEIT.
               04  01D-MEIT  PIC  9(004).
               04  02D-MEIT  PIC  N(026).
               04  03D-MEIT  PIC  9(001).
               04  04D-MEIT  PIC  Z(005).
             03  D-SPC.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  ﾃｲｼ ﾌﾞﾝ  ***".
             03  E-ME9   PIC  N(008) VALUE
                  "ＥＮＤ　ＤＡＴＡ".
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
           "C-ACP" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SEN" "9" "3" "54" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "5" "0" "10" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD1" "9" "5" "8" "4" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD1" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD1" "9" "5" "8" "6" "A-TCD1" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD1" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "8" "0" "10" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-TCD2" "9" "8" "4" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-TCD2" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD2" "9" "8" "3" "6" "A-TCD2" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD2" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "67" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "368" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MID" "X" "7" "75" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-MEC" " " "0" "0" "20" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MEC" "X" "5" "15" "6" " " "D-MEC" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MEC" "X" "7" "10" "14" "01D-MEC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-ME1" " " "0" "0" "22" "D-MEC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-ME1" " " "5" "0" "8" " " "D-ME1" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-ME1" "X" "5" "12" "2" " " "01D-ME1" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-ME1" "N" "5" "15" "6" "02D-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04D-ME1" "N" "7" "10" "14" "01D-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-ME2" " " "0" "0" "20" "D-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-ME2" "N" "5" "15" "6" " " "D-ME2" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-ME2" "N" "7" "10" "14" "01D-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "5" "0" "100" "D-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-TNA" "N" "5" "22" "52" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HNA" "N" "5" "22" "48" "D-TNA" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-DSP" " " "W-L" "0" "202" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-MEIH" " " "W-L" "0" "60" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MEIH" "9" "W-L" "3" "6" " " "D-MEIH" RETURNING RESU.
       CALL "SD_From" USING
           "01D-MEIH" BY REFERENCE THT-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MEIH" "N" "W-L" "10" "48" "01D-MEIH" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-MEIH" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-MEIH" "9" "W-L" "65" "1" "02D-MEIH" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-MEIH" BY REFERENCE THT-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-MEIH" "Z" "W-L" "68" "5" "03D-MEIH" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-MEIH" BY REFERENCE THT-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-MEIT" " " "W-L" "0" "62" "D-MEIH" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-MEIT" "9" "W-L" "4" "4" " " "D-MEIT" RETURNING RESU.
       CALL "SD_From" USING
           "01D-MEIT" BY REFERENCE THT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-MEIT" "N" "W-L" "10" "52" "01D-MEIT" " " RETURNING RESU.
       CALL "SD_From" USING
           "02D-MEIT" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "03D-MEIT" "9" "W-L" "65" "1" "02D-MEIT" " " RETURNING RESU.
       CALL "SD_From" USING
           "03D-MEIT" BY REFERENCE THT-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "04D-MEIT" "Z" "W-L" "68" "5" "03D-MEIT" " " RETURNING RESU.
       CALL "SD_From" USING
           "04D-MEIT" BY REFERENCE THT-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SPC" " " "W-L" "0" "80" "D-MEIT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-SPC" "X" "W-L" "1" "40" " " "D-SPC" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-SPC" "X" "W-L" "41" "40" "01D-SPC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "85" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "85" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME9" "N" "24" "15" "16" "E-ME4" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SH015M" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MEC" D-MEC "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN = 9
               GO TO M-90
           END-IF
           IF  W-SEN NOT = 1 AND 2
               GO TO M-10
           END-IF.
       M-15.
           IF  W-SEN = 1
               CALL "SD_Output" USING "D-ME1" D-ME1 "p" RETURNING RESU
           END-IF
           IF  W-SEN = 2
               CALL "SD_Output" USING "D-ME2" D-ME2 "p" RETURNING RESU
               GO TO M-50
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-TCD1 "A-TCD1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
      *
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
      *
           MOVE SPACE TO THT-KEY.
           MOVE W-TCD TO THT-TCD.
       M-25.
      *           START THTM KEY NOT < THT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY" "NOT < " THT-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO M-20
           END-IF
      *           READ THTM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO M-20
           END-IF
           IF  W-TCD NOT = THT-TCD
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-30.
           MOVE 0 TO W-END.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-35.
           MOVE THT-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊　品名　なし　＊" TO HI-NAME
           END-IF
           IF  HI-ENG NOT = ZERO
               GO TO M-37
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 22
               GO TO M-40
           END-IF
           CALL "SD_Output" USING "D-MEIH" D-MEIH "p" RETURNING RESU.
       M-37.
      *           READ THTM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  W-TCD NOT = THT-TCD
               MOVE 1 TO W-END
               CALL "SD_Output" USING "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO M-40
           END-IF
           GO TO M-35.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               PERFORM CLE-RTN THRU CLE-EX
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-40
           END-IF
           IF  W-END = 1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SH015M" RETURNING RESU
               CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU
               CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU
               GO TO M-15
           END-IF
           PERFORM CLE-RTN THRU CLE-EX.
           GO TO M-30.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-HCD2 "A-HCD2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
      *
           MOVE SPACE TO THT-KEY.
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD TO THT-HCD.
           GO TO M-25.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-HCD1 "A-HCD1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  HI-ENG NOT = ZERO
               CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
      *
           MOVE 0 TO CHK.
           MOVE SPACE TO THT-KEY2.
           MOVE W-HCD TO THT-HCD.
       M-55.
      *           START THTM KEY NOT < THT-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            THTM_PNAME1 "THT-KEY2" "NOT < " THT-KEY2 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO M-50
           END-IF.
       M-57.
      *           READ THTM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  W-HCD NOT = THT-HCD
               CALL "SD_Output" USING "E-ME3" E-ME3 "p" RETURNING RESU
               GO TO M-50
           END-IF
           IF  CHK = 1
               IF  W-TCD > THT-TCD2
                   GO TO M-57
               END-IF
           END-IF.
       M-60.
           MOVE 0 TO W-END.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-65.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 22
               GO TO M-70
           END-IF
           MOVE THT-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE "＊　得意先　なし　＊" TO T-NAME
           END-IF
           CALL "SD_Output" USING "D-MEIT" D-MEIT "p" RETURNING RESU.
      *
      *           READ THTM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT RECORD AT END" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO M-70
           END-IF
           IF  W-HCD NOT = THT-HCD
               MOVE 1 TO W-END
               CALL "SD_Output" USING "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO M-70
           END-IF
           GO TO M-65.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               PERFORM CLE-RTN THRU CLE-EX
               GO TO M-75
           END-IF
           IF  ESTAT NOT = HTB
               GO TO M-70
           END-IF
           IF  W-END = 1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SH015M" RETURNING RESU
               CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU
               CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU
               GO TO M-15
           END-IF
           PERFORM CLE-RTN THRU CLE-EX.
           GO TO M-60.
       M-75.
           CALL "SD_Accept" USING BY REFERENCE A-TCD2 "A-TCD2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-75
           END-IF
      *
           MOVE 1 TO CHK.
           MOVE SPACE TO THT-KEY2.
           MOVE W-HCD TO THT-HCD.
           GO TO M-55.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CLE-RTN.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       CLE-020.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L NOT = 22
               CALL "SD_Output" USING "D-SPC" D-SPC "p" RETURNING RESU
               GO TO CLE-020
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       CLE-EX.
           EXIT.
