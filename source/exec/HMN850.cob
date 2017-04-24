       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN850.
      *********************************************************
      *    PROGRAM         :  棚卸決算単価入力　              *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHN85                          *
      *        変更　　　  :  62/05/18                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(021) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　棚卸　決算単価　入力リスト　　＊＊＊".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
           02  F              PIC  X(007) VALUE "   :   ".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　名　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
       01  W-P.
           02  W-PD    OCCURS  59.
             03  P-HCD1       PIC  9(006).
             03  F            PIC  X(001).
             03  P-HNA1       PIC  N(024).
             03  P-T1         PIC ZZZ,ZZ9.
             03  F            PIC  X(003).
             03  P-X          PIC  X(001).
             03  F            PIC  X(003).
             03  P-HCD2       PIC  9(006).
             03  F            PIC  X(001).
             03  P-HNA2       PIC  N(024).
             03  P-T2         PIC ZZZ,ZZ9.
       01  W-DATA.
           02  W-HCD          PIC  9(006).
           02  W-HCDD         PIC  9(006).
           02  W-T            PIC  9(005).
           02  W-DMM          PIC  9(001).
           02  W-INV          PIC  9(001).
           02  W-SU           PIC S9(006).
           02  W-L            PIC  9(002).
           02  W-POC          PIC  9(001).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-LCD.
             03  W-LD         PIC  9(002).
             03  W-CD         PIC  9(001).
           02  W-SHCD         PIC  9(006).
           02  W-EHCD         PIC  9(006) VALUE 999999.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHHTF.
           COPY LIHIM.
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-HCD   PIC  9(006).
             03  A-T     PIC  9(005).
             03  A-DMM   PIC  9(001).
           02  FILLER.
             03  A-SHCD  PIC  9(006).
             03  A-EHCD  PIC  9(006).
       01  C-DSP.
           02  FILLER.
             03  D-HNA   PIC  N(024).
             03  D-T     PIC ZZZZ9.
           02  D-PRN   PIC  X(030) VALUE
                "（  ｺｰﾄﾞ  000000 ～ 999999  ）".
           02  D-PRNC  PIC  X(030) VALUE
                "                              ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME2   PIC  X(025) VALUE
                  "***  HIM REWRITE ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "W-L" "0" "12" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-L" "7" "6" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T" "9" "W-L" "63" "5" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-T" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-L" "71" "1" "A-T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "22" "0" "12" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "22" "17" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "22" "27" "6" "A-SHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "113" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" "N" "3" "0" "113" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-HNA" "N" "W-L" "14" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-T" "ZZZZ9" "W-L" "63" "5" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-T" BY REFERENCE W-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-PRN" "X" "22" "7" "30" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-PRNC" "X" "22" "7" "30" "D-PRN" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "103" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "103" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "16" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
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
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
           MOVE ZERO TO W-POC W-HCDD.
           CALL "SD_Screen_Output" USING "SCHN85" RETURNING RESU.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-10.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 21
               CALL "SCHN85"
               MOVE 4 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF.
       M-15.
           MOVE ZERO TO HHT-KEY.
           MOVE W-HCDD TO HHT-HCD.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" "NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
       M-20.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  HHT-HCD = W-HCDD
               GO TO M-20
           END-IF
           IF  ZERO = HHT-TSU(01) AND HHT-TSU(02) AND HHT-TSU(03)
                 AND HHT-TSU(04) AND HHT-TSU(05) AND HHT-TSU(06)
                 AND HHT-TSU(07) AND HHT-TSU(08) AND HHT-TSU(09)
                 AND HHT-TSU(10)
               GO TO M-20
           END-IF
           COMPUTE W-SU = HHT-TSU(01) + HHT-TSU(02) + HHT-TSU(03)
                        + HHT-TSU(04) + HHT-TSU(05) + HHT-TSU(06)
                        + HHT-TSU(07) + HHT-TSU(08) + HHT-TSU(09)
                        + HHT-TSU(10).
           MOVE HHT-HCD TO W-HCD.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = ADV
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE 0 TO W-INV.
           IF  HI-KT NOT = ZERO
               MOVE HI-KT TO W-T
           ELSE
               MOVE 1 TO W-INV
               MOVE HI-FT TO W-T
           END-IF.
       M-30.
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-T "A-T" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-T = ZERO
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM = 9
               GO TO M-25
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
      *
           MOVE W-HCD TO W-HCDD.
           IF  W-INV = 1
               IF  HI-FT = W-T
                   GO TO M-10
               END-IF
           END-IF
      *
           IF  W-INV = 0
               IF  HI-FT = W-T
                   MOVE ZERO TO W-T
               END-IF
           END-IF
      *           READ HI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-25
           END-IF
           MOVE W-T TO HI-KT.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-40
           END-IF
           GO TO M-10.
       M-40.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               CALL "SD_Output" USING
                "D-PRNC" D-PRNC "p" RETURNING RESU
               MOVE ZERO TO W-HCDD
               GO TO M-15
           END-IF
           IF ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-50
           END-IF
           MOVE W-SHCD TO HI-HCD.
      *           START HI-M KEY NOT < HI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HI-M_PNAME1 "HI-KEY" "NOT < " HI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF.
       M-55.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  HI-KT = ZERO
               GO TO M-55
           END-IF
           IF  HI-HCD > W-EHCD
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-45
           END-IF
      *
           PERFORM S-50 THRU S-60.
           MOVE ZERO TO W-LCD.
       M-60.
           PERFORM S-20 THRU S-30.
       M-65.
      *           READ HI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  HI-KT = ZERO
               GO TO M-65
           END-IF
           IF  HI-HCD > W-EHCD
               GO TO M-70
           END-IF
           GO TO M-60.
       M-70.
           PERFORM S-35 THRU S-45.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "D-PRNC" D-PRNC "p" RETURNING RESU.
           MOVE 0 TO W-POC.
           MOVE ZERO TO W-HCDD
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
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
           IF  W-LD NOT = 60
               GO TO S-25
           END-IF
           IF  W-CD = ZERO
               MOVE 1 TO W-CD
               MOVE ZERO TO W-LD
               GO TO S-20
           END-IF
           PERFORM S-35 THRU S-45.
           PERFORM S-50 THRU S-60.
           MOVE ZERO TO W-LCD.
           GO TO S-20.
       S-25.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
               MOVE HI-HCD TO P-HCD1(W-LD)
               MOVE HI-NAME TO P-HNA1(W-LD)
               MOVE HI-KT TO P-T1(W-LD)
           ELSE
               MOVE HI-HCD TO P-HCD2(W-LD)
               MOVE HI-NAME TO P-HNA2(W-LD)
               MOVE HI-KT TO P-T2(W-LD)
           END-IF.
       S-30.
           EXIT.
       S-35.
           IF  W-POC = 0
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               MOVE DATE-02R TO H-DATE
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-LD.
       S-40.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 60
               IF  P-X(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   GO TO S-40
               END-IF
           END-IF.
       S-45.
           EXIT.
       S-50.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       S-55.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 60
               MOVE SPACE TO P-HNA1(W-LD) P-HNA2(W-LD)
               GO TO S-55
           END-IF.
       S-60.
           EXIT.
