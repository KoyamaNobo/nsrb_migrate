       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT310.
      *********************************************************
      *    PROGRAM         :  仕入先日付別仕入明細問合せ　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBT31                          *
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
       01  W-D.
           02  W-SCD          PIC  9(004).
           02  W-SJCD         PIC  9(006).
           02  W-EJCD         PIC  9(006).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GP.
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
           02  W-NGPD  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-DCN          PIC  N(004).
           02  W-KIN          PIC S9(008).
           02  W-L1           PIC  9(002).
           02  W-L2           PIC  9(002).
           02  W-END          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LIJM.
           COPY LSJSSW.
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
           02  FILLER.
             03  A-SCD   PIC  9(004).
             03  A-SJCD  PIC  9(006).
             03  A-EJCD  PIC  9(006).
           02  A-NGPS  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNA   PIC  N(024).
           02  D-MEI.
             03  FILLER.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  X(001)        VALUE "-".
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
             03  FILLER.
               04  FILLER  PIC  N(004).
               04  FILLER  PIC ZZ/ZZ/ZZ .
               04  FILLER  PIC --,---,---.-- .
               04  FILLER  PIC ----,---.-- .
               04  FILLER  PIC ---,---,--- .
           02  FILLER.
             03  D-NM    PIC  X(053) VALUE
               "NEXT=ﾘﾀｰﾝ , ｺｰﾄﾞ=F10 , 材料=F8 , 日付=F5 , 終了=F9   ".
             03  D-EM    PIC  X(012) VALUE "[ END DATA ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  ｼｲﾚｻｷ ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-ACP" " " "0" "0" "23" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "4" "0" "16" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "4" "1" "4" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SJCD" "9" "4" "55" "6" "A-SCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SJCD" BY REFERENCE W-SJCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EJCD" "9" "4" "63" "6" "A-SJCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EJCD" BY REFERENCE W-EJCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NGPS" "9" "8" "1" "6" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NGPS" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "72" "1" "A-NGPS" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "232" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "4" "6" "48" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "0" "0" "119" "D-SNA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" " " "W-L1" "6" "68" " " "D-MEI"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MEI" "9" "W-L1" "1" "6" " " "01D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MEI" BY REFERENCE JR-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0201D-MEI" "9" "W-L1" "8" "6" "0101D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MEI" BY REFERENCE JR-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MEI" "X" "W-L1" "14" "1" "0201D-MEI" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0401D-MEI" "9" "W-L1" "15" "1" "0301D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401D-MEI" BY REFERENCE JR-GNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501D-MEI" "9" "W-L1" "17" "6" "0401D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0501D-MEI" BY REFERENCE JR-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0601D-MEI" "N" "W-L1" "24" "48" "0501D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0601D-MEI" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" " " "W-L2" "6" "51" "01D-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MEI" "N" "W-L2" "26" "8" " " "02D-MEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MEI" BY REFERENCE W-DCN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MEI" "ZZ/ZZ/ZZ" "W-L2" "35" "8" "0102D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-MEI" BY REFERENCE JR-SDAT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-MEI" "--,---,---.--" "W-L2" "44" "13" "0202D-MEI"
           " "  RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-MEI" BY REFERENCE JR-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-MEI" "----,---.--" "W-L2" "58" "11" "0302D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402D-MEI" BY REFERENCE JR-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-MEI" "---,---,---" "W-L2" "70" "11" "0402D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502D-MEI" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "23" "0" "65" "D-MEI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "20" "53" " " "03C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EM" "X" "23" "1" "12" "D-NM" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "235" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "235" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "75" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "75" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT31" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JSSR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-SJCD "A-SJCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-EJCD "A-EJCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-SJCD > W-EJCD
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-NGPS "A-NGPS" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE 20 TO W-NEN1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JSSR-F_PNAME1 " " BY REFERENCE JSSR-F_IDLST "0".
       M-30.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  JR-SCD < W-SCD
               GO TO M-30
           END-IF
           IF  JR-SCD > W-SCD
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  JR-JCD < W-SJCD OR > W-EJCD
               GO TO M-30
           END-IF
           IF  JR-DATE < W-NGP
               GO TO M-30
           END-IF
      *
           MOVE 0 TO W-END.
           MOVE 6 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 7 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       M-35.
           ADD 2 TO W-L1 W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 > 21
               CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU
               GO TO M-45
           END-IF
           MOVE JR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　　材料マスタなし　" TO J-NAME
           END-IF
           MOVE SPACE TO W-DCN.
           IF  JR-DC = 11
               MOVE "数量訂正" TO W-DCN
           ELSE
               IF  JR-DC = 12
                   MOVE "単価訂正" TO W-DCN
               ELSE
                   IF  JR-DC = 13
                       MOVE "輸入振替" TO W-DCN
                   ELSE
                       IF  JR-DC = 14
                           MOVE "無償他　" TO W-DCN
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE JR-KIN TO W-KIN.
           IF  JR-SHZ NOT = ZERO
               MOVE JR-SHZ TO W-KIN
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
       M-40.
      *           READ JSSR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSSR-F_PNAME1 BY REFERENCE JSSR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  JR-SCD > W-SCD
               MOVE 1 TO W-END
               CALL "SD_Output" USING "D-EM" D-EM "p" RETURNING RESU
               GO TO M-45
           END-IF
           IF  JR-JCD < W-SJCD OR > W-EJCD
               GO TO M-40
           END-IF
           IF  JR-DATE < W-NGP
               GO TO M-40
           END-IF
           GO TO M-35.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  W-END = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Screen_Output" USING "SCBT31" RETURNING RESU
               CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU
               CALL "SD_Output" USING "A-SJCD" A-SJCD "p" RETURNING RESU
               CALL "SD_Output" USING "A-EJCD" A-EJCD "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND ADV AND PF8 AND PF5
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT31" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SJCD" A-SJCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EJCD" A-EJCD "p" RETURNING RESU.
           IF  ESTAT = HTB
               MOVE 6 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               MOVE 7 TO W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               GO TO M-35
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           IF  ESTAT = PF8
               GO TO M-15
           END-IF
           IF  ESTAT = PF5
               GO TO M-25
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JSSR-F_IDLST JSSR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
