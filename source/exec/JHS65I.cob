       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS65I.
      *********************************************************
      *    PROGRAM         :  “ˆê“`•[’ù³“ü—ÍiÔ‚¿‚á‚ñ–{•Üj*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SJH65I                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001) VALUE 0.
       01  W-CRT.
           02  W-STC          PIC  9(007).
           02  W-DNO          PIC  9(007).
           02  W-MEI.
             03  W-MEID  OCCURS   9.
               04  W-SU       PIC  9(005).
               04  W-NSU      PIC  9(005).
               04  W-GTN      PIC  9(007).
               04  W-UTN      PIC  9(007).
               04  W-GKIN     PIC  9(009).
               04  W-UKIN     PIC  9(009).
           02  W-DNGP.
             03  W-DNEN1      PIC  9(002).
             03  W-DNGPS      PIC  9(006).
           02  W-DMM          PIC  9(001).
       01  W-DATA.
           02  W-SNGP         PIC  9(006).
           02  W-ENGP         PIC  9(006).
           02  W-NGP          PIC  9(006).
           02  W-NGPS  REDEFINES W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-NSU       PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-L1           PIC  9(002).
           02  W-L2           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-SIZM         PIC  N(004).
           02  W-SZ           PIC  X(004).
           02  W-SZD   REDEFINES W-SZ.
             03  F            PIC  X(003).
             03  W-SZH        PIC  X(001).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           COPY LSTAT.
      *
           COPY LITDNA.
           COPY LIAHNH.
           COPY LICODE.
           COPY LIHIM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-STC   PIC  9(007).
             03  A-DNO   PIC  9(007).
           02  FILLER.
             03  A-NSU   PIC  9(005).
           02  A-DNGP  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-HED.
             03  D-NHSN  PIC  N(016).
             03  D-NHPS  PIC 99/99/99 .
           02  D-MEI.
             03  FILLER.
               04  D-JAN   PIC  X(013).
               04  D-HNA   PIC  N(024).
               04  D-SIZM  PIC  N(004).
             03  FILLER.
               04  D-NSU   PIC ZZZZ9 .
               04  D-SU    PIC ZZ,ZZ9 .
               04  D-GTN   PIC ZZZZ,ZZ9 .
               04  D-GKIN  PIC ZZ,ZZZ,ZZ9 .
               04  D-UTN   PIC ZZZZ,ZZ9 .
               04  D-UKIN  PIC ZZ,ZZZ,ZZ9 .
           02  D-TOT.
             03  FILLER  PIC ZZZZ9 .
             03  FILLER  PIC ZZ,ZZ9 .
             03  FILLER  PIC ZZ,ZZZ,ZZ9 .
             03  FILLER  PIC ZZ,ZZZ,ZZ9 .
           02  D-MEIC.
             03  FILLER.
               04  FILLER  PIC  X(013) VALUE "             ".
               04  FILLER  PIC  X(048) VALUE
                    "                                                ".
               04  FILLER  PIC  X(008) VALUE "        ".
             03  FILLER.
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(006) VALUE "      ".
               04  FILLER  PIC  X(008) VALUE "        ".
               04  FILLER  PIC  X(010) VALUE "          ".
               04  FILLER  PIC  X(008) VALUE "        ".
               04  FILLER  PIC  X(010) VALUE "          ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA Å¼  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  ¼¬ÃÝ Å¼  ***".
             03  E-ME3   PIC  X(027) VALUE
                  "***  TDNAF DATA ´×°  ***".
             03  E-ME4   PIC  X(021) VALUE
                  "***  ÌßÛ¸Þ×Ñ ´×°  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  “`•[”­s@Ï‚Ý  ***".
             03  E-ME11  PIC  X(027) VALUE
                  "***  TDNAF REWRITE ´×°  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "2" "0" "14" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STC" "9" "2" "10" "7" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STC" BY REFERENCE W-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "2" "58" "7" "A-STC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "W-L2" "0" "5" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NSU" "9" "W-L2" "31" "5" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NSU" BY REFERENCE W-NSU(1) "5" "1" BY REFERENCE CNT 42
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNGP" "9" "22" "8" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNGP" BY REFERENCE W-DNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "A-DNGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "303" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HED" " " "2" "0" "40" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NHSN" "N" "2" "18" "32" " " "D-HED" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NHSN" BY REFERENCE AHNH-NHSN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NHPS" "99/99/99" "2" "73" "8" "D-NHSN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-NHPS" BY REFERENCE TDNA-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "0" "0" "116" "D-HED" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" " " "W-L1" "0" "69" " " "D-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JAN" "X" "W-L1" "1" "13" " " "01D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "D-JAN" BY REFERENCE TDNA-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "W-L1" "15" "48" "D-JAN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZM" "N" "W-L1" "64" "8" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZM" BY REFERENCE W-SIZM "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" " " "W-L2" "0" "47" "01D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "D-NSU" "ZZZZ9" "W-L2" "31" "5" " " "02D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NSU" BY REFERENCE W-NSU(1) "5" "1" BY REFERENCE CNT 42
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZ,ZZ9" "W-L2" "37" "6" "D-NSU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE TDNA-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "D-GTN" "ZZZZ,ZZ9" "W-L2" "43" "8" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GTN" BY REFERENCE W-GTN(1) "7" "1" BY REFERENCE CNT 42
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKIN" "ZZ,ZZZ,ZZ9" "W-L2" "52" "10" "D-GTN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GKIN" BY REFERENCE W-GKIN(1) "9" "1" BY REFERENCE CNT 42
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UTN" "ZZZZ,ZZ9" "W-L2" "62" "8" "D-GKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTN" BY REFERENCE W-UTN(1) "7" "1" BY REFERENCE CNT 42
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UKIN" "ZZ,ZZZ,ZZ9" "W-L2" "71" "10" "D-UTN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UKIN" BY REFERENCE W-UKIN(1) "9" "1" BY REFERENCE CNT 42
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TOT" " " "22" "0" "31" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TOT" "ZZZZ9" "22" "31" "5" " " "D-TOT" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TOT" BY REFERENCE WT-NSU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TOT" "ZZ,ZZ9" "22" "37" "6" "01D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TOT" BY REFERENCE WT-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TOT" "ZZ,ZZZ,ZZ9" "22" "52" "10" "02D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TOT" BY REFERENCE WT-GKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TOT" "ZZ,ZZZ,ZZ9" "22" "71" "10" "03D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-TOT" BY REFERENCE WT-UKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEIC" " " "0" "0" "116" "D-TOT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEIC" " " "W-L1" "0" "69" " " "D-MEIC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MEIC" "X" "W-L1" "1" "13" " " "01D-MEIC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MEIC" "X" "W-L1" "15" "48" "0101D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MEIC" "X" "W-L1" "64" "8" "0201D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
           "02D-MEIC" " " "W-L2" "0" "47" "01D-MEIC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MEIC" "X" "W-L2" "31" "5" " " "02D-MEIC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MEIC" "X" "W-L2" "37" "6" "0102D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-MEIC" "X" "W-L2" "43" "8" "0202D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-MEIC" "X" "W-L2" "52" "10" "0302D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-MEIC" "X" "W-L2" "62" "8" "0402D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602D-MEIC" "X" "W-L2" "71" "10" "0502D-MEIC" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "133" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "133" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "27" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "21" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH65I" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           MOVE W-MSIZ TO W-ASIZD.
           ACCEPT W-NGP FROM DATE.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NGP TO W-SNGP.
           ACCEPT W-NGP FROM DATE.
           ADD 1 TO W-GET.
           IF  W-GET > 12
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           MOVE W-NGP TO W-ENGP.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE SPACE TO AHNH-NHSN
           END-IF
           CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           MOVE 0 TO W-INV.
           MOVE SPACE TO TDNA-KEY.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
      *           START TDNAF KEY NOT < TDNA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNAF_PNAME1 "TDNA-KEY" " NOT < " TDNA-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF (W-STC NOT = TDNA-STC) OR (W-DNO NOT = TDNA-DNO)
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF  TDNA-PC = 9
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF  TDNA-RC = 1
               IF  W-INV = 0
                   MOVE 1 TO W-INV
               END-IF
           END-IF
      *
           MOVE ZERO TO W-MEI WT-D.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-END = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           IF  W-INV = 1
               GO TO M-15
           END-IF
           MOVE ZERO TO CNT.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
       M-20.
           ADD 1 TO CNT.
           ADD 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  CNT > 9
               GO TO M-35
           END-IF
           IF  W-L1 > 20
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-90
           END-IF
           IF  W-SU(CNT) = ZERO
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-NSU "A-NSU" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           COMPUTE W-GKIN(CNT) = W-NSU(CNT) * W-GTN(CNT).
           COMPUTE W-UKIN(CNT) = W-NSU(CNT) * W-UTN(CNT).
           CALL "SD_Output" USING "D-NSU" D-NSU "p" RETURNING RESU.
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
           GO TO M-20.
       M-30.
           SUBTRACT 1 FROM CNT.
           SUBTRACT 2 FROM W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  CNT = 0
               GO TO M-15
           END-IF
           IF  W-L1 < 4
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  W-SU(CNT) = ZERO
               GO TO M-30
           END-IF
           GO TO M-25.
       M-35.
           MOVE ZERO TO WT-NSU CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT < 10
               ADD W-NSU(CNT) TO WT-NSU
               ADD W-GKIN(CNT) TO WT-GKIN
               ADD W-UKIN(CNT) TO WT-UKIN
               GO TO M-40
           END-IF
           CALL "SD_Output" USING "D-TOT" D-TOT "p" RETURNING RESU.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-DNGP "A-DNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-30
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           IF  W-DNGPS < W-SNGP OR > W-ENGP
               GO TO M-45
           END-IF
           MOVE W-DNGPS TO W-NGP.
           IF  W-NEN < 09
               GO TO M-45
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-45
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-45
           END-IF
           MOVE W-NGP TO W-DNGPS.
           MOVE 20 TO W-DNEN1.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-65
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-65
           END-IF
      *
           MOVE SPACE TO TDNA-KEY.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
      *           START TDNAF KEY NOT < TDNA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNAF_PNAME1 "TDNA-KEY" " NOT < " TDNA-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF (W-STC NOT = TDNA-STC) OR (W-DNO NOT = TDNA-DNO)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE ZERO TO CNT.
       M-70.
           ADD 1 TO CNT.
           IF  CNT > 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  W-NSU(CNT) = TDNA-SU
               MOVE ZERO TO TDNA-NSU TDNA-TSC
           ELSE
               MOVE W-NSU(CNT) TO TDNA-NSU
               MOVE 1 TO TDNA-TSC
           END-IF
           MOVE W-DNGP TO TDNA-DNGP.
           MOVE 0 TO TDNA-PC.
      *           REWRITE TDNA-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF
           IF (W-STC NOT = TDNA-STC) OR (W-DNO NOT = TDNA-DNO)
               GO TO M-75
           END-IF
           GO TO M-70.
       M-75.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH65I" RETURNING RESU.
           CALL "SD_Output" USING "A-STC" A-STC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU.
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DSP-RTN.
           CALL "SD_Output" USING "D-NHPS" D-NHPS "p" RETURNING RESU.
           MOVE TDNA-DNGP TO W-DNGP.
           MOVE ZERO TO CNT.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
       DSP-010.
           ADD 1 TO CNT.
           IF  CNT > 9
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
           ADD 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 > 20
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
      *
           MOVE ZERO TO CODE-HCD.
           MOVE SPACE TO CODE-KEY.
           MOVE ZERO TO CODE-TCD.
           MOVE TDNA-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME W-SIZM
               MOVE "@‚i‚`‚m‚È‚µ@" TO HI-NAME
               GO TO DSP-020
           END-IF
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME W-SIZM
               MOVE "@‚i‚`‚m‚È‚µ@" TO HI-NAME
               GO TO DSP-020
           END-IF
           IF (CODE-TCD NOT = ZERO) OR (CODE-JAN NOT = TDNA-JAN)
               MOVE SPACE TO HI-NAME W-SIZM
               MOVE "@‚i‚`‚m‚È‚µ@" TO HI-NAME
               GO TO DSP-020
           END-IF
           MOVE CODE-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 0 TO HI-HKB
               MOVE SPACE TO HI-NAME
               MOVE "@•i–¼‚È‚µ@" TO HI-NAME
           END-IF
           MOVE W-SIZ(CODE-SIZ,CODE-SNO) TO W-SZ.
           IF  HI-HKB = 1
               MOVE 5 TO W-SZH
           END-IF
           MOVE W-SZ TO W-SIZM.
       DSP-020.
           MOVE TDNA-SU TO W-SU(CNT).
           MOVE TDNA-GTN TO W-GTN(CNT).
           MOVE TDNA-UTN TO W-UTN(CNT).
           IF  TDNA-TSC = 1
               MOVE TDNA-NSU TO W-NSU(CNT)
               COMPUTE W-GKIN(CNT) = TDNA-NSU * TDNA-GTN
               COMPUTE W-UKIN(CNT) = TDNA-NSU * TDNA-UTN
           ELSE
               MOVE TDNA-SU TO W-NSU(CNT)
               MOVE TDNA-GKIN TO W-GKIN(CNT)
               MOVE TDNA-UKIN TO W-UKIN(CNT)
           END-IF
           ADD W-SU(CNT) TO WT-SU.
           ADD W-NSU(CNT) TO WT-NSU.
           ADD W-GKIN(CNT) TO WT-GKIN.
           ADD W-UKIN(CNT) TO WT-UKIN.
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
      *
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO DSP-030
           END-IF
           IF (W-STC NOT = TDNA-STC) OR (W-DNO NOT = TDNA-DNO)
               GO TO DSP-030
           END-IF
           GO TO DSP-010.
       DSP-030.
           ADD 1 TO CNT.
           IF  CNT < 10
               ADD 2 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               COMPUTE W-L2 = W-L1 + 1
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               CALL "SD_Output" USING "D-MEIC" D-MEIC "p" RETURNING RESU
               GO TO DSP-030
           END-IF
           CALL "SD_Output" USING "A-DNGP" A-DNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TOT" D-TOT "p" RETURNING RESU.
       DSP-EX.
           EXIT.
