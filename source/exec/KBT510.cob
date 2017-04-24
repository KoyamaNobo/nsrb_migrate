       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT510.
      **************************************************************
      *    PROGRAM         :  óöï®êªïiédì¸Å@ïiñºï î≠íçì¸å…écñ‚çáÇπ *
      *    PRINTER TYPE    :  JIPS                                 *
      *    SCREEN          :  SCBT51                               *
      *    COMPILE TYPE    :  COBOL                                *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-HCD          PIC  9(006).
           02  W-NO           PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-G            PIC  9(002).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-ASUD.
             03  W-GYO   OCCURS  11.
               04  W-ASU   OCCURS   4.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(005).
               04  W-SUT      PIC S9(006).
           02  W-KIN          PIC S9(008).
           02  W-KEY2         PIC  X(014).
           02  W-AGSD.
             03  W-AGSU  OCCURS   4.
               04  W-GSUD  OCCURS  10.
                 05  W-GSU    PIC S9(005).
           02  W-TSU          PIC S9(006).
           02  W-ANSD.
             03  W-ANSU  OCCURS   4.
               04  W-NSUD  OCCURS  10.
                 05  W-NSU    PIC S9(005).
             03  W-NSUT       PIC S9(006).
           02  W-ZC           PIC  9(001).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LIHIM.
           COPY LIHSHF.
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
           02  A-HCD   PIC  9(006).
           02  A-NO    PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-HNA   PIC  N(024).
           02  D-MEI.
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  N(015).
             03  FILLER  PIC ------9 .
             03  FILLER  PIC ZZZZ9 .
             03  FILLER  PIC --------9 .
             03  FILLER  PIC  9(006).
             03  FILLER  PIC  9(002).
             03  FILLER  PIC  X(001)   VALUE "-".
             03  FILLER  PIC  9(004).
             03  FILLER  PIC  X(001)   VALUE "-".
             03  FILLER  PIC  9(002).
           02  D-SSD.
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC ------9 .
           02  D-GSD.
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC  -(006).
             03  FILLER  PIC ------9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
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
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "2" "6" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "19" "1" "2" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "76" "1" "A-NO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "183" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "2" "13" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "73" "D-HNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" "9" "W-L" "1" "2" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MEI" BY REFERENCE W-G "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" "9" "W-L" "4" "4" "01D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MEI" BY REFERENCE HSH-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MEI" "N" "W-L" "9" "30" "02D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MEI" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MEI" "------9" "W-L" "40" "7" "03D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-MEI" BY REFERENCE W-SUT(1) "6" "1" BY REFERENCE W-G 206
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MEI" "ZZZZ9" "W-L" "48" "5" "04D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-MEI" BY REFERENCE HSH-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-MEI" "--------9" "W-L" "54" "9" "05D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-MEI" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-MEI" "9" "W-L" "64" "6" "06D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "07D-MEI" BY REFERENCE HSH-NNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-MEI" "9" "W-L" "71" "2" "07D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "08D-MEI" BY REFERENCE HSH-RSN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-MEI" "X" "W-L" "73" "1" "08D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-MEI" "9" "W-L" "74" "4" "09D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "10D-MEI" BY REFERENCE HSH-RNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "11D-MEI" "X" "W-L" "78" "1" "10D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12D-MEI" "9" "W-L" "79" "2" "11D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "12D-MEI" BY REFERENCE HSH-RND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSD" " " "0" "0" "31" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01D-SSD" "------" "19" "W-C" "6" " " "D-SSD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" 
            BY REFERENCE W-G 206 "1" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SSD" "------" "20" "W-C" "6" "01D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" 
            BY REFERENCE W-G 206 "2" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-SSD" "------" "21" "W-C" "6" "02D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" 
            BY REFERENCE W-G 206 "3" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-SSD" "------" "22" "W-C" "6" "03D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-SSD" BY REFERENCE W-SU(1,1,1) "5" "3" 
            BY REFERENCE W-G 206 "4" 50 
            BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-SSD" "------9" "22" "71" "7" "04D-SSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-SSD" BY REFERENCE W-SUT(1) "6" "1" BY REFERENCE W-G 206
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSD" " " "0" "0" "31" "D-SSD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GSD" "------" "19" "W-C" "6" " " "D-GSD"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-GSD" BY REFERENCE W-GSU(1,1) "5" "2" 
            "1" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-GSD" "------" "20" "W-C" "6" "01D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-GSD" BY REFERENCE W-GSU(1,1) "5" "2" 
            "2" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-GSD" "------" "21" "W-C" "6" "02D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-GSD" BY REFERENCE W-GSU(1,1) "5" 
            "2" "3" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-GSD" "------" "22" "W-C" "6" "03D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-GSD" BY REFERENCE W-GSU(1,1) "5" 
            "2" "4" 50 BY REFERENCE CNT 5 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-GSD" "------9" "22" "71" "7" "04D-GSD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-GSD" BY REFERENCE W-TSU "6" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "30" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Screen_Output" USING "SCBT51" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
            "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
            HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ïiñºÅ@Ç»Çµ  ***          " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
      *
           MOVE SPACE TO HSH-KEY2.
           MOVE W-HCD TO HSH-HCD.
      *           START HSHF KEY NOT < HSH-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHF_PNAME1 "HSH-KEY2" " NOT < " HSH-KEY2 RETURNING RET.
           IF  RET = 1
               MOVE "***  ÉfÅ[É^Å@Ç»Çµ  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF.
       M-15.
      *           READ HSHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ÉfÅ[É^Å@Ç»Çµ  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  HSH-HCD NOT = W-HCD
               MOVE "***  ÉfÅ[É^Å@Ç»Çµ  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  HSH-ENGP NOT = ZERO
               GO TO M-15
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           IF  W-ZC = 0
               GO TO M-15
           END-IF.
       M-20.
           MOVE ZERO TO W-ASUD W-G.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-25.
           ADD 1 TO W-G.
           IF  W-G = 12
               MOVE HSH-KEY2 TO W-KEY2
               GO TO M-35
           END-IF
           MOVE W-ANSD TO W-GYO(W-G).
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           COMPUTE W-KIN = W-SUT(W-G) * HSH-T.
           MOVE HSH-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "Å@ÅñÅñÅñÅ@édì¸êÊÅ@Ç»ÇµÅ@ÅñÅñÅñ" TO S-NAME
           END-IF
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
       M-30.
      *           READ HSHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-KEY2
               MOVE "ÇdÇmÇcÅ@ÇcÇ`ÇsÇ`              " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  HSH-HCD NOT = W-HCD
               MOVE SPACE TO W-KEY2
               MOVE "ÇdÇmÇcÅ@ÇcÇ`ÇsÇ`              " TO W-EM
               GO TO M-35
           END-IF
           IF  HSH-ENGP NOT = ZERO
               GO TO M-30
           END-IF
           PERFORM SET-RTN THRU SET-EX.
           IF  W-ZC = 0
               GO TO M-30
           END-IF
           GO TO M-25.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-40
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM = 9
               CALL "SD_Screen_Output" USING "SCBT51" RETURNING RESU
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
           CALL "SD_Screen_Output" USING "SCBT51" RETURNING RESU.
           IF  W-KEY2 = SPACE
               GO TO M-10
           END-IF
           MOVE W-KEY2 TO HSH-KEY2.
      *           READ HSHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-KEY2
               MOVE "ÇcÇ`ÇsÇ`Å@ÉGÉâÅ[              " TO W-EM
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           GO TO M-20.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF
           IF  W-NO = 99
               GO TO M-50
           END-IF
           IF  W-NO < 1 OR > 11
               GO TO M-40
           END-IF
      *
           MOVE W-NO TO W-G.
           MOVE 4 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       M-45.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "D-SSD" D-SSD "p" RETURNING RESU
               GO TO M-45
           END-IF
           GO TO M-40.
       M-50.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-40.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET-RTN.
           MOVE ZERO TO W-ANSD W-ZC W-S.
       SET-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SET-EX
           END-IF
           MOVE ZERO TO CNT.
       SET-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SET-020
           END-IF
           COMPUTE W-NSU(W-S,CNT) =
                HSH-HSU(W-S,CNT) - HSH-NSU(W-S,CNT) - HSH-ISU(W-S,CNT).
           ADD W-NSU(W-S,CNT) TO W-NSUT.
           IF  W-ZC = 0
               IF  W-NSU(W-S,CNT) NOT = ZERO
                   MOVE 1 TO W-ZC
               END-IF
           END-IF
           GO TO SET-040.
       SET-EX.
           EXIT.
       KEI-RTN.
           MOVE ZERO TO W-AGSD.
           MOVE SPACE TO HSH-KEY2.
           MOVE W-HCD TO HSH-HCD.
      *           START HSHF KEY NOT < HSH-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHF_PNAME1 "HSH-KEY2" " NOT < " HSH-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO KEI-080
           END-IF.
       KEI-020.
      *           READ HSHF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KEI-080
           END-IF
           IF  HSH-HCD NOT = W-HCD
               GO TO KEI-080
           END-IF
           IF  HSH-ENGP NOT = ZERO
               GO TO KEI-020
           END-IF
      *
           MOVE ZERO TO W-S.
       KEI-040.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO KEI-020
           END-IF
           MOVE ZERO TO CNT.
       KEI-060.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               COMPUTE W-GSU(W-S,CNT) = W-GSU(W-S,CNT) +
                 HSH-HSU(W-S,CNT) - HSH-NSU(W-S,CNT) - HSH-ISU(W-S,CNT)
               GO TO KEI-060
           END-IF
           GO TO KEI-040.
       KEI-080.
           MOVE ZERO TO W-S W-TSU.
       KEI-100.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO KEI-140
           END-IF
           MOVE ZERO TO CNT.
       KEI-120.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD W-GSU(W-S,CNT) TO W-TSU
               GO TO KEI-120
           END-IF
           GO TO KEI-100.
       KEI-140.
           MOVE 4 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       KEI-160.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "D-GSD" D-GSD "p" RETURNING RESU
               GO TO KEI-160
           END-IF.
       KEI-EX.
           EXIT.
