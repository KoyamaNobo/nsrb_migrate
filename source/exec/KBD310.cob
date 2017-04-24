       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD310.
      *********************************************************
      *    PROGRAM         :  履物製品仕入発注入力            *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBD31                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-RNO.
             03  W-RSN        PIC  9(002).
             03  W-RNG        PIC  9(004).
             03  W-RNGD  REDEFINES W-RNG.
               04  W-RNEN     PIC  9(002).
               04  W-RGET     PIC  9(002).
             03  W-RND        PIC  9(002).
           02  W-SU           PIC  9(004).
           02  W-SUT          PIC  9(005).
           02  W-KIN          PIC  9(008).
           02  W-L            PIC  9(002).
           02  W-C            PIC S9(002).
           02  CNT            PIC  9(002).
           02  W-S            PIC  9(001).
           02  W-SD           PIC  9(001).
           02  W-DATE         PIC  9(008).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NEN        PIC  9(004).
             03  W-NEND  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPD  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-HNGP.
             03  W-HNEN       PIC  9(004).
             03  W-HGET       PIC  9(002).
             03  W-HPEY       PIC  9(002).
           02  W-ENGP         PIC  9(006).
           02  W-HNGPS        PIC  9(006).
           02  W-SCD          PIC  9(004).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIHIM.
           COPY LIHSHF.
           COPY LIHSHN.
           COPY LIHSSF.
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
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-RSN   PIC  9(002).
             03  A-RNG   PIC  9(004).
             03  A-RND   PIC  9(002).
           02  A-HDD   PIC  9(006).
           02  A-SCD   PIC  9(004).
           02  A-HCD   PIC  9(006).
           02  A-HSU   PIC  9(004).
           02  A-T     PIC  9(005).
           02  A-NDD   PIC  9(006).
           02  A-EC    PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNA   PIC  N(024).
           02  D-HNA   PIC  N(024).
           02  D-HSU   PIC  Z(004).
           02  D-SUT   PIC ZZ,ZZ9 .
           02  D-T     PIC  Z(005).
           02  D-KIN   PIC ZZ,ZZZ,ZZZ .
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
           COPY LSSEM.
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
            "C-ACP" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "2" "59" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "3" "0" "8" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RSN" "9" "3" "11" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RSN" BY REFERENCE W-RSN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RNG" "9" "3" "14" "4" "A-RSN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RNG" BY REFERENCE W-RNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RND" "9" "3" "19" "2" "A-RNG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RND" BY REFERENCE W-RND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HDD" "9" "4" "11" "6" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HDD" BY REFERENCE HSH-HNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "5" "11" "4" "A-HDD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE HSH-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "6" "11" "6" "A-SCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE HSH-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HSU" "9" "W-L" "W-C" "4" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HSU" BY REFERENCE HSH-HSU(1,1) "4" "2" 
            BY REFERENCE W-S 40 BY REFERENCE CNT 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T" "9" "18" "59" "5" "A-HSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-T" BY REFERENCE HSH-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NDD" "9" "21" "2" "6" "A-T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NDD" BY REFERENCE HSH-NNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EC" "9" "21" "58" "1" "A-NDD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EC" BY REFERENCE W-EC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "62" "1" "A-EC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "121" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "5" "16" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "6" "18" "48" "D-SNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HSU" "Z" "W-L" "W-C" "4" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HSU" BY REFERENCE HSH-HSU(1,1) "4" "2" 
            BY REFERENCE W-S 40 BY REFERENCE CNT 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SUT" "ZZ,ZZ9" "18" "52" "6" "D-HSU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SUT" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "Z" "18" "59" "5" "D-SUT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE HSH-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "D-KIN" "ZZ,ZZZ,ZZZ" "18" "65" "10" "D-T" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
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
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Screen_Output" USING "SCBD31" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-DATA.
           ACCEPT W-ENGP FROM DATE.
           MOVE W-ENGP TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-DATE TO W-HNGP.
           SUBTRACT 1 FROM W-HGET.
           IF  W-HGET = ZERO
               SUBTRACT 1 FROM W-HNEN
               MOVE 12 TO W-HGET
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HSHNF_PNAME1 "SHARED" BY REFERENCE HSHNF_IDLST "1"
            "HSHN-KEY" BY REFERENCE HSHN-KEY.
           CALL "DB_F_Open" USING
            "I-O" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
            "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
             HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT = 9
               GO TO M-90
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-RSN "A-RSN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-RSN = ZERO
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-RNG "A-RNG" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-RNG = ZERO
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-RND "A-RND" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-RND = ZERO
               GO TO M-25
           END-IF
      *
           INITIALIZE HSH-R.
           MOVE W-RNO TO HSH-KEY.
      *           READ HSHF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  W-ACT = 1
               MOVE "***  DATA ｱﾘ  ***             " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-ACT NOT = 3
               GO TO M-45
           END-IF
      *
           MOVE SPACE TO HSHN-KEY.
           MOVE W-RNO TO HSHN-RNO.
      *           START HSHNF KEY NOT < HSHN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHNF_PNAME1 "HSHN-KEY" " NOT < " HSHN-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
      *           READ HSHNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHNF_PNAME1 BY REFERENCE HSHN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  W-RNO NOT = HSHN-RNO
               GO TO M-30
           END-IF
           MOVE "***  ｼﾞｯｾｷ ﾃﾞｰﾀ ｱﾘ  ***       " TO W-EM.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO M-25.
       M-30.
           CALL "DB_F_Open" USING
            "INPUT" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       M-35.
      *           READ HSS-F NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               GO TO M-45
           END-IF
           IF  W-RNO = HSS-RNO
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               MOVE "***  ﾆｭｳﾘｮｸ ﾃﾞｰﾀ ｱﾘ  ***      " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           GO TO M-35.
       M-40.
           IF  W-ACT NOT = 1
               MOVE "***  DATA ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-45.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  W-ACT = 3
               IF  W-DMM = 9
                   GO TO M-25
               END-IF
           END-IF
      *
           IF  W-ACT NOT = 1
               GO TO M-50
           END-IF
      *           WRITE HSH-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSHF_PNAME1 HSHF_LNAME HSH-R RETURNING RET.
           IF  RET = 1
               MOVE "***  WRITE ｴﾗｰ  ***           " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-60.
       M-50.
           IF  W-ACT NOT = 2
               GO TO M-55
           END-IF
      *           REWRITE HSH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSHF_PNAME1 HSHF_LNAME HSH-R RETURNING RET.
           IF  RET = 1
               MOVE "***  REWRITE ｴﾗｰ  ***         " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-60.
       M-55.
      *           DELETE HSHF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSHF_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "***  DELETE ｴﾗｰ  ***          " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-60.
           CALL "SD_Screen_Output" USING "SCBD31" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           IF  W-ACT = 1
               MOVE HSH-HNGPS TO W-HNGPS
               MOVE HSH-SCD TO W-SCD
               CALL "SD_Output" USING "A-RSN" A-RSN "p" RETURNING RESU
               CALL "SD_Output" USING "A-RNG" A-RNG "p" RETURNING RESU
               CALL "SD_Output" USING "A-RND" A-RND "p" RETURNING RESU
               CALL "SD_Output" USING "A-HDD" A-HDD "p" RETURNING RESU
               CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU
               CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHNF_IDLST HSHNF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DSP-RTN.
           MOVE HSH-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "＊＊＊　仕入先　なし　　＊＊＊" TO S-NAME
           END-IF
      *
           MOVE HSH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊＊　品名　なし　　＊＊＊　" TO HI-NAME
           END-IF
      *
           IF  HSH-ENGP = ZERO
               MOVE 0 TO W-EC
           ELSE
               MOVE 1 TO W-EC
           END-IF
           CALL "SD_Output" USING "A-HDD" A-HDD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NDD" A-NDD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-EC" A-EC "p" RETURNING RESU.
           MOVE ZERO TO W-SUT W-KIN.
           MOVE 0 TO W-SD.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       DSP-010.
           ADD 1 TO W-SD.
           IF  W-SD > 4
               GO TO DSP-030
           END-IF
           ADD 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-SD = 1
               MOVE 2 TO W-S
           END-IF
           IF  W-SD = 2
               MOVE 3 TO W-S
           END-IF
           IF  W-SD = 3
               MOVE 4 TO W-S
           END-IF
           IF  W-SD = 4
               MOVE 1 TO W-S
           END-IF
           MOVE ZERO TO CNT.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       DSP-020.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO DSP-010
           END-IF
           ADD 5 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           CALL "SD_Output" USING "D-HSU" D-HSU "p" RETURNING RESU.
           ADD HSH-HSU(W-S,CNT) TO W-SUT.
           GO TO DSP-020.
       DSP-030.
           COMPUTE W-KIN = W-SUT * HSH-T.
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       DSP-EX.
           EXIT.
       ACP-RTN.
           IF  W-ACT = 3
               GO TO ACP-340
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-HDD "A-HDD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  HSH-HNGPS = ZERO
               MOVE W-ENGP TO HSH-HNGPS
               CALL "SD_Output" USING "A-HDD" A-HDD "p" RETURNING RESU
           END-IF
           MOVE ZERO TO HSH-HNEN1.
           IF  HSH-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO HSH-HNEN
           END-IF
           IF  HSH-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO HSH-HNEN
           END-IF
           IF  HSH-HDD > W-DATE OR < W-HNGP
               IF  W-ACT = 1
                   MOVE "***  ﾋﾂﾞｹ ｴﾗｰ  ***            " TO W-EM
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO ACP-020
               ELSE
                   MOVE "***  ﾋﾂﾞｹ ﾁｪｯｸ  ***           " TO W-EM
                   CALL "SD_Output" USING
                    "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
               END-IF
           END-IF.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           MOVE HSH-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ｼｲﾚｻｷ ﾅｼ  ***            " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-040
           END-IF
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  HSH-SCD < 5000
               GO TO ACP-040
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           MOVE HSH-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ﾋﾝﾒｲ ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       ACP-070.
           MOVE 0 TO W-SD.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 0 TO HI-S(4,10).
       ACP-080.
           ADD 1 TO W-SD.
           IF  W-SD > 4
               GO TO ACP-180
           END-IF
           ADD 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-SD = 1
               MOVE 2 TO W-S
           END-IF
           IF  W-SD = 2
               MOVE 3 TO W-S
           END-IF
           IF  W-SD = 3
               MOVE 4 TO W-S
           END-IF
           IF  W-SD = 4
               MOVE 1 TO W-S
           END-IF
           MOVE ZERO TO CNT.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       ACP-100.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO ACP-080
           END-IF
           ADD 5 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  HI-S(W-S,CNT) = ZERO
               MOVE ZERO TO HSH-HSU(W-S,CNT)
               CALL "SD_Output" USING "D-HSU" D-HSU "p" RETURNING RESU
               GO TO ACP-100
           END-IF.
       ACP-120.
           MOVE HSH-HSU(W-S,CNT) TO W-SU.
           CALL "SD_Accept" USING BY REFERENCE A-HSU "A-HSU" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = ADV
               MOVE W-SU TO HSH-HSU(W-S,CNT)
               CALL "SD_Output" USING "D-HSU" D-HSU "p" RETURNING RESU
               GO TO ACP-180
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF
           CALL "SD_Output" USING "D-HSU" D-HSU "p" RETURNING RESU.
           GO TO ACP-100.
       ACP-140.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               GO TO ACP-160
           END-IF
           SUBTRACT 5 FROM W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  HI-S(W-S,CNT) = ZERO
               GO TO ACP-140
           END-IF
           GO TO ACP-120.
       ACP-160.
           SUBTRACT 1 FROM W-SD.
           IF  W-SD = 0
               GO TO ACP-060
           END-IF
           SUBTRACT 3 FROM W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-SD = 1
               MOVE 2 TO W-S
           END-IF
           IF  W-SD = 2
               MOVE 3 TO W-S
           END-IF
           IF  W-SD = 3
               MOVE 4 TO W-S
           END-IF
           IF  W-SD = 4
               MOVE 1 TO W-S
           END-IF
           MOVE 11 TO CNT.
           MOVE 52 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO ACP-140.
       ACP-180.
           MOVE ZERO TO W-SD W-SUT.
       ACP-200.
           ADD 1 TO W-SD.
           IF  W-SD > 4
               GO TO ACP-240
           END-IF
           MOVE ZERO TO CNT.
       ACP-220.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO ACP-200
           END-IF
           ADD HSH-HSU(W-SD,CNT) TO W-SUT.
           GO TO ACP-220.
       ACP-240.
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
       ACP-260.
           CALL "SD_Accept" USING BY REFERENCE A-T "A-T" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 5 TO W-SD
               MOVE 21 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO ACP-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-260
           END-IF
           COMPUTE W-KIN = W-SUT * HSH-T.
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       ACP-280.
           CALL "SD_Accept" USING BY REFERENCE A-NDD "A-NDD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-280
           END-IF
           MOVE ZERO TO HSH-NNEN1.
           IF  HSH-NNGPS = ZERO
               GO TO ACP-300
           END-IF
           IF  HSH-NNGPS = 999999
               MOVE 99 TO HSH-NNEN1
               GO TO ACP-300
           END-IF
           IF  HSH-NNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO HSH-NNEN
           END-IF
           IF  HSH-NNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO HSH-NNEN
           END-IF
           IF  HSH-HDD > HSH-NDD
               MOVE "***  ﾋﾂﾞｹ ｴﾗｰ  ***            " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-280
           END-IF.
       ACP-300.
           IF W-ACT = 1
               MOVE 0 TO W-EC
               CALL "SD_Output" USING "A-EC" A-EC "p" RETURNING RESU
               GO TO ACP-340
           END-IF.
       ACP-320.
           CALL "SD_Accept" USING BY REFERENCE A-EC "A-EC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-320
           END-IF
           IF  W-EC > 1
               GO TO ACP-320
           END-IF
           IF (HSH-ENGP NOT = ZERO) OR (W-EC = 0)
               GO TO ACP-340
           END-IF
      *
           MOVE SPACE TO HSHN-KEY.
           MOVE W-RNO TO HSHN-RNO.
      *           START HSHNF KEY NOT < HSHN-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSHNF_PNAME1 "HSHN-KEY" " NOT < " HSHN-KEY RETURNING RET.
           IF  RET = 1
               MOVE "***  ﾆｭｳｺ ﾃﾞｰﾀ ﾅｼ  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-320
           END-IF
      *           READ HSHNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHNF_PNAME1 BY REFERENCE HSHN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               MOVE "***  ﾆｭｳｺ ﾃﾞｰﾀ ﾅｼ  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-320
           END-IF
           IF  W-RNO NOT = HSHN-RNO
               MOVE "***  ﾆｭｳｺ ﾃﾞｰﾀ ﾅｼ  ***        " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-320
           END-IF.
       ACP-340.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-EX
               ELSE
                   IF  W-ACT = 1
                       GO TO ACP-280
                   ELSE
                       GO TO ACP-320
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-340
           END-IF
           IF  W-DMM = 9
               IF  W-ACT NOT = 3
                   GO TO ACP-070
               ELSE
                   GO TO ACP-EX
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-340
           END-IF
      *
           IF  HSH-ENGP = ZERO
               IF  W-EC = 1
                   MOVE W-ENGP TO HSH-ENGP
               END-IF
           END-IF
           IF  HSH-ENGP NOT = ZERO
               IF  W-EC = 0
                   MOVE ZERO TO HSH-ENGP
               END-IF
           END-IF.
       ACP-EX.
           EXIT.
