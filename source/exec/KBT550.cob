       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBT550.
      *********************************************************
      *    PROGRAM         :  製品仕入ロット別入庫明細問合せ  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBT55                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-END          PIC  9(001).
           02  W-RNO          PIC  9(008).
           02  W-RNOD  REDEFINES W-RNO.
             03  W-RSN        PIC  9(002).
             03  W-RNG        PIC  9(004).
             03  W-RND        PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-C            PIC S9(002).
           02  W-AZCD.
             03  W-ZCD   OCCURS   4.
               04  W-ZC       PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-DE           PIC  9(001).
           02  W-S            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-SUT          PIC  9(006).
           02  W-KIN          PIC  9(008).
           02  W-GP           PIC  9(004).
           02  W-KBN          PIC  N(002).
           02  W-ASUD.
             03  W-ASU   OCCURS   4.
               04  W-SUD   OCCURS  10.
                 05  W-SU     PIC S9(004).
           02  W-GSU          PIC S9(006).
           02  W-KBNO         PIC  X(006).
           02  W-DC           PIC  9(001).
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-RSN   PIC  9(002).
             03  A-RNG   PIC  9(004).
             03  A-RND   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID.
             03  FILLER.
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  N(024).
             03  FILLER.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
             03  FILLER.
               04  FILLER  PIC ZZ,ZZ9 .
               04  FILLER  PIC ZZ,ZZ9 .
               04  FILLER  PIC ZZ,ZZZ,ZZ9 .
               04  FILLER  PIC 99/99/99 .
           02  D-MEI.
             03  D-GP    PIC ZZ/ZZ .
             03  D-KBN   PIC  N(002).
             03  D-SIZ   PIC  9(001).
             03  D-SU    PIC  -(005).
             03  D-GSU   PIC ---,--9 .
             03  D-KBNO  PIC  9(006).
           02  FILLER.
             03  D-KRM   PIC  N(003) VALUE "完了済".
             03  D-KRMC  PIC  X(006) VALUE   "      ".
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
            "01C-ACP" " " "2" "0" "8" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RSN" "9" "2" "12" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RSN" BY REFERENCE W-RSN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RNG" "9" "2" "15" "4" "A-RSN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RNG" BY REFERENCE W-RNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-RND" "9" "2" "20" "2" "A-RNG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-RND" BY REFERENCE W-RND "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "68" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "176" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID" " " "0" "0" "136" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID" " " "3" "0" "52" " " "D-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MID" "9" "3" "12" "4" " " "01D-MID" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MID" BY REFERENCE HSH-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201D-MID" "N" "3" "17" "48" "0101D-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MID" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID" " " "4" "0" "54" "01D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MID" "9" "4" "12" "6" " " "02D-MID" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MID" BY REFERENCE HSH-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "0202D-MID" "N" "4" "19" "48" "0102D-MID" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-MID" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID" " " "5" "0" "30" "02D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-MID" "ZZ,ZZ9" "5" "12" "6" " " "03D-MID"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103D-MID" BY REFERENCE W-SUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-MID" "ZZ,ZZ9" "5" "24" "6" "0103D-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-MID" BY REFERENCE HSH-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-MID" "ZZ,ZZZ,ZZ9" "5" "36" "10" "0203D-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-MID" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-MID" "99/99/99" "5" "52" "8" "0303D-MID" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0403D-MID" BY REFERENCE HSH-NNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-L" "0" "28" "D-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GP" "ZZ/ZZ" "W-L" "3" "5" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "D-GP" BY REFERENCE W-GP "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KBN" "N" "W-L" "9" "4" "D-GP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KBN" BY REFERENCE W-KBN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ" "9" "W-L" "14" "1" "D-KBN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ" BY REFERENCE W-S "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "-----" "W-L" "W-C" "5" "D-SIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU(1,1) "4" "2" BY REFERENCE W-S 40
            BY REFERENCE CNT 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSU" "---,--9" "W-L" "65" "7" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GSU" BY REFERENCE W-GSU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KBNO" "9" "W-L" "73" "6" "D-GSU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KBNO" BY REFERENCE W-KBNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "23" "0" "12" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KRM" "N" "23" "3" "6" " " "03C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KRMC" "X" "23" "3" "6" "D-KRM" " " RETURNING RESU.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT55" RETURNING RESU.
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
           CALL "DB_F_Open" USING
            "INPUT" HSHNF_PNAME1 "SHARED" BY REFERENCE HSHNF_IDLST "1"
            "HSHN-KEY" BY REFERENCE HSHN-KEY.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-RSN "A-RSN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-RSN = ZERO
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-RNG "A-RNG" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-RNG = ZERO
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-RND "A-RND" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-RND = ZERO
               GO TO M-20
           END-IF
      *
           MOVE W-RNO TO HSH-KEY.
      *           READ HSHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ﾊｯﾁｭｳNO ﾅｼ  ***          " TO W-EM
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
      *
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
           PERFORM SUM-RTN THRU SUM-EX.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           IF  HSH-ENGP = ZERO
               CALL "SD_Output" USING "D-KRMC" D-KRMC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-KRM" D-KRM "p" RETURNING RESU
           END-IF
           MOVE 9 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 0 TO W-END W-DE.
      *
           MOVE HSH-HGP TO W-GP.
           MOVE "発注" TO W-KBN.
           MOVE ZERO TO W-ASUD.
           MOVE HSH-AHSUD TO W-ASUD.
           PERFORM SET-RTN THRU SET-EX.
           MOVE 1 TO W-DC.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-END = 9
               GO TO M-90
           END-IF
           IF  W-END = 5
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
               GO TO M-25
           END-IF.
       M-18.
      *           READ HSHNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSHNF_PNAME1 BY REFERENCE HSHN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  W-RNO NOT = HSHN-RNO
               GO TO M-25
           END-IF
      *
           MOVE HSHN-GP TO W-GP.
           IF  HSHN-HPC = 0
               MOVE "入庫" TO W-KBN
           ELSE
               MOVE "返品" TO W-KBN
           END-IF
           MOVE HSHN-KBNO TO W-KBNO.
           MOVE ZERO TO W-ASUD.
           MOVE HSHN-ASUD TO W-ASUD.
           PERFORM SET-RTN THRU SET-EX.
           MOVE 0 TO W-DC.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-END = 9
               GO TO M-90
           END-IF
           IF  W-END = 5
               GO TO M-45
           END-IF
           GO TO M-18.
       M-25.
           CALL "DB_F_Open" USING
            "INPUT" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       M-30.
      *           READ HSS-F NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  HSS-HKC NOT = 0
               GO TO M-30
           END-IF
           IF  W-RNO NOT = HSS-RNO
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO W-GP.
           IF  HSS-HPC = 0
               MOVE "入庫" TO W-KBN
           ELSE
               MOVE "返品" TO W-KBN
           END-IF
           MOVE HSS-DNO TO W-KBNO.
           MOVE ZERO TO W-ASUD.
           MOVE HSS-ASUD TO W-ASUD.
           PERFORM SET-RTN THRU SET-EX.
           MOVE 0 TO W-DC.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-END = 9
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               GO TO M-90
           END-IF
           IF  W-END = 5
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               GO TO M-45
           END-IF
           GO TO M-30.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
      *
           MOVE "残高" TO W-KBN.
           PERFORM ZAN-RTN THRU ZAN-EX.
           PERFORM SET-RTN THRU SET-EX.
           MOVE 2 TO W-DC.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-END = 9
               GO TO M-90
           END-IF
           IF  W-END = 5
               GO TO M-45
           END-IF
           MOVE "ＥＮＤ　ＤＡＴＡ              " TO W-EM.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
       M-40.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-40
           END-IF.
       M-45.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT55" RETURNING RESU.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSHNF_IDLST HSHNF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SUM-RTN.
           MOVE ZERO TO W-S W-SUT.
       SUM-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO SUM-060
           END-IF
           MOVE ZERO TO CNT.
       SUM-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SUM-020
           END-IF
           ADD HSH-HSU(W-S,CNT) TO W-SUT.
           GO TO SUM-040.
       SUM-060.
           COMPUTE W-KIN = W-SUT * HSH-T.
       SUM-EX.
           EXIT.
       SET-RTN.
           MOVE ZERO TO W-S W-AZCD W-GSU.
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
           IF  W-SU(W-S,CNT) NOT = ZERO
               ADD W-SU(W-S,CNT) TO W-GSU
               IF  W-ZC(W-S) = 0
                   MOVE 1 TO W-ZC(W-S)
               END-IF
           END-IF
           GO TO SET-040.
       SET-EX.
           EXIT.
       DSP-RTN.
           MOVE 0 TO W-S W-SC W-EC.
       DSP-020.
           ADD 1 TO W-S.
           IF  W-S = 5
               GO TO DSP-EX
           END-IF
           IF  W-ZC(W-S) = 0
               GO TO DSP-020
           END-IF
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 23
               GO TO DSP-080
           END-IF.
       DSP-040.
           IF  W-S = 1
               MOVE 1 TO W-SC
               IF  0 = W-ZC(2) AND W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(1)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 2
               IF  0 = W-ZC(3) AND W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(1) AND W-ZC(2)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
           IF  W-S = 3
               IF  0 = W-ZC(4)
                   MOVE 1 TO W-EC
               END-IF
           END-IF
           IF  W-S = 4
               MOVE 1 TO W-EC
               IF  0 = W-ZC(1) AND W-ZC(2) AND W-ZC(3)
                   MOVE 1 TO W-SC
               END-IF
           END-IF
      *
           IF  W-SC = 1
               CALL "SD_Output" USING "D-KBN" D-KBN "p" RETURNING RESU
               IF  W-DC = 0
                   CALL "SD_Output" USING "D-GP" D-GP "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-KBNO" D-KBNO "p" RETURNING RESU
               ELSE
                   IF  W-DC = 1
                       CALL "SD_Output" USING
                        "D-GP" D-GP "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           IF  W-EC = 1
               CALL "SD_Output" USING "D-GSU" D-GSU "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-SIZ" D-SIZ "p" RETURNING RESU.
      *
           MOVE 10 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       DSP-060.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD 5 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO DSP-060
           END-IF
           GO TO DSP-020.
       DSP-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 9 TO W-END
               GO TO DSP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO DSP-080
           END-IF
           IF  W-DMM = 9
               MOVE 5 TO W-END
               GO TO DSP-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO DSP-080
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCBT55" RETURNING RESU.
           CALL "SD_Output" USING "D-MID" D-MID "p" RETURNING RESU.
           IF  HSH-ENGP = ZERO
               CALL "SD_Output" USING "D-KRMC" D-KRMC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-KRM" D-KRM "p" RETURNING RESU
           END-IF
           MOVE 10 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE 1 TO W-SC.
           GO TO DSP-040.
       DSP-EX.
           EXIT.
       ZAN-RTN.
           MOVE ZERO TO W-ASUD CNT.
       ZAN-020.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO ZAN-EX
           END-IF
           COMPUTE W-SU(1,CNT) = HSH-HSU(1,CNT) -
                                 HSH-NSU(1,CNT) - HSH-ISU(1,CNT).
           COMPUTE W-SU(2,CNT) = HSH-HSU(2,CNT) -
                                 HSH-NSU(2,CNT) - HSH-ISU(2,CNT).
           COMPUTE W-SU(3,CNT) = HSH-HSU(3,CNT) -
                                 HSH-NSU(3,CNT) - HSH-ISU(3,CNT).
           COMPUTE W-SU(4,CNT) = HSH-HSU(4,CNT) -
                                 HSH-NSU(4,CNT) - HSH-ISU(4,CNT).
           GO TO ZAN-020.
       ZAN-EX.
           EXIT.
