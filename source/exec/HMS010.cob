       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMS010.
      *********************************************************
      *    PROGRAM         :  荷札・入日記　入力　　　　      *
      *    PRINTER TYPE    :  ****                            *
      *    SCREEN          :  SCHS01                          *
      *        変更　　　  :  62/05/25                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-R.
           02  W-N.
             03  W-CCD  PIC 9(7).
             03  W-UC   PIC 9.
             03  W-KS   PIC 9(3).
             03  W-MS   PIC 9(3).
             03  W-TE   PIC N(36).
             03  W-DATE PIC 9(6).
           02  W-SIN.
             03  W-IN  OCCURS 24  PIC X(62).
       01  W-D.
           02  W-HS.
             03  W-HCD  PIC 9(6).
             03  W-SU  OCCURS 27  PIC 9(2).
             03  I-GSU  PIC 9(2).
           02  W-CCDM   PIC 9(7) VALUE ZERO.
           02  CNT.
             03  CNT1   PIC 9(2).
             03  CNT2   PIC 9(2).
           02  W-L      PIC 9(2).
           02  W-LCD.
             03  W-LD   PIC 9(2).
             03  W-CD   PIC 9(2).
           02  W-UCND.
             03  W-UCN  PIC N(3).
             03  F      PIC N(3).
           02  W-CCDD   PIC 9(7) VALUE ZERO.
           02  W-UNC    PIC 9    VALUE ZERO.
           02  W-NO     PIC 9(3) VALUE ZERO.
           02  W-NGP    PIC 9(6).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN  PIC 9(2).
             03  W-GET  PIC 9(2).
             03  W-PEY  PIC 9(2).
           02  W-TED    PIC N(36).
           02  W-DMM    PIC 9.
       01  ERR-STAT     PIC X(2).
           COPY LSTAT.
      *
           COPY L-JCON.
           COPY LITCM.
           COPY LIHIM2.
      *FD  SNF-F
       01  SNF-F_HMS010.
           02  SNF-F_PNAME1   PIC  X(004) VALUE "SNFF".
           02  F              PIC  X(001).
           02  SNF-F_LNAME    PIC  X(012) VALUE "SNF-F_HMS010".
           02  F              PIC  X(001).
           02  SNF-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SNF-F_SORT     PIC  X(100) VALUE SPACE.
           02  SNF-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SNF-F_RES      USAGE  POINTER.
       01  SNF-R.
           02  N-NO     PIC 9(3).
           02  N-CCD    PIC 9(7).
           02  N-UC     PIC 9.
           02  N-KS     PIC 9(3).
           02  N-MS     PIC 9(3).
           02  N-TE     PIC N(36).
           02  N-DATE   PIC 9(06).
           02  F        PIC X(07).
       77  F            PIC X(01).
      *FD  SIN-F
       01  SIN-F_HMS010.
           02  SIN-F_PNAME1   PIC  X(004) VALUE "SINF".
           02  F              PIC  X(001).
           02  SIN-F_LNAME    PIC  X(012) VALUE "SIN-F_HMS010".
           02  F              PIC  X(001).
           02  SIN-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SIN-F_SORT     PIC  X(100) VALUE SPACE.
           02  SIN-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SIN-F_RES      USAGE  POINTER.
       01  SIN-R.
           02  I-NO     PIC 9(3).
           02  I-HCD    PIC 9(6).
           02  I-SU  OCCURS 27  PIC 9(2).
           02  I-GSU    PIC 9(2).
           02  F        PIC X(20).
       77  F            PIC X(01).
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
           02  A-DATE  PIC 9(6).
           02  FILLER.
             03  A-CCD   PIC 9(7).
             03  A-UC    PIC 9 .
             03  A-KS    PIC 9(3).
           02  A-HCD   PIC 9(6).
           02  A-TE    PIC N(36).
           02  A-MS    PIC 9(3).
           02  A-DMM   PIC 9 .
       01  C-DSP.
           02  FILLER.
             03  D-TNA   PIC N(24).
             03  D-UCN   PIC N(3).
             03  D-KS    PIC Z(3).
           02  D-HNA   PIC N(24).
           02  D-MS    PIC Z(3).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC X(19) VALUE
                  "***  TC-M 無し  ***".
             03  E-ME2   PIC X(19) VALUE
                  "***  HI-M 無し  ***".
             03  E-ME98  PIC X(05) VALUE X"1B4A05".
             03  E-ME99  PIC X(05) VALUE X"1B4205".
             03  E-CL    PIC X(50) VALUE
                  "                                                  ".
           02  E-SPACE.
                03  FILLER  PIC  X(06) VALUE "      ".
                03  FILLER  PIC  X(48) VALUE
                  "                                                ".
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
            "C-ACP" " " "0" "0" "99" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "3" "72" "6" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "5" "0" "11" "A-DATE" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CCD" "9" "5" "3" "7" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CCD" BY REFERENCE W-CCD "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UC" "9" "5" "65" "1" "A-CCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UC" BY REFERENCE W-UC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KS" "9" "5" "75" "3" "A-UC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KS" BY REFERENCE W-KS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-L" "9" "6" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TE" "N" "20" "8" "72" "A-HCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TE" BY REFERENCE W-TE "72" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MS" "9" "21" "8" "3" "A-TE" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MS" BY REFERENCE W-MS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "45" "1" "A-MS" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "108" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "5" "0" "57" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "5" "12" "48" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UCN" "N" "5" "67" "6" "D-TNA" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-UCN" BY REFERENCE W-UCN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KS" "Z" "5" "75" "3" "D-UCN" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KS" BY REFERENCE W-KS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "W-L" "16" "48" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MS" "Z" "21" "8" "3" "D-HNA" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-MS" BY REFERENCE W-MS "3" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "152" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "98" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "19" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "19" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SPACE" " " "W-L" "0" "54" "01C-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-SPACE" "X" "W-L" "9" "6" " " "E-SPACE" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-SPACE" "X" "W-L" "16" "48" "01E-SPACE" " "
            RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "DB_F_Open" USING
            "INPUT" SNF-F_PNAME1 " " BY REFERENCE SNF-F_IDLST "0".
       M-10.
      *           READ SNF-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNF-F_PNAME1 BY REFERENCE SNF-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           MOVE N-NO TO W-NO.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE SNF-F_IDLST SNF-F_PNAME1.
       M-20.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" SNF-F_PNAME1 " " BY REFERENCE SNF-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" SIN-F_PNAME1 " " BY REFERENCE SIN-F_IDLST "0".
           ACCEPT W-NGP FROM DATE.
           MOVE SPACE TO W-TED.
       M-25.
           CALL "SD_Screen_Output" USING "SCHS01" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
       M-27.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-27
           END-IF
           IF  W-NGP = ZERO
               GO TO M-30
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-27
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-27
           END-IF.
       M-30.
           PERFORM S-05 THRU S-15.
           MOVE W-NGP TO W-DATE.
       M-32.
           IF  W-CCDM NOT = ZERO
               MOVE W-CCDM TO W-CCD
               CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-CCD "A-CCD" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9 OR C2
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-27
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-32
           END-IF
           MOVE W-CCD TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-32
           END-IF
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-CCD = W-CCDD
               MOVE W-UNC TO W-UC
               CALL "SD_Output" USING "A-UC" A-UC "p" RETURNING RESU
               GO TO M-40
           END-IF
           MOVE TC-UCD TO W-UC.
           CALL "SD_Output" USING "A-UC" A-UC "p" RETURNING RESU.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-UC "A-UC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-32
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-UC = ZERO
               GO TO M-35
           END-IF.
       M-40.
           MOVE 2 TO JCON1-01.
           MOVE W-UC TO JCON1-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO JCON2-03
           END-IF
           MOVE JCON2-03 TO W-UCND.
           CALL "SD_Output" USING "D-UCN" D-UCN "p" RETURNING RESU.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-KS "A-KS" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-35
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF
           CALL "SD_Output" USING "D-KS" D-KS "p" RETURNING RESU.
           IF  W-MS = ZERO
               MOVE W-KS TO W-MS
               CALL "SD_Output" USING "D-MS" D-MS "p" RETURNING RESU
           END-IF
           IF  W-KS = ZERO
               GO TO M-45
           END-IF
           MOVE ZERO TO CNT1.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-50.
           MOVE ZERO TO W-HS.
           ADD 1 TO CNT1 W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE W-IN(CNT1) TO W-HS.
           IF  CNT1 = 13
               GO TO M-65
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C1 OR ADV
               GO TO M-65
           END-IF
           IF  ESTAT = BTB
               GO TO M-60
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           MOVE W-HS TO W-IN(CNT1).
           GO TO M-50.
       M-60.
           SUBTRACT 1 FROM CNT1.
           IF  CNT1 = ZERO
               GO TO M-45
           END-IF
           MOVE W-IN(CNT1) TO W-HS.
           SUBTRACT 1 FROM W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT1 = 12
               MOVE 19 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
           END-IF
           GO TO M-55.
       M-65.
           PERFORM S-20 THRU S-35.
       M-70.
           MOVE W-TED TO W-TE.
           CALL "SD_Output" USING "A-TE" A-TE "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TE "A-TE" "N" "72"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-60
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           MOVE W-TE TO W-TED.
           GO TO M-80.
       M-75.
           CALL "SD_Accept" USING BY REFERENCE A-MS "A-MS" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-70
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-75
           END-IF
           IF  W-MS = ZERO
               MOVE W-KS TO W-MS
           END-IF
           CALL "SD_Output" USING "D-MS" D-MS "p" RETURNING RESU.
       M-80.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-75
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           IF  W-DMM = 9
               GO TO M-32
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-80
           END-IF
      *
           ADD 1 TO W-NO.
           MOVE ZERO TO SNF-R.
           MOVE SPACE TO N-TE.
           MOVE W-NO TO N-NO.
           MOVE W-CCD TO N-CCD.
           MOVE W-UC TO N-UC.
           MOVE W-KS TO N-KS.
           MOVE W-MS TO N-MS.
           MOVE W-TE TO N-TE.
           MOVE W-DATE TO N-DATE.
      *           WRITE SNF-R.
      *//////////////
           CALL "DB_Insert" USING
            SNF-F_PNAME1 SNF-F_LNAME SNF-R RETURNING RET.
           MOVE W-CCD TO W-CCDD.
           MOVE W-UC TO W-UNC.
           MOVE ZERO TO CNT.
       M-85.
           ADD 1 TO CNT1.
           IF  CNT1 = 13
               GO TO M-30
           END-IF
           MOVE ZERO TO W-HS.
           MOVE W-IN(CNT1) TO W-HS.
           IF  W-HCD = ZERO
               GO TO M-30
           END-IF
           MOVE ZERO TO SIN-R.
           MOVE W-NO TO I-NO.
           MOVE W-HCD TO I-HCD.
      *           WRITE SIN-R.
      *//////////////
           CALL "DB_Insert" USING
            SIN-F_PNAME1 SIN-F_LNAME SIN-R RETURNING RET.
           GO TO M-85.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SNF-F_IDLST SNF-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SIN-F_IDLST SIN-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "SD_Screen_Output" USING "SCHS01" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           MOVE ZERO TO W-N CNT1.
           MOVE ALL "　" TO W-TE.
       S-10.
           ADD 1 TO CNT1.
           IF  CNT1 NOT = 13
               MOVE ZERO TO W-IN(CNT1)
               GO TO S-10
           END-IF.
       S-15.
           EXIT.
       S-20.
           MOVE CNT1 TO CNT2.
           MOVE W-L TO W-LD.
       S-25.
           IF  CNT2 = 13
               GO TO S-30
           END-IF
           MOVE ZERO TO W-IN(CNT2).
           CALL "SD_Output" USING "E-SPACE" E-SPACE "p" RETURNING RESU.
           ADD 1 TO W-L CNT2.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO S-25.
       S-30.
           MOVE W-LD TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-35.
           EXIT.
