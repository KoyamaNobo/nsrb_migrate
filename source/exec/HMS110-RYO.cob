       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMS110.
      *********************************************************
      *    PROGRAM         :  ‰×ŽDE“ü“ú‹L@ì¬@@          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        •ÏX@@@  :  62/06/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  15K              PIC X(05) VALUE X"1A24212078".
       77  20K              PIC X(05) VALUE X"1A24212474".
       77  WWK              PIC X(08) VALUE X"1A26222166222176".
       77  SWK              PIC X(08) VALUE X"1A26222166212078".
       77  SSK              PIC X(08) VALUE X"1A26212068212078".
       01  W-P1.
           02  F            PIC X(09).
           02  P-UB         PIC X(8) VALUE "999-9999".
           02  F            PIC X(56).
           02  P-DATE       PIC 99BBB99BBB99.
       01  W-P2.
           02  P-151        PIC X(05).
           02  F            PIC X(07).
           02  P-JU         PIC N(20).
           02  P-205        PIC X(05).
           02  F            PIC X(17).
           02  P-UM         PIC N(01).
           02  P-UNO        PIC X(08).
       01  W-P3.
           02  F            PIC X(13).
           02  P-155        PIC X(05).
           02  P-JS         PIC N(20).
           02  F            PIC X(11).
           02  P-NJS        PIC N(14).
       01  W-P4.
           02  F            PIC X(09).
           02  P-CNA        PIC N(26).
           02  P-201        PIC X(05).
           02  F            PIC X(06).
           02  P-SWK        PIC X(8).
           02  P-NA         PIC N(10).
           02  F            PIC X(02).
           02  P-UNA        PIC N(06).
           02  P-SSK1       PIC X(8).
           02  F            PIC X(3).
           02  P-WWK        PIC X(8).
           02  P-KS         PIC N(3) VALUE "‚X‚X‚X".
           02  P-KSD  REDEFINES P-KS.
             03  P-KS1      PIC N.
             03  P-KS2      PIC N.
             03  P-KS3      PIC N.
           02  P-SSK2       PIC X(8).
       01  W-P5.
           02  F            PIC X(54).
           02  P-TM         PIC X(03).
           02  F            PIC X(01).
           02  P-TEL        PIC X(14).
       01  W-P6.
           02  P-152        PIC X(05).
           02  F            PIC X(02).
           02  P-HNA1       PIC N(14).
           02  P-202        PIC X(05).
       01  W-P7.
           02  P-153        PIC X(05).
           02  F            PIC X(08).
           02  P-HNA2       PIC N(10).
           02  F            PIC X.
           02  P-ASU.
             03  P-SU  OCCURS 25  PIC Z(3).
           02  P-GSU        PIC Z(4).
           02  P-203        PIC X(05).
       01  W-P8.
           02  P-154        PIC X(05).
           02  F            PIC X(25).
           02  P-TE         PIC N(36).
           02  P-204        PIC X(05).
       01  W-D.
           02  W-D1.
             03  W-NO       PIC 9(3).
             03  W-CCD      PIC 9(7).
             03  W-UC       PIC 9.
             03  W-KS       PIC 9(3).
           02  W-MS         PIC 9(3).
           02  W-TE         PIC N(36).
           02  W-SIN.
             03  W-SI  OCCURS 24  PIC X(62).
       01  W-HS.
           02  W-HCD        PIC 9(6).
           02  W-ASU.
             03  W-AS  OCCURS 27  PIC 9(2).
           02  W-GSU        PIC 9(2).
       01  CNT.
           02  CNT1         PIC 9(2).
           02  CNT2         PIC 9(2).
           02  CNT3         PIC 9(2).
       01  W-C              PIC 9(3).
       01  CHK              PIC 9(2).
       01  W-LC             PIC 9(2).
       01  W-DATE           PIC 9(6).
       01  W-NAD.
           02  W-NAD1.
             03  W-ND1  OCCURS 14  PIC N.
           02  W-NA1  REDEFINES W-NAD1  PIC N(14).
           02  W-NAD2.
             03  W-ND2  OCCURS 10  PIC N.
           02  W-NA2  REDEFINES W-NAD2  PIC N(10).
       01  W-NC             PIC 9(2).
       01  W-TEST.
           02  W-TEST1      PIC X(54) VALUE ALL "9".
           02  W-TEST2      PIC N(36) VALUE ALL "‚m".
       01  W-TC             PIC 9.
       01  W-DMM            PIC 9.
       01  ERR-STAT         PIC X(2).
           COPY LSTAT.
      *
           COPY LITCM.
           COPY L-JCON.
           COPY LIHIM2.
      *FD  SNF-F
       01  SNF-F_HMS110.
           02  SNF-F_PNAME1   PIC  X(008) VALUE "SNFF-RYO".
           02  F              PIC  X(001).
           02  SNF-F_LNAME    PIC  X(012) VALUE "SNF-F_HMS110".
           02  F              PIC  X(001).
           02  SNF-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SNF-F_SORT     PIC  X(100) VALUE SPACE.
           02  SNF-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SNF-F_RES      USAGE  POINTER.
       01  SNF-R.
           02  N-NO           PIC 9(3).
           02  N-CCD          PIC 9(7).
           02  N-UC           PIC 9.
           02  N-KS           PIC 9(3).
           02  N-MS           PIC 9(3).
           02  N-TE           PIC N(36).
           02  N-DATE         PIC 9(6).
           02  F              PIC X(07).
       77  F                  PIC X(01).
      *FD  SIN-F
       01  SIN-F_HMS110.
           02  SIN-F_PNAME1   PIC  X(008) VALUE "SINF-RYO".
           02  F              PIC  X(001).
           02  SIN-F_LNAME    PIC  X(012) VALUE "SIN-F_HMS110".
           02  F              PIC  X(001).
           02  SIN-F_KEY1     PIC  X(100) VALUE SPACE.
           02  SIN-F_SORT     PIC  X(100) VALUE SPACE.
           02  SIN-F_IDLST    PIC  X(100) VALUE SPACE.
           02  SIN-F_RES      USAGE  POINTER.
       01  SIN-R.
           02  I-NO           PIC 9(3).
           02  I-HCD          PIC 9(6).
           02  I-ASU.
             03  I-SU  OCCURS 27  PIC 9(2).
           02  I-GSU          PIC 9(2).
           02  F              PIC X(20).
       77  F                  PIC X(01).
      *FD  SP-F
       77  SP-R               PIC X(170).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(17) VALUE
                "–––––––––––––––––".
           02  FILLER  PIC N(17) VALUE
                "–––––––––––––––––".
           02  FILLER  PIC N(17) VALUE
                "–––@@@@@@@@@@@–––".
           02  FILLER  PIC N(17) VALUE
                "–––@‰×ŽDE“ü“ú‹L@ì¬@–––".
           02  FILLER  PIC N(17) VALUE
                "–––@@@@@@@@@@@–––".
           02  FILLER  PIC N(17) VALUE
                "–––––––––––––––––".
           02  FILLER  PIC N(17) VALUE
                "–––––––––––––––––".
           02  FILLER  PIC X(34) VALUE
                "ƒeƒXƒgˆóü  ‚µ‚È‚¢=1 ‚·‚é=9   ØÀ°Ý".
           02  FILLER  PIC X(22) VALUE
                "Šm”F  OK=1 NO=9   ØÀ°Ý".
       01  C-ACP.
           02  A-TC    PIC 9 .
           02  A-DMM   PIC 9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC X(21) VALUE
                  "***  PROGRAM ´×°  ***".
             03  E-ME98  PIC X(05) VALUE X"1B4A05".
             03  E-ME99  PIC X(05) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "294" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "34" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "34" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "34" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "34" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "34" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "34" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "34" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "15" "34" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "21" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TC" "9" "12" "44" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TC" BY REFERENCE W-TC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "38" "1" "A-TC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "31" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "PR_Open" RETURNING RESP.
           MOVE   15K   TO P-151 P-152 P-153 P-154 P-155.
           MOVE   20K   TO P-201 P-202 P-203 P-204 P-205.
           MOVE WWK TO P-WWK.
           MOVE SWK TO P-SWK.
           MOVE SSK TO P-SSK1 P-SSK2.
           MOVE W-TEST1 TO W-ASU W-DATE.
           MOVE W-DATE  TO P-DATE.
           MOVE W-TEST2 TO P-JU P-JS P-CNA P-HNA1 P-HNA2 P-UNA P-TE.
           MOVE 99 TO P-GSU.
           MOVE "§" TO P-UM.
           MOVE "700-0975" TO P-UNO.
           MOVE "“úiƒSƒ€Š”Ž®‰ïŽÐ" TO P-NA.
           MOVE "‰ªŽRŽs¡‚W’š–Ú‚P‚U|‚P‚V" TO P-NJS.
           MOVE "TEL" TO P-TM.
           MOVE "086-243-2456" TO P-TEL.
           MOVE ZERO TO CHK.
       M-10.
           ADD 1 TO CHK.
           IF  CHK = 26
               GO TO M-15
           END-IF
           MOVE W-AS(CHK) TO P-SU(CHK).
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-TC "A-TC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-TC = 1
               GO TO M-30
           END-IF
           IF  W-TC NOT = 9
               GO TO M-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P4  TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5  TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO  TO CHK.
       M-20.
           ADD 1 TO CHK.
           IF  CHK = 7
               GO TO M-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P6  TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P7 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-20.
       M-25.
           MOVE SPACE TO SP-R.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
           GO TO M-15.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
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
            "INPUT" SNF-F_PNAME1 " " BY REFERENCE SNF-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SIN-F_PNAME1 " " BY REFERENCE SIN-F_IDLST "0".
      *           READ SIN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SIN-F_PNAME1 BY REFERENCE SIN-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF.
       M-35.
      *           READ SNF-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNF-F_PNAME1 BY REFERENCE SNF-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-15.
           MOVE N-NO TO W-NO.
           MOVE N-CCD TO W-CCD.
           MOVE N-CCD TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE N-UC TO W-UC.
           MOVE N-KS TO W-KS.
           MOVE N-MS TO W-MS.
           MOVE N-TE TO W-TE.
           MOVE N-DATE TO W-DATE.
           MOVE ZERO TO CNT.
       M-40.
           IF  W-NO NOT = I-NO
               GO TO M-45
           END-IF
           ADD 1 TO CNT1.
           MOVE ZERO TO W-HS.
           MOVE I-HCD TO W-HCD.
           MOVE I-ASU TO W-ASU.
           MOVE I-GSU TO W-GSU.
           MOVE W-HS TO W-SI(CNT1).
      *           READ SIN-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SIN-F_PNAME1 BY REFERENCE SIN-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           GO TO M-40.
       M-45.
           MOVE ZERO TO W-C.
       M-50.
           PERFORM S-20 THRU S-65.
           ADD 1 TO W-C.
           IF  W-C = W-MS
               GO TO M-35
           END-IF
           GO TO M-50.
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
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Open" USING
            "OUTPUT" SNF-F_PNAME1 " " BY REFERENCE SNF-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" SIN-F_PNAME1 " " BY REFERENCE SIN-F_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE SNF-F_IDLST SNF-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SIN-F_IDLST SIN-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-D1 CNT1.
           MOVE SPACE TO W-TE.
       S-10.
           ADD 1 TO CNT1.
           IF  CNT1 = 25
               GO TO S-15
           END-IF
           MOVE ZERO TO W-SI(CNT1).
           GO TO S-10.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1 W-P2 W-P3 W-P4 W-P5.
       S-25.
           MOVE   15K  TO P-151 P-155.
           MOVE   20K  TO P-201 P-205.
           MOVE WWK TO P-WWK.
           MOVE SWK TO P-SWK.
           MOVE SSK TO P-SSK1 P-SSK2.
           MOVE TC-UNO TO P-UB.
           IF  W-DATE NOT = ZERO
               MOVE W-DATE TO P-DATE
           END-IF
           MOVE TC-JSU TO P-JU.
           MOVE TC-JSS TO P-JS.
           MOVE TC-NAME TO P-CNA.
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
           MOVE JCON2-03 TO P-UNA.
           MOVE "§" TO P-UM.
           MOVE "700-0975" TO P-UNO.
           MOVE "“úiƒSƒ€Š”Ž®‰ïŽÐ" TO P-NA.
           MOVE "‰ªŽRŽs¡‚W’š–Ú‚P‚U|‚P‚V" TO P-NJS.
           MOVE "TEL" TO P-TM.
           MOVE "086-243-2456" TO P-TEL.
           MOVE ZERO TO CNT2.
           MOVE W-KS TO P-KS.
           IF  P-KS1 NOT = "‚O"
               GO TO S-30
           END-IF
           MOVE SPACE TO P-KS1.
           IF  P-KS2 NOT = "‚O"
               GO TO S-30
           END-IF
           MOVE SPACE TO P-KS2.
       S-30.
           PERFORM S-85 THRU S-90.
       S-35.
           IF  CNT1 = CNT2
               GO TO S-60
           END-IF
           IF  CNT2 = 6 OR 12 OR 18
               PERFORM S-70 THRU S-90
           END-IF
           MOVE ZERO TO W-HS.
           ADD 1 TO CNT2.
           MOVE W-SI(CNT2) TO W-HS.
           MOVE W-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAD.
           IF  W-NA2 = SPACE
               GO TO S-45
           END-IF
           MOVE 15 TO W-NC.
       S-40.
           SUBTRACT 1 FROM W-NC.
           IF  W-NC = 4
               MOVE HI-NAME TO W-NAD
               GO TO S-45
           END-IF
           IF  W-ND1(W-NC) = SPACE
               GO TO S-45
           END-IF
           IF  W-ND2(1)  = SPACE
               GO TO S-45
           END-IF
           IF  W-ND2(10) NOT = SPACE
               MOVE HI-NAME TO W-NAD
               GO TO S-45
           END-IF
           MOVE W-ND2(9) TO W-ND2(10).
           MOVE W-ND2(8) TO W-ND2(9).
           MOVE W-ND2(7) TO W-ND2(8).
           MOVE W-ND2(6) TO W-ND2(7).
           MOVE W-ND2(5) TO W-ND2(6).
           MOVE W-ND2(4) TO W-ND2(5).
           MOVE W-ND2(3) TO W-ND2(4).
           MOVE W-ND2(2) TO W-ND2(3).
           MOVE W-ND2(1) TO W-ND2(2).
           MOVE W-ND1(W-NC) TO W-ND2(1).
           MOVE SPACE TO W-ND1(W-NC).
           GO TO S-40.
       S-45.
           MOVE SPACE TO W-P6 W-P7.
           MOVE  15K  TO P-152 P-153.
           MOVE  20K  TO P-202 P-203.
           MOVE W-NA1 TO P-HNA1.
           MOVE W-NA2 TO P-HNA2.
           IF  0 = HI-S1(1) AND HI-S1(2) AND HI-S1(3) AND HI-S1(4)
                            AND HI-S1(5) AND HI-S1(6) AND HI-S1(7)
               MOVE W-AS(02) TO P-SU(01)
               MOVE W-AS(04) TO P-SU(02)
               MOVE W-AS(05) TO P-SU(03)
               MOVE W-AS(06) TO P-SU(04)
               MOVE W-AS(07) TO P-SU(05)
               MOVE W-AS(08) TO P-SU(06)
               MOVE W-AS(09) TO P-SU(07)
               MOVE W-AS(10) TO P-SU(08)
               MOVE W-AS(11) TO P-SU(09)
               MOVE W-AS(12) TO P-SU(10)
               MOVE W-AS(13) TO P-SU(11)
               MOVE W-AS(14) TO P-SU(12)
               MOVE W-AS(15) TO P-SU(13)
               MOVE W-AS(16) TO P-SU(14)
               MOVE W-AS(17) TO P-SU(15)
               MOVE W-AS(18) TO P-SU(16)
               MOVE W-AS(19) TO P-SU(17)
               MOVE W-AS(20) TO P-SU(18)
               MOVE W-AS(21) TO P-SU(19)
               MOVE W-AS(22) TO P-SU(20)
               MOVE W-AS(23) TO P-SU(21)
               MOVE W-AS(24) TO P-SU(22)
               MOVE W-AS(25) TO P-SU(23)
               MOVE W-AS(26) TO P-SU(24)
               MOVE W-AS(27) TO P-SU(25)
           ELSE
               MOVE W-AS(01) TO P-SU(01)
               MOVE W-AS(02) TO P-SU(02)
               MOVE W-AS(03) TO P-SU(03)
               MOVE W-AS(04) TO P-SU(04)
               MOVE W-AS(05) TO P-SU(05)
               MOVE W-AS(06) TO P-SU(06)
               MOVE W-AS(07) TO P-SU(07)
           END-IF.
       S-55.
           MOVE W-GSU TO P-GSU.
           MOVE SPACE TO SP-R.
           MOVE W-P6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P7 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-35.
       S-60.
           PERFORM S-70 THRU S-80.
       S-65.
           EXIT.
       S-70.
           MOVE CNT2 TO CHK.
       S-75.
           IF  CHK > 6
               SUBTRACT 6 FROM CHK
               GO TO S-75
           END-IF
           COMPUTE W-LC = 14 - (CHK * 2).
           MOVE SPACE TO W-P8.
           MOVE  15K  TO P-154.
           MOVE  20K  TO P-204.
           MOVE W-TE TO P-TE.
           MOVE W-P8 TO SP-R.
           CALL "PR_LineFeed" USING W-LC RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-80.
           EXIT.
       S-85.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-90.
           EXIT.
