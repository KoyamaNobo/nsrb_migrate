       IDENTIFICATION DIVISION.
       PROGRAM-ID. JT255R.
      *********************************************************
      *    PROGRAM         :  é¿ç›å…ñæç◊ñ‚çáÇπÅ@Å@Å@Å@Å@Å@Å@Å@*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SJ255R                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT  SECTION.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  W-D.
           02  W-AZCD.
             03  W-ZCD   OCCURS  4.
               04  W-ZC       PIC  9(001).
           02  W-AZSUD.
             03  W-AZSU   OCCURS  4.
               04  W-ZSUD  OCCURS  10.
                 05  W-ZSU    PIC S9(006).
             03  W-SSU        PIC S9(006).
           02  W-SUD.
             03  W-SU    OCCURS  10  PIC S9(006).
           02  W-KEY          PIC  9(006).
           02  W-HCD          PIC  9(006).
           02  W-SIZ          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SC           PIC  9(002).
           02  W-L            PIC  9(002).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-END          PIC  9(001).
       01  W-DATE.
           02  W-SG           PIC  9(002).
           02  W-SP           PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHIM.
      *FD  NJZAI
       01  NJZAI_JT255R.
           02  NJZAI_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                  PIC  X(001).
           02  NJZAI_LNAME        PIC  X(012) VALUE "NJZAI_JT255R".
           02  F                  PIC  X(001).
           02  NJZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  NJZAI_SORT         PIC  X(100) VALUE SPACE.
           02  NJZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  NJZAI_RES          USAGE  POINTER.
       01  NJZAI-R.
           02   NJZAI-KEY.
                03    NJZAI-01    PIC 9(1).
                03    NJZAI-02    PIC 9(6).
                03    NJZAI-03    PIC 9(1).
           02   NJZAI-04.
                03  NJZAI-041     OCCURS  10.
                    04  NJZAI-0411   PIC S9(6)     COMP-3.
           02   NJZAI-05.
                03  NJZAI-051     OCCURS  10.
                    04  NJZAI-0511   PIC S9(6)     COMP-3.
           02   NJZAI-06.
                03  NJZAI-061     OCCURS  10.
                    04  NJZAI-0611   PIC S9(6)     COMP-3.
           02   NJZAI-07.
                03  NJZAI-071     OCCURS  10.
                    04  NJZAI-0711   PIC S9(6)     COMP-3.
           02   NJZAI-08.
                03  NJZAI-081     OCCURS  10.
                    04  NJZAI-0811   PIC S9(6)     COMP-3.
           02   NJZAI-09.
                03  NJZAI-091     OCCURS  10.
                    04  NJZAI-0911   PIC S9(6)     COMP-3.
           02   NJZAI-10.
                03  NJZAI-101     OCCURS  10.
                    04  NJZAI-1011   PIC S9(6)     COMP-3.
           02   NJZAI-11.
                03  NJZAI-111     OCCURS  10.
                    04  NJZAI-1111   PIC S9(6)     COMP-3.
           02   FILLER            PIC X(12).
           02   NJZAI-99          PIC X(01).
           02   FILLER            PIC X(171).
       77  F                      PIC X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-KEY   PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DATE.
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
           02  FILLER.
             03  D-M1.
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
             03  D-M2.
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC  -(007).
               04  FILLER  PIC ------9 .
             03  D-C1    PIC  X(007) VALUE "       ".
             03  D-C2    PIC  X(007) VALUE "       ".
             03  D-C3    PIC  X(007) VALUE "       ".
           02  D-NM    PIC  X(037) VALUE
                "(  éüÕﬂ∞ºﬁ=ÿ¿∞›  ∫∞ƒﬁ=BSKIP   ÿ¿∞›  )".
           02  D-END   PIC  X(037) VALUE
                "        (  ÇdÇmÇcÅ@ÇcÇ`ÇsÇ`   ÿ¿∞›  )".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "22" "9" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "51" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "231" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DATE" " " "1" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DATE" "Z9" "1" "67" "2" " " "D-DATE" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DATE" BY REFERENCE W-SG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DATE" "Z9" "1" "71" "2" "01D-DATE" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DATE" BY REFERENCE W-SP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "153" "D-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-M1" " " "W-L" "0" "54" " " "02C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-M1" "9" "W-L" "1" "6" " " "D-M1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-M1" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-M1" "N" "W-L" "8" "48" "01D-M1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-M1" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-M2" " " "W-L" "0" "78" "D-M1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-M2" "9" "W-L" "2" "1" " " "D-M2" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-M2" BY REFERENCE W-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-M2" "-------" "W-L" "3" "7" "01D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-M2" BY REFERENCE W-SU(1) "6" "1" "1" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-M2" "-------" "W-L" "10" "7" "02D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-M2" BY REFERENCE W-SU(1) "6" "1" "2" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-M2" "-------" "W-L" "17" "7" "03D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-M2" BY REFERENCE W-SU(1) "6" "1" "3" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-M2" "-------" "W-L" "24" "7" "04D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-M2" BY REFERENCE W-SU(1) "6" "1" "4" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-M2" "-------" "W-L" "31" "7" "05D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-M2" BY REFERENCE W-SU(1) "6" "1" "5" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-M2" "-------" "W-L" "38" "7" "06D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-M2" BY REFERENCE W-SU(1) "6" "1" "6" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-M2" "-------" "W-L" "45" "7" "07D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-M2" BY REFERENCE W-SU(1) "6" "1" "7" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-M2" "-------" "W-L" "52" "7" "08D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "09D-M2" BY REFERENCE W-SU(1) "6" "1" "8" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-M2" "-------" "W-L" "59" "7" "09D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "10D-M2" BY REFERENCE W-SU(1) "6" "1" "9" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "11D-M2" "-------" "W-L" "66" "7" "10D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "11D-M2" BY REFERENCE W-SU(1) "6" "1" "10" 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "12D-M2" "------9" "W-L" "73" "7" "11D-M2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "12D-M2" BY REFERENCE W-SSU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-C1" "X" "W-L" "59" "7" "D-M2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-C2" "X" "W-L" "66" "7" "D-C1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-C3" "X" "W-L" "73" "7" "D-C2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "X" "23" "22" "37" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-END" "X" "23" "22" "37" "D-NM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "77" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "77" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           COPY LIBCPR.
           MOVE D-HNG TO W-SG.
           MOVE D-HNP TO W-SP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO NJZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Screen_Output" USING "SJ255R" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "0".
       M-15.
      *           READ NJZAI AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-10
           END-IF
           IF  W-KEY > NJZAI-02
               GO TO M-15
           END-IF
           MOVE 0 TO W-END.
           MOVE 6 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-20.
           MOVE NJZAI-02 TO W-HCD.
           MOVE ZERO TO W-AZCD W-AZSUD.
       M-25.
           MOVE ZERO TO W-SC.
       M-30.
           ADD 1 TO W-SC.
           IF  W-SC = 11
               GO TO M-35
           END-IF
           COMPUTE W-ZSU(NJZAI-03,W-SC) =
                 NJZAI-0411(W-SC) - NJZAI-0511(W-SC) + NJZAI-0611(W-SC)
               + NJZAI-0711(W-SC) - NJZAI-0811(W-SC) - NJZAI-0911(W-SC)
               + NJZAI-1111(W-SC).
           IF  W-ZSU(NJZAI-03,W-SC) NOT = ZERO
               ADD W-ZSU(NJZAI-03,W-SC) TO W-SSU
               IF  W-ZC(NJZAI-03) = 0
                   MOVE 1 TO W-ZC(NJZAI-03)
               END-IF
           END-IF
           GO TO M-30.
       M-35.
      *           READ NJZAI AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               GO TO M-40
           END-IF
           IF  W-HCD = NJZAI-02
               GO TO M-25
           END-IF.
       M-40.
           IF  W-AZCD = ZERO
               IF  W-END = 0
                   GO TO M-20
               ELSE
                   GO TO M-50
               END-IF
           END-IF
      *
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "Å@ÅñÅñÅ@ÇgÇhÇlÅ@Ç»ÇµÅ@ÅñÅñÅ@" TO HI-NAME
               GO TO M-20
           END-IF
           IF  HI-BC1 NOT = 26
               IF  W-END = 0
                   GO TO M-20
               ELSE
                   GO TO M-50
               END-IF
           END-IF
      *
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO CHK.
           IF  W-L = 22
               PERFORM S-05 THRU S-10
           END-IF
           IF  CHK = 1
               GO TO M-95
           END-IF
           IF  CHK = 9
               GO TO M-10
           END-IF
      *
           CALL "SD_Output" USING "D-M1" D-M1 "p" RETURNING RESU.
      *
           MOVE ZERO TO CNT.
       M-45.
           ADD 1 TO CNT.
           IF  CNT = 5
               IF  W-END = 0
                   GO TO M-20
               ELSE
                   GO TO M-50
               END-IF
           END-IF
           COMPUTE W-SIZ = CNT + 1.
           IF  W-SIZ = 5
               MOVE 1 TO W-SIZ
           END-IF
           IF  W-ZC(W-SIZ) = 0
               GO TO M-45
           END-IF
           MOVE ZERO TO W-SUD.
           MOVE W-AZSU(W-SIZ) TO W-SUD.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO CHK.
           IF  W-L = 22
               PERFORM S-05 THRU S-10
           END-IF
           IF  CHK = 1
               GO TO M-95
           END-IF
           IF  CHK = 9
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-M2" D-M2 "p" RETURNING RESU.
           IF  W-SIZ = 4
               CALL "SD_Output" USING "D-C1" D-C1 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 3 OR 4
               CALL "SD_Output" USING "D-C2" D-C2 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 2
               IF  1 = W-ZC(3) OR W-ZC(4) OR W-ZC(1)
                   CALL "SD_Output" USING "D-C3" D-C3 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-SIZ = 3
               IF  1 = W-ZC(4) OR W-ZC(1)
                   CALL "SD_Output" USING "D-C3" D-C3 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-SIZ = 4
               IF  1 = W-ZC(1)
                   CALL "SD_Output" USING "D-C3" D-C3 "p" RETURNING RESU
               END-IF
           END-IF
           GO TO M-45.
       M-50.
           PERFORM S-05 THRU S-10.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  W-END = 9
               CALL "SD_Output" USING "D-END" D-END "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  W-END NOT = 0
               MOVE 9 TO CHK
               GO TO S-10
           END-IF
           IF  ESTAT = PF9
               MOVE 1 TO CHK
               GO TO S-10
           END-IF
           IF  ESTAT = BTB
               MOVE 9 TO CHK
               GO TO S-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-05
           END-IF
           CALL "SD_Screen_Output" USING "SJ255R" RETURNING RESU.
           CALL "SD_Output" USING "D-DATE" D-DATE "p" RETURNING RESU.
           MOVE 7 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       S-10.
           EXIT.
