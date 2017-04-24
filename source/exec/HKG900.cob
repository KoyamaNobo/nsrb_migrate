       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG900.
      *******************************************************************
      *    PROGRAM         :  未請求分　請求処理                        *
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-NGP.
             03  W-NG         PIC  9(006).
             03  W-NGD   REDEFINES W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-DTW1         PIC  9(003).
           02  W-DTW2         PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITTM.
           COPY LITSKF.
           COPY LISKDF.
      *FD  SMF
       01  SMF_HKG900.
           02  SMF_PNAME1     PIC  X(003) VALUE "SMF".
           02  F              PIC  X(001).
           02  SMF_LNAME      PIC  X(010) VALUE "SMF_HKG900".
           02  F              PIC  X(001).
           02  SMF_KEY1       PIC  X(100) VALUE SPACE.
           02  SMF_SORT       PIC  X(100) VALUE SPACE.
           02  SMF_IDLST      PIC  X(100) VALUE SPACE.
           02  SMF_RES        USAGE  POINTER.
       01  SM-R.
           02  SM-TCD         PIC  X(004).
           02  SM-DATE        PIC  9(008).
           02  SM-SZZ         PIC S9(009).
           02  SM-SZZZ        PIC S9(007).
           02  SM-SUK         PIC S9(009).
           02  SM-SUKZ        PIC S9(007).
           02  SM-STS         PIC S9(007).
           02  SM-STSZ        PIC S9(005).
           02  SM-SNK         PIC S9(009).
           02  SM-SNKZ        PIC S9(007).
           02  F              PIC  X(010).
           02  SM-DNO         PIC  9(006).
           02  F              PIC  X(004).
           02  SM-TNC         PIC  9(002).
           02  F              PIC  X(008).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　未請求分　請求処理　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(024) VALUE
                  "***  TSKF WRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  TSKF REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  SKDF REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME5   PIC  X(023) VALUE
                  "***  SMF WRITE ｴﾗｰ  ***".
             03  E-TCD   PIC  9(004).
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "266" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "20" "38" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "20" "38" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "20" "38" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "20" "38" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "20" "38" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "20" "38" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "20" "38" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "118" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "118" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "15" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "23" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "9" "24" "45" "4" "E-ME5" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
      *
           IF  W-GET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               MOVE 31 TO W-PEY
           ELSE
               IF  W-GET = 4 OR 6 OR 9 OR 11
                   MOVE 30 TO W-PEY
               ELSE
                   DIVIDE 4 INTO W-NEN GIVING W-DTW1
                                             REMAINDER W-DTW2
                   IF  W-DTW2 = 0
                       MOVE 29 TO W-PEY
                   ELSE
                       MOVE 28 TO W-PEY
                   END-IF
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           CALL "DB_F_Open" USING
            "I-O" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" SMF_PNAME1 " " BY REFERENCE SMF_IDLST "0".
       M-10.
      *           READ TT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF
           IF  ZERO = TT-TUZ AND TT-TUZZ
               GO TO M-10
           END-IF
      *
           MOVE TT-KEY TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  T-SS NOT = 00 AND 99
               GO TO M-10
           END-IF
      *
           PERFORM SMW-RTN THRU SMW-EX.
      *
           MOVE TT-KEY TO TSK-KEY.
      *           READ TSKF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
      *
           MOVE TT-TUZ TO TSK-HTS(4).
           MOVE TT-TUZZ TO TSK-SZS(4).
           MOVE W-NGP TO TSK-ZNGP(4).
      *           REWRITE TSK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-10.
       M-15.
           MOVE ZERO TO TSK-R.
           MOVE TT-KEY TO TSK-KEY.
           MOVE TT-TUZ TO TSK-HTS(4).
           MOVE TT-TUZZ TO TSK-SZS(4).
           MOVE W-NGP TO TSK-ZNGP(4).
           MOVE T-TNC TO TSK-TNC.
           MOVE T-BC TO TSK-BMC.
           MOVE T-DCC TO TSK-DCC.
      *           WRITE TSK-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TSKF_PNAME1 TSKF_LNAME TSK-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-20
           END-IF
           GO TO M-10.
       M-20.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SMF_IDLST SMF_PNAME1.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
       M-25.
      *           READ SKDF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  SKD-NG NOT = W-NG
               GO TO M-25
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO M-25
           END-IF
           IF  SKD-SKD NOT = ZERO
               GO TO M-25
           END-IF
           MOVE SKD-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF  T-SS NOT = 00 AND 99
               GO TO M-25
           END-IF
           MOVE W-NGP TO SKD-SKD.
           MOVE 999999 TO SKD-SNO.
      *           REWRITE SKD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SKDF_PNAME1 SKDF_LNAME SKD-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SMW-RTN.
           MOVE ZERO TO SM-R.
           MOVE TT-TCD TO SM-TCD.
           MOVE W-NGP TO SM-DATE.
           MOVE TT-TZZ TO SM-SZZ.
           MOVE TT-TZZZ TO SM-SZZZ.
           COMPUTE SM-SUK = TT-TUA - TT-TNB.
           COMPUTE SM-SUKZ = TT-TUAZ - TT-TNBZ.
           MOVE TT-TNK TO SM-SNK.
           MOVE TT-TNKZ TO SM-SNKZ.
           MOVE TT-TNC TO SM-TNC.
      *           WRITE SM-R.
      *//////////////
           CALL "DB_Insert" USING
            SMF_PNAME1 SMF_LNAME SM-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO SMW-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-TCD" E-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SMW-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SMF_IDLST SMF_PNAME1.
           MOVE "SMF          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SMF_PNAME1 " " BY REFERENCE SMF_IDLST "0".
           GO TO SMW-RTN.
       SMW-EX.
           EXIT.
