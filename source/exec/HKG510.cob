       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG510.
      *********************************************************
      *    PROGRAM         :  入金予定ワーク作成              *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-ME               PIC  X(040).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0064".
           02  W-FID22        PIC  X(003).
       01  W-R.
           02  WR-TCD         PIC  9(004).
           02  WR-NGP         PIC  9(008).
           02  WR-NGPD  REDEFINES WR-NGP.
             03  WR-NG.
               04  WR-NEN     PIC  9(004).
               04  WR-GET     PIC  9(002).
             03  WR-PEY       PIC  9(002).
           02  WR-ZS          PIC S9(009).
           02  WR-ZSZ         PIC S9(007).
           02  WR-UR          PIC S9(009).
           02  WR-URZ         PIC S9(007).
           02  WR-TS          PIC S9(007).
           02  WR-TSZ         PIC S9(005).
           02  WR-NK          PIC S9(009).
           02  WR-NKZ         PIC S9(007).
           02  WR-DATE        PIC  9(006).
           02  WR-SI          PIC  9(003).
           02  WR-SU          PIC  9(001).
           02  F              PIC  X(019).
           02  WR-DC          PIC  9(001).
           02  F              PIC  X(026).
       01  W-DATA.
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-D.
             03  W-KIN        PIC S9(009).
             03  W-SHZ        PIC S9(007).
             03  W-KEI        PIC S9(009).
           02  W-RD.
             03  W-RKIN       PIC S9(009).
             03  W-RSHZ       PIC S9(007).
             03  W-RKEI       PIC S9(009).
           02  W-FD.
             03  W-FKIN       PIC S9(009).
             03  W-FSHZ       PIC S9(007).
             03  W-FKEI       PIC S9(009).
           02  W-NK.
             03  W-NKIN       PIC S9(009).
             03  W-NSHZ       PIC S9(009).
           02  W-TCD          PIC  9(004).
           02  CHK            PIC  9(001).
           02  W-NSU          PIC  9(003).
           02  W-KS1          PIC  9(003).
           02  W-KS2          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIBFDD.
           COPY LISKDF.
           COPY LITM.
      *FD  SMWF
       01  SMWF_HKG510.
           02  SMWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SMWF_LNAME     PIC  X(011) VALUE "SMWF_HKG510".
           02  F              PIC  X(001).
           02  SMWF_KEY1      PIC  X(100) VALUE SPACE.
           02  SMWF_SORT      PIC  X(100) VALUE SPACE.
           02  SMWF_IDLST     PIC  X(100) VALUE SPACE.
           02  SMWF_RES       USAGE  POINTER.
       01  SMW-R.
           02  SMW-TCD        PIC  9(004).
           02  SMW-NGP        PIC  9(008).
           02  SMW-NGPD  REDEFINES SMW-NGP.
             03  SMW-NG       PIC  9(006).
             03  SMW-PEY      PIC  9(002).
           02  SMW-ZS         PIC S9(009).
           02  SMW-ZSZ        PIC S9(007).
           02  SMW-UR         PIC S9(009).
           02  SMW-URZ        PIC S9(007).
           02  SMW-TS         PIC S9(007).
           02  SMW-TSZ        PIC S9(005).
           02  SMW-NK         PIC S9(009).
           02  SMW-NKZ        PIC S9(007).
           02  SMW-DATE       PIC  9(006).
           02  SMW-SI         PIC  9(003).
           02  SMW-SU         PIC  9(001).
           02  F              PIC  X(019).
           02  SMW-DC         PIC  9(001).
           02  F              PIC  X(026).
       77  F                  PIC  X(001).
      *FD  NYWF
       01  NYWF_HKG510.
           02  NYWF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYWF_LNAME     PIC  X(011) VALUE "NYWF_HKG510".
           02  F              PIC  X(001).
           02  NYWF_KEY1      PIC  X(100) VALUE SPACE.
           02  NYWF_SORT      PIC  X(100) VALUE SPACE.
           02  NYWF_IDLST     PIC  X(100) VALUE SPACE.
           02  NYWF_RES       USAGE  POINTER.
       01  NYW-R.
           02  NYW-DATE       PIC  9(008).
           02  NYW-NGP   REDEFINES NYW-DATE.
             03  NYW-NEN      PIC  9(004).
             03  NYW-GET      PIC  9(002).
             03  NYW-PEY      PIC  9(002).
           02  NYW-TCD        PIC  9(004).
           02  NYW-KIN        PIC S9(009).
           02  NYW-SHZ        PIC S9(007).
           02  NYW-KEI        PIC S9(009).
           02  NYW-NC.
             03  NYW-NC1      PIC  9(001).
             03  NYW-NC2      PIC  9(001).
           02  NYW-TGK        PIC  9(008).
           02  NYW-TGKD  REDEFINES NYW-TGK.
             03  NYW-TNEN     PIC  9(004).
             03  NYW-TGET     PIC  9(002).
             03  NYW-TPEY     PIC  9(002).
           02  NYW-SS         PIC  9(008).
           02  NYW-SSD   REDEFINES NYW-SS.
             03  NYW-SNEN     PIC  9(004).
             03  NYW-SGET     PIC  9(002).
             03  NYW-SPEY     PIC  9(002).
           02  NYW-TNC        PIC  9(002).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　入金予定ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
             03  E-KEY   PIC  9(004).
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "280" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "104" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "104" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "40" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-ME "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "56" "4" "E-ME" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE SMW-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           ACCEPT W-NGP FROM DATE.
           COPY LIBCPR.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO SMWF_PNAME1.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO NYWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SMWF_PNAME1 " " BY REFERENCE SMWF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 "SHARED" BY REFERENCE SKDF_IDLST "1"
            "SKD-KEY" BY REFERENCE SKD-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" NYWF_PNAME1 " " BY REFERENCE NYWF_IDLST "0".
       M-10.
      *           READ SMWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SMWF_PNAME1 BY REFERENCE SMW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-NG > SMW-NG
               GO TO M-10
           END-IF
           IF  CHK = 1
               IF  SMW-TCD = W-TCD
                   GO TO M-10
               END-IF
           END-IF.
       M-15.
           MOVE SMW-TCD TO W-TCD.
           MOVE 0 TO CHK.
           MOVE SMW-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE "***  ﾄｸｲｻｷ ﾅｼ  ***" TO W-ME
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               MOVE 1 TO CHK
               GO TO M-10
           END-IF
           IF  T-ENG NOT = ZERO
               MOVE 1 TO CHK
               GO TO M-10
           END-IF
           IF  SMW-DATE NOT = ZERO
               MOVE 1 TO CHK
               GO TO M-10
           END-IF
      *
           PERFORM S-35 THRU S-45.
           MOVE ZERO TO W-FD.
           COMPUTE W-FKIN = SMW-ZS + SMW-UR + SMW-TS - SMW-NK - W-NKIN.
           COMPUTE W-FSHZ = SMW-ZSZ + SMW-URZ + SMW-TSZ - SMW-NKZ -
                            W-NSHZ.
           COMPUTE W-FKEI = W-FKIN + W-FSHZ.
           IF  W-FKEI <= ZERO
               MOVE 1 TO CHK
               GO TO M-10
           END-IF.
       M-20.
           MOVE ZERO TO W-R.
           MOVE SMW-R TO W-R.
           MOVE ZERO TO W-RD.
           MOVE W-FD TO W-RD.
       M-25.
      *           READ SMWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SMWF_PNAME1 BY REFERENCE SMW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  W-NG > SMW-NG
               GO TO M-25
           END-IF
           IF  CHK = 1
               IF  SMW-TCD = W-TCD
                   GO TO M-25
               END-IF
           END-IF
           MOVE ZERO TO W-FD.
           IF  SMW-TCD NOT = W-TCD
               GO TO M-30
           END-IF
           IF  SMW-DATE NOT = ZERO
               MOVE 1 TO CHK
               GO TO M-25
           END-IF
           COMPUTE W-FKIN = SMW-ZS + SMW-UR + SMW-TS - SMW-NK.
           COMPUTE W-FSHZ = SMW-ZSZ + SMW-URZ + SMW-TSZ - SMW-NKZ.
           COMPUTE W-FKEI = W-FKIN + W-FSHZ.
           IF  W-FKEI <= ZERO
               MOVE 1 TO CHK
               GO TO M-25
           END-IF
      *
           PERFORM S-05 THRU S-30.
           GO TO M-20.
       M-30.
           PERFORM S-05 THRU S-30.
           GO TO M-15.
       M-85.
           MOVE ZERO TO W-FD.
           PERFORM S-05 THRU S-30.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SMWF_IDLST SMWF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE NYWF_IDLST NYWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-D.
           COMPUTE W-KIN = W-RKIN - W-FKIN.
           COMPUTE W-SHZ = W-RSHZ - W-FSHZ.
           COMPUTE W-KEI = W-RKEI - W-FKEI.
           IF  ZERO = W-KIN AND W-SHZ AND W-KEI
               GO TO S-30
           END-IF
           IF  W-KEI < 0
               GO TO S-30
           END-IF
      *
           MOVE ZERO TO NYW-R.
           IF  T-NKY = ZERO
               MOVE 99999999 TO NYW-DATE
               GO TO S-15
           END-IF
           MOVE WR-NGP TO NYW-DATE.
           MOVE NYW-PEY TO W-NSU.
           ADD T-NKY TO W-NSU.
       S-10.
           IF  W-NSU > 30
               SUBTRACT 30 FROM W-NSU
               ADD 1 TO NYW-GET
               IF  NYW-GET = 13
                   MOVE 1 TO NYW-GET
                   ADD 1 TO NYW-NEN
                   GO TO S-10
               ELSE
                   GO TO S-10
               END-IF
           END-IF
           MOVE W-NSU TO NYW-PEY.
           IF  NYW-PEY < 28
               GO TO S-15
           END-IF
           IF  NYW-GET = 2
               DIVIDE 4 INTO NYW-NEN GIVING W-KS1 REMAINDER W-KS2
           END-IF
           IF  NYW-GET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               MOVE 31 TO NYW-PEY
               IF  NYW-GET = 4 OR 6 OR 9 OR 11
                   MOVE 30 TO NYW-PEY
                   IF  W-KS2 = 0
                       MOVE 29 TO NYW-PEY
                   ELSE
                       MOVE 28 TO NYW-PEY
                   END-IF
               END-IF
           END-IF.
       S-15.
           MOVE W-TCD TO NYW-TCD.
           MOVE W-KIN TO NYW-KIN.
           MOVE W-SHZ TO NYW-SHZ.
           MOVE W-KEI TO NYW-KEI.
           MOVE T-SHC1 TO NYW-NC1.
           MOVE T-SHC2 TO NYW-NC2.
           IF  NYW-NC1 NOT = 3
               GO TO S-25
           END-IF
           IF  T-SSI = ZERO
               MOVE 99999999 TO NYW-TGK
               GO TO S-25
           END-IF
           MOVE NYW-DATE TO NYW-TGK.
           MOVE NYW-TPEY TO W-NSU.
           ADD T-SSI TO W-NSU.
       S-20.
           IF  W-NSU > 30
               SUBTRACT 30 FROM W-NSU
               ADD 1 TO NYW-TGET
               IF  NYW-TGET = 13
                   MOVE 1 TO NYW-TGET
                   ADD 1 TO NYW-TNEN
                   GO TO S-20
               ELSE
                   GO TO S-20
               END-IF
           END-IF
           MOVE W-NSU TO NYW-TPEY.
           IF  NYW-TPEY < 28
               GO TO S-25
           END-IF
           IF  NYW-TGET = 2
               DIVIDE 4 INTO NYW-TNEN GIVING W-KS1 REMAINDER W-KS2
           END-IF
           IF  NYW-TGET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
               MOVE 31 TO NYW-TPEY
               IF  NYW-GET = 4 OR 6 OR 9 OR 11
                   MOVE 30 TO NYW-TPEY
                   IF  W-KS2 = 0
                       MOVE 29 TO NYW-TPEY
                   ELSE
                       MOVE 28 TO NYW-TPEY
                   END-IF
               END-IF
           END-IF.
       S-25.
           MOVE WR-NGP TO NYW-SS.
           MOVE T-TNC TO NYW-TNC.
      *           WRITE NYW-R.
      *//////////////
           CALL "DB_Insert" USING
            NYWF_PNAME1 NYWF_LNAME NYW-R RETURNING RET.
       S-30.
           EXIT.
       S-35.
           MOVE ZERO TO W-NK.
           MOVE SPACE TO SKD-KEY.
           MOVE W-TCD TO SKD-TCD.
           MOVE SMW-NGP TO SKD-DATE.
           MOVE 5 TO SKD-DTC.
      *           START SKDF KEY NOT < SKD-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SKDF_PNAME1 "SKD-KEY" " NOT < " SKD-KEY RETURNING RET.
           IF  RET = 1
               GO TO S-45
           END-IF.
       S-40.
      *           READ SKDF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" SKDF_PNAME1 BY REFERENCE SKD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO S-45
           END-IF
           IF  W-TCD NOT = SKD-TCD
               GO TO S-45
           END-IF
           IF  SKD-DTC = 3
               ADD SKD-KIN TO W-NKIN
               ADD SKD-SHZ TO W-NSHZ
           END-IF
           GO TO S-40.
       S-45.
           EXIT.
