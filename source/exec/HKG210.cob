       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKG210.
      ******************************************************************
      *    PROGRAM         :  請求用売掛残高データ　抽出               *
      *    PRINTER TYPE    :  JIPS                                     *
      *    SCREEN          :  ******                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-FID3.
           02  W-FID31        PIC  X(006) VALUE "WK0512".
           02  W-FID32        PIC  X(003).
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-NGPD.
             03  W-NGD        PIC  9(006).
             03  W-PEYD       PIC  9(002).
           02  W-NGA          PIC  9(006).
           02  W-NGB          PIC  9(006).
           02  W-SKD          PIC  9(008).
           02  W-SKDD  REDEFINES W-SKD.
             03  W-SNEN       PIC  9(004).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SET          PIC  9(003).
           02  W-AMA          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-TCD          PIC  9(004).
           02  W-C            PIC  9(001).
           02  W-INV          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LITSKF.
      *FD  SKDF
       01  SKDF_HKG210.
           02  SKDF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKDF_LNAME     PIC  X(011) VALUE "SKDF_HKG210".
           02  F              PIC  X(001).
           02  SKDF_KEY1      PIC  X(100) VALUE SPACE.
           02  SKDF_SORT      PIC  X(100) VALUE SPACE.
           02  SKDF_IDLST     PIC  X(100) VALUE SPACE.
           02  SKDF_RES       USAGE  POINTER.
       01  SKD-R.
           02  SKD-KEY.
             03  SKD-TCD      PIC  9(004).
             03  SKD-DATE     PIC  9(008).
             03  SKD-DTC      PIC  9(001).
             03  SKD-DNO      PIC  9(006).
             03  SKD-GNO      PIC  9(001).
           02  SKD-HCD        PIC  9(006).
           02  SKD-SU         PIC S9(006)V9(02).
           02  SKD-T          PIC S9(006)V9(02).
           02  SKD-KIN        PIC S9(009).
           02  SKD-DC         PIC  9(001).
           02  SKD-CSC        PIC  9(001).
           02  SKD-SKD        PIC  9(008).
           02  SKD-SKDD  REDEFINES SKD-SKD.
             03  SKD-SNG.
               04  SKD-SNEN   PIC  9(004).
               04  SKD-SGET   PIC  9(002).
             03  SKD-SPEY     PIC  9(002).
           02  SKD-TNC        PIC  9(002).
           02  SKD-BMC        PIC  9(001).
           02  SKD-DCC        PIC  9(001).
           02  F              PIC  X(002).
           02  SKD-TCD2       PIC  9(004).
           02  SKD-CCD        PIC  9(003).
           02  SKD-BI         PIC  N(024).
           02  SKD-HNO        PIC  X(036).
           02  SKD-SHZ        PIC S9(007).
           02  SKD-KSU        PIC  9(003).
           02  SKD-JCD        PIC  9(006).
           02  F              PIC  X(012).
           02  SKD-SNO        PIC  9(006).
           02  F              PIC  X(064).
       77  F                  PIC  X(001).
      *FD  SUZF
       01  SUZF_HKG210.
           02  SUZF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SUZF_LNAME     PIC  X(011) VALUE "SUZF_HKG210".
           02  F              PIC  X(001).
           02  SUZF_KEY1      PIC  X(100) VALUE SPACE.
           02  SUZF_SORT      PIC  X(100) VALUE SPACE.
           02  SUZF_IDLST     PIC  X(100) VALUE SPACE.
           02  SUZF_RES       USAGE  POINTER.
       01  SUZ-R.
           02  SUZ-KEY.
             03  SUZ-TCD      PIC  9(004).
           02  SUZ-ZSD.
             03  SUZ-SZZ      PIC S9(009).
             03  SUZ-ZNGP     PIC  9(008).
           02  SUZ-KKD.
             03  SUZ-SNK      PIC S9(009).
             03  SUZ-SUA      PIC S9(009).
             03  SUZ-SUAZ     PIC S9(007).
           02  SUZ-TNC        PIC  9(002).
           02  SUZ-BMC        PIC  9(001).
           02  F              PIC  X(015).
       77  F                  PIC  X(001).
      *FD  SKDWF
       01  SKDWF_HKG210.
           02  SKDWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SKDWF_LNAME    PIC  X(012) VALUE "SKDWF_HKG210".
           02  F              PIC  X(001).
           02  SKDWF_KEY1     PIC  X(100) VALUE SPACE.
           02  SKDWF_SORT     PIC  X(100) VALUE SPACE.
           02  SKDWF_IDLST    PIC  X(100) VALUE SPACE.
           02  SKDWF_RES      USAGE  POINTER.
       01  SKDW-R.
           02  SKDW-DATE      PIC  9(008).
           02  F              PIC  X(504).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　請求用売掛残高データ　抽出　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-TSK   PIC  9(004).
             03  E-TCD   PIC  9(004).
           COPY LIBSCR.
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
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "43" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "43" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-TSK" "9" "24" "45" "4" "E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TSK" BY REFERENCE TSK-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "9" "24" "45" "4" "E-TSK" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE SKD-TCD "4" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           COPY LIBCPR.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-NGA W-NGD.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           MOVE W-NG TO W-NGB.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22 W-FID32.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO SUZF_PNAME1.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0256ID TO SKDF_PNAME1.
           MOVE W-FID3 TO WK0512ID.
           MOVE WK0512ID TO SKDWF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SUZF_PNAME1 " " BY REFERENCE SUZF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SKDF_PNAME1 " " BY REFERENCE SKDF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" SKDWF_PNAME1 " " BY REFERENCE SKDWF_IDLST "0".
      *           READ SKDWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDWF_PNAME1 BY REFERENCE SKDW-R " " RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO SKDW-DATE
           END-IF
           MOVE SKDW-DATE TO W-SKD.
           CALL "DB_F_Close" USING
            BY REFERENCE SKDWF_IDLST SKDWF_PNAME1.
           ADD 1 TO W-SGET.
           IF  W-SGET = 13
               ADD 1 TO W-SNEN
               MOVE 1 TO W-SGET
           END-IF
           IF  W-SPEY > 27
               IF  W-SGET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   MOVE 31 TO W-SPEY
               ELSE
                   IF  W-SGET = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO W-SPEY
                   ELSE
                       DIVIDE 4 INTO W-SNEN GIVING W-SET
                                            REMAINDER W-AMA
                       IF  W-SET = 0
                           MOVE 29 TO W-SPEY
                       ELSE
                           MOVE 28 TO W-SPEY
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
       M-10.
      *           READ TSKF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  ZERO = TSK-HTS(5) AND TSK-SZS(5) AND
                     TSK-HTS(4) AND TSK-SZS(4) AND
                     TSK-HTS(3) AND TSK-SZS(3)
               GO TO M-10
           END-IF
           MOVE ZERO TO SUZ-R.
           MOVE TSK-TCD TO SUZ-TCD.
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE TSK-ZNGP(5) TO SUZ-ZNGP
               COMPUTE SUZ-SZZ = TSK-HTS(5) + TSK-SZS(5)
           ELSE
               IF  TSK-ZNGP(4) NOT = ZERO
                   MOVE TSK-ZNGP(4) TO SUZ-ZNGP
                   COMPUTE SUZ-SZZ = TSK-HTS(4) + TSK-SZS(4)
               ELSE
                   IF  TSK-ZNGP(3) NOT = ZERO
                       MOVE TSK-ZNGP(3) TO SUZ-ZNGP
                       COMPUTE SUZ-SZZ = TSK-HTS(3) + TSK-SZS(3)
                   END-IF
               END-IF
           END-IF
           MOVE TSK-TNC TO SUZ-TNC.
           MOVE TSK-BMC TO SUZ-BMC.
      *           WRITE SUZ-R.
      *///////////////
           CALL "DB_Insert" USING
            SUZF_PNAME1 SUZF_LNAME SUZ-R RETURNING RET.
           GO TO M-10.
       M-15.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           MOVE ZERO TO TSK-KEY.
       M-20.
      *           READ SKDF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDF_PNAME1 BY REFERENCE SKD-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SKD-SKD = 99999999
               GO TO M-20
           END-IF
           IF  SKD-DTC = 5
               GO TO M-20
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO M-20
           END-IF
           IF  SKD-TCD NOT = TSK-KEY
               MOVE SKD-TCD TO TSK-KEY
      *               READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE ZERO TO TSK-ZNGP(3) TSK-ZNGP(4) TSK-ZNGP(5)
               END-IF
           END-IF
           IF (TSK-ZNGP(5) NOT = ZERO) OR (TSK-ZNGP(4) NOT = ZERO)
               IF  SKD-SNG > W-NGB
                   GO TO M-20
               ELSE
                   GO TO M-25
               END-IF
           END-IF
           IF  TSK-ZNGP(3) NOT = ZERO
               IF  SKD-SNG NOT = W-NGA
                   GO TO M-20
               ELSE
                   GO TO M-25
               END-IF
           END-IF
           IF  SKD-SKD > W-SKD
               GO TO M-20
           END-IF.
       M-25.
           MOVE SKD-TCD TO W-TCD.
           IF  SKD-TCD NOT = T-KEY
               MOVE SKD-TCD TO T-KEY
      *               READ T-M WITH UNLOCK INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE 1 TO W-INV
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-TCD" E-TCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-CL" E-CL "p" RETURNING RESU
                   MOVE ZERO TO T-R
               END-IF
           END-IF.
       M-30.
           IF  T-SS NOT = ZERO
               IF  SKD-SKD = ZERO
                   GO TO M-35
               END-IF
           END-IF
      *
           MOVE ZERO TO SUZ-R.
           MOVE W-TCD TO SUZ-TCD.
           MOVE T-TNC TO SUZ-TNC.
           MOVE T-BC TO SUZ-BMC.
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC = 0
                       IF  SKD-DC NOT = 8
                           ADD SKD-KIN TO SUZ-SUA
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO SUZ-SUAZ
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC = 0
                       IF  SKD-DC = 8
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO SUZ-SUAZ
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC NOT = 0
                       IF  SKD-DC NOT = 8
                           ADD SKD-KIN TO SUZ-SUA
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO SUZ-SUAZ
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC NOT = 1 AND 2 AND 4 AND 5
                   IF  SKD-CSC NOT = 0
                       IF  SKD-DC = 8
                           IF  SKD-GNO = 1
                               ADD SKD-SHZ TO SUZ-SUAZ
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 4
                   IF  SKD-GNO = 1
                       ADD SKD-SHZ TO SUZ-SUAZ
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 1 OR 2 OR 5
                   IF  SKD-CSC = 0
                       SUBTRACT SKD-KIN FROM SUZ-SUA
                       IF  SKD-GNO = 1
                           ADD SKD-SHZ TO SUZ-SUAZ
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 0
               IF  SKD-DC = 1 OR 2 OR 5
                   IF  SKD-CSC NOT = 0
                       SUBTRACT SKD-KIN FROM SUZ-SUA
                       IF  SKD-GNO = 1
                           ADD SKD-SHZ TO SUZ-SUAZ
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 1
               IF  SKD-CSC = 0
                   SUBTRACT SKD-KIN FROM SUZ-SUA
                   IF  SKD-GNO = 1
                       SUBTRACT SKD-SHZ FROM SUZ-SUAZ
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 1
               IF  SKD-CSC NOT = 0
                   SUBTRACT SKD-KIN FROM SUZ-SUA
                   IF  SKD-GNO = 1
                       SUBTRACT SKD-SHZ FROM SUZ-SUAZ
                   END-IF
               END-IF
           END-IF
           IF  SKD-DTC = 3
               ADD SKD-KIN TO SUZ-SNK
               ADD SKD-SHZ TO SUZ-SNK
           END-IF
      *           WRITE SUZ-R.
      *//////////////////////
           CALL "DB_Insert" USING
            SUZF_PNAME1 SUZF_LNAME SUZ-R RETURNING RET.
       M-35.
      *           READ SKDF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SKDF_PNAME1 BY REFERENCE SKD-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SKD-SKD = 99999999
               GO TO M-35
           END-IF
           IF  SKD-DTC = 5
               GO TO M-35
           END-IF
           IF  SKD-SNO NOT = ZERO
               GO TO M-35
           END-IF
           IF  SKD-TCD NOT = TSK-KEY
               MOVE SKD-TCD TO TSK-KEY
      *               READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE ZERO TO TSK-ZNGP(3) TSK-ZNGP(4) TSK-ZNGP(5)
               END-IF
           END-IF
           IF (TSK-ZNGP(5) NOT = ZERO) OR (TSK-ZNGP(4) NOT = ZERO)
               IF  SKD-SNG > W-NGB
                   GO TO M-35
               ELSE
                   GO TO M-40
               END-IF
           END-IF
           IF  TSK-ZNGP(3) NOT = ZERO
               IF  SKD-SNG NOT = W-NGA
                   GO TO M-35
               ELSE
                   GO TO M-40
               END-IF
           END-IF
           IF  SKD-SKD > W-SKD
               GO TO M-35
           END-IF.
       M-40.
           IF  SKD-TCD = W-TCD
               GO TO M-30
           END-IF
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SKDF_IDLST SKDF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SUZF_IDLST SUZF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
