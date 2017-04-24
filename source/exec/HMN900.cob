       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN900.
      *********************************************************
      *    PROGRAM         :  決算用棚卸ワーク　作成          *
      *    PRINTER TYPE    :  *****                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-INV          PIC  9(001).
           02  W-SU           PIC S9(007).
           02  W-TN           PIC  9(005).
           02  W-KKIN         PIC S9(009).
           02  W-FKIN         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHTIM.
           COPY LIHHTF.
           COPY LIHIM.
      *FD  HKTWF
       01  HKTWF_HMN900.
           02  HKTWF_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HKTWF_LNAME   PIC  X(012) VALUE "HKTWF_HMN900".
           02  F              PIC  X(001).
           02  HKTWF_KEY1    PIC  X(100) VALUE SPACE.
           02  HKTWF_SORT    PIC  X(100) VALUE SPACE.
           02  HKTWF_IDLST   PIC  X(100) VALUE SPACE.
           02  HKTWF_RES     USAGE  POINTER.
       01  HKTW-R.
           02  HKTW-HCD       PIC  9(006).
           02  HKTW-SNO       PIC  9(001).
           02  HKTW-SU        PIC S9(007).
           02  HKTW-KTN       PIC  9(005).
           02  HKTW-KKIN      PIC S9(009).
           02  HKTW-FTN       PIC  9(005).
           02  HKTW-FKIN      PIC S9(009).
           02  HKTW-BC1       PIC  9(002).
           02  HKTW-BC2       PIC  9(002).
           02  HKTW-BC3       PIC  9(002).
           02  HKTW-BMC       PIC  9(002).
           02  HKTW-BMNO      PIC  9(001).
           02  F              PIC  X(013).
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
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　決算用棚卸ワーク　作成　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-KEY   PIC  9(006).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "84" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "84" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "9" "24" "41" "6" "E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HI-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO HKTWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" HKTWF_PNAME1 " " BY REFERENCE HKTWF_IDLST "0".
       M-10.
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           COMPUTE W-SU = HTI-SU(01) + HTI-SU(02) + HTI-SU(03)
                        + HTI-SU(04) + HTI-SU(05) + HTI-SU(06)
                        + HTI-SU(07) + HTI-SU(08) + HTI-SU(09)
                        + HTI-SU(10).
           IF  W-SU = ZERO
               GO TO M-10
           END-IF
      *
           MOVE HTI-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-FT
           END-IF
      *
           MOVE ZERO TO HKTW-R.
           MOVE HTI-HCD TO HKTW-HCD.
           MOVE HTI-SNO TO HKTW-SNO.
           IF  HTI-GNO < 5
               COMPUTE HKTW-SU = W-SU * HTI-ISU
           ELSE
               MOVE W-SU TO HKTW-SU
           END-IF
           IF  HI-KT NOT = ZERO
               MOVE HI-KT TO HKTW-KTN
               COMPUTE HKTW-KKIN = HKTW-SU * HKTW-KTN
           ELSE
               MOVE HI-FT TO HKTW-KTN
               COMPUTE HKTW-KKIN = HKTW-SU * HKTW-KTN
           END-IF
           MOVE HI-FT TO HKTW-FTN.
           COMPUTE HKTW-FKIN = HKTW-SU * HKTW-FTN.
           MOVE HI-BC1 TO HKTW-BC1.
           MOVE HI-BC2 TO HKTW-BC2.
           MOVE HI-BC3 TO HKTW-BC3.
           MOVE HI-BMC TO HKTW-BMC.
           MOVE HI-BMNO TO HKTW-BMNO.
      *           WRITE HKTW-R.
      *//////////////
           CALL "DB_Insert" USING
            HKTWF_PNAME1 HKTWF_LNAME HKTW-R RETURNING RET.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
       M-20.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  HHT-SIZ NOT = 4
               GO TO M-20
           END-IF
           IF  HHT-TSU(10) = ZERO
               GO TO M-20
           END-IF
      *
           MOVE HHT-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO HI-FT
           END-IF
      *
           MOVE ZERO TO HKTW-R.
           MOVE HHT-HCD TO HKTW-HCD.
           MOVE 9 TO HKTW-SNO.
           MOVE HHT-TSU(10) TO HKTW-SU.
           IF  HI-KT NOT = ZERO
               MOVE HI-KT TO HKTW-KTN
               COMPUTE HKTW-KKIN = HKTW-SU * HKTW-KTN
           ELSE
               MOVE HI-FT TO HKTW-KTN
               COMPUTE HKTW-KKIN = HKTW-SU * HKTW-KTN
           END-IF
           MOVE HI-FT TO HKTW-FTN.
           COMPUTE HKTW-FKIN = HKTW-SU * HKTW-FTN.
           MOVE HHT-BC1 TO HKTW-BC1.
           MOVE HHT-BC2 TO HKTW-BC2.
           MOVE HHT-BC3 TO HKTW-BC3.
           MOVE HI-BMC TO HKTW-BMC.
           MOVE HI-BMNO TO HKTW-BMNO.
      *           WRITE HKTW-R.
      *//////////////
           CALL "DB_Insert" USING
            HKTWF_PNAME1 HKTWF_LNAME HKTW-R RETURNING RET.
           GO TO M-20.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HKTWF_IDLST HKTWF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
