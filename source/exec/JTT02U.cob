       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JTT02U.
      ****************************************************************
      *    PROGRAM         :  ëqï ç›å…ÇdÇwÇbÇdÇkïœä∑ÉèÅ[ÉNÅ@çÏê¨     *
      *                    :  (WK0256Å®WK0128000)                    *
      ****************************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT                 PIC  X(2).
       77  W-FILE                   PIC  X(13).
       77  WK0256ID                 PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1              PIC  X(003).
           02  STN-NO2              PIC  X(003).
       01  W-FID1.
           02  W-FID11              PIC  X(006) VALUE "WK0256".
           02  W-FID12              PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(02).
       COPY  LSTAT.
      *
      *FD  WZAIKO
       01  WZAIKO_JTT02U.
           02  WZAIKO_PNAME1        PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  WZAIKO_LNAME         PIC  X(013) VALUE "WZAIKO_JTT02U".
           02  F                    PIC  X(001).
           02  WZAIKO_KEY1          PIC  X(100) VALUE SPACE.
           02  WZAIKO_SORT          PIC  X(100) VALUE SPACE.
           02  WZAIKO_IDLST         PIC  X(100) VALUE SPACE.
           02  WZAIKO_RES           USAGE  POINTER.
       01  WZAIKO-R.
           02   WZAIKO-R1.
               03   WZAIKO-KURA     PIC 9(1).
               03   WZAIKO-HCD      PIC 9(6).
               03   WZAIKO-SMS      PIC N(16).
               03   WZAIKO-SIZ      PIC X(4).
               03   WZAIKO-ITF      PIC X(16).
               03   WZAIKO-SU       PIC S9(6).
               03   WZAIKO-ISU      PIC 9(03).
               03   WZAIKO-JAN      PIC X(13).
               03   WZAIKO-BC       PIC 9(06).
               03   WZAIKO-BMC      PIC 9(02).
               03   WZAIKO-BMNO     PIC 9(01).
               03   WZAIKO-NGP      PIC 9(06).
               03   FILLER          PIC X(32).
           02   FILLER              PIC X(128).
       77  F                        PIC  X(001).
      *FD  ZAIKO
       01  ZAIKO_JTT02U.
           02  ZAIKO_PNAME1         PIC  X(009) VALUE "WK0128000".
           02  F                    PIC  X(001).
           02  ZAIKO_LNAME          PIC  X(012) VALUE "ZAIKO_JTT02U".
           02  F                    PIC  X(001).
           02  ZAIKO_KEY1           PIC  X(100) VALUE SPACE.
           02  ZAIKO_SORT           PIC  X(100) VALUE SPACE.
           02  ZAIKO_IDLST          PIC  X(100) VALUE SPACE.
           02  ZAIKO_RES            USAGE  POINTER.
       01  ZAIKO-R.
           02   ZAIKO-KURA          PIC 9(1).
           02   ZAIKO-HCD           PIC 9(6).
           02   ZAIKO-SMS           PIC N(16).
           02   ZAIKO-SIZ           PIC X(4).
           02   ZAIKO-ITF           PIC X(16).
           02   ZAIKO-SU            PIC S9(6).
           02   ZAIKO-ISU           PIC 9(03).
           02   ZAIKO-JAN           PIC X(13).
           02   ZAIKO-BC            PIC 9(06).
           02   ZAIKO-BMC           PIC 9(02).
           02   ZAIKO-BMNO          PIC 9(01).
           02   ZAIKO-NGP           PIC 9(06).
           02   ZAIKO-CHK           PIC 9(01).
           02   FILLER              PIC X(31).
       77  F                        PIC  X(001).
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
           02  FILLER  PIC  X(22) VALUE
                "                      ".
           02  FILLER  PIC  X(20) VALUE
               "ëqï ç›å…ÉèÅ[ÉNÅ@ïœä∑".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC X(17) VALUE
                  "***  DATA ≈º  ***".
       COPY  LSSEM.
       PROCEDURE   DIVISION.
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
            "C-MID" " " "0" "0" "42" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "RX" "1" "27" "22" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "1" "28" "20" "01C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0256ID.
           MOVE WK0256ID TO WZAIKO_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" WZAIKO_PNAME1 " " BY REFERENCE WZAIKO_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" ZAIKO_PNAME1 " " BY REFERENCE ZAIKO_IDLST "0".
      *           READ WZAIKO AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WZAIKO_PNAME1 BY REFERENCE WZAIKO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-10.
           INITIALIZE ZAIKO-R.
           MOVE WZAIKO-R1 TO ZAIKO-R.
       M-15.
      *           READ WZAIKO AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WZAIKO_PNAME1 BY REFERENCE WZAIKO-R " "
            RETURNING RET.
           IF  RET = 1
      *               WRITE ZAIKO-R
      *//////////////
               CALL "DB_Insert" USING
                ZAIKO_PNAME1 ZAIKO_LNAME ZAIKO-R RETURNING RET
               GO TO M-90
           END-IF
           IF  ZAIKO-JAN NOT = SPACE
               IF  WZAIKO-JAN = ZAIKO-JAN
                   ADD WZAIKO-SU TO ZAIKO-SU
                   GO TO M-15
               END-IF
           END-IF
      *           WRITE ZAIKO-R.
      *//////////////
           CALL "DB_Insert" USING
            ZAIKO_PNAME1 ZAIKO_LNAME ZAIKO-R RETURNING RET.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE WZAIKO_IDLST WZAIKO_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE ZAIKO_IDLST ZAIKO_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
