       IDENTIFICATION DIVISION.
       PROGRAM-ID. JAN01U.
      ********************************************
      *****    コート変換ファイル　再セット  *****
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
      *
           COPY LICODE.
           COPY LIHIM.
      *FD  JANWF
       01  JANWF_JAN01U.
           02  JANWF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JANWF_LNAME    PIC  X(012) VALUE "JANWF_JAN01U".
           02  F              PIC  X(001).
           02  JANWF_KEY1     PIC  X(100) VALUE SPACE.
           02  JANWF_SORT     PIC  X(100) VALUE SPACE.
           02  JANWF_IDLST    PIC  X(100) VALUE SPACE.
           02  JANWF_RES      USAGE  POINTER.
       01  JANW-R.
           02  JANW-D.
             03  JANW-JAN     PIC  9(013).
             03  JANW-HCD     PIC  9(006).
             03  JANW-SIZ     PIC  9(001).
             03  JANW-SNO     PIC  9(002).
             03  JANW-NNAME   PIC  X(020).
           02  F              PIC  X(022).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　コード変換ファイル　セット　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(005) VALUE
                    "２重データ".
             03  E-ME2   PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  ﾋﾝﾒｲﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  ｻｲｽﾞ ﾅｼ  ***".
             03  E-ME5   PIC  X(020) VALUE
                  "***  DELETE ｴﾗｰ  ***".
             03  E-JAN.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  9(001).
               04  FILLER  PIC  9(002).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
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
            "C-ERR" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "109" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "10" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "19" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "20" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JAN" " " "24" "0" "22" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-JAN" "X" "24" "38" "13" " " "E-JAN" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-JAN" BY REFERENCE JANW-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-JAN" "9" "24" "52" "6" "01E-JAN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-JAN" BY REFERENCE JANW-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-JAN" "9" "24" "59" "1" "02E-JAN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03E-JAN" BY REFERENCE JANW-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04E-JAN" "9" "24" "61" "2" "03E-JAN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04E-JAN" BY REFERENCE JANW-SNO "2" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "1"
            "CODE-KEY" BY REFERENCE CODE-KEY.
       M-10.
      *           READ CODEF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  CODE-TCD NOT = ZERO
               GO TO M-10
           END-IF
      *           DELETE CODEF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING CODEF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE CODEF_IDLST CODEF_PNAME1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-10.
       M-15.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO JANWF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JANWF_PNAME1 " " BY REFERENCE JANWF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-20.
      *           READ JANWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JANWF_PNAME1 BY REFERENCE JANW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JANW-HCD = ZERO
               GO TO M-20
           END-IF
           MOVE JANW-HCD TO HI-MHCD HI-HCD.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           MOVE 0 TO HI-S(4,10).
           IF  HI-S(JANW-SIZ,JANW-SNO) = 0
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF.
       M-25.
           INITIALIZE CODE-R.
           MOVE ZERO TO CODE-TCD.
           MOVE JANW-JAN TO CODE-JAN.
           MOVE JANW-HCD TO CODE-HCD.
           MOVE JANW-SIZ TO CODE-SIZ.
           MOVE JANW-SNO TO CODE-SNO.
           MOVE JANW-NNAME TO CODE-NAME.
      *           WRITE CODE-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            CODEF_PNAME1 CODEF_LNAME CODE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-30
           END-IF
           GO TO M-20.
       M-30.
           IF  ERR-STAT NOT = 22 AND 24
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ERR-STAT = 22
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JAN" E-JAN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "1"
            "CODE-KEY" BY REFERENCE CODE-KEY.
           GO TO M-25.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JANWF_IDLST JANWF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
