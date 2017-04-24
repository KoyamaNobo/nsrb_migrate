       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBP970.
      ********************************************
      *****     銀行入出金取引データ累積     *****
      ********************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE       SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-SNO              PIC  9(003).
       01  W-NGP              PIC  9(006).
      *
      *FD  RNOUST-F
       01  RNOUST-F_FBP970.
           02  RNOUST-F_PNAME1 PIC  X(007) VALUE "RNOUSTF".
           02  F               PIC  X(001).
           02  RNOUST-F_LNAME  PIC  X(015) VALUE "RNOUST-F_FBP970".
           02  F               PIC  X(001).
           02  RNOUST-F_KEY1   PIC  X(100) VALUE SPACE.
           02  RNOUST-F_SORT   PIC  X(100) VALUE SPACE.
           02  RNOUST-F_IDLST  PIC  X(100) VALUE SPACE.
           02  RNOUST-F_RES    USAGE  POINTER.
       01  RNOUST-R            PIC  X(200).
       77  F                   PIC  X(001).
      *FD  RNOUSTYR
       01  RNOUSTYR_FBP970.
           02  RNOUSTYR_PNAME1 PIC  X(008) VALUE "RNOUSTYR".
           02  F               PIC  X(001).
           02  RNOUSTYR_LNAME  PIC  X(015) VALUE "RNOUSTYR_FBP970".
           02  F               PIC  X(001).
           02  RNOUSTYR_KEY1   PIC  X(100) VALUE SPACE.
           02  RNOUSTYR_SORT   PIC  X(100) VALUE SPACE.
           02  RNOUSTYR_IDLST  PIC  X(100) VALUE SPACE.
           02  RNOUSTYR_RES    USAGE  POINTER.
       01  RNOUSTY-R.
           02  F               PIC  X(191).
           02  RNOUSTY-SNO     PIC  9(003).
           02  RNOUSTY-NGP     PIC  X(006).
       77  F                   PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　銀行　入出金受信データ　累積　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME5   PIC  X(028) VALUE
                  "***  RNOUSTYR WRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "28" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "28" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           ACCEPT W-NGP FROM DATE.
           PERFORM S-05 THRU S-20.
           CALL "DB_F_Open" USING
            "INPUT" RNOUST-F_PNAME1 " " BY REFERENCE RNOUST-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" RNOUSTYR_PNAME1 " " BY REFERENCE
            RNOUSTYR_IDLST "0".
       M-20.
      *           READ RNOUST-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUST-F_PNAME1 BY REFERENCE RNOUST-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF.
       M-25.
           MOVE SPACE TO RNOUSTY-R.
           MOVE RNOUST-R TO RNOUSTY-R.
           MOVE W-SNO TO RNOUSTY-SNO.
           MOVE W-NGP TO RNOUSTY-NGP.
      *           WRITE RNOUSTY-R.
      *//////////////
           CALL "DB_Insert" USING
            RNOUSTYR_PNAME1 RNOUSTYR_LNAME RNOUSTY-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE RNOUST-F_IDLST RNOUST-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE RNOUSTYR_IDLST RNOUSTYR_PNAME1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTYR_IDLST RNOUSTYR_PNAME1.
           MOVE "RNOUSTYR     " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" RNOUSTYR_PNAME1 " " BY REFERENCE
            RNOUSTYR_IDLST "0".
           GO TO M-25.
       M-30.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUST-F_IDLST RNOUST-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTYR_IDLST RNOUSTYR_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           CALL "DB_F_Open" USING
            "INPUT" RNOUSTYR_PNAME1 " " BY REFERENCE RNOUSTYR_IDLST "0".
           MOVE 999 TO W-SNO.
       S-10.
      *           READ RNOUSTYR AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTYR_PNAME1 BY REFERENCE RNOUSTY-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-15
           END-IF
           IF  RNOUSTY-NGP NOT = W-NGP
               GO TO S-10
           END-IF
           IF  RNOUSTY-SNO NOT = W-SNO
               MOVE RNOUSTY-SNO TO W-SNO
           END-IF
           GO TO S-10.
       S-15.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTYR_IDLST RNOUSTYR_PNAME1.
           IF  W-SNO = 999
               MOVE ZERO TO W-SNO
           ELSE
               ADD 1 TO W-SNO
           END-IF.
       S-20.
           EXIT.
