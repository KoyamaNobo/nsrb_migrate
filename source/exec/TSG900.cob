       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG900.
      *******************************************
      *****     支払手形　手形期日抽出      *****
      *******************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
       01  W-DATA.
           02  W-MGET         PIC  9(002).
           02  W-FGET         PIC  9(002).
           02  W-TCD          PIC  9(004).
           02  W-DC           PIC  9(001).
           02  W-D.
             03  W-STS        PIC  9(003).
             03  W-TTS        PIC  9(002).
             03  W-THN        PIC  9(002).
             03  W-TMC        PIC  9(001).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LISM.
           COPY LSSHIT.
      *FD  SAITO-F
       01  SAITO-F_TSG900.
           02  SAITO-F_PNAME1 PIC  X(009)  VALUE SPACE.
           02  F              PIC  X(001).
           02  SAITO-F_LNAME  PIC  X(014)  VALUE "SAITO-F_TSG900".
           02  F              PIC  X(001).
           02  SAITO-F_KEY1   PIC  X(100)  VALUE SPACE.
           02  SAITO-F_KEY2   PIC  X(100)  VALUE SPACE.
           02  SAITO-F_SORT   PIC  X(100)  VALUE SPACE.
           02  SAITO-F_IDLST  PIC  X(100)  VALUE SPACE.
           02  SAITO-F_RES    USAGE  POINTER.
       01  SAITO-R.
           02  SAITO-TCD      PIC  9(004).
           02  SAITO-TTS      PIC  9(002).
           02  SAITO-THN      PIC  9(002).
           02  SAITO-TMC      PIC  9(001).
           02  F              PIC  X(055).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊　　手形期日　抽出　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-ME3   PIC  X(019) VALUE
                  "***  SM ｻｲﾄ ﾅｼ  ***".
             03  E-KEY   PIC  X(004).
             03  E-STAT  PIC  X(002).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "238" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "34" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "34" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "34" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "34" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "34" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "34" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "34" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "67" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "67" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "10" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "10" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "10" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "4" "E-ME3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE S-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           MOVE W-FID2 TO WK0064ID.
           MOVE WK0064ID TO SAITO-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
      *
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
               USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" SAITO-F_PNAME1 " " BY REFERENCE SAITO-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
       M-25.
           MOVE ST-TCD TO W-TCD.
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
               USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  S-STS = ZERO
               CALL "C3_Set_Jrcode" USING 
               USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE ZERO TO W-D.
           COMPUTE W-STS = S-STS + 20.
           DIVIDE 30 INTO W-STS GIVING W-TTS REMAINDER W-THN.
       M-30.
           MOVE ST-FDG TO W-FGET.
           MOVE ST-MKG TO W-MGET.
       M-40.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  ST-TCD NOT = W-TCD
               GO TO M-45
           END-IF
           IF  W-TMC = 1
               GO TO M-40
           END-IF
           IF  ST-FDG NOT = W-FGET
               GO TO M-30
           END-IF
           IF  ST-MKG NOT = W-MGET
               MOVE 1 TO W-TMC
           END-IF
           GO TO M-40.
       M-45.
           PERFORM S-05 THRU S-15.
           GO TO M-25.
       M-80.
           PERFORM S-05 THRU S-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE SAITO-F_IDLST SAITO-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO SAITO-R.
           MOVE W-TCD TO SAITO-TCD.
           MOVE W-TTS TO SAITO-TTS.
           MOVE W-THN TO SAITO-THN.
           MOVE W-TMC TO SAITO-TMC.
      *           WRITE SAITO-R.
      *//////////////
           CALL "DB_Insert" USING
            SAITO-F_PNAME1 SAITO-F_LNAME SAITO-R RETURNING RET.
       S-15.
           EXIT.
