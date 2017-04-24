       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM112.
      *********************************************************
      *    PROGRAM         :  履物品名マスター　予定原価セット*
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT           PIC  X(002).
       01  W-D.
           02  W-SEN          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-FILE         PIC  X(013).
           COPY LSTAT.
      *
           COPY LIHIM.
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　履物品名マスタ　予定原価クリア　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  N(004) VALUE
                  "予定原価".
             03  FILLER  PIC  X(005) VALUE
                  "=1 , ".
             03  FILLER  PIC  N(005) VALUE
                  "予定原価②".
             03  FILLER  PIC  X(005) VALUE
                  "=2 , ".
             03  FILLER  PIC  N(005) VALUE
                  "予定原価③".
             03  FILLER  PIC  X(009) VALUE
                  "=3   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(025) VALUE
                  "***  HIM REWRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "119" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "12" "0" "47" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "N" "12" "16" "8" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "X" "12" "24" "5" "0102C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302C-MID" "N" "12" "29" "10" "0202C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402C-MID" "X" "12" "39" "5" "0302C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502C-MID" "N" "12" "44" "10" "0402C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602C-MID" "X" "12" "54" "9" "0502C-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "22" "30" "22" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "12" "58" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "47" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "25" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "25" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SEN < 1 OR > 3
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-20.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-SEN = 1
               MOVE ZERO TO HI-YG
           END-IF
           IF  W-SEN = 2
               MOVE ZERO TO HI-YG2
           END-IF
           IF  W-SEN = 3
               MOVE ZERO TO HI-YG3
           END-IF
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
