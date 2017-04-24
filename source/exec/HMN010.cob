       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN010.
      *********************************************************
      *    PROGRAM         :  履物棚卸ファイル　クリア        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-MSG              PIC  X(040).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIHIM.
           COPY LIHTIM.
      *FD  HTIW-M
       01  HTIW-M_HMN010.
           02  HTIW-M_PNAME1  PIC  X(005) VALUE "HTIMW".
           02  F              PIC  X(001).
           02  HTIW-M_LNAME   PIC  X(013) VALUE "HTIW-M_PNAME1".
           02  F              PIC  X(001).
           02  HTIW-M_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-M_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-M_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-M_RES     USAGE  POINTER.
       01  HTIW-R.
           02  HTIW-KEY.
             03  HTIW-DNO.
               04  HTIW-DNO1  PIC  9(005).
               04  HTIW-DNO2  PIC  X(001).
             03  HTIW-GNO     PIC  9(001).
           02  F              PIC  X(078).
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
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　履物　棚卸ファイル　クリア　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(040).
             03  E-FILE  PIC  X(013).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "262" " " " " RETURNING RESU.
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
           "06C-MID" "X" "20" "40" "22" "05C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "20" "57" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "53" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-ME" BY REFERENCE W-MSG "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-FILE" "X" "24" "60" "13" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-FILE" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               MOVE SPACE TO W-MSG
               MOVE "***  ｷｬﾝｾﾙ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "OUTPUT" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HTIW-M_PNAME1 " " BY REFERENCE HTIW-M_IDLST "1"
            "HTIW-KEY" BY REFERENCE HTIW-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIW-M_IDLST HTIW-M_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-15.
      *           READ HI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HI-M_PNAME1 BY REFERENCE HI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE ZERO TO HI-KT.
      *           REWRITE HI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HI-M_PNAME1 HI-M_LNAME HI-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-FILE W-MSG
               MOVE "HIM" TO W-FILE
               MOVE "***  REWRITE ｴﾗｰ  ***" TO W-MSG
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-FILE" E-FILE "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
