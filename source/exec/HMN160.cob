       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN150.
      *********************************************************
      *    PROGRAM         :  棚卸Ｗチェックファイル　集計    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :                                  *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
      *FD  HTWCF
       01  HTWCF_HMN160.
           02  HTWCF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTWCF_LNAME    PIC  X(012) VALUE "HTWCF_HMN160".
           02  F              PIC  X(001).
           02  HTWCF_KEY1     PIC  X(100) VALUE SPACE.
           02  HTWCF_SORT     PIC  X(100) VALUE SPACE.
           02  HTWCF_IDLST    PIC  X(100) VALUE SPACE.
           02  HTWCF_RES      USAGE  POINTER.
       01  HTWC-R.
           02  HTWC-KEY.
             03  HTWC-HCD     PIC  9(006).                              品名ｺｰﾄﾞ
             03  HTWC-SIZ     PIC  9(001).
           02  HTWC-ASUD.                                               棚卸数
             03  HTWC-ASU     PIC S9(006)  OCCURS  10.
           02  HTWC-BSUD.                                               棚卸数
             03  HTWC-BSU     PIC S9(006)  OCCURS  10.
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  HTWCW
       01  HTWCW_HMN160.
           02  HTWCW_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTWCW_LNAME    PIC  X(012) VALUE "HTWCW_HMN160".
           02  F              PIC  X(001).
           02  HTWCW_KEY1     PIC  X(100) VALUE SPACE.
           02  HTWCW_SORT     PIC  X(100) VALUE SPACE.
           02  HTWCW_IDLST    PIC  X(100) VALUE SPACE.
           02  HTWCW_RES      USAGE  POINTER.
       01  HTWCW-R.
           02  HTWCW-KEY.
             03  HTWCW-HCD    PIC  9(006).                              品名ｺｰﾄﾞ
             03  HTWCW-SIZ    PIC  9(001).
           02  HTWCW-ASUD.                                              棚卸数
             03  HTWCW-ASU    PIC S9(006)  OCCURS  10.
           02  HTWCW-BSUD.                                              棚卸数
             03  HTWCW-BSU    PIC S9(006)  OCCURS  10.
           02  F              PIC  X(001).
           02  F              PIC  X(128).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　棚卸Ｗチェックファイル　集計　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "336" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "79" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "79" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " "  RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO HTWCF_PNAME1.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0128ID TO HTWCW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HTWCF_PNAME1 " " BY REFERENCE HTWCF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HTWCW_PNAME1 " " BY REFERENCE HTWCW_IDLST "0".
      *           READ HTWCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTWCF_PNAME1 BY REFERENCE HTWC-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-10.
           INITIALIZE HTWCW-R.
           MOVE HTWC-KEY TO HTWCW-KEY.
           MOVE ZERO TO HTWCW-ASUD HTWCW-BSUD.
       M-15.
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               ADD HTWC-ASU(CNT) TO HTWCW-ASU(CNT)
               ADD HTWC-BSU(CNT) TO HTWCW-BSU(CNT)
               GO TO M-20
           END-IF.
       M-25.
      *           READ HTWCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTWCF_PNAME1 BY REFERENCE HTWC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  HTWC-KEY = HTWCW-KEY
               GO TO M-15
           END-IF
      *
      *           WRITE HTWCW-R.
      *//////////////
           CALL "DB_Insert" USING
            HTWCW_PNAME1 HTWCW_LNAME HTWCW-R RETURNING RET.
           GO TO M-10.
       M-30.
      *           WRITE HTWCW-R.
      *//////////////
           CALL "DB_Insert" USING
            HTWCW_PNAME1 HTWCW_LNAME HTWCW-R RETURNING RET.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HTWCF_IDLST HTWCF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTWCW_IDLST HTWCW_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
