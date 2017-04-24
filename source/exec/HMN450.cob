       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN450.
      *********************************************************
      *    PROGRAM         :  履物棚卸入力ワーク　作成　　　　*
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
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHTIM.
      *FD  HTIW-F
       01  HTIW-F_HMN450.
           02  HTIW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIW-F_LNAME   PIC  X(013) VALUE "HTIW-F_HMN450".
           02  F              PIC  X(001).
           02  HTIW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-F_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-F_RES     USAGE  POINTER.
       01  HTIW-R.
           02  F              PIC  X(007).
           02  HTIW-SOC       PIC  9(001).
           02  HTIW-HCD       PIC  9(006).
           02  HTIW-SIZ       PIC  9(001).
           02  HTIW-SUD.
             03  HTIW-SU      PIC S9(006)  OCCURS  10.
           02  HTIW-BC.
             03  HTIW-BC1     PIC  9(002).
             03  HTIW-BC2     PIC  9(002).
             03  HTIW-BC3     PIC  9(002).
           02  F              PIC  X(175).
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
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　履物棚卸入力ワーク　作成　　＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "28" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID22.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0256ID TO HTIW-F_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HTIW-F_PNAME1 " " BY REFERENCE HTIW-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
       M-10.
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           MOVE ZERO TO HTIW-R.
           MOVE HTI-SNO TO HTIW-SOC.
           MOVE HTI-HCD TO HTIW-HCD.
           MOVE HTI-SIZ TO HTIW-SIZ.
           IF  HTI-GNO > 4
               MOVE HTI-SUD TO HTIW-SUD
           ELSE
               COMPUTE HTIW-SU(01) = HTI-SU(01) * HTI-ISU
               COMPUTE HTIW-SU(02) = HTI-SU(02) * HTI-ISU
               COMPUTE HTIW-SU(03) = HTI-SU(03) * HTI-ISU
               COMPUTE HTIW-SU(04) = HTI-SU(04) * HTI-ISU
               COMPUTE HTIW-SU(05) = HTI-SU(05) * HTI-ISU
               COMPUTE HTIW-SU(06) = HTI-SU(06) * HTI-ISU
               COMPUTE HTIW-SU(07) = HTI-SU(07) * HTI-ISU
               COMPUTE HTIW-SU(08) = HTI-SU(08) * HTI-ISU
               COMPUTE HTIW-SU(09) = HTI-SU(09) * HTI-ISU
               COMPUTE HTIW-SU(10) = HTI-SU(10) * HTI-ISU
           END-IF
           MOVE HTI-BC TO HTIW-BC.
      *           WRITE HTIW-R.
      *///////////////
           CALL "DB_Insert" USING
            HTIW-F_PNAME1 HTIW-F_LNAME HTIW-R RETURNING RET.
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIW-F_IDLST HTIW-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
