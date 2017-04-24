       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN150.
      *********************************************************
      *    PROGRAM         :  棚卸Ｗチェックファイル１作成    *
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
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHTIM.
      *FD  HTIMW
       01  HTIMW_HMN150.
           02  HTIMW_PNAME1   PIC  X(005) VALUE "HTIMW".
           02  F              PIC  X(001).
           02  HTIMW_LNAME    PIC  X(012) VALUE "HTIMW_HMN150".
           02  F              PIC  X(001).
           02  HTIMW_KEY1     PIC  X(100) VALUE SPACE.
           02  HTIMW_SORT     PIC  X(100) VALUE SPACE.
           02  HTIMW_IDLST    PIC  X(100) VALUE SPACE.
           02  HTIMW_RES      USAGE  POINTER.
       01  HTIW-R.
           02  HTIW-KEY.
             03  HTIW-DNO.                                              伝票№
               04  HTIW-DNO1  PIC  9(005).                              伝票№
               04  HTIW-DNO2  PIC  X(001).
             03  HTIW-GNO     PIC  9(001).                              行№
           02  HTIW-SNO       PIC  9(001).
           02  HTIW-HCD       PIC  9(006).                              品名ｺｰﾄﾞ
           02  HTIW-SIZ       PIC  9(001).
           02  HTIW-SUD.                                                棚卸数
             03  HTIW-SU      PIC S9(006)  OCCURS  10.
           02  HTIW-BC.
             03  HTIW-BC1     PIC  9(002).
             03  HTIW-BC2     PIC  9(002).
             03  HTIW-BC3     PIC  9(002).
           02  HTIW-ISU       PIC  9(003).
           02  F              PIC  X(001).
       77  F                  PIC  X(001).
      *FD  HTWCF
       01  HTWCF_HMN150.
           02  HTWCF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTWCF_LNAME    PIC  X(012) VALUE "HTWCF_HMN150".
           02  F              PIC  X(001).
           02  HTWCF_KEY1     PIC  X(100) VALUE SPACE.
           02  HTWCF_SORT     PIC  X(100) VALUE SPACE.
           02  HTWCF_IDLST    PIC  X(100) VALUE SPACE.
           02  HTWCF_RES      USAGE  POINTER.
       01  HTWC-R.
           02  HTWC-HCD       PIC  9(006).                              品名ｺｰﾄﾞ
           02  HTWC-SIZ       PIC  9(001).
           02  HTWC-ASUD.                                               棚卸数
             03  HTWC-ASU     PIC S9(006)  OCCURS  10.
           02  HTWC-BSUD.                                               棚卸数
             03  HTWC-BSU     PIC S9(006)  OCCURS  10.
           02  F              PIC  X(001).
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
                "＊＊＊　　棚卸Ｗチェックファイル１作成　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(019) VALUE
                  "***  ｲﾘｽｳ ZERO  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-HTI   PIC  9(006).
             03  E-HTIW  PIC  9(006).
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
            "C-ERR" " " "0" "0" "93" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "93" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "19" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HTI" "9" "24" "45" "6" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-HTI" BY REFERENCE HTI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HTIW" "9" "24" "45" "6" "E-HTI" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-HTIW" BY REFERENCE HTIW-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-HTIW" " "  RETURNING RESU.
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
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO HTWCF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" HTWCF_PNAME1 " " BY REFERENCE HTWCF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HTIMW_PNAME1 "SHARED" BY REFERENCE HTIMW_IDLST "1"
            "HTIW-KEY" BY REFERENCE HTIW-KEY.
       M-10.
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           IF  HTI-SUD = ZERO
               GO TO M-10
           END-IF
           IF  HTI-SNO NOT = 6
               GO TO M-10
           END-IF
           IF  HTI-BC3 = 30
               GO TO M-10
           END-IF
      *
           MOVE ZERO TO HTWC-R.
           MOVE HTI-HCD TO HTWC-HCD.
           MOVE HTI-SIZ TO HTWC-SIZ.
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-25
           END-IF
           IF  HTI-SU(CNT) NOT = ZERO
               IF  HTI-GNO > 4
                   MOVE HTI-SU(CNT) TO HTWC-ASU(CNT)
               ELSE
                   COMPUTE HTWC-ASU(CNT) = HTI-SU(CNT) * HTI-ISU
                   IF  HTI-ISU = ZERO
                       CALL "SD_Output" USING
                        "E-ME1" E-ME1 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-HTI" E-HTI "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-CL" E-CL "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           GO TO M-20.
       M-25.
      *           WRITE HTWC-R.
      *//////////////
           CALL "DB_Insert" USING
            HTWCF_PNAME1 HTWCF_LNAME HTWC-R RETURNING RET.
           GO TO M-10.
       M-30.
      *           READ HTIMW NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTIMW_PNAME1 BY REFERENCE HTIW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  HTIW-SUD = ZERO
               GO TO M-30
           END-IF
      *
           MOVE ZERO TO HTWC-R.
           MOVE HTIW-HCD TO HTWC-HCD.
           MOVE HTIW-SIZ TO HTWC-SIZ.
           MOVE ZERO TO CNT.
       M-40.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-45
           END-IF
           IF  HTIW-SU(CNT) NOT = ZERO
               IF  HTIW-GNO > 4
                   MOVE HTIW-SU(CNT) TO HTWC-BSU(CNT)
               ELSE
                   COMPUTE HTWC-BSU(CNT) = HTIW-SU(CNT) * HTIW-ISU
                   IF  HTIW-ISU = ZERO
                       CALL "SD_Output" USING
                        "E-ME1" E-ME1 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-HTIW" E-HTIW "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-CL" E-CL "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           GO TO M-40.
       M-45.
      *           WRITE HTWC-R.
      *//////////////
           CALL "DB_Insert" USING
            HTWCF_PNAME1 HTWCF_LNAME HTWC-R RETURNING RET.
           GO TO M-30.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIMW_IDLST HTIMW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTWCF_IDLST HTWCF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
