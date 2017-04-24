       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN650.
      *********************************************************
      *    PROGRAM         :  棚卸データ抽出                  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  W-FID3.
           02  W-FID31        PIC  X(006) VALUE "WK0256".
           02  W-FID32        PIC  X(003).
       01  W-DATA.
           02  W-HCD          PIC  9(006).
           02  W-DMM          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *FD  CODEF
       01  CODEF_HMN650.
           02  CODEF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  CODEF_LNAME    PIC  X(012) VALUE "CODEF_HMN650".
           02  F              PIC  X(001).
           02  CODEF_KEY1     PIC  X(100) VALUE SPACE.
           02  CODEF_SORT     PIC  X(100) VALUE SPACE.
           02  CODEF_IDLST    PIC  X(100) VALUE SPACE.
           02  CODEF_RES      USAGE  POINTER.
       01  CODE-R.
           02  CODE-MHCD      PIC  9(006).
           02  CODE-HCD       PIC  9(006).
           02  CODE-CHK       PIC  9(001).
           02  F              PIC  X(051).
       77  F                  PIC  X(001).
      *FD  HTI-M
       01  HTI-M_HMN650.
           02  HTI-M_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTI-M_LNAME    PIC  X(012) VALUE "HTI-M_HMN650".
           02  F              PIC  X(001).
           02  HTI-M_KEY1     PIC  X(100) VALUE SPACE.
           02  HTI-M_SORT     PIC  X(100) VALUE SPACE.
           02  HTI-M_IDLST    PIC  X(100) VALUE SPACE.
           02  HTI-M_RES      USAGE  POINTER.
       01  HTI-R.
           02  HTI-KEY.
             03  HTI-DNO.
               04  HTI-DNO1   PIC  9(005).
               04  HTI-DNO2   PIC  X(001).
             03  HTI-GNO      PIC  9(001).
           02  HTI-SNO        PIC  9(001).
           02  HTI-HCD        PIC  9(006).
           02  HTI-SIZ        PIC  9(001).
           02  HTI-SUD.
             03  HTI-SU       PIC S9(006)  OCCURS  10.
           02  HTI-BC.
             03  HTI-BC1      PIC  9(002).
             03  HTI-BC2      PIC  9(002).
             03  HTI-BC3      PIC  9(002).
           02  HTI-ISU        PIC  9(003).
           02  F              PIC  X(172).
       77  F                  PIC  X(001).
      *FD  HTIW-F
       01  HTIW-F_HMN650.
           02  HTIW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIW-F_LNAME   PIC  X(013) VALUE "HTIW-M_HMN650".
           02  F              PIC  X(001).
           02  HTIW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-F_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-F_RES     USAGE  POINTER.
       01  HTIW-R.
           02  HTIW-KEY.
             03  HTIW-DNO.
               04  HTIW-DNO1  PIC  9(005).
               04  HTIW-DNO2  PIC  X(001).
             03  HTIW-GNO     PIC  9(001).
           02  HTIW-SNO       PIC  9(001).
           02  HTIW-HCD       PIC  9(006).
           02  HTIW-SIZ       PIC  9(001).
           02  HTIW-SUD.
             03  HTIW-SU      PIC S9(006)  OCCURS  10.
           02  HTIW-BC.
             03  HTIW-BC1     PIC  9(002).
             03  HTIW-BC2     PIC  9(002).
             03  HTIW-BC3     PIC  9(002).
           02  HTIW-ISU       PIC  9(003).
           02  HTIW-MHCD      PIC  9(006).
           02  F              PIC  X(038).
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
                "＊＊＊　　棚卸データ抽出（親子コード）　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
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
           "C-MID" " " "0" "0" "230" " " " " RETURNING RESU.
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
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "96" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "96" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME78" "N" "24" "5" "4" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22 W-FID32.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO CODEF_PNAME1.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO HTIW-F_PNAME1.
           MOVE W-FID3 TO WK0256ID.
           MOVE WK0256ID TO HTI-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 " " BY REFERENCE CODEF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HTI-M_PNAME1 " " BY REFERENCE HTI-M_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" HTIW-F_PNAME1 " " BY REFERENCE HTIW-F_IDLST "0".
      *
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
       M-20.
      *           READ HTI-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HTI-M_PNAME1 BY REFERENCE HTI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF.
       M-25.
           IF HTI-HCD < CODE-HCD
               GO TO M-20
           END-IF
           IF HTI-HCD = CODE-HCD
               GO TO M-30
           END-IF
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           GO TO M-25.
       M-30.
           INITIALIZE HTIW-R.
           MOVE HTI-R TO HTIW-R.
           MOVE CODE-MHCD TO HTIW-MHCD.
      *           WRITE HTIW-R.
      *///////////////
           CALL "DB_Insert" USING
            HTIW-F_PNAME1 HTIW-F_LNAME HTIW-R RETURNING RET.
           GO TO M-20.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIW-F_IDLST HTIW-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
