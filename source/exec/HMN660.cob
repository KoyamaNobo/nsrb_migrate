       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN660.
      *********************************************************
      *    PROGRAM         :  履物棚卸在庫表ワーク　作成　　　*
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0128".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-AC           PIC  9(001).
           02  W-ZCD.
             03  W-ZC    OCCURS   4  PIC  9(001).
           02  W-ZSD.
             03  W-ZS    OCCURS  10  PIC S9(006).
           02  W-D.
             03  W-HCD        PIC  9(006).
             03  W-ASUD.
               04  W-ASU   OCCURS  4.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(006).
             03  W-BC1        PIC  9(002).
             03  W-BC2        PIC  9(002).
             03  W-BC3        PIC  9(002).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHHTF.
      *FD  CODEF
       01  CODEF_HMN660.
           02  CODEF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  CODEF_LNAME    PIC  X(012) VALUE "CODEF_HMN660".
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
      *FD  HTIW-F
       01  HTIW-F_HMN660.
           02  HTIW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIW-F_LNAME   PIC  X(013) VALUE "HTIW-F_HMN660".
           02  F              PIC  X(001).
           02  HTIW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  HTIW-F_SORT    PIC  X(100) VALUE SPACE.
           02  HTIW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  HTIW-F_RES     USAGE  POINTER.
       01  HTIW-R.
           02  F              PIC  X(006).
           02  HTIW-GNO       PIC  9(001).
           02  HTIW-SOC       PIC  9(001).
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
           02  HTIW-DC        PIC  9(001).
           02  F              PIC  X(037).
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
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　帳簿在庫抽出（親子コード）　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "10" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "10" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "10" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "10" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "10" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "10" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "10" "42" "06C-MID" " " RETURNING RESU.
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
       M-00.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO CODEF_PNAME1.
           MOVE W-FID2 TO WK0128ID.
           MOVE WK0128ID TO HTIW-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 " " BY REFERENCE CODEF_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" HTIW-F_PNAME1 " " BY REFERENCE HTIW-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
       M-05.
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
      *
           MOVE SPACE TO HHT-KEY.
           MOVE CODE-MHCD TO HHT-MHCD.
           MOVE CODE-HCD TO HHT-HCD.
      *           START HHTF KEY NOT < HHT-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY2" "NOT <" HHT-KEY2 RETURNING RET.
           IF  RET = 1
               GO TO M-05
           END-IF
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-05
           END-IF
           IF (CODE-MHCD NOT = HHT-MHCD) OR (CODE-HCD NOT = HHT-HCD)
               GO TO M-05
           END-IF.
       M-10.
           MOVE ZERO TO W-D W-ZCD.
           MOVE HHT-BC1 TO W-BC1.
           MOVE HHT-BC2 TO W-BC2.
           MOVE HHT-BC3 TO W-BC3.
       M-15.
           PERFORM S-05 THRU S-35.
       M-20.
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF (CODE-MHCD NOT = HHT-MHCD) OR (CODE-HCD NOT = HHT-HCD)
               GO TO M-45
           END-IF
           GO TO M-15.
       M-45.
           PERFORM S-40 THRU S-50.
           GO TO M-05.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HTIW-F_IDLST HTIW-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE ZERO TO W-ZSD.
           MOVE ZERO TO CNT CHK.
       S-10.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO S-30
           END-IF
           IF  HHT-SIZ = 4
               IF  CNT = 10
                   GO TO S-10
               END-IF
           END-IF
           COMPUTE W-ZS(CNT) = HHT-ZSU(CNT) + HHT-NSU(CNT)
                             - HHT-USU(CNT) - HHT-ASS(CNT).
           IF  CHK = 0
               IF  W-ZS(CNT) NOT = ZERO
                   MOVE 1 TO CHK
               END-IF
           END-IF
           GO TO S-10.
       S-30.
           IF  CHK NOT = 0
               MOVE CHK TO W-ZC(HHT-SIZ)
               MOVE W-ZSD TO W-ASU(HHT-SIZ)
           END-IF.
       S-35.
           EXIT.
       S-40.
           MOVE ZERO TO CHK W-C.
       S-45.
           ADD 1 TO W-C.
           IF  W-C = 5
               GO TO S-50
           END-IF
           IF  W-ZC(W-C) = 0
               GO TO S-45
           END-IF
      *
           MOVE ZERO TO HTIW-R.
           IF  W-C = 1
               MOVE 8 TO HTIW-GNO
           END-IF
           IF  W-C = 2
               MOVE 5 TO HTIW-GNO
           END-IF
           IF  W-C = 3
               MOVE 6 TO HTIW-GNO
           END-IF
           IF  W-C = 4
               MOVE 7 TO HTIW-GNO
           END-IF
           MOVE 9 TO HTIW-SOC.
           MOVE CODE-MHCD TO HTIW-MHCD.
           MOVE CODE-HCD TO HTIW-HCD.
           MOVE W-C TO HTIW-SIZ.
           MOVE W-ASU(W-C) TO HTIW-SUD.
           MOVE W-BC1 TO HTIW-BC1.
           MOVE W-BC2 TO HTIW-BC2.
           MOVE W-BC3 TO HTIW-BC3.
           MOVE 1 TO HTIW-DC.
      *           WRITE HTIW-R.
      *///////////////
           CALL "DB_Insert" USING
            HTIW-F_PNAME1 HTIW-F_LNAME HTIW-R RETURNING RET.
           GO TO S-45.
       S-50.
           EXIT.
