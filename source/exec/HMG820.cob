       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG820.
      *********************************************************
      *    履物在庫表ワーク　作成　（親コード"－"分のみ）     *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
       77  W-FILE             PIC  X(013).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0064".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0512".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(002).
           02  W-C            PIC  9(001).
           02  W-ASD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  9(001).
           02  W-ZSD.
             03  W-ZS    OCCURS  10  PIC S9(006).
           02  W-D.
             03  W-MHCD       PIC  9(006).
             03  W-HCD        PIC  9(006).
             03  W-ASUD.
               04  W-ASU   OCCURS  4.
                 05  W-SUD   OCCURS  10.
                   06  W-SU   PIC S9(006).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHHTF.
      *FD  CODEF
       01  CODEF_HMG820.
           02  CODEF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  CODEF_LNAME    PIC  X(012) VALUE "CODEF_HMG820".
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
      *FD  ZCMF
       01  ZCMF_HMG820.
           02  ZCMF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  ZCMF_LNAME     PIC  X(011) VALUE "ZCMF_HMG820".
           02  F              PIC  X(001).
           02  ZCMF_KEY1      PIC  X(100) VALUE SPACE.
           02  ZCMF_SORT      PIC  X(100) VALUE SPACE.
           02  ZCMF_IDLST     PIC  X(100) VALUE SPACE.
           02  ZCMF_RES       USAGE  POINTER.
       01  ZCM-R.
           02  ZCM-KEY.
             03  ZCM-MHCD     PIC  9(006).
             03  ZCM-HCD      PIC  9(006).
             03  ZCM-SIZ      PIC  9(001).
           02  ZCM-AZS.
             03  ZCM-ZSD   OCCURS  10.
               04  ZCM-ZS     PIC S9(006).
           02  F              PIC  X(439).
       77  F                  PIC  X(001).
      *FD  ZCOF
       01  ZCOF_HMG820.
           02  ZCOF_PNAME1    PIC  X(004) VALUE "ZCOF".
           02  F              PIC  X(001).
           02  ZCOF_LNAME     PIC  X(011) VALUE "ZCOF_HMG820".
           02  F              PIC  X(001).
           02  ZCOF_KEY1      PIC  X(100) VALUE SPACE.
           02  ZCOF_SORT      PIC  X(100) VALUE SPACE.
           02  ZCOF_IDLST     PIC  X(100) VALUE SPACE.
           02  ZCOF_RES       USAGE  POINTER.
       01  ZCO-R.
           02  ZCO-KEY.
             03  ZCO-MHCD     PIC  9(006).
             03  ZCO-HCD      PIC  9(006).
             03  ZCO-SIZ      PIC  9(001).
           02  ZCO-AZS.
             03  ZCO-ZSD   OCCURS  10.
               04  ZCO-ZS     PIC S9(006).
           02  F              PIC  X(012).
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
                "＊＊＊　　履物在庫表ワーク　作成　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　（親コード”－”分のみ）　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  ZCOF WRITE ｴﾗｰ  ***".
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
            "C-ERR" " " "0" "0" "0" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "0" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "0" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "0" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "0" "15" "24" "E-ME2" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0064ID.
           MOVE WK0064ID TO CODEF_PNAME1.
           MOVE W-FID2 TO WK0512ID.
           MOVE WK0512ID TO ZCMF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" ZCMF_PNAME1 " " BY REFERENCE ZCMF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" ZCOF_PNAME1 " " BY REFERENCE ZCOF_IDLST "1"
            "ZCO-KEY" BY REFERENCE ZCO-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 " " BY REFERENCE CODEF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HHTF_PNAME1 "SHARED" BY REFERENCE HHTF_IDLST "2"
            "HHT-KEY" BY REFERENCE HHT-KEY "HHT-KEY2" BY REFERENCE
            HHT-KEY2.
       M-10.
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  CODE-CHK = 1
               GO TO M-10
           END-IF.
       M-15.
           PERFORM RED-RTN THRU RED-EX.
           IF  W-HCD NOT = ZERO
               PERFORM WRI-RTN THRU WRI-EX
           END-IF.
       M-20.
      *           READ CODEF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  CODE-CHK = 1
               IF  W-ASD = ZERO
                   GO TO M-20
               END-IF
           END-IF
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE HHTF_IDLST HHTF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZCMF_IDLST ZCMF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZCOF_IDLST ZCOF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       RED-RTN.
           IF  CODE-CHK = 0
               MOVE ZERO TO W-ASD
           END-IF
           MOVE ZERO TO W-D.
      *
           MOVE SPACE TO HHT-KEY.
           MOVE CODE-MHCD TO HHT-MHCD.
           MOVE CODE-HCD TO HHT-HCD.
      *           START HHTF KEY NOT < HHT-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HHTF_PNAME1 "HHT-KEY" " NOT < " HHT-KEY RETURNING RET.
           IF  RET = 1
               GO TO RED-EX
           END-IF
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO RED-EX
           END-IF
           IF (CODE-MHCD NOT = HHT-MHCD) OR (CODE-HCD NOT = HHT-HCD)
               GO TO RED-EX
           END-IF
      *
           MOVE HHT-MHCD TO W-MHCD.
           MOVE HHT-HCD TO W-HCD.
       RED-010.
           PERFORM SET-RTN THRU SET-EX.
      *
      *           READ HHTF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HHTF_PNAME1 BY REFERENCE HHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO RED-EX
           END-IF
           IF (HHT-MHCD = CODE-MHCD) AND (HHT-HCD = CODE-HCD)
               GO TO RED-010
           END-IF.
       RED-EX.
           EXIT.
       SET-RTN.
           MOVE ZERO TO W-ZSD.
           MOVE ZERO TO CNT.
       SET-010.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO SET-020
           END-IF
           IF  HHT-SIZ = 4
               IF  CNT = 10
                   GO TO SET-010
               END-IF
           END-IF
           IF  CODE-CHK = 1
               IF  W-SIZ(HHT-SIZ,CNT) = 0
                   GO TO SET-010
               END-IF
           END-IF
           COMPUTE W-ZS(CNT) = HHT-ZSU(CNT) + HHT-NSU(CNT)
                             - HHT-USU(CNT) - HHT-ASS(CNT).
           IF  CODE-CHK = 0
               IF  W-ZS(CNT) < ZERO
                   MOVE 1 TO W-SIZ(HHT-SIZ,CNT)
               ELSE
                   MOVE ZERO TO W-ZS(CNT)
               END-IF
           END-IF
           GO TO SET-010.
       SET-020.
           MOVE W-ZSD TO W-ASU(HHT-SIZ).
       SET-EX.
           EXIT.
       WRI-RTN.
           MOVE ZERO TO W-C.
       WRI-010.
           ADD 1 TO W-C.
           IF  W-C = 5
               GO TO WRI-EX
           END-IF
           IF  0 = W-SIZ(W-C,01) AND W-SIZ(W-C,02) AND W-SIZ(W-C,03) AND
                  W-SIZ(W-C,04) AND W-SIZ(W-C,05) AND W-SIZ(W-C,06) AND
                  W-SIZ(W-C,07) AND W-SIZ(W-C,08) AND W-SIZ(W-C,09) AND
                  W-SIZ(W-C,10)
               GO TO WRI-010
           END-IF
           IF  ZERO = W-SU(W-C,01) AND W-SU(W-C,02) AND W-SU(W-C,03) AND
                     W-SU(W-C,04) AND W-SU(W-C,05) AND W-SU(W-C,06) AND
                     W-SU(W-C,07) AND W-SU(W-C,08) AND W-SU(W-C,09) AND
                     W-SU(W-C,10)
               GO TO WRI-010
           END-IF
      *
           IF  W-MHCD = W-HCD
               MOVE ZERO TO ZCM-R
               MOVE W-MHCD TO ZCM-MHCD
               MOVE W-HCD TO ZCM-HCD
               MOVE W-C TO ZCM-SIZ
               MOVE W-ASU(W-C) TO ZCM-AZS
      *               WRITE ZCM-R
      *//////////////
               CALL "DB_Insert" USING
                ZCMF_PNAME1 ZCMF_LNAME ZCM-R RETURNING RET
               GO TO WRI-010
           END-IF.
       WRI-020.
           MOVE ZERO TO ZCO-R.
           MOVE W-MHCD TO ZCO-MHCD.
           MOVE W-HCD TO ZCO-HCD.
           MOVE W-C TO ZCO-SIZ.
           MOVE W-ASU(W-C) TO ZCO-AZS.
      *           WRITE ZCO-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            ZCOF_PNAME1 ZCOF_LNAME ZCO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-030
           END-IF
           GO TO WRI-010.
       WRI-030.
           IF  ERR-STAT NOT = "24"
               MOVE 1 TO W-END
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRI-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE ZCOF_IDLST ZCOF_PNAME1.
           MOVE "ZCOF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" ZCOF_PNAME1 " " BY REFERENCE ZCOF_IDLST "1"
            "ZCO-KEY" BY REFERENCE ZCO-KEY.
           GO TO WRI-020.
       WRI-EX.
           EXIT.
