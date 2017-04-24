       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS27U.
      *********************************************************
      *    PROGRAM         :  受注ＥＯＳ受信集計ワーク２作成  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0256".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0512".
           02  W-FID22        PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(002).
           COPY LSTAT.
      *
      *FD  JKEIF
       01  JKEIF_JHS27U.
           02  JKEIF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JKEIF_LNAME    PIC  X(012) VALUE "JKEIF_JHS27U".
           02  F              PIC  X(001).
           02  JKEIF_KEY1     PIC  X(100) VALUE SPACE.
           02  JKEIF_SORT     PIC  X(100) VALUE SPACE.
           02  JKEIF_IDLST    PIC  X(100) VALUE SPACE.
           02  JKEIF_RES      USAGE  POINTER.
       01  JKEI-R.
           02  JKEI-HCD       PIC  9(006).
           02  JKEI-ASUD.
             03  JKEI-ASU   OCCURS   4.
               04  JKEI-SUD   OCCURS  10.
                 05  JKEI-SU  PIC  9(005).
           02  F              PIC  X(049).
           02  JKEI-SIGN      PIC  9(001).
       77  F                  PIC  X(001).
      *FD  WJKEIF
       01  WJKEIF_JHS27U.
           02  WJKEIF_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  WJKEIF_LNAME   PIC  X(013) VALUE "WJKEIF_JHS27U".
           02  F              PIC  X(001).
           02  WJKEIF_KEY1    PIC  X(100) VALUE SPACE.
           02  WJKEIF_SORT    PIC  X(100) VALUE SPACE.
           02  WJKEIF_IDLST   PIC  X(100) VALUE SPACE.
           02  WJKEIF_RES     USAGE  POINTER.
       01  WJKEI-R.
           02  WJKEI-HCD      PIC  9(006).
           02  WJKEI-ASUD.
             03  WJKEI-ASU   OCCURS   4.
               04  WJKEI-SUD   OCCURS  10.
                 05  WJKEI-SU PIC  9(005).
           02  WJKEI-KEI      PIC  9(006).
           02  WJKEI-ZC.
             03  WJKEI-ZC1    PIC  9(001).
             03  WJKEI-ZC2    PIC  9(001).
             03  WJKEI-ZC3    PIC  9(001).
             03  WJKEI-ZC4    PIC  9(001).
           02  F              PIC  X(039).
           02  WJKEI-SIGN     PIC  9(001).
           02  F              PIC  X(256).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　受注ＥＯＳ受信集計ワーク　作成　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
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
            "C-MID" " " "0" "0" "50" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "25" "50" " " "C-MID" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "17" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22.
           MOVE W-FID1 TO WK0256ID.
           MOVE WK0256ID TO JKEIF_PNAME1.
           MOVE W-FID2 TO WK0512ID.
           MOVE WK0512ID TO WJKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JKEIF_PNAME1 " " BY REFERENCE JKEIF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" WJKEIF_PNAME1 " " BY REFERENCE WJKEIF_IDLST "0".
      *
      *           READ JKEIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JKEIF_PNAME1 BY REFERENCE JKEI-R " " RETURNING RET.
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
           MOVE ZERO TO WJKEI-R.
           MOVE JKEI-HCD TO WJKEI-HCD.
           MOVE JKEI-SIGN TO WJKEI-SIGN.
       M-15.
           MOVE ZERO TO CNT.
       M-20.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-25
           END-IF
           ADD JKEI-SU(1,CNT) TO WJKEI-SU(1,CNT).
           ADD JKEI-SU(2,CNT) TO WJKEI-SU(2,CNT).
           ADD JKEI-SU(3,CNT) TO WJKEI-SU(3,CNT).
           ADD JKEI-SU(4,CNT) TO WJKEI-SU(4,CNT).
           COMPUTE WJKEI-KEI = JKEI-SU(1,CNT) + JKEI-SU(2,CNT) +
                           JKEI-SU(3,CNT) + JKEI-SU(4,CNT) + WJKEI-KEI.
           IF  JKEI-SU(1,CNT) NOT = ZERO
               IF  WJKEI-ZC1 = 0
                   MOVE 1 TO WJKEI-ZC1
               END-IF
           END-IF
           IF  JKEI-SU(2,CNT) NOT = ZERO
               IF  WJKEI-ZC2 = 0
                   MOVE 1 TO WJKEI-ZC2
               END-IF
           END-IF
           IF  JKEI-SU(3,CNT) NOT = ZERO
               IF  WJKEI-ZC3 = 0
                   MOVE 1 TO WJKEI-ZC3
               END-IF
           END-IF
           IF  JKEI-SU(4,CNT) NOT = ZERO
               IF  WJKEI-ZC4 = 0
                   MOVE 1 TO WJKEI-ZC4
               END-IF
           END-IF
           GO TO M-20.
       M-25.
      *           READ JKEIF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JKEIF_PNAME1 BY REFERENCE JKEI-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  JKEI-HCD = WJKEI-HCD
               GO TO M-15
           END-IF
      *           WRITE WJKEI-R.
      *//////////////
           CALL "DB_Insert" USING
            WJKEIF_PNAME1 WJKEIF_LNAME WJKEI-R RETURNING RET.
           GO TO M-10.
       M-80.
      *           WRITE WJKEI-R.
      *//////////////
           CALL "DB_Insert" USING
            WJKEIF_PNAME1 WJKEIF_LNAME WJKEI-R RETURNING RET.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE WJKEIF_IDLST WJKEIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JKEIF_IDLST JKEIF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
