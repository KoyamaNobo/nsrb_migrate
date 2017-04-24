       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG025.
      *********************************************************
      *    PROGRAM         :  品種別製品受払エクセル変換      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/06                        *
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
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0256".
           02  W-FID12        PIC  X(003).
       01  WS-D.
           02  WS-ZS          PIC S9(007)V9(02).
           02  WS-ZK          PIC S9(008).
           02  WS-KS          PIC S9(007)V9(02).
           02  WS-KK          PIC S9(009).
           02  WS-US          PIC S9(007)V9(02).
           02  WS-UK          PIC S9(009).
           02  WS-YS          PIC S9(007)V9(02).
           02  WS-YK          PIC S9(008).
           02  WS-UKIN        PIC S9(009).
           02  WS-AR          PIC S9(009).
       01  WT-D.
           02  WT-ZK          PIC S9(008).
           02  WT-KK          PIC S9(009).
           02  WT-UK          PIC S9(009).
           02  WT-YK          PIC S9(008).
           02  WT-UKIN        PIC S9(009).
           02  WT-AR          PIC S9(009).
       01  WA-D.
           02  WA-ZK          PIC S9(008).
           02  WA-KK          PIC S9(009).
           02  WA-UK          PIC S9(009).
           02  WA-YK          PIC S9(008).
           02  WA-UKIN        PIC S9(009).
           02  WA-AR          PIC S9(009).
       01  W-DATA.
           02  W-D.
             03  W-KS         PIC S9(007)V9(02).
             03  W-YS         PIC S9(007)V9(02).
             03  W-YK         PIC S9(008).
             03  W-UKIN       PIC S9(008).
             03  W-AR         PIC S9(008).
           02  W-YC           PIC  9(002).
           02  W-NC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIKHM.
           COPY LSKHTM.
      *FD  EXL-F
       01  EXL-F_HKG025.
           02  EXL-F_PNAME1   PIC  X(009) VALUE "WK0512000".
           02  F              PIC  X(001).
           02  EXL-F_LNAME    PIC  X(012) VALUE "EXL-F_HKG025".
           02  F              PIC  X(001).
           02  EXL-F_KEY1     PIC  X(100) VALUE SPACE.
           02  EXL-F_SORT     PIC  X(100) VALUE SPACE.
           02  EXL-F_IDLST    PIC  X(100) VALUE SPACE.
           02  EXL-F_RES      USAGE  POINTER.
       01  EXL-R.
           02  EXL-YC         PIC  9(002).
           02  EXL-HCD        PIC  X(005).
           02  EXL-NAME       PIC  N(020).
           02  EXL-T          PIC  9(006)V9(02).
           02  EXL-ZS         PIC S9(007)V9(02).
           02  EXL-ZK         PIC S9(009).
           02  EXL-KS         PIC S9(007)V9(02).
           02  EXL-KK         PIC S9(009).
           02  EXL-US         PIC S9(007)V9(02).
           02  EXL-UK         PIC S9(009).
           02  EXL-YS         PIC S9(007)V9(02).
           02  EXL-YK         PIC S9(009).
           02  EXL-UKIN       PIC S9(009).
           02  EXL-AR         PIC S9(009).
           02  F              PIC  X(367).
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
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　工品製品受払表　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(017) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME2   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-KEY   PIC  X(005).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "238" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "34" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "34" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "34" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "34" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "34" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "34" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "34" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "31" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "16" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "35" "5" "E-ME2" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12.
           MOVE W-FID1 TO WK0256ID.
           MOVE WK0256ID TO KHT-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KHT-M_PNAME1 " " BY REFERENCE KHT-M_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" EXL-F_PNAME1 " " BY REFERENCE EXL-F_IDLST "0".
           MOVE ZERO TO WA-D.
       M-10.
      *           READ KHT-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" KHT-M_PNAME1 BY REFERENCE KHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-10
           END-IF
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  KHT-YC NOT = 10 AND 11
               IF  KH-GT1 = ZERO
                   MOVE ZERO TO KHT-SSU
               END-IF
           END-IF
           COMPUTE W-UKIN = KHT-UKIN - KHT-NKIN.
           COMPUTE W-AR = W-UKIN - KHT-GKIN.
           IF  KHT-NC = 9
               MOVE ZERO TO KHT-ZSU KHT-ZKIN
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-ZKIN AND KHT-KSU AND KHT-HSU
                 AND KHT-ISU AND KHT-KKIN AND KHT-SSU AND KHT-GKIN
                 AND W-UKIN AND W-AR
               GO TO M-10
           END-IF.
       M-15.
           MOVE KHT-YC TO W-YC.
           MOVE ZERO TO WT-D CHK.
       M-20.
           MOVE KHT-NC TO W-NC.
           MOVE ZERO TO WS-D CNT.
       M-25.
           COMPUTE W-KS = KHT-KSU - KHT-HSU + KHT-ISU.
           IF  KHT-NC = 9
               MOVE ZERO TO W-YS W-YK
           ELSE
               COMPUTE W-YS = KHT-ZSU - KHT-SSU + W-KS
               COMPUTE W-YK = W-YS * KH-GT1
           END-IF
      *
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-NAME.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-YC TO EXL-YC
           END-IF
           MOVE KHT-KEY TO EXL-HCD.
           MOVE KH-NAME TO EXL-NAME.
           IF  KH-GT1 NOT = ZERO
               MOVE KH-GT1 TO EXL-T
           END-IF
           MOVE KHT-ZSU TO EXL-ZS.
           MOVE KHT-ZKIN TO EXL-ZK.
           MOVE W-KS TO EXL-KS.
           MOVE KHT-KKIN TO EXL-KK.
           MOVE KHT-SSU TO EXL-US.
           MOVE KHT-GKIN TO EXL-UK.
           MOVE W-YS TO EXL-YS.
           MOVE W-YK TO EXL-YK.
           MOVE W-UKIN TO EXL-UKIN.
           MOVE W-AR TO EXL-AR.
      *           WRITE EXL-R.
      *///////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       M-35.
           ADD KHT-ZSU TO WS-ZS.
           ADD KHT-ZKIN TO WS-ZK.
           ADD W-KS TO WS-KS.
           ADD KHT-KKIN TO WS-KK.
           ADD KHT-SSU TO WS-US.
           ADD KHT-GKIN TO WS-UK.
           ADD W-YS TO WS-YS.
           ADD W-YK TO WS-YK.
           ADD W-UKIN TO WS-UKIN.
           ADD W-AR TO WS-AR.
           IF  CNT = 1
               MOVE 2 TO CNT
           END-IF
           IF  CNT = 0
               MOVE 1 TO CNT
           END-IF.
       M-40.
      *           READ KHT-M AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" KHT-M_PNAME1 BY REFERENCE KHT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  KHT-YC = ZERO
               GO TO M-40
           END-IF
           MOVE KHT-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  KHT-YC NOT = 10 AND 11
               IF  KH-GT1 = ZERO
                   MOVE ZERO TO KHT-SSU
               END-IF
           END-IF
           COMPUTE W-UKIN = KHT-UKIN - KHT-NKIN.
           COMPUTE W-AR = W-UKIN - KHT-GKIN.
           IF  KHT-NC = 9
               MOVE ZERO TO KHT-ZSU KHT-ZKIN
           END-IF
           IF  ZERO = KHT-ZSU AND KHT-ZKIN AND KHT-KSU AND KHT-HSU
                 AND KHT-ISU AND KHT-KKIN AND KHT-SSU AND KHT-GKIN
                 AND W-UKIN AND W-AR
               GO TO M-40
           END-IF
           IF  KHT-YC NOT = W-YC
               GO TO M-45
           END-IF
           IF  KHT-NC = W-NC
               GO TO M-25
           END-IF
           PERFORM TPR-RTN THRU TPR-EX.
           GO TO M-20.
       M-45.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           GO TO M-15.
       M-90.
           PERFORM TPR-RTN THRU TPR-EX.
           PERFORM SPR-RTN THRU SPR-EX.
           PERFORM APR-RTN THRU APR-EX.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE EXL-F_IDLST EXL-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       TPR-RTN.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-NAME.
           MOVE "　　　　　　　　　　（　小計　）　" TO EXL-NAME.
           MOVE WS-ZS TO EXL-ZS.
           MOVE WS-ZK TO EXL-ZK.
           MOVE WS-KS TO EXL-KS.
           MOVE WS-KK TO EXL-KK.
           MOVE WS-US TO EXL-US.
           MOVE WS-UK TO EXL-UK.
           MOVE WS-YS TO EXL-YS.
           MOVE WS-YK TO EXL-YK.
           MOVE WS-UKIN TO EXL-UKIN.
           MOVE WS-AR TO EXL-AR.
      *           WRITE EXL-R.
      *///////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
      *
           ADD WS-ZK TO WT-ZK.
           ADD WS-KK TO WT-KK.
           ADD WS-UK TO WT-UK.
           ADD WS-YK TO WT-YK.
           ADD WS-UKIN TO WT-UKIN.
           ADD WS-AR TO WT-AR.
       TPR-EX.
           EXIT.
       SPR-RTN.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-NAME.
           MOVE "　　　　　　　［　合　計　］　　" TO EXL-NAME.
           MOVE WT-ZK TO EXL-ZK.
           MOVE WT-KK TO EXL-KK.
           MOVE WT-UK TO EXL-UK.
           MOVE WT-YK TO EXL-YK.
           MOVE WT-UKIN TO EXL-UKIN.
           MOVE WT-AR TO EXL-AR.
      *           WRITE EXL-R.
      *///////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
      *
           ADD WT-ZK TO WA-ZK.
           ADD WT-KK TO WA-KK.
           ADD WT-UK TO WA-UK.
           ADD WT-YK TO WA-YK.
           ADD WT-UKIN TO WA-UKIN.
           ADD WT-AR TO WA-AR.
       SPR-EX.
           EXIT.
       APR-RTN.
           INITIALIZE EXL-R.
           MOVE SPACE TO EXL-NAME.
           MOVE "　　【　総　合　計　】　" TO EXL-NAME.
           MOVE WA-ZK TO EXL-ZK.
           MOVE WA-KK TO EXL-KK.
           MOVE WA-UK TO EXL-UK.
           MOVE WA-YK TO EXL-YK.
           MOVE WA-UKIN TO EXL-UKIN.
           MOVE WA-AR TO EXL-AR.
      *           WRITE EXL-R.
      *///////////////
           CALL "DB_Insert" USING
            EXL-F_PNAME1 EXL-F_LNAME EXL-R RETURNING RET.
       APR-EX.
           EXIT.
