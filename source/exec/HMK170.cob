       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMK170.
       DATE-WRITTEN. 1996-01-19.
      *********************************************************
      *    PROGRAM         :  教育振興会会費請求ワーク作成２  *
      *    PRINTER TYPE    :  JIPS*                           *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  CNT            PIC  9(004).
           02  CHK            PIC  9(001).
           02  WT-D.
             03  WT-KIN       PIC S9(010).
             03  WT-KHG       PIC S9(009).
           02 WN-D.
             03  WN-KHG       PIC S9(009).
             03  WN-KHGC      PIC S9(009).
       01  ERR-STAT           PIC  X(002).
      *FD  KSKS-F
       01  KSKS-F_HMK170.
           02  KSKS-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KSKS-F_LNAME   PIC  X(013) VALUE "KSKS-F_HMK170".
           02  F              PIC  X(001).
           02  KSKS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KSKS-F_SORT    PIC  X(100) VALUE SPACE.
           02  KSKS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KSKS-F_RES     USAGE  POINTER.
       01  KSKS-R.
           02  KSKS-NC        PIC  9(001).
           02  KSKS-HCD       PIC  9(004).
           02  KSKS-SU        PIC S9(006).
           02  KSKS-T         PIC S9(005).
           02  KSKS-KIN       PIC S9(008).
           02  KSKS-SKK       PIC S9(007)V9(02).
           02  KSKS-SK        PIC S9(007).
           02  F              PIC  X(024).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　教育振興会会費請求ワーク作成２　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
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
            "C-MID" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
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
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KSKS-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KSKS-F_PNAME1 " " BY REFERENCE KSKS-F_IDLST "0".
           MOVE ZERO TO CNT WT-D WN-D CHK.
       M-10.
      *           READ KSKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KSKS-F_PNAME1 BY REFERENCE KSKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           ADD 1 TO CNT.
           ADD KSKS-KIN TO WT-KIN.
           ADD KSKS-SK TO WN-KHG.
           GO TO M-10.
       M-15.
           CALL "DB_F_Close" USING
            BY REFERENCE KSKS-F_IDLST KSKS-F_PNAME1.
           COMPUTE WT-KHG ROUNDED = WT-KIN * 0.03.
           IF  WN-KHG = WT-KHG
               GO TO M-95
           END-IF
           PERFORM S-05 THRU S-15.
           CALL "DB_F_Open" USING
            "I-O" KSKS-F_PNAME1 " " BY REFERENCE KSKS-F_IDLST "0".
       M-20.
      *           READ KSKS-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KSKS-F_PNAME1 BY REFERENCE KSKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  CHK NOT = 0
               GO TO M-25
           END-IF
           IF  COMPLETION_CODE = 000
               ADD 1 TO KSKS-SK WN-KHG
           END-IF
           IF  COMPLETION_CODE = 100
               SUBTRACT 1 FROM KSKS-SK WN-KHG
           END-IF
      *           REWRITE KSKS-R.
      *///////////////
           CALL "DB_Update" USING
            KSKS-F_PNAME1 KSKS-F_LNAME KSKS-R RETURNING RET.
           IF  COMPLETION_CODE = 000
               IF  WN-KHG < WT-KHG
                   GO TO M-20
               END-IF
           END-IF
           IF  COMPLETION_CODE = 100
               IF  WN-KHG > WT-KHG
                   GO TO M-20
               END-IF
           END-IF
           GO TO M-90.
       M-25.
           IF  COMPLETION_CODE = 000
               SUBTRACT 1 FROM KSKS-SK WN-KHG
           END-IF
           IF  COMPLETION_CODE = 100
               ADD 1 TO KSKS-SK WN-KHG
           END-IF
      *           REWRITE KSKS-R.
      *///////////////
           CALL "DB_Update" USING
            KSKS-F_PNAME1 KSKS-F_LNAME KSKS-R RETURNING RET.
           IF  COMPLETION_CODE = 000
               IF  WN-KHG > WT-KHG
                   GO TO M-20
               END-IF
           END-IF
           IF  COMPLETION_CODE = 100
               IF  WN-KHG < WT-KHG
                   GO TO M-20
               END-IF
           END-IF.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE KSKS-F_IDLST KSKS-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           IF  COMPLETION_CODE = 000
               IF  WN-KHG > WT-KHG
                   GO TO S-10
               END-IF
           END-IF
           IF  COMPLETION_CODE = 100
               IF  WN-KHG < WT-KHG
                   GO TO S-10
               END-IF
           END-IF
           IF  COMPLETION_CODE = 000
               COMPUTE WN-KHGC = WN-KHG + CNT
           ELSE
               COMPUTE WN-KHGC = WN-KHG - CNT
           END-IF
           IF  COMPLETION_CODE = 000
               IF  WN-KHGC < WT-KHG
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           IF  COMPLETION_CODE = 100
               IF  WN-KHGC > WT-KHG
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           GO TO S-15.
       S-10.
           MOVE 1 TO CHK.
           IF  COMPLETION_CODE = 000
               COMPUTE WN-KHGC = WN-KHG - CNT
           ELSE
               COMPUTE WN-KHGC = WN-KHG + CNT
           END-IF
           IF  COMPLETION_CODE = 000
               IF  WN-KHGC > WT-KHG
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           IF  COMPLETION_CODE = 100
               IF  WN-KHGC < WT-KHG
                   CALL "SD_Output" USING
                    "E-ME1" E-ME1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF.
       S-15.
           EXIT.
