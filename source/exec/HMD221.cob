       IDENTIFICATION   DIVISION.
       PROGRAM-ID. HMD221.
      *********************************************************
      *    PROGRAM         :  ＳＴＲＡＮ　印字区分　復旧      *
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
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-DNOD         PIC  9(006).
           02  W-DHC          PIC  9(001).
           02  CHK            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
      *FD  S-TRAN
       01  S-TRAN_HMD221.
           02  S-TRAN_PNAME1  PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013) VALUE "S-TRAN_HMD221".
           02  F              PIC  X(001).
           02  S-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  S-R.
           02  S-R1.
             03  S1-DNO       PIC  9(006).
             03  S1-GNO       PIC  9(001).
             03  S1-DATE      PIC  9(008).
             03  S1-TCD       PIC  9(004).
             03  S1-HCD       PIC  9(006).
             03  S1-SIZ       PIC  9(001).
             03  S1-ASU.
               04  S1-SUD   OCCURS 10.
                 05  S1-SU    PIC S9(004)   COMP-3.
             03  S1-SUT       PIC S9(005).
             03  S1-T         PIC  9(005).
             03  S1-KIN       PIC S9(008).
             03  S1-CSC       PIC  9(001).
             03  S1-DC        PIC  9(001).
             03  S1-FT        PIC  9(005).
             03  S1-CCD       PIC  9(003).
             03  F            PIC  X(011).
             03  S1-HSC       PIC  9(001).
             03  S1-KOSU      PIC  9(003).
             03  S1-FRC       PIC  9(001).
             03  S1-TCD2      PIC  9(004).
             03  S1-BIK       PIC  X(010).
             03  F            PIC  X(012).
             03  S1-DHC       PIC  9(001).
             03  S1-UNC       PIC  9(001).
           02  S-R2     REDEFINES S-R1.
             03  S2-DNO       PIC  9(006).
             03  S2-GNO       PIC  9(001).
             03  S2-DATE      PIC  9(008).
             03  S2-TCD       PIC  9(004).
             03  S2-BI        PIC  N(024).
             03  S2-HNO       PIC  9(006).
             03  F            PIC  X(030).
             03  S2-TAX       PIC S9(007).
             03  S2-SHZZ      PIC S9(007).
             03  S2-UZ        PIC S9(009).
             03  S2-DHC       PIC  9(001).
             03  S2-UNC       PIC  9(001).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　売上・値引データ同期合わせ　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "344" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "2" "15" "46" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "3" "15" "46" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "4" "15" "46" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "5" "15" "46" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "6" "15" "46" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "7" "15" "46" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "8" "15" "46" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "22" "35" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "52" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "35" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "35" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF.
       M-15.
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
      *
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF.
       M-25.
           MOVE S1-DNO TO W-DNO.
           MOVE S1-DHC TO W-DHC.
       M-30.
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  S1-DNO = W-DNO
               IF  S1-DHC = W-DHC
                   GO TO M-30
               ELSE
                   GO TO M-35
               END-IF
           END-IF
           GO TO M-25.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           IF  CHK NOT = 0
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ADD 1 TO CHK.
           CALL "DB_F_Open" USING
            "I-O" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
       M-40.
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1
               GO TO M-15
           END-IF
           IF  S1-DNO NOT = W-DNO
               GO TO M-40
           END-IF
           IF  S1-DHC = 1
               MOVE 0 TO S1-DHC
      *               REWRITE S-R.
      *///////////////
               CALL "DB_Update" USING
                S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET
           END-IF
           GO TO M-40.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           IF  W-DNO = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
