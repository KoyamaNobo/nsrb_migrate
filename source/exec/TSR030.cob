       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR030.
      ******************************************************
      *****     領収書　変換　（ＴＤＴＭ→ＴＮＫＦ）   *****
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           COPY LSTAT.
      *
      *FD  TDT-M
       01  TDT-M_TSR030.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_TSR030".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-TKC.
               04  TD-C1      PIC  9(001).
               04  TD-C2      PIC  9(001).
             03  TD-TNO       PIC  9(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DATE        PIC  9(006).
           02  TD-MND         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BKC         PIC  9(004).
           02  TD-FRN         PIC  N(024).
           02  TD-SAD.
             03  TD-S     OCCURS   7  PIC S9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN        PIC  9(004).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PC          PIC  9(001).
           02  TD-RSC         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  TNK-F
       01  TNK-F_TSR030.
           02  TNK-F_PNAME1   PIC  X(004) VALUE "TNKF".
           02  F              PIC  X(001).
           02  TNK-F_LNAME    PIC  X(012) VALUE "TNK-F_TSR030".
           02  F              PIC  X(001).
           02  TNK-F_KEY1     PIC  X(100) VALUE SPACE.
           02  TNK-F_SORT     PIC  X(100) VALUE SPACE.
           02  TNK-F_IDLST    PIC  X(100) VALUE SPACE.
           02  TNK-F_RES      USAGE  POINTER.
       01  TNK-R.
           02  TN-DATE        PIC  9(006).
           02  TN-TCD         PIC  9(004).
           02  TN-KIN         PIC S9(009).
           02  TN-SOC         PIC  9(001).
           02  TN-SHC         PIC  9(001).
           02  F              PIC  X(011).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　領収書　変換　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　（ＴＤＴＭ　→　ＴＮＫＦ）　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(024) VALUE
                  "***  TNKF WRITE ｴﾗｰ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  TDTM REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(006).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "316" " " " " RETURNING RESU.
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
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "23" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "56" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "56" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "24" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "50" "6" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TD-KEY "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           CALL "DB_F_Open" USING "I-O SEQUENTIAL" TDT-M_PNAME1 
            " " BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" TNK-F_PNAME1 " " BY REFERENCE TNK-F_IDLST "0".
       M-30.
      *           READ TDT-M AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TDT-M_PNAME1 BY REFERENCE TDT-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TD-C1 NOT = 0 AND 1                                       ﾘｮｳｼｭｳｼｮ
               GO TO M-30
           END-IF
           IF  TD-RSC = 9
               GO TO M-30
           END-IF
           IF  TD-HCR = 1
               GO TO M-30
           END-IF.
       M-35.
           INITIALIZE TNK-R.
           MOVE TD-DATE TO TN-DATE.
           MOVE TD-TCD TO TN-TCD.
           MOVE TD-KIN TO TN-KIN.
           IF  TD-TKC = 03 OR 04
               MOVE 5 TO TN-SOC.
           MOVE TD-RSC TO TN-SHC.
      *           WRITE TNK-R.
      *//////////////
           CALL "DB_Insert" USING
            TNK-F_PNAME1 TNK-F_LNAME TNK-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-40
           END-IF
      *
           MOVE 1 TO TD-HCR.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-30.
       M-40.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TNK-F_IDLST TNK-F_PNAME1.
           MOVE "TNKF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" TNK-F_PNAME1 " " BY REFERENCE TNK-F_IDLST "0".
           GO TO M-35.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TNK-F_IDLST TNK-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
