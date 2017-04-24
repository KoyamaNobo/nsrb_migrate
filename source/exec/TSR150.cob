       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR150.
      ************************************************
      *****     領収書ファイル　累積・クリア     *****
      ************************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-DATA.
           02  W-CHK        PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  W-FILE       PIC  X(013).
       01  ERR-STAT         PIC  X(002).
      *
      *FD  RSR-F
       01  RSR-F_TSR150.
           02  RSR-F_PNAME1 PIC  X(004) VALUE "RSRF".
           02  F            PIC  X(001).
           02  RSR-F_LNAME  PIC  X(012) VALUE "RSR-F_TSR150".
           02  F            PIC  X(001).
           02  RSR-F_KEY1   PIC  X(100) VALUE SPACE.
           02  RSR-F_SORT   PIC  X(100) VALUE SPACE.
           02  RSR-F_IDLST  PIC  X(100) VALUE SPACE.
           02  RSR-F_RES    USAGE  POINTER.
       01  RSR-R            PIC  X(064).
       77  F                PIC  X(001).
      *FD  RS-F
       01  RS-F_TSR150.
           02  RS-F_PNAME1  PIC  X(003) VALUE "RSF".
           02  F            PIC  X(001).
           02  RS-F_LNAME   PIC  X(011) VALUE "RS-F_TSR150".
           02  F            PIC  X(001).
           02  RS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  RS-F_SORT    PIC  X(100) VALUE SPACE.
           02  RS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  RS-F_RES     USAGE  POINTER.
       01  RS-R             PIC  X(064).
       77  F                PIC  X(001).
      *FD  TNK-F
       01  TNK-F_TSR150.
           02  TNK-F_PNAME1 PIC  X(004) VALUE "TNKF".
           02  F            PIC  X(001).
           02  TNK-F_LNAME  PIC  X(012) VALUE "TNK-F_TSR150".
           02  F            PIC  X(001).
           02  TNK-F_KEY1   PIC  X(100) VALUE SPACE.
           02  TNK-F_SORT   PIC  X(100) VALUE SPACE.
           02  TNK-F_IDLST  PIC  X(100) VALUE SPACE.
           02  TNK-F_RES    USAGE  POINTER.
       01  TNK-R            PIC  X(032).
       77  F                PIC  X(001).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　領収書ファイル　累積・クリア　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(024) VALUE
                  "***  RSRF WRITE ｴﾗｰ  ***".
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                    "オーバーフロー、領域を拡張し、ＦＮＣ＋再開".
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
            "C-MID" " " "0" "0" "336" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "175" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "175" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "24" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "21" "42" "01E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
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
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" RSR-F_PNAME1 " " BY REFERENCE RSR-F_IDLST "0".
       M-10.
      *           READ RS-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RS-F_PNAME1 BY REFERENCE RS-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF.
       M-15.
           MOVE RS-R TO RSR-R.
      *           WRITE RSR-R.
      *//////////////
           CALL "DB_Insert" USING
            RSR-F_PNAME1 RSR-F_LNAME RSR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE RSR-F_IDLST RSR-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE RS-F_IDLST RS-F_PNAME1
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE RSR-F_IDLST RSR-F_PNAME1.
           MOVE "RSRF         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" RSR-F_PNAME1 " " BY REFERENCE RSR-F_IDLST "0".
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE RSR-F_IDLST RSR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
      *
           CALL "DB_F_Open" USING
            "OUTPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" TNK-F_PNAME1 " " BY REFERENCE TNK-F_IDLST "0".
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TNK-F_IDLST TNK-F_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
