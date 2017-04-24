       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSR010.
      **************************************
      *****    領収書ファイル　作成    *****
      **************************************
       AUTHOR. S-NAKAO.
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
       01  W-TBLD.
           02  W-AMTB.
             03  F          PIC  X(045) VALUE
                  "100000001050000001028000001018000001008030000".
             03  F          PIC  X(045) VALUE
                  "004000001002000001001000001000050000000000001".
           02  W-MTBD  REDEFINES W-AMTB.
             03  W-MTB   OCCURS 10  PIC  9(009).
           02  W-ARTB.
             03  F          PIC  X(050) VALUE
                  "20000100000600004000020000100000600004000020000000".
           02  W-RTBD  REDEFINES W-ARTB.
             03  W-RTB   OCCURS 10  PIC  9(005).
           02  W-AKTB.
             03  F          PIC  X(045) VALUE
                  "100000000050000000030000000020000000010000000".
             03  F          PIC  X(045) VALUE
                  "005000000003000000002000000001000000000049999".
           02  W-KTBD  REDEFINES W-AKTB.
             03  W-KTB   OCCURS 10  PIC  9(009).
       01  W-D.
           02  W-C          PIC  9(001).
           02  CNT          PIC  9(002).
           02  W-GKIN       PIC S9(009).
           02  W-KIN        PIC S9(009).
           02  CHK          PIC  9(001).
           02  W-SEQ        PIC  9(003).
       01  ERR-STAT         PIC  X(002).
      *
      *FD  TK-F
       01  TK-F_TSR010.
           02  TK-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F            PIC  X(001).
           02  TK-F_LNAME   PIC  X(011) VALUE "TK-F_TSR010".
           02  F            PIC  X(001).
           02  TK-F_KEY1    PIC  X(100) VALUE SPACE.
           02  TK-F_SORT    PIC  X(100) VALUE SPACE.
           02  TK-F_IDLST   PIC  X(100) VALUE SPACE.
           02  TK-F_RES     USAGE  POINTER.
       01  TK-R.
           02  TK-DATE      PIC  9(006).
           02  TK-TCD       PIC  9(004).
           02  TK-KIN       PIC S9(009).
           02  TK-SOC       PIC  9(001).
           02  TK-SHC       PIC  9(001).
           02  F            PIC  X(043).
       77  F                PIC  X(001).
      *FD  RS-F
       01  RS-F_TSR010.
           02  RS-F_PNAME1  PIC  X(003) VALUE "RSF".
           02  F            PIC  X(001).
           02  RS-F_LNAME   PIC  X(011) VALUE "RS-F_TSR010".
           02  F            PIC  X(001).
           02  RS-F_KEY1    PIC  X(100) VALUE SPACE.
           02  RS-F_SORT    PIC  X(100) VALUE SPACE.
           02  RS-F_IDLST   PIC  X(100) VALUE SPACE.
           02  RS-F_RES     USAGE  POINTER.
       01  RS-R.
           02  F            PIC  X(012).
           02  RS-DATE      PIC  9(006).
           02  RS-TCD       PIC  9(004).
           02  RS-KIN       PIC  9(009).
           02  RS-INS       PIC  9(005).
           02  RS-SOC       PIC  9(001).
           02  RS-SHC       PIC  9(001).
           02  F            PIC  X(023).
           02  RS-SEQ       PIC  9(003).
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
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　領収書ファイル　作成　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
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
            "C-MID" " " "0" "0" "280" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "33" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
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
           MOVE WK0064ID TO TK-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TK-F_PNAME1 " " BY REFERENCE TK-F_IDLST "0".
       M-10.
      *           READ TK-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TK-F_PNAME1 BY REFERENCE TK-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TK-F_IDLST TK-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  TK-KIN = ZERO OR < 0
               GO TO M-10
           END-IF
           MOVE ZERO TO W-SEQ.
           CALL "DB_F_Open" USING
            "OUTPUT" RS-F_PNAME1 " " BY REFERENCE RS-F_IDLST "0".
       M-15.
           MOVE ZERO TO CNT W-C CHK W-GKIN.
           MOVE TK-KIN TO W-KIN.
           IF  TK-SOC = 5 OR 8
               GO TO M-35
           END-IF
           IF  TK-TCD = 4745
               GO TO M-25
           END-IF
           IF  TK-KIN > 15000000
               MOVE 2 TO CHK
           ELSE
               MOVE 1 TO CHK
           END-IF.
       M-20.
           ADD 1 TO CNT.
           IF  CNT = 11
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-MTB(CNT) > W-KIN
               GO TO M-20
           END-IF
           IF  W-KTB(CNT) < W-KIN
               MOVE W-KTB(CNT) TO W-KIN
           END-IF
           GO TO M-35.
       M-25.
           MOVE 11 TO CNT.
       M-30.
           SUBTRACT 1 FROM CNT.
           IF  CNT = 0
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  W-KTB(CNT) < W-KIN
               GO TO M-30
           END-IF.
       M-35.
           INITIALIZE RS-R.
           MOVE TK-DATE TO RS-DATE.
           MOVE TK-TCD TO RS-TCD.
           MOVE W-KIN TO RS-KIN.
           IF  TK-SOC = 5 OR 8
               MOVE TK-SOC TO RS-SOC
           ELSE
               MOVE W-RTB(CNT) TO RS-INS
           END-IF
           MOVE TK-SHC TO RS-SHC.
           ADD 1 TO W-SEQ.
           MOVE W-SEQ TO RS-SEQ.
      *           WRITE RS-R.
      *//////////////
           CALL "DB_Insert" USING
            RS-F_PNAME1 RS-F_LNAME RS-R RETURNING RET.
           ADD W-KIN TO W-GKIN.
           ADD 1 TO W-C.
           IF  W-GKIN = TK-KIN
               GO TO M-40
           END-IF
           MOVE ZERO TO CNT.
           COMPUTE W-KIN = TK-KIN - W-GKIN.
           IF  W-C = CHK
               GO TO M-25
           END-IF
           GO TO M-20.
       M-40.
      *           READ TK-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" TK-F_PNAME1 BY REFERENCE TK-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  TK-KIN = ZERO OR < 0
               GO TO M-40
           END-IF
           GO TO M-15.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE RS-F_IDLST RS-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TK-F_IDLST TK-F_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
