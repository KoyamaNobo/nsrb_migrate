       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD030.
      *********************************************************
      *    PROGRAM         :  出荷ファイル生成                *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/14                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE ZERO.
       01  ERR-STAT           PIC  X(002).
      *
      *FD  S-TRAN
       01  S-TRAN_HMD030.
           02  S-TRAN_PNAME1  PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013) VALUE "S-TRAN_HMD030".
           02  F              PIC  X(001).
           02  S-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  ST-R.
           02  ST-KEY         PIC  9(007).
           02  ST-KEYD  REDEFINES ST-KEY.
             03  ST-DNO       PIC  9(006).
             03  ST-GNO       PIC  9(001).
           02  F              PIC  X(121).
       77  F                  PIC  X(001).
      *FD  STRANYR
       01  STRANYR_HMD030.
           02  STRANYR_PNAME1 PIC  X(007) VALUE "STRANYR".
           02  F              PIC  X(001).
           02  STRANYR_LNAME  PIC  X(014) VALUE "STRANYR_HMD030".
           02  F              PIC  X(001).
           02  STRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  STRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  STRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  STRANYR_RES    USAGE  POINTER.
       01  STRANYR-R          PIC  X(128).
       77  F                  PIC  X(001).
      *FD  R-STRANYR
       01  R-STRANYR_HMD030.
           02  R-STRANYR_PNAME1 PIC  X(011) VALUE "STRANYR-RDB".
           02  F                PIC  X(001).
           02  R-STRANYR_LNAME  PIC  X(016) VALUE "R-STRANYR_HMD030".
           02  F                PIC  X(001).
           02  R-STRANYR_KEY1   PIC  X(100) VALUE SPACE.
           02  R-STRANYR_SORT   PIC  X(100) VALUE SPACE.
           02  R-STRANYR_IDLST  PIC  X(100) VALUE SPACE.
           02  R-STRANYR_RES    USAGE  POINTER.
       01  RSTRANYR-R         PIC  X(128).
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
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　出荷ファイル累積　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(016) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME8   PIC  X(027) VALUE
                  "***  STRANYR WRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(029) VALUE
                  "***  R-STRANYR WRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  9(007).
           COPY LSSEM.
       PROCEDURE   DIVISION.
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
            "C-MID" " " "0" "0" "224" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "32" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "32" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "32" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "32" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "32" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "32" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "32" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "63" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "27" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "29" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "50" "7" "E-ME11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE ST-KEY "7" "0"  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" STRANYR_PNAME1 " " BY REFERENCE STRANYR_IDLST "0".
           CALL "DB_F_Open" USING
            "EXTEND" R-STRANYR_PNAME1 " " BY REFERENCE
            R-STRANYR_IDLST "0".
       M-10.
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE ST-R " " RETURNING RET.
           IF  RET = 1
              GO TO M-95
           END-IF
      *
           PERFORM STYR-RTN THRU STYR-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
           IF  ST-GNO > 6
               GO TO M-10
           END-IF
      *
           PERFORM RSTYR-RTN THRU RSTYR-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
      *
           GO TO M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE R-STRANYR_IDLST R-STRANYR_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       STYR-RTN.
           INITIALIZE STRANYR-R.
           MOVE ST-R TO STRANYR-R.
      *           WRITE STRANYR-R.
      *//////////////
           CALL "DB_Insert" USING
            STRANYR_PNAME1 STRANYR_LNAME STRANYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO STYR-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO STYR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE STRANYR_IDLST STRANYR_PNAME1.
           MOVE "STRANYR      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" STRANYR_PNAME1 " " BY REFERENCE STRANYR_IDLST "0".
           GO TO STYR-RTN.
       STYR-EX.
           EXIT.
       RSTYR-RTN.
           INITIALIZE RSTRANYR-R.
           MOVE ST-R TO RSTRANYR-R.
      *           WRITE RSTRANYR-R.
      *//////////////
           CALL "DB_Insert" USING
            R-STRANYR_PNAME1 R-STRANYR_LNAME RSTRANYR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO RSTYR-EX
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME11" E-ME11 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-END
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO RSTYR-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE R-STRANYR_IDLST R-STRANYR_PNAME1.
           MOVE "R-STRANYR    " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" R-STRANYR_PNAME1 " " BY REFERENCE
            R-STRANYR_IDLST "0".
           GO TO RSTYR-RTN.
       RSTYR-EX.
           EXIT.
