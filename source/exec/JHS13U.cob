       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS13U.
      ************************************************
      *****    ＴＤＮＮＦ　訂正更新（ナフコ）    *****
      ************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  CHK            PIC  9(001).
      *
       COPY LITDNN.
      *    80/3
      *FD  NAFCOD
       01  NAFCOD_JHS13U.
           02  NAFCOD_PNAME1  PIC  X(006) VALUE "NAFCOD".
           02  F              PIC  X(001).
           02  NAFCOD_LNAME   PIC  X(013) VALUE "NAFCOD_JHS13U".
           02  F              PIC  X(001).
           02  NAFCOD_KEY1    PIC  X(100) VALUE SPACE.
           02  NAFCOD_SORT    PIC  X(100) VALUE SPACE.
           02  NAFCOD_IDLST   PIC  X(100) VALUE SPACE.
           02  NAFCOD_RES     USAGE  POINTER.
       01  NAFCO-R.
           02  NAFCO-KEY.
             03  NAFCO-STC.
               04  NAFCO-SCD  PIC  9(002).
               04  NAFCO-TCD  PIC  9(003).
               04  F          PIC  X(004).
             03  NAFCO-DNO.
               04  F          PIC  X(002).
               04  NAFCO-DNOD PIC  9(007).
             03  NAFCO-DGN    PIC  9(002).
           02  NAFCO-SHN      PIC  X(025).
           02  NAFCO-KKK      PIC  X(025).
           02  NAFCO-SU       PIC  9(005).
           02  NAFCO-TSU      PIC  9(005).
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　ＴＤＮＮＦ　訂正更新　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　（ナフコ）　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  TDNNF ﾅｼ  ***".
             03  E-ME2   PIC  X(019) VALUE
                  "***  TDNNF ﾅｼ2  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾃﾞｰﾀ ｴﾗｰ  ***".
             03  E-ME5   PIC  X(027) VALUE
                  "***  TDNNF REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  X(020).
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
            "C-MID" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "44" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "102" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "19" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "27" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "20" "E-ME5" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE NAFCO-KEY "20" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" NAFCOD_PNAME1 " " BY REFERENCE NAFCOD_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 " " BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
       M-10.
      *           READ NAFCOD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NAFCOD_PNAME1 BY REFERENCE NAFCO-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  NAFCO-SU = NAFCO-TSU
               GO TO M-10
           END-IF
      *
           MOVE NAFCO-KEY TO TDNN2-KEY.
      *           READ TDNNF INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" TDNNF_PNAME1 BY REFERENCE TDNN-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE NAFCO-TSU TO TDNN2-TSU.
      *           REWRITE TDNN-R2 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R2 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NAFCOD_IDLST NAFCOD_PNAME1.
           IF  COMPLETION_CODE NOT = 255
               CALL "DB_F_Open" USING
                "OUTPUT" NAFCOD_PNAME1 " " BY REFERENCE NAFCOD_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE NAFCOD_IDLST NAFCOD_PNAME1
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       CHK-RTN.
           CALL "DB_F_Open" USING
            "INPUT" NAFCOD_PNAME1 " " BY REFERENCE NAFCOD_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TDNNF_PNAME1 " " BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
       CHK-05.
      *           READ NAFCOD AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NAFCOD_PNAME1 BY REFERENCE NAFCO-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-90
           END-IF
      *
           MOVE NAFCO-KEY TO TDNN2-KEY.
      *           READ TDNNF INVALID KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" TDNNF_PNAME1 BY REFERENCE TDNN-R1 " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO CHK-EX
           END-IF
           IF (NAFCO-SHN NOT = TDNN2-SHN) OR
              (NAFCO-KKK NOT = TDNN2-KKK) OR
              (NAFCO-SU  NOT = TDNN2-SU )
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO CHK-EX
           END-IF
           GO TO CHK-05.
       CHK-90.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NAFCOD_IDLST NAFCOD_PNAME1.
       CHK-EX.
           EXIT.
