       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBM410.
      *********************************************************
      *    PROGRAM         :  材料統計マスター　前残修正入力  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ______                          *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-JCD          PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-ZKS          PIC S9(007)V9(2).
           02  W-ZKK          PIC S9(008).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIJM.
           COPY LIJTM.
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
                "＊＊＊　　材料統計マスター　前残修正入力　　＊＊＊".
           02  FILLER.
             03  FILLER  PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  FILLER  PIC  N(003) VALUE "材料名".
           02  FILLER.
             03  FILLER  PIC  N(005) VALUE "前月繰越数".
             03  FILLER  PIC  N(004) VALUE "単　　価".
             03  FILLER  PIC  N(006) VALUE "前月繰越金額".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-JCD   PIC  9(006).
           02  A-ZKS   PIC S9(007)V9(2).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(024).
           02  FILLER.
             03  D-ZKS   PIC ZZZZZZ9.99- .
             03  D-ST    PIC ----,--9.99 .
             03  D-ZKK   PIC ---,---,--9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(040) VALUE
                  "***  JM ﾅｼ  ***                         ".
             03  E-ME2   PIC  X(040) VALUE
                  "***  JTM ﾅｼ  ***                        ".
             03  E-ME5   PIC  X(040) VALUE
                  "***  ｻﾞｲｺｸﾌﾞﾝ ｴﾗｰ  ***                  ".
             03  E-ME6   PIC  X(040) VALUE
                  "***  PROGRAM ｴﾗｰ  ***                   ".
             03  E-ME14  PIC  X(040) VALUE
                  "***  JTM REWRITE ｴﾗｰ  ***               ".
             03  E-ME20  PIC  X(040) VALUE
                  "***  キャンセル  ***                    ".
             03  E-JCD   PIC  9(006).
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
            "C-MID" " " "0" "0" "112" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "14" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" " " "5" "0" "10" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID" "X" "5" "6" "4" " " "02C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID" "N" "5" "18" "6" "0102C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" " " "7" "0" "30" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID" "N" "7" "19" "10" " " "03C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID" "N" "7" "35" "8" "0103C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0303C-MID" "N" "7" "46" "12" "0203C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "X" "22" "40" "22" "03C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD" "9" "5" "11" "6" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZKS" "S9" "8" "18" "9" "A-JCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZKS" BY REFERENCE W-ZKS "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "57" "1" "A-ZKS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "81" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "5" "25" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "8" "0" "33" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZKS" "ZZZZZZ9.99-" "8" "18" "11" " " "02C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZKS" BY REFERENCE W-ZKS "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ST" "----,--9.99" "8" "32" "11" "D-ZKS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ST" BY REFERENCE J-ST "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZKK" "---,---,--9" "8" "47" "11" "D-ST" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-ZKK" BY REFERENCE W-ZKK "8" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "246" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "246" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "40" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "40" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "40" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "40" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "40" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JCD" "9" "24" "45" "6" "E-ME20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JCD" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "I-O" JT-M_PNAME1 "SHARED" BY REFERENCE JT-M_IDLST "1"
            "JT-KEY" BY REFERENCE JT-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ST" D-ST "p" RETURNING RESU.
           IF  J-ZC NOT = 0
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE W-JCD TO JT-KEY.
      *           READ JT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-M_PNAME1 BY REFERENCE JT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               GO TO M-10
           END-IF
           MOVE JT-ZKS TO W-ZKS.
           MOVE JT-ZKK TO W-ZKK.
           CALL "SD_Output" USING "D-ZKS" D-ZKS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ZKK" D-ZKK "p" RETURNING RESU.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-ZKS "A-ZKS" "S9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           COMPUTE W-ZKK = W-ZKS * J-ST.
           CALL "SD_Output" USING "D-ZKS" D-ZKS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ZKK" D-ZKK "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           MOVE W-ZKS TO JT-ZKS.
           MOVE W-ZKK TO JT-ZKK.
      *           REWRITE JT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JT-M_PNAME1 JT-M_LNAME JT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           GO TO M-10.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JT-M_IDLST JT-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
