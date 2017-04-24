       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM250.
      *******************************************************
      *    PROGRAM         :  直送先マスター　一括登録      *
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-MTCD         PIC  9(004).
           02  W-STCD         PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-KEY          PIC  9(007).
           02  W-DATE         PIC  9(006).
           COPY LSTAT.
      *
           COPY LITCM.
      *FD  MSTRN
       01  MSTRN_HMM250.
           02  MSTRN_PNAME1   PIC  X(005) VALUE "MSTRN".
           02  F              PIC  X(001).
           02  MSTRN_LNAME    PIC  X(012) VALUE "MSTRN_HMM250".
           02  F              PIC  X(001).
           02  MSTRN_KEY1     PIC  X(100) VALUE SPACE.
           02  MSTRN_KEY2     PIC  X(100) VALUE SPACE.
           02  MSTRN_KEY3     PIC  X(100) VALUE SPACE.
           02  MSTRN_SORT     PIC  X(100) VALUE SPACE.
           02  MSTRN_IDLST    PIC  X(100) VALUE SPACE.
           02  MSTRN_RES      USAGE  POINTER.
       01  MS-R.
           02  MS-KEY.
             03  MS-ID        PIC  X(001).
             03  MS-TCD       PIC  9(004).
             03  MS-CCD       PIC  9(003).
           02  F              PIC  X(006).
           02  MS-ACT         PIC  9(001).
           02  MS-NGP         PIC  9(006).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　直送先マスター　一括移行登録　　＊＊＊".
           02  FILLER  PIC  X(015) VALUE
                  "( 1000件 以内 )".
           02  FILLER  PIC  X(012) VALUE
                  "元得意先ｺｰﾄﾞ".
           02  FILLER  PIC  N(001) VALUE "↓".
           02  FILLER  PIC  X(012) VALUE
                  "先得意先ｺｰﾄﾞ".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-MTCD  PIC  9(004).
           02  A-STCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MTNA  PIC  N(026).
           02  D-STNA  PIC  N(026).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(016) VALUE
                  "***  TCM ﾅｼ  ***".
             03  E-ME2   PIC  X(021) VALUE
                  "***  ﾄｳﾛｸﾃﾞｰﾀ ｱﾘ  ***".
             03  E-ME3   PIC  X(023) VALUE
                  "***  TCM WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(019) VALUE
                  "***  ﾓﾄｺｰﾄﾞ ﾅｼ  ***".
             03  E-ME8   PIC  X(025) VALUE
                  "***  MSTRN WRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(027) VALUE
                  "***  MSTRN REWRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "111" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "5" "42" "15" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "10" "8" "12" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "11" "13" "2" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "X" "12" "8" "12" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "X" "20" "28" "22" "05C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MTCD" "9" "10" "21" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MTCD" BY REFERENCE W-MTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "12" "21" "4" "A-MTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "45" "1" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "104" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MTNA" "N" "10" "26" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-MTNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-STNA" "N" "12" "26" "52" "D-MTNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-STNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "131" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "131" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "23" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "19" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "25" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "27" "E-ME8" " " RETURNING RESU.
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
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-MTCD "A-MTCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           MOVE W-MTCD TO TC-TCD.
           MOVE 1 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-MTNA" D-MTNA "p" RETURNING RESU.
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
      *
           MOVE W-STCD TO TC-TCD.
           MOVE 1 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "D-STNA" D-STNA "p" RETURNING RESU.
      *
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  W-STCD = TC-TCD
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-15
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-25
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           ACCEPT W-DATE FROM DATE.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "I-O" MSTRN_PNAME1 "SHARED" BY REFERENCE MSTRN_IDLST "1"
            "MS-KEY" BY REFERENCE MS-KEY.
      *
           MOVE W-MTCD TO TC-TCD.
           MOVE 1 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-30.
      *           READ TC-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  TC-TCD NOT = W-MTCD
               GO TO M-95
           END-IF
           MOVE TC-KEY TO W-KEY.
           MOVE W-STCD TO TC-TCD.
      *           WRITE TC-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           PERFORM MS-RTN THRU MS-EX.
      *
           MOVE W-KEY TO TC-KEY.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-30.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE MSTRN_IDLST MSTRN_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MS-RTN.
           MOVE "1" TO MS-ID.
           MOVE TC-TCD TO MS-TCD.
           MOVE TC-CCD TO MS-CCD.
      *           READ MSTRN INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" MSTRN_PNAME1 BY REFERENCE MS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO MS-020
           END-IF
           MOVE 1 TO MS-ACT.
           MOVE W-DATE TO MS-NGP.
      *           REWRITE MS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            MSTRN_PNAME1 MSTRN_LNAME MS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO MS-EX.
       MS-020.
           MOVE ZERO TO MS-R.
           MOVE "1" TO MS-ID.
           MOVE TC-TCD TO MS-TCD.
           MOVE TC-CCD TO MS-CCD.
           MOVE 1 TO MS-ACT.
           MOVE W-DATE TO MS-NGP.
      *           WRITE MS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            MSTRN_PNAME1 MSTRN_LNAME MS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       MS-EX.
           EXIT.
