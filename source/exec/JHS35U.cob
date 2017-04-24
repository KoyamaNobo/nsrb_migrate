       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS15U.
      ********************************************************
      *****    その他受信データ生成（赤ちゃん本舗）      *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  W-DATE             PIC  9(006).
      *
      *FD  JCAASF
       01  JCAASF_JHS35U.
           02  JCAASF_PNAME1 PIC  X(006) VALUE "JCAASF".
           02  F             PIC  X(001).
           02  JCAASF_LNAME  PIC  X(013) VALUE "JCAASF_JHS35U".
           02  F             PIC  X(001).
           02  JCAASF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAASF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAASF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAASF_RES    USAGE  POINTER.
       01  JCAAS-R.
           02  JCAAS-RC       PIC  X(001).
           02  JCAAS-REC1     PIC  X(126).
           02  JCAAS-REC2  REDEFINES JCAAS-REC1.
             03  JCAAS-DC     PIC  9(002).
             03  JCAAS-NG     PIC  9(006).
             03  JCAAS-TCD    PIC  9(006).
             03  JCAAS-CS     PIC  9(001).
             03  JCAAS-MC     PIC  9(002).
             03  JCAAS-DATE   PIC  9(008).
             03  JCAAS-DNO    PIC  9(011).
             03  JCAAS-TSC    PIC  9(002).
             03  JCAAS-BMC    PIC  9(007).
             03  JCAAS-KINF   PIC  X(001).
             03  JCAAS-KIN    PIC  9(009).
             03  JCAAS-SHZF   PIC  X(001).
             03  JCAAS-SHZ    PIC  9(007).
             03  JCAAS-KEIF   PIC  X(001).
             03  JCAAS-KEI    PIC  9(009).
             03  JCAAS-TSKF   PIC  X(001).
             03  JCAAS-TSK    PIC  9(009).
             03  JCAAS-RIT    PIC  9(003)V9(02).
             03  JCAAS-TEK    PIC  X(015).
             03  F            PIC  X(023).
           02  JCAAS-CHK      PIC  X(001).
       77  F                  PIC  X(001).
      *FD  JCAASRF
       01  JCAASRF_JHS35U.
           02  JCAASRF_PNAME1 PIC  X(007) VALUE "JCAASRF".
           02  F              PIC  X(001).
           02  JCAASRF_LNAME  PIC  X(014) VALUE "JCAASRF_JHS35U".
           02  F              PIC  X(001).
           02  JCAASRF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAASRF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAASRF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAASRF_RES    USAGE  POINTER.
       01  JCAASR-R.
           02  JCAASR-RC      PIC  X(001).
           02  JCAASR-REC1    PIC  X(121).
           02  JCAASR-REC2  REDEFINES JCAASR-REC1.
             03  JCAASR-DC    PIC  9(002).
             03  JCAASR-NG    PIC  9(006).
             03  JCAASR-TCD   PIC  9(006).
             03  JCAASR-CS    PIC  9(001).
             03  JCAASR-MC    PIC  9(002).
             03  JCAASR-DATE  PIC  9(008).
             03  JCAASR-DNO   PIC  9(011).
             03  JCAASR-TSC   PIC  9(002).
             03  JCAASR-BMC   PIC  9(007).
             03  JCAASR-KINF  PIC  X(001).
             03  JCAASR-KIN   PIC  9(009).
             03  JCAASR-SHZF  PIC  X(001).
             03  JCAASR-SHZ   PIC  9(007).
             03  JCAASR-KEIF  PIC  X(001).
             03  JCAASR-KEI   PIC  9(009).
             03  JCAASR-TSKF  PIC  X(001).
             03  JCAASR-TSK   PIC  9(009).
             03  JCAASR-RIT   PIC  9(003)V9(02).
             03  JCAASR-TEK   PIC  X(015).
             03  F            PIC  X(018).
           02  JCAASR-NGP     PIC  9(006).
       77  F                  PIC  X(001).
      *FD  AKATF
       01  AKATF_JHS35U.
           02  AKATF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  AKATF_LNAME    PIC  X(012) VALUE "AKATF_JHS35U".
           02  F              PIC  X(001).
           02  AKATF_KEY1     PIC  X(100) VALUE SPACE.
           02  AKATF_SORT     PIC  X(100) VALUE SPACE.
           02  AKATF_IDLST    PIC  X(100) VALUE SPACE.
           02  AKATF_RES      USAGE  POINTER.
       01  AKAT-R.
           02  AKAT-DC        PIC  9(002).
           02  AKAT-NG        PIC  9(006).
           02  AKAT-TCD       PIC  9(006).
           02  AKAT-CS        PIC  9(001).
           02  AKAT-MC        PIC  9(002).
           02  AKAT-DATE      PIC  9(008).
           02  AKAT-DNO       PIC  9(011).
           02  AKAT-TSC       PIC  9(002).
           02  AKAT-BMC       PIC  9(007).
           02  AKAT-KIN       PIC S9(009).
           02  AKAT-SHZ       PIC S9(007).
           02  AKAT-KEI       PIC S9(009).
           02  AKAT-TSK       PIC S9(009).
           02  AKAT-RIT       PIC  9(003)V9(02).
           02  AKAT-TEK       PIC  X(015).
           02  F              PIC  X(029).
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　その他受信データ累積・生成　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　（赤ちゃん本舗）　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(006) VALUE
                  "データ　なし".
             03  E-ME2   PIC  N(007) VALUE
                  "データ　エラー".
             03  E-ME5   PIC  N(005) VALUE
                  "累積　済み".
             03  E-ME12  PIC  X(026) VALUE
                  "***  JCAARF WRITE ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "322" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "62" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "12" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "14" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "N" "24" "15" "10" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "I-O" JCAASF_PNAME1 " " BY REFERENCE JCAASF_IDLST "0".
       M-10.
      *           READ JCAASF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAASF_PNAME1 BY REFERENCE JCAAS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAASF_IDLST JCAASF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JCAAS-CHK = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  JCAAS-RC NOT = "A"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAASF_IDLST JCAASF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ACCEPT W-DATE FROM DATE.
           CALL "DB_F_Open" USING
            "EXTEND" JCAASRF_PNAME1 " " BY REFERENCE JCAASRF_IDLST "0".
       M-15.
           INITIALIZE JCAASR-R.
           MOVE JCAAS-R TO JCAASR-R.
           MOVE W-DATE TO JCAASR-NGP.
      *           WRITE JCAASR-R.
      *//////////////
           CALL "DB_Insert" USING
            JCAASRF_PNAME1 JCAASRF_LNAME JCAASR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME12" E-ME12 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAASF_IDLST JCAASF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCAASRF_IDLST JCAASRF_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAASRF_IDLST JCAASRF_PNAME1.
           MOVE "JCAASRF      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JCAASRF_PNAME1 " " BY REFERENCE JCAASRF_IDLST "0".
           GO TO M-15.
       M-20.
           MOVE 1 TO JCAAS-CHK.
      *           REWRITE JCAAS-R.
      *///////////////
           CALL "DB_Update" USING
            JCAASF_PNAME1 JCAASF_LNAME JCAAS-R RETURNING RET.
      *
      *           READ JCAASF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAASF_PNAME1 BY REFERENCE JCAAS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           GO TO M-15.
       M-25.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAASF_IDLST JCAASF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAASRF_IDLST JCAASRF_PNAME1.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO AKATF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JCAASF_PNAME1 " " BY REFERENCE JCAASF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" AKATF_PNAME1 " " BY REFERENCE AKATF_IDLST "0".
       M-30.
      *           READ JCAASF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAASF_PNAME1 BY REFERENCE JCAAS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JCAAS-RC NOT = "D"
               GO TO M-30
           END-IF
      *
           MOVE SPACE TO AKAT-R.
           MOVE JCAAS-DC   TO AKAT-DC.
           MOVE JCAAS-NG   TO AKAT-NG.
           MOVE JCAAS-TCD  TO AKAT-TCD.
           MOVE JCAAS-CS   TO AKAT-CS.
           MOVE JCAAS-MC   TO AKAT-MC.
           MOVE JCAAS-DATE TO AKAT-DATE.
           MOVE JCAAS-DNO  TO AKAT-DNO.
           MOVE JCAAS-TSC  TO AKAT-TSC.
           MOVE JCAAS-BMC  TO AKAT-BMC.
           MOVE JCAAS-KIN  TO AKAT-KIN.
           IF  JCAAS-KINF = "-"
               COMPUTE AKAT-KIN = -1 * AKAT-KIN
           END-IF
           MOVE JCAAS-SHZ  TO AKAT-SHZ.
           IF  JCAAS-SHZF = "-"
               COMPUTE AKAT-SHZ = -1 * AKAT-SHZ
           END-IF
           MOVE JCAAS-KEI  TO AKAT-KEI.
           IF  JCAAS-KEIF = "-"
               COMPUTE AKAT-KEI = -1 * AKAT-KEI
           END-IF
           MOVE JCAAS-TSK  TO AKAT-TSK.
           IF  JCAAS-TSKF = "-"
               COMPUTE AKAT-TSK = -1 * AKAT-TSK
           END-IF
           MOVE JCAAS-RIT  TO AKAT-RIT.
           MOVE JCAAS-TEK  TO AKAT-TEK.
      *           WRITE AKAT-R.
      *//////////////
           CALL "DB_Insert" USING
            AKATF_PNAME1 AKATF_LNAME AKAT-R RETURNING RET.
           GO TO M-30.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAASF_IDLST JCAASF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE AKATF_IDLST AKATF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
