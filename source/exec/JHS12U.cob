       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS12U.
      ****************************************************
      *****    受注受信データ累積・更新（ナフコ）    *****
      ****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       01  W-DATA.
           02  W-STC          PIC  X(009).
           02  W-DNO          PIC  X(009).
           02  W-DGN          PIC  9(002).
           02  W-C            PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-NGP          PIC  9(006).
      *
           COPY LITDNN.
      *FD  JCANF
       01  JCANF_JHS12U.
           02  JCANF_PNAME1 PIC  X(005) VALUE "JCANF".
           02  F            PIC  X(001).
           02  JCANF_LNAME  PIC  X(012) VALUE "JCANF_JHS12U".
           02  F            PIC  X(001).
           02  JCANF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCANF_SORT   PIC  X(100) VALUE SPACE.
           02  JCANF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCANF_RES    USAGE  POINTER.
       01  JCAN-R.
           02  JCAN-RC1       PIC  X(001).
           02  JCAN-DR.
             03  JCAN-ID      PIC  X(001).
             03  F            PIC  X(254).
           02  JCAN-HR    REDEFINES JCAN-DR.
             03  JCANH-DC1    PIC  9(002).
             03  JCANH-DNO.
               04  F          PIC  X(002).
               04  JCANH-DNOD PIC  9(007).
             03  JCANH-STC.
               04  JCANH-SCD  PIC  9(002).
               04  JCANH-TCD  PIC  9(003).
               04  F          PIC  X(004).
             03  F            PIC  X(002).
             03  JCANH-BCD    PIC  9(002).
             03  JCANH-DPC    PIC  9(002).
             03  JCANH-HNGP   PIC  9(006).
             03  JCANH-NNGP   PIC  9(006).
             03  JCANH-THC    PIC  9(006).
             03  JCANH-STA    PIC  X(002).
             03  JCANH-SNA    PIC  X(015).
             03  JCANH-TNA    PIC  X(015).
             03  JCANH-TSN    PIC  X(015).
             03  JCANH-TST    PIC  X(012).
             03  JCANH-HCC    PIC  9(001).
             03  JCANH-F1     PIC  X(022).
             03  JCANH-DH1    PIC  9(001).
             03  JCANH-RC2    PIC  X(001).
             03  JCANH-DC2    PIC  9(002).
             03  JCANH-AR     PIC  X(007).
             03  JCANH-DUR    PIC  X(026).
             03  JCANH-DSHR   PIC  X(014).
             03  JCANH-DSMR   PIC  X(007).
             03  JCANH-ER     PIC  X(005).
             03  JCANH-FSR    PIC  X(015).
             03  JCANH-FUR    PIC  X(007).
             03  JCANH-LCR    PIC  X(016).
             03  JCANH-LUR    PIC  X(020).
             03  JCANH-LSR    PIC  X(007).
             03  JCANH-DH2    PIC  9(001).
           02  JCAN-MR    REDEFINES JCAN-DR.
             03  JCANM-DC1    PIC  9(002).
             03  JCANM-DGN    PIC  9(002).
             03  JCANM-JAN    PIC  X(013).
             03  JCANM-GAR    PIC  X(006).
             03  F            PIC  X(001).
             03  JCANM-TNI    PIC  X(003).
             03  JCANM-SU     PIC  9(005).
             03  F            PIC  X(001).
             03  JCANM-GTN    PIC  9(007).
             03  F            PIC  X(002).
             03  JCANM-UTN    PIC  9(007).
             03  JCANM-GKIN   PIC  9(010).
             03  JCANM-UKIN   PIC  9(010).
             03  F            PIC  X(009).
             03  JCANM-SHN    PIC  X(025).
             03  JCANM-HSC    PIC  X(008).
             03  JCANM-COR    PIC  X(006).
             03  JCANM-SIZ    PIC  X(005).
             03  F            PIC  X(004).
             03  JCANM-DH1    PIC  9(001).
             03  JCANM-RC2    PIC  X(001).
             03  JCANM-DC2    PIC  9(002).
             03  JCANM-KKK    PIC  X(025).
             03  JCANM-PCH    PIC  X(001).
             03  JCANM-PSI    PIC  X(001).
             03  JCANM-PBM    PIC  9(002).
             03  JCANM-PJAN   PIC  X(013).
             03  JCANM-PSHN   PIC  X(020).
             03  JCANM-PKKK   PIC  X(020).
             03  JCANM-PUTN   PIC  9(007).
             03  JCANM-PMS    PIC  9(005).
             03  F            PIC  X(030).
             03  JCANM-DH2    PIC  9(001).
       77  F                  PIC  X(001).
      *FD  JCANRF
       01  JCANRF_JHS12U.
           02  JCANRF_PNAME1  PIC  X(006) VALUE "JCANRF".
           02  F              PIC  X(001).
           02  JCANRF_LNAME   PIC  X(013) VALUE "JCANRF_JHS12U".
           02  F              PIC  X(001).
           02  JCANRF_KEY1    PIC  X(100) VALUE SPACE.
           02  JCANRF_SORT    PIC  X(100) VALUE SPACE.
           02  JCANRF_IDLST   PIC  X(100) VALUE SPACE.
           02  JCANRF_RES     USAGE  POINTER.
       01  JCANR-R.
           02  JCANR-R1       PIC  X(256).
           02  F              PIC  X(079).
           02  JCANR-NGP      PIC  9(006).
       77  F                  PIC  X(001).
      *    80/3
      *FD  NAFCOD
       01  NAFCOD_JHS12U.
           02  NAFCOD_PNAME1  PIC  X(006) VALUE "NAFCOD".
           02  F              PIC  X(001).
           02  NAFCOD_LNAME   PIC  X(013) VALUE "NAFCOD_JHS12U".
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
                "＊＊＊　　受注受信データ累積・更新　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　（ナフコ）　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(006) VALUE
                  "データ　なし".
             03  E-ME2   PIC  N(007) VALUE
                  "データエラー１".
             03  E-ME3   PIC  N(007) VALUE
                  "データエラー２".
             03  E-ME4   PIC  N(005) VALUE
                  "行　エラー".
             03  E-ME5   PIC  X(025) VALUE
                  "***  TDNNF WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  JCANRF WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  NAFCOD WRITE ｴﾗｰ  ***".
             03  E-DGN.
               04  E-NO   PIC  9(002).
               04  FILLER  PIC  9(002).
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
            "C-ERR" " " "0" "0" "131" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "131" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "N" "24" "15" "12" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "N" "24" "15" "14" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "N" "24" "15" "14" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "N" "24" "15" "10" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "25" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-DGN" " " "24" "0" "4" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NO" "9" "24" "28" "2" " " "E-DGN" RETURNING RESU.
       CALL "SD_From" USING 
            "E-NO" BY REFERENCE W-DGN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-DGN" "9" "24" "31" "2" "E-NO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-DGN" BY REFERENCE JCANM-DGN "2" "0" RETURNING RESU.
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
            "INPUT" JCANF_PNAME1 " " BY REFERENCE JCANF_IDLST "0".
       M-15.
      *           READ JCANF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCANF_PNAME1 BY REFERENCE JCAN-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCANF_IDLST JCANF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JCAN-RC1 NOT = "A"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCANF_IDLST JCANF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ACCEPT W-NGP FROM DATE.
           MOVE 0 TO CHK.
           CALL "DB_F_Open" USING
            "EXTEND" JCANRF_PNAME1 " " BY REFERENCE JCANRF_IDLST "0".
       M-25.
           INITIALIZE JCANR-R.
           MOVE JCAN-R TO JCANR-R1.
           MOVE W-NGP TO JCANR-NGP.
      *           WRITE JCANR-R.
      *//////////////
           CALL "DB_Insert" USING
            JCANRF_PNAME1 JCANRF_LNAME JCANR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME6" E-ME6 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCANF_IDLST JCANF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCANRF_IDLST JCANRF_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANRF_IDLST JCANRF_PNAME1.
           MOVE "JCANRF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JCANRF_PNAME1 " " BY REFERENCE JCANRF_IDLST "0".
           GO TO M-25.
       M-30.
      *           READ JCANF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCANF_PNAME1 BY REFERENCE JCAN-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               IF  JCAN-RC1 NOT = "B"
                   IF  JCAN-RC1 = "A"
                       GO TO M-25
                   ELSE
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  255
                       CALL "DB_F_Close" USING
                        BY REFERENCE JCANF_IDLST JCANF_PNAME1
                       CALL "SD_Output" USING
                        "E-ME2" E-ME2 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-95
                   END-IF
               END-IF
           END-IF
           GO TO M-25.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANF_IDLST JCANF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANRF_IDLST JCANRF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCANF_PNAME1 " " BY REFERENCE JCANF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 " " BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" NAFCOD_PNAME1 " " BY REFERENCE NAFCOD_IDLST "0".
           MOVE 9 TO W-DGN.
       M-40.
      *           READ JCANF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCANF_PNAME1 BY REFERENCE JCAN-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  JCAN-RC1 = "A"
               GO TO M-40
           END-IF.
       M-45.
           IF  JCAN-RC1 NOT = "B"
               GO TO M-60
           END-IF
           IF  W-DGN = ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE JCANH-STC TO W-STC.
           MOVE JCANH-DNO TO W-DNO.
           MOVE ZERO TO W-DGN.
       M-50.
           PERFORM MOV1-RTN THRU MOV1-EX.
      *           WRITE TDNN-R1 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
           GO TO M-40.
       M-55.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           MOVE "TDNNF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 " " BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           GO TO M-50.
       M-60.
           IF  JCAN-RC1 NOT = "D"
               GO TO M-45
           END-IF
           ADD 1 TO W-DGN.
           IF  W-DGN > 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NO" E-NO "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  JCANM-DGN NOT = W-DGN
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-DGN" E-DGN "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-65.
           PERFORM MOV2-RTN THRU MOV2-EX.
      *           WRITE TDNN-R2 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNNF_PNAME1 TDNNF_LNAME TDNN-R2 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-70
           END-IF
           GO TO M-75.
       M-70.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           MOVE "TDNNF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TDNNF_PNAME1 " " BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           GO TO M-65.
       M-75.
           INITIALIZE NAFCO-R.
           MOVE W-STC TO NAFCO-STC.
           MOVE W-DNO TO NAFCO-DNO.
           MOVE JCANM-DGN TO NAFCO-DGN.
           MOVE JCANM-SU TO NAFCO-SU NAFCO-TSU.
           MOVE JCANM-SHN TO NAFCO-SHN.
           MOVE JCANM-KKK TO NAFCO-KKK.
      *           WRITE NAFCO-R.
      *//////////////
           CALL "DB_Insert" USING
            NAFCOD_PNAME1 NAFCOD_LNAME NAFCO-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-40
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NAFCOD_IDLST NAFCOD_PNAME1.
           MOVE "NAFCOD       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" NAFCOD_PNAME1 " " BY REFERENCE NAFCOD_IDLST "0".
           GO TO M-75.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JCANF_IDLST JCANF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NAFCOD_IDLST NAFCOD_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MOV1-RTN.
           INITIALIZE TDNN-R1.
           MOVE W-STC TO TDNN1-STC.
           MOVE W-DNO TO TDNN1-DNO.
           MOVE ZERO TO TDNN1-DGN.
           MOVE JCANH-BCD TO TDNN1-BCD.
           MOVE JCANH-DPC TO TDNN1-DPC.
           MOVE JCANH-HNGP TO TDNN1-HNGP.
           MOVE JCANH-NNGP TO TDNN1-NNGP.
           MOVE JCANH-THC TO TDNN1-THC.
           MOVE JCANH-STA TO TDNN1-STA.
           MOVE JCANH-SNA TO TDNN1-SNA.
           MOVE JCANH-TNA TO TDNN1-TNA.
           MOVE JCANH-TSN TO TDNN1-TSN.
           MOVE JCANH-TST TO TDNN1-TST.
           MOVE JCANH-HCC TO TDNN1-HCC.
           MOVE JCANH-F1 TO TDNN1-F1.
           MOVE JCANH-AR TO TDNN1-AR.
           MOVE JCANH-DUR TO TDNN1-DUR.
           MOVE JCANH-DSHR TO TDNN1-DSHR.
           MOVE JCANH-DSMR TO TDNN1-DSMR.
           MOVE JCANH-ER TO TDNN1-ER.
           MOVE JCANH-FSR TO TDNN1-FSR.
           MOVE JCANH-FUR TO TDNN1-FUR.
           MOVE JCANH-LCR TO TDNN1-LCR.
           MOVE JCANH-LUR TO TDNN1-LUR.
           MOVE JCANH-LSR TO TDNN1-LSR.
           MOVE 1 TO TDNN1-PC.
       MOV1-EX.
           EXIT.
       MOV2-RTN.
           INITIALIZE TDNN-R2.
           MOVE W-STC TO TDNN2-STC.
           MOVE W-DNO TO TDNN2-DNO.
           MOVE JCANM-DGN TO TDNN2-DGN.
           MOVE JCANM-JAN TO TDNN2-JAN.
           MOVE JCANM-GAR TO TDNN2-GAR.
           MOVE JCANM-TNI TO TDNN2-TNI.
           MOVE JCANM-SU TO TDNN2-SU TDNN2-TSU.
           MOVE JCANM-GTN TO TDNN2-GTN.
           MOVE JCANM-UTN TO TDNN2-UTN.
           MOVE JCANM-GKIN TO TDNN2-GKIN.
           MOVE JCANM-UKIN TO TDNN2-UKIN.
           MOVE JCANM-SHN TO TDNN2-SHN.
           MOVE JCANM-HSC TO TDNN2-HSC.
           MOVE JCANM-COR TO TDNN2-COR.
           MOVE JCANM-SIZ TO TDNN2-SIZ.
           MOVE JCANM-KKK TO TDNN2-KKK.
           MOVE JCANM-PCH TO TDNN2-PCH.
           MOVE JCANM-PSI TO TDNN2-PSI.
           MOVE JCANM-PBM TO TDNN2-PBM.
           MOVE JCANM-PJAN TO TDNN2-PJAN.
           MOVE JCANM-PSHN TO TDNN2-PSHN.
           MOVE JCANM-PKKK TO TDNN2-PKKK.
           MOVE JCANM-PUTN TO TDNN2-PUTN.
           MOVE JCANM-PMS TO TDNN2-PMS.
           MOVE 1 TO TDNN2-PC.
       MOV2-EX.
           EXIT.
