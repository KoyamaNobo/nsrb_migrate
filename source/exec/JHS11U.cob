       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS11U.
      ********************************************************
      *****    受注受信データ累積・更新（ワークマン）    *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(002).
       77  W-FILE           PIC  X(013).
       01  W-R.
           02  WR-RCC       PIC  9(001).
           02  WR-DTC       PIC  X(002).
           02  WR-D1.
             03  WR-DNO     PIC  9(009).
             03  WR-STC.
               04  WR-SCD   PIC  9(004).
               04  WR-TCD   PIC  9(005).
             03  WR-BC      PIC  9(003).
             03  WR-SHC     PIC  9(001).
             03  WR-DPC     PIC  X(002).
             03  WR-HNGP    PIC  9(006).
             03  WR-NNGP    PIC  9(006).
             03  WR-THC     PIC  9(006).
             03  WR-MHC     PIC  X(001).
             03  F          PIC  X(001).
             03  WR-SNA     PIC  X(020).
             03  WR-TNA     PIC  X(020).
             03  WR-HCC     PIC  9(001).
             03  WR-HSP     PIC  X(001).
             03  WR-DHC     PIC  X(001).
             03  WR-KHC     PIC  X(001).
             03  WR-KCC     PIC  X(001).
             03  WR-UBC     PIC  X(001).
             03  WR-NCC     PIC  9(001).
             03  WR-EDI     PIC  9(001).
             03  WR-NKC     PIC  9(001).
             03  WR-ZAC     PIC  9(001).
             03  F          PIC  X(031).
           02  WR-D2    REDEFINES WR-D1.
             03  WR-DGN     PIC  9(002).
             03  WR-HCD     PIC  X(013).
             03  WR-ISU     PIC  9(003)V9(01).
             03  WR-KSU     PIC  9(004).
             03  WR-HTC     PIC  X(002).
             03  WR-SU      PIC  9(005)V9(01).
             03  WR-GTN     PIC  9(007)V9(02).
             03  WR-UTN     PIC  9(007).
             03  WR-GKIN    PIC  9(010).
             03  WR-UKIN    PIC  9(010).
             03  WR-GCN     PIC  9(006).
             03  WR-CCD     PIC  X(003).
             03  WR-SHN     PIC  X(025).
             03  WR-JAN     PIC  X(013).
             03  F          PIC  X(004).
             03  WR-TSH     PIC  9(005).
             03  WR-TKC     PIC  X(001).
             03  F          PIC  X(001).
       01  W-DATA.
           02  W-STC        PIC  9(009).
           02  W-DNO        PIC  9(009).
           02  W-DGN        PIC  9(002).
           02  W-FR         PIC  9(001).
           02  W-C          PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-NGP        PIC  9(006).
           COPY LSTAT.
      *
           COPY LSJCAW.
           COPY LITDNW.
      *FD  JCAWRF
       01  JCAWRF_JHS11U.
           02  JCAWRF_PNAME1 PIC  X(006) VALUE "JCAWRF".
           02  F             PIC  X(001).
           02  JCAWRF_LNAME  PIC  X(013) VALUE "JCAWRF_JHS11U".
           02  F             PIC  X(001).
           02  JCAWRF_KEY1   PIC  X(100) VALUE SPACE.
           02  JCAWRF_SORT   PIC  X(100) VALUE SPACE.
           02  JCAWRF_IDLST  PIC  X(100) VALUE SPACE.
           02  JCAWRF_RES    USAGE  POINTER.
       01  JCAWR-R.
           02  JCAWR-R1      PIC  X(256).
           02  F             PIC  X(079).
           02  JCAWR-NGP     PIC  9(006).
       77  F                 PIC  X(001).
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
                "＊＊＊　　　　（ワークマン）　　　　　＊＊＊".
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
                  "***  TDNWF WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  JCAWRF WRITE ｴﾗｰ  ***".
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
            "C-ERR" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "101" " " "C-ERR" RETURNING RESU.
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
            "INPUT" JCAWF_PNAME1 " " BY REFERENCE JCAWF_IDLST "0".
       M-15.
      *           READ JCAWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAWF_PNAME1 BY REFERENCE JCAW-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAWF_IDLST JCAWF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE SPACE TO W-R.
           MOVE JCAW-R1 TO W-R.
           IF  WR-RCC NOT = "A"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE JCAWF_IDLST JCAWF_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           ACCEPT W-NGP FROM DATE.
           MOVE 0 TO CHK.
           CALL "DB_F_Open" USING
            "EXTEND" JCAWRF_PNAME1 " " BY REFERENCE JCAWRF_IDLST "0".
       M-25.
           INITIALIZE JCAWR-R.
           MOVE JCAW-R TO JCAWR-R1.
           MOVE W-NGP TO JCAWR-NGP.
      *           WRITE JCAWR-R.
      *//////////////
           CALL "DB_Insert" USING
            JCAWRF_PNAME1 JCAWRF_LNAME JCAWR-R RETURNING RET.
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
                BY REFERENCE JCAWF_IDLST JCAWF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCAWRF_IDLST JCAWRF_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAWRF_IDLST JCAWRF_PNAME1.
           MOVE "JCAWRF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JCAWRF_PNAME1 " " BY REFERENCE JCAWRF_IDLST "0".
           GO TO M-25.
       M-30.
      *           READ JCAWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAWF_PNAME1 BY REFERENCE JCAW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE SPACE TO W-R
               MOVE JCAW-R1 TO W-R
               IF  WR-RCC NOT = "B"
                   IF  WR-RCC = "A"
                       CALL "SD_Output" USING
                        "E-ME1" E-ME1 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       GO TO M-35
                   ELSE
                       CALL "C3_Set_Jrcode" USING 
                        USER_ID BY REFERENCE COMPLETION_CODE  255
                       CALL "DB_F_Close" USING
                        BY REFERENCE JCAWF_IDLST JCAWF_PNAME1
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
            BY REFERENCE JCAWF_IDLST JCAWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAWRF_IDLST JCAWRF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCAWF_PNAME1 " " BY REFERENCE JCAWF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           MOVE 9 TO W-DGN.
       M-40.
      *           READ JCAWF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JCAWF_PNAME1 BY REFERENCE JCAW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE SPACE TO W-R.
           MOVE JCAW-R1 TO W-R.
           IF  WR-RCC = "A"
               GO TO M-40
           END-IF
           MOVE 0 TO W-FR.
       M-45.
           ADD 1 TO W-FR.
           IF  W-FR > 2
               GO TO M-40
           END-IF
           MOVE SPACE TO W-R.
           IF  W-FR = 1
               MOVE JCAW-R1 TO W-R
           ELSE
               MOVE JCAW-R2 TO W-R
           END-IF
      *
           IF  WR-RCC NOT = "B"
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
           MOVE WR-STC TO W-STC.
           MOVE WR-DNO TO W-DNO.
           MOVE ZERO TO W-DGN.
       M-50.
           PERFORM MOV1-RTN THRU MOV1-EX.
      *           WRITE TDNW-R1 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-55
           END-IF
           GO TO M-45.
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
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           MOVE "TDNWF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           GO TO M-50.
       M-60.
           IF  WR-RCC NOT = "D"
               GO TO M-45
           END-IF
           ADD 1 TO W-DGN.
           IF  W-DGN > 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  WR-DGN NOT = W-DGN
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF.
       M-65.
           PERFORM MOV2-RTN THRU MOV2-EX.
      *           WRITE TDNW-R2 INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNWF_PNAME1 TDNWF_LNAME TDNW-R2 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-70
           END-IF
           GO TO M-45.
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
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           MOVE "TDNWF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           GO TO M-65.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JCAWF_IDLST JCAWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MOV1-RTN.
           INITIALIZE TDNW-R1.
           MOVE W-STC TO TDNW1-STC.
           MOVE W-DNO TO TDNW1-DNO.
           MOVE W-DGN TO TDNW1-DGN.
           MOVE WR-BC TO TDNW1-BC.
           MOVE WR-SHC TO TDNW1-SHC.
           MOVE WR-DPC TO TDNW1-DPC.
           MOVE WR-HNGP TO TDNW1-HNGP.
           MOVE WR-NNGP TO TDNW1-NNGP.
           MOVE WR-THC TO TDNW1-THC.
           MOVE WR-MHC TO TDNW1-MHC.
           MOVE WR-SNA TO TDNW1-SNA.
           MOVE WR-TNA TO TDNW1-TNA.
           MOVE WR-HCC TO TDNW1-HCC.
           MOVE WR-HSP TO TDNW1-HSP.
           MOVE WR-DHC TO TDNW1-DHC.
           MOVE WR-KHC TO TDNW1-KHC.
           MOVE WR-KCC TO TDNW1-KCC.
           MOVE WR-UBC TO TDNW1-UBC.
           MOVE WR-NCC TO TDNW1-NCC.
           MOVE WR-EDI TO TDNW1-EDI.
           MOVE WR-NKC TO TDNW1-NKC.
           MOVE WR-ZAC TO TDNW1-ZAC.
       MOV1-EX.
           EXIT.
       MOV2-RTN.
           INITIALIZE TDNW-R2.
           MOVE W-STC TO TDNW2-STC.
           MOVE W-DNO TO TDNW2-DNO.
           MOVE WR-DGN TO TDNW2-DGN.
           MOVE WR-HCD TO TDNW2-HCD.
           MOVE WR-ISU TO TDNW2-ISU.
           MOVE WR-KSU TO TDNW2-KSU.
           MOVE WR-HTC TO TDNW2-HTC.
           MOVE WR-SU TO TDNW2-SU.
           MOVE WR-GTN TO TDNW2-GTN.
           MOVE WR-UTN TO TDNW2-UTN.
           MOVE WR-GKIN TO TDNW2-GKIN.
           MOVE WR-UKIN TO TDNW2-UKIN.
           MOVE WR-GCN TO TDNW2-GCN.
           MOVE WR-CCD TO TDNW2-CCD.
           MOVE WR-SHN TO TDNW2-SHN.
           MOVE WR-JAN TO TDNW2-JAN.
           MOVE WR-TSH TO TDNW2-TSH.
           MOVE WR-TKC TO TDNW2-TKC.
       MOV2-EX.
           EXIT.
