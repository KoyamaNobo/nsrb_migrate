       IDENTIFICATION DIVISION.
       PROGRAM-ID. JHS11U.
      ********************************************************
      *****    受注受信データＥＤＩ累積・更新（ワークマン）***
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(002).
       77  W-FILE           PIC  X(013).
       01  W-DATA.
           02  W-DMM        PIC  9(001).
           02  W-KEY.
             03  W-STC.
               04  W-STC1   PIC  9(004).
               04  W-STC2   PIC  9(005).
             03  W-DNO.
               04  F        PIC  9(002).
               04  W-DNOD   PIC  9(007).
             03  W-DGN      PIC  9(002).
           02  CNT1         PIC  9(002).
           02  CNT2         PIC  9(002).
           02  CNT3         PIC  9(002).
           02  W-NGPL.
             03  W-NEN1     PIC  9(002).
             03  W-DATE.
               04  W-NEN2   PIC  9(002).
               04  W-GETD   PIC  9(002).
               04  F        PIC  9(002).
           02  W-SNGP       PIC  9(006).
           02  W-NGP.
             03  F          PIC  9(002).
             03  W-NGPS     PIC  9(006).
             03  W-NGPD  REDEFINES W-NGPS.
               04  W-NEN    PIC  9(002).
               04  W-GET    PIC  9(002).
               04  W-PEY    PIC  9(002).
           02  W-DDDD  REDEFINES W-NGP.
             03  W-DDD   OCCURS   8.
               04  W-DD     PIC  X(001).
           02  W-MDDD.
             03  W-MDD   OCCURS  10.
               04  W-MD     PIC  X(001).
           COPY LSTAT.
      *
           COPY LSWMJC.
           COPY LITDNW.
      *FD  WMJCRF
       01  WMJCRF_JHS10U.
           02  WMJCRF_PNAME1 PIC  X(006) VALUE "WMJCRF".
           02  F             PIC  X(001).
           02  WMJCRF_LNAME  PIC  X(013) VALUE "WMJCRF_JHS10U".
           02  F             PIC  X(001).
           02  WMJCRF_KEY1   PIC  X(100) VALUE SPACE.
           02  WMJCRF_SORT   PIC  X(100) VALUE SPACE.
           02  WMJCRF_IDLST  PIC  X(100) VALUE SPACE.
           02  WMJCRF_RES    USAGE  POINTER.
       01  WMJCR-R.
           02  WMJCR-R1      PIC  X(1301).
           02  WMJCR-NGP     PIC  9(006).
       77  F                 PIC  X(001).
      *FD  TDNWRF
       01  TDNWRF_JHS10U.
           02  TDNWRF_PNAME1 PIC  X(006) VALUE "TDNWRF".
           02  F             PIC  X(001).
           02  TDNWRF_LNAME  PIC  X(013) VALUE "TDNWRF_JHS10U".
           02  F             PIC  X(001).
           02  TDNWRF_KEY1   PIC  X(100) VALUE SPACE.
           02  TDNWRF_SORT   PIC  X(100) VALUE SPACE.
           02  TDNWRF_IDLST  PIC  X(100) VALUE SPACE.
           02  TDNWRF_RES    USAGE  POINTER.
       01  TDNWR-R.
           02  TDNWR-KEY     PIC  X(020).
           02  F             PIC  X(227).
           02  TDNWR-DATE    PIC  9(008).
           02  F             PIC  X(001).
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
                "＊＊＊　　（ワークマンＥＤＩ）　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  N(006) VALUE
                  "データ　なし".
             03  E-ME2   PIC  N(007) VALUE
                  "日付エラー　１".
             03  E-ME3   PIC  N(007) VALUE
                  "日付エラー　２".
             03  E-ME4   PIC  N(005) VALUE
                  "行　エラー".
             03  E-ME5   PIC  X(025) VALUE
                  "***  TDNWF WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  WMJCRF WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  N(005) VALUE
                  "重複エラー".
             03  E-KEYW  PIC  X(020).
             03  E-KEY   PIC  X(020).
             03  E-MD    PIC  X(010).
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
            "C-ERR" " " "0" "0" "161" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "161" " " "C-ERR" RETURNING RESU.
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
            "E-ME7" "N" "24" "15" "10" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEYW" "X" "24" "45" "20" "E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEYW" BY REFERENCE W-KEY "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "20" "E-KEYW" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TDNW1-KEY "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-MD" "X" "24" "33" "10" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-MD" BY REFERENCE W-MDDD "10" "0" RETURNING RESU.
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
           ACCEPT W-SNGP FROM DATE.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-95
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" WMJCF_PNAME1 " " BY REFERENCE WMJCF_IDLST "0".
       M-15.
      *           READ WMJCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WMJCF_PNAME1 BY REFERENCE WMJC-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_F_Close" USING
                BY REFERENCE WMJCF_IDLST WMJCF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "EXTEND" WMJCRF_PNAME1 " " BY REFERENCE WMJCRF_IDLST "0".
       M-25.
           INITIALIZE WMJCR-R.
           MOVE WMJC-R TO WMJCR-R1.
           MOVE W-SNGP TO WMJCR-NGP.
      *           WRITE WMJCR-R.
      *//////////////
           CALL "DB_Insert" USING
            WMJCRF_PNAME1 WMJCRF_LNAME WMJCR-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME6" E-ME6 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "34"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_F_Close" USING
                BY REFERENCE WMJCF_IDLST WMJCF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE WMJCRF_IDLST WMJCRF_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCRF_IDLST WMJCRF_PNAME1.
           MOVE "WMJCRF       " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" WMJCRF_PNAME1 " " BY REFERENCE WMJCRF_IDLST "0".
           GO TO M-25.
       M-30.
      *           READ WMJCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WMJCF_PNAME1 BY REFERENCE WMJC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-35
           END-IF
           GO TO M-25.
       M-35.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCF_IDLST WMJCF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCRF_IDLST WMJCRF_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" WMJCF_PNAME1 " " BY REFERENCE WMJCF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
       M-40.
      *           READ WMJCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WMJCF_PNAME1 BY REFERENCE WMJC-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  WMJC-107 NOT = 1
               GO TO M-55
           END-IF
      
           INITIALIZE TDNW-R1.
           MOVE ZERO TO TDNW1-KEY.
           MOVE WMJC-029 TO TDNW1-STC1.
           MOVE WMJC-043 TO TDNW1-STC2.
           MOVE WMJC-037 TO TDNW1-DNO.
           MOVE 0 TO TDNW1-DGN.
           MOVE WMJC-077 TO TDNW1-BC.
           MOVE WMJC-086 TO TDNW1-SHC.
           MOVE WMJC-092 TO TDNW1-DPC.
      *
           MOVE SPACE TO W-MDDD.
           MOVE ZERO TO W-NGP.
           MOVE WMJC-079 TO W-MDDD.
           PERFORM SET-RTN THRU SET-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           MOVE W-NGPS TO TDNW1-HNGP.
      *
           MOVE SPACE TO W-MDDD.
           MOVE ZERO TO W-NGP.
           MOVE WMJC-081 TO W-MDDD.
           PERFORM SET-RTN THRU SET-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           MOVE W-NGPS TO TDNW1-NNGP.
      *
           MOVE WMJC-054 TO TDNW1-THC.
           MOVE WMJC-038 TO TDNW1-MHC.
           MOVE WMJC-036 TO TDNW1-SNA.
           MOVE WMJC-046 TO TDNW1-TNA.
           MOVE WMJC-087 TO TDNW1-HCC.
           MOVE WMJC-066 TO TDNW1-HSP.
           MOVE WMJC-093 TO TDNW1-DHC.
           MOVE WMJC-065 TO TDNW1-KHC.
           MOVE WMJC-100 TO TDNW1-KCC.
           MOVE WMJC-078 TO TDNW1-UBC.
           MOVE WMJC-039 TO TDNW1-NCC.
           MOVE WMJC-088 TO TDNW1-EDI.
           MOVE WMJC-068 TO TDNW1-NKC.
           MOVE WMJC-097 TO TDNW1-ZAC.
       M-45.
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
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           GO TO M-55.
       M-50.
           IF  ERR-STAT NOT = "24"
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
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           MOVE "TDNWF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           GO TO M-45.
       M-55.
           INITIALIZE TDNW-R2.
           MOVE ZERO TO TDNW2-KEY.
           MOVE WMJC-029 TO TDNW2-STC1.
           MOVE WMJC-043 TO TDNW2-STC2.
           MOVE WMJC-037 TO TDNW2-DNO.
           MOVE WMJC-107 TO TDNW2-DGN.
           MOVE WMJC-118 TO TDNW2-HCD.
           MOVE WMJC-155 TO TDNW2-ISU.
           MOVE WMJC-159 TO TDNW2-KSU.
           MOVE WMJC-157 TO TDNW2-HTC.
           MOVE WMJC-158 TO TDNW2-SU.
           MOVE WMJC-151 TO TDNW2-GTN.
           MOVE WMJC-153 TO TDNW2-UTN.
           MOVE WMJC-150 TO TDNW2-GKIN.
           MOVE WMJC-152 TO TDNW2-UKIN.
           MOVE WMJC-112 TO TDNW2-GCN.
           MOVE WMJC-111 TO TDNW2-CCD.
           MOVE WMJC-122 TO TDNW2-SHN.
           MOVE WMJC-120 TO TDNW2-TSH.
           MOVE WMJC-108 TO TDNW2-TKC.
       M-60.
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
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-65
           END-IF
           GO TO M-40.
       M-65.
           IF  ERR-STAT NOT = "24"
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
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           MOVE "TDNWF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNWF_PNAME1 " " BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           GO TO M-60.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCF_IDLST WMJCF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET-RTN.
           IF  W-MDDD = SPACE OR ZERO
               GO TO SET-EX
           END-IF
           MOVE 11 TO CNT1.
           MOVE  9 TO CNT2.
       SET-10.
           SUBTRACT 1 FROM CNT1.
           IF  CNT1 = 0
               GO TO SET-90
           END-IF
           IF  W-MD(CNT1) = SPACE
               GO TO SET-10
           END-IF
           IF  W-MD(CNT1) = "/"
               GO TO SET-20
           END-IF
           SUBTRACT 1 FROM CNT2.
           IF  CNT2 = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-MD" E-MD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           MOVE W-MD(CNT1) TO W-DD(CNT2).
           GO TO SET-10.
       SET-20.
           COMPUTE CNT3 = CNT1 + 2.
           IF  W-MD(CNT3) = 0 OR 1 OR 2 OR 3 OR 4 OR 5
                             OR 6 OR 7 OR 8 OR 9
               GO TO SET-10
           END-IF
           SUBTRACT 1 FROM CNT2.
           IF  CNT2 = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-MD" E-MD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           GO TO SET-10.
       SET-90.
           IF  W-NGP = ZERO
               GO TO SET-EX
           END-IF
           IF (W-NEN < 15) OR (W-GET < 1 OR > 12) OR (W-PEY < 1 OR > 31)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-MD" E-MD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       SET-EX.
           EXIT.
       CHK-RTN.
           MOVE ZERO TO W-NGPL.
           MOVE W-SNGP TO W-DATE.
           MOVE 20 TO W-NEN1.
           SUBTRACT 1 FROM W-GETD.
           IF  W-GETD = ZERO
               MOVE 12 TO W-GETD
               SUBTRACT 1 FROM W-NEN2
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" TDNWRF_PNAME1 " " BY REFERENCE TDNWRF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" WMJCF_PNAME1 " " BY REFERENCE WMJCF_IDLST "0".
       CHK-010.
      *           READ WMJCF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" WMJCF_PNAME1 BY REFERENCE WMJC-R " " RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO CHK-090
           END-IF
           MOVE ZERO TO W-KEY.
           MOVE WMJC-029 TO W-STC1.
           MOVE WMJC-043 TO W-STC2.
           MOVE WMJC-037 TO W-DNO.
           MOVE 0 TO W-DGN.
       CHK-020.
      *           READ TDNWRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TDNWRF_PNAME1 BY REFERENCE TDNWR-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-090
           END-IF
           IF  TDNWR-DATE < W-NGPL
               GO TO CHK-020
           END-IF
           IF  TDNWR-KEY NOT = W-KEY
               GO TO CHK-020
           END-IF
           CALL "C3_Set_Jrcode" USING
            USER_ID BY REFERENCE COMPLETION_CODE 255.
           CALL "SD_Output" USING "E-ME7" E-ME7 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEYW" E-KEYW "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       CHK-090.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWRF_IDLST TDNWRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WMJCF_IDLST WMJCF_PNAME1.
       CHK-EX.
           EXIT.
