       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JTN06U.
      *********************************************************
      *    PROGRAM         :  トラスコ他指図変換ワーク　作成  *
      *    PRINTER TYPE    :  JIPS                            *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-TCD          PIC  9(004).
           02  W-NGP          PIC  9(006).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-DATE         PIC  9(006).
           02  W-SNGP         PIC  9(006).
           02  W-ENGP         PIC  9(006).
           02  W-DMM          PIC  9(001).
           02  W-CHK          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIHIM2.
           COPY LITCM.
           COPY L-TDIF.
      *FD  TSHW
       01  TSHW_JTN06U.
           02  TSHW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TSHW_LNAME     PIC  X(011) VALUE "TSHW_JTN06U".
           02  F              PIC  X(001).
           02  TSHW_KEY1      PIC  X(100) VALUE SPACE.
           02  TSHW_SORT      PIC  X(100) VALUE SPACE.
           02  TSHW_IDLST     PIC  X(100) VALUE SPACE.
           02  TSHW_RES       USAGE  POINTER.
       01  TSHW-R.
           02  TSHW-DATE      PIC  9(006).
           02  TSHW-TCD       PIC  9(004).
           02  TSHW-CCD       PIC  9(003).
           02  TSHW-TPC       PIC  9(004).
           02  TSHW-SOK       PIC  9(001).
           02  TSHW-UNS       PIC  9(001).
           02  TSHW-HNO       PIC  X(010).
           02  TSHW-JNOD.
             03  TSHW-JNO     PIC  9(006).
             03  TSHW-JGN     PIC  9(001).
           02  TSHW-ISU       PIC  9(003).
           02  TSHW-HCD       PIC  9(006).
           02  TSHW-SIZ       PIC  9(001).
           02  TSHW-ASU.
             03  TSHW-SUD   OCCURS  10.
               04  TSHW-SU    PIC S9(004).
           02  TSHW-SUT       PIC S9(005).
           02  TSHW-TEKI      PIC  N(028).
           02  TSHW-TED   REDEFINES TSHW-TEKI.
             03  TSHW-THT     PIC  N(009).
             03  TSHW-TTE     PIC  N(019).
           02  TSHW-DC        PIC  9(001).
           02  F              PIC  X(108).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　トラスコ他指図変換ワーク　作成　　＊＊＊".
           02  FILLER  PIC  X(012) VALUE
                "得意先      ".
           02  FILLER  PIC  X(024) VALUE
                "指図日   '  年   月   日".
           02  FILLER.
             03  FILLER  PIC  N(002) VALUE "確認".
             03  FILLER  PIC  X(023) VALUE
                  "(OK=1,NO=9) --->   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-NEN   PIC  9(002).
             03  A-GET   PIC  9(002).
             03  A-PEY   PIC  9(002).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NAME  PIC  N(026).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  TDIF REWRITE ｴﾗｰ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-KEY   PIC  X(007).
             03  E-ME4.
               04  FILLER  PIC  X(026) VALUE
                    "***  ﾋﾝﾒｲ ﾅｼ (      )  ***".
               04  FILLER  PIC  9(006).
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
            "C-MID" " " "0" "0" "113" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "10" "10" "12" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "12" "10" "24" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" " " "23" "0" "27" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0104C-MID" "N" "23" "43" "4" " " "04C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "0204C-MID" "X" "23" "47" "23" "0104C-MID" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "10" "18" "4" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "12" "0" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "12" "20" "2" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "12" "25" "2" "A-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "12" "30" "2" "A-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "10" "23" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "100" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" " " "24" "0" "32" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME4" "X" "24" "15" "26" " " "E-ME4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME4" "9" "24" "29" "6" "01E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME4" BY REFERENCE TDI-ISU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "7" "E-ME4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TDI-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT W-DATE FROM DATE.
           MOVE W-DATE TO W-NGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE W-NGP TO W-SNGP.
           MOVE W-DATE TO W-NGP.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
           END-IF
           MOVE W-NGP TO W-ENGP.
           MOVE W-DATE TO W-NGP.
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
      *
           MOVE W-TCD TO TC-TCD.
           MOVE 1 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           IF  W-TCD NOT = 4990 AND 5000 AND 9850 AND 6010
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-25
           END-IF
           IF  W-NGP < W-SNGP OR > W-ENGP
               GO TO M-15
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               GO TO M-95
           END-IF
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
       M-35.
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDIF_IDLST TDIF_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  200
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TDI-UPC NOT = 0
               GO TO M-35
           END-IF
           IF  TDI-PRC = 0
               GO TO M-35
           END-IF
           IF  TDI-TCD NOT = W-TCD
               GO TO M-35
           END-IF
           IF  TDI-DATE NOT = W-NGP
               GO TO M-35
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO TSHW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" TSHW_PNAME1 " " BY REFERENCE TSHW_IDLST "0".
       M-40.
           MOVE 0 TO W-CHK.
           MOVE TDI-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-CHK
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
      *
           INITIALIZE TSHW-R.
           MOVE SPACE TO TSHW-HNO TSHW-TEKI.
           MOVE TDI-DATE TO TSHW-DATE.
           MOVE TDI-TCD TO TSHW-TCD.
           MOVE TDI-CCD TO TSHW-CCD.
           MOVE TDI-TPC TO TSHW-TPC.
           MOVE TDI-SOK TO TSHW-SOK.
           MOVE TDI-UNS TO TSHW-UNS.
           MOVE TDI-HNO TO TSHW-HNO.
           MOVE TDI-JNOD TO TSHW-JNOD.
           IF  W-CHK = 0
               MOVE HI-ISU TO TSHW-ISU
           ELSE
               MOVE TDI-ISU TO TSHW-ISU
           END-IF
           MOVE TDI-HCD TO TSHW-HCD.
           MOVE TDI-SKB TO TSHW-SIZ.
           MOVE TDI-SU TO TSHW-SU(TDI-SNO) TSHW-SUT.
           MOVE TDI-TEKI TO TSHW-TEKI.
           IF  TDI-SU < ZERO
               MOVE 1 TO TSHW-DC
           END-IF
      *           WRITE TSHW-R.
      *//////////////
           CALL "DB_Insert" USING
            TSHW_PNAME1 TSHW_LNAME TSHW-R RETURNING RET.
      *
           IF  W-CHK = 0
               MOVE HI-ISU TO TDI-ISU
           END-IF
           MOVE 2 TO TDI-PRC.
      *           REWRITE TDI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-45.
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TDI-UPC NOT = 0
               GO TO M-45
           END-IF
           IF  TDI-PRC = 0
               GO TO M-45
           END-IF
           IF  TDI-TCD NOT = W-TCD
               GO TO M-45
           END-IF
           IF  TDI-DATE NOT = W-NGP
               GO TO M-45
           END-IF
           GO TO M-40.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSHW_IDLST TSHW_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
