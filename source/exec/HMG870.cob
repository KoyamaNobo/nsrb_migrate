       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMG870.
      *********************************************************
      *    履物出荷トランワーク　作成　（出荷分）             *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-END              PIC  9(001) VALUE 0.
       77  W-FILE             PIC  X(013).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID1.
           02  W-FID11        PIC  X(006) VALUE "WK0128".
           02  W-FID12        PIC  X(003).
       01  W-FID2.
           02  W-FID21        PIC  X(006) VALUE "WK0256".
           02  W-FID22        PIC  X(003).
       01  W-FID3.
           02  W-FID31        PIC  X(006) VALUE "WK0512".
           02  W-FID32        PIC  X(003).
       01  W-DATA.
           02  W-RE           PIC  9(001).
           02  CNT            PIC  9(002).
           02  CHK            PIC  9(001).
           02  W-SU           PIC S9(004).
           02  W-SUD          PIC S9(006).
           02  W-ASU          PIC S9(006).
           02  W-AKIN         PIC S9(008).
       01  ERR-STAT           PIC  X(002).
      *
           COPY LIHIM.
      *FD  ZCMF
       01  ZCMF_HMG870.
           02  ZCMF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  ZCMF_LNAME     PIC  X(011) VALUE "ZCMF_HMG870".
           02  F              PIC  X(001).
           02  ZCMF_KEY1      PIC  X(100) VALUE SPACE.
           02  ZCMF_SORT      PIC  X(100) VALUE SPACE.
           02  ZCMF_IDLST     PIC  X(100) VALUE SPACE.
           02  ZCMF_RES       USAGE  POINTER.
       01  ZCM-R.
           02  ZCM-KEY.
             03  ZCM-MHCD     PIC  9(006).
             03  ZCM-HCD      PIC  9(006).
             03  ZCM-SIZ      PIC  9(001).
           02  ZCM-ASU.
             03  ZCM-SUD   OCCURS  10.
               04  ZCM-SU     PIC S9(006).
           02  F              PIC  X(439).
       77  F                  PIC  X(001).
      *FD  ZCOF
       01  ZCOF_HMG870.
           02  ZCOF_PNAME1    PIC  X(004) VALUE "ZCOF".
           02  F              PIC  X(001).
           02  ZCOF_LNAME     PIC  X(011) VALUE "ZCOF_HMG870".
           02  F              PIC  X(001).
           02  ZCOF_KEY1      PIC  X(100) VALUE SPACE.
           02  ZCOF_SORT      PIC  X(100) VALUE SPACE.
           02  ZCOF_IDLST     PIC  X(100) VALUE SPACE.
           02  ZCOF_RES       USAGE  POINTER.
       01  ZCO-R.
           02  ZCO-KEY.
             03  ZCO-MHCD     PIC  9(006).
             03  ZCO-HCD      PIC  9(006).
             03  ZCO-SIZ      PIC  9(001).
           02  ZCO-ASU.
             03  ZCO-SUD   OCCURS  10.
               04  ZCO-SU     PIC S9(006).
           02  ZCO-CSU        PIC S9(006).
           02  F              PIC  X(006).
       77  F                  PIC  X(001).
      *FD  STROF
       01  STROF_HMG870.
           02  STROF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  STROF_LNAME    PIC  X(012) VALUE "STROF_HMG870".
           02  F              PIC  X(001).
           02  STROF_KEY1     PIC  X(100) VALUE SPACE.
           02  STROF_SORT     PIC  X(100) VALUE SPACE.
           02  STROF_IDLST    PIC  X(100) VALUE SPACE.
           02  STROF_RES      USAGE  POINTER.
       01  STRO-R.
           02  STRO-DNO       PIC  9(006).
           02  STRO-GNO       PIC  9(001).
           02  STRO-DATE      PIC  9(008).
           02  STRO-TCD       PIC  9(004).
           02  STRO-HCD       PIC  9(006).
           02  STRO-SIZ       PIC  9(001).
           02  STRO-ASUD.
             03  STRO-SUD   OCCURS  10.
               04  STRO-SU    PIC S9(004) COMP-3.
           02  STRO-SUT       PIC S9(005).
           02  STRO-BT        PIC  9(005).
           02  STRO-KIN       PIC S9(008).
           02  STRO-CSC       PIC  9(001).
           02  STRO-DC        PIC  9(001).
           02  STRO-FT        PIC  9(005).
           02  STRO-CCD       PIC  9(003).
           02  STRO-BC        PIC  9(006).
           02  STRO-SOC       PIC  9(001).
           02  STRO-TNC       PIC  9(002).
           02  STRO-FKC       PIC  9(002).
           02  STRO-SHZ       PIC  9(001).
           02  STRO-KSU       PIC  9(003).
           02  STRO-FRC       PIC  9(001).
           02  STRO-TCD2      PIC  9(004).
           02  STRO-GBI       PIC  X(010).
           02  STRO-SKD       PIC  9(008).
           02  STRO-BMC       PIC  9(002).
           02  STRO-BMNO      PIC  9(001).
           02  F              PIC  X(001).
           02  STRO-DHC       PIC  9(001).
           02  STRO-SNC       PIC  9(001).
           02  F              PIC  X(128).
       77  F                  PIC  X(001).
      *FD  STRNF
       01  STRNF_HMG870.
           02  STRNF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  STRNF_LNAME    PIC  X(012) VALUE "STRNF_HMG870".
           02  F              PIC  X(001).
           02  STRNF_KEY1     PIC  X(100) VALUE SPACE.
           02  STRNF_SORT     PIC  X(100) VALUE SPACE.
           02  STRNF_IDLST    PIC  X(100) VALUE SPACE.
           02  STRNF_RES      USAGE  POINTER.
       01  STRN-R.
           02  STRN-DNO       PIC  9(006).
           02  STRN-GNO       PIC  9(001).
           02  STRN-DATE      PIC  9(008).
           02  STRN-TCD       PIC  9(004).
           02  STRN-HCD       PIC  9(006).
           02  STRN-SIZ       PIC  9(001).
           02  STRN-ASUD.
             03  STRN-SUD   OCCURS  10.
               04  STRN-SU    PIC S9(004) COMP-3.
           02  STRN-SUT       PIC S9(005).
           02  STRN-BT        PIC  9(005).
           02  STRN-KIN       PIC S9(008).
           02  STRN-CSC       PIC  9(001).
           02  STRN-DC        PIC  9(001).
           02  STRN-FT        PIC  9(005).
           02  STRN-CCD       PIC  9(003).
           02  STRN-BC        PIC  9(006).
           02  STRN-SOC       PIC  9(001).
           02  STRN-TNC       PIC  9(002).
           02  STRN-FKC       PIC  9(002).
           02  STRN-SHZ       PIC  9(001).
           02  STRN-KSU       PIC  9(003).
           02  STRN-FRC       PIC  9(001).
           02  STRN-TCD2      PIC  9(004).
           02  STRN-GBI       PIC  X(010).
           02  STRN-SKD       PIC  9(008).
           02  STRN-BMC       PIC  9(002).
           02  STRN-BMNO      PIC  9(001).
           02  F              PIC  X(001).
           02  STRN-DHC       PIC  9(001).
           02  STRN-SNC       PIC  9(001).
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
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　出荷トランワーク　作成　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME4   PIC  X(026) VALUE
                  "***  ZCOF REWRITE ｴﾗｰ  ***".
             03  E-ME5   PIC  X(016) VALUE
                  "***  HIM ﾅｼ  ***".
             03  E-ME6   PIC  X(021) VALUE
                  "***  HIM ｻｲｽﾞ ﾅｼ  ***".
             03  E-ME7.
               04  FILLER   PIC  X(025) VALUE
                    "***  TOTAL ｶﾞ O ﾃﾞﾅｲ  ***".
               04  02E-ME7  PIC ----,--9 .
               04  03E-ME7  PIC ---,---,--9 .
             03  E-ME8   PIC  X(025) VALUE
                  "***  ﾌﾘｶｴﾀﾝｶ ｶﾞ ZERO  ***".
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
            "C-MID" " " "0" "0" "294" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "167" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "167" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "26" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "16" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "21" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" " " "24" "0" "44" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME7" "X" "24" "15" "25" " " "E-ME7"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME7" "----,--9" "24" "41" "8" "01E-ME7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME7" BY REFERENCE W-ASU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03E-ME7" "---,---,--9" "24" "50" "11" "02E-ME7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03E-ME7" BY REFERENCE W-AKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "25" "E-ME7" " "  RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-ASU W-AKIN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID12 W-FID22 W-FID32.
           MOVE W-FID2 TO WK0256ID.
           MOVE WK0256ID TO STROF_PNAME1.
           MOVE W-FID1 TO WK0128ID.
           MOVE WK0128ID TO STRNF_PNAME1.
           MOVE W-FID3 TO WK0512ID.
           MOVE WK0512ID TO ZCMF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" STRNF_PNAME1 " " BY REFERENCE STRNF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" ZCMF_PNAME1 " " BY REFERENCE ZCMF_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" ZCOF_PNAME1 " " BY REFERENCE ZCOF_IDLST "1"
            "ZCO-KEY" BY REFERENCE ZCO-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
      *           READ ZCMF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" ZCMF_PNAME1 BY REFERENCE ZCM-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           MOVE ZERO TO CNT.
       M-15.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO M-10
           END-IF.
       M-20.
           IF  ZCM-SU(CNT) >= ZERO
               GO TO M-15
           END-IF
           COMPUTE W-SUD = -1 * ZCM-SU(CNT).
      *
           MOVE SPACE TO ZCO-KEY.
           MOVE ZCM-MHCD TO ZCO-MHCD.
      *           START ZCOF KEY NOT < ZCO-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            ZCOF_PNAME1 "ZCO-KEY" " NOT < " ZCO-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-10
           END-IF.
       M-25.
      *           READ ZCOF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" ZCOF_PNAME1 BY REFERENCE ZCO-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-15
           END-IF
           IF  ZCM-MHCD NOT = ZCO-MHCD
               GO TO M-15
           END-IF
           IF  ZCM-SIZ NOT = ZCO-SIZ
               GO TO M-25
           END-IF
           IF  ZCO-SU(CNT) = ZERO
               GO TO M-25
           END-IF
      *
           MOVE 0 TO W-RE.
           CALL "DB_F_Open" USING
            "INPUT" STROF_PNAME1 " " BY REFERENCE STROF_IDLST "0".
       M-30.
      *           READ STROF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" STROF_PNAME1 BY REFERENCE STRO-R " " RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-RE
               GO TO M-50
           END-IF
           IF  STRO-SNC = 1
               GO TO M-30
           END-IF
           IF  STRO-DC = 3 OR 4
               GO TO M-30
           END-IF
           IF  ZCM-HCD NOT = STRO-HCD
               GO TO M-30
           END-IF
           MOVE ZCO-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE STROF_IDLST STROF_PNAME1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  ZCM-SIZ NOT = STRO-SIZ
               GO TO M-30
           END-IF
           IF  STRO-SU(CNT) = ZERO
               GO TO M-30
           END-IF
           IF  STRO-DC = 1 OR 2
               MOVE STRO-SU(CNT) TO W-SU
               GO TO M-35
           END-IF
      *
           IF  STRO-SU(CNT) <= W-SUD AND <= ZCO-SU(CNT)
               MOVE STRO-SU(CNT) TO W-SU
           END-IF
           IF  W-SUD <= STRO-SU(CNT) AND <= ZCO-SU(CNT)
               MOVE W-SUD TO W-SU
           END-IF
           IF  ZCO-SU(CNT) <= W-SUD AND <= STRO-SU(CNT)
               MOVE ZCO-SU(CNT) TO W-SU
           END-IF
           IF  W-SU = ZERO
               GO TO M-30
           END-IF.
       M-35.
           IF  HI-S(ZCM-SIZ,CNT) = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE STROF_IDLST STROF_PNAME1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-40.
           MOVE ZERO TO STRN-R.
           MOVE ZERO TO STRN-SU(01) STRN-SU(02) STRN-SU(03) STRN-SU(04)
                        STRN-SU(05) STRN-SU(06) STRN-SU(07) STRN-SU(08)
                        STRN-SU(09) STRN-SU(10).
           MOVE STRO-TCD TO STRN-TCD.
           MOVE STRO-TCD2 TO STRN-TCD2.
           MOVE STRO-HCD TO STRN-HCD.
           MOVE STRO-SIZ TO STRN-SIZ.
           COMPUTE STRN-SU(CNT) = -1 * W-SU.
           MOVE STRN-SU(CNT) TO STRN-SUT.
           MOVE STRO-BT TO STRN-BT.
           COMPUTE STRN-KIN = STRN-SUT * STRN-BT.
           MOVE STRO-CSC TO STRN-CSC.
           MOVE STRO-DC TO STRN-DC.
           MOVE STRO-FT TO STRN-FT.
           MOVE STRO-BC TO STRN-BC.
           MOVE STRO-SOC TO STRN-SOC.
           MOVE STRO-TNC TO STRN-TNC.
           MOVE STRO-FKC TO STRN-FKC.
           MOVE STRO-SHZ TO STRN-SHZ.
           MOVE 1 TO STRN-FRC.
           MOVE STRO-BMC TO STRN-BMC.
           MOVE STRO-BMNO TO STRN-BMNO.
           MOVE STRO-SNC TO STRN-SNC.
           MOVE SPACE TO STRN-GBI.
      *           WRITE STRN-R.
      *//////////////
           CALL "DB_Insert" USING
            STRNF_PNAME1 STRNF_LNAME STRN-R RETURNING RET.
           ADD STRN-SUT TO W-ASU.
           ADD STRN-KIN TO W-AKIN.
      *
           IF  STRO-DC NOT = 2
               IF  STRO-DC = 0 OR 7
                   ADD W-SU TO ZCM-SU(CNT)
               ELSE
                   SUBTRACT W-SU FROM ZCM-SU(CNT)
               END-IF
           END-IF
      *
           MOVE ZERO TO STRN-R.
           MOVE ZERO TO STRN-SU(01) STRN-SU(02) STRN-SU(03) STRN-SU(04)
                        STRN-SU(05) STRN-SU(06) STRN-SU(07) STRN-SU(08)
                        STRN-SU(09) STRN-SU(10).
           MOVE STRO-TCD TO STRN-TCD.
           MOVE STRO-TCD2 TO STRN-TCD2.
           MOVE ZCO-HCD TO STRN-HCD.
           MOVE STRO-SIZ TO STRN-SIZ.
           MOVE W-SU TO STRN-SU(CNT) STRN-SUT.
           MOVE STRO-BT TO STRN-BT.
           COMPUTE STRN-KIN = STRN-SUT * STRN-BT.
           MOVE STRO-CSC TO STRN-CSC.
           MOVE STRO-DC TO STRN-DC.
           IF  STRO-DC NOT = 2
               MOVE HI-FT TO STRN-FT
               IF  STRN-FT = ZERO
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "E-ME8" E-ME8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-45
               END-IF
           END-IF
           MOVE HI-BC TO STRN-BC.
           MOVE HI-BMC TO STRN-BMC.
           MOVE HI-BMNO TO STRN-BMNO.
           MOVE STRO-SOC TO STRN-SOC.
           MOVE STRO-TNC TO STRN-TNC.
           MOVE STRO-FKC TO STRN-FKC.
           MOVE STRO-SHZ TO STRN-SHZ.
           MOVE 1 TO STRN-FRC.
           MOVE STRO-SNC TO STRN-SNC.
           MOVE SPACE TO STRN-GBI.
      *           WRITE STRN-R.
      *//////////////
           CALL "DB_Insert" USING
            STRNF_PNAME1 STRNF_LNAME STRN-R RETURNING RET.
           ADD STRN-SUT TO W-ASU.
           ADD STRN-KIN TO W-AKIN.
      *
           IF  STRO-DC = 2
               GO TO M-45
           END-IF
           IF  STRO-DC = 0 OR 7
               ADD W-SU TO ZCO-CSU
               SUBTRACT W-SU FROM ZCO-SU(CNT)
           ELSE
               SUBTRACT W-SU FROM ZCO-CSU
               ADD W-SU TO ZCO-SU(CNT)
           END-IF
      *           REWRITE ZCO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            ZCOF_PNAME1 ZCOF_LNAME ZCO-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE STROF_IDLST STROF_PNAME1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-45.
           IF  ERR-STAT = 255
               GO TO M-90
           END-IF
           IF  W-END = 0
               MOVE 1 TO W-END
           END-IF
           COMPUTE W-SUD = -1 * ZCM-SU(CNT).
           IF  ZCM-SU(CNT) < ZERO
               GO TO M-30
           END-IF.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE STROF_IDLST STROF_PNAME1.
           IF  W-RE = 1
               GO TO M-25
           END-IF
           GO TO M-20.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE STRNF_IDLST STRNF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZCMF_IDLST ZCMF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ZCOF_IDLST ZCOF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  W-END = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF (W-ASU NOT = ZERO) OR (W-AKIN NOT = ZERO)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
