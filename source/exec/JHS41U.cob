       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS41U.
      *********************************************************
      *    PROGRAM         :  ワークマンＥＯＳ自動指図        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
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
       77  W-ERR              PIC  9(001) VALUE 0.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-L            PIC  9(002).
           02  W-EC           PIC  9(001).
           02  W-OKC          PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-D.
             03  W-OSN        PIC  9(001).
             03  W-CCD        PIC  9(003).
             03  W-TENC       PIC  9(004).
             03  W-ISU        PIC  9(003).
           COPY LSTAT.
      *
           COPY LITDNW.
           COPY LICODE.
           COPY LIHIM2.
           COPY LITCM.
      *FD  WTNAF
       01  WTNAF_JHS41U.
           02  WTNAF_PNAME1   PIC  X(005) VALUE "WTNAF".
           02  F              PIC  X(001).
           02  WTNAF_LNAME    PIC  X(012) VALUE "WTNAF_JHS41U".
           02  F              PIC  X(001).
           02  WTNAF_KEY1     PIC  X(100) VALUE SPACE.
           02  WTNAF_SORT     PIC  X(100) VALUE SPACE.
           02  WTNAF_IDLST    PIC  X(100) VALUE SPACE.
           02  WTNAF_RES      USAGE  POINTER.
       01  WTNA-R.
           02  WTNA-KEY.
             03  WTNA-TENC    PIC  9(004).
           02  WTNA-TENM      PIC  N(026).
           02  WTNA-OSN       PIC  9(001).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
      *FD  SHWW
       01  SHWW_JHS41U.
           02  SHWW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHWW_LNAME     PIC  X(011) VALUE "SHWW_JHS41U".
           02  F              PIC  X(001).
           02  SHWW_KEY1      PIC  X(100) VALUE SPACE.
           02  SHWW_SORT      PIC  X(100) VALUE SPACE.
           02  SHWW_IDLST     PIC  X(100) VALUE SPACE.
           02  SHWW_RES       USAGE  POINTER.
       01  SHWW-R.
           02  SHWW-CCD       PIC  9(003).
           02  SHWW-TENC      PIC  9(004).
           02  SHWW-ISU       PIC  9(003).
           02  SHWW-HCD       PIC  9(006).
           02  SHWW-SIZ       PIC  9(001).
           02  SHWW-ASU.
             03  SHWW-SUD   OCCURS  10.
               04  SHWW-SU    PIC S9(004).
           02  F              PIC  X(007).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  N(024) VALUE
                "＊＊＊　　ワークマン指図変換ワーク作成　　＊＊＊".
           02  FILLER  PIC  X(22)
                   VALUE "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-OKC     PIC  9(01).
       01  DSP-AREA.
           02  FILLER.
             03  DSP-CCS.
               04  FILLER  PIC  N(004) VALUE "直送なし".
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  X(020).
               04  FILLER  PIC  X(020).
             03  DSP-HNM.
               04  FILLER  PIC  N(004) VALUE "品名なし".
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  X(025).
             03  DSP-SIZ.
               04  FILLER  PIC  X(008) VALUE "ｻｲｽﾞなし".
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  X(025).
             03  DSP-ISU.
               04  FILLER  PIC  N(004) VALUE "入数なし".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
             03  DSP-TEN.
               04  FILLER  PIC  N(004) VALUE "店名なし".
               04  FILLER  PIC  9(004).
               04  FILLER  PIC  X(020).
               04  FILLER  PIC  X(020).
             03  DSP-CLE.
               04  FILLER  PIC  X(042) VALUE
                    "                                        ".
               04  FILLER  PIC  X(034) VALUE
                    "                                  ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
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
            "C-MID" " " "0" "0" "70" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "13" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "22" "43" "22" "01C-MID" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "22" "60" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "334" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "W-L" "0" "334" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-CCS" " " "W-L" "0" "52" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CCS" "N" "W-L" "4" "8" " " "DSP-CCS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CCS" "9" "W-L" "13" "4" "01DSP-CCS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-CCS" BY REFERENCE TDNW1-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-CCS" "X" "W-L" "18" "20" "02DSP-CCS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-CCS" BY REFERENCE TDNW1-SNA "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-CCS" "X" "W-L" "38" "20" "03DSP-CCS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-CCS" BY REFERENCE TDNW1-TNA "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HNM" " " "W-L" "0" "46" "DSP-CCS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HNM" "N" "W-L" "4" "8" " " "DSP-HNM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-HNM" "X" "W-L" "13" "13" "01DSP-HNM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-HNM" BY REFERENCE TDNW2-HCD "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-HNM" "X" "W-L" "27" "25" "02DSP-HNM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-HNM" BY REFERENCE TDNW2-SHN "25" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SIZ" " " "W-L" "0" "46" "DSP-HNM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-SIZ" "X" "W-L" "4" "8" " " "DSP-SIZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-SIZ" "X" "W-L" "13" "13" "01DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-SIZ" BY REFERENCE TDNW2-HCD "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-SIZ" "X" "W-L" "27" "25" "02DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-SIZ" BY REFERENCE TDNW2-SHN "25" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ISU" " " "W-L" "0" "62" "DSP-SIZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-ISU" "N" "W-L" "4" "8" " " "DSP-ISU" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-ISU" "9" "W-L" "13" "6" "01DSP-ISU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-ISU" BY REFERENCE HI-MHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-ISU" "N" "W-L" "20" "48" "02DSP-ISU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-ISU" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TEN" " " "W-L" "0" "52" "DSP-ISU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-TEN" "N" "W-L" "4" "8" " " "DSP-TEN" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-TEN" "9" "W-L" "13" "4" "01DSP-TEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-TEN" BY REFERENCE TDNW1-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-TEN" "X" "W-L" "18" "20" "02DSP-TEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-TEN" BY REFERENCE TDNW1-SNA "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-TEN" "X" "W-L" "38" "20" "03DSP-TEN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-TEN" BY REFERENCE TDNW1-TNA "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLE" " " "W-L" "0" "76" "DSP-TEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLE" "X" "W-L" "2" "42" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLE" "X" "W-L" "44" "34" "01DSP-CLE" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "35" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-OKC = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-OKC NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO SHWW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SHWW_PNAME1 " " BY REFERENCE SHWW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE ZERO TO W-D.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  TDNW1-HC NOT = 0
               GO TO M-15
           END-IF
           IF  TDNW1-DGN NOT = ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-20.
           MOVE TDNW1-TCD TO W-TENC.
           MOVE W-TENC TO WTNA-KEY.
      *           READ WTNAF UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 5 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-25
           END-IF
           IF  WTNA-OSN = 0
               MOVE 2 TO W-CCD
           END-IF
           IF  WTNA-OSN = 1
               MOVE 3 TO W-CCD
           END-IF
           MOVE 9850 TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 1 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
           END-IF.
       M-25.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  TDNW1-HC NOT = 0
               GO TO M-25
           END-IF
           IF  TDNW1-DGN = ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF.
       M-30.
           MOVE SPACE TO CODE-KEY.
           MOVE 9850 TO CODE-TCD.
           MOVE TDNW2-WCO TO CODE-WCO.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           IF (CODE-TCD NOT = 9850) OR (TDNW2-WCO NOT = CODE-WCO)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           MOVE CODE-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 2 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           IF  HI-ISU = ZERO
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 3 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
           IF  CODE-JAN = "5270500      "
               PERFORM SET-RTN THRU SET-EX
               GO TO M-35
           END-IF
           MOVE 0 TO HI-S(4,10).
           IF  HI-S(CODE-SIZ,CODE-SNO) = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 4 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
               GO TO M-35
           END-IF
      *
           IF  W-ERR NOT = 0
               GO TO M-35
           END-IF
           MOVE ZERO TO SHWW-R.
           MOVE W-CCD TO SHWW-CCD.
           MOVE W-TENC TO SHWW-TENC.
           MOVE HI-ISU TO SHWW-ISU.
           MOVE CODE-HCD TO SHWW-HCD.
           MOVE CODE-SIZ TO SHWW-SIZ.
           MOVE TDNW2-SU TO SHWW-SU(CODE-SNO).
      *           WRITE SHWW-R.
      *//////////////
           CALL "DB_Insert" USING
            SHWW_PNAME1 SHWW_LNAME SHWW-R RETURNING RET.
       M-35.
      *           READ TDNWF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  TDNW1-HC NOT = 0
               GO TO M-35
           END-IF
           IF  TDNW1-DGN = ZERO
               GO TO M-20
           END-IF
           GO TO M-30.
       M-50.
           IF  W-EC = 0
               GO TO M-90
           END-IF
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           IF  W-OKC NOT = 1
               GO TO M-50
           END-IF.
       M-90.
           CALL "DB_F_Close" USING BY REFERENCE SHWW_IDLST SHWW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNWF_IDLST TDNWF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       SET-RTN.
           MOVE 0 TO CNT.
       SET-010.
           ADD 1 TO CNT.
           IF  CNT > 7
               GO TO SET-EX
           END-IF
           MOVE ZERO TO SHWW-R.
           MOVE W-CCD TO SHWW-CCD.
           MOVE W-TENC TO SHWW-TENC.
           MOVE HI-ISU TO SHWW-ISU.
           MOVE CODE-HCD TO SHWW-HCD.
           IF  CNT = 7
               MOVE 1 TO SHWW-SIZ
           ELSE
               MOVE 4 TO SHWW-SIZ
           END-IF
           IF  CNT = 1
               MOVE 1 TO SHWW-SU(02)
           ELSE
               IF  CNT = 2
                   MOVE 1 TO SHWW-SU(03)
               ELSE
                   IF  CNT = 3
                       MOVE 2 TO SHWW-SU(04)
                   ELSE
                       IF  CNT = 4
                           MOVE 2 TO SHWW-SU(05)
                       ELSE
                           IF  CNT = 5
                               MOVE 2 TO SHWW-SU(06)
                           ELSE
                               IF  CNT = 6
                                   MOVE 1 TO SHWW-SU(07)
                               ELSE
                                   IF  CNT = 7
                                       MOVE 1 TO SHWW-SU(08)
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *           WRITE SHWW-R.
      *//////////////
           CALL "DB_Insert" USING
            SHWW_PNAME1 SHWW_LNAME SHWW-R RETURNING RET.
           GO TO SET-010.
       SET-EX.
           EXIT.
       MSG-RTN.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L = 20
               GO TO MSG-010
           END-IF
           IF  W-EC = 1
               CALL "SD_Output" USING
                "DSP-CCS" DSP-CCS "p" RETURNING RESU
           END-IF
           IF  W-EC = 2
               CALL "SD_Output" USING
                "DSP-HNM" DSP-HNM "p" RETURNING RESU
           END-IF
           IF  W-EC = 3
               CALL "SD_Output" USING
                "DSP-ISU" DSP-ISU "p" RETURNING RESU
           END-IF
           IF  W-EC = 4
               CALL "SD_Output" USING
                "DSP-SIZ" DSP-SIZ "p" RETURNING RESU
           END-IF
           IF  W-EC = 5
               CALL "SD_Output" USING
                "DSP-TEN" DSP-TEN "p" RETURNING RESU
           END-IF
           IF  W-EC = 0
               MOVE 1 TO W-EC
           END-IF
           GO TO MSG-EX.
       MSG-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO MSG-010
           END-IF
           IF  W-OKC NOT = 1
               GO TO MSG-010
           END-IF
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       MSG-020.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L < 21
               CALL "SD_Output" USING
                "DSP-CLE" DSP-CLE "p" RETURNING RESU
               GO TO MSG-020
           END-IF
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           GO TO MSG-RTN.
       MSG-EX.
           EXIT.
