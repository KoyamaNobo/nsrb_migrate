       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS42U.
      *********************************************************
      *    PROGRAM         :  ナフコＥＯＳ自動指図            *
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
           02  W-DC           PIC  9(001).
           02  W-EC           PIC  9(001).
           02  W-OKC          PIC  9(001).
           02  W-D.
             03  W-DNO        PIC  9(007).
             03  W-HJC        PIC  9(002).
             03  W-CCD        PIC  9(003).
             03  W-NHB        PIC  9(001).
           02  W-DUR.
             03  W-DURD       PIC  X(010).
             03  F            PIC  X(016).
           COPY LSTAT.
      *
           COPY LITDNN.
           COPY LICODE.
           COPY LIHIM2.
           COPY LITCM.
      *FD  SHNW
       01  SHNW_JHS42U.
           02  SHNW_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  SHNW_LNAME     PIC  X(011) VALUE "SHNW_JHS42U".
           02  F              PIC  X(001).
           02  SHNW_KEY1      PIC  X(100) VALUE SPACE.
           02  SHNW_SORT      PIC  X(100) VALUE SPACE.
           02  SHNW_IDLST     PIC  X(100) VALUE SPACE.
           02  SHNW_RES       USAGE  POINTER.
       01  SHNW-R.
           02  SHNW-HJC       PIC  9(002).
           02  SHNW-CCD       PIC  9(003).
           02  SHNW-DNO       PIC  9(007).
           02  SHNW-HCD       PIC  9(006).
           02  SHNW-SIZ       PIC  9(001).
           02  SHNW-ASU.
             03  SHNW-SUD   OCCURS  10.
               04  SHNW-SU    PIC S9(004).
           02  SHNW-ISU       PIC  9(003).
           02  SHNW-NHB       PIC  9(001).
           02  SHNW-NFN       PIC  9(001). 
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
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　ナフコ指図変換ワーク作成　　＊＊＊".
           02  FILLER  PIC  X(22)
                   VALUE "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-OKC     PIC  9(01).
       01  DSP-AREA.
           02  FILLER.
             03  DSP-CCS.
               04  FILLER  PIC  N(004) VALUE "直送なし".
               04  FILLER  PIC  9(003).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  X(015).
             03  DSP-HNM.
               04  FILLER  PIC  N(004) VALUE "品名なし".
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  X(025).
               04  FILLER  PIC  X(025).
             03  DSP-SIZ.
               04  FILLER  PIC  X(008) VALUE "ｻｲｽﾞなし".
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  X(025).
               04  FILLER  PIC  X(025).
             03  DSP-ISU.
               04  FILLER  PIC  N(004) VALUE "入数なし".
               04  FILLER  PIC  9(006).
               04  FILLER  PIC  N(024).
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
            "C-MID" " " "0" "0" "66" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID" "N" "1" "15" "44" " " "C-MID" RETURNING RESU.
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
            "DSP-AREA" " " "0" "0" "321" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" " " "W-L" "0" "321" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-CCS" " " "W-L" "0" "41" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CCS" "N" "W-L" "4" "8" " " "DSP-CCS" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-CCS" "9" "W-L" "13" "3" "01DSP-CCS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02DSP-CCS" BY REFERENCE TDNN1-TCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-CCS" "X" "W-L" "17" "15" "02DSP-CCS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03DSP-CCS" BY REFERENCE TDNN1-SNA "15" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-CCS" "X" "W-L" "33" "15" "03DSP-CCS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04DSP-CCS" BY REFERENCE TDNN1-TNA "15" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HNM" " " "W-L" "0" "71" "DSP-CCS" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-HNM" "N" "W-L" "4" "8" " " "DSP-HNM" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-HNM" "X" "W-L" "13" "13" "01DSP-HNM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02DSP-HNM" BY REFERENCE TDNN2-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-HNM" "X" "W-L" "27" "25" "02DSP-HNM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03DSP-HNM" BY REFERENCE TDNN2-SHN "25" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-HNM" "X" "W-L" "53" "25" "03DSP-HNM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04DSP-HNM" BY REFERENCE TDNN2-KKK "25" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SIZ" " " "W-L" "0" "71" "DSP-HNM" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-SIZ" "X" "W-L" "4" "8" " " "DSP-SIZ" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-SIZ" "X" "W-L" "13" "13" "01DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02DSP-SIZ" BY REFERENCE TDNN2-JAN "13" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-SIZ" "X" "W-L" "27" "25" "02DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03DSP-SIZ" BY REFERENCE TDNN2-SHN "25" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-SIZ" "X" "W-L" "53" "25" "03DSP-SIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04DSP-SIZ" BY REFERENCE TDNN2-KKK "25" "0" RETURNING RESU.
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
            "DSP-CLE" " " "W-L" "0" "76" "DSP-ISU" " " RETURNING RESU.
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
           MOVE WK0064ID TO SHNW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" SHNW_PNAME1 " " BY REFERENCE SHNW_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE ZERO TO W-D W-DC.
           MOVE 3 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-15.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
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
           IF  TDNN1-HC NOT = 0
               GO TO M-15
           END-IF
           IF  TDNN1-DGN NOT = ZERO
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-20.
           MOVE TDNN1-DNOD TO W-DNO.
           MOVE TDNN1-SCD TO W-HJC.
           MOVE TDNN1-TCD TO W-CCD.
           IF  W-CCD NOT = 196
               MOVE 0 TO W-NHB
           ELSE
               MOVE TDNN1-DUR TO W-DUR
               IF  W-DURD = "ﾉｳﾋﾝﾊﾞｼｮ:0"
                   MOVE 1 TO W-NHB
               ELSE
                   IF  W-DURD = "ﾉｳﾋﾝﾊﾞｼｮ:1" OR "ﾉｳﾋﾝﾊﾞｼｮ:2"
                       MOVE 2 TO W-NHB
                   ELSE
                       MOVE 0 TO W-NHB
                   END-IF
               END-IF
           END-IF
           IF  W-CCD = 081
               IF  TDNN1-NHB = 1
                   MOVE 1 TO W-NHB
                   MOVE 981 TO W-CCD
               END-IF
           END-IF
           MOVE 5000 TO TC-TCD.
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
               MOVE 1 TO TC-NFN
               PERFORM MSG-RTN THRU MSG-EX
           END-IF.
       M-25.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  TDNN1-HC NOT = 0
               GO TO M-25
           END-IF
           IF  TDNN1-DGN = ZERO
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF.
       M-30.
           IF  TDNN2-TSU = ZERO
               GO TO M-35
           END-IF
           MOVE SPACE TO CODE-KEY.
           MOVE ZERO TO CODE-TCD.
           MOVE TDNN2-JAN TO CODE-JAN.
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
       M-32.
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
           IF (CODE-TCD NOT = ZERO) OR (TDNN2-JAN NOT = CODE-JAN)
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
           END-IF
           MOVE 0 TO HI-S(4,10).
           IF  HI-S(CODE-SIZ,CODE-SNO) = 0
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 4 TO W-EC
               PERFORM MSG-RTN THRU MSG-EX
           END-IF
      *
           IF  W-ERR NOT = 0
               GO TO M-35
           END-IF
           MOVE ZERO TO SHNW-R.
           MOVE W-HJC TO SHNW-HJC.
           MOVE W-CCD TO SHNW-CCD.
           MOVE W-DNO TO SHNW-DNO.
           MOVE CODE-HCD TO SHNW-HCD.
           MOVE CODE-SIZ TO SHNW-SIZ.
           MOVE TDNN2-SU TO SHNW-SU(CODE-SNO).
           MOVE HI-ISU TO SHNW-ISU.
           MOVE W-NHB TO SHNW-NHB.
           MOVE TC-NFN TO SHNW-NFN.
      *           WRITE SHNW-R.
      *//////////////
           CALL "DB_Insert" USING
            SHNW_PNAME1 SHNW_LNAME SHNW-R RETURNING RET.
           IF  W-DC = ZERO
               MOVE 1 TO W-DC
           END-IF.
       M-35.
      *           READ TDNNF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-50
           END-IF
           IF  TDNN1-HC NOT = 0
               GO TO M-35
           END-IF
           IF  TDNN1-DGN = ZERO
               GO TO M-20
           END-IF
           GO TO M-30.
       M-50.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
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
           CALL "DB_F_Close" USING BY REFERENCE SHNW_IDLST SHNW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNNF_IDLST TDNNF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
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
