       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS26U.
      *********************************************************
      *    PROGRAM         :  受注ＥＯＳ受信集計ワーク１作成  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  ﾜｰｸﾏﾝ=0 , ﾅﾌｺ=1                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN            PIC  9(001).
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  W-DATA.
           02  W-NAME         PIC  N(008).
           02  W-D.
             03  W-SEN        PIC  9(001).
             03  W-DMM        PIC  9(001).
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "          SS   S   M   L  LL28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5    ----".
           COPY LSTAT.
      *
           COPY LITDNW.
           COPY LITDNN.
           COPY LICODE.
      *FD  JKEIF
       01  JKEIF_JHS26U.
           02  JKEIF_PNAME1   PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  JKEIF_LNAME    PIC  X(012) VALUE "JKEIF_JHS26U".
           02  F              PIC  X(001).
           02  JKEIF_KEY1     PIC  X(100) VALUE SPACE.
           02  JKEIF_SORT     PIC  X(100) VALUE SPACE.
           02  JKEIF_IDLST    PIC  X(100) VALUE SPACE.
           02  JKEIF_RES      USAGE  POINTER.
       01  JKEI-R.
           02  JKEI-HCD       PIC  9(006).
           02  JKEI-ASUD.
             03  JKEI-ASU   OCCURS   4.
               04  JKEI-SUD   OCCURS  10.
                 05  JKEI-SU  PIC  9(005).
           02  F              PIC  X(049).
           02  JKEI-SIGN      PIC  9(001).
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
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　受注ＥＯＳ受信集計ワーク　作成　　＊＊＊".
           02  FILLER  PIC  X(033) VALUE
                  "指図変換  前 = 0  ,  後 = 1   ( )".
           02  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-DSP.
           02  D-NAME  PIC  N(008).
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME5.
               04  FILLER  PIC  X(027) VALUE
                    "***  ﾄｸｲｻｷｼｮｳﾋﾝｺｰﾄﾞ ﾅｼ  ***".
               04  FILLER  PIC  X(013).
             03  E-ME6.
               04  FILLER  PIC  X(028) VALUE
                    "***  ﾄｸｲｻｷｼｮｳﾋﾝｺｰﾄﾞ ｴﾗｰ  ***".
               04  FILLER  PIC  X(013).
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
            "C-MID" " " "0" "0" "105" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "15" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "12" "20" "33" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "20" "35" "22" "02C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "7" "33" "16" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE W-NAME "16" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "12" "51" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "52" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "116" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "116" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" " " "24" "0" "40" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME5" "X" "24" "15" "27" " " "E-ME5" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME5" "X" "24" "45" "13" "01E-ME5" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME5" BY REFERENCE TDNW2-HCD "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" " " "24" "0" "41" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME6" "X" "24" "15" "28" " " "E-ME6" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME6" "X" "24" "45" "13" "01E-ME6" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME6" BY REFERENCE TDNW2-HCD "13" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN = 0
               MOVE "［ワークマン］　" TO W-NAME
           ELSE
               IF  JS-SIGN = 1
                   MOVE "　［ナフコ］　　" TO W-NAME
               ELSE
                   CALL "DB_Close"
                   STOP RUN
               END-IF
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE ZERO TO W-D.
           PERFORM ACP-RTN THRU ACP-EX.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  JS-SIGN = 1
               GO TO M-50
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TDNWF_PNAME1 "SHARED" BY REFERENCE TDNWF_IDLST "1"
            "TDNW1-KEY" BY REFERENCE TDNW1-KEY.
       M-10.
      *           READ TDNWF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TDNW1-DGN = ZERO
               GO TO M-10
           END-IF
           IF  W-SEN = 0
               IF  TDNW1-HC NOT = 0
                   GO TO M-10
               END-IF
           END-IF
           IF  W-SEN NOT = 1
               IF  TDNW1-HC = 0
                   GO TO M-10
               END-IF
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JKEIF_PNAME1 " " BY REFERENCE JKEIF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
       M-20.
           MOVE SPACE TO CODE-KEY.
           MOVE 9850 TO CODE-TCD.
           MOVE TDNW2-HCD TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF (9850 NOT = CODE-TCD) OR (TDNW2-WCO NOT = CODE-WCO)
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF (CODE-HCD = ZERO) OR (CODE-SIZ = 0) OR (CODE-SNO = ZERO)
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-25
           END-IF
           PERFORM WRI-RTN THRU WRI-EX.
       M-25.
      *           READ TDNWF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNWF_PNAME1 BY REFERENCE TDNW-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TDNW1-DGN = ZERO
               GO TO M-25
           END-IF
           IF  W-SEN = 0
               IF  TDNW1-HC NOT = 0
                   GO TO M-25
               END-IF
           END-IF
           IF  W-SEN NOT = 1
               IF  TDNW1-HC = 0
                   GO TO M-25
               END-IF
           END-IF
           GO TO M-20.
       M-50.
           CALL "DB_F_Open" USING
            "INPUT" TDNNF_PNAME1 "SHARED" BY REFERENCE TDNNF_IDLST "1"
            "TDNN1-KEY" BY REFERENCE TDNN1-KEY.
       M-55.
      *           READ TDNNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_F_Close" USING
                BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  TDNN2-DGN = ZERO
               GO TO M-55
           END-IF
           IF  W-SEN = 0
               IF  TDNN1-HC NOT = 0
                   GO TO M-55
               END-IF
           END-IF
           IF  W-SEN NOT = 1
               IF  TDNN1-HC = 0
                   GO TO M-55
               END-IF
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JKEIF_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JKEIF_PNAME1 " " BY REFERENCE JKEIF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "2"
            "CODE-KEY" BY REFERENCE CODE-KEY "CODE-KEY2" BY REFERENCE
            CODE-KEY2.
       M-60.
           MOVE SPACE TO CODE-KEY.
           MOVE ZERO TO CODE-TCD.
           MOVE TDNN2-JAN TO CODE-JAN.
      *           START CODEF KEY NOT < CODE-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CODEF_PNAME1 "CODE-KEY" " NOT < " CODE-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-70
           END-IF.
       M-65.
      *           READ CODEF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CODEF_PNAME1 BY REFERENCE CODE-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-70
           END-IF
           IF (ZERO NOT = CODE-TCD) OR (TDNN2-JAN NOT = CODE-JAN)
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-70
           END-IF
           IF (CODE-HCD = ZERO) OR (CODE-SIZ = 0) OR (CODE-SNO = ZERO)
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-70
           END-IF
           PERFORM WRI-RTN THRU WRI-EX.
       M-70.
      *           READ TDNNF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNNF_PNAME1 BY REFERENCE TDNN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  TDNN2-DGN = ZERO
               GO TO M-70
           END-IF
           IF  W-SEN = 0
               IF  TDNN1-HC NOT = 0
                   GO TO M-70
               END-IF
           END-IF
           IF  W-SEN NOT = 1
               IF  TDNN1-HC = 0
                   GO TO M-70
               END-IF
           END-IF
           GO TO M-60.
       M-90.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE TDNWF_IDLST TDNWF_PNAME1
           ELSE
               IF  JS-SIGN = 1
                   CALL "DB_F_Close" USING
                    BY REFERENCE TDNNF_IDLST TDNNF_PNAME1
               END-IF
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JKEIF_IDLST JKEIF_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-SEN > 1
               GO TO ACP-RTN
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-050
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-050
           END-IF.
       ACP-EX.
           EXIT.
       WRI-RTN.
           MOVE ZERO TO JKEI-R.
           MOVE CODE-HCD TO JKEI-HCD.
           IF  JS-SIGN = 0
               MOVE TDNW2-SU TO JKEI-SU(CODE-SIZ,CODE-SNO)
           ELSE
               IF  JS-SIGN = 1
                   MOVE TDNN2-TSU TO JKEI-SU(CODE-SIZ,CODE-SNO)
               END-IF
           END-IF
           MOVE JS-SIGN TO JKEI-SIGN.
      *           WRITE JKEI-R.
      *//////////////
           CALL "DB_Insert" USING
            JKEIF_PNAME1 JKEIF_LNAME JKEI-R RETURNING RET.
       WRI-EX.
           EXIT.
