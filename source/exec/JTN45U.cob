       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT410U.
      *********************************************************
      *    PROGRAM         :  品名別出荷日報抽出　　　　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  91/09/13                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       INPUT-OUTPUT     SECTION.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT               PIC X(2).
       77  ERR-SW                 PIC 9(1)  VALUE 0.
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  W-JS                   PIC 9(01).
       01  W-JSD                  PIC 9(01).
       01  W-JS-MEI               PIC N(03).
       01  W-DATE                 PIC 9(08).
       01  W-AREA.
           02  I                  PIC  9(02).
      ***
       COPY  LWMSG.
      ***
           COPY   LTWK03.
           COPY   LJSTRR.
           COPY   L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLR-01.
           02  FILLER  PIC  X(01) VALUE " ".
      ***
       01  DSP-AREA.
           02  FILLER  PIC  X(20) VALUE
               " 品名別出荷日報抽出 " .
           02  FILLER  PIC  X(02) VALUE  "〔".
           02  FILLER  PIC  X(02) VALUE  "〕".
       01  DSP-AREA1.
           02  FILLER  PIC N(03).
      *
       01  DISP-MSG-SPACE1.
           02  FILLER  PIC X(40)  VALUE " ".
      ***
       COPY  LSMSG.
      ***
       PROCEDURE   DIVISION.
      ****************************
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *CLR-01
       CALL "SD_Init" USING 
            "CLR-01" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "24" "60" "1" " " "CLR-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "20" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "1" "1" "2" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "1" "9" "2" "02DSP-AREA" " "
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-AREA1" "N" "1" "3" "6" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-AREA1" BY REFERENCE W-JS-MEI "6" "0" RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "24" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE1" "X" "24" "1" "40" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE   SPACE   TO    JCON6-KEY.
           MOVE   6       TO    JCON6-01.
      *           READ   JCON    UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    "JCON"     TO  ERR-F
               MOVE    "I"        TO  ERR-M
               PERFORM    ERR-RTN    THRU    ERR-EX
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP    RUN
           END-IF
           MOVE   JCON6-08      TO  W-JS.
           MOVE   JCON6-09      TO  W-DATE.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           IF  W-JS  =  0
               MOVE  "教　育"     TO  W-JS-MEI
           END-IF
           IF  W-JS  =  1
               MOVE  "一　般"     TO  W-JS-MEI
           END-IF
           CALL "SD_Output" USING
            "DSP-AREA1" DSP-AREA1 "p" RETURNING RESU.
           PERFORM  GAMEN-RTN   THRU   GAMEN-EX.
           PERFORM  RED-RTN     THRU   RED-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INI-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INI-EX.
            EXIT.
      *
      ******************************
      ***   R E D   R T N        ***
      ******************************
      **
       RED-RTN.
       RED-010.
      ***  出荷指図ファイル　ＲＥＡＤ
           IF  ERR-SW         =   1
               GO  TO  RED-EX
           END-IF
      *           READ  JSTRRF      AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  RED-EX
           END-IF
           IF  JSTRR-03  NOT  =   0  AND  3  AND 7
               GO  TO  RED-010
           END-IF
           IF  JSTRR-17  NOT  =   1
               GO  TO  RED-010
           END-IF
           IF (JSTRR-1211(01) =  0)  AND  (JSTRR-1211(02) =  0)  AND
              (JSTRR-1211(03) =  0)  AND  (JSTRR-1211(04) =  0)  AND
              (JSTRR-1211(05) =  0)  AND  (JSTRR-1211(06) =  0)  AND
              (JSTRR-1211(07) =  0)  AND  (JSTRR-1211(08) =  0)  AND
              (JSTRR-1211(09) =  0)  AND  (JSTRR-1211(10) =  0)
               GO  TO  RED-010
           END-IF
           MOVE  JSTRR-16       TO  W-JSD.
           IF  W-JSD          =  2
               MOVE  1        TO  W-JSD
           END-IF
           IF  W-JSD     NOT  =  W-JS
               GO  TO  RED-010
           END-IF
           IF  JSTRR-90      <  W-DATE
               GO  TO  RED-010
           END-IF
           IF  JSTRR-90      >  W-DATE
               GO  TO  RED-EX
           END-IF
      *****
           PERFORM  WRI-RTN     THRU  WRI-EX.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO  TO  RED-010.
       RED-EX.
           EXIT.
      *
      *******************************
      ***   G A M E N   R T N     ***
      *******************************
      **
       GAMEN-RTN.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK03_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTRRF_PNAME1 "SHARED" BY REFERENCE
            JSTRRF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK03_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK03_IDLST "0".
       GAMEN-EX.
           EXIT.
      *
      ***************************
      ***   W R I   R T N     ***
      ***************************
      **
       WRI-RTN.
           MOVE  SPACE     TO  W03-R.
           INITIALIZE          W03-R.
           MOVE  JSTRR-R   TO  W03-R.
      *           WRITE    W03-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK03_PNAME1 JT-WK03_LNAME W03-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  1            TO  ERR-SW
               MOVE  "W"          TO  ERR-M
               MOVE  "JT-WK03"    TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *
      **************************
      ***  ﾌ ｧ ｲ ﾙ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
