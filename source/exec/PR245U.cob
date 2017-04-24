       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR245U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       INPUT-OUTPUT        SECTION.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  INV-SW          PIC 9(1).
       77  ERR-STAT        PIC X(02).
       01  I               PIC 9(02).
       01  W-ACT           PIC X(01).
      *
       COPY    LWMSG_PR.
       COPY    SIWAKE.
       COPY    FCTL.
       COPY    LHOZAN.
       COPY    ACCUNT.
      *
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER          PIC  X(12)  VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           03  ACP-010         PIC  X(1).
       01  DISP-BUZZER.
           03  DISP-BUZ-Q      PIC  X(05)  VALUE X"1B4A02".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-011     PIC  X(12)
                              VALUE  "ｶﾓｸ ﾏｽﾀ ﾆ ﾅｼ".
               05  DSP-013     PIC 9(4).
       01  DSP-AREA2.
           02  FILLER  PIC N(007) VALUE  "マスタ更新　⑥".
       COPY    LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" "X" "24" "77" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-010" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
      *DISP-BUZZER
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-Q" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "16" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "12" "0" "16" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "X" "12" "5" "12" " " "DSP-010" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-013" "9" "12" "20" "4" "DSP-011" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-013" BY REFERENCE ACCNTCD OF SD-REC "4" "0"
            RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING
            "DSP-AREA2" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA2" "RN" "1" "35" "14" " " "DSP-AREA2"
             RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA2" DSP-AREA2 "p"
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" SSD_PNAME1 "EXCLUSIVE" BY REFERENCE SSD_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY1" BY REFERENCE FCTL-KEY1.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "I-O" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           MOVE  "DATE  "         TO      FCTL-KEY1.
      *           READ  FCTL-F           UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SSD_IDLST SSD_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HZM-F_IDLST HZM-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE  FCTL-REC1        TO      Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
       ST-10.
      *           READ       SSD           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSD_PNAME1 BY REFERENCE SD-REC " " RETURNING RET.
           IF  RET = 1
               PERFORM  CLSE-ENT     THRU    CLSE-EXT
               STOP     RUN
           END-IF.
           MOVE       ACCNTCD OF SD-REC TO     AM-KEY.
      *           READ       AM   UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "DSP-011" DSP-011 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DSP-013" DSP-013 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-Q" DISP-BUZ-Q "p"
                                               RETURNING RESU
               CALL "SD_Accept" USING
                     BY REFERENCE ACP-010 "ACP-010" "X" "1"
                     BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-10
           END-IF.
           IF  HOJYO OF AM-REC   NOT =  1
               GO TO ST-10
           END-IF.
           MOVE       TRDATE         TO     ZYMD.
           PERFORM    Z-RTN        THRU     Z-EXT.
           MOVE       ZI             TO     I.
           IF  I  >  15
               GO TO    ST-10
           END-IF.
           MOVE 0     TO INV-SW.
           MOVE       KACD1          TO    HZM-KEY.
      *           READ       HZM-F        INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HZM-F_PNAME1 BY REFERENCE HZM-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
               INITIALIZE HZM-R
               MOVE KACD1     TO HZM-KEY
           END-IF.
           IF  DR-CR OF SD-REC  =  1
               ADD    AMOUNT         TO     HZM-TJKR(I)
           ELSE
               ADD    AMOUNT         TO     HZM-TJKS(I)
           END-IF.
           MOVE       HZM-KEY        TO     ERR-K.
           IF  INV-SW = 0
               GO TO ST-999
           END-IF.
      *           WRITE HZM-R INVALID
      *///////////////
           CALL "DB_Insert" USING
            HZM-F_PNAME1 HZM-F_LNAME HZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"  TO     ERR-F
               MOVE  "W"      TO     ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO ST-10.
       ST-999.
      *           REWRITE    HZM-R          INVALID
      *///////////////
           CALL "DB_Update" USING
            HZM-F_PNAME1 HZM-F_LNAME HZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"  TO     ERR-F
               MOVE  "R"      TO     ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO      ST-10.
       ED.
           EXIT.
      ******
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
       CLSE-EXT.
           EXIT.
      ******
       COPY  LPMSG_PR.
