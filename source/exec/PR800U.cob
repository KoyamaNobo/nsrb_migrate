       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR800U.
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT        PIC  X(02).
       01  SOEJI.
           02  I           PIC  9(02).
           02  SOE         PIC  9(02).
       01  SUM-AREA.
           02  W-KR        PIC  S9(11).
           02  W-KS        PIC  S9(11).
       COPY    LWMSG_PR.
       COPY    LKAZAN.
       COPY    ACCUNT.
       COPY    FCTL.
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER       PIC  9(003).
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RESP                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
       01  DSP-CLR-AREA.
           03  DSP-CLR.
               05  FILLER  PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DSP-INIT.
           03  FILLER.
               05  FILLER  PIC  N(01)  VALUE  "年".
               05  FILLER  PIC  N(02)  VALUE  "月度".
               05  FILLER  PIC  9(02).
               05  FILLER  PIC  9(02).
               05  FILLER  PIC  X(10)
                   VALUE " 期末繰越 ".
       COPY    LSMSG_PR.
       PROCEDURE           DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *    01  DSP-CLR-AREA
       CALL "SD_Init" USING
            "DSP-CLR-AREA" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-CLR" " " "1" "0" "12" " " "DSP-CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLEAR" "X" "1" "0" "12" " " "DSP-CLR"  RETURNING RESU.
      *    01  DSP-INIT
       CALL "SD_Init" USING
            "DSP-INIT" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-INIT" " " "1" "0" "20" " " "DSP-INIT"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0101DSP-INIT" "N" "1" "4" "2" " " "01DSP-INIT"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0201DSP-INIT" "N" "1" "8" "4" "0101DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0301DSP-INIT" "9" "1" "2" "2" "0201DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301DSP-INIT"
             BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0401DSP-INIT" "9" "1" "6" "2" "0301DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0401DSP-INIT"
            BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0501DSP-INIT" "RX" "1" "36" "10" "0401DSP-INIT" " "
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           PERFORM  INI-RTN     THRU     INI-EX.
       ST-10.
           CALL "DB_F_Open" USING
            "I-O" KZM-F_PNAME1 "EXCLUSIVE" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING 
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           PERFORM  UPD-RTN     THRU     UPD-EX.
           PERFORM  CLSE-ENT    THRU     CLSE-EXT.
       ED.
           CALL "DB_Close".
           STOP     RUN.
      **********************************
      *    初期　処理                  *
      **********************************
       INI-RTN.
           CALL "SD_Output" USING
                   "DSP-CLR-AREA" DSP-CLR-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING 
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE   "DATE  "      TO  FCTL-KEY1.
      *           READ   FCTL-F   UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "INV-CON" INV-CON "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               STOP     RUN
           END-IF.
           MOVE FCTL-REC1     TO Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           IF  Z-GEMYMD NOT = Z-TOUT(Z-KSMM)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF.
           CALL "SD_Output" USING
            "DSP-INIT" DSP-INIT "p" RETURNING RESU.
       INI-EX.
           EXIT.
      **********************************
      *    更新　処理                  *
      **********************************
       UPD-RTN.
           MOVE ZERO     TO SUM-AREA.
      *           READ   KZM-F    NEXT AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" KZM-F_PNAME1 BY REFERENCE KZM-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-EX
           END-IF.
           MOVE KZM-KEY     TO ERR-K.
           MOVE KZM-KEY     TO AM-KEY.
      *           READ AM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-999
           END-IF.
           PERFORM ZENKI-RTN THRU ZENKI-EX
                   VARYING I FROM 1 BY 1
                   UNTIL   I > 12.
           IF  BS-PL = 1
               MOVE ZERO     TO KZM-ZAN
           ELSE
               IF  DR-CR = 1
                   COMPUTE KZM-ZAN = KZM-ZAN + W-KR - W-KS
               ELSE
                   COMPUTE KZM-ZAN = KZM-ZAN + W-KS - W-KR
               END-IF
           END-IF.
           MOVE 13     TO I.
       UPD-000.
           COMPUTE SOE = Z-KSMM + ( I - 12 ).
           IF  SOE > 12
               COMPUTE SOE = SOE - 12
           END-IF.
           MOVE KZM-TJIS(I)     TO KZM-TJIS(SOE).
           IF  I NOT = 15
               ADD  1     TO I
               GO TO UPD-000
           END-IF.
           INITIALIZE  KZM-TJIS(13).
           INITIALIZE  KZM-TJIS(14).
           INITIALIZE  KZM-TJIS(15).
           MOVE   KZM-KEY       TO  ERR-K.
      *           REWRITE   KZM-R      INVALID
      *///////////////
           CALL "DB_Update" USING
            KZM-F_PNAME1 KZM-F_LNAME KZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"   TO   ERR-F
               MOVE  "R"       TO   ERR-M
               PERFORM  ERR-ENT     THRU     ERR-EXT
           END-IF.
           GO TO UPD-RTN.
       UPD-999.
      *           DELETE KZM-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING KZM-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"   TO   ERR-F
               MOVE  "D"       TO   ERR-M
               PERFORM  ERR-ENT     THRU     ERR-EXT
           END-IF.
           GO TO UPD-RTN.
       UPD-EX.
           EXIT.
      **********************************
      *    終了　処理                  *
      **********************************
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
       CLSE-EXT.
           EXIT.
      **
      **********************************
      *    当期　→　前期              *
      **********************************
       ZENKI-RTN.
           ADD  KZM-TJKR(I)     TO W-KR.
           ADD  KZM-TJKS(I)     TO W-KS.
           MOVE   KZM-TJIS(I)   TO  KZM-ZJIS(I).
           INITIALIZE  KZM-TJIS(I).
       ZENKI-EX.
           EXIT.
       COPY  LPMSG_PR.
