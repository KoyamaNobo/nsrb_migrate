       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR805U.
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT        PIC  X(02).
       77  OHZM-KMCD       PIC  9(04).
       01  SOEJI.
           02  I           PIC  9(02).
           02  SOE         PIC  9(02).
       01  SUM-AREA.
           02  W-KR        PIC S9(11).
           02  W-KS        PIC S9(11).
       COPY    LWMSG_PR.
       COPY    LHOZAN.
       COPY    ACCUNT.
       COPY    FCTL.
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
       COPY  LSMSG_PR.
       PROCEDURE           DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           PERFORM  INI-RTN     THRU     INI-EX.
           CALL "DB_F_Open" USING
            "I-O" HZM-F_PNAME1 "EXCLUSIVE" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
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
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE FCTL-REC1     TO Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
       INI-EX.
           EXIT.
      **********************************
      *    更新　処理                  *
      **********************************
       UPD-RTN.
      *           READ   HZM-F    NEXT AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HZM-F_PNAME1 BY REFERENCE HZM-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-EX
           END-IF.
           MOVE HZM-KMCD     TO OHZM-KMCD.
           GO TO UPD-100.
       UPD-000.
      *           READ   HZM-F    NEXT AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HZM-F_PNAME1 BY REFERENCE HZM-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-EX
           END-IF.
           IF  HZM-KMCD = OHZM-KMCD
               GO TO UPD-200
           END-IF.
       UPD-100.
           MOVE HZM-KMCD     TO AM-KEY.
      *           READ AM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-900
           END-IF.
       UPD-200.
           MOVE ZERO     TO SUM-AREA.
           PERFORM ZERO-RTN THRU ZERO-EX
                   VARYING I FROM 1 BY 1
                   UNTIL   I >  12.
           IF  BS-PL = 1
               MOVE ZERO     TO HZM-ZAN
           ELSE
               IF  DR-CR = 1
                   COMPUTE HZM-ZAN = HZM-ZAN + W-KR - W-KS
               ELSE
                   COMPUTE HZM-ZAN = HZM-ZAN + W-KS - W-KR
               END-IF
           END-IF.
           MOVE 13     TO I.
       UPD-300.
           COMPUTE SOE = Z-KSMM + ( I - 12 ).
           IF  SOE > 12
               COMPUTE SOE = SOE - 12
           END-IF.
           MOVE HZM-TJIS(I)     TO HZM-TJIS(SOE).
           IF  I NOT = 15
               ADD  1     TO I
               GO TO UPD-300
           END-IF.
           INITIALIZE  HZM-TJIS(13).
           INITIALIZE  HZM-TJIS(14).
           INITIALIZE  HZM-TJIS(15).
           MOVE HZM-KMCD     TO OHZM-KMCD ERR-K.
      *           REWRITE   HZM-R      INVALID
      *///////////////
           CALL "DB_Update" USING
            HZM-F_PNAME1 HZM-F_LNAME HZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"   TO   ERR-F
               MOVE  "R"       TO   ERR-M
               PERFORM  ERR-ENT     THRU     ERR-EXT
           END-IF.
           GO TO UPD-000.
       UPD-900.
           MOVE HZM-KMCD     TO OHZM-KMCD ERR-K.
      *           DELETE HZM-F         INVALID
      *///////////////
           CALL "DB_Delete" USING HZM-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"   TO   ERR-F
               MOVE  "D"       TO   ERR-M
               PERFORM  ERR-ENT     THRU     ERR-EXT
           END-IF.
      *           READ   HZM-F    NEXT AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HZM-F_PNAME1 BY REFERENCE HZM-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-EX
           END-IF.
           IF  HZM-KMCD NOT = OHZM-KMCD
               GO TO UPD-100
           END-IF.
           GO TO UPD-900.
       UPD-EX.
           EXIT.
      **********************************
      *    終了　処理                  *
      **********************************
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
       CLSE-EXT.
           EXIT.
      **********************************
      *    当期実績　クリア            *
      **********************************
       ZERO-RTN.
           ADD  HZM-TJKR(I)     TO W-KR.
           ADD  HZM-TJKS(I)     TO W-KS.
           INITIALIZE  HZM-TJIS(I).
       ZERO-EX.
           EXIT.
       COPY  LPMSG_PR.
