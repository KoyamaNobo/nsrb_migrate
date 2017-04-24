       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR815U.
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
       COPY    LWMSG_PR.
       COPY    KEIHI.
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
            "I-O" HH-F_PNAME1 "EXCLUSIVE" BY REFERENCE HH-F_IDLST "1"
            "HH-KEY" BY REFERENCE HH-KEY.
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
      *           READ   HH-F     NEXT AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HH-F_PNAME1 BY REFERENCE HH-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-EX
           END-IF.
           MOVE   HH-KEY        TO  ERR-K.
           PERFORM  ZERO-RTN    THRU     ZERO-EX
                    VARYING     I   FROM  1  BY  1
                    UNTIL       I   >    12.
           MOVE 13     TO I.
       UPD-000.
           COMPUTE SOE = Z-KSMM + ( I - 12 ).
           IF  SOE > 12
               COMPUTE SOE = SOE - 12
           END-IF.
           MOVE HH-GEL(I)     TO HH-GEL(SOE).
           INITIALIZE  HH-GEL(I).
           IF  I NOT = 15
               ADD  1     TO I
               GO TO UPD-000
           END-IF.
      *           REWRITE   HH-R       INVALID
      *///////////////
           CALL "DB_Update" USING
            HH-F_PNAME1 HH-F_LNAME HH-R RETURNING RET.
           IF  RET = 1
               MOVE  "HH-F"    TO   ERR-F
               MOVE  "R"       TO   ERR-M
               PERFORM  ERR-ENT     THRU     ERR-EXT
           END-IF.
           GO TO UPD-RTN.
       UPD-EX.
           EXIT.
      **********************************
      *    終了　処理                  *
      **********************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
       CLSE-EXT.
           EXIT.
      **********************************
      *    当期実績　クリア            *
      **********************************
       ZERO-RTN.
           INITIALIZE  HH-GEL(I).
       ZERO-EX.
           EXIT.
       COPY  LPMSG_PR.
