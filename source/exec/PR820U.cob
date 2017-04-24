       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         PR820U.
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
       01  W-DATA.
           02  W-SUUTI     PIC  9(03).
           02  W-AMARI     PIC  9(01).
       COPY    LWMSG_PR.
       COPY    FCTL.
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
       COPY    LSMSG_PR.
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
           CALL "DB_F_Open" USING
            "I-O" FCTL-F_PNAME1 "EXCLUSIVE" BY REFERENCE FCTL-F_IDLST
            "1" "FCTL-KEY" BY REFERENCE FCTL-KEY.
           PERFORM  INI-RTN     THRU     INI-EX.
           PERFORM  FCTL-RTN    THRU     FCTL-EX.
           PERFORM  CLSE-ENT    THRU     CLSE-EXT.
       ED.
           CALL "DB_Close".
           STOP     RUN.
      **********************************
      *    初期　処理                  *
      **********************************
       INI-RTN.
           MOVE   "DATE  "      TO  FCTL-KEY1.
      *           READ   FCTL-F            INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               PERFORM  CLSE-ENT THRU  CLSE-EXT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "INV-CON" INV-CON "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP     RUN
           END-IF.
       INI-EX.
           EXIT.
      **********************************
      *    更新　処理                  *
      **********************************
      **********************************
      *    コントロールＦ　更新        *
      **********************************
       FCTL-RTN.
           PERFORM  YY-RTN      THRU     YY-EX
                    VARYING     I   FROM  1  BY  1
                    UNTIL       I   >    15.
           DIVIDE 4 INTO FCTL-TOUTYY(2)  GIVING W-SUUTI
                                                REMAINDER W-AMARI.
           IF  W-AMARI        =  0
               MOVE  29             TO  FCTL-TOUTDD(2)
           ELSE
               MOVE  28             TO  FCTL-TOUTDD(2)
           END-IF.
           MOVE   FCTL-KEY1     TO  ERR-K.
      *           REWRITE   FCTL-REC1  INVALID
      *///////////////
           CALL "DB_Update" USING
            FCTL-F_PNAME1 FCTL-F_LNAME FCTL-REC1 RETURNING RET.
           IF  RET = 1
               MOVE  "FCTL-F"  TO   ERR-F
               MOVE  "R"       TO   ERR-M
               PERFORM  ERR-ENT     THRU     ERR-EXT
           END-IF.
       FCTL-EX.
           EXIT.
      **********************************
      *    終了　処理                  *
      **********************************
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
       CLSE-EXT.
           EXIT.
      **
      **********************************
      *    当期　→　前期              *
      **********************************
      **********************************
      *    当期／翌期の年　＋　１      *
      **********************************
       YY-RTN.
           ADD    1             TO  FCTL-TOUFYY(I) FCTL-TOUTYY(I).
       YY-EX.
           EXIT.
       COPY  LPMSG_PR.
