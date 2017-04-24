       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             PR700U.
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        SYSTEM100.
       OBJECT-COMPUTER.        SYSTEM100.
       INPUT-OUTPUT            SECTION.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       77  ERR-STAT            PIC  X(02).
       01  WK-AREA.
           02  CHK             PIC  X(01).
       01  SDH_PR700U.
           02  SDH_PNAME1      PIC  X(009)  VALUE "SIWAKE-H1".
           02  F               PIC  X(001).
           02  SDH_LNAME       PIC  X(003)  VALUE "SDH".
           02  F               PIC  X(001).
           02  SDH_KEY1        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2        PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3        PIC  X(100)  VALUE SPACE.
           02  SDH_SORT        PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST       PIC  X(100)  VALUE SPACE.
           02  SDH_RES         USAGE  POINTER.
       COPY    SIWAKH.
       COPY    FCTL.
       COPY    SIWAID.
       COPY    LWMSG_PR.
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER       PIC  9(003).
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RESP                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
       01  DSP-CLR-AREA.
           03  DSP-CLR.
               05  FILLER      PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DSP-INIT.
           03      FILLER.
               05  FILLER      PIC  N(01)  VALUE  "年".
               05  FILLER      PIC  N(02)  VALUE  "月度".
               05  FILLER      PIC  9(02).
               05  FILLER      PIC  9(02).
               05  FILLER      PIC  X(10)
                   VALUE " 月次繰越 ".
           03  FILLER.
               05  FILLER      PIC  N(02)
                   VALUE "確認".
               05  FILLER      PIC  X(13)
                   VALUE "OK=1,NO=9 ( )".
       01  ACP-AREA.
           03  ACP-CHK         PIC X(1).
       01  MG-AREA.
           03     FILLER.
               05 MG-01    PIC N(23) VALUE
                  "翌期の月次繰越は、当期分の期末繰越後処理可！！".
               05 MG-02     PIC N(23) VALUE
                  "期末月に付き、月次繰越後、期末繰越を実行します".
               05 MG-03      PIC N(23) VALUE
                  "日次未処理の為、月次繰越実行不可！！　　　　　".
       COPY  LSMSG_PR.
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
            "DSP-INIT" " " "0" "0" "37" " " " "  RETURNING RESU.
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
            "0301DSP-INIT" BY REFERENCE FCTL-GEMYY2
                             "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0401DSP-INIT" "9" "1" "6" "2" "0301DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0401DSP-INIT" BY REFERENCE FCTL-GEMMM
                            "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0501DSP-INIT" "RX" "1" "36" "10" "0401DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-INIT" " " "24" "0" "17" "01DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0102DSP-INIT" "N" "24" "61" "4" " " "02DSP-INIT"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0202DSP-INIT" "X" "24" "66" "13" "0102DSP-INIT" " "
            RETURNING RESU.
      *    01  ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CHK" "X" "24" "77" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-CHK" BY REFERENCE CHK "1" "0" RETURNING RESU.
      *    01  MG-AREA
       CALL "SD_Init" USING
            "MG-AREA" " " "0" "0" "138" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-AREA" " " "10" "0" "138" " " "MG-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "MG-01" "N" "10" "10" "46" " " "01MG-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "MG-02" "N" "10" "10" "46" "MG-01" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "MG-03" "N" "10" "10" "46" "MG-02" " "  RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           CALL "SD_Output" USING
            "DSP-CLR-AREA" DSP-CLR-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" FCTL-F_PNAME1 "EXCLUSIVE" BY REFERENCE FCTL-F_IDLST
            "1" "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "I-O" SDH_PNAME1 "EXCLUSIVE" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           MOVE  "DATE "        TO  FCTL-KEY1.
      *           READ   FCTL-F   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "INV-CON" INV-CON "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO ST-999
           END-IF.
           MOVE   FCTL-REC1     TO  Z-R.
           CALL "SD_Output" USING
            "DSP-INIT" DSP-INIT "p" RETURNING RESU
           MOVE Z-GEMYMD     TO ZYMD.
           PERFORM Z-RTN THRU Z-EXT.
           IF  ZI > 12
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "MG-01" MG-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO ST-999
           END-IF.
           IF  Z-GEMYMD = Z-TOUT(Z-KSMM)
               CALL "SD_Output" USING
                "MG-02" MG-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
           END-IF.
           CALL "DB_F_Open" USING "I-O SEQUENTIAL" SDI_PNAME1 
            "EXCLUSIVE" BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
      *           READ SDI NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO ST-09
           END-IF.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 255.
           CALL "SD_Output" USING
            "MG-03" MG-03 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           GO TO ST-999.
       ST-09.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
       ST-10.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-CHK "ACP-CHK" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO ST-999
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ST-10
           END-IF.
           IF  CHK  NOT =  "1" AND "9"
               GO  TO  ST-10
           END-IF.
           IF  CHK  =  "9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO ST-999
           END-IF.
           PERFORM  SDH-RTN     THRU     SDH-EX.
           PERFORM  FCTL-RTN    THRU     FCTL-EX.
       ST-999.
           PERFORM  CLSE-ENT    THRU     CLSE-EXT.
       ED.
           CALL "DB_Close".
           STOP     RUN.
      **********************************
      *    仕訳ヒストリ　更新処理      *
      **********************************
       SDH-RTN.
      *           READ   SDH      AT   END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SDH_PNAME1 BY REFERENCE SH-REC " " RETURNING RET.
           IF  RET = 1
               GO  TO   SDH-EX
           END-IF.
           IF  HTRDATE  >  FCTL-GEMYMD
               GO TO SDH-EX
           END-IF.
           MOVE   SH-KEY1       TO  ERR-K.
      *    DELETE SDH.
      *//////////////////////
           CALL "DB_Delete" USING SDH_PNAME1 RETURNING RET.
           IF  ERR-STAT  NOT =  "00"
               MOVE     "SDH"    TO  ERR-F
               MOVE     "D"      TO  ERR-M
               PERFORM  ERR-ENT  THRU  ERR-EXT
           END-IF.
           GO TO SDH-RTN.
       SDH-EX.
           EXIT.
      **********************************
      *    コントロールＦ　更新処理    *
      **********************************
       FCTL-RTN.
           MOVE FCTL-GEMYY     TO  FCTL-UPDYY.
           MOVE FCTL-GEMMM     TO  FCTL-UPDMM.
           MOVE FCTL-KEY1      TO  ERR-K.
      *           REWRITE  FCTL-REC1   INVALID
      *//////////////////////
           CALL "DB_Update" USING
            FCTL-F_PNAME1 FCTL-F_LNAME FCTL-REC1 RETURNING RET.
           IF  RET = 1
               MOVE     "FCTL-F" TO  ERR-F
               MOVE     "R"      TO  ERR-M
               PERFORM  ERR-ENT  THRU  ERR-EXT
           END-IF.
       FCTL-EX.
           EXIT.
      **********************************
      *    終了　処理                  *
      **********************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
       CLSE-EXT.
           EXIT.
       COPY  LPMSG_PR.
