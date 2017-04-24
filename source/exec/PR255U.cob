       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR255U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       77  INV-SW              PIC 9(01).
      *
       COPY  LWMSG_PR.
       COPY  SIWAKE.
      *
       01  SDH_PR255U.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_PNAME2   PIC  X(009)  VALUE "SIWAKE-H2".
           02  F            PIC  X(001).
           02  SDH_PNAME3   PIC  X(009)  VALUE "SIWAKE-H3".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F            PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY4     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY  SIWAKH.
       77  F                    PIC  X(001).
      *
       COPY  FCTL.
      *
       77  USER_ID              PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE      PIC  X(003) VALUE ZERO.
       77  ESTAT                PIC  X(002).
       77  RESU                 PIC  9(001).
       77  RET                  PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER           PIC X(12)  VALUE "CLEAR SCREEN".
       01  DISP-BUZZER.
           03  DISP-BUZ-J-01    PIC X(05)  VALUE X"1B4A01".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-012      PIC X(17) VALUE "FCONTRL  ﾘﾗｲﾄ ｴﾗｰ".
               05  DSP-013      PIC X(17) VALUE "FCONTRL  ﾘｰﾄﾞ ｴﾗｰ".
       01  MG-AREA.
           02  MG-01.
             03  FILLER         PIC N(20)
                 VALUE "＊　仕訳ヒストリ二重エラー　＊".
             03  FILLER         PIC X(15).
       01  DSP-AREA2.
           02  FILLER           PIC N(07) VALUE  "マスタ更新　⑧".
       COPY  LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DISP-BUZZER
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J-01" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "12" "0" "34" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-012" "X" "12" "5" "17" " " "DSP-010" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-013" "X" "12" "5" "17" "DSP-012" " " RETURNING RESU.
      *MG-AREA
       CALL "SD_Init" USING
            "MG-AREA" " " "0" "0" "55" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "MG-01" " " "24" "0" "55" " " "MG-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-01" "N" "24" "2" "40" " " "MG-01" RETURNING RESU.
       CALL "SD_Init" USING
            "02MG-01" "X" "24" "45" "15" "01MG-01" " " RETURNING RESU.
       CALL "SD_From" USING
            "02MG-01" BY REFERENCE ERR-K "30" "0"
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
            "INPUT" SSD_PNAME1 " " BY REFERENCE SSD_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" SDH_PNAME1 " " BY REFERENCE SDH_IDLST "3"
            "SH-KEY1" BY REFERENCE SH-KEY1 "SH-KEY2" BY REFERENCE
            SH-KEY2 "SH-KEY3" BY REFERENCE SH-KEY3.
       ST-10.
      *           READ       SSD          AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SSD_PNAME1 BY REFERENCE SD-REC " " RETURNING RET.
           IF  RET = 1
               GO TO ST-100
           END-IF.
           MOVE       0            TO     INV-SW.
           MOVE       TRDATE       TO     HTRDATE.
           MOVE       JUNLNO       TO     HJUNLNO.
           MOVE       LINENO       TO     HLINENO.
           MOVE       DR-CR        TO     HDR-CR.
           MOVE SH-KEY1     TO ERR-K.
      *           READ       SDH          INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" SDH_PNAME1 BY REFERENCE SH-REC " " RETURNING RET.
           IF  RET = 1
               MOVE         1      TO           INV-SW
           END-IF.
           IF  DELKB = SPACE
               IF  INV-SW = 1
                   PERFORM SDH-SET-RTN THRU SDH-SET-EX
                   PERFORM SDH-WRI-RTN THRU SDH-WRI-EX
               ELSE
                   CALL "SD_Output" USING "MG-01" MG-01 "p"
                                                      RETURNING RESU
                   CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               END-IF
           END-IF.
           IF (DELKB NOT = SPACE) AND (INV-SW = 0)
               PERFORM SDH-DEL-RTN THRU SDH-DEL-EX
           END-IF.
           GO TO      ST-10.
       ST-100.
           CALL "DB_F_Open" USING
            "I-O" FCTL-F_PNAME1 " " BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY3" BY REFERENCE FCTL-KEY3.
           MOVE  "SEL   "  TO  FCTL-KEY3.
      *           READ  FCTL-F  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  ERRORSHORI-2
           END-IF.
           MOVE   0   TO  FCTL-SO.
      *           REWRITE  FCTL-REC3   INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            FCTL-F_PNAME1 FCTL-F_LNAME FCTL-REC3 RETURNING RET.
           IF  RET = 1
               GO  TO  ERRORSHORI-1
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           PERFORM  CLSE-ENT    THRU     CLSE-EXT.
           CALL "DB_Close".
           STOP   RUN.
       ERRORSHORI-1.
           CALL "SD_Output" USING "DSP-012" DSP-012 "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-J-01" DISP-BUZ-J-01 "p"
                                  RETURNING RESU.
           GO  TO  OWARI.
       ERRORSHORI-2.
           CALL "SD_Output" USING "DSP-013" DSP-013 "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-J-01" DISP-BUZ-J-01 "p"
                                  RETURNING RESU.
           GO  TO  OWARI.
       OWARI.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           PERFORM CLSE-ENT        THRU   CLSE-EXT.
           CALL "DB_Close".
           STOP    RUN.
       SDH-SET-RTN.
           MOVE       SD-REC       TO     SH-REC.
           MOVE       ACCNTCD      TO     HACCNTCD2.
           MOVE       TRDATE       TO     HTRDATE2.
           MOVE       JUNLNO       TO     HJUNLNO2.
           MOVE       LINENO       TO     HLINENO2.
           MOVE       DR-CR        TO     HDR-CR2.
           MOVE       ETAX         TO     HETAX.
       SDH-SET-EX.
           EXIT.
       SDH-WRI-RTN.
      *           WRITE  SH-REC       INVALID
      *///////////////
           CALL "DB_Insert" USING
            SDH_PNAME1 SDH_LNAME SH-REC RETURNING RET.
           IF  RET = 1
               MOVE         "SDH"  TO           ERR-F
               MOVE         "W"    TO           ERR-M
               PERFORM    ERR-ENT  THRU         ERR-EXT
           END-IF.
       SDH-WRI-EX.
           EXIT.
       SDH-DEL-RTN.
      *           DELETE SDH          INVALID
      *///////////////
           CALL "DB_Delete" USING SDH_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE         "D"    TO           ERR-M
               PERFORM    ERR-ENT  THRU         ERR-EXT
           END-IF.
       SDH-DEL-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
       CLSE-EXT.
           EXIT.
      *****
       COPY  LPMSG_PR.
