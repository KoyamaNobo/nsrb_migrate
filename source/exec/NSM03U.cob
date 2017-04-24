       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     NSM03U.
       AUTHOR.         A.KOMATSUBARA.
      **********************************
      *        人事マスタ変換          *
      **********************************
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       INPUT-OUTPUT    SECTION.
       DATA    DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT              PIC  X(02).
       01  SW-AREA.
           02  END-SW            PIC  9(01)    VALUE 0.
           02  TK-SW             PIC  9(01)    VALUE 0.
       01  W-AREA.
           02  W-OKC             PIC  X(01).
           02  W-TCD.
               03  W-TCD1        PIC  9(01).
               03  W-TCD2        PIC  9(01).
               03  W-TCD3        PIC  9(01).
               03  W-TCD4        PIC  9(01).
               03  W-TCD5        PIC  9(01).
       COPY    LWMSG_PR.
       COPY    LIJIN.
       COPY    TKLIB.
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
       01  DISP-C.
           02  DISP-CLE    PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DISP-AREA.
           02  FILLER.
               03  FILLER    PIC  N(07)  VALUE
                   "人事マスタ変換".
           02  FILLER.
               03  FILLER    PIC  N(02)  VALUE  "確認".
               03  FILLER    PIC  X(13)  VALUE
                   "OK=1,NO=9 ( )".
       01  ACEP-AREA.
           02  ACP-OKC      PIC  X(01).
       COPY    LSMSG_PR.
       PROCEDURE   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *    01  DISP-C
       CALL "SD_Init" USING 
            "DISP-C" " " "1" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C"  RETURNING RESU.
      *    01  DISP-AREA
       CALL "SD_Init" USING 
            "DISP-AREA" " " "0" "0" "31" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-AREA" " " "1" "0" "14" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DISP-AREA" "RN" "1" "21" "14" " " "01DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DISP-AREA" " " "24" "0" "17" "01DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DISP-AREA" "N" "24" "61" "4" " " "02DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DISP-AREA" "X" "24" "66" "13" "0102DISP-AREA" " "
            RETURNING RESU.
      *    01  ACEP-AREA
       CALL "SD_Init" USING 
            "ACEP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "X" "24" "77" "1" " " "ACEP-AREA"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *************************
      *    ＭＡＩＮ処理       *
      *************************
       HAJIME.
           PERFORM     INI-RTN   THRU   INI-EX.
           PERFORM     UPD-RTN   THRU   UPD-EX.
           PERFORM     CLSE-ENT  THRU   CLSE-EXT.
       OWARI.
           CALL "DB_Close".
           STOP RUN.
      *************************
      *    初期処理           *
      *************************
       INI-RTN.
           CALL "SD_Output" USING
            "DISP-C" DISP-C "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-AREA" DISP-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING 
            "INPUT" JINJIM_PNAME1 "SHARED" BY REFERENCE JINJIM_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING 
            "I-O" TK_PNAME1 "EXCLUSIVE" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
       INI-010.
           CALL "SD_Accept" USING
            BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               PERFORM  CLSE-ENT THRU CLSE-EXT
               CALL "DB_Close"
               STOP  RUN
               END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-010
           END-IF.
           IF  W-OKC  NOT =  "1" AND "9"
               GO  TO  INI-010
           END-IF.
           IF  W-OKC  =  "9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               PERFORM  CLSE-ENT THRU CLSE-EXT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       INI-EX.
           EXIT.
      *************************
      *    更新処理           *
      *************************
       UPD-RTN.
           MOVE  00000           TO   TK-KEY.
      *           START TK  KEY IS  NOT <    TK-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            TK_PNAME1 "TK-KEY" " NOT < " TK-KEY RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   TK-SW
           END-IF
           PERFORM  TK1-RTN      THRU TK1-EX
                 UNTIL  TK-SW    =    1.
           PERFORM  TK2-RTN      THRU TK2-EX
                 UNTIL  END-SW   =    1.
       UPD-EX.
           EXIT.
      *************************
      *    終了処理           *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE JINJIM_IDLST JINJIM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    取引先マスタ更新① *
      *************************
       TK1-RTN.
      *           READ  TK              NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TK_PNAME1 BY REFERENCE TK-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   TK-SW
               GO  TO  TK1-EX
           END-IF.
           MOVE  TK-KEY          TO   W-TCD.
           IF  W-TCD1  NOT =  0
               MOVE  1           TO   TK-SW
               GO  TO  TK1-EX
           END-IF.
           MOVE  TK-KEY          TO   ERR-K.
      *           DELETE  TK            INVALID
      *///////////////
           CALL "DB_Delete" USING TK_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "TK"    TO   ERR-F
               MOVE  "D"     TO   ERR-M
               PERFORM ERR-ENT    THRU  ERR-EXT
           END-IF.
       TK1-EX.
           EXIT.
      *************************
      *    取引先マスタ更新② *
      *************************
       TK2-RTN.
      *           READ  JINJIM          UNLOCK  AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JINJIM_PNAME1 BY REFERENCE JINJI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   END-SW
               GO  TO  TK2-EX
           END-IF.
           MOVE  SPACE           TO   TK-REC.
           INITIALIZE            TK-REC.
           MOVE  SPACE           TO   TK-NAMEN.
           MOVE  J-SIC           TO   TK-CD.
           MOVE  J-NAME          TO   TK-NAMEN.
           MOVE  J-PNO           TO   TK-PRC.
           MOVE  J-KBC           TO   TK-BKC.
           MOVE  TK-KEY          TO   ERR-K.
      *           WRITE TK-REC          INVALID
      *//////////////////////
           CALL "DB_Insert" USING
            TK_PNAME1 TK_LNAME TK-REC RETURNING RET.
           IF  RET = 1
               MOVE  "TK"      TO   ERR-F
               MOVE  "W"       TO   ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       TK2-EX.
           EXIT.
           COPY    LPMSG_PR.
