       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     NSM01U.
       AUTHOR.         A.KOMATSUBARA.
      **********************************
      *        得意先マスタ変換        *
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
               03  W-TCD2        PIC  9(04).
           02  SCNT              PIC  9(02).
           02  DCNT              PIC  9(02).
           02  CNT               PIC  9(02).
           02  W-NAME            PIC  N(26).
           02  W-NAMED  REDEFINES W-NAME.
               03  W-NAME1       PIC  N(10).
               03  W-NAME2       PIC  N(16).
           02  W-NAD    REDEFINES W-NAME.
               03  W-NA          PIC  N(01)  OCCURS  26.
           02  W-NAMEW           PIC  N(26).
           02  W-NADW   REDEFINES W-NAMEW.
               03  W-NAW         PIC  N(01)  OCCURS  26.
           02  W-TNAD1           PIC  N(04).
           02  W-TND1   REDEFINES W-TNAD1.
               03  W-TN1         PIC  N(01)  OCCURS   4.
           02  W-TNAD2           PIC  N(06).
           02  W-TND2   REDEFINES W-TNAD2.
               03  W-TN2         PIC  N(01)  OCCURS   6.
           02  W-TNAD3           PIC  N(09).
           02  W-TND3   REDEFINES W-TNAD3.
               03  W-TN3         PIC  N(01)  OCCURS   9.
           02  W-TCDD            PIC  9(04).
           02  W-WNK             PIC  9(02).
           02  W-DC              PIC  9(01).
       COPY    LWMSG_PR.
       COPY    LITM2.
       COPY    LITTM.
       COPY    TKLIB.
       77  USER_ID               PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE       PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER         PIC  9(003).
       77  ESTAT                 PIC  X(002).
       77  RESU                  PIC  9(001).
       77  RESP                  PIC  9(001).
       77  RET                   PIC  9(001) VALUE ZERO.
       01  DISP-C.
           02  DISP-CLE          PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DISP-AREA.
           02  FILLER.
               03  FILLER        PIC  N(08)  VALUE
                   "得意先マスタ変換".
           02  FILLER.
               03  FILLER        PIC  N(02)  VALUE  "確認".
               03  FILLER        PIC  X(13)  VALUE
                   "OK=1,NO=9 ( )".
       01  ACEP-AREA.
           02  ACP-OKC           PIC  X(01).
       COPY    LSMSG_PR.
       PROCEDURE   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *       DISP-C
       CALL "SD_Init" USING 
            "DISP-C" " " "1" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C"  RETURNING RESU.
      *       01  DISP-AREA
       CALL "SD_Init" USING 
            "DISP-AREA" " " "0" "0" "33" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-AREA" " " "1" "0" "16" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DISP-AREA" "RN" "1" "21" "16" " " "01DISP-AREA"
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
      *       01  ACEP-AREA
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
            "DISP-C" DISP-C "p" RETURNING RESU
           CALL "SD_Output" USING
            "DISP-AREA" DISP-AREA "p" RETURNING RESU
           CALL "DB_F_Open" USING 
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING 
            "INPUT" TT-M_PNAME1 "SHARED" BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
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
           END-IF
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
           MOVE  10000           TO   TK-KEY.
      *           START TK  KEY IS  NOT <    TK-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            TK_PNAME1 "TK-KEY" " NOT < " TK-KEY RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   TK-SW
           END-IF
           PERFORM  TK1-RTN      THRU TK1-EX
                 UNTIL  TK-SW    =    1.
           PERFORM  TK2-RTN      THRU TK2-EX.
       UPD-EX.
           EXIT.
      *************************
      *    終了処理           *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
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
           IF  W-TCD1  NOT =  1
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
      *           READ  T-M      NEXT   UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   END-SW
               GO  TO  TK2-EX
           END-IF.
       TK2-010.
           MOVE  T-NTCD          TO   W-TCDD.
           MOVE  ZERO            TO   W-WNK.
       TK2-020.
           MOVE  T-TCD           TO   TT-KEY.
      *           READ  TT-M    WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO         TO   TT-TUZ  TT-TUZZ
           END-IF.
           IF  (TT-TUZ  NOT = ZERO)  OR  (TT-TUZZ  NOT = ZERO)
                ADD  1             TO   W-WNK
           END-IF.
      *           READ  T-M       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   END-SW
               GO  TO  TK2-030
           END-IF.
           IF  T-NTCD   NOT  =  W-TCDD
               GO  TO  TK2-030
           END-IF.
           GO  TO  TK2-020.
       TK2-030.
           MOVE  SPACE           TO   T-KEY2.
           MOVE  W-TCDD          TO   T-NTCD.
      *           START  T-M    KEY  NOT <  T-KEY2  INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            T-M_PNAME1 "T-KEY2" " NOT < " T-KEY2 RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO   ERR-MSGX
               MOVE  "データ　エラー"   TO   ERR-MSGN
               CALL "SD_Output" USING
                "DISP-MSG-01" DISP-MSG-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               MOVE  1         TO   END-SW
               GO  TO  TK2-EX
           END-IF
           MOVE  0               TO   W-DC.
       TK2-040.
      *           READ  T-M       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  TK2-050
           END-IF.
           IF  T-NTCD      NOT  =  W-TCDD
               GO  TO  TK2-050
           END-IF.
           MOVE  SPACE           TO   TK-REC.
           MOVE  SPACE           TO   W-NAME.
           INITIALIZE            TK-REC.
           ADD   10000           TO   T-TCD  GIVING  TK-CD.
           IF  T-TNA = SPACE
               MOVE T-NAME    TO W-NAME
           ELSE
               MOVE T-TNA     TO W-NAME
           END-IF.
           IF  W-NAME2  NOT =     SPACE
               PERFORM TNA-RTN THRU TNA-EX
           END-IF.
           MOVE  W-NAME1         TO   TK-NAMEN.
           IF  T-TNC < 20
               MOVE 22 TO TK-BKC
           END-IF.
           IF  T-TNC > 19 AND < 80
               MOVE 23 TO TK-BKC
           END-IF.
           IF  T-TNC > 79 AND < 90
               MOVE 24 TO TK-BKC
           END-IF.
           IF  T-TNC > 89
               MOVE 33 TO TK-BKC
           END-IF.
           MOVE T-NTCD TO TK-TCD.
           IF  W-WNK  <  2
               MOVE  0     TO  TK-WNK
           ELSE
               MOVE  1     TO  TK-WNK
           END-IF.
           MOVE T-SS  TO TK-SS.
           MOVE  TK-KEY          TO   ERR-K.
      *           WRITE TK-REC          INVALID
      *///////////////
           CALL "DB_Insert" USING
            TK_PNAME1 TK_LNAME TK-REC RETURNING RET.
           IF  RET = 1
               MOVE  "TK"      TO   ERR-F
               MOVE  "W"       TO   ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           MOVE  1     TO  W-DC.
           GO  TO  TK2-040.
       TK2-050.
           IF  W-DC        =  0
               MOVE  SPACE     TO   ERR-MSGX
               MOVE  "データ　エラー"   TO   ERR-MSGN
               CALL "SD_Output" USING
                "DISP-MSG-01" DISP-MSG-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               MOVE  1         TO   END-SW
               GO  TO  TK2-EX
           END-IF.
           IF  END-SW    NOT  =  1
               GO  TO  TK2-010
           END-IF.
       TK2-EX.
           EXIT.
      *************************
      *    取引先名　短縮     *
      *************************
       TNA-RTN.
           MOVE SPACE TO W-NAMEW.
           MOVE W-NAME TO W-NAMEW.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO DCNT CNT.
       TNA-010.
           ADD 1 TO DCNT.
           IF  DCNT = 27
               GO TO TNA-100
           END-IF.
           MOVE W-NAW(DCNT) TO W-NA(DCNT).
           IF  W-NAW(DCNT) = SPACE
               ADD 1 TO CNT
           ELSE
               MOVE ZERO TO CNT
           END-IF.
           IF  CNT < 4
               GO TO TNA-010
           END-IF.
       TNA-100.
           IF  W-NAME2 = SPACE
               GO TO TNA-EX
           END-IF.
           MOVE SPACE TO W-TNAD1.
           MOVE ZERO TO SCNT.
       TNA-110.
           ADD 1 TO SCNT.
           IF  SCNT = 24
               GO TO TNA-400
           END-IF.
           COMPUTE DCNT = SCNT - 1.
           MOVE ZERO TO CNT.
       TNA-120.
           ADD 1 TO CNT DCNT.
           IF  CNT NOT = 5
               MOVE W-NA(DCNT) TO W-TN1(CNT)
               GO TO TNA-120
           END-IF.
           IF  W-TNAD1 NOT = "株式会社" AND "有限会社"
               GO TO TNA-110
           END-IF.
           IF  W-TNAD1 = "株式会社"
               MOVE "㈱　　　" TO W-TNAD1
           END-IF.
           IF  W-TNAD1 = "有限会社"
               MOVE "㈲　　　" TO W-TNAD1
           END-IF.
           COMPUTE DCNT = SCNT - 1.
           MOVE ZERO TO CNT.
       TNA-130.
           ADD 1 TO CNT DCNT.
           IF  CNT NOT = 5
               MOVE W-TN1(CNT) TO W-NA(DCNT)
               GO TO TNA-130
           END-IF.
           COMPUTE CNT = SCNT + 3.
       TNA-140.
           ADD 1 TO CNT SCNT.
           IF  CNT NOT = 27
               MOVE W-NA(CNT) TO W-NA(SCNT)
               GO TO TNA-140
           END-IF.
           MOVE SPACE TO W-NA(24) W-NA(25) W-NA(26).
           GO TO TNA-400.
       TNA-400.
           IF  W-NAME2 = SPACE
               GO TO TNA-EX
           END-IF.
           MOVE SPACE TO W-NAMEW.
           MOVE W-NAME TO W-NAMEW.
           MOVE SPACE TO W-NAME.
           MOVE ZERO TO SCNT DCNT.
       TNA-410.
           ADD 1 TO DCNT.
           IF  DCNT = 27
               GO TO TNA-EX
           END-IF.
           IF  W-NAW(DCNT) NOT = SPACE
               ADD 1 TO SCNT
               MOVE W-NAW(DCNT) TO W-NA(SCNT)
           END-IF.
           GO TO TNA-410.
       TNA-EX.
           EXIT.
           COPY    LPMSG_PR.
