       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR060U.
       AUTHOR.         A.KOMATSUBARA.
      **********************************
      *        部門別配列マスタ生成    *
      **********************************
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       INPUT-OUTPUT    SECTION.
       DATA    DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT              PIC  X(02).
       77  WKSP                  PIC  X(30) VALUE SPACE.
       01  W-AREA.
           02  END-SW            PIC  9(01).                            AT END
      *
       COPY    LWMSG_PR.
       COPY    BUMONF.
       COPY    BPLHAI.
       COPY    BGNHAI.
       COPY    BKHHAI.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-AREA.
           02  FILLER.
               03  FILLER    PIC  X(22).
               03  FILLER    PIC  N(10)  VALUE
                   "部門別配列マスタ生成".
      **
       COPY    LSMSG_PR.
      **
       PROCEDURE   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING
            "DISP-AREA" " " "0" "0" "42" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-AREA" " " "1" "0" "42" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DISP-AREA" "RX" "1" "30" "22" " " "01DISP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "02DISP-AREA" BY REFERENCE WKSP "22" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DISP-AREA" "N" "1" "31" "20" "02DISP-AREA" " "
            RETURNING RESU.
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
           PERFORM     UPD-RTN   THRU   UPD-EX
                       UNTIL     END-SW  =  1.
           PERFORM     CLSE-ENT  THRU   CLSE-EXT.
       OWARI.
           CALL "DB_Close".
           STOP RUN.
      *************************
      *    初期処理           *
      *************************
       INI-RTN.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-AREA" DISP-AREA "p"
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" BPLHAI_PNAME1 "EXCLUSIVE" BY REFERENCE 
            BPLHAI_IDLST "1" "BPLHAI-KEY" BY REFERENCE BPLHAI-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" BGNHAI_PNAME1 "EXCLUSIVE" BY REFERENCE 
            BGNHAI_IDLST "1" "BGNHAI-KEY" BY REFERENCE BGNHAI-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" BKHHAI-K_PNAME1 "EXCLUSIVE" BY REFERENCE 
            BKHHAI-K_IDLST "1" "BKHHAI-KEY" BY REFERENCE BKHHAI-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE BPLHAI_IDLST BPLHAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BGNHAI_IDLST BGNHAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BKHHAI-K_IDLST BKHHAI-K_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" BPLHAI_PNAME1 "EXCLUSIVE" BY REFERENCE 
            BPLHAI_IDLST "1" "BPLHAI-KEY" BY REFERENCE BPLHAI-KEY.
           CALL "DB_F_Open" USING
            "I-O" BGNHAI_PNAME1 "EXCLUSIVE" BY REFERENCE 
            BGNHAI_IDLST "1" "BGNHAI-KEY" BY REFERENCE BGNHAI-KEY.
           CALL "DB_F_Open" USING
            "I-O" BKHHAI-K_PNAME1 "EXCLUSIVE" BY REFERENCE 
            BKHHAI-K_IDLST "1" "BKHHAI-KEY" BY REFERENCE BKHHAI-KEY.
       INI-EX.
           EXIT.
      *************************
      *    更新処理           *
      *************************
       UPD-RTN.
      *           READ  BNM     NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  1      TO     END-SW
               GO  TO  UPD-EX
           END-IF.
           PERFORM  PL-RTN    THRU   PL-EX.
           PERFORM  KH-RTN    THRU   KH-EX.
           PERFORM  GN-RTN    THRU   GN-EX.
       UPD-EX.
           EXIT.
      *************************
      *    終了処理           *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BPLHAI_IDLST BPLHAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BGNHAI_IDLST BGNHAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BKHHAI-K_IDLST BKHHAI-K_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    損益配列           *
      *************************
       PL-RTN.
           IF  BNM-PLPG(1)  =  ZERO
               GO  TO  PL-EX
           END-IF.
           MOVE  BNMPLLB(1)    TO  BPLHAI-KEY.
           MOVE  BNM-KEY       TO  BPLHAI-BUCD.
           MOVE  BPLHAI-REC    TO  ERR-K.
           MOVE  "BPLHAI"      TO  ERR-F.
      *           WRITE BPLHAI-REC    INVALID
      *///////////////
           CALL "DB_Insert" USING
            BPLHAI_PNAME1 BPLHAI_LNAME BPLHAI-REC RETURNING RET.
           IF  RET = 1
               GO TO PL-000
           END-IF.
           GO TO PL-EX.
       PL-000.
           IF  ERR-STAT NOT = "22"
               MOVE "W"     TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           MOVE "R"     TO ERR-M.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
       PL-EX.
           EXIT.
      *************************
      *    経費配列           *
      *************************
       KH-RTN.
           IF  BNM-KHPG(1)  =  ZERO
               GO  TO  KH-EX.
           MOVE  BNM-KHLB(1)   TO  BKHHAI-KEY.
           MOVE  BNM-KEY       TO  BKHHAI-BUCD.
           MOVE  BKHHAI-REC    TO  ERR-K.
           MOVE  "BKHHAI"      TO  ERR-F.
      *           WRITE BKHHAI-REC    INVALID
      *///////////////
           CALL "DB_Insert" USING
            BKHHAI-K_PNAME1 BKHHAI-K_LNAME BKHHAI-REC RETURNING RET.
           IF  RET = 1
               GO TO KH-000
           END-IF.
           GO TO KH-EX.
       KH-000.
           IF  ERR-STAT NOT = "22"
               MOVE "W"     TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           MOVE "R"     TO ERR-M.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
       KH-EX.
           EXIT.
      *************************
      *    製造配列           *
      *************************
       GN-RTN.
           IF  BNM-GNPG(1)  =  ZERO
               GO  TO  GN-EX
           END-IF.
           MOVE  BNM-GNLB(1)   TO  BGNHAI-KEY.
           MOVE  BNM-KEY       TO  BGNHAI-BUCD.
           MOVE  BGNHAI-REC    TO  ERR-K.
           MOVE  "BGNHAI"      TO  ERR-F.
      *           WRITE BGNHAI-REC    INVALID
      *///////////////
           CALL "DB_Insert" USING
            BGNHAI_PNAME1 BGNHAI_LNAME BGNHAI-REC RETURNING RET.
           IF  RET = 1
               GO TO GN-000
           END-IF.
           GO TO GN-EX.
       GN-000.
           IF  ERR-STAT NOT = "22"
               MOVE "W"     TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           MOVE "R"     TO ERR-M.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
       GN-EX.
           EXIT.
      *******
           COPY    LPMSG_PR.
