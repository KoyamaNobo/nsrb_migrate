       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR015M.
       AUTHOR.         A.KOMATSUBARA.
      **********************************
      *        科目マスタ(1) メンテ    *
      **********************************
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       INPUT-OUTPUT    SECTION.
       DATA    DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT              PIC  X(02).
       01  W-GAMEN.
           02  W-ACT                     PIC  9(01).
           02  W-GAMEN-DATA.
               03  W-ACCTCD              PIC  9(04).
               03  W-KNGNMN              PIC  N(10).
               03  W-DR-CR               PIC  9(01).
               03  W-BFDZN               PIC S9(11).
               03  W-TEG-BAN             PIC  9(02).
               03  W-HOJYO               PIC  9(01).
               03  W-BS-PL               PIC  9(01).
               03  W-KEIHI               PIC  9(01).
               03  W-MOTKB               PIC  9(01).
               03  W-OKC                 PIC  X(01).
       01  W-AREA.
           02  INV-SW                    PIC  9(01).
       01  W-NSPACE                      PIC  N(10)  VALUE
                  "　　　　　　　　　　".
       01  W-SPACE                       PIC  X(12)  VALUE
                    "            ".
      *
       COPY    LWMSG_PR.
       COPY    ACCUNT.
       COPY    KANGEL.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DISP-C .
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-AREA.
           02  DISP-KNGNMN.
               03  FILLER          PIC  N(10).
           02  DISP-BFDZN.
               03  FILLER          PIC  ZZZZZZZZZZ9-.
       01  ACEP-AREA.
           02  ACEP-ACT        PIC  9(01).
           02  ACEP-ACCTCD     PIC  9(04).
           02  ACEP-DR-CR      PIC  9(01).
           02  ACEP-BFDZN      PIC S9(11).
           02  ACEP-TEG-BAN    PIC  9(02).
           02  ACEP-HOJYO      PIC  9(01).
           02  ACEP-BS-PL      PIC  9(01).
           02  ACEP-KEIHI      PIC  9(01).
           02  ACEP-MOTKB      PIC  9(01).
           02  ACEP-OKC        PIC  X(01).
       01  DISP-SP.
           02  FILLER.
               03  FILLER         PIC X(001) VALUE " ".
           02  DISP-SP-DATA.
               03  FILLER.
                   04  FILLER     PIC X(004) VALUE "    ".
                   04  FILLER     PIC X(001) VALUE " ".
               03  FILLER.
                   04  FILLER     PIC N(10).
                   04  FILLER     PIC X(001) VALUE " ".
               03  FILLER.
                   04  FILLER     PIC X(001) VALUE " ".
                   04  FILLER     PIC X(001) VALUE " ".
               03  FILLER.
                   04  FILLER     PIC X(12).
                   04  FILLER     PIC X(001) VALUE " ".
               03  FILLER.
                   04  FILLER     PIC X(002) VALUE "  ".
               03  FILLER.
                   04  FILLER     PIC X(001) VALUE " ".
      **
       COPY    LSMSG_PR.
      **
       PROCEDURE   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-C
       CALL "SD_Init" USING
            "DISP-C" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING
            "DISP-AREA" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-KNGNMN" " " "6" "0" "20" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-KNGNMN" "N" "6" "18" "20" " " "DISP-KNGNMN"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-KNGNMN" BY REFERENCE W-KNGNMN "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BFDZN" " " "8" "0" "12" "DISP-KNGNMN" " "
            RETURNING RESU.
       CALL "SD_Init" USING
           "01DISP-BFDZN" "ZZZZZZZZZZ9-" "8" "27" "12" " " "DISP-BFDZN"
           RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-BFDZN" BY REFERENCE W-BFDZN "12" "0"
            RETURNING RESU.
      *ACEP-AREA
       CALL "SD_Init" USING
            "ACEP-AREA" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACEP-ACT" "9" "3" "67" "1" " " "ACEP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-ACCTCD" "9" "5" "34" "4" "ACEP-ACT" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-ACCTCD" BY REFERENCE W-ACCTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-DR-CR" "9" "7" "37" "1" "ACEP-ACCTCD" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-DR-CR" BY REFERENCE W-DR-CR "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-BFDZN" "S9" "8" "27" "11" "ACEP-DR-CR" " "
             RETURNING RESU.
       CALL "SD_Into" USING
            "ACEP-BFDZN" BY REFERENCE W-BFDZN "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-TEG-BAN" "9" "9" "36" "2" "ACEP-BFDZN" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-TEG-BAN" BY REFERENCE W-TEG-BAN "2" "0" 
            RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-HOJYO" "9" "5" "75" "1" "ACEP-BFDZN" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-HOJYO" BY REFERENCE W-HOJYO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-BS-PL" "9" "6" "75" "1" "ACEP-HOJYO" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-BS-PL" BY REFERENCE W-BS-PL "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-KEIHI" "9" "7" "75" "1" "ACEP-BS-PL" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-KEIHI" BY REFERENCE W-KEIHI "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-MOTKB" "9" "8" "75" "1" "ACEP-KEIHI" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-MOTKB" BY REFERENCE W-MOTKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
             "ACEP-OKC" "X" "24" "77" "1" "ACEP-MOTKB" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DISP-SP
       CALL "SD_Init" USING
            "DISP-SP" " " "0" "0" "45" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-SP" " " "3" "0" "1" " " "DISP-SP" RETURNING RESU.
       CALL "SD_Init" USING
            "02DISP-SP" "X" "3" "67" "1" " " "01DISP-SP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-SP-DATA" " " "0" "0" "44" "01DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DISP-SP" " " "5" "0" "5" " " "DISP-SP-DATA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DISP-SP" "X" "5" "34" "4" " " "03DISP-SP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DISP-SP" "X" "5" "75" "1" "04DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DISP-SP" " " "6" "0" "21" "03DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DISP-SP" "N" "6" "18" "20" " " "06DISP-SP"
            RETURNING RESU.
       CALL "SD_From" USING
            "07DISP-SP" BY REFERENCE W-NSPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DISP-SP" "X" "6" "75" "1" "07DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DISP-SP" " " "7" "0" "2" "06DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10DISP-SP" "X" "7" "37" "1" " " "09DISP-SP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "11DISP-SP" "X" "7" "75" "1" "10DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12DISP-SP" " " "8" "0" "13" "09DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "13DISP-SP" "X" "8" "27" "12" " " "12DISP-SP"
            RETURNING RESU.
       CALL "SD_From" USING
            "13DISP-SP" BY REFERENCE W-SPACE "12" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "14DISP-SP" "X" "8" "75" "1" "13DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15DISP-SP" " " "9" "0" "2" "12DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "16DISP-SP" "X" "9" "36" "2" " " "15DISP-SP"
            RETURNING RESU.
       CALL "SD_Init" USING
            "17DISP-SP" " " "24" "0" "1" "16DISP-SP" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "18DISP-SP" "X" "24" "77" "1" " " "17DISP-SP"
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
           PERFORM     ACP-RTN   THRU   ACP-EX.
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
           CALL "SD_Screen_Output" USING "GR0150" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           INITIALIZE  W-GAMEN.
       INI-EX.
           EXIT.
      *************************
      *    入力処理           *
      *************************
       ACP-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-ACT "ACEP-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  ACP-EX
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-RTN
           END-IF.
           IF  W-ACT  NOT =  1 AND 2 AND 3
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                         RETURNING RESU
               GO  TO  ACP-RTN
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-ACCTCD "ACEP-ACCTCD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT  =  "09"
               INITIALIZE W-GAMEN-DATA
               CALL "SD_Output" USING "DISP-SP-DATA" DISP-SP-DATA "p"
                                         RETURNING RESU
               GO  TO  ACP-RTN
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-010
           END-IF.
           MOVE  0     TO  INV-SW.
           PERFORM SCHK-RTN  THRU  SCHK-EX.
           IF  W-ACT  =  1
               IF  INV-SW  =  0
                   CALL "SD_Output" USING "NOR-D01" NOR-D01 "p"
                                         RETURNING RESU
                   CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                         RETURNING RESU
                   GO  TO  ACP-010
               END-IF
           END-IF.
           IF  W-ACT  =  2 OR 3
               IF  INV-SW  =  1
                   CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                         RETURNING RESU
                   CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                         RETURNING RESU
                   GO  TO  ACP-010
               ELSE
                   PERFORM DSP-RTN  THRU DSP-EX
               END-IF
           END-IF.
           MOVE  0     TO  INV-SW.
           PERFORM MCHK-RTN  THRU MCHK-EX.
           IF  INV-SW  =  1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                         RETURNING RESU
               GO  TO  ACP-010
           END-IF.
           CALL "SD_Output" USING "DISP-KNGNMN" DISP-KNGNMN "p"
                                         RETURNING RESU.
           IF  W-ACT  =  3
               GO  TO  ACP-090
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-DR-CR "ACEP-DR-CR" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-010
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-020
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-BFDZN "ACEP-BFDZN" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-020
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-030
           END-IF.
           CALL "SD_Output" USING "DISP-BFDZN" DISP-BFDZN "p"
                                         RETURNING RESU.
       ACP-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-TEG-BAN "ACEP-TEG-BAN" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-030
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-040
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-HOJYO "ACEP-HOJYO" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-040
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-050
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-BS-PL "ACEP-BS-PL" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-050
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-060
           END-IF.
       ACP-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-KEIHI "ACEP-KEIHI" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-060
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-070
           END-IF.
       ACP-080.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-MOTKB "ACEP-MOTKB" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-070
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-080
           END-IF.
       ACP-090.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-OKC "ACEP-OKC" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  W-ACT  =  3
                   GO  TO  ACP-010
               ELSE
                   GO  TO  ACP-080
               END-IF
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-090
           END-IF.
           IF  W-OKC  NOT =  "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  ACP-090
           END-IF.
           IF  W-OKC  =  "1"
               PERFORM UPD-RTN  THRU UPD-EX
               INITIALIZE  W-GAMEN-DATA
               CALL "SD_Output" USING "DISP-SP-DATA" DISP-SP-DATA "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "OK-01" OK-01 "p"
                                  RETURNING RESU
               GO  TO  ACP-010
           END-IF.
           IF  W-OKC  =  "9"
               INITIALIZE  W-GAMEN
               CALL "SD_Output" USING "DISP-SP" DISP-SP "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                  RETURNING RESU
               GO  TO  ACP-RTN
           END-IF.
       ACP-EX.
           EXIT.
      *************************
      *    終了処理           *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    存在チェック       *
      *************************
       SCHK-RTN.
           MOVE  W-ACCTCD   TO   AM-KEY.
      *           READ  AM         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC " " RETURNING RET.
           IF  RET = 1
               MOVE 1    TO  INV-SW
           END-IF.
       SCHK-EX.
           EXIT.
      *************************
      *    表示処理　　       *
      *************************
       DSP-RTN.
           MOVE  DR-CR      TO   W-DR-CR.
           MOVE  BFDZN      TO   W-BFDZN.
           MOVE  TEG-BAN    TO   W-TEG-BAN.
           MOVE  HOJYO      TO   W-HOJYO.
           MOVE  BS-PL      TO   W-BS-PL.
           MOVE  KEIHI      TO   W-KEIHI.
           MOVE  MOTKB      TO   W-MOTKB.
           CALL "SD_Output" USING "ACEP-DR-CR" ACEP-DR-CR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DISP-BFDZN" DISP-BFDZN "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACEP-TEG-BAN" ACEP-TEG-BAN "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACEP-HOJYO" ACEP-HOJYO "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACEP-BS-PL" ACEP-BS-PL "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACEP-KEIHI" ACEP-KEIHI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACEP-MOTKB" ACEP-MOTKB "p"
                                  RETURNING RESU.
       DSP-EX.
           EXIT.
      ********************************
      *    漢字科目Ｍ　存在チェック  *
      ********************************
       MCHK-RTN.
           MOVE  ZERO       TO   KNG-KEY.
           MOVE  W-ACCTCD   TO   K-ACCD.
      *           READ  KNG        UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1    TO  INV-SW
               GO  TO    MCHK-EX
           END-IF.
           MOVE  KNGNMN     TO   W-KNGNMN.
       MCHK-EX.
           EXIT.
      *************************
      *    更新処理           *
      *************************
       UPD-RTN.
           IF  W-ACT  =  1
               PERFORM   WRITE-RTN THRU   WRITE-EX
           END-IF.
           IF  W-ACT  =  2
               PERFORM REWRITE-RTN THRU REWRITE-EX
           END-IF.
           IF  W-ACT  =  3
               PERFORM  DELETE-RTN THRU  DELETE-EX
           END-IF.
       UPD-EX.
           EXIT.
      *************************
      *    登録処理           *
      *************************
       WRITE-RTN.
           INITIALIZE  AM-REC.
           PERFORM SET-RTN THRU SET-EX.
           MOVE    AM-KEY               TO   ERR-K.
      *           WRITE   AM-REC  INVALID
      *///////////////
           CALL "DB_Insert" USING
            AM_PNAME1 AM_LNAME AM-REC RETURNING RET.
           IF  RET = 1
               MOVE    "AM"         TO   ERR-F
               MOVE    "W"          TO   ERR-M
               PERFORM ERR-ENT    THRU   ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      *************************
      *    修正処理           *
      *************************
       REWRITE-RTN.
           PERFORM SET-RTN THRU SET-EX.
           MOVE    AM-KEY               TO   ERR-K.
      *           REWRITE AM-REC  INVALID
      *///////////////
           CALL "DB_Update" USING
            AM_PNAME1 AM_LNAME AM-REC RETURNING RET.
           IF  RET = 1
               MOVE    "AM"         TO   ERR-F
               MOVE    "R"          TO   ERR-M
               PERFORM ERR-ENT    THRU   ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *************************
      *    削除処理           *
      *************************
       DELETE-RTN.
           MOVE    AM-KEY               TO   ERR-K.
      *           DELETE  AM       INVALID
      *///////////////
           CALL "DB_Delete" USING AM_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "AM"         TO   ERR-F
               MOVE    "D"          TO   ERR-M
               PERFORM ERR-ENT    THRU   ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      *************************
      *    項目セット処理     *
      *************************
       SET-RTN.
           MOVE  W-ACCTCD   TO   AM-KEY.
           MOVE  W-DR-CR    TO   DR-CR.
           MOVE  W-BFDZN    TO   BFDZN.
           MOVE  W-TEG-BAN  TO   TEG-BAN.
           MOVE  W-HOJYO    TO   HOJYO.
           MOVE  W-BS-PL    TO   BS-PL.
           MOVE  W-KEIHI    TO   KEIHI.
           MOVE  W-MOTKB    TO   MOTKB.
       SET-EX.
           EXIT.
      *******
           COPY    LPMSG_PR.
