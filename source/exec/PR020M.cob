       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR020M.
       AUTHOR.         A.KOMATSUBARA.
      **********************************
      *        科目マスタ(2) メンテ    *
      **********************************
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       DATA    DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT              PIC  X(02).
       01  W-GAMEN.
           02  W-OKC                     PIC  X(01).
       01  W-AREA.
           02  INV-SW                    PIC  9(01).
           02  L                         PIC  9(02).
           02  I                         PIC  9(02).
       COPY    LWMSG_PR.
      *
       COPY    ACCUNT.
       COPY    KANGEL.
       COPY    BS-LIB.
       COPY    PL-LIB.
      ***
      *FD  GEN
       01  GEN_PR020M.
           02  GEN_PNAME1       PIC  X(007)  VALUE "GENKA-F".
           02  F                PIC  X(001).
           02  GEN_LNAME        PIC  X(010)  VALUE "GEN_PR020M".
           02  F                PIC  X(001).
           02  GEN_KEY1         PIC  X(100)  VALUE SPACE.
           02  GEN_KEY2         PIC  X(100)  VALUE SPACE.
           02  GEN_SORT         PIC  X(100)  VALUE SPACE.
           02  GEN_IDLST        PIC  X(100)  VALUE SPACE.
           02  GEN_RES          USAGE  POINTER.
       01  GEN-REC.
           02  GEN-KEY          PIC X(3).
           02  GEN-LIN          PIC 9.
           02  GEN-GKB          PIC 9.
           02  GEN-NAM          PIC X(20).
           02  GEN-NAMN     REDEFINES   GEN-NAM   PIC N(10).
           02  GEN-YY.
             03  GEN-ZENKI      PIC S9(11).
             03  GEN-TOUKI      PIC S9(11).
           02  GEN-MM.
             03  GEN-ZENMM      PIC S9(11).
             03  GEN-TOUMM      PIC S9(11).
           02  GEN-URIKB        PIC X.
           02  GEN-PKB          PIC 9.
           02  GEN-TANA         PIC 9.
           02  GEN-YM.
             03  GEN-YYWK       PIC 99.
             03  GEN-MMWK       PIC 99.
           02  FILLER           PIC X(9).
       77  F                    PIC X(1).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-AREA.
           02  DISP-KNGNMN.
               03  FILLER          PIC  N(10).
       01  ACEP-AREA.
           02  ACEP-ACCTCD     PIC  9(04).
           02  ACEP-BSKEY      PIC  9(03).
           02  ACEP-BSDR-CR    PIC  9(01).
           02  ACEP-BSCOM      PIC  9(01).
           02  ACEP-PLKEY      PIC  9(03).
           02  ACEP-PLCOM      PIC  9(01).
           02  ACEP-GNKEY      PIC  9(03).
           02  ACEP-GNCOM      PIC  9(01).
           02  ACEP-OKC        PIC  X(01).
      ***
       01  SP-BS-AREA.
           02  SP-BSKEY        PIC  Z(03).
           02  SP-BSDR-CR      PIC  Z(01).
           02  SP-BSCOM        PIC  Z(01).
       01  SP-PL-AREA.
           02  SP-PLKEY        PIC  Z(03).
           02  SP-PLCOM        PIC  Z(01).
       01  SP-GN-AREA.
           02  SP-GNKEY        PIC  Z(03).
           02  SP-GNCOM        PIC  Z(01).
      **
       COPY    LSMSG_PR.
      **
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-C
       CALL "SD_Init" USING
            "DISP-C" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING 
            "DISP-AREA" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-KNGNMN" " " "5" "0" "20" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-KNGNMN" "N" "5" "56" "20" " " "DISP-KNGNMN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-KNGNMN" BY REFERENCE KNGNMN "20" "0" RETURNING RESU.
      *ACEP-AREA
       CALL "SD_Init" USING 
            "ACEP-AREA" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-ACCTCD" "9" "5" "34" "4" " " "ACEP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-ACCTCD" BY REFERENCE AM-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-BSKEY" "9" "L" "15" "3" "ACEP-ACCTCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-BSKEY" BY REFERENCE BSKEY(1) "3" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-BSDR-CR" "9" "L" "22" "1" "ACEP-BSKEY" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-BSDR-CR" BY REFERENCE BSDR-CR(1) "1" "1" BY REFERENCE
            I 2 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-BSCOM" "9" "L" "27" "1" "ACEP-BSDR-CR" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-BSCOM" BY REFERENCE BSCOM(1) "1" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-PLKEY" "9" "L" "43" "3" "ACEP-BSCOM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-PLKEY" BY REFERENCE PLKEY(1) "3" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-PLCOM" "9" "L" "50" "1" "ACEP-PLKEY" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-PLCOM" BY REFERENCE PLCOM(1) "1" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-GNKEY" "9" "L" "66" "3" "ACEP-PLCOM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-GNKEY" BY REFERENCE GNKEY(1) "3" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-GNCOM" "9" "L" "73" "1" "ACEP-GNKEY" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-GNCOM" BY REFERENCE GNCOM(1) "1" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACEP-OKC" "X" "24" "77" "1" "ACEP-GNCOM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACEP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *SP-BS-AREA
       CALL "SD_Init" USING 
            "SP-BS-AREA" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-BSKEY" "Z" "L" "15" "3" " " "SP-BS-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "SP-BSKEY" BY REFERENCE BSKEY(1) "3" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-BSDR-CR" "Z" "L" "22" "1" "SP-BSKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "SP-BSDR-CR" BY REFERENCE BSDR-CR(1) "1" "1" BY REFERENCE
            I 2 RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-BSCOM" "Z" "L" "27" "1" "SP-BSDR-CR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "SP-BSCOM" BY REFERENCE BSCOM(1) "1" "1" BY REFERENCE I 2
            RETURNING RESU.
      *SP-PL-AREA
       CALL "SD_Init" USING 
            "SP-PL-AREA" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-PLKEY" "Z" "L" "43" "3" " " "SP-PL-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "SP-PLKEY" BY REFERENCE PLKEY(1) "3" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-PLCOM" "Z" "L" "50" "1" "SP-PLKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "SP-PLCOM" BY REFERENCE PLCOM(1) "1" "1" BY REFERENCE I 2
            RETURNING RESU.
      *SP-GN-AREA
       CALL "SD_Init" USING 
            "SP-GN-AREA" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-GNKEY" "Z" "L" "66" "3" " " "SP-GN-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "SP-GNKEY" BY REFERENCE GNKEY(1) "3" "1" BY REFERENCE I 2
            RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-GNCOM" "Z" "L" "73" "1" "SP-GNKEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "SP-GNCOM" BY REFERENCE GNCOM(1) "1" "1" BY REFERENCE I 2
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
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
           CALL "SD_Output" USING "DISP-C" DISP-C "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BS_PNAME1 "SHARED" BY REFERENCE BS_IDLST "1"
            "BS-KEY" BY REFERENCE BS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" PL_PNAME1 "SHARED" BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" GEN_PNAME1 "SHARED" BY REFERENCE GEN_IDLST "1"
            "GEN-KEY" BY REFERENCE GEN-KEY.
       INI-EX.
           EXIT.
      *************************
      *    入力処理           *
      *************************
       ACP-RTN.
           CALL "SD_Screen_Output" USING "GR0200" RETURNING RESU.
           IF  W-OKC = "1"
               CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU
           END-IF
           IF  W-OKC = "9"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
           END-IF
           MOVE SPACE     TO W-OKC.
       ACP-000.
           CALL "SD_Accept" USING BY REFERENCE ACEP-ACCTCD "ACEP-ACCTCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  ACP-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-000
           END-IF
           MOVE  0     TO  INV-SW.
           PERFORM SCHK-RTN  THRU  SCHK-EX.
           IF  INV-SW  =  1
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACP-000
           END-IF
           MOVE  0     TO  INV-SW.
           PERFORM MCHK-RTN  THRU MCHK-EX.
           IF  INV-SW  =  1
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACP-000
           END-IF
           CALL "SD_Output" USING
            "DISP-KNGNMN" DISP-KNGNMN "p" RETURNING RESU.
           PERFORM DSP-RTN   THRU DSP-EX.
       ACP-010.
           MOVE  1     TO  I.
           PERFORM  ACP-BS-RTN  THRU  ACP-BS-EX.
           IF  ESTAT  =  "09"
               GO  TO  ACP-RTN
           END-IF
      *
           MOVE ZERO     TO SKNKEY.
           MOVE ZERO     TO SKNKOU.
       ACP-030.
           MOVE  1     TO  I.
           PERFORM  ACP-PL-RTN  THRU  ACP-PL-EX.
           IF  ESTAT  =  "09"
               GO  TO  ACP-010
           END-IF.
       ACP-040.
           MOVE  1     TO  I.
           PERFORM  ACP-GN-RTN  THRU  ACP-GN-EX.
           IF  ESTAT  =  "09"
               GO  TO  ACP-030
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE ACEP-OKC "ACEP-OKC"
            "X" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-040
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-050
           END-IF
           IF  W-OKC  NOT =  "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p" RETURNING RESU
               GO  TO  ACP-050
           END-IF
           IF  W-OKC  =  "1"
               PERFORM UPD-RTN  THRU UPD-EX
           END-IF
           GO  TO  ACP-RTN.
       ACP-EX.
           EXIT.
      *************************
      *    終了処理           *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    存在チェック       *
      *************************
       SCHK-RTN.
      *           READ  AM         INVALID
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC " " RETURNING RET.
           IF  RET = 1
               MOVE 1    TO  INV-SW
           END-IF.
       SCHK-EX.
           EXIT.
      ********************************
      *    漢字科目Ｍ　存在チェック  *
      ********************************
       MCHK-RTN.
           MOVE  ZERO     TO   KNG-KEY.
           MOVE  AM-KEY   TO   K-ACCD.
      *           READ  KNG        UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1    TO  INV-SW
           END-IF.
       MCHK-EX.
           EXIT.
      *************************
      *    表示処理　　       *
      *************************
       DSP-RTN.
           PERFORM  DSP-BS-RTN  THRU  DSP-BS-EX
                    VARYING     I FROM 1 BY 1
                    UNTIL       I   >  6.
           PERFORM  DSP-PL-RTN  THRU  DSP-PL-EX
                    VARYING     I FROM 1 BY 1
                    UNTIL       I   >  12.
           PERFORM  DSP-GN-RTN  THRU  DSP-GN-EX
                    VARYING     I FROM 1 BY 1
                    UNTIL       I   >  12.
       DSP-EX.
           EXIT.
      *************************
      *    貸借項目入力処理   *
      *************************
       ACP-BS-RTN.
           COMPUTE L = I + 9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACEP-BSKEY "ACEP-BSKEY"
            "9" "3" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  I  =  1
                   GO  TO  ACP-BS-EX
               ELSE
                   COMPUTE   I  =  I  -  1
                   GO  TO  ACP-BS-RTN
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-BS-RTN
           END-IF
           IF  BSKEY(I) = ZERO
               PERFORM SP-BS-RTN THRU SP-BS-EX
               GO TO ACP-BS-EX
           END-IF
           MOVE  BSKEY (I)   TO  BS-KEY.
      *           READ  BS     UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BS_PNAME1 BY REFERENCE BS-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p" RETURNING RESU
               GO  TO  ACP-BS-RTN
           END-IF.
       ACP-BS-010.
           CALL "SD_Accept" USING BY REFERENCE
            ACEP-BSDR-CR "ACEP-BSDR-CR" "9" "1" BY REFERENCE ESTAT
            RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-BS-RTN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-BS-010
           END-IF.
       ACP-BS-020.
           CALL "SD_Accept" USING BY REFERENCE ACEP-BSCOM "ACEP-BSCOM"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-BS-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-BS-020
           END-IF
           IF  I NOT = 6
               ADD 1     TO I
               GO TO ACP-BS-RTN
           END-IF.
       ACP-BS-EX.
           EXIT.
      *************************
      *    損益項目入力処理   *
      *************************
       ACP-PL-RTN.
           COMPUTE L = I + 9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACEP-PLKEY "ACEP-PLKEY"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  I  =  1
                   GO  TO  ACP-PL-EX
               ELSE
                   COMPUTE   I  =  I  -  1
                   GO  TO  ACP-PL-RTN
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-PL-RTN
           END-IF
           IF  PLKEY(I) = ZERO
               PERFORM SP-PL-RTN THRU SP-PL-EX
               GO TO ACP-PL-EX
           END-IF
           MOVE  PLKEY (I)   TO  PL-KEY.
      *           READ  PL        UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" PL_PNAME1 BY REFERENCE PL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p" RETURNING RESU
               GO  TO  ACP-PL-RTN
           END-IF.
       ACP-PL-010.
           CALL "SD_Accept" USING BY REFERENCE ACEP-PLCOM "ACEP-PLCOM"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-PL-RTN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-PL-010
           END-IF
           IF  I NOT = 12
               ADD 1     TO I
               GO TO ACP-PL-RTN
           END-IF.
       ACP-PL-EX.
           EXIT.
      *************************
      *    原価項目入力処理   *
      *************************
       ACP-GN-RTN.
           COMPUTE L = I + 9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE ACEP-GNKEY "ACEP-GNKEY"
            "9" "3" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  I  =  1
                   GO  TO  ACP-GN-EX
               ELSE
                   COMPUTE   I  =  I  -  1
                   GO  TO  ACP-GN-RTN
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-GN-RTN
           END-IF
           IF  GNKEY(I) = ZERO
               PERFORM SP-GN-RTN THRU SP-GN-EX
               GO TO ACP-GN-EX
           END-IF
           MOVE  GNKEY (I)   TO  GEN-KEY.
      *           READ  GEN    UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" GEN_PNAME1 BY REFERENCE GEN-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p" RETURNING RESU
               GO  TO  ACP-GN-RTN
           END-IF.
       ACP-GN-010.
           CALL "SD_Accept" USING BY REFERENCE ACEP-GNCOM "ACEP-GNCOM"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-GN-RTN
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-GN-010
           END-IF
           IF  I NOT = 12
               ADD 1     TO I
               GO TO ACP-GN-RTN
           END-IF.
       ACP-GN-EX.
           EXIT.
      *************************
      *    更新処理           *
      *************************
       UPD-RTN.
           MOVE     AM-KEY      TO      ERR-K.
      *           REWRITE AM-REC  INVALID
      *///////////////
           CALL "DB_Update" USING
            AM_PNAME1 AM_LNAME BY REFERENCE AM-REC RETURNING RET.
           IF  RET = 1
               MOVE    "AM"         TO   ERR-F
               MOVE    "R"          TO   ERR-M
               PERFORM ERR-ENT    THRU   ERR-EXT
           END-IF.
       UPD-EX.
           EXIT.
      ********************************
      *    貸借項目表示処理          *
      ********************************
       DSP-BS-RTN.
           COMPUTE L  =  I  +  9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING
            "SP-BS-AREA" SP-BS-AREA "p" RETURNING RESU.
       DSP-BS-EX.
           EXIT.
      ********************************
      *    損益項目表示処理          *
      ********************************
       DSP-PL-RTN.
           COMPUTE L  =  I  +  9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING
            "SP-PL-AREA" SP-PL-AREA "p" RETURNING RESU.
       DSP-PL-EX.
           EXIT.
      ********************************
      *    原価項目表示処理          *
      ********************************
       DSP-GN-RTN.
           COMPUTE L  =  I  +  9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING
            "SP-GN-AREA" SP-GN-AREA "p" RETURNING RESU.
       DSP-GN-EX.
           EXIT.
      *****
       SP-BS-RTN.
           MOVE ZERO     TO BSGOU(I).
           COMPUTE L  =  I  +  9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING
            "SP-BS-AREA" SP-BS-AREA "p" RETURNING RESU.
           IF  I NOT = 6
               ADD 1     TO I
               GO TO SP-BS-RTN
           END-IF.
       SP-BS-EX.
           EXIT.
      *****
       SP-PL-RTN.
           MOVE ZERO     TO PLGOU(I).
           COMPUTE L  =  I  +  9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING
            "SP-PL-AREA" SP-PL-AREA "p" RETURNING RESU.
           IF  I NOT = 12
               ADD 1     TO I
               GO TO SP-PL-RTN
           END-IF.
       SP-PL-EX.
           EXIT.
      *****
       SP-GN-RTN.
           MOVE ZERO     TO GNGOU(I).
           COMPUTE L  =  I  +  9.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING
            "SP-GN-AREA" SP-GN-AREA "p" RETURNING RESU.
           IF  I NOT = 12
               ADD 1     TO I
               GO TO SP-GN-RTN
           END-IF.
       SP-GN-EX.
           EXIT.
      *******
           COPY    LPMSG_PR.
