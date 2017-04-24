       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR045M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  åªÅDóaã‡É}ÉXÉ^ÉÅÉìÉeÉiÉìÉX    *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/15                      *
      *    COMPILE TYPE  :  COBOL                         *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.     SYSTEM3100.
       OBJECT-COMPUTER.     SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC  X(02).
       77  W-SPACE                 PIC  N(10)  VALUE
           "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".
       01  W-AREA.
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-KAMOKUCD  PIC  9(04).                          â»ñ⁄ÇbÇc
                   04  W-GINCD     PIC  9(04).
                   04  W-KNAM      PIC  N(10).
                   04  W-GNAM      PIC  N(10).
                   04  W-AREA3.
                       05  W-ZAN       PIC S9(11).                      ëOì˙écçÇ
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
           COPY  LWMSG_PR.
      ***  åªÅDóaã‡É}ÉXÉ^
           COPY  LGYM.
           COPY  L-BANK.
           COPY  KANGEL.
      **
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      ******************************
      *Å@Å@âÊñ ÉNÉäÉAÅ[çÄñ⁄Å@Å@    *
      ******************************
       01  DSP-CLR.
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA1.
           02  FILLER  PIC X(001) VALUE " ".                            ACT
           02  CLR-AREA2.
               03  FILLER  PIC X(004) VALUE "    ".                     â»ñ⁄ÇbÇc
               03  FILLER  PIC N(10).
               03  FILLER  PIC X(004) VALUE "    ".                     ã‚çsÇbÇc
               03  FILLER  PIC N(10).
               03  CLR-AREA3.
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(001) VALUE " ".                    ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT         PIC 9(01).                               ACT
           03  ACP-KAMOKUCD    PIC 9(04).                               â»ñ⁄ÇbÇc
      *    ã‚çsÇbÇc
           03  ACP-GINCD       PIC 9(04).
      *    ëOì˙écçÇ
           03  ACP-ZAN         PIC S9(11).
           03  ACP-KAKU        PIC X(01).                               ämîF
      ***********************
      *    âÊñ ï\é¶         *
      ***********************
       01  DSP-DSP.
           03  DSP-ZAN         PIC ZZZZZZZZZZ9-.
           03  DSP-KNAM        PIC N(10).
           03  DSP-GNAM        PIC N(10).
      ***
       COPY  LSMSG_PR.
      ***
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA1
       CALL "SD_Init" USING
            "CLR-AREA1" " " "0" "0" "62" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "61" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "5" "34" "4" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "N" "5" "56" "20" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "02CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA2" "X" "6" "34" "4" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA2" "N" "6" "56" "20" "03CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA3" " " "0" "0" "13" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" "X" "7" "27" "12" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA3" "X" "24" "77" "1" "01CLR-AREA3" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "21" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAMOKUCD" "9" "5" "34" "4" "ACP-ACT" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAMOKUCD" BY REFERENCE W-KAMOKUCD "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-GINCD" "9" "6" "34" "4" "ACP-KAMOKUCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-GINCD" BY REFERENCE W-GINCD "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZAN" "S9" "7" "27" "11" "ACP-GINCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ZAN" BY REFERENCE W-ZAN "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-ZAN" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *DSP-DSP
       CALL "SD_Init" USING
            "DSP-DSP" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-ZAN" "ZZZZZZZZZZ9-" "7" "27" "12" " " "DSP-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-ZAN" BY REFERENCE W-ZAN "12" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KNAM" "N" "5" "56" "20" "DSP-ZAN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KNAM" BY REFERENCE W-KNAM "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-GNAM" "N" "6" "56" "20" "DSP-KNAM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-GNAM" BY REFERENCE W-GNAM "20" "0"
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           PERFORM  MAIN-RTN    THRU  MAIN-EX.
           PERFORM  CLSE-ENT     THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP  RUN.
      **************************
      *    èâä˙èàóù            *
      **************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0450" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" GYM_PNAME1 "SHARED" BY REFERENCE GYM_IDLST "1"
            "GYM-KEY" BY REFERENCE GYM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BM_PNAME1 "SHARED" BY REFERENCE BM_IDLST "1"
            "BM-KEY" BY REFERENCE BM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           INITIALIZE   W-AREA.
       INI-EX.
           EXIT.
      *****************************
      *    ÇlÇ`ÇhÇmÅ@èàóùÅ@Å@Å@Å@ *
      *****************************
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "P9"
               GO  TO  MAIN-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO  TO  MAIN-RTN
           END-IF.
           CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA2.
       MAIN-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAMOKUCD "ACP-KAMOKUCD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           MOVE  W-KAMOKUCD  TO  K-ACCD.
           MOVE  ZERO        TO  K-HOCD.
      *           READ  KNG         UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                RETURNING RESU
               GO  TO  MAIN-010
           END-IF.
           MOVE  KNGNMN      TO  W-KNAM.
      *    â»ñ⁄ñº
           CALL "SD_Output" USING "DSP-KNAM" DSP-KNAM "p"
                                  RETURNING RESU.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-GINCD "ACP-GINCD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
           IF  W-GINCD = ZERO
               MOVE SPACE     TO BANKNMN
               GO TO MAIN-029
           END-IF.
           MOVE  W-GINCD     TO  BM-KEY.
      *           READ  BM          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BM_PNAME1 BY REFERENCE BM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                RETURNING RESU
               GO  TO  MAIN-020
           END-IF.
       MAIN-029.
           MOVE  BANKNMN     TO  W-GNAM.
           CALL "SD_Output" USING "DSP-GNAM" DSP-GNAM "p"
                                  RETURNING RESU.
      *
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
      *
           MOVE  W-KAMOKUCD     TO  GYM-011.
           MOVE  W-GINCD        TO  GYM-012.
      ***  åªÅDóaã‡É}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  GYM  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" GYM_PNAME1 BY REFERENCE GYM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-030
           END-IF.
           GO  TO  MAIN-035.
      **
       MAIN-030.
           IF  W-ACT = 2 OR 3
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
      ***  É}ÉXÉ^ñ¢ìoò^
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               GO  TO  MAIN-050
           END-IF.
       MAIN-035.
           IF  W-ACT = 1
               CALL "SD_Output" USING "NOR-M01" NOR-M01 "p"
                                  RETURNING RESU
      ***  É}ÉXÉ^ìoò^çœÇ›
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               MOVE  GYM-03     TO  W-ZAN
               CALL "SD_Output" USING "DSP-ZAN" DSP-ZAN "p"
                                  RETURNING RESU
           END-IF.
           IF  W-ACT NOT = 2
               GO  TO  MAIN-060
           END-IF.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ZAN "ACP-ZAN" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-050
           END-IF.
           CALL "SD_Output" USING "DSP-ZAN" DSP-ZAN "p"
                                  RETURNING RESU.
       MAIN-060.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-020
               ELSE
                   GO  TO  MAIN-050
               END-IF
           END-IF.
           IF  W-KAKU = 9                                               = "02"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CLR-AREA1" CLR-AREA1 "p"
                                  RETURNING RESU
               INITIALIZE  W-AREA1
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU NOT = 1                                           = "04"
               GO  TO  MAIN-060
           END-IF.
           PERFORM  KOU-RTN     THRU  KOU-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA2.
           GO  TO  MAIN-010.
       MAIN-EX.
           EXIT.
      ************************
      *    èIóπèàóù          *
      ************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE GYM_IDLST GYM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BM_IDLST BM_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ÇjÇnÇtÅ|ÇqÇsÇm      *
      **************************
       KOU-RTN.
           IF  W-ACT = 1
               PERFORM  WRITE-RTN     THRU  WRITE-EX
           END-IF.
           IF  W-ACT = 2
               PERFORM  REWRITE-RTN   THRU  REWRITE-EX
           END-IF.
           IF  W-ACT = 3
               PERFORM  DELETE-RTN    THRU  DELETE-EX
           END-IF.
       KOU-EX.
           EXIT.
      ******************************
      *    ÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      ******************************
       WRITE-RTN.
           MOVE  SPACE     TO  GYM-R.
           INITIALIZE  GYM-R.
           MOVE  W-KAMOKUCD     TO  GYM-011.
           MOVE  W-GINCD        TO  GYM-012.
           MOVE  W-ZAN          TO  GYM-03.
           MOVE  GYM-KEY        TO  ERR-K.
      *           WRITE  GYM-R  INVALID
      *///////////////
           CALL "DB_Insert" USING
            GYM_PNAME1 GYM_LNAME GYM-R RETURNING RET.
           IF  RET = 1
               MOVE  "GYM"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-KAMOKUCD     TO  GYM-011.
           MOVE  W-GINCD        TO  GYM-012.
           MOVE  W-ZAN          TO  GYM-03.
           MOVE  GYM-KEY        TO  ERR-K.
      *           REWRITE  GYM-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            GYM_PNAME1 GYM_LNAME GYM-R RETURNING RET.
           IF  RET = 1
               MOVE  "GYM"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-KAMOKUCD     TO  GYM-011.
           MOVE  W-GINCD        TO  GYM-012.
           MOVE  GYM-KEY        TO  ERR-K.
      *           DELETE  GYM  INVALID
      *///////////////
           CALL "DB_Delete" USING GYM_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "GYM"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
