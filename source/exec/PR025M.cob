       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR025M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  å¥âøÉ}ÉXÉ^ÉÅÉìÉeÉiÉìÉX        *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/30                      *
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
       01  W-SPACE                 PIC  N(10)  VALUE
                                   "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".              â»ñ⁄ñº
      ***  ANKÇÃSPACEÇ™ì¸ÇÈÇ∆Å@Ç¢ÇØÇ»Ç¢Ç©ÇÁ
       01  W-AREA.
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-LINE      PIC  9(03).                          ÉâÉCÉìáÇ
                   04  W-AREA3.
                       05  W-KAI       PIC  9(01).                      â¸çsêî
                       05  W-GKBN      PIC  9(01).                      çáåvãÊï™
                       05  W-KAMOKUMEI PIC  N(10).                      â»ñ⁄ñº
                       05  W-UKBN      PIC  X(01).
                       05  W-IKBN      PIC  9(01).                      àÛéöãÊï™
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
       COPY  LWMSG_PR.
      ***  êªë¢å¥âøÉ}ÉXÉ^Å@
       COPY  LGENKF.
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
           02  FILLER  PIC  X(001) VALUE " ".                           ACT
           02  CLR-AREA2.
               03  FILLER  PIC  X(003) VALUE "   ".                     ÉâÉCÉìáÇ
               03  CLR-AREA3.
                   04  FILLER  PIC  X(001) VALUE " ".                   â¸çsêî
                   04  FILLER  PIC  X(001) VALUE " ".                   çáåvãÊï™
                   04  CLR-KAMOKUMEI
                       PIC N(10).                                       â»ñ⁄ñº
                   04  FILLER  PIC  X(001) VALUE " ".                   îÑè„ãÊï™
                   04  FILLER  PIC  X(001) VALUE " ".                   àÛéöãÊï™
                   04  FILLER  PIC  X(001) VALUE " ".                   ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
           02  ACP-KAKU        PIC X(01).                               ämîF
       01  ACP-AREA.
           03  ACP-ACT         PIC 9(01).                               ACT
           03  ACP-LINE        PIC 9(03).                               ÉâÉCÉìáÇ
           03  ACP-KAI         PIC 9(01).                               â¸çsêî
           03  ACP-GKBN        PIC 9(01).                               çáåvãÊï™
           03  ACP-KAMOKUMEI   PIC N(10).                               â»ñ⁄ñº
           03  ACP-UKBN        PIC X(01).                               îÑè„ãÊï™
           03  ACP-IKBN        PIC 9(01).                               àÛéöãÊï™
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
            "CLR-AREA1" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-AREA2" " " "0" "0" "28" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA2" "X" "5" "35" "3" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-AREA3" " " "0" "0" "25" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA3" "X" "6" "37" "1" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-AREA3" "X" "7" "37" "1" "01CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-KAMOKUMEI" "N" "8" "18" "20" "02CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "CLR-KAMOKUMEI" BY REFERENCE W-SPACE "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-AREA3" "X" "7" "75" "1" "CLR-KAMOKUMEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-AREA3" "X" "8" "75" "1" "04CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLR-AREA3" "X" "24" "77" "1" "05CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" "X" "24" "77" "1" "CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-LINE" "9" "5" "35" "3" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-LINE" BY REFERENCE W-LINE "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAI" "9" "6" "37" "1" "ACP-LINE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAI" BY REFERENCE W-KAI "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GKBN" "9" "7" "37" "1" "ACP-KAI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-GKBN" BY REFERENCE W-GKBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAMOKUMEI" "N" "8" "18" "20" "ACP-GKBN" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAMOKUMEI" BY REFERENCE W-KAMOKUMEI "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-UKBN" "X" "7" "75" "1" "ACP-KAMOKUMEI" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-UKBN" BY REFERENCE W-UKBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-IKBN" "9" "8" "75" "1" "ACP-UKBN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-IKBN" BY REFERENCE W-IKBN "1" "0" RETURNING RESU.
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
           CALL "SD_Output" USING "CLR-KAMOKUMEI" CLR-KAMOKUMEI "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0250" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" GEN_PNAME1 "SHARED" BY REFERENCE GEN_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
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
                 BY REFERENCE ACP-LINE "ACP-LINE" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
           MOVE  W-LINE         TO  PL-KEY.
      ***  êªë¢å¥âøÉ}ÉXÉ^Å@ÇqÇdÇ`ÇcÅ@
      *           READ  GEN  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" GEN_PNAME1 BY REFERENCE PL-REC " " RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-020
           END-IF.
           GO  TO  MAIN-030.
       MAIN-020.
           IF  W-ACT = 2 OR 3
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
      ***  É}ÉXÉ^ñ¢ìoò^
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               GO  TO  MAIN-040
           END-IF.
       MAIN-030.
           IF  W-ACT = 1
               CALL "SD_Output" USING "NOR-M01" NOR-M01 "p"
                                  RETURNING RESU
      ***  É}ÉXÉ^ìoò^çœÇ›
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               MOVE  PL-KEY     TO  W-LINE
               MOVE  PL-LIN     TO  W-KAI
               MOVE  PL-GKB     TO  W-GKBN
               MOVE  PL-NAMN    TO  W-KAMOKUMEI
               MOVE  PL-URIKB   TO  W-UKBN
               MOVE  PL-PKB     TO  W-IKBN
               CALL "SD_Output" USING "ACP-AREA" ACP-AREA "p"
                                  RETURNING RESU
           END-IF.
           IF  W-ACT NOT = 2
               GO  TO  MAIN-090
           END-IF.
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAI "ACP-KAI" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-040
           END-IF.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-GKBN "ACP-GKBN" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-040
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-050
           END-IF.
       MAIN-060.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAMOKUMEI "ACP-KAMOKUMEI" "N" "20"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-050
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-060
           END-IF.
           CALL "SD_Output" USING "ACP-KAMOKUMEI" ACP-KAMOKUMEI "p"
                                  RETURNING RESU.
       MAIN-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-UKBN "ACP-UKBN" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-060
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-070
           END-IF.
       MAIN-080.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-IKBN "ACP-IKBN" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-070
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-080
           END-IF.
       MAIN-090.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-010
               ELSE
                   GO  TO  MAIN-080
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
               GO  TO  MAIN-090
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
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
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
           MOVE  SPACE     TO  PL-REC.
           INITIALIZE  PL-REC.
               MOVE  W-LINE     TO  PL-KEY.
               MOVE  W-KAI      TO  PL-LIN.
               MOVE  W-GKBN     TO  PL-GKB.
               MOVE  W-KAMOKUMEI TO  PL-NAMN.
               MOVE  W-UKBN     TO  PL-URIKB.
               MOVE  W-IKBN     TO  PL-PKB.
           MOVE  PL-KEY        TO  ERR-K.
      *           WRITE  PL-REC  INVALID
      *///////////////
           CALL "DB_Insert" USING
            GEN_PNAME1 GEN_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               MOVE  "GEN"     TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
               MOVE  W-LINE     TO  PL-KEY.
               MOVE  W-KAI      TO  PL-LIN.
               MOVE  W-GKBN     TO  PL-GKB.
               MOVE  W-KAMOKUMEI TO  PL-NAMN.
               MOVE  W-UKBN     TO  PL-URIKB.
               MOVE  W-IKBN     TO  PL-PKB.
           MOVE  PL-KEY        TO  ERR-K.
      *           REWRITE  PL-REC  INVALID 
      *///////////////
           CALL "DB_Update" USING
            GEN_PNAME1 GEN_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               MOVE  "GEN"     TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-LINE        TO  PL-KEY.
           MOVE  PL-KEY        TO  ERR-K.
      *           DELETE  GEN INVALID
      *///////////////
           CALL "DB_Delete" USING GEN_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "GEN"     TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
       COPY  LPMSG_PR.
