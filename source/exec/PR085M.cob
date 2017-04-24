       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR085M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  ìEóvÉ}ÉXÉ^ÉÅÉìÉeÉiÉìÉX        *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/26                      *
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
       01  W-SPACE                 PIC  N(20)  VALUE
                  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".           ìKóvñº
      ***  ANKÇÃSPACEÇ™ì¸ÇÈÇ∆Å@Ç¢ÇØÇ»Ç¢Ç©ÇÁ
       01  W-AREA.
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-TEKIYOUCD     PIC  9(03).                      ìEóvÇbÇc
                   04  W-AREA3.
                       05  W-TEKIYOUMEI    PIC  N(20).                  ìEóvñº
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
           COPY  LWMSG_PR.
      ***  ìEóvÉ}ÉXÉ^
           COPY  LTKI.
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
           03  FILLER      PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA1.
           02  FILLER      PIC X(001) VALUE " ".                        ACT
           02  CLR-AREA2.
               03  FILLER      PIC X(003) VALUE "   ".                  ìEóvÇbÇc
               03  CLR-AREA3.
                   04  CLR-TEKIYOUMEI  PIC N(20).                       ìEóvñº
                   04  FILLER      PIC X(001) VALUE " ".                ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT         PIC 9(01).                               ACT
           03  ACP-TEKIYOUCD   PIC 9(03).                               ìEóvÇbÇc
           03  ACP-TEKIYOUMEI  PIC N(20).                               ìEóvñº
           03  ACP-KAKU        PIC X(01).                               ämîF
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
            "CLR-AREA1" " " "0" "0" "45" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "44" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "5" "35" "3" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA3" " " "0" "0" "41" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-TEKIYOUMEI" "N" "6" "18" "40" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_From" USING
            "CLR-TEKIYOUMEI" BY REFERENCE W-SPACE "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" "X" "24" "77" "1" "CLR-TEKIYOUMEI" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "45" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEKIYOUCD" "9" "5" "35" "3" "ACP-ACT" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TEKIYOUCD" BY REFERENCE W-TEKIYOUCD "3" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEKIYOUMEI" "N" "6" "18" "40" "ACP-TEKIYOUCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TEKIYOUMEI" BY REFERENCE W-TEKIYOUMEI "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-TEKIYOUMEI" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
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
           CALL "SD_Output" USING "CLR-TEKIYOUMEI" CLR-TEKIYOUMEI "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0850" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TKI_PNAME1 "SHARED" BY REFERENCE TKI_IDLST "1"
            "TKI-KEY" BY REFERENCE TKI-KEY.
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
                 BY REFERENCE ACP-TEKIYOUCD "ACP-TEKIYOUCD" "9" "3"
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
           MOVE  W-TEKIYOUCD    TO  TKI-KEY.                            ìEóvÇbÇc
      ***  ìEóvÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  TKI  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TKI_PNAME1 BY REFERENCE TKI-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-020
           END-IF.
           GO  TO  MAIN-030.
      **
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
               MOVE  TKI-02     TO  W-TEKIYOUMEI                        ìEóvñº
               CALL "SD_Output" USING
                                  "ACP-TEKIYOUMEI" ACP-TEKIYOUMEI "p"
                                  RETURNING RESU
           END-IF.
           IF  W-ACT NOT = 2
               GO  TO  MAIN-050
           END-IF.
      **
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TEKIYOUMEI "ACP-TEKIYOUMEI" "N" "40"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-040
           END-IF.
           CALL "SD_Output" USING "ACP-TEKIYOUMEI" ACP-TEKIYOUMEI "p"
                                  RETURNING RESU.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-010
               ELSE
                   GO  TO  MAIN-040
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
               GO  TO  MAIN-050
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
           CALL "DB_F_Close" USING BY REFERENCE TKI_IDLST TKI_PNAME1.
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
           MOVE  SPACE     TO  TKI-R.
           INITIALIZE  TKI-R.
           MOVE  W-TEKIYOUCD    TO  TKI-KEY.                            ìEóvÇbÇc
           MOVE  W-TEKIYOUMEI   TO  TKI-02.                             ìEóvñº
           MOVE  TKI-KEY        TO  ERR-K.
      *           WRITE  TKI-R  INVALID
      *///////////////
           CALL "DB_Insert" USING
            TKI_PNAME1 TKI_LNAME TKI-R RETURNING RET.
           IF  RET = 1
               MOVE  "TKI"     TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-TEKIYOUCD    TO  TKI-KEY.                            ìEóvÇbÇc
           MOVE  W-TEKIYOUMEI   TO  TKI-02.                             ìEóvñº
           MOVE  TKI-KEY        TO  ERR-K.
      *           REWRITE  TKI-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            TKI_PNAME1 TKI_LNAME TKI-R RETURNING RET.
           IF  RET = 1
               MOVE  "TKI"     TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-TEKIYOUCD    TO  TKI-KEY.                            ìEóvÇbÇc
           MOVE  TKI-KEY        TO  ERR-K.
      *           DELETE  TKI  INVALID
      *///////////////
           CALL "DB_Delete" USING TKI_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "TKI"     TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
