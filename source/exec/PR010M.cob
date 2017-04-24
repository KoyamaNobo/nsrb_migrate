       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR010M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  äøéöâ»ñ⁄É}ÉXÉ^ÉÅÉìÉeÉiÉìÉX    *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/13                      *
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
                   04  W-KAMOKUCD  PIC  9(04).                          â»ñ⁄ÇbÇc
                   04  W-HOJOCD    PIC  9(04).                          ï‚èïÇbÇc
                   04  W-AREA3.
                       05  W-KAMOKUMEI PIC  N(10).                      â»ñ⁄ñº
                       05  W-KAZEIKBN  PIC  X(01).                      â€ê≈ãÊï™
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
       COPY  LWMSG_PR.
      ***
      ***  äøéöâ»ñ⁄É}ÉXÉ^
           COPY  KANGEL.
      **
      ******************************
      *Å@Å@âÊñ ÉNÉäÉAÅ[çÄñ⁄Å@Å@    *
      ******************************
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           03  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA1.
           02  FILLER  PIC X(001) VALUE " ".                            ACT
           02  CLR-AREA2.
               03  FILLER  PIC X(004) VALUE "    ".                     â»ñ⁄ÇbÇc
               03  FILLER  PIC X(004) VALUE "    ".                     ï‚èïÇbÇc
               03  CLR-AREA3.
                   04  CLR-KAMOKUMEI  PIC N(10).                        â»ñ⁄ñº
                   04  FILLER  PIC X(001) VALUE " ".                    â€ê≈ãÊï™
                   04  FILLER  PIC X(001) VALUE " ".                    ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT         PIC 9(01).
           03  ACP-KAMOKUCD    PIC 9(04).
           03  ACP-HOJOCD      PIC 9(04).
           03  ACP-KAMOKUMEI   PIC N(10).
           03  ACP-KAZEIKBN    PIC X(01).
           03  ACP-KAKU        PIC X(01).
      ***
       COPY  LSMSG_PR.
      ***
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA1
       CALL "SD_Init" USING
            "CLR-AREA1" " " "0" "0" "31" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "30" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "5" "34" "4" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "X" "6" "34" "4" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA3" " " "0" "0" "22" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-KAMOKUMEI" "N" "7" "18" "20" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_From" USING
            "CLR-KAMOKUMEI" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA3" "X" "7" "75" "1" "CLR-KAMOKUMEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA3" "X" "24" "77" "1" "02CLR-AREA3" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "31" " " " " RETURNING RESU.
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
            "ACP-HOJOCD" "9" "6" "34" "4" "ACP-KAMOKUCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-HOJOCD" BY REFERENCE W-HOJOCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAMOKUMEI" "N" "7" "18" "20" "ACP-HOJOCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAMOKUMEI" BY REFERENCE W-KAMOKUMEI "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAZEIKBN" "X" "7" "75" "1" "ACP-KAMOKUMEI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAZEIKBN" BY REFERENCE W-KAZEIKBN "1" "0" 
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-KAZEIKBN" " "
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
           CALL "SD_Output" USING "CLR-KAMOKUMEI" CLR-KAMOKUMEI "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0100" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
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
                 BY REFERENCE ACP-KAMOKUCD "ACP-KAMOKUCD"  "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-HOJOCD "ACP-HOJOCD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
      *
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
      *
           MOVE  W-KAMOKUCD     TO  K-ACCD.
           MOVE  W-HOJOCD       TO  K-HOCD.
      ***  äøéöâ»ñ⁄É}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  KNG  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R " " RETURNING RET.
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
               GO  TO  MAIN-040
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
               MOVE  KNGNMN     TO  W-KAMOKUMEI                         â»ñ⁄ñº
               MOVE  KNGTAX     TO  W-KAZEIKBN                          â€ê≈ãÊï™
               CALL "SD_Output" USING "ACP-KAMOKUMEI" ACP-KAMOKUMEI "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "ACP-KAZEIKBN" ACP-KAZEIKBN "p"
                                  RETURNING RESU
           END-IF.
           IF  W-ACT NOT = 2
               GO  TO  MAIN-060
           END-IF.
      **
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAMOKUMEI "ACP-KAMOKUMEI" "N" "20"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-040
           END-IF.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAZEIKBN "ACP-KAZEIKBN" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-040
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-050
           END-IF.
           IF  W-KAZEIKBN NOT =
                    " " AND "1" AND "2" AND "3" AND "5" AND "6" AND "7"
               GO TO MAIN-050
           END-IF.
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
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
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
           MOVE  SPACE     TO  KNG-R.
           INITIALIZE  KNG-R.
           MOVE  W-KAMOKUCD     TO  K-ACCD.                             â»ñ⁄ÇbÇc
           MOVE  W-HOJOCD       TO  K-HOCD.                             ï‚èïÇbÇc
           MOVE  W-KAMOKUMEI    TO  KNGNMN.                             â»ñ⁄ñº
           MOVE  W-KAZEIKBN     TO  KNGTAX.                             â€ê≈ãÊï™
           MOVE  KNG-KEY        TO  ERR-K.
      *           WRITE  KNG-R  INVALID
      *///////////////
           CALL "DB_Insert" USING
            KNG_PNAME1 KNG_LNAME KNG-R RETURNING RET.
           IF  RET = 1
               MOVE  "KNG"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-KAMOKUCD     TO  K-ACCD.                             â»ñ⁄ÇbÇc
           MOVE  W-HOJOCD       TO  K-HOCD.                             ï‚èïÇbÇc
           MOVE  W-KAMOKUMEI    TO  KNGNMN.                             â»ñ⁄ñº
           MOVE  W-KAZEIKBN     TO  KNGTAX.                             â€ê≈ãÊï™
           MOVE  KNG-KEY        TO  ERR-K.
      *           REWRITE  KNG-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            KNG_PNAME1 KNG_LNAME KNG-R RETURNING RET.
           IF  RET = 1
               MOVE  "KNG"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-KAMOKUCD     TO  K-ACCD.                             â»ñ⁄ÇbÇc
           MOVE  W-HOJOCD       TO  K-HOCD.                             ï‚èïÇbÇc
           MOVE  KNG-KEY        TO  ERR-K.
      *           DELETE  KNG  INVALID
      *///////////////
           CALL "DB_Delete" USING KNG_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "KNG"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
