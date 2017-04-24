       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR095M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  ï‚èïécçÇÉ}ÉXÉ^ÉÅÉìÉeÉiÉìÉX    *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/12/07                      *
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
       77  W-SPACE                 PIC  N(10)
                  VALUE  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".
      ***  ANK ÇÃ SPACE Ç™ ÇÕÇ¢Ç¡ÇΩÇÁÇ¢ÇØÇ»Ç¢ÇÃÇ≈
       77  W-OWARI                 PIC  X(05).
      ***  SPACE Ç∂Ç·Ç»Ç©Ç¡ÇΩÇÁÅCSTOP RUN  Ç≈èIóπÅB
       77  I                       PIC  9(02).                          ìYéö
      ***  âÊñ è„ÇÃìYéö
       77  J                       PIC  9(02).                          ìYéö
      ***  BZM-FÇ…èëÇ≠éûÇÃìYéö
       77  LIN                     PIC  9(02).
       01  W-AREA.
           02  W-Z9                PIC  Z9.
           02  W-TOUYOK-NC         PIC  N(02).
           02  KETSAN              PIC  9(02).                          èâåàéZåé
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉãÇ©ÇÁÇÃåàéZåéÇÇΩÇﬂÇ∆Ç≠
           02  TOUYOK-MONTH        PIC  9(02).                          åàéZåé
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-KACD      PIC  9(04).                          â»ñ⁄ÇbÇc
                   04  W-HOCD      PIC  9(04).                          ï‚èïÇbÇc
                   04  W-KAMEI     PIC  N(10).                          â»ñ⁄ñºÅ@
                   04  W-HOMEI     PIC  N(10).                          ï‚èïñºÅ@
                   04  W-AREA3.
                       05  W-ZMZ                PIC S9(11).             ëOä˙ññéc
                       05  W-AREA4     OCCURS 15.
                           07  W-TOUYOK-KARI       PIC S9(11).          ìñóÇéÿï˚
                           07  W-TOUYOK-KASHI      PIC S9(11).          ìñóÇë›ï˚
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
           COPY  LWMSG_PR.
      ***  ï‚èïécçÇÉ}ÉXÉ^
           COPY  LHOZAN.
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉã
           COPY  FCTL.
      ***  äøéöâ»ñ⁄É}ÉXÉ^
           COPY  KANGEL.
      ***
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
               03  FILLER  PIC  X(004) VALUE "    ".                    â»ñ⁄ÇbÇc
               03  FILLER  PIC  X(004) VALUE "    ".                    ï‚èïÇbÇc
               03  FILLER  PIC N(10).                                   â»ñ⁄ñºÅ@
               03  FILLER  PIC N(10).                                   ï‚èïñºÅ@
               03  CLR-AREA3.
                   04  FILLER  PIC  X(012) VALUE "            ".        ëOä˙ññéc
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC  X(012) VALUE "            ".
                       05  FILLER  PIC  X(012) VALUE "            ".
                   04  FILLER  PIC  X(001) VALUE " ".                   ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT               PIC 9(01).                         ACT
           03  ACP-KACD              PIC 9(04).                         â»ñ⁄ÇbÇc
           03  ACP-HOCD              PIC 9(04).                         ï‚èïÇbÇc
           03  ACP-ZMZ               PIC S9(11).                        ëOä˙ññéc
           03  ACP-TOUYOK-KARI       PIC S9(11).                        ìñóÇéÿï˚
           03  ACP-TOUYOK-KASHI      PIC S9(11).                        ìñóÇë›ï˚
           03  ACP-KAKU              PIC X(01).                         ämîF
      *********************
      *    âÊñ ï\é¶       *
      *********************
       01  DSP-DSP.
           03  DSP-TOUYOK-TSUKI     PIC N(02).                          ìñóÇä˙åé
           03  DSP-ZMZ              PIC ZZZZZZZZZZ9-.                   ëOä˙ññéc
           03  DSP-KAMEI            PIC N(10).                           â»ñ⁄ñº
           03  DSP-HOMEI            PIC N(10).                           ï‚èïñº
           03  DSP-TOUYOK-KARI      PIC ZZZZZZZZZZ9-.                   ìñóÇéÿï˚
           03  DSP-TOUYOK-KASHI     PIC ZZZZZZZZZZ9-.                   ìñóÇë›ï˚
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
            "CLR-AREA1" " " "0" "0" "422" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "66" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "421" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "4" "34" "4" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "X" "5" "34" "4" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA2" "N" "4" "56" "20" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA2" "N" "5" "56" "20" "03CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA3" " " "0" "0" "373" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" "X" "6" "27" "12" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA3" " " "9" "0" "24" "01CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA3" "X" "9" "38" "12" " " "02CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA3" "X" "9" "51" "12" "03CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA3" " " "10" "0" "24" "02CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA3" "X" "10" "38" "12" " " "05CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-AREA3" "X" "10" "51" "12" "06CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-AREA3" " " "11" "0" "24" "05CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-AREA3" "X" "11" "38" "12" " " "08CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-AREA3" "X" "11" "51" "12" "09CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11CLR-AREA3" " " "12" "0" "24" "08CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12CLR-AREA3" "X" "12" "38" "12" " " "11CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "13CLR-AREA3" "X" "12" "51" "12" "12CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "14CLR-AREA3" " " "13" "0" "24" "11CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15CLR-AREA3" "X" "13" "38" "12" " " "14CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "16CLR-AREA3" "X" "13" "51" "12" "15CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "17CLR-AREA3" " " "14" "0" "24" "14CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "18CLR-AREA3" "X" "14" "38" "12" " " "17CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "19CLR-AREA3" "X" "14" "51" "12" "18CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "20CLR-AREA3" " " "15" "0" "24" "17CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "21CLR-AREA3" "X" "15" "38" "12" " " "20CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "22CLR-AREA3" "X" "15" "51" "12" "21CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "23CLR-AREA3" " " "16" "0" "24" "20CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "24CLR-AREA3" "X" "16" "38" "12" " " "23CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "25CLR-AREA3" "X" "16" "51" "12" "24CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "26CLR-AREA3" " " "17" "0" "24" "23CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "27CLR-AREA3" "X" "17" "38" "12" " " "26CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "28CLR-AREA3" "X" "17" "51" "12" "27CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "29CLR-AREA3" " " "18" "0" "24" "26CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "30CLR-AREA3" "X" "18" "38" "12" " " "29CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "31CLR-AREA3" "X" "18" "51" "12" "30CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "32CLR-AREA3" " " "19" "0" "24" "29CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "33CLR-AREA3" "X" "19" "38" "12" " " "32CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "34CLR-AREA3" "X" "19" "51" "12" "33CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "35CLR-AREA3" " " "20" "0" "24" "32CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "36CLR-AREA3" "X" "20" "38" "12" " " "35CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "37CLR-AREA3" "X" "20" "51" "12" "36CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "38CLR-AREA3" " " "21" "0" "24" "35CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "39CLR-AREA3" "X" "21" "38" "12" " " "38CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "40CLR-AREA3" "X" "21" "51" "12" "39CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "41CLR-AREA3" " " "22" "0" "24" "38CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "42CLR-AREA3" "X" "22" "38" "12" " " "41CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "43CLR-AREA3" "X" "22" "51" "12" "42CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "44CLR-AREA3" " " "23" "0" "24" "41CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "45CLR-AREA3" "X" "23" "38" "12" " " "44CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "46CLR-AREA3" "X" "23" "51" "12" "45CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "47CLR-AREA3" "X" "24" "77" "1" "44CLR-AREA3" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "43" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "66" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD" "9" "4" "34" "4" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD" BY REFERENCE W-KACD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-HOCD" "9" "5" "34" "4" "ACP-KACD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-HOCD" BY REFERENCE W-HOCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZMZ" "S9" "6" "27" "11" "ACP-HOCD" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-ZMZ" BY REFERENCE W-ZMZ "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOUYOK-KARI" "S9" "LIN" "38" "11" "ACP-ZMZ" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-TOUYOK-KARI" BY REFERENCE W-TOUYOK-KARI(1) "11" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOUYOK-KASHI" "S9" "LIN" "51" "11"
            "ACP-TOUYOK-KARI" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-TOUYOK-KASHI" BY REFERENCE W-TOUYOK-KASHI(1) "11" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-TOUYOK-KASHI" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *DSP-DSP
       CALL "SD_Init" USING
            "DSP-DSP" " " "0" "0" "80" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUYOK-TSUKI" "N" "LIN" "31" "4" " " "DSP-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUYOK-TSUKI" BY REFERENCE W-TOUYOK-NC "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-ZMZ" "ZZZZZZZZZZ9-" "6" "27" "12" "DSP-TOUYOK-TSUKI"
            " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-ZMZ" BY REFERENCE W-ZMZ "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KAMEI" "N" "4" "56" "20" "DSP-ZMZ" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KAMEI" BY REFERENCE W-KAMEI "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HOMEI" "N" "5" "56" "20" "DSP-KAMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-HOMEI" BY REFERENCE W-HOMEI "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUYOK-KARI" "ZZZZZZZZZZ9-" "LIN" "38" "12"
            "DSP-HOMEI" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUYOK-KARI" BY REFERENCE W-TOUYOK-KARI(1) "12" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUYOK-KASHI" "ZZZZZZZZZZ9-" "LIN" "51" "12"
            "DSP-TOUYOK-KARI" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUYOK-KASHI" BY REFERENCE W-TOUYOK-KASHI(1) "12" "1"
            BY REFERENCE I 22 RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           IF  W-OWARI NOT = SPACE
               GO  TO  PROCE-010
           END-IF.
           PERFORM  MAIN-RTN    THRU  MAIN-EX.
       PROCE-010.
           PERFORM  CLSE-ENT     THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP  RUN.
      **************************
      *    èâä˙èàóù            *
      **************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0950" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE  "DATE  "     TO  FCTL-KEY.
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉãÅ@ÇqÇdÇ`Çc
      *           READ  FCTL-F  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-MCT" INV-MCT "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                  RETURNING RESU
               MOVE  "OWARI"     TO  W-OWARI
               GO  TO  INI-EX
           END-IF.
           MOVE  FCTL-KSMM   TO  KETSAN.
           ADD  1     TO  KETSAN.
           IF  KETSAN = 13
               MOVE  1     TO  KETSAN
           END-IF.
      ***  ï€ë∂ÇµÇƒÇ®Ç≠ÅB
           MOVE  KETSAN      TO  TOUYOK-MONTH.
      ***  ë´ÇµÇƒÇ¢Ç≠ÅB
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       INI-010.
           IF  I NOT < 13
               GO  TO  INI-020
           END-IF.
           MOVE  TOUYOK-MONTH     TO  W-Z9.
           MOVE  W-Z9                TO  W-TOUYOK-NC.
           CALL "SD_Output" USING "DSP-TOUYOK-TSUKI"
                                  DSP-TOUYOK-TSUKI "p"
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           ADD  1     TO  TOUYOK-MONTH.
           IF  TOUYOK-MONTH = 13
               MOVE  1     TO TOUYOK-MONTH
           END-IF.
           GO  TO  INI-010.
      *
       INI-020.
           MOVE  KETSAN     TO  TOUYOK-MONTH.
           MOVE  1     TO  I.
           MOVE  21    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       INI-030.
           IF  I NOT < 4
               GO  TO  INI-EX
           END-IF.
           MOVE  TOUYOK-MONTH     TO  W-Z9.
           MOVE  W-Z9                TO  W-TOUYOK-NC.
           CALL "SD_Output" USING "DSP-TOUYOK-TSUKI"
                                  DSP-TOUYOK-TSUKI "p"
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           ADD  1     TO  TOUYOK-MONTH.
           IF  TOUYOK-MONTH = 13
               MOVE  1     TO  TOUYOK-MONTH
           END-IF.
           GO  TO  INI-030.
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
                 BY REFERENCE ACP-KACD "ACP-KACD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
      *
           MOVE  W-KACD     TO  K-ACCD.                                 â»ñ⁄ÇbÇc
           MOVE  ZERO       TO  K-HOCD.                                 ï‚èïÇbÇc
      ***  äøéöâ»ñ⁄É}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  KNG  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  KNGNMN
           END-IF.
           MOVE  KNGNMN     TO  W-KAMEI.
           CALL "SD_Output" USING "DSP-KAMEI" DSP-KAMEI "p"
                                  RETURNING RESU.
      *
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-HOCD "ACP-HOCD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
           IF  W-HOCD = ZERO
               GO TO MAIN-020
           END-IF.
           MOVE  W-KACD     TO  K-ACCD.                                 â»ñ⁄ÇbÇc
           MOVE  W-HOCD     TO  K-HOCD.                                 ï‚èïÇbÇc
      ***  äøéöâ»ñ⁄É}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  KNG  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  KNGNMN
           END-IF.
           MOVE  KNGNMN     TO  W-HOMEI.
           CALL "SD_Output" USING "DSP-HOMEI" DSP-HOMEI "p"
                                  RETURNING RESU.
      *
      *
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
      *
      *
           MOVE  W-KACD      TO  HZM-KMCD.                              â»ñ⁄ÇbÇc
           MOVE  W-HOCD      TO  HZM-HOCD.                              ï‚èïÇbÇc
      ***  ï‚èïécçÇÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  HZM-F  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HZM-F_PNAME1 BY REFERENCE HZM-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-030
           END-IF.
           GO  TO  MAIN-040.
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
       MAIN-040.
           IF  W-ACT = 1
               CALL "SD_Output" USING "NOR-M01" NOR-M01 "p"
                                  RETURNING RESU
      ***  É}ÉXÉ^ìoò^çœÇ›
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               PERFORM  WORK-RTN     THRU  WORK-EX
               PERFORM  DSP-RTN      THRU  DSP-EX
               IF  W-ACT = 3
                   GO  TO  MAIN-080
               END-IF
           END-IF.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ZMZ "ACP-ZMZ" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01"
               GO  TO  MAIN-050
           END-IF.
           CALL "SD_Output" USING "DSP-ZMZ" DSP-ZMZ "p"
                                  RETURNING RESU.
      *
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       MAIN-060.
           IF  I NOT < 16
               GO  TO  MAIN-080
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TOUYOK-KARI "ACP-TOUYOK-KARI"
                 "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   GO  TO  MAIN-050
               ELSE
                   SUBTRACT  1     FROM  I  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-060
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-060
           END-IF.
           CALL "SD_Output" USING "DSP-TOUYOK-KARI" DSP-TOUYOK-KARI "p"
                                  RETURNING RESU.
       MAIN-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TOUYOK-KASHI "ACP-TOUYOK-KASHI"
                 "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-060
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-070
           END-IF.
           CALL "SD_Output" USING "DSP-TOUYOK-KASHI"
                                  DSP-TOUYOK-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  MAIN-060.
       MAIN-080.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-020
               ELSE
                   MOVE  15    TO  I
                   MOVE  23    TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-060
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
               GO  TO  MAIN-080
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
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ÇvÇnÇqÇjÅ|ÇqÇsÇm    *
      **************************
       WORK-RTN.
           MOVE  HZM-ZAN     TO  W-ZMZ.
           MOVE  1           TO  I.
           MOVE  KETSAN      TO  J.
       WORK-010.
           IF  I NOT < 13
               GO  TO  WORK-020
           END-IF.
           MOVE  HZM-TJKR(J)     TO  W-TOUYOK-KARI(I).                  ìñä˙éÿï˚
           MOVE  HZM-TJKS(J)     TO  W-TOUYOK-KASHI(I).                 ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  WORK-010.
       WORK-020.
           IF  I NOT < 16
               GO  TO  WORK-EX
           END-IF.
           MOVE  HZM-TJKR(I)     TO  W-TOUYOK-KARI(I).                  óÇä˙éÿï˚
           MOVE  HZM-TJKS(I)     TO  W-TOUYOK-KASHI(I).                 óÇä˙ë›ï˚
           ADD  1     TO  I.
           GO  TO  WORK-020.
       WORK-EX.
           EXIT.
      ************************
      *    ÇcÇrÇoÅ|ÇqÇsÇm    *
      ************************
       DSP-RTN.
           CALL "SD_Output" USING "DSP-ZMZ" DSP-ZMZ "p"
                                  RETURNING RESU.
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       DSP-010.
           IF  I NOT < 13
               GO  TO  DSP-020
           END-IF.
           CALL "SD_Output" USING "DSP-TOUYOK-KARI" DSP-TOUYOK-KARI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOUYOK-KASHI"
                                  DSP-TOUYOK-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  DSP-010.
       DSP-020.
           IF  I NOT < 16
               GO  TO  DSP-EX
           END-IF.
           CALL "SD_Output" USING "DSP-TOUYOK-KARI" DSP-TOUYOK-KARI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOUYOK-KASHI"
                                  DSP-TOUYOK-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  DSP-020.
       DSP-EX.
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
           MOVE  SPACE     TO  HZM-R.
           INITIALIZE  HZM-R.
           MOVE  W-KACD     TO  HZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  W-HOCD     TO  HZM-HOCD.                               ï‚èïÇbÇc
           MOVE  W-ZMZ      TO  HZM-ZAN.                                ëOä˙ññéc
           MOVE  HZM-KEY    TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       WRITE-010.
           IF  I NOT < 13
               GO  TO  WRITE-020
           END-IF.
           MOVE  W-TOUYOK-KARI(I)     TO  HZM-TJKR(J).                  ìñä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  HZM-TJKS(J).                  ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  WRITE-010.
       WRITE-020.
           IF  I NOT < 16
               GO  TO  WRITE-030
           END-IF.
           MOVE  W-TOUYOK-KARI(I)     TO  HZM-TJKR(I).                  óÇä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  HZM-TJKS(I).                  óÇä˙ë›ï˚
           ADD  1     TO  I.
           GO  TO  WRITE-020.
       WRITE-030.
      *           WRITE  HZM-R  INVALID
      *///////////////
           CALL "DB_Insert" USING
            HZM-F_PNAME1 HZM-F_LNAME HZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-KACD     TO  HZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  W-HOCD     TO  HZM-HOCD.                               ï‚èïÇbÇc
           MOVE  W-ZMZ      TO  HZM-ZAN.                                ëOä˙ññéc
           MOVE  HZM-KEY    TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       REWRITE-010.
           IF  I NOT < 13
               GO  TO  REWRITE-020
           END-IF.
           MOVE  W-TOUYOK-KARI(I)     TO  HZM-TJKR(J).                  ìñä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  HZM-TJKS(J).                  ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  REWRITE-010.
       REWRITE-020.
           IF  I NOT < 16
               GO  TO  REWRITE-030
           END-IF.
           MOVE  W-TOUYOK-KARI(I)     TO  HZM-TJKR(I).                  óÇä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  HZM-TJKS(I).                  óÇä˙ë›ï˚
           ADD  1     TO  I.
           GO  TO  REWRITE-020.
       REWRITE-030.
      *           REWRITE  HZM-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            HZM-F_PNAME1 HZM-F_LNAME HZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-KACD     TO  HZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  W-HOCD     TO  HZM-HOCD.                               ï‚èïÇbÇc
           MOVE  HZM-KEY    TO  ERR-K.
      *           DELETE  HZM-F  INVALID
      *///////////////
           CALL "DB_Delete" USING HZM-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "HZM-F"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
