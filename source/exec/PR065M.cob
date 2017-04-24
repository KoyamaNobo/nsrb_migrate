       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR065M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  Œo”ïƒ}ƒXƒ^ƒƒ“ƒeƒiƒ“ƒX        *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/21                      *
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
       77  W-OWARI                 PIC  X(05).
      ***  SPACE ‚¶‚á‚È‚©‚Á‚½‚çCSTOP RUN  ‚ÅI—¹B
       77  I                       PIC  9(02).                          “YŽš
      ***  ‰æ–Êã‚Ì“YŽš
       77  J                       PIC  9(02).                          “YŽš
      ***  HH-F‚É‘‚­Žž‚Ì“YŽš
       77  LIN                     PIC  9(02).
       77  W-SPACE                 PIC  N(10)  VALUE
                                   "@@@@@@@@@@".
       01  W-AREA.
      ***  •ÒW€–Ú
           02  W-TOU-Z9                PIC  Z9.                         “–Šú
           02  W-JIT-NC                PIC  N(02).
      ***  ƒRƒ“ƒgƒ[ƒ‹ƒtƒ@ƒCƒ‹‚©‚ç‚ÌŒˆŽZŒŽ‚ð‚½‚ß‚Æ‚­
           02  KETSAN              PIC  9(02).                          ‰ŒˆŽZŒŽ
           02  TOUYOK-MONTH        PIC  9(02).                          ŒˆŽZŒŽ
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-BUMON-CD      PIC  9(04).                      •”–å‚b‚c
                   04  W-KAMOKU-CD     PIC  9(04).                      ‰È–Ú‚b‚c
                   04  W-HOJO-CD       PIC  9(04).
                   04  W-BUMON-NM      PIC  N(10).
                   04  W-KAMOKU-NM     PIC  N(10).
                   04  W-HOJO-NM       PIC  N(10).
                   04  W-AREA3.
                       05  W-GOKEI-CD       PIC  9(03).                 ‡Œv‚b‚c
                       05  W-KAIGYOUSU      PIC  9(01).                 ‰üs”@
                       05  W-KAKU      PIC  X(01).                      Šm”F
                       05  W-AREA6.
                           06  W-AREA7     OCCURS 15.
                               07  W-JISEKI         PIC S9(11).
      ***
           COPY  LWMSG_PR.
      ***  Œo”ïƒ}ƒXƒ^
           COPY  KEIHI.
      ***  •”–å–¼ƒ}ƒXƒ^
           COPY  BUMONF.
      ***  Š¿Žš‰È–Úƒ}ƒXƒ^
           COPY  KANGEL.
      ***  ƒRƒ“ƒgƒ[ƒ‹ƒtƒ@ƒCƒ‹
           COPY  FCTL.
      **
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      ******************************
      *@@‰æ–ÊƒNƒŠƒA[€–Ú@@    *
      ******************************
       01  DSP-CLR.
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA1.
           02  FILLER  PIC X(001) VALUE " ".                            ACT
           02  CLR-AREA2.
               03  FILLER  PIC X(004) VALUE "    ".
               03  FILLER  PIC X(004) VALUE "    ".
               03  FILLER  PIC X(004) VALUE "    ".
               03  FILLER  PIC N(10).
               03  FILLER  PIC N(10).
               03  FILLER  PIC N(10).
               03  CLR-AREA3.
                   04  FILLER  PIC X(003) VALUE "   ".
                   04  FILLER  PIC X(001) VALUE " ".                    ‰üs”
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER  PIC X(001) VALUE " ".                    Šm”F
      ***************************
      *    ‰æ–Ê“ü—Í€–Ú         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT               PIC 9(01).                         ACT
           03  ACP-BUMON-CD          PIC 9(04).
           03  ACP-KAMOKU-CD         PIC 9(04).
           03  ACP-HOJO-CD           PIC 9(04).
           03  ACP-GOKEI-CD          PIC 9(03).
           03  ACP-KAIGYOUSU         PIC 9(01).                         ‰üs”@
           03  ACP-JISEKI            PIC S9(11).
           03  ACP-KAKU              PIC X(01).                         Šm”F
      *********************
      *    ‰æ–Ê•\Ž¦       *
      *********************
       01  DSP-DSP.
           03  DSP-JIT-TSUKI        PIC N(02).
           03  DSP-JISEKI           PIC ZZZZZZZZZZ9-.
           03  DSP-BUMON-NM         PIC N(10).
           03  DSP-KAMOKU-NM        PIC N(10).
           03  DSP-HOJO-NM          PIC N(10).
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
            "CLR-AREA1" " " "0" "0" "228" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "227" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "4" "34" "4" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "X" "5" "34" "4" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA2" "X" "6" "34" "4" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA2" "N" "4" "56" "20" "03CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA2" "N" "5" "56" "20" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "05CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA2" "N" "6" "56" "20" "05CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA3" " " "0" "0" "185" "06CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" "X" "7" "35" "3" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA4" "X" "7" "75" "1" "01CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA5" "X" "9" "25" "12" "02CLR-AREA4" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA6" "X" "10" "25" "12" "03CLR-AREA5" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA7" "X" "11" "25" "12" "04CLR-AREA6" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA8" "X" "12" "25" "12" "05CLR-AREA7" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-AREA9" "X" "13" "25" "12" "06CLR-AREA8" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-AREA10" "X" "14" "25" "12" "07CLR-AREA9" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-AREA11" "X" "15" "25" "12" "08CLR-AREA10" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-AREA12" "X" "16" "25" "12" "09CLR-AREA11" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11CLR-AREA13" "X" "17" "25" "12" "10CLR-AREA12" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12CLR-AREA14" "X" "18" "25" "12" "11CLR-AREA13" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "13CLR-AREA15" "X" "19" "25" "12" "12CLR-AREA14" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "14CLR-AREA16" "X" "20" "25" "12" "13CLR-AREA15" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15CLR-AREA17" "X" "21" "25" "12" "14CLR-AREA16" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "16CLR-AREA18" "X" "22" "25" "12" "15CLR-AREA17" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "17CLR-AREA19" "X" "23" "25" "12" "16CLR-AREA18" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "18CLR-AREA20" "X" "24" "77" "1" "17CLR-AREA19" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUMON-CD" "9" "4" "34" "4" "ACP-ACT" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUMON-CD" BY REFERENCE W-BUMON-CD "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAMOKU-CD" "9" "5" "34" "4" "ACP-BUMON-CD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAMOKU-CD" BY REFERENCE W-KAMOKU-CD "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-HOJO-CD" "9" "6" "34" "4" "ACP-KAMOKU-CD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-HOJO-CD" BY REFERENCE W-HOJO-CD "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-GOKEI-CD" "9" "7" "35" "3" "ACP-HOJO-CD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-GOKEI-CD" BY REFERENCE W-GOKEI-CD "3" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAIGYOUSU" "9" "7" "75" "1" "ACP-GOKEI-CD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAIGYOUSU" BY REFERENCE W-KAIGYOUSU "1" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-JISEKI" "S9" "LIN" "25" "11" "ACP-KAIGYOUSU" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-JISEKI" BY REFERENCE W-JISEKI(1) "11" "1"
            BY REFERENCE I 11 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-JISEKI" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *DSP-DSP
       CALL "SD_Init" USING
            "DSP-DSP" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-JIT-TSUKI" "N" "LIN" "18" "4" " " "DSP-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-JIT-TSUKI" BY REFERENCE W-JIT-NC "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-JISEKI" "ZZZZZZZZZZ9-" "LIN" "25" "12" "DSP-JIT-TSUKI"
            " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-JISEKI" BY REFERENCE W-JISEKI(1) "12" "1"
            BY REFERENCE I 11 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-BUMON-NM" "N" "4" "56" "20" "DSP-JISEKI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-BUMON-NM" BY REFERENCE W-BUMON-NM "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KAMOKU-NM" "N" "5" "56" "20" "DSP-BUMON-NM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KAMOKU-NM" BY REFERENCE W-KAMOKU-NM "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HOJO-NM" "N" "6" "56" "20" "DSP-KAMOKU-NM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-HOJO-NM" BY REFERENCE W-HOJO-NM "20" "0"
            RETURNING RESU.
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
      *    ‰Šúˆ—            *
      **************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0650" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HH-F_PNAME1 "SHARED" BY REFERENCE HH-F_IDLST "1"
            "HH-KEY" BY REFERENCE HH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE  "DATE  "     TO  FCTL-KEY.
      ***  ƒRƒ“ƒgƒ[ƒ‹ƒtƒ@ƒCƒ‹@‚q‚d‚`‚c
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
      ***  •Û‘¶‚µ‚Ä‚¨‚­B
           MOVE  KETSAN      TO  TOUYOK-MONTH.
      ***  ‘«‚µ‚Ä‚¢‚­B
           MOVE  1     TO  I.
           MOVE  09    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       INI-010.
           IF  I >  15
               GO  TO  INI-EX
           END-IF.
           MOVE  TOUYOK-MONTH     TO  W-TOU-Z9.
           MOVE  W-TOU-Z9         TO  W-JIT-NC.
           CALL "SD_Output" USING "DSP-JIT-TSUKI"
                                  DSP-JIT-TSUKI "p"
                                  RETURNING RESU
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           ADD  1     TO  TOUYOK-MONTH.
           IF  TOUYOK-MONTH = 13
               MOVE  1     TO TOUYOK-MONTH
           END-IF.
           GO  TO  INI-010.
       INI-EX.
           EXIT.
      *****************************
      *    ‚l‚`‚h‚m@ˆ—@@@@ *
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
                 BY REFERENCE ACP-BUMON-CD "ACP-BUMON-CD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           MOVE  W-BUMON-CD     TO     BNM-KEY.
      *           READ  BNM            UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                RETURNING RESU
               GO  TO  MAIN-010
           END-IF.
           MOVE  BNMNMN         TO     W-BUMON-NM.
           CALL "SD_Output" USING "DSP-BUMON-NM" DSP-BUMON-NM "p"
                                  RETURNING RESU.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAMOKU-CD "ACP-KAMOKU-CD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
           MOVE  W-KAMOKU-CD    TO     K-ACCD.
           MOVE  ZERO           TO     K-HOCD.
      *           READ  KNG            UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                RETURNING RESU
               GO  TO  MAIN-020
           END-IF.
           MOVE  KNGNMN         TO     W-KAMOKU-NM.
           CALL "SD_Output" USING "DSP-KAMOKU-NM" DSP-KAMOKU-NM "p"
                                  RETURNING RESU.
       MAIN-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-HOJO-CD "ACP-HOJO-CD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-030
           END-IF.
           IF  W-HOJO-CD = ZERO
               MOVE SPACE     TO KNGNMN
               GO TO MAIN-031
           END-IF.
           MOVE  W-KAMOKU-CD    TO     K-ACCD.
           MOVE  W-HOJO-CD      TO     K-HOCD.
      *           READ  KNG            UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                RETURNING RESU
               GO  TO  MAIN-030
           END-IF.
       MAIN-031.
           MOVE  KNGNMN         TO     W-HOJO-NM.
           CALL "SD_Output" USING "DSP-HOJO-NM" DSP-HOJO-NM "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
           MOVE  W-BUMON-CD      TO  HH-BUCD.
           MOVE  W-KAMOKU-CD     TO  HH-KACD.
           MOVE  W-HOJO-CD       TO  HH-HOCD.
      ***  Œo”ïƒ}ƒXƒ^@‚q‚d‚`‚c
      *           READ  HH-F  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HH-F_PNAME1 BY REFERENCE HH-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-040
           END-IF.
           GO  TO  MAIN-050.
       MAIN-040.
           IF  W-ACT = 2 OR 3
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
      ***  ƒ}ƒXƒ^–¢“o˜^
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               GO  TO  MAIN-060
           END-IF.
       MAIN-050.
           IF  W-ACT = 1
               CALL "SD_Output" USING "NOR-M01" NOR-M01 "p"
                                  RETURNING RESU
      ***  ƒ}ƒXƒ^“o˜^Ï‚Ý
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           ELSE
               PERFORM  WORK-RTN     THRU  WORK-EX
               PERFORM  DSP-RTN      THRU  DSP-EX
               IF  W-ACT = 3
                   GO  TO  MAIN-110
               END-IF
           END-IF.
       MAIN-060.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-GOKEI-CD "ACP-GOKEI-CD" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-030
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-060
           END-IF.
       MAIN-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAIGYOUSU "ACP-KAIGYOUSU" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-060
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-070
           END-IF.
           MOVE  1     TO  I.
           MOVE  09    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       MAIN-100.
           IF  I > 15
               GO  TO  MAIN-110
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-JISEKI "ACP-JISEKI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   MOVE  15     TO  I
                   MOVE  23     TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-070
               ELSE
                   SUBTRACT 1     FROM  I  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-100
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-100
           END-IF.
           CALL "SD_Output" USING "DSP-JISEKI" DSP-JISEKI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  MAIN-100.
       MAIN-110.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-030
               ELSE
                   MOVE  15    TO  I
                   MOVE  23    TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-100
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
               GO  TO  MAIN-110
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
      *    I—¹ˆ—          *
      ************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ‚v‚n‚q‚j|‚q‚s‚m    *
      **************************
       WORK-RTN.
           MOVE  HH-GOCD     TO  W-GOKEI-CD.
           MOVE  HH-GYO      TO  W-KAIGYOUSU.
           MOVE  1           TO  I.
           MOVE  KETSAN      TO  J.
       WORK-010.
           IF  I > 15
               GO  TO  WORK-EX
           END-IF.
           MOVE  HH-GEL(J)        TO  W-JISEKI(I).
           ADD  1     TO  I  J.
           IF  I = 13
               MOVE  13    TO  J
           END-IF.
           IF  (J = 13) AND (I < 13)
               MOVE  1     TO  J
           END-IF.
           GO  TO  WORK-010.
       WORK-EX.
           EXIT.
      ************************
      *    ‚c‚r‚o|‚q‚s‚m    *
      ************************
       DSP-RTN.
           CALL "SD_Output" USING "ACP-GOKEI-CD" ACP-GOKEI-CD "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-KAIGYOUSU" ACP-KAIGYOUSU "p"
                                  RETURNING RESU.
           MOVE  1     TO  I.
           MOVE  09    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       DSP-010.
           IF  I > 15
               GO  TO  DSP-EX
           END-IF.
           CALL "SD_Output" USING "DSP-JISEKI" DSP-JISEKI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  DSP-010.
       DSP-EX.
           EXIT.
      **************************
      *    ‚j‚n‚t|‚q‚s‚m      *
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
      *    ‚v‚q‚h‚s‚d|‚q‚s‚m      *
      ******************************
       WRITE-RTN.
           MOVE  SPACE     TO  HH-R.
           INITIALIZE  HH-R.
           MOVE  W-BUMON-CD     TO  HH-BUCD.
           MOVE  W-KAMOKU-CD    TO  HH-KACD.
           MOVE  W-HOJO-CD      TO  HH-HOCD.
           MOVE  W-GOKEI-CD     TO  HH-GOCD.
           MOVE  W-KAIGYOUSU    TO  HH-GYO.
           MOVE  HH-KEY         TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       WRITE-010.
           IF  I > 15
               GO  TO  WRITE-020
           END-IF.
           MOVE  W-JISEKI(I)        TO  HH-GEL(J).
           ADD  1     TO  I  J.
           IF  I = 13
               MOVE  13    TO  J
           END-IF.
           IF  (J = 13) AND (I < 13)
               MOVE  1     TO  J
           END-IF.
           GO  TO  WRITE-010.
       WRITE-020.
      *           WRITE  HH-R  INVALID
      *///////////////
           CALL "DB_Insert" USING
            HH-F_PNAME1 HH-F_LNAME HH-R RETURNING RET.
           IF  RET = 1
               MOVE  "HH-F"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ‚q‚d‚v‚q‚h‚s‚d|‚q‚s‚m      *
      **********************************
       REWRITE-RTN.
           MOVE  W-BUMON-CD     TO  HH-BUCD.
           MOVE  W-KAMOKU-CD    TO  HH-KACD.
           MOVE  W-HOJO-CD      TO  HH-HOCD.
           MOVE  W-GOKEI-CD     TO  HH-GOCD.
           MOVE  W-KAIGYOUSU    TO  HH-GYO.
           MOVE  HH-KEY         TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       REWRITE-010.
           IF  I > 15
               GO  TO  REWRITE-020
           END-IF.
           MOVE  W-JISEKI(I)        TO  HH-GEL(J).
           ADD  1     TO  I  J.
           IF  I = 13
               MOVE  13    TO  J
           END-IF.
           IF  (J = 13) AND (I < 13)
               MOVE  1     TO  J
           END-IF.
           GO  TO  REWRITE-010.
       REWRITE-020.
      *           REWRITE  HH-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            HH-F_PNAME1 HH-F_LNAME HH-R RETURNING RET.
           IF  RET = 1
               MOVE  "HH-F"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ‚c‚d‚k‚d‚s‚d|‚q‚s‚m     *
      *******************************
       DELETE-RTN.
           MOVE  W-BUMON-CD     TO  HH-BUCD.
           MOVE  W-KAMOKU-CD    TO  HH-KACD.
           MOVE  W-HOJO-CD      TO  HH-HOCD.
           MOVE  HH-KEY         TO  ERR-K.
      *           DELETE  HH-F  INVALID
      *///////////////
           CALL "DB_Delete" USING HH-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "HH-F"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
