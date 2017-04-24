       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT360I.
       AUTHOR.                        E-SHIGIHARA.
      ***************************************************
      *    PROGRAM        : éÛíçécè¡çûÇ›Å@Å@Å@Å@Å@Å@Å@Å@*
      *    DATA WRITTEN   : 87/09/03                    *
      *    SCREEN USED    : SJ360I                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       01  ACT-WORK.
           02  W-JNO                 PIC 9(06).
           02  W-OK                  PIC 9(01).
           02  OLD-GYO               PIC 9(01).
           02  W-01                  PIC N(02).
           02  W-02                  PIC N(26).
           02  W-03                  PIC N(26).
           02  W-04                  PIC N(24).
           02  W-05                  PIC S9(06).
           02  W-06                  PIC S9(06).
           02  CNT                   PIC 9(01)   VALUE   0.
           02  I                     PIC 9(02)   VALUE   0.
           02  W-ZAN                 PIC S9(06)  OCCURS  10.
           02  W-TU                  PIC S9(07).
           02  W-AZU                 PIC S9(07).
           02  ZAN                   PIC S9(06).
           02  T-ZAN                 PIC S9(06).
       01  WORK-AREA.
           02  L                     PIC 9(02)   VALUE   0.
       COPY    LWMSG.
      *
           COPY  LIHIM2.
           COPY  LITCM.
           COPY  LJMSTD.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-AREA.
           02  CLE-02.
               03  FILLER  PIC  X(48) VALUE
                  "                                                ".
               03  FILLER  PIC  X(04) VALUE   "    ".
               03  FILLER  PIC  X(48) VALUE
                  "                                                ".
               03  FILLER  PIC  X(08) VALUE "        ".
               03  FILLER  PIC  X(01) VALUE " ".
           02  CLE-03  PIC  X(11) VALUE "CLEAR  DATA".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(22) VALUE "éÛÅ@íçÅ@écÅ@è¡Å@çûÅ@Ç›".
           02  DSP-02.
               03  FILLER  PIC  X(04) VALUE "éÛíç".
               03  FILLER  PIC  X(02) VALUE "NO".
               03  FILLER  PIC  X(04) VALUE "ãÊï™".
               03  FILLER  PIC  X(08) VALUE "ìæà”êÊñº".
           02  DSP-03.
               03  FILLER  PIC  X(08) VALUE "íºëóêÊñº".
           02  DSP-04.
               03  FILLER  PIC  X(16) VALUE "ïiÅ@Å@Å@Å@Å@Å@ñº".
               03  FILLER  PIC  X(08) VALUE "éÛíçécêî".
           02  DSP-05.
               03  FILLER  PIC  X(01) VALUE   "1".
           02  DSP-06.
               03  FILLER  PIC  X(01) VALUE   "2".
           02  DSP-07.
               03  FILLER  PIC  X(01) VALUE   "3".
           02  DSP-08.
               03  FILLER  PIC  X(01) VALUE   "4".
           02  DSP-09.
               03  FILLER  PIC  X(01) VALUE   "5".
           02  DSP-10.
               03  FILLER  PIC  X(01) VALUE   "6".
           02  DSP-11.
               03  FILLER  PIC  X(04) VALUE "çáåv".
           02  DSP-12.
               03  FILLER  PIC  X(06) VALUE "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE "Åj".
               03  FILLER  PIC  X(04) VALUE "--->".
               03  FILLER  PIC  X(04) VALUE "ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-JNO     PIC 9(06).
           02  ACP-01      PIC N(02).
           02  ACP-02      PIC N(26).
           02  ACP-03      PIC N(26).
           02  ACP-04      PIC N(24).
           02  ACP-05      PIC ----,--9 .
           02  ACP-06      PIC ----,--9 .
           02  ACP-OK      PIC 9(01).
           02  ACP-INV     PIC 9(06).
       01  DSP-ERR.
           02  INV-01      PIC  X(20) VALUE
             "éÛíçÉ}ÉXÉ^Å[Å@ñ¢ìoò^".
           02  INV-03      PIC  X(24) VALUE
             "óaÇËãÊï™ÉGÉâÅ[Å@è¡çûïsâ¬".
           02  INV-04      PIC  X(20) VALUE
             "èoâ◊èàóùíÜÅ@è¡çûïsâ¬".
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "120" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "109" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-02" "X" "3" "23" "48" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-02" "X" "4" "9" "4" "01CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-02" "X" "4" "23" "48" "02CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-02" "X" "20" "54" "8" "03CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-02" "X" "23" "62" "1" "04CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "X" "L" "0" "11" "CLE-02" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "22" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "X" "1" "21" "22" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "3" "0" "18" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "3" "2" "4" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-02" "X" "3" "6" "2" "01DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-02" "X" "3" "9" "4" "02DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-02" "X" "3" "14" "8" "03DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "4" "0" "8" "DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "4" "14" "8" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "6" "0" "24" "DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "X" "6" "10" "16" " " "DSP-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-04" "X" "6" "54" "8" "01DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" " " "8" "0" "1" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-05" "X" "8" "2" "1" " " "DSP-05" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06" " " "10" "0" "1" "DSP-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06" "X" "10" "2" "1" " " "DSP-06" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "12" "0" "1" "DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "12" "2" "1" " " "DSP-07" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" " " "14" "0" "1" "DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-08" "X" "14" "2" "1" " " "DSP-08" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" " " "16" "0" "1" "DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-09" "X" "16" "2" "1" " " "DSP-09" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" " " "18" "0" "1" "DSP-09" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-10" "X" "18" "2" "1" " " "DSP-10" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" " " "20" "0" "4" "DSP-10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-11" "X" "20" "48" "4" " " "DSP-11" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" " " "23" "0" "25" "DSP-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-12" "X" "23" "41" "6" " " "DSP-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-12" "X" "23" "47" "9" "01DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-12" "X" "23" "56" "2" "02DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-12" "X" "23" "58" "4" "03DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-12" "X" "23" "63" "4" "04DSP-12" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "185" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JNO" "9" "4" "2" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JNO" BY REFERENCE W-JNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-01" "N" "4" "9" "4" "ACP-JNO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-01" BY REFERENCE W-01 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-02" "N" "3" "23" "52" "ACP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-02" BY REFERENCE W-02 "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-03" "N" "4" "23" "52" "ACP-02" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-03" BY REFERENCE W-03 "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-04" "N" "L" "4" "48" "ACP-03" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-04" BY REFERENCE W-04 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-05" "----,--9" "L" "54" "8" "ACP-04" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-05" BY REFERENCE W-05 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-06" "----,--9" "20" "54" "8" "ACP-05" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-06" BY REFERENCE W-06 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "62" "1" "ACP-06" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-INV" "9" "24" "35" "6" "ACP-OK" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-INV" BY REFERENCE JMSTD-05 "6" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-01" "X" "24" "1" "20" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-03" "X" "24" "1" "24" "INV-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-04" "X" "24" "1" "20" "INV-03" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-JNO "ACP-JNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING "ACP-JNO" ACP-JNO "p" RETURNING RESU.
           MOVE    W-JNO    TO  JMSTD-07.
           MOVE    ZERO     TO  JMSTD-08.
           MOVE    ZERO     TO  JMSTD-09.
      *           START   JMSTD     KEY  NOT  <  JMSTD-KEY1  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-01" INV-01 "p" RETURNING RESU
               GO  TO  MR010
           END-IF.
       MR020.
      *           READ    JMSTD     NEXT      AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               COMPUTE    L  =  6 + (OLD-GYO * 2)
               CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU
               CALL "SD_Output" USING "ACP-04" ACP-04 "p" RETURNING RESU
               CALL "SD_Output" USING "ACP-05" ACP-05 "p" RETURNING RESU
               GO  TO  MR060
           END-IF
           IF  W-JNO    NOT     =  JMSTD-07
               IF  CNT          =      0
                   CALL "SD_Output" USING
                    "INV-D01" INV-D01 "p" RETURNING RESU
                   GO  TO  MR010
               ELSE
                   COMPUTE    L  =  6 + (OLD-GYO * 2)
                   CALL "SD_Arg_Match_Line" USING
                    "L" "2" L RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-04" ACP-04 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "ACP-05" ACP-05 "p" RETURNING RESU
                   GO  TO  MR060
               END-IF
           END-IF
           IF  CNT      =    0
               MOVE    JMSTD-08    TO    OLD-GYO
               GO  TO  MR030
           END-IF
           IF  JMSTD-08     =     OLD-GYO
               GO  TO  MR050
           END-IF
           COMPUTE    L  =  6 + (OLD-GYO * 2)
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           CALL "SD_Output" USING "ACP-04" ACP-04 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-05" ACP-05 "p" RETURNING RESU.
           MOVE     0      TO    W-05.
           MOVE  JMSTD-08   TO    OLD-GYO.
           GO  TO  MR040.
       MR030.
           IF  JMSTD-01  =  1  OR  2
               CALL "SD_Output" USING "INV-03" INV-03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  MR010
           END-IF
           IF  JMSTD-01  =  0  OR  5  OR  6
               IF  JMSTD-151(1) = ZERO  AND  JMSTD-151(2) = ZERO  AND
                   JMSTD-151(3) = ZERO  AND  JMSTD-151(4) = ZERO  AND
                   JMSTD-151(5) = ZERO  AND  JMSTD-151(6) = ZERO  AND
                   JMSTD-151(7) = ZERO  AND  JMSTD-151(8) = ZERO  AND
                   JMSTD-151(9) = ZERO  AND  JMSTD-151(10) = ZERO
                   GO  TO  MR-040
               ELSE
                   CALL "SD_Output" USING
                    "INV-04" INV-04 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO  TO  MR010
               END-IF
           END-IF.
       MR-040.
           IF  JMSTD-01  = "0"
               MOVE    "Å@Å@"     TO     W-01
           END-IF
           IF  JMSTD-01  = "5"
               MOVE    "óaÇË"     TO     W-01
           END-IF
           IF  JMSTD-01  = "6"
               MOVE    "éÊÇË"     TO     W-01
           END-IF
           CALL "SD_Output" USING "ACP-01" ACP-01 "p" RETURNING RESU.
           MOVE    JMSTD-04    TO   TC-TCD.
           MOVE    "001"       TO   TC-CCD.
      *           READ    TC-M   UNLOCK         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  TC-NAME
           END-IF
           MOVE    TC-NAME    TO    W-02.
           CALL "SD_Output" USING "ACP-02" ACP-02 "p" RETURNING RESU.
           MOVE    JMSTD-04    TO   TC-TCD.
           MOVE    JMSTD-10    TO   TC-CCD.
      *           READ    TC-M   UNLOCK         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  TC-NAME
           END-IF
           MOVE    TC-NAME    TO    W-03.
           CALL "SD_Output" USING "ACP-03" ACP-03 "p" RETURNING RESU.
           MOVE         1      TO    CNT.
       MR040.
           MOVE    JMSTD-05    TO    HI-MHCD HI-HCD.
      *           READ    HI2-M  UNLOCK         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO   HI-NAME
           END-IF
           MOVE    HI-NAME    TO    W-04.
       MR050.
           MOVE    0          TO    I.
       MR051.
           ADD     1          TO    I.
           IF  I          >    10
               GO  TO  MR052
           END-IF
           COMPUTE ZAN = JMSTD-1111(I) - JMSTD-1211(I) - JMSTD-141(I).
           ADD     ZAN        TO    T-ZAN.
           MOVE    0          TO    ZAN.
           GO  TO  MR051.
       MR052.
           ADD     T-ZAN      TO    W-05.
           ADD     T-ZAN      TO    W-06.
           MOVE    0          TO    T-ZAN.
           GO  TO  MR020.
       MR060.
           CALL "SD_Output" USING "ACP-06" ACP-06 "p" RETURNING RESU.
       MR070.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               PERFORM    CLR-RTN   THRU     CLR-EX
               INITIALIZE   ACT-WORK
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR070
           END-IF
           IF  W-OK   NOT  =  "1"   AND      "9"
               GO  TO  MR070
           END-IF
           IF  W-OK        =  "9"
               PERFORM    CLR-RTN   THRU     CLR-EX
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               INITIALIZE   ACT-WORK
               GO  TO  MR010
           END-IF
           PERFORM     DEL-RTN     THRU      DEL-EX.
           PERFORM     CLR-RTN     THRU      CLR-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           INITIALIZE   ACT-WORK.
           GO  TO  MR010.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           INITIALIZE     ACT-WORK.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ360I" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "EXCLUSIVE" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇm                         *
      *********************************************
       CLR-RTN.
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
           MOVE      6    TO     L.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
       CLR-010.
           ADD       2    TO     L.
           CALL "SD_Arg_Match_Line" USING "L" "2" L RETURNING RESU.
           IF  L     >     18
               CALL "SD_Output" USING
                "DSP-AREA" DSP-AREA "p" RETURNING RESU
               GO  TO  CLR-EX
           END-IF
           CALL "SD_Output" USING "CLE-03" CLE-03 "p" RETURNING RESU.
           GO  TO  CLR-010.
       CLR-EX.
           EXIT.
      *********************************************
      *    ÇcÇdÇkÅ|ÇqÇsÇm                         *
      *********************************************
       DEL-RTN.
           MOVE   W-JNO    TO      JMSTD-07.
           MOVE   ZERO     TO      JMSTD-08.
           MOVE   ZERO     TO      JMSTD-09.
      *           START   JMSTD     KEY  NOT  <  JMSTD-KEY1  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               GO  TO  DEL-EX
           END-IF.
       DEL-010.
      *           READ    JMSTD     NEXT   AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  DEL-EX
           END-IF
           IF  W-JNO    NOT     =  JMSTD-07
               GO  TO  DEL-EX
           END-IF
      *           DELETE      JMSTD        INVALID
      *///////////////
           CALL "DB_Delete" USING JMSTD_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "JMSTD"   TO   ERR-F
               MOVE    "D"       TO   ERR-M
               MOVE   JMSTD-KEY1 TO   ERR-K
               PERFORM   ERR-RTN  THRU   ERR-EX
           END-IF
           GO  TO  DEL-010.
       DEL-EX.
           EXIT.
       COPY LPMSG.
