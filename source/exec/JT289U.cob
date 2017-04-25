       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT289U.
       AUTHOR.                        _________.
      ***************************************************
      *    PROGRAM        : ïiñºï éÛíçécíäèoÅ@Å@Å@Å@    *
      *    DATA WRITTEN   : 01/02/01                    *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : JIPS                        *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  JS-CHK                    PIC 9(01).
       77  JS-SIGN                   PIC 9(01).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  ACT-WORK.
           02  W-KBN                 PIC 9(01).
           02  W-FROM                PIC 9(06).
           02  W-TO                  PIC 9(06).
           02  W-SDAT.
             03  W-S20               PIC 9(02).
             03  W-SNGP              PIC 9(06).
           02  W-EDAT.
             03  W-E20               PIC 9(02).
             03  W-ENGP              PIC 9(06).
           02  W-TFR                 PIC 9(04).
           02  W-TTO                 PIC 9(04).
           02  W-OK                  PIC 9(01).
           02  W-SEN                 PIC 9(01).
           02  W-JMST                PIC X(222).
           02  W-TEKI                PIC N(32).
           02  W-TEK   REDEFINES W-TEKI.
             03  F                   PIC N(09).
             03  W-TEK1              PIC N(06).
             03  W-TEK2              PIC N(02).
             03  F                   PIC N(15).
           02  W-90                  PIC 9(01).
       COPY    LWMSG.
      *
           COPY  LJMST3.
           COPY  LTWK04.
      *
       77  ESTAT                    PIC  X(002).
       77  RESU                     PIC  9(001).
       77  RET                      PIC  9(001) VALUE ZERO.
       77  USER_ID                  PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE          PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER    PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-02.
           02  FILLER    PIC  X(01) VALUE " ".
           02  FILLER    PIC  X(06) VALUE "      ".
           02  FILLER    PIC  X(06) VALUE "      ".
           02  FILLER    PIC  X(06) VALUE "      ".
           02  FILLER    PIC  X(06) VALUE "      ".
           02  FILLER    PIC  X(04) VALUE "    ".
           02  FILLER    PIC  X(04) VALUE "    ".
           02  FILLER    PIC  X(01) VALUE " ".
           02  FILLER    PIC  X(01) VALUE " ".
       01  DSP-AREA1.
           02  DSP-00.
               03  FILLER    PIC  X(25) 
                               VALUE  "                         ".
               03  FILLER    PIC  X(02) VALUE  "ïi".
               03  FILLER    PIC  X(02) VALUE  "ñº".
               03  FILLER    PIC  X(02) VALUE  "ï ".
               03  FILLER    PIC  X(02) VALUE  "éÛ".
               03  FILLER    PIC  X(02) VALUE  "íç".
               03  FILLER    PIC  X(02) VALUE  "éc".
               03  FILLER    PIC  X(02) VALUE  "í†".
               03  FILLER    PIC  X(02) VALUE  "ëº".
           02  DSP-01.
               03  FILLER    PIC  X(25) 
                               VALUE  "                         ".
               03  FILLER    PIC  X(02) VALUE  "ïi".
               03  FILLER    PIC  X(02) VALUE  "ñº".
               03  FILLER    PIC  X(02) VALUE  "ï ".
               03  FILLER    PIC  X(02) VALUE  "éÛ".
               03  FILLER    PIC  X(02) VALUE  "íç".
               03  FILLER    PIC  X(02) VALUE  "êî".
               03  FILLER    PIC  X(02) VALUE  "í†".
               03  FILLER    PIC  X(02) VALUE  "ëº".
       01  DSP-AREA2.
           02  DSP-12.
               03  FILLER    PIC  X(38) VALUE
                   "éÛíç = 0 , óaÇË = 5 , éÊÇÊÇØ = 6 ...  ".
           02  DSP-02.
               03  FILLER    PIC  X(04) VALUE  "ïiñº".
               03  FILLER    PIC  X(04) VALUE  "∫∞ƒﬁ".
               03  FILLER    PIC  X(06) VALUE  "éÛíçì˙".
               03  FILLER    PIC  X(06) VALUE  "ìæà”êÊ".
               03  FILLER    PIC  X(04) VALUE  "∫∞ƒﬁ".
           02  DSP-03.
               03  FILLER    PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  DSP-04.
               03  FILLER    PIC  X(04) VALUE  "ÇsÇn".
           02  DSP-07.
               03  FILLER    PIC  X(06) VALUE  "ämîFÅi".
               03  FILLER    PIC  X(09) VALUE  "OK=1,NO=9".
               03  FILLER    PIC  X(02) VALUE  "Åj".
               03  FILLER    PIC  X(08) VALUE  "--> ÿ¿∞›".
           02  DSP-08  PIC  X(09) VALUE  "ÇO ã≥Å@àÁ".
           02  DSP-09  PIC  X(09) VALUE  "ÇP àÍÅ@î ".
           02  DSP-10  PIC  X(09) VALUE  "ÇX ëSÅ@åè".
           02  DSP-11  PIC  X(08) VALUE  "ëIë [ ]".
       01  DSP-AREA3.
           02  FILLER    PIC  X(08) VALUE  "ÅiîíéÜÅj".
       01  ACP-AREA.
           02  ACP-KBN     PIC 9(01).
           02  ACP-FROM    PIC 9(06).
           02  ACP-TO      PIC 9(06).
           02  ACP-SNGP    PIC 9(06).
           02  ACP-ENGP    PIC 9(06).
           02  ACP-TFR     PIC 9(04).
           02  ACP-TTO     PIC 9(04).
           02  ACP-OK      PIC 9(01).
           02  ACP-SEN     PIC 9(01).
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-02
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-02" "X" "6" "54" "1" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-02" "X" "11" "22" "6" "01CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-02" "X" "13" "22" "6" "02CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-02" "X" "11" "31" "6" "03CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-02" "X" "13" "31" "6" "04CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLE-02" "X" "11" "42" "4" "05CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07CLE-02" "X" "13" "42" "4" "06CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08CLE-02" "X" "17" "62" "1" "07CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09CLE-02" "X" "23" "61" "1" "08CLE-02" " " RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "82" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-00" " " "1" "0" "41" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-00" "RX" "1" "23" "25" " " "DSP-00" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-00" "X" "1" "24" "2" "01DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-00" "X" "1" "27" "2" "02DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-00" "X" "1" "30" "2" "03DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-00" "X" "1" "33" "2" "04DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-00" "X" "1" "36" "2" "05DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-00" "X" "1" "39" "2" "06DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-00" "X" "1" "42" "2" "07DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-00" "X" "1" "45" "2" "08DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "41" "DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "23" "25" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "24" "2" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-01" "X" "1" "27" "2" "02DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "X" "1" "30" "2" "03DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-01" "X" "1" "33" "2" "04DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-01" "X" "1" "36" "2" "05DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-01" "X" "1" "39" "2" "06DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-01" "X" "1" "42" "2" "07DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-01" "X" "1" "45" "2" "08DSP-01" " " RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING 
            "DSP-AREA2" " " "0" "0" "134" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" " " "6" "0" "38" " " "DSP-AREA2" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-12" "X" "6" "17" "38" " " "DSP-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "9" "0" "24" "DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "9" "22" "4" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-02" "X" "9" "26" "4" "01DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-02" "X" "9" "31" "6" "02DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-02" "X" "9" "38" "6" "03DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-02" "X" "9" "44" "4" "04DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "11" "0" "8" "DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "11" "12" "8" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "13" "0" "4" "DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "X" "13" "12" "4" " " "DSP-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "23" "0" "25" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "23" "41" "6" " " "DSP-07" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-07" "X" "23" "47" "9" "01DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-07" "X" "23" "56" "2" "02DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-07" "X" "23" "58" "8" "03DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" "X" "11" "51" "9" "DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" "X" "13" "51" "9" "DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" "X" "15" "51" "9" "DSP-09" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "X" "17" "56" "8" "DSP-10" " " RETURNING RESU.
      *DSP-AREA3
       CALL "SD_Init" USING 
            "DSP-AREA3" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA3" "X" "1" "49" "8" " " "DSP-AREA3"
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "35" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KBN" "9" "6" "54" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KBN" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FROM" "9" "11" "22" "6" "ACP-KBN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TO" "9" "13" "22" "6" "ACP-FROM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNGP" "9" "11" "31" "6" "ACP-TO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNGP" BY REFERENCE W-SNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENGP" "9" "13" "31" "6" "ACP-SNGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ENGP" BY REFERENCE W-ENGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TFR" "9" "11" "42" "4" "ACP-ENGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TFR" BY REFERENCE W-TFR "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TTO" "9" "13" "42" "4" "ACP-TFR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TTO" BY REFERENCE W-TTO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "61" "1" "ACP-TTO" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "17" "62" "1" "ACP-OK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
           IF  JS-CHK > 2
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO MR999
           END-IF
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO MR999
           END-IF.
       MR000.
           CALL "SD_Accept" USING BY REFERENCE ACP-KBN "ACP-KBN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT        =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  MR999
           END-IF
           IF  ESTAT   NOT  =  "01"   AND    "06"
               GO  TO  MR000
           END-IF
           IF  W-KBN   NOT  =  0  AND  5  AND  6
               GO  TO  MR000
           END-IF
           CALL "SD_Output" USING "ACP-KBN" ACP-KBN "p" RETURNING RESU.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM "ACP-FROM" 
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR000
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING
            "ACP-FROM" ACP-FROM "p" RETURNING RESU.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO "ACP-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR020
           END-IF
           CALL "SD_Output" USING "ACP-TO" ACP-TO "p" RETURNING RESU.
           IF  W-FROM   >  W-TO
               GO  TO  MR010
           END-IF.
       MR022.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNGP "ACP-SNGP" "9"
            "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR022
           END-IF
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
       MR024.
           CALL "SD_Accept" USING BY REFERENCE ACP-ENGP "ACP-ENGP" "9"
            "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR022
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR024
           END-IF
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
           IF  W-SNGP   >  W-ENGP
               GO  TO  MR024
           END-IF.
       MR026.
           CALL "SD_Accept" USING BY REFERENCE ACP-TFR "ACP-TFR" "9"
            "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR024
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR026
           END-IF
           CALL "SD_Output" USING "ACP-TFR" ACP-TFR "p" RETURNING RESU.
       MR028.
           CALL "SD_Accept" USING BY REFERENCE ACP-TTO "ACP-TTO" "9"
            "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR026
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR028
           END-IF
           CALL "SD_Output" USING "ACP-TTO" ACP-TTO "p" RETURNING RESU.
           IF  W-TFR    >  W-TTO
               GO  TO  MR028
           END-IF.
       MR035.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR028
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  MR035
           END-IF
           IF  W-SEN   NOT  = 0 AND 1 AND 9
               GO  TO  MR035
           END-IF.
       MR040.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR035
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR040
           END-IF
           IF  W-OK   NOT  =  "1"   AND      "9"
               GO  TO  MR040
           END-IF
           IF  W-OK        =  "9"
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               INITIALIZE   ACT-WORK
               GO  TO  MR000
           END-IF
      *
           MOVE    W-FROM      TO     JMST3-03.
           MOVE    W-TFR       TO     JMST3-04.
           MOVE    0           TO     JMST3-05.
           MOVE    0           TO     JMST3-06.
           MOVE    0           TO     JMST3-07.
           MOVE    0           TO     JMST3-08.
           MOVE    0           TO     JMST3-09.
      *           START    JMST3  KEY  NOT  <  JMST3-KEY    INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  MR999
           END-IF
           MOVE    20          TO     W-S20  W-E20.
       MR050.
      *           READ      JMST3 NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO   TO   MR999
           END-IF
           IF  W-TO              < JMST3-03
               GO   TO   MR999
           END-IF
           IF  JMST3-04     <  W-TFR   OR  >  W-TTO
               GO  TO  MR050
           END-IF
           IF  W-KBN        =  0
               IF  JMST3-01   NOT  =  0  AND  2
                   GO  TO  MR050
               END-IF
           END-IF
           IF  W-KBN        =  5
               IF  JMST3-01   NOT  =  5
                   GO  TO  MR050
               END-IF
           END-IF
           IF  W-KBN        =  6
               IF  JMST3-01   NOT  =  6
                   GO  TO  MR050
               END-IF
           END-IF
           IF  JMST3-02     <  W-SDAT  OR  >  W-EDAT
               GO  TO  MR050
           END-IF
           MOVE    JMST3-90    TO     W-90.
           IF  W-90         =  2
               MOVE    1           TO     W-90
           END-IF
           IF  W-SEN    NOT =  9
               IF  W-SEN      NOT  =  W-90
                   GO  TO  MR050
               END-IF
           END-IF.
       MR060.
           MOVE    SPACE       TO     WK04-R.
           INITIALIZE    WK04-R.
           MOVE    JMST3-R     TO     W-JMST.
           MOVE    W-JMST      TO     WK04-R.
           MOVE    W-KBN       TO     WK04-89.
           MOVE    W-SEN       TO     WK04-90.
           MOVE    JMST3-17    TO     WK04-17.
           MOVE    JMST3-91    TO     WK04-91.
           MOVE    SPACE       TO     W-TEKI.
           MOVE    JMST3-13    TO     W-TEKI.
           MOVE    W-TEK1      TO     WK04-801.
           MOVE    W-TEK2      TO     WK04-803.
           MOVE    JMST3-23    TO     WK04-23.
      *           WRITE   WK04-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK04_PNAME1 JT-WK04_LNAME WK04-R RETURNING RET.
           GO   TO   MR050.
       MR999.
           PERFORM   END-RTN   THRU   END-EX.
           CALL "DB_Close".
           STOP      RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           ACCEPT JS-CHK FROM ARGUMENT-VALUE.
           IF  JS-CHK > 2
               GO TO INT-RTN
           END-IF
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               GO TO INT-RTN
           END-IF
           INITIALIZE     ACT-WORK.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           IF    JS-CHK      NOT  =   2
                 CALL "SD_Output" USING
                "DSP-00" DSP-00 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-AREA2" DSP-AREA2 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "DSP-01" DSP-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-AREA2" DSP-AREA2 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
       COPY    LPMSG.
