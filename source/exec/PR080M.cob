       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR080M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  ïîécÉ}ÉXÉ^ÉÅÉìÉeÉiÉìÉX        *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/27                      *
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
       77  I-Y                     PIC  9(01).
      ***  BZM-FÇ…èëÇ≠éûÇÃìYéö
       77  LIN                     PIC  9(02).
       01  W-AREA.
           02  W-ZEN-Z9                PIC  Z9.
           02  W-TOU-Z9                PIC  Z9.
           02  W-YOK-Z9                PIC  Z9.
           02  W-ZEN-X                 PIC  X(02).
           02  W-TOU-X                 PIC  X(02).
           02  W-YOK-X                 PIC  X(02).
           02  KETSAN              PIC  9(02).                          èâåàéZåé
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉãÇ©ÇÁÇÃåàéZåéÇÇΩÇﬂÇ∆Ç≠
           02  ZENTOUYOK-MONTH     PIC  9(02).                          åàéZåé
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-BUCD      PIC  9(04).                          ïîñÂÇbÇc
                   04  W-BUMEI     PIC  N(10).                          ïîñÂñºÅ@
                   04  W-KACD      PIC  9(04).                          â»ñ⁄ÇbÇc
                   04  W-KAMEI     PIC  N(10).                          â»ñ⁄ñºÅ@
                   04  W-AREA3.
                       05  W-AREA4     OCCURS 12.
                           07  W-ZEN-KARI       PIC S9(11).             ëOä˙éÿï˚
                           07  W-ZEN-KASHI      PIC S9(11).             ëOä˙ë›ï˚
                           07  W-TOU-KARI       PIC S9(11).             ìñä˙éÿï˚
                           07  W-TOU-KASHI      PIC S9(11).             ìñä˙ë›ï˚
                       05  W-AREA5     OCCURS 3.
                           07  W-YOK-KARI       PIC S9(11).             óÇä˙éÿï˚
                           07  W-YOK-KASHI      PIC S9(11).             óÇä˙ë›ï˚
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
           COPY  LWMSG_PR.
      ***  ïîécÉ}ÉXÉ^
           COPY  LBUZAN.
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉã
           COPY  FCTL.
      ***  ïîñÂñºÉ}ÉXÉ^
           COPY  BUMONF.
      ***  äøéöâ»ñ⁄É}ÉXÉ^
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
               03  FILLER  PIC X(004) VALUE "    ".
               03  FILLER  PIC N(10).
               03  FILLER  PIC X(004) VALUE "    ".
               03  FILLER  PIC N(10).
               03  CLR-AREA3.
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER.
                       05  FILLER  PIC X(012) VALUE "            ".
                       05  FILLER  PIC X(012) VALUE "            ".
                   04  FILLER      PIC X(001) VALUE " ".                ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT                PIC 9(01).                        ACT
           03  ACP-BUCD               PIC 9(04).
           03  ACP-KACD               PIC 9(04).
           03  ACP-ZEN-KARI           PIC S9(11).
           03  ACP-ZEN-KASHI          PIC S9(11).
           03  ACP-TOU-KARI           PIC S9(11).
           03  ACP-TOU-KASHI          PIC S9(11).
           03  ACP-YOK-KARI           PIC S9(11).
           03  ACP-YOK-KASHI          PIC S9(11).
           03  ACP-KAKU               PIC X(01).                        ämîF
      *********************
      *    âÊñ ï\é¶       *
      *********************
       01  DSP-DSP.
           03  DSP-ZEN-TSUKI          PIC N(02).
           03  DSP-TOU-TSUKI          PIC N(02).
           03  DSP-YOK-TSUKI          PIC N(02).
           03  DSP-BUMEI              PIC N(10).
           03  DSP-KAMEI              PIC N(10).
           03  DSP-ZEN-KARI           PIC ZZZZZZZZZZ9-.
           03  DSP-ZEN-KASHI          PIC ZZZZZZZZZZ9-.
           03  DSP-TOU-KARI           PIC ZZZZZZZZZZ9-.
           03  DSP-TOU-KASHI          PIC ZZZZZZZZZZ9-.
           03  DSP-YOK-KARI           PIC ZZZZZZZZZZ9-.
           03  DSP-YOK-KASHI          PIC ZZZZZZZZZZ9-.
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
            "CLR-AREA1" " " "0" "0" "698" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "66" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "697" "01CLR-AREA1" " "
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
            "CLR-AREA3" " " "0" "0" "649" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" " " "9" "0" "48" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA3" "X" "9" "14" "12" " " "01CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA3" "X" "9" "27" "12" "02CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA3" "X" "9" "54" "12" "03CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA3" "X" "9" "67" "12" "04CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA3" " " "10" "0" "48" "01CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-AREA3" "X" "10" "14" "12" " " "06CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-AREA3" "X" "10" "27" "12" "07CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-AREA3" "X" "10" "54" "12" "08CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-AREA3" "X" "10" "67" "12" "09CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11CLR-AREA3" " " "11" "0" "48" "06CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12CLR-AREA3" "X" "11" "14" "12" " " "11CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "13CLR-AREA3" "X" "11" "27" "12" "12CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "14CLR-AREA3" "X" "11" "54" "12" "13CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15CLR-AREA3" "X" "11" "67" "12" "14CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "16CLR-AREA3" " " "12" "0" "48" "11CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "17CLR-AREA3" "X" "12" "14" "12" " " "16CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "18CLR-AREA3" "X" "12" "27" "12" "17CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "19CLR-AREA3" "X" "12" "54" "12" "18CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "20CLR-AREA3" "X" "12" "67" "12" "19CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "21CLR-AREA3" " " "13" "0" "48" "16CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "22CLR-AREA3" "X" "13" "14" "12" " " "21CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "23CLR-AREA3" "X" "13" "27" "12" "22CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "24CLR-AREA3" "X" "13" "54" "12" "23CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "25CLR-AREA3" "X" "13" "67" "12" "24CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "26CLR-AREA3" " " "14" "0" "48" "21CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "27CLR-AREA3" "X" "14" "14" "12" " " "26CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "28CLR-AREA3" "X" "14" "27" "12" "27CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "29CLR-AREA3" "X" "14" "54" "12" "28CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "30CLR-AREA3" "X" "14" "67" "12" "29CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "31CLR-AREA3" " " "15" "0" "48" "26CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "32CLR-AREA3" "X" "15" "14" "12" " " "31CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "33CLR-AREA3" "X" "15" "27" "12" "32CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "34CLR-AREA3" "X" "15" "54" "12" "33CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "35CLR-AREA3" "X" "15" "67" "12" "34CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "36CLR-AREA3" " " "16" "0" "48" "31CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "37CLR-AREA3" "X" "16" "14" "12" " " "36CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "38CLR-AREA3" "X" "16" "27" "12" "37CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "39CLR-AREA3" "X" "16" "54" "12" "38CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "40CLR-AREA3" "X" "16" "67" "12" "39CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "41CLR-AREA3" " " "17" "0" "48" "36CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "42CLR-AREA3" "X" "17" "14" "12" " " "41CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "43CLR-AREA3" "X" "17" "27" "12" "42CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "44CLR-AREA3" "X" "17" "54" "12" "43CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "45CLR-AREA3" "X" "17" "67" "12" "44CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "46CLR-AREA3" " " "18" "0" "48" "41CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "47CLR-AREA3" "X" "18" "14" "12" " " "46CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "48CLR-AREA3" "X" "18" "27" "12" "47CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "49CLR-AREA3" "X" "18" "54" "12" "48CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "50CLR-AREA3" "X" "18" "67" "12" "49CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "51CLR-AREA3" " " "19" "0" "48" "46CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "52CLR-AREA3" "X" "19" "14" "12" " " "51CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "53CLR-AREA3" "X" "19" "27" "12" "52CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "54CLR-AREA3" "X" "19" "54" "12" "53CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "55CLR-AREA3" "X" "19" "67" "12" "54CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "56CLR-AREA3" " " "20" "0" "48" "51CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "57CLR-AREA3" "X" "20" "14" "12" " " "56CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "58CLR-AREA3" "X" "20" "27" "12" "57CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "59CLR-AREA3" "X" "20" "54" "12" "58CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "60CLR-AREA3" "X" "20" "67" "12" "59CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "61CLR-AREA3" " " "21" "0" "48" "56CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "62CLR-AREA3" "X" "21" "14" "12" " " "61CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "63CLR-AREA3" "X" "21" "27" "12" "62CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "64CLR-AREA3" " " "22" "0" "48" "61CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "65CLR-AREA3" "X" "22" "14" "12" " " "64CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "66CLR-AREA3" "X" "22" "27" "12" "65CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "67CLR-AREA3" " " "23" "0" "48" "64CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "68CLR-AREA3" "X" "23" "14" "12" " " "67CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "69CLR-AREA3" "X" "23" "27" "12" "68CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "70CLR-AREA3" "X" "24" "77" "1" "67CLR-AREA3" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "66" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUCD" "9" "5" "34" "4" "ACP-ACT" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUCD" BY REFERENCE W-BUCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD" "9" "6" "34" "4" "ACP-BUCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD" BY REFERENCE W-KACD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZEN-KARI" "S9" "LIN" "14" "11" "ACP-KACD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ZEN-KARI" BY REFERENCE W-ZEN-KARI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZEN-KASHI" "S9" "LIN" "27" "11" "ACP-ZEN-KARI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ZEN-KASHI" BY REFERENCE W-ZEN-KASHI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOU-KARI" "S9" "LIN" "54" "11" "ACP-ZEN-KASHI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TOU-KARI" BY REFERENCE W-TOU-KARI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOU-KASHI" "S9" "LIN" "67" "11" "ACP-TOU-KARI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TOU-KASHI" BY REFERENCE W-TOU-KASHI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-YOK-KARI" "S9" "LIN" "54" "11" "ACP-TOU-KASHI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-YOK-KARI" BY REFERENCE W-YOK-KARI(1) "11" "1" 
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-YOK-KASHI" "S9" "LIN" "67" "11" "ACP-YOK-KARI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-YOK-KASHI" BY REFERENCE W-YOK-KASHI(1) "11" "1" 
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-YOK-KASHI" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *DSP-DSP
       CALL "SD_Init" USING 
            "DSP-DSP" " " "0" "0" "124" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ZEN-TSUKI" "N" "LIN" "7" "4" " " "DSP-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ZEN-TSUKI" BY REFERENCE W-ZEN-X "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TOU-TSUKI" "N" "LIN" "47" "4" "DSP-ZEN-TSUKI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TOU-TSUKI" BY REFERENCE W-TOU-X "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-YOK-TSUKI" "N" "LIN" "47" "4" "DSP-TOU-TSUKI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-YOK-TSUKI" BY REFERENCE W-YOK-X "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-BUMEI" "N" "5" "56" "20" "DSP-YOK-TSUKI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-BUMEI" BY REFERENCE W-BUMEI "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KAMEI" "N" "6" "56" "20" "DSP-BUMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KAMEI" BY REFERENCE W-KAMEI "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ZEN-KARI" "ZZZZZZZZZZ9-" "LIN" "14" "12" "DSP-KAMEI"
            " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ZEN-KARI" BY REFERENCE W-ZEN-KARI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-ZEN-KASHI" "ZZZZZZZZZZ9-" "LIN" "27" "12"
            "DSP-ZEN-KARI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-ZEN-KASHI" BY REFERENCE W-ZEN-KASHI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TOU-KARI" "ZZZZZZZZZZ9-" "LIN" "54" "12"
            "DSP-ZEN-KASHI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TOU-KARI" BY REFERENCE W-TOU-KARI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TOU-KASHI" "ZZZZZZZZZZ9-" "LIN" "67" "12"
            "DSP-TOU-KARI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TOU-KASHI" BY REFERENCE W-TOU-KASHI(1) "11" "1" 
            BY REFERENCE I 44 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-YOK-KARI" "ZZZZZZZZZZ9-" "LIN" "54" "12"
            "DSP-TOU-KASHI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-YOK-KARI" BY REFERENCE W-YOK-KARI(1) "11" "1" 
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-YOK-KASHI" "ZZZZZZZZZZ9-" "LIN" "67" "12"
            "DSP-YOK-KARI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-YOK-KASHI" BY REFERENCE W-YOK-KASHI(1) "11" "1" 
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
           CALL "SD_Screen_Output" USING "GR0800" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" BZM-F_PNAME1 "SHARED" BY REFERENCE BZM-F_IDLST "1"
            "BZM-KEY" BY REFERENCE BZM-KEY.
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
           MOVE  KETSAN      TO  ZENTOUYOK-MONTH.
      ***  ë´ÇµÇƒÇ¢Ç≠ÅB
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       INI-010.
           IF  I NOT < 13
               GO  TO  INI-020
           END-IF.
           MOVE  ZENTOUYOK-MONTH     TO  W-ZEN-Z9  W-TOU-Z9.
           MOVE  W-ZEN-Z9            TO  W-ZEN-X.
           MOVE  W-TOU-Z9            TO  W-TOU-X.
           CALL "SD_Output" USING "DSP-ZEN-TSUKI"
                                  DSP-ZEN-TSUKI "p"
                                  RETURNING RESU
           CALL "SD_Output" USING "DSP-TOU-TSUKI"
                                  DSP-TOU-TSUKI "p"
                                  RETURNING RESU
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           ADD  1     TO  ZENTOUYOK-MONTH.
           IF  ZENTOUYOK-MONTH = 13
               MOVE  1     TO ZENTOUYOK-MONTH
           END-IF.
           GO  TO  INI-010.
      *
       INI-020.
           MOVE  KETSAN     TO  ZENTOUYOK-MONTH.
           MOVE  1     TO  I.
           MOVE  21    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       INI-030.
           IF  I NOT < 4
               GO  TO  INI-EX
           END-IF.
           MOVE  ZENTOUYOK-MONTH     TO  W-YOK-Z9.
           MOVE  W-YOK-Z9            TO  W-YOK-X.
           CALL "SD_Output" USING "DSP-YOK-TSUKI"
                                  DSP-YOK-TSUKI "p"
                                  RETURNING RESU
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           ADD  1     TO  ZENTOUYOK-MONTH.
           IF  ZENTOUYOK-MONTH = 13
               MOVE  1     TO ZENTOUYOK-MONTH
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
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p"
                                  RETURNING RESU.
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO  TO  MAIN-RTN
           END-IF.
           CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA2.
       MAIN-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-BUCD "ACP-BUCD" "9" "4"
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
           MOVE  W-BUCD     TO  BNM-KEY.
      ***  ïîñÂñºÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  BNM  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  BNMNMN
           END-IF.
           MOVE  BNMNMN     TO  W-BUMEI.
           CALL "SD_Output" USING "DSP-BUMEI" DSP-BUMEI "p"
                                  RETURNING RESU.
       MAIN-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-KACD "ACP-KACD"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
      *
           MOVE  W-KACD     TO  K-ACCD.
           MOVE  ZERO       TO  K-HOCD.
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
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
      *
           MOVE  W-BUCD      TO  BZM-BMON.
           MOVE  W-KACD      TO  BZM-KMCD.
      ***  ïîécÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  BZM-F  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BZM-F_PNAME1 BY REFERENCE BZM-REC " "
            RETURNING RET.
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
                   GO  TO  MAIN-140
               END-IF
           END-IF.
       MAIN-050.
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       MAIN-060.
           IF  I NOT < 13
               GO  TO  MAIN-080
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ZEN-KARI "ACP-ZEN-KARI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   GO  TO  MAIN-020
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
           CALL "SD_Output" USING "DSP-ZEN-KARI" DSP-ZEN-KARI "p"
                                  RETURNING RESU.
       MAIN-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ZEN-KASHI "ACP-ZEN-KASHI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-060
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-070
           END-IF.
           CALL "SD_Output" USING "DSP-ZEN-KASHI" DSP-ZEN-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  MAIN-060.
       MAIN-080.
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       MAIN-090.
           IF  I NOT < 13
               GO  TO  MAIN-110
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TOU-KARI "ACP-TOU-KARI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   MOVE  12     TO  I
                   MOVE  20     TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-060
               ELSE
                   SUBTRACT  1     FROM  I  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-090
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-090
           END-IF.
           CALL "SD_Output" USING "DSP-TOU-KARI" DSP-TOU-KARI "p"
                                  RETURNING RESU.
       MAIN-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TOU-KASHI "ACP-TOU-KASHI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-090
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-100
           END-IF.
           CALL "SD_Output" USING "DSP-TOU-KASHI" DSP-TOU-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  MAIN-090.
       MAIN-110.
           MOVE  1     TO  I.
           MOVE  21    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       MAIN-120.
           IF  I NOT < 4
               GO  TO  MAIN-140
           END-IF.
           CALL "SD_Accept" USING BY REFERENCE ACP-YOK-KARI
                 "ACP-YOK-KARI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   MOVE  12     TO  I
                   MOVE  20     TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-090
               ELSE
                   SUBTRACT  1     FROM  I  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-120
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-120
           END-IF.
           CALL "SD_Output" USING "DSP-YOK-KARI" DSP-YOK-KARI "p"
                                  RETURNING RESU.
       MAIN-130.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-YOK-KASHI "ACP-YOK-KASHI" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-120
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-130
           END-IF.
           CALL "SD_Output" USING "DSP-YOK-KASHI" DSP-YOK-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  MAIN-120.
       MAIN-140.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-020
               ELSE
                   MOVE  3     TO  I
                   MOVE  23    TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-120
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
               GO  TO  MAIN-140
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
            BY REFERENCE BZM-F_IDLST BZM-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ÇvÇnÇqÇjÅ|ÇqÇsÇm    *
      **************************
       WORK-RTN.
           MOVE  1           TO  I.
           MOVE  KETSAN      TO  J.
       WORK-010.
           IF  I NOT < 16
               GO  TO  WORK-EX
           END-IF.
           IF  I  >  12
               COMPUTE  I-Y  =   I  -  12
               MOVE  BZM-TJKR(I)     TO  W-YOK-KARI(I-Y)
               MOVE  BZM-TJKS(I)     TO  W-YOK-KASHI(I-Y)
               ADD   1       TO  I
               GO  TO  WORK-010
           END-IF.
           MOVE  BZM-ZJKR(J)     TO  W-ZEN-KARI(I).                     ëOä˙éÿï˚
           MOVE  BZM-ZJKS(J)     TO  W-ZEN-KASHI(I).                    ëOä˙ë›ï˚
           MOVE  BZM-TJKR(J)     TO  W-TOU-KARI(I).                     ìñä˙éÿï˚
           MOVE  BZM-TJKS(J)     TO  W-TOU-KASHI(I).                    ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  WORK-010.
       WORK-EX.
           EXIT.
      ************************
      *    ÇcÇrÇoÅ|ÇqÇsÇm    *
      ************************
       DSP-RTN.
           MOVE  1     TO  I.
           MOVE  9     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       DSP-010.
           IF  I NOT < 13
               GO  TO  DSP-020
           END-IF.
           CALL "SD_Output" USING "DSP-ZEN-KARI" DSP-ZEN-KARI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-ZEN-KASHI" DSP-ZEN-KASHI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOU-KARI" DSP-TOU-KARI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOU-KASHI" DSP-TOU-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  DSP-010.
       DSP-020.
           MOVE  1     TO  I.
           MOVE  21    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       DSP-030.
           IF  I NOT < 4
               GO  TO  DSP-EX
           END-IF.
           CALL "SD_Output" USING "DSP-YOK-KARI" DSP-YOK-KARI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-YOK-KASHI" DSP-YOK-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  DSP-030.
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
           MOVE  SPACE     TO  BZM-REC.
           INITIALIZE  BZM-REC.
           MOVE  W-BUCD     TO  BZM-BMON.                               ïîñÂÇbÇc
           MOVE  W-KACD     TO  BZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  BZM-KEY    TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       WRITE-010.
           IF  I NOT < 16
               GO  TO  WRITE-020
           END-IF.
           IF  I  >  12
               COMPUTE  I-Y  =  I  -  12
               MOVE  W-YOK-KARI(I-Y)   TO  BZM-TJKR(I)
               MOVE  W-YOK-KASHI(I-Y)  TO  BZM-TJKS(I)
               ADD   1       TO  I
               GO  TO  WRITE-010
           END-IF.
           MOVE  W-ZEN-KARI(I)     TO  BZM-ZJKR(J).                     ëOä˙éÿï˚
           MOVE  W-ZEN-KASHI(I)    TO  BZM-ZJKS(J).                     ëOä˙ë›ï˚
           MOVE  W-TOU-KARI(I)     TO  BZM-TJKR(J).                     ìñä˙éÿï˚
           MOVE  W-TOU-KASHI(I)    TO  BZM-TJKS(J).                     ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  WRITE-010.
       WRITE-020.
      *           WRITE  BZM-REC  INVALID
      *///////////////
           CALL "DB_Insert" USING
            BZM-F_PNAME1 BZM-F_LNAME BZM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "BZM-F"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-BUCD     TO  BZM-BMON.                               ïîñÂÇbÇc
           MOVE  W-KACD     TO  BZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  BZM-KEY    TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       REWRITE-010.
           IF  I NOT < 16
               GO  TO  REWRITE-020
           END-IF.
           IF  I  >  12
               COMPUTE  I-Y  =  I  -  12
               MOVE  W-YOK-KARI(I-Y)   TO  BZM-TJKR(I)
               MOVE  W-YOK-KASHI(I-Y)  TO  BZM-TJKS(I)
               ADD   1       TO    I
               GO  TO  REWRITE-010
           END-IF.
           MOVE  W-ZEN-KARI(I)     TO  BZM-ZJKR(J).                     ëOä˙éÿï˚
           MOVE  W-ZEN-KASHI(I)    TO  BZM-ZJKS(J).                     ëOä˙ë›ï˚
           MOVE  W-TOU-KARI(I)     TO  BZM-TJKR(J).                     ìñä˙éÿï˚
           MOVE  W-TOU-KASHI(I)    TO  BZM-TJKS(J).                     ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  REWRITE-010.
       REWRITE-020.
      *           REWRITE  BZM-REC  INVALID
      *///////////////
           CALL "DB_Update" USING
            BZM-F_PNAME1 BZM-F_LNAME BZM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "BZM-F"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-BUCD     TO  BZM-BMON.                               ïîñÂÇbÇc
           MOVE  W-KACD     TO  BZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  BZM-KEY         TO  ERR-K.
      *           DELETE  BZM-F  INVALID
      *///////////////
           CALL "DB_Delete" USING BZM-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "BZM-F"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
