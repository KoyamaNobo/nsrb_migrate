       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR090M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  â»ñ⁄écçÇÉ}ÉXÉ^ÉÅÉìÉeÉiÉìÉX    *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/12/04                      *
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
           02  W-ZEN-NC            PIC  N(02).
           02  W-TOUYOK-NC         PIC  N(02).
           02  KETSAN              PIC  9(02).                          èâåàéZåé
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉãÇ©ÇÁÇÃåàéZåéÇÇΩÇﬂÇ∆Ç≠
           02  ZENTOUYOK-MONTH     PIC  9(02).                          åàéZåé
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-KACD      PIC  9(04).                          â»ñ⁄ÇbÇc
                   04  W-KAMEI     PIC  N(10).                          â»ñ⁄ñºÅ@
                   04  W-AREA3.
                       05  W-ZMZ                PIC S9(11).             ëOä˙ññéc
                       05  W-AREA4     OCCURS 12.
                           07  W-ZEN-KARI       PIC S9(11).             ëOä˙éÿï˚
                           07  W-ZEN-KASHI      PIC S9(11).             ëOä˙ë›ï˚
                       05  W-AREA5     OCCURS 15.
                           07  W-TOUYOK-KARI       PIC S9(11).          ìñóÇéÿï˚
                           07  W-TOUYOK-KASHI      PIC S9(11).          ìñóÇë›ï˚
                       05  W-KAKU      PIC  X(01).                      ämîF
      ***
           COPY  LWMSG_PR.
      ***  â»ñ⁄écçÇÉ}ÉXÉ^
           COPY  LKAZAN.
      ***  ÉRÉìÉgÉçÅ[ÉãÉtÉ@ÉCÉã
           COPY  FCTL.
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
               03  FILLER  PIC X(004) VALUE "    ".                     â»ñ⁄ÇbÇc
               03  FILLER  PIC N(10).                                   â»ñ⁄ñºÅ@
               03  CLR-AREA3.
                   04  FILLER  PIC X(012) VALUE "            ".         ëOä˙ññéc
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
                   04  FILLER  PIC X(001) VALUE " ".                    ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT               PIC 9(01).                         ACT
           03  ACP-KACD              PIC 9(04).                         â»ñ⁄ÇbÇc
           03  ACP-ZMZ               PIC S9(11).                        ëOä˙ññéc
           03  ACP-KAMEI             PIC N(10).                         â»ñ⁄ñº
           03  ACP-ZEN-KARI          PIC S9(11).                        ëOä˙éÿï˚
           03  ACP-ZEN-KASHI         PIC S9(11).                        ëOä˙ë›ï˚
           03  ACP-TOUYOK-KARI       PIC S9(11).                        ìñóÇéÿï˚
           03  ACP-TOUYOK-KASHI      PIC S9(11).                        ìñóÇë›ï˚
           03  ACP-KAKU              PIC X(01).                         ämîF
      *********************
      *    âÊñ ï\é¶       *
      *********************
       01  DSP-DSP.
           03  DSP-ZEN-TSUKI        PIC N(02).                          ëOä˙åé
           03  DSP-TOUYOK-TSUKI     PIC N(02).                          ìñóÇä˙åé
           03  DSP-ZMZ              PIC ZZZZZZZZZZ9-.                   ëOä˙ññéc
           03  DSP-ZEN-KARI         PIC ZZZZZZZZZZ9-.                   ëOä˙éÿï˚
           03  DSP-ZEN-KASHI        PIC ZZZZZZZZZZ9-.                   ëOä˙ë›ï˚
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
            "CLR-AREA1" " " "0" "0" "686" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA1" "X" "3" "66" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "685" "01CLR-AREA1" " "
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
            "CLR-AREA3" " " "0" "0" "661" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA3" "X" "6" "27" "12" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA3" " " "9" "0" "48" "01CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA3" "X" "9" "14" "12" " " "02CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA3" "X" "9" "27" "12" "03CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA3" "X" "9" "54" "12" "04CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA3" "X" "9" "67" "12" "05CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-AREA3" " " "10" "0" "48" "02CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-AREA3" "X" "10" "14" "12" " " "07CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-AREA3" "X" "10" "27" "12" "08CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-AREA3" "X" "10" "54" "12" "09CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11CLR-AREA3" "X" "10" "67" "12" "10CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12CLR-AREA3" " " "11" "0" "48" "07CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "13CLR-AREA3" "X" "11" "14" "12" " " "12CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "14CLR-AREA3" "X" "11" "27" "12" "13CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "15CLR-AREA3" "X" "11" "54" "12" "14CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "16CLR-AREA3" "X" "11" "67" "12" "15CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "17CLR-AREA3" " " "12" "0" "48" "12CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "18CLR-AREA3" "X" "12" "14" "12" " " "17CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "19CLR-AREA3" "X" "12" "27" "12" "18CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "20CLR-AREA3" "X" "12" "54" "12" "19CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "21CLR-AREA3" "X" "12" "67" "12" "20CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "22CLR-AREA3" " " "13" "0" "48" "17CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "23CLR-AREA3" "X" "13" "14" "12" " " "22CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "24CLR-AREA3" "X" "13" "27" "12" "23CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "25CLR-AREA3" "X" "13" "54" "12" "24CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "26CLR-AREA3" "X" "13" "67" "12" "25CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "27CLR-AREA3" " " "14" "0" "48" "22CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "28CLR-AREA3" "X" "14" "14" "12" " " "27CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "29CLR-AREA3" "X" "14" "27" "12" "28CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "30CLR-AREA3" "X" "14" "54" "12" "29CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "31CLR-AREA3" "X" "14" "67" "12" "30CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "32CLR-AREA3" " " "15" "0" "48" "27CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "33CLR-AREA3" "X" "15" "14" "12" " " "32CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "34CLR-AREA3" "X" "15" "27" "12" "33CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "35CLR-AREA3" "X" "15" "54" "12" "34CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "36CLR-AREA3" "X" "15" "67" "12" "35CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "37CLR-AREA3" " " "16" "0" "48" "32CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "38CLR-AREA3" "X" "16" "14" "12" " " "37CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "39CLR-AREA3" "X" "16" "27" "12" "38CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "40CLR-AREA3" "X" "16" "54" "12" "39CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "41CLR-AREA3" "X" "16" "67" "12" "40CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "42CLR-AREA3" " " "17" "0" "48" "37CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "43CLR-AREA3" "X" "17" "14" "12" " " "42CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "44CLR-AREA3" "X" "17" "27" "12" "43CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "45CLR-AREA3" "X" "17" "54" "12" "44CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "46CLR-AREA3" "X" "17" "67" "12" "45CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "47CLR-AREA3" " " "18" "0" "48" "42CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "48CLR-AREA3" "X" "18" "14" "12" " " "47CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "49CLR-AREA3" "X" "18" "27" "12" "48CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "50CLR-AREA3" "X" "18" "54" "12" "49CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "51CLR-AREA3" "X" "18" "67" "12" "50CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "52CLR-AREA3" " " "19" "0" "48" "47CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "53CLR-AREA3" "X" "19" "14" "12" " " "52CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "54CLR-AREA3" "X" "19" "27" "12" "53CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "55CLR-AREA3" "X" "19" "54" "12" "54CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "56CLR-AREA3" "X" "19" "67" "12" "55CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "57CLR-AREA3" " " "20" "0" "48" "52CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "58CLR-AREA3" "X" "20" "14" "12" " " "57CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "59CLR-AREA3" "X" "20" "27" "12" "58CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "60CLR-AREA3" "X" "20" "54" "12" "59CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "61CLR-AREA3" "X" "20" "67" "12" "60CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "62CLR-AREA3" " " "21" "0" "24" "57CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "63CLR-AREA3" "X" "21" "54" "12" " " "62CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "64CLR-AREA3" "X" "21" "67" "12" "63CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "65CLR-AREA3" " " "22" "0" "24" "62CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "66CLR-AREA3" "X" "22" "54" "12" " " "65CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "67CLR-AREA3" "X" "22" "67" "12" "66CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "68CLR-AREA3" " " "23" "0" "24" "65CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "69CLR-AREA3" "X" "23" "54" "12" " " "68CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING
            "70CLR-AREA3" "X" "23" "67" "12" "69CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "71CLR-AREA3" "X" "24" "77" "1" "68CLR-AREA3" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "81" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "66" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD" "9" "5" "34" "4" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD" BY REFERENCE W-KACD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZMZ" "S9" "6" "27" "11" "ACP-KACD" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-ZMZ" BY REFERENCE W-ZMZ "11" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAMEI" "N" "5" "56" "20" "ACP-ZMZ" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAMEI" BY REFERENCE W-KAMEI "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZEN-KARI" "S9" "LIN" "14" "11" "ACP-KAMEI" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-ZEN-KARI" BY REFERENCE W-ZEN-KARI(1) "11" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ZEN-KASHI" "S9" "LIN" "27" "11" "ACP-ZEN-KARI" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-ZEN-KASHI" BY REFERENCE W-ZEN-KASHI(1) "11" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOUYOK-KARI" "S9" "LIN" "54" "11" "ACP-ZEN-KASHI" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-TOUYOK-KARI" BY REFERENCE W-TOUYOK-KARI(1) "11" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOUYOK-KASHI" "S9" "LIN" "67" "11"
            "ACP-TOUYOK-KARI" " "
            RETURNING RESU.
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
            "DSP-DSP" " " "0" "0" "68" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-ZEN-TSUKI" "N" "LIN" "7" "4" " " "DSP-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-ZEN-TSUKI" BY REFERENCE W-ZEN-NC "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUYOK-TSUKI" "N" "LIN" "47" "4" "DSP-ZEN-TSUKI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUYOK-TSUKI" BY REFERENCE W-TOUYOK-NC "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-ZMZ" "ZZZZZZZZZZ9-" "6" "27" "12"
            "DSP-TOUYOK-TSUKI" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-ZMZ" BY REFERENCE W-ZMZ "12" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-ZEN-KARI" "ZZZZZZZZZZ9-" "LIN" "14" "12" "DSP-ZMZ" " "
             RETURNING RESU.
       CALL "SD_From" USING
            "DSP-ZEN-KARI" BY REFERENCE W-ZEN-KARI(1) "12" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-ZEN-KASHI" "ZZZZZZZZZZ9-" "LIN" "27" "12"
            "DSP-ZEN-KARI" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-ZEN-KASHI" BY REFERENCE W-ZEN-KASHI(1) "12" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUYOK-KARI" "ZZZZZZZZZZ9-" "LIN" "54" "12"
            "DSP-ZEN-KASHI" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOUYOK-KARI" BY REFERENCE W-TOUYOK-KARI(1) "12" "1"
            BY REFERENCE I 22 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOUYOK-KASHI" "ZZZZZZZZZZ9-" "LIN" "67" "12"
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
           CALL "SD_Screen_Output" USING "GR0900" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
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
           MOVE  ZENTOUYOK-MONTH     TO  W-Z9.
           MOVE  W-Z9                TO  W-ZEN-NC  W-TOUYOK-NC.
           CALL "SD_Output" USING "DSP-ZEN-TSUKI" DSP-ZEN-TSUKI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOUYOK-TSUKI"
                                  DSP-TOUYOK-TSUKI "p"
                                  RETURNING RESU.
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
           MOVE  ZENTOUYOK-MONTH     TO  W-Z9.
           MOVE  W-Z9                TO  W-TOUYOK-NC.
           CALL "SD_Output" USING "DSP-TOUYOK-TSUKI"
                                  DSP-TOUYOK-TSUKI "p"
                                  RETURNING RESU.
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
           CALL "SD_Output" USING "ACP-KAMEI" ACP-KAMEI "p"
                                  RETURNING RESU.
      *
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
      *
           MOVE  W-KACD      TO  KZM-KMCD.                              â»ñ⁄ÇbÇc
      ***  â»ñ⁄écçÇÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  KZM-F  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KZM-F_PNAME1 BY REFERENCE KZM-R " " RETURNING RET.
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
               PERFORM  WORK-RTN     THRU  WORK-EX
               PERFORM  DSP-RTN      THRU  DSP-EX
               IF  W-ACT = 3
                   GO  TO  MAIN-110
               END-IF
           END-IF.
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ZMZ "ACP-ZMZ" "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01"
               GO  TO  MAIN-040
           END-IF.
           CALL "SD_Output" USING "DSP-ZMZ" DSP-ZMZ "p"
                                  RETURNING RESU.
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
                   GO  TO  MAIN-040
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
           IF  I NOT < 16
               GO  TO  MAIN-110
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TOUYOK-KARI "ACP-TOUYOK-KARI"
                 "S9" "11"
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
           CALL "SD_Output" USING "DSP-TOUYOK-KARI" DSP-TOUYOK-KARI "p"
                                  RETURNING RESU.
       MAIN-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TOUYOK-KASHI "ACP-TOUYOK-KASHI"
                 "S9" "11"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-090
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-100
           END-IF.
           CALL "SD_Output" USING "DSP-TOUYOK-KASHI"
                                  DSP-TOUYOK-KASHI "p"
                                  RETURNING RESU.
           ADD  1     TO  I  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           GO  TO  MAIN-090.
       MAIN-110.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-010
               ELSE
                   MOVE  15    TO  I
                   MOVE  23    TO  LIN
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU
                   GO  TO  MAIN-090
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
      *    èIóπèàóù          *
      ************************
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ÇvÇnÇqÇjÅ|ÇqÇsÇm    *
      **************************
       WORK-RTN.
           MOVE  KZM-ZAN     TO  W-ZMZ.
           MOVE  1           TO  I.
           MOVE  KETSAN      TO  J.
       WORK-010.
           IF  I NOT < 13
               GO  TO  WORK-020
           END-IF.
           MOVE  KZM-ZJKR(J)     TO  W-ZEN-KARI(I).                     ëOä˙éÿï˚
           MOVE  KZM-ZJKS(J)     TO  W-ZEN-KASHI(I).                    ëOä˙ë›ï˚
           MOVE  KZM-TJKR(J)     TO  W-TOUYOK-KARI(I).                  ìñä˙éÿï˚
           MOVE  KZM-TJKS(J)     TO  W-TOUYOK-KASHI(I).                 ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  WORK-010.
       WORK-020.
           IF  I NOT < 16
               GO  TO  WORK-EX
           END-IF.
           MOVE  KZM-TJKR(I)     TO  W-TOUYOK-KARI(I).                  óÇä˙éÿï˚
           MOVE  KZM-TJKS(I)     TO  W-TOUYOK-KASHI(I).                 óÇä˙ë›ï˚
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
           CALL "SD_Output" USING "DSP-ZEN-KARI" DSP-ZEN-KARI "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-ZEN-KASHI" DSP-ZEN-KASHI "p"
                                  RETURNING RESU.
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
           MOVE  21    TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
       DSP-030.
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
           MOVE  SPACE     TO  KZM-R.
           INITIALIZE  KZM-R.
           MOVE  W-KACD     TO  KZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  W-ZMZ      TO  KZM-ZAN.                                ëOä˙ññéc
           MOVE  KZM-KEY    TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       WRITE-010.
           IF  I NOT < 13
               GO  TO  WRITE-020
           END-IF.
           MOVE  W-ZEN-KARI(I)        TO  KZM-ZJKR(J).                  ëOä˙éÿï˚
           MOVE  W-ZEN-KASHI(I)       TO  KZM-ZJKS(J).                  ëOä˙ë›ï˚
           MOVE  W-TOUYOK-KARI(I)     TO  KZM-TJKR(J).                  ìñä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  KZM-TJKS(J).                  ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  WRITE-010.
       WRITE-020.
           IF  I NOT < 16
               GO  TO  WRITE-030
           END-IF.
           MOVE  W-TOUYOK-KARI(I)     TO  KZM-TJKR(I).                  óÇä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  KZM-TJKS(I).                  óÇä˙ë›ï˚
           ADD  1     TO  I.
           GO  TO  WRITE-020.
       WRITE-030.
      *           WRITE  KZM-R  INVALID
      *///////////////
           CALL "DB_Insert" USING
            KZM-F_PNAME1 KZM-F_LNAME KZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-KACD     TO  KZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  W-ZMZ      TO  KZM-ZAN.                                ëOä˙ññéc
           MOVE  KZM-KEY    TO  ERR-K.
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
       REWRITE-010.
           IF  I NOT < 13
               GO  TO  REWRITE-020
           END-IF.
           MOVE  W-ZEN-KARI(I)        TO  KZM-ZJKR(J).                  ëOä˙éÿï˚
           MOVE  W-ZEN-KASHI(I)       TO  KZM-ZJKS(J).                  ëOä˙ë›ï˚
           MOVE  W-TOUYOK-KARI(I)     TO  KZM-TJKR(J).                  ìñä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  KZM-TJKS(J).                  ìñä˙ë›ï˚
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  REWRITE-010.
       REWRITE-020.
           IF  I NOT < 16
               GO  TO  REWRITE-030
           END-IF.
           MOVE  W-TOUYOK-KARI(I)     TO  KZM-TJKR(I).                  óÇä˙éÿï˚
           MOVE  W-TOUYOK-KASHI(I)    TO  KZM-TJKS(I).                  óÇä˙ë›ï˚
           ADD  1     TO  I.
           GO  TO  REWRITE-020.
       REWRITE-030.
      *           REWRITE  KZM-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            KZM-F_PNAME1 KZM-F_LNAME KZM-R RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-KACD     TO  KZM-KMCD.                               â»ñ⁄ÇbÇc
           MOVE  KZM-KEY         TO  ERR-K.
      *           DELETE  KZM-F  INVALID
      *///////////////
           CALL "DB_Delete" USING KZM-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "KZM-F"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
