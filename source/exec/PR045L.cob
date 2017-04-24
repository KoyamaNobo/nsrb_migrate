       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR045L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  åªÅDóaã‡É}ÉXÉ^ÉäÉXÉgÅ@Å@Å@Å@  *
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
       77  C2                      PIC  X(05)  VALUE  X"1A24212474".
       77  LCNT                    PIC  9(02).
       77  PCNT                    PIC  9(05).
       77  RTN-SW                  PIC  9(01).
      ***  RTN-SW = 1 ÇÃéûÅCMAIN-RTN Ç÷ñﬂÇÈÅB
       01  HIZUKE                  PIC  9(06).                          ºΩ√—ÀΩﬁπ
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  OLD-NO                  PIC  9(04).                          â»ñ⁄ÇbÇc
       01  W-AREA.
           02  W-KAMOKUCD-FROM     PIC  9(04).                          â»ñ⁄ÇeÇq
           02  W-KAMOKUCD-TO       PIC  9(04).                          â»ñ⁄ÇsÇn
           02  W-GINCD-FROM        PIC  9(04).                          ã‚çsÇeÇq
           02  W-GINCD-TO          PIC  9(04).                          ã‚çsÇsÇn
           02  W-KAKU              PIC  X(01).                          ämîF
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(19) VALUE
               "åªÅ@ÅEÅ@óaÅ@ã‡Å@É}Å@ÉXÅ@É^Å@ÉäÅ@ÉXÅ@Ég".
           02  F                   PIC  X(32) VALUE  SPACE.
           02  M-YY                PIC  Z9.                             NEN
           02  F                   PIC  N(01) VALUE  "îN".
           02  M-MM                PIC  Z9.                             TSUKI
           02  F                   PIC  N(01) VALUE  "åé".
           02  M-DD                PIC  Z9.                             HI
           02  F                   PIC  N(03) VALUE  "ì˙çÏê¨".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  M-PCNT              PIC  ZZZZ9.                          PCNT
           02  F                   PIC  N(01) VALUE  "ï≈".
       01  MID-02.
           02  F                   PIC  N(05) VALUE  "â»ñ⁄ÉRÅ[Éh".
           02  F                   PIC  X(06) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "â»".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "ñ⁄".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "ñº".
           02  F                   PIC  X(06) VALUE  SPACE.
           02  F                   PIC  N(05) VALUE  "ã‚çsÉRÅ[Éh".
           02  F                   PIC  X(06) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "ã‚".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "çs".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "ñº".
           02  F                   PIC  X(13) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "ëOì˙écçÇ".
      ***
           COPY  LWMSG_PR.
      ***  åªÅDóaã‡É}ÉXÉ^
           COPY  LGYM.
      ***  ã‚çsÅ@Å@É}ÉXÉ^
           COPY  L-BANK.
      ***  äøéöâ»ñ⁄É}ÉXÉ^
           COPY  KANGEL.
      ***  ÉvÉäÉìÉ^Å[
       01  PRINTF.
      *           LABEL       RECORD  OMITTED
      *           LINAGE          IS       66.
           02  PRINTR              PIC  X(250).
           02  PRINTR1             REDEFINES  PRINTR.
               03  C-2B            PIC  X(05).                          2BYTE
               03  F               PIC  X(03).
               03  P-01            PIC  9(04).                          â»ñ⁄ÇbÇc
               03  F               PIC  X(05).
               03  P-KNAM          PIC  N(10).
               03  F               PIC  X(05).
               03  P-02            PIC  9(04).
               03  F               PIC  X(05).
               03  P-GNAM          PIC  N(10).
               03  F               PIC  X(02).
               03  P-04            PIC  ---,---,---,--9.                ëOì˙écçÇ
       77  F                       PIC  X(001).
      **
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      ******************************
      *Å@Å@âÊñ ÉNÉäÉAÅ[çÄñ⁄Å@Å@    *
      ******************************
       01  DSP-CLR.
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           03  FILLER   PIC X(004) VALUE "    ".                        â»ñ⁄ÇeÇq
           03  FILLER   PIC X(004) VALUE "    ".                        â»ñ⁄ÇsÇn
           03  FILLER   PIC X(004) VALUE "    ".
           03  FILLER   PIC X(004) VALUE "    ".
           03  FILLER   PIC X(001) VALUE " ".                           ämîFÅ@Å@
      *******************
      *    âÊñ ï\é¶     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC N(011) VALUE
               " åªÅEóaã‡É}ÉXÉ^ÉäÉXÉg ".
           03  FILLER  PIC N(004) VALUE  "ÇeÇqÇnÇl".
           03  FILLER  PIC N(002) VALUE  "ÇsÇn".
           03  FILLER  PIC N(005) VALUE  "â»ñ⁄ÉRÅ[Éh".
           03  FILLER  PIC N(005) VALUE  "ã‚çsÉRÅ[Éh".
           03  FILLER  PIC N(001) VALUE  "Å`".
           03  FILLER  PIC N(001) VALUE  "Å`".
           03  FILLER  PIC X(018)
               VALUE  "ämîF OK=1,NO=9 ( )".
      ***********************
      *    âÊñ ì¸óÕ         *
      ***********************
       01  ACP-AREA.
           03  ACP-KAMOKUCD-FROM    PIC 9(04).                          â»ñ⁄ÇeÇq
           03  ACP-KAMOKUCD-TO      PIC 9(04).                          â»ñ⁄ÇsÇn
           03  ACP-GINCD-FROM       PIC 9(04).
           03  ACP-GINCD-TO         PIC 9(04).
           03  ACP-KAKU             PIC X(01).                          ämîFÅ@Å@
       COPY  LSMSG_PR.
      ***
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "6" "33" "4" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "51" "4" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "8" "33" "4" "02CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA" "X" "8" "51" "4" "03CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA" "X" "24" "77" "1" "04CLR-AREA" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "30" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "N" "4" "31" "8" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "N" "4" "51" "4" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" "N" "6" "11" "10" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-AREA" "N" "8" "11" "10" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "N" "6" "43" "2" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-AREA" "N" "8" "43" "2" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-AREA" "N" "24" "61" "18" "07DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAMOKUCD-FROM" "9" "6" "33" "4" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAMOKUCD-FROM" BY REFERENCE W-KAMOKUCD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAMOKUCD-TO" "9" "6" "51" "4" "ACP-KAMOKUCD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KAMOKUCD-TO" BY REFERENCE W-KAMOKUCD-TO "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-GINCD-FROM" "9" "8" "33" "4" "ACP-KAMOKUCD-TO" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-GINCD-FROM" BY REFERENCE W-GINCD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-GINCD-TO" "9" "8" "51" "4" "ACP-GINCD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-GINCD-TO" BY REFERENCE W-GINCD-TO "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-GINCD-TO" " "
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
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           ACCEPT  HIZUKE  FROM  DATE.
           CALL "DB_F_Open" USING
            "INPUT" GYM_PNAME1 "SHARED" BY REFERENCE GYM_IDLST "1"
            "GYM-KEY" BY REFERENCE GYM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BM_PNAME1 "SHARED" BY REFERENCE BM_IDLST "1"
            "BM-KEY" BY REFERENCE BM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE  90     TO  LCNT.
       INI-EX.
           EXIT.
      *****************************
      *    ÇlÇ`ÇhÇmÅ@èàóùÅ@Å@Å@Å@ *
      *****************************
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAMOKUCD-FROM "ACP-KAMOKUCD-FROM"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO  TO  MAIN-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAMOKUCD-TO "ACP-KAMOKUCD-TO" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-KAMOKUCD-TO = ZERO
               MOVE  ALL "9"     TO  W-KAMOKUCD-TO
           END-IF.
           IF  W-KAMOKUCD-FROM > W-KAMOKUCD-TO
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-GINCD-FROM "ACP-GINCD-FROM" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
       MAIN-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-GINCD-TO "ACP-GINCD-TO" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-030
           END-IF.
           IF  W-GINCD-TO = ZERO
               MOVE  ALL "9"     TO  W-GINCD-TO
           END-IF.
           IF  W-GINCD-FROM > W-GINCD-TO
               GO  TO  MAIN-030
           END-IF.
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  W-KAKU = 9                                               = "02"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               INITIALIZE  W-AREA
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU NOT = 1                                           = "04"
               GO  TO  MAIN-040
           END-IF.
           PERFORM  LST-RTN     THRU  LST-EX.
           IF  RTN-SW = 1
               MOVE  ZERO     TO  RTN-SW
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-EX.
           EXIT.
      ************************
      *    èIóπèàóù          *
      ************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE GYM_IDLST GYM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BM_IDLST BM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    ÇkÇrÇsÅ|ÇqÇsÇm     *
      *************************
       LST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-KAMOKUCD-FROM     TO  GYM-011.                       â»ñ⁄ÇbÇc
           MOVE  W-GINCD-FROM        TO  GYM-012.                       ã‚çsÇbÇc
      *           START  GYM  KEY  NOT < GYM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            GYM_PNAME1 "GYM-KEY" " NOT < " GYM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                         RETURNING RESU
      ***  ÉfÅ[É^ñ¢ìoò^Å@ï\é¶
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      **
      ***  åªÅDóaã‡É}ÉXÉ^Å@ÇqÇdÇ`Çc
       LST-010.
      *           READ  GYM  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" GYM_PNAME1 BY REFERENCE GYM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-020
           END-IF.
           GO  TO  LST-030.
       LST-020.
           IF  LCNT = 90
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                         RETURNING RESU
      ***  ÉfÅ[É^ñ¢ìoò^Å@ï\é¶
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           ELSE
               GO  TO  LST-999
           END-IF.
       LST-030.
      ***  â»ñ⁄ÉRÅ[ÉhÇ∆ã‚çsÉRÅ[ÉhÇ∆Ç≈ÇÊÇ›îÚÇŒÇµ
           IF  GYM-011 > W-KAMOKUCD-TO
               IF  LCNT = 90
                   CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                             RETURNING RESU
                   CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                             RETURNING RESU
      ***  ÉfÅ[É^ñ¢ìoò^Å@ï\é¶
                   MOVE  1     TO  RTN-SW
                   GO  TO  LST-999
               ELSE
                   GO  TO  LST-999
           END-IF.
           IF  GYM-012 < W-GINCD-FROM
               GO  TO  LST-010
           END-IF.
           IF  GYM-012 > W-GINCD-TO
               GO  TO  LST-010
           END-IF.
           IF  LCNT NOT < 62
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF.
           IF  ( GYM-011 NOT = OLD-NO )  OR  (LCNT = 4 )
               PERFORM  HEAD-RTN    THRU  HEAD-EX
               MOVE  GYM-011     TO  OLD-NO
               GO  TO  LST-010
           ELSE
               PERFORM  MEI-RTN     THRU  MEI-EX
               MOVE  GYM-011     TO  OLD-NO
               GO  TO  LST-010
           END-IF.
       LST-999.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      ****************************
      *    ÇlÇhÇcÅ|ÇqÇsÇmÅ@      *
      ****************************
       MID-RTN.
           IF  LCNT NOT = 90
               MOVE  SPACE     TO  PRINTR
               CALL "PR_Write" USING PRINTR RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD  1     TO  PCNT.
           MOVE  PCNT   TO  M-PCNT.
           MOVE  YY     TO  M-YY.
           MOVE  MM     TO  M-MM.
           MOVE  DD     TO  M-DD.
           MOVE  MID-01 TO PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  MID-02 TO PRINTR.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  4     TO  LCNT.
       MID-EX.
           EXIT.
      ***************************
      *    ÇgÇdÇ`ÇcÅ|ÇqÇsÇm     *
      ***************************
       HEAD-RTN.
           MOVE  C2         TO  C-2B.
           MOVE  GYM-011    TO  P-01.                                   â»ñ⁄ÇbÇc
           MOVE  GYM-011    TO  K-ACCD.
           MOVE  ZERO       TO  K-HOCD.
      *           READ  KNG        UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                 MOVE SPACE TO  KNGNMN
           END-IF.
           MOVE  KNGNMN     TO  P-KNAM.
           MOVE  GYM-012    TO  P-02.                                   ã‚çsÇbÇc
           MOVE  GYM-012    TO  BM-KEY.
      *           READ  BM         UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BM_PNAME1 BY REFERENCE BM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO  BANKNMN
           END-IF.
           MOVE  BANKNMN    TO  P-GNAM.
           MOVE  GYM-03     TO  P-04.                                   ëOì˙écçÇ
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           ADD  2     TO  LCNT.
       HEAD-EX.
           EXIT.
      ***************************
      *    ÇlÇdÇhÅ|ÇqÇsÇm Å@Å@  *
      ***************************
       MEI-RTN.
           MOVE  C2         TO  C-2B.
           MOVE  GYM-012    TO  P-02.                                   ã‚çsÇbÇc
           MOVE  GYM-012    TO  BM-KEY.
      *           READ  BM         UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BM_PNAME1 BY REFERENCE BM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO  BANKNMN
           END-IF.
           MOVE  BANKNMN    TO  P-GNAM.
           MOVE  GYM-03     TO  P-04.                                   ëOì˙écçÇ
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           ADD  1     TO  LCNT.
       MEI-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
