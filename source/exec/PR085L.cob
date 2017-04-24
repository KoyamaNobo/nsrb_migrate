       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR085L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  ìEóvÉ}ÉXÉ^ÉäÉXÉgÅ@Å@Å@Å@Å@Å@  *
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
       77  C2                      PIC  X(05)  VALUE  X"1A24212474".
       77  LCNT                    PIC  9(02).
       77  PCNT                    PIC  9(05).
       77  RTN-SW                  PIC  9(01).
      ***  RTN-SW = 1 ÇÃéûÅCMAIN-RTN Ç÷ñﬂÇÈÅB
       01  HIZUKE                  PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  W-AREA.
           02  W-TEKIYOUCD-FROM    PIC  9(03).
           02  W-TEKIYOUCD-TO      PIC  9(03).
           02  W-KAKU              PIC  X(01).
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(15) VALUE
               "ìEÅ@óvÅ@É}Å@ÉXÅ@É^Å@ÉäÅ@ÉXÅ@Ég".
           02  F                   PIC  X(40) VALUE  SPACE.
           02  M-YY                PIC  Z9.
           02  F                   PIC  N(01) VALUE  "îN".
           02  M-MM                PIC  Z9.
           02  F                   PIC  N(01) VALUE  "åé".
           02  M-DD                PIC  Z9.
           02  F                   PIC  N(03) VALUE  "ì˙çÏê¨".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  M-PCNT              PIC  ZZZZ9.
           02  F                   PIC  N(01) VALUE  "ï≈".
       01  MID-02.
           02  F                   PIC  N(05) VALUE  "ìEóvÉRÅ[Éh".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(07) VALUE
                                   "ìEÅ@Å@óvÅ@Å@ñº".
      ***
           COPY  LWMSG_PR.
      ***  ìEóvÉ}ÉXÉ^
           COPY  LTKI.
      ***  ÉvÉäÉìÉ^Å[
       01  PRINTF.
           02  PRINTR              PIC  X(250).
           02  PRINTR1             REDEFINES     PRINTR.
               03  C-2B            PIC  X(05).
               03  F               PIC  X(03).
               03  P-01            PIC  9(03).
               03  F               PIC  X(06).
               03  P-02            PIC  N(20).
       77  F                       PIC  X(001).
      *
       77  SP-R               PIC  X(204).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
      ******************************
      *Å@Å@âÊñ ÉNÉäÉAÅ[çÄñ⁄Å@Å@    *
      ******************************
       01  DSP-CLR.
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           03  FILLER   PIC X(003) VALUE "   ".
           03  FILLER   PIC X(003) VALUE "   ".
           03  FILLER   PIC X(001) VALUE " ".
      *******************
      *    âÊñ ï\é¶     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC N(009) VALUE
               " ìEóvÉ}ÉXÉ^ÉäÉXÉg ".
           03  FILLER  PIC N(004) VALUE  "ÇeÇqÇnÇl".
           03  FILLER  PIC N(002) VALUE  "ÇsÇn".
           03  FILLER  PIC N(005) VALUE  "ìEóvÉRÅ[Éh".
           03  FILLER  PIC N(001) VALUE  "Å`".
           03  FILLER  PIC X(018) VALUE
               "ämîF OK=1,NO=9 ( )".
      ***********************
      *    âÊñ ì¸óÕ         *
      ***********************
       01  ACP-AREA.
           03  ACP-TEKIYOUCD-FROM       PIC 9(03).
           03  ACP-TEKIYOUCD-TO         PIC 9(03).
           03  ACP-KAKU                 PIC X(01).
       COPY  LSMSG_PR.
      ***
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "6" "34" "3" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "52" "3" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "24" "77" "1" "02CLR-AREA" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "32" "18" " " "DSP-AREA"
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
            "05DSP-AREA" "N" "6" "43" "2" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "N" "24" "61" "18" "05DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEKIYOUCD-FROM" "9" "6" "34" "3" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TEKIYOUCD-FROM" BY REFERENCE W-TEKIYOUCD-FROM "3" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEKIYOUCD-TO" "9" "6" "52" "3"
            "ACP-TEKIYOUCD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TEKIYOUCD-TO" BY REFERENCE W-TEKIYOUCD-TO "3" "0"
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
            "INPUT" TKI_PNAME1 "SHARED" BY REFERENCE TKI_IDLST "1"
            "TKI-KEY" BY REFERENCE TKI-KEY.
           MOVE  90     TO  LCNT.
       INI-EX.
           EXIT.
      *****************************
      *    ÇlÇ`ÇhÇmÅ@èàóùÅ@Å@Å@Å@ *
      *****************************
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TEKIYOUCD-FROM "ACP-TEKIYOUCD-FROM"
                 "9" "3" BY REFERENCE ESTAT RETURNING RESU.
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
                 BY REFERENCE ACP-TEKIYOUCD-TO "ACP-TEKIYOUCD-TO"
                 "9" "3" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-TEKIYOUCD-TO = ZERO
               MOVE  ALL "9"     TO  W-TEKIYOUCD-TO
           END-IF.
           IF  W-TEKIYOUCD-FROM > W-TEKIYOUCD-TO
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU = 9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               INITIALIZE  W-AREA
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU NOT = 1
               GO  TO  MAIN-020
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
           CALL "DB_F_Close" USING BY REFERENCE TKI_IDLST TKI_PNAME1.
       CLSE-EXT.
           EXIT.
      ************************
      *    ÇkÇrÇsÅ|ÇqÇsÇm    *
      ************************
       LST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-TEKIYOUCD-FROM        TO  TKI-KEY.
      *           START  TKI  KEY  NOT < TKI-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            TKI_PNAME1 "TKI-KEY" " NOT < " TKI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                              RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                              RETURNING RESU
      *      ***  ÉfÅ[É^ñ¢ìoò^Å@ï\é¶
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      **
      ***  ìEóvÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
       LST-010.
      *           READ  TKI  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TKI_PNAME1 BY REFERENCE TKI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-999
           END-IF.
           IF  TKI-KEY > W-TEKIYOUCD-TO
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
               END-IF
           END-IF.
           IF  LCNT NOT < 62
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF.
           PERFORM  MEI-RTN     THRU  MEI-EX.
           GO  TO  LST-010.
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
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  4     TO  LCNT.
       MID-EX.
           EXIT.
      ***************************
      *    ÇlÇdÇhÅ|ÇqÇsÇm Å@Å@  *
      ***************************
       MEI-RTN.
           MOVE  C2         TO  C-2B.
           MOVE  TKI-01     TO  P-01.
           MOVE  TKI-02     TO  P-02.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           ADD  1     TO  LCNT.
       MEI-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
