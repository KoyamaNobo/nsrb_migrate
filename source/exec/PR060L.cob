       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR060L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  ïîñÂñºÉ}ÉXÉ^ÉäÉXÉgÅ@Å@Å@Å@Å@  *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/19                      *
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
       77  I                       PIC  9(01).
       77  RTN-SW                  PIC  9(01).
      ***  RTN-SW = 1 ÇÃéûÅCMAIN-RTN Ç÷ñﬂÇÈÅB
       01  HIZUKE                  PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  W-AREA.
           02  W-BUMONCD-FROM      PIC  9(04).
           02  W-BUMONCD-TO        PIC  9(04).
           02  W-KAKU              PIC  X(01).
      *
       01  PRINTR1.
           02  C-2B                PIC  X(05).
           02  F                   PIC  X(01)  VALUE  SPACE.
           02  P1-01               PIC  9(04).
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  P1-02               PIC  N(10).
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  P1-03               PIC  9(01).
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  N(01)  VALUE  "ï≈".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  FF     OCCURS 3.
               03  P1-04               PIC  Z9.
               03  F                   PIC  X(06)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFF    OCCURS 6.
               03  P1-05               PIC  Z9.
               03  F                   PIC  X(06)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFFF   OCCURS 3.
               03  P1-06               PIC  Z9.
               03  F                   PIC  X(06)  VALUE  SPACE.
      *
       01  PRINTR2.
           02  F                   PIC  X(35)  VALUE  SPACE.
           02  F                   PIC  N(01)  VALUE  "óÒ".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  FFFFF     OCCURS 3.
               03  P2-01               PIC  9(01).
               03  F                   PIC  X(07)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFFFFF    OCCURS 6.
               03  P2-02               PIC  9(01).
               03  F                   PIC  X(07)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFFFFFF   OCCURS 3.
               03  P2-03               PIC  9(01).
               03  F                   PIC  X(07)  VALUE  SPACE.
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(17) VALUE
               "ïîÅ@ñÂÅ@ñºÅ@É}Å@ÉXÅ@É^Å@ÉäÅ@ÉXÅ@Ég".
           02  F                   PIC  X(36) VALUE  SPACE.
           02  M-YY                PIC  Z9.
           02  F                   PIC  N(01) VALUE  "îN".
           02  M-MM                PIC  Z9.
           02  F                   PIC  N(01) VALUE  "åé".
           02  M-DD                PIC  Z9.
           02  F                   PIC  N(03) VALUE  "ì˙çÏê¨".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  M-PCNT              PIC  ZZZZ9.
           02  F                   PIC  N(01) VALUE  "ï≈".
      *
       01  MID-02.
           02  F                   PIC  N(03) VALUE  "ïîÅ@ñÂ".
           02  F                   PIC  X(06) VALUE  SPACE.
           02  F                   PIC  X(12) VALUE  "ïî Å@ñÂ   ñº".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(02) VALUE  "ïîñÂ".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  X(08) VALUE  "<------ ".
           02  F                   PIC  N(03) VALUE  "ëπâvóp".
           02  F                   PIC  X(08) VALUE  " ------>".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  X(20) VALUE
               "<------------------ ".
           02  F                   PIC  N(03) VALUE  "åoîÔóp".
           02  F                   PIC  X(20) VALUE
               " ------------------>".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  X(08) VALUE  "<------ ".
           02  F                   PIC  N(03) VALUE  "êªë¢óp".
           02  F                   PIC  X(08) VALUE  " ------>".
      *
       01  MID-03.
           02  F                   PIC  N(03) VALUE  "ÉRÅ[Éh".
           02  F                   PIC  X(23) VALUE  SPACE.
           02  F                   PIC  N(02) VALUE  "ãÊï™".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "ñæÅ@ç◊".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåvá@".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåváA".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "ñæÅ@ç◊".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåvá@".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåváA".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåváB".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåváC".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåváD".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "ñæÅ@ç◊".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåvá@".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "çáåváA".
      ***
           COPY  LWMSG_PR.
      ***  ïîñÂñºÉ}ÉXÉ^
           COPY  BUMONF.
      ***  ÉvÉäÉìÉ^Å[
      *       FD  PRINTF
       77  PRINTR                  PIC  X(250).
      **
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
           03  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(001) VALUE " ".
      *******************
      *    âÊñ ï\é¶     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC X(020) VALUE
               " ïîñÂñºÉ}ÉXÉ^ÉäÉXÉg ".
           03  FILLER  PIC X(008) VALUE  "ÇeÇqÇnÇl".
           03  FILLER  PIC X(004) VALUE  "ÇsÇn".
           03  FILLER  PIC X(010) VALUE  "ïîñÂÉRÅ[Éh".
           03  FILLER  PIC X(002) VALUE  "Å`".
           03  FILLER  PIC X(018)
               VALUE  "ämîF OK=1,NO=9 ( )".
      ***********************
      *    âÊñ ì¸óÕ         *
      ***********************
       01  ACP-AREA.
           03  ACP-BUMONCD-FROM    PIC 9(04).
           03  ACP-BUMONCD-TO      PIC 9(04).
           03  ACP-KAKU            PIC X(01).
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
            "CLR-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "6" "33" "4" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "51" "4" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "24" "77" "1" "02CLR-AREA" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "RN" "1" "31" "20" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "X" "4" "31" "8" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA2" "X" "4" "51" "4" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA2" "X" "6" "11" "10" "03CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA2" "X" "6" "43" "2" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA2" "X" "24" "61" "18" "05CLR-AREA2" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUMONCD-FROM" "9" "6" "33" "4" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUMONCD-FROM" BY REFERENCE W-BUMONCD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUMONCD-TO" "9" "6" "51" "4" "ACP-BUMONCD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUMONCD-TO" BY REFERENCE W-BUMONCD-TO "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-BUMONCD-TO" " "
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
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           MOVE  90     TO  LCNT.
       INI-EX.
           EXIT.
      *****************************
      *    ÇlÇ`ÇhÇmÅ@èàóùÅ@Å@Å@Å@ *
      *****************************
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-BUMONCD-FROM "ACP-BUMONCD-FROM"
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
                 BY REFERENCE ACP-BUMONCD-TO "ACP-BUMONCD-TO" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-BUMONCD-TO = ZERO
               MOVE  ALL "9"     TO  W-BUMONCD-TO
           END-IF.
           IF  W-BUMONCD-FROM > W-BUMONCD-TO
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
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
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ÇkÇrÇsÅ|ÇqÇsÇm      *
      **************************
       LST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-BUMONCD-FROM     TO  BNM-KEY.
      *           START  BNM  KEY  NOT < BNM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            BNM_PNAME1 "BNM-KEY" " NOT < " BNM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                             RETURNING RESU
      **  ÉfÅ[É^ñ¢ìoò^Å@ï\é¶
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      **
      ***  ïîñÂñºÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
       LST-010.
      *           READ  BNM  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-999.
           IF  BNM-KEY > W-BUMONCD-TO
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
           MOVE  MID-03 TO PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  5     TO  LCNT.
       MID-EX.
           EXIT.
      ***************************
      *    ÇlÇdÇhÅ|ÇqÇsÇm Å@Å@  *
      ***************************
       MEI-RTN.
           MOVE  C2               TO  C-2B.
           MOVE  BNM-KEY          TO  P1-01.
           MOVE  BNMNMN           TO  P1-02.
           MOVE  BNM-BUMONKBN     TO  P1-03.
      ***  ï≈  ************************************
           MOVE  BNM-PLPG(1)     TO  P1-04(1).
           MOVE  BNM-PLPG(2)     TO  P1-04(2).
           MOVE  BNM-PLPG(3)     TO  P1-04(3).
      *
           MOVE  1     TO  I.
       MEI-030.
           IF  I NOT < 7
               GO  TO  MEI-040
           END-IF.
           MOVE  BNM-KHPG(I)     TO  P1-05(I).
           ADD  1     TO  I.
           GO  TO  MEI-030.
       MEI-040.
           MOVE  BNM-GNPG(1)     TO  P1-06(1).
           MOVE  BNM-GNPG(2)     TO  P1-06(2).
           MOVE  BNM-GNPG(3)     TO  P1-06(3).
           MOVE  PRINTR1 TO PRINTR.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE            TO  PRINTR.
      *
      ***  óÒ  ************************************
           MOVE  BNM-PLLN(1)     TO  P2-01(1).
           MOVE  BNM-PLLN(2)     TO  P2-01(2).
           MOVE  BNM-PLLN(3)     TO  P2-01(3).
      *
           MOVE  1     TO  I.
       MEI-010.
           IF  I NOT < 7
               GO  TO  MEI-020
           END-IF.
           MOVE  BNM-KHLN(I)     TO  P2-02(I).
           ADD  1     TO  I.
           GO  TO  MEI-010.
       MEI-020.
           MOVE  BNM-GNLN(1)     TO  P2-03(1).
           MOVE  BNM-GNLN(2)     TO  P2-03(2).
           MOVE  BNM-GNLN(3)     TO  P2-03(3).
      *           WRITE  PRINTR  FROM  PRINTR2  AFTER 1.
           MOVE  PRINTR2         TO  PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE            TO  PRINTR.
           ADD  3     TO  LCNT.
       MEI-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
