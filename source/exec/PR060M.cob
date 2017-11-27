       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR060M.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  ïîñÂñºÉ}ÉXÉ^ÉÅÉìÉeÉiÉìÉX      *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/16                      *
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
       77  I                       PIC  9(01).
       77  COL_L                   PIC  9(02).
       01  W-SPACE                 PIC  N(10)  VALUE
                                   "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@".
      ***  ANKÇÃSPACEÇ™ì¸ÇÈÇ∆Å@Ç¢ÇØÇ»Ç¢Ç©ÇÁ
       01  W-AREA.
           02  W-AREA1.
               03  W-ACT           PIC  9(01).                          ACT
               03  W-AREA2.
                   04  W-CD        PIC  9(04).                          ïîñÂÇbÇc
                   04  W-AREA3.
                       05  W-KBN       PIC  9(01).                      ïîñÂãÊï™
                       05  W-MEI       PIC  N(10).                      ïîñÂñº
                       05  W-KAKU      PIC  X(01).                      ämîF
                       05  W-AREA4.
                           06  W-AREA5     OCCURS 3.                    ëπâvóp
                               07  W-10A    PIC  9(02).                 ï≈
                               07  W-10B    PIC  9(01).                 óÒ
                       05  W-AREA6.
                           06  W-AREA7     OCCURS 6.                    åoîÔóp
                               07  W-20A    PIC  9(02).                 ï≈
                               07  W-20B    PIC  9(01).                 óÒ
                       05  W-AREA8.
                           06  W-AREA9     OCCURS 3.                    êªë¢óp
                               07  W-30A    PIC  9(02).                 ï≈
                               07  W-30B    PIC  9(01).                 óÒ
      ***
      ***************************************
      *    ¥◊∞ DISPLAY (‹∞∏)                * 
      ***************************************
       01  DISP-ERR-WORK.
           02  DISP-MSG.
               03  ERR-MSGX.
                   04  ERR-MSGN     PIC N(25).
               03  ERR-SPACE        PIC X(50).
               03  ERR-F            PIC X(12).
               03  ERR-M            PIC X(01).
               03  ERR-K            PIC X(30).
               03  ERR-FLG          PIC X(02).
      *******************************
      *    ÉvÉäÉìÉ^áÇïœçXÉèÅ[ÉN     *
      *******************************
       01  ASNPRN.
           03  ASNPRN1              PIC  X(03)   VALUE  "PRN".
           03  ASNPRN2              PIC  9(03).
       01  PMEDIA                   PIC  X(06)   VALUE  SPACE.
      *******************************
      *    äYìñåééÊçûÇ›èàóùÉèÅ[ÉN   *
      *******************************
       01  ZYMD                     PIC  9(08). 
       01  ZI                       PIC  9(02).
       01  Z-R.
           02  Z-KEY1               PIC  X(06).
           02  Z-KSMM               PIC  9(02).
           02  Z-KONYMD.
               03  Z-KONYY          PIC  9(04). 
               03  Z-KONYYL  REDEFINES Z-KONYY. 
                 04  Z-KONYY1       PIC  9(02). 
                 04  Z-KONYY2       PIC  9(02). 
               03  Z-KONMM          PIC  9(02).
               03  Z-KONDD          PIC  9(02).
           02  Z-ZENYMD.
               03  Z-ZENYY          PIC  9(04). 
               03  Z-ZENMM          PIC  9(02).
               03  Z-ZENDD          PIC  9(02).
           02  Z-GESYMD.
               03  Z-GESYY          PIC  9(04). 
               03  Z-GESYYL  REDEFINES Z-GESYY. 
                 04  Z-GESYY1       PIC  9(02). 
                 04  Z-GESYY2       PIC  9(02). 
               03  Z-GESMM          PIC  9(02).
               03  Z-GESDD          PIC  9(02).
           02  Z-GEMYMD.
               03  Z-GEMYY          PIC  9(04). 
               03  Z-GEMYYL  REDEFINES Z-GEMYY. 
                 04  Z-GEMYY1       PIC  9(02). 
                 04  Z-GEMYY2       PIC  9(02). 
               03  Z-GEMMM          PIC  9(02).
               03  Z-GEMDD          PIC  9(02).
           02  Z-ACEPSIN            PIC  9(01).
           02  Z-TOUKI.
             03  Z-TOU     OCCURS 15.
               04  Z-TOUF.
                 05  Z-TOUFYY       PIC  9(04). 
                 05  Z-TOUFYYL  REDEFINES Z-TOUFYY. 
                   06  Z-TOUFYY1    PIC  9(02). 
                   06  Z-TOUFYY2    PIC  9(02). 
                 05  Z-TOUFMM       PIC  9(02).
                 05  Z-TOUFDD       PIC  9(02).
               04  Z-TOUT.
                 05  Z-TOUTYY       PIC  9(04). 
                 05  Z-TOUTYYL  REDEFINES Z-TOUTYY. 
                   06  Z-TOUTYY1    PIC  9(02). 
                   06  Z-TOUTYY2    PIC  9(02). 
                 05  Z-TOUTMM       PIC  9(02).
                 05  Z-TOUTDD       PIC  9(02).
           02  Z-UPDYM.
             03  Z-UPDYY            PIC  9(04). 
             03  Z-UPDMM            PIC  9(02).
           02  Z-SIMEBI             PIC  9(02).
           02  FILLER               PIC  X(223). 
      ***  ïîñÂñºÉ}ÉXÉ^
           COPY  BUMONF.
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
               03  FILLER  PIC X(004) VALUE "    ".                     ïîñÂÇbÇc
               03  CLR-AREA3.
                   04  FILLER  PIC X(001) VALUE " ".                    ïîñÂãÊï™
                   04  CLR-BUMONMEI   PIC N(10).                        ïîñÂñº
                   04  FILLER  PIC X(002) VALUE "  ".                   ëπâvóp
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(002) VALUE "  ".                   åoîÔóp
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(002) VALUE "  ".                   êªë¢óp
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(002) VALUE "  ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".
                   04  FILLER  PIC X(001) VALUE " ".                    ämîF
      ***************************
      *    âÊñ ì¸óÕçÄñ⁄         *
      ***************************
       01  ACP-AREA.
           03  ACP-ACT         PIC 9(01).                               ACT
           03  ACP-CD          PIC 9(04).                               ïîñÂÇbÇc
           03  ACP-KBN         PIC 9(01).                               ïîñÂãÊï™
           03  ACP-MEI         PIC N(10).                               ïîñÂñº
           03  ACP-10A         PIC 9(02).                               ëπâvóp
           03  ACP-10B         PIC 9(01).                               ëπâvóp
           03  ACP-20A         PIC 9(02).                               åoîÔóp
           03  ACP-20B         PIC 9(01).                               åoîÔóp
           03  ACP-30A         PIC 9(02).                               êªë¢óp
           03  ACP-30B         PIC 9(01).                               êªë¢óp
           03  ACP-KAKU        PIC X(01).                               ämîF
      *********************
      *    âÊñ ï\é¶       *
      *********************
       01  DSP-DSP.
           03  DSP-10A         PIC Z9.                                  ëπâvóp
           03  DSP-20A         PIC Z9.                                  åoîÔóp
           03  DSP-30A         PIC Z9.                                  êªë¢óp
      ***
      **
      **   MESSEGE  AREA
      **
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(50).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(50).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4210"..
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@É}ÉXÉ^Å@ìoò^çœÅ@Åñ".
           02  NOR-D01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@ÉfÅ[É^Å@ìoò^çœÅ@Åñ".
           02  INV-M01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@É}ÉXÉ^Å@ñ¢ìoò^Å@Åñ".
           02  INV-D01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@ÉfÅ[É^Å@ñ¢ìoò^Å@Åñ".
           02  OK-01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(14) VALUE
               "ÅñÅ@ÇnÅ@ÇjÅ@Åñ".
           02  CAN-01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(18) VALUE
               "ÅñÅ@ÉLÉÉÉìÉZÉãÅ@Åñ".
           02  ERR-01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(18) VALUE
               "ÅñÅ@ì¸óÕÉGÉâÅ[Å@Åñ".
           02  INV-MCT.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(28) VALUE
               "ÅñÅ@ÉRÉìÉgÉçÅ[ÉãÇlñ¢ìoò^Å@Åñ".
           02  INV-CON.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(42) VALUE
               "ÅñÅ@ÉRÉìÉgÉçÅ[ÉãÇeñ¢ìoò^Å@èàóùë±çsïsâ¬Å@Åñ".
           02  ERR-YMD.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@ì˙ïtì¸óÕÉGÉâÅ[Å@Åñ".
           02  ERR-DIS.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(05) VALUE
               "<<<  ".
               03  FILLER  PIC X(12).
               03  FILLER  PIC X(01).
               03  FILLER  PIC X(11) VALUE
               "¥◊∞ STATUS=".
               03  FILLER  PIC X(02).
               03  FILLER  PIC X(05) VALUE
               "  >>>".
               03  FILLER  PIC X(05) VALUE
               " KEY=".
               03  FILLER  PIC X(30).
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
            "CLR-AREA1" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA1" "X" "3" "67" "1" " " "CLR-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-AREA2" " " "3" "0" "62" "01CLR-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA2" "X" "5" "34" "4" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-AREA3" " " "5" "0" "58" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA3" "X" "6" "37" "1" " " "CLR-AREA3"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-BUMONMEI" "N" "6" "56" "20" "01CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLR-BUMONMEI" BY REFERENCE W-SPACE "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-AREA3" "X" "10" "25" "2" "CLR-BUMONMEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-AREA3" "X" "10" "32" "2" "03CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-AREA3" "X" "10" "39" "2" "04CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLR-AREA3" "X" "11" "26" "1" "05CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07CLR-AREA3" "X" "11" "33" "1" "06CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08CLR-AREA3" "X" "11" "40" "1" "07CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09CLR-AREA3" "X" "15" "25" "2" "08CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10CLR-AREA3" "X" "15" "32" "2" "09CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11CLR-AREA3" "X" "15" "39" "2" "10CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12CLR-AREA3" "X" "16" "26" "1" "11CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13CLR-AREA3" "X" "16" "33" "1" "12CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "14CLR-AREA3" "X" "16" "40" "1" "13CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "15CLR-AREA3" "X" "15" "46" "2" "14CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "16CLR-AREA3" "X" "15" "53" "2" "15CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "17CLR-AREA3" "X" "15" "60" "2" "16CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "18CLR-AREA3" "X" "16" "47" "1" "17CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "19CLR-AREA3" "X" "16" "54" "1" "18CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "20CLR-AREA3" "X" "16" "61" "1" "19CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "21CLR-AREA3" "X" "20" "25" "2" "20CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "22CLR-AREA3" "X" "20" "32" "2" "21CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "23CLR-AREA3" "X" "20" "39" "2" "22CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "24CLR-AREA3" "X" "21" "26" "1" "23CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "25CLR-AREA3" "X" "21" "33" "1" "24CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "26CLR-AREA3" "X" "21" "40" "1" "25CLR-AREA3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "27CLR-AREA3" "X" "24" "77" "1" "26CLR-AREA3" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CD" "9" "5" "34" "4" "ACP-ACT" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-CD" BY REFERENCE W-CD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KBN" "9" "6" "37" "1" "ACP-CD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KBN" BY REFERENCE W-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-MEI" "N" "6" "56" "20" "ACP-KBN" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-MEI" BY REFERENCE W-MEI "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-10A" "9" "10" "COL_L" "2" "ACP-MEI" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-10A" BY REFERENCE W-10A(1) "2" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-10B" "9" "11" "COL_L PLUS 1" "1" "ACP-10A" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-10B" BY REFERENCE W-10B(1) "1" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-20A" "9" "15" "COL_L" "2" "ACP-10B" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-20A" BY REFERENCE W-20A(1) "2" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-20B" "9" "16" "COL_L PLUS 1" "1" "ACP-20A" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-20B" BY REFERENCE W-20B(1) "1" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-30A" "9" "20" "COL_L" "2" "ACP-20B" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-30A" BY REFERENCE W-30A(1) "2" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-30B" "9" "21" "COL_L PLUS 1" "1" "ACP-30A" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-30B" BY REFERENCE W-30B(1) "1" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-30B" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *DSP-DSP
       CALL "SD_Init" USING
            "DSP-DSP" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-10A" "Z9" "10" "COL_L" "2" " " "DSP-DSP"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-10A" BY REFERENCE W-10A(1) "2" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-20A" "Z9" "15" "COL_L" "2" "DSP-10A" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-20A" BY REFERENCE W-20A(1) "2" "1"
            BY REFERENCE I 3 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-30A" "Z9" "20" "COL_L" "2" "DSP-20A" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-30A" BY REFERENCE W-30A(1) "2" "1"
            BY REFERENCE I 3 RETURNING RESU.
      *
      *DISP-ERR-AREA
       CALL "SD_Init" USING
            "DISP-ERR-AREA" " " "24" "0" "961" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-MSG-01" " " "24" "0" "50" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-MSG-01" "X" "24" "2" "50" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-MSG-01" BY REFERENCE ERR-MSGX "50" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-MSG-SPACE" " " "24" "0" "50" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-MSG-SPACE" "X" "24" "2" "50" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "50" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-BUZ-B" "X" "24" "80" "5" " " "DISP-BUZ-B"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING
            "NOR-M01" " " "24" "0" "72" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01NOR-M01" "X" "24" "2" "50" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_From" USING
            "01NOR-M01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02NOR-M01" "X" "24" "2" "22" "01NOR-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "NOR-D01" " " "24" "0" "72" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01NOR-D01" "X" "24" "2" "50" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_From" USING
            "01NOR-D01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02NOR-D01" "X" "24" "2" "22" "01NOR-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "INV-M01" " " "24" "0" "72" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-M01" "X" "24" "2" "50" " " "INV-M01" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-M01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-M01" "X" "24" "2" "22" "01INV-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "INV-D01" " " "24" "0" "72" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-D01" "X" "24" "2" "50" " " "INV-D01" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-D01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-D01" "X" "24" "2" "22" "01INV-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "OK-01" " " "24" "0" "64" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01OK-01" "X" "24" "2" "50" " " "OK-01" RETURNING RESU.
       CALL "SD_From" USING
            "01OK-01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02OK-01" "X" "24" "2" "14" "01OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CAN-01" " " "24" "0" "68" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CAN-01" "X" "24" "2" "50" " " "CAN-01" RETURNING RESU.
       CALL "SD_From" USING
            "01CAN-01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02CAN-01" "X" "24" "2" "18" "01CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-01" " " "24" "0" "68" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01ERR-01" "X" "24" "2" "50" " " "ERR-01" RETURNING RESU.
       CALL "SD_From" USING
            "01ERR-01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02ERR-01" "X" "24" "2" "18" "01ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "INV-MCT" " " "24" "0" "78" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-MCT" "X" "24" "2" "50" " " "INV-MCT" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-MCT" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-MCT" "X" "24" "2" "28" "01INV-MCT" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "INV-CON" " " "24" "0" "92" "INV-MCT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-CON" "X" "24" "2" "50" " " "INV-CON" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-CON" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-CON" "X" "24" "2" "42" "01INV-CON" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-YMD" " " "24" "0" "72" "INV-CON" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01ERR-YMD" "X" "24" "2" "50" " " "ERR-YMD" RETURNING RESU.
       CALL "SD_From" USING
            "01ERR-YMD" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02ERR-YMD" "X" "24" "2" "22" "01ERR-YMD" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-DIS" " " "24" "0" "121" "ERR-YMD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01ERR-DIS" "X" "24" "2" "50" " " "ERR-DIS" RETURNING RESU.
       CALL "SD_From" USING
            "01ERR-DIS" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02ERR-DIS" "X" "24" "2" "5" "01ERR-DIS" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03ERR-DIS" "X" "24" "7" "12" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04ERR-DIS" "X" "24" "19" "1" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "05ERR-DIS" "X" "24" "20" "11" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06ERR-DIS" "X" "24" "31" "2" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "07ERR-DIS" "X" "24" "33" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08ERR-DIS" "X" "24" "38" "5" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09ERR-DIS" "X" "24" "43" "30" "08ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "09ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
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
           CALL "SD_Output" USING "CLR-BUMONMEI" CLR-BUMONMEI "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0600" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
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
                 BY REFERENCE ACP-CD "ACP-CD" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           CALL "SD_Output" USING "CLR-AREA3" CLR-AREA3 "p"
                                  RETURNING RESU.
           INITIALIZE  W-AREA3.
           MOVE  W-CD       TO  BNM-KEY.
      ***  ïîñÂñºÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  BNM  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BNM_PNAME1 BY REFERENCE BNM-REC " " RETURNING RET.
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
               PERFORM  DSP-RTN     THRU  DSP-EX
               IF  W-ACT = 3
                   GO  TO  MAIN-150
               END-IF
           END-IF.
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KBN "ACP-KBN" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-040
           END-IF.
       MAIN-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-MEI "ACP-MEI" "N" "20"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-040
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-050
           END-IF.
           CALL "SD_Output" USING "ACP-MEI" ACP-MEI "p"
                                  RETURNING RESU.
       MAIN-060.
           MOVE  1     TO  I.
           MOVE  25    TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
       MAIN-070.
           IF  I NOT < 4
               GO  TO  MAIN-090
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-10A "ACP-10A" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   GO  TO  MAIN-050
               ELSE
                   SUBTRACT  1     FROM  I
                   SUBTRACT  7     FROM  COL_L
                   CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
                    RETURNING RESU
                   GO  TO  MAIN-070
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-070
           END-IF.
           CALL "SD_Output" USING "DSP-10A" DSP-10A "p"
                                  RETURNING RESU.
       MAIN-080.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-10B "ACP-10B" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-070
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-080
           END-IF.
           CALL "SD_Output" USING "ACP-10B" ACP-10B "p"
                                  RETURNING RESU.
           ADD  1     TO  I.
           ADD  7     TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
           GO  TO  MAIN-070.
       MAIN-090.
           MOVE  1     TO  I.
           MOVE  25    TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
       MAIN-100.
           IF  I NOT < 7
               GO  TO  MAIN-120
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-20A "ACP-20A" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   MOVE  3     TO  I
                   MOVE  39    TO  COL_L
                   CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
                    RETURNING RESU
                   GO  TO  MAIN-070
               ELSE
                   SUBTRACT  1     FROM  I
                   SUBTRACT  7     FROM  COL_L
                   CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
                    RETURNING RESU
                   GO  TO  MAIN-100
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-100
           END-IF.
           CALL "SD_Output" USING "DSP-20A" DSP-20A "p"
                                  RETURNING RESU.
       MAIN-110.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-20B "ACP-20B" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-100
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-110
           END-IF.
           CALL "SD_Output" USING "ACP-20B" ACP-20B "p"
                                    RETURNING RESU.
           ADD  1     TO  I.
           ADD  7     TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
           GO  TO  MAIN-100.
       MAIN-120.
           MOVE  1     TO  I.
           MOVE  25    TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
       MAIN-130.
           IF  I NOT < 4
               GO  TO  MAIN-150
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-30A "ACP-30A" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  I = 1
                   MOVE  6     TO  I
                   MOVE  60    TO  COL_L
                   CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
                    RETURNING RESU
                   GO  TO  MAIN-100
               ELSE
                   SUBTRACT  1     FROM  I
                   SUBTRACT  7     FROM  COL_L
                   CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
                    RETURNING RESU
                   GO  TO  MAIN-130
               END-IF
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-130
           END-IF.
           CALL "SD_Output" USING "DSP-30A" DSP-30A "p"
                                  RETURNING RESU.
       MAIN-140.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-30B "ACP-30B" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-130
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-140
           END-IF.
           ADD  1     TO  I.
           ADD  7     TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
           GO  TO  MAIN-130.
       MAIN-150.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               IF  W-ACT = 3
                   GO  TO  MAIN-010
               ELSE
                   MOVE  3     TO  I
                   MOVE  39    TO  COL_L
                   CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
                    RETURNING RESU
                   GO  TO  MAIN-130
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
               GO  TO  MAIN-150
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
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ÇvÇnÇqÇjÅ|ÇqÇsÇm    *
      **************************
       WORK-RTN.
           MOVE  BNM-BUMONKBN     TO  W-KBN.
           MOVE  BNMNMN           TO  W-MEI.
           MOVE  1     TO  I.
       WORK-010.
           IF  I NOT < 4
               GO  TO  WORK-020
           END-IF.
           MOVE  BNM-PLPG(I)  TO  W-10A(I).
           MOVE  BNM-PLLN(I)  TO  W-10B(I).
           MOVE  BNM-GNPG(I)  TO  W-30A(I).
           MOVE  BNM-GNLN(I)  TO  W-30B(I).
           ADD  1     TO  I.
           GO  TO  WORK-010.
       WORK-020.
           MOVE  1     TO  I.
       WORK-030.
           IF  I NOT < 7
               GO  TO  WORK-EX
           END-IF.
           MOVE  BNM-KHPG(I)  TO  W-20A(I).
           MOVE  BNM-KHLN(I)  TO  W-20B(I).
           ADD  1     TO  I.
           GO  TO  WORK-030.
       WORK-EX.
           EXIT.
      ************************
      *    ÇcÇrÇoÅ|ÇqÇsÇm    *
      ************************
       DSP-RTN.
           CALL "SD_Output" USING "ACP-KBN" ACP-KBN "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-MEI" ACP-MEI "p"
                                  RETURNING RESU.
           MOVE  1     TO  I.
           MOVE  25    TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
       DSP-010.
           IF  I NOT < 4
               GO  TO  DSP-020
           END-IF.
           CALL "SD_Output" USING "DSP-10A" DSP-10A "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-10B" ACP-10B "p"
                                  RETURNING RESU.
           ADD  1     TO  I.
           ADD  7     TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
           GO  TO  DSP-010.
       DSP-020.
           MOVE  1     TO  I.
           MOVE  25    TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
       DSP-030.
           IF  I NOT < 7
               GO  TO  DSP-040
           END-IF.
           CALL "SD_Output" USING "DSP-20A" DSP-20A "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-20B" ACP-20B "p"
                                  RETURNING RESU.
           ADD  1     TO  I.
           ADD  7     TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
           GO  TO  DSP-030.
       DSP-040.
           MOVE  1     TO  I.
           MOVE  25    TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
       DSP-050.
           IF  I NOT < 4
               GO  TO  DSP-EX
           END-IF.
           CALL "SD_Output" USING "DSP-30A" DSP-30A "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-30B" ACP-30B "p"
                                  RETURNING RESU.
           ADD  1     TO  I.
           ADD  7     TO  COL_L.
           CALL "SD_Arg_Match_Col" USING "COL_L" "2" COL_L 
            RETURNING RESU.
           GO  TO  DSP-050.
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
           MOVE  SPACE     TO  BNM-REC.
           INITIALIZE  BNM-REC.
           MOVE  W-CD           TO  BNM-KEY.
           MOVE  W-KBN          TO  BNM-BUMONKBN.
           MOVE  W-MEI          TO  BNMNMN.
           MOVE  W-CD           TO  ERR-K.
       WRITE-010.
           MOVE  1     TO  I.
       WRITE-020.
           IF  I NOT < 4
               GO  TO  WRITE-030
           END-IF.
           MOVE  W-10A(I)     TO  BNM-PLPG(I).
           MOVE  W-10B(I)     TO  BNM-PLLN(I).
           MOVE  W-30A(I)     TO  BNM-GNPG(I).
           MOVE  W-30B(I)     TO  BNM-GNLN(I).
           ADD  1     TO  I.
           GO  TO  WRITE-020.
       WRITE-030.
           MOVE  1     TO  I.
       WRITE-040.
           IF  I NOT < 7
               GO  TO  WRITE-050
           END-IF.
           MOVE  W-20A(I)     TO  BNM-KHPG(I).
           MOVE  W-20B(I)     TO  BNM-KHLN(I).
           ADD  1     TO  I.
           GO  TO  WRITE-040.
       WRITE-050.
      *           WRITE  BNM-REC  INVALID
      *///////////////
           CALL "DB_Insert" USING
            BNM_PNAME1 BNM_LNAME BNM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "BNM"      TO  ERR-F
               MOVE  "W"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      **********************************
      *    ÇqÇdÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      **********************************
       REWRITE-RTN.
           MOVE  W-CD           TO  BNM-KEY.
           MOVE  W-KBN          TO  BNM-BUMONKBN.
           MOVE  W-MEI          TO  BNMNMN.
           MOVE  W-CD           TO  ERR-K.
       REWRITE-010.
           MOVE  1     TO  I.
       REWRITE-020.
           IF  I NOT < 4
               GO  TO  REWRITE-030
           END-IF.
           MOVE  W-10A(I)     TO  BNM-PLPG(I).
           MOVE  W-10B(I)     TO  BNM-PLLN(I).
           MOVE  W-30A(I)     TO  BNM-GNPG(I).
           MOVE  W-30B(I)     TO  BNM-GNLN(I).
           ADD  1     TO  I.
           GO  TO  REWRITE-020.
       REWRITE-030.
           MOVE  1     TO  I.
       REWRITE-040.
           IF  I NOT < 7
               GO  TO  REWRITE-050
           END-IF.
           MOVE  W-20A(I)     TO  BNM-KHPG(I).
           MOVE  W-20B(I)     TO  BNM-KHLN(I).
           ADD  1     TO  I.
           GO  TO  REWRITE-040.
       REWRITE-050.
      *           REWRITE  BNM-REC  INVALID
      *///////////////
           CALL "DB_Update" USING
            BNM_PNAME1 BNM_LNAME BNM-REC RETURNING RET.
           IF  RET = 1
               MOVE  "BNM"      TO  ERR-F
               MOVE  "R"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       REWRITE-EX.
           EXIT.
      *******************************
      *    ÇcÇdÇkÇdÇsÇdÅ|ÇqÇsÇm     *
      *******************************
       DELETE-RTN.
           MOVE  W-CD           TO  BNM-KEY.
      *           DELETE  BNM  INVALID
      *///////////////
           CALL "DB_Delete" USING BNM_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "BNM"      TO  ERR-F
               MOVE  "D"      TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       DELETE-EX.
           EXIT.
      **
      *****************************
      *    ¥◊∞ DISPLAY (“≤›)      *
      *****************************
       ERR-ENT.
           MOVE    ERR-STAT  TO  ERR-FLG.
           PERFORM CLSE-ENT THRU CLSE-EXT.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-010.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
           "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           GO TO ERR-010.
       ERR-EXT.
           EXIT.
      *****************************
      *    äYìñåééÊçûÇ›èàóù       *
      *****************************
       Z-RTN.
           MOVE    1         TO  ZI.
       Z-010.
           IF  ZI  >  15
               MOVE  99      TO  ZI
               GO    TO      Z-EXT
           END-IF.
           IF  Z-TOUF(ZI)  >  ZYMD
               ADD   1       TO  ZI
               GO    TO      Z-010
           END-IF.
           IF  Z-TOUT(ZI)  <  ZYMD
               ADD   1       TO  ZI
               GO    TO      Z-010
           END-IF.
       Z-EXT.
           EXIT.
