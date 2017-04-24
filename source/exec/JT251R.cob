       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JT250R.
       AUTHOR.               I.N.
      **************************************************
      *    PROGRAM      :    ëqï ç›å…ñ‚çáÇπ            *
      *    DATA WRITTEN :    91/10/30                  *
      *    SCREEN  USED :    SJ251R                    *
      *    COMPILE TYPE :    CBL85                     *
      *    JS-SIGN      :    0:ALLéÛî≠íç  , 1:éÛî≠íçì˙ïtëIë *
      *    W-JS         :    0:ëSé–       , 1:ëÅìá     *
      **************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT             PIC  X(02).
       77  JS-SIGN              PIC  9(01).
       77  W-JS                 PIC  9(01).
       01  I                    PIC  9(02).
       01  P                    PIC  9(01).
       01  O                    PIC  9(01).
       01  LIN1                 PIC  9(02).
       01  SW                   PIC  9(01).
       01  WORK-AREA.
           02  W-ACT            PIC  9(01).
           02  W-SCD            PIC  9(01).
           02  W-HIN            PIC  9(06).
           02  W-MID.
               03  W-M0         PIC  N(03).
               03  W-M1         PIC  N(03).
               03  W-M2         PIC  N(03).
               03  W-M3         PIC  N(03).
               03  W-M4         PIC  N(03).
           02  WK-HCD           PIC  9(06).
           02  O-HIN            PIC  9(06).
           02  W-DATE           PIC  9(08).
           02  W-NGP   REDEFINES W-DATE.
             03  W-NEN          PIC  9(04).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1       PIC  9(02).
               04  W-NEN2       PIC  9(02).
             03  W-GET          PIC  9(02).
             03  W-PEY          PIC  9(02).
           02  W-DC             PIC  9(01).
       01  WORK-MEISAI.
           02  AZU-SW           PIC  9(01).
           02  NYZ-SW           PIC  9(01).
           02  W-KEI.
               03  W-KEIZ       PIC S9(06).
               03  W-KEIA       PIC S9(06).
               03  W-KEIB       PIC S9(06).
               03  W-KEIC       PIC S9(06).
               03  W-KEID       PIC S9(06).
           02  W-MEISAI.
               03  W-MEI-T4     OCCURS  4.
                   04  W-MEI.
                       05  W-M1Z   PIC S9(06)  OCCURS  10.
                       05  W-M1A   PIC S9(06)  OCCURS  10.
                       05  W-M1B   PIC S9(06)  OCCURS  10.
                       05  W-M1C   PIC S9(06)  OCCURS  10.
                       05  W-M1D   PIC S9(06)  OCCURS  10.
           02  WK-ZEN.
               03  WK-A-T4      OCCURS  4.
                   04  WK-A     PIC S9(06)  OCCURS  10.
           02  WK-NYU.
               03  WK-B-T4      OCCURS  4.
                   04  WK-B     PIC S9(06)  OCCURS  10.
           02  WK-SYU.
               03  WK-C-T4      OCCURS  4.
                   04  WK-C     PIC S9(06)  OCCURS  10.
           02  WK-SYO.
               03  WK-D-T4      OCCURS  4.
                   04  WK-D     PIC S9(06)  OCCURS  10.
           02  WK-AZU.
               03  WK-E-T4      OCCURS  4.
                   04  WK-E     PIC S9(06)  OCCURS  10.
           02  WK-SYY.
               03  WK-F-T4      OCCURS  4.
                   04  WK-F     PIC S9(06)  OCCURS  10.
           02  WK-NYY.
               03  WK-G-T4      OCCURS  4.
                   04  WK-G     PIC S9(06)  OCCURS  10.
           02  WK-YNU.
               03  WK-H-T4      OCCURS  4.
                   04  WK-H     PIC S9(06)  OCCURS  10.
           02  WK-SSI.
               03  WK-K-T4      OCCURS  4.
                   04  WK-K     PIC S9(06)  OCCURS  10.
           02  WK-GNZ.
               03  WK-L-T4      OCCURS  4.
                   04  WK-L     PIC S9(06)  OCCURS  10.
      *
           COPY     LWMSG.
      *
           COPY     LIBFDD.
           COPY     LNJZAI.
           COPY     LIHIM2.
           COPY     LJMST3.
           COPY     L-JCON.
           COPY     LJNYZ.
           COPY     LIHSHF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-ACT     PIC  9(01).
           02  ACP-HIN     PIC  9(06).
           02  ACP-SCD     PIC  9(01).
           02  FILLER.
             03  ACP-NEN   PIC  9(02).
             03  ACP-GET   PIC  9(02).
             03  ACP-PEY   PIC  9(02).
       01  DSP-AREA.
           02  DSP-HINNM  PIC  N(24).
           02  DSP-SNM    PIC  N(06).
           02  DSP-MID1.
               03  DSP-M10      PIC  N(03).
               03  DSP-M11      PIC  N(03).
               03  DSP-M12      PIC  N(03).
               03  DSP-M13      PIC  N(03).
               03  DSP-M14      PIC  N(03).
           02  DSP-MID2.
               03  DSP-M20      PIC  N(03).
               03  DSP-M21      PIC  N(03).
               03  DSP-M22      PIC  N(03).
               03  DSP-M23      PIC  N(03).
               03  DSP-M24      PIC  N(03).
           02  DSP-MEISAI.
               03  DSP-MEI1.
                   04  DSP-MLZ   PIC  ------- .
                   04  DSP-MLA   PIC  ------- .
                   04  DSP-MLB   PIC  ------- .
                   04  DSP-MLC   PIC  ------- .
                   04  DSP-MLD   PIC  ------- .
               03  DSP-MEI2.
                   04  DSP-MRZ   PIC  ------- .
                   04  DSP-MRA   PIC  ------- .
                   04  DSP-MRB   PIC  ------- .
                   04  DSP-MRC   PIC  ------- .
                   04  DSP-MRD   PIC  ------- .
           02  DSP-KEI.
               03  DSP-KEIZ   PIC  ------9 .
               03  DSP-KEIA   PIC  ------9 .
               03  DSP-KEIB   PIC  ------9 .
               03  DSP-KEIC   PIC  ------9 .
               03  DSP-KEID   PIC  ------9 .
       01  C-DSP.
           02  FILLER.
             03  DSP-NGP   PIC  X(29) VALUE
                  "ÅÉ éÛî≠íçî[ä˙    .  .   ñò ÅÑ".
             03  DSP-NGPC  PIC  X(23) VALUE
                  "                       ".
       01  CLR-AREA.
           02  CLR-HIN PIC  X(07)  VALUE  " ".
           02  CLR-HNM PIC  X(48)  VALUE  " ".
           02  CLR-SCD PIC  X(14)  VALUE  " ".
           02  CLR-MEISAI.
               03  CLR-MEI1  PIC  X(35)  VALUE  " ".
               03  CLR-MEI2  PIC  X(35)  VALUE  " ".
           02  CLR-KEI PIC  X(35)  VALUE  " ".
       01  EMSG-AREA.
           02  EMSG-02  PIC  X(18)
                        VALUE  "ïiñºÉ}ÉXÉ^Å@ñ¢ìoò^".
           02  EMSG-03  PIC  X(32)
                        VALUE  "ÉRÉìÉgÉçÅ[ÉãÇeÅ@Åiëqå…ÅjÅ@ñ¢ìoò^".
           02  EMSG-04  PIC  X(32)
                        VALUE  "ÅÉÅ@óaÇËÅ@óLÇËÅ@ÅÑÅ@Å@Å@Å@Å@Å@Å@".
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(39).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(39).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@É}ÉXÉ^Å@ìoò^çœÅ@Åñ".
           02  NOR-D01.
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@ÉfÅ[É^Å@ìoò^çœÅ@Åñ".
           02  INV-M01.
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@É}ÉXÉ^Å@ñ¢ìoò^Å@Åñ".
           02  INV-D01.
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@ÉfÅ[É^Å@ñ¢ìoò^Å@Åñ".
           02  OK-01.
               03  FILLER  PIC X(14) VALUE
               "ÅñÅ@ÇnÅ@ÇjÅ@Åñ".
           02  CAN-01.
               03  FILLER  PIC X(18) VALUE
               "ÅñÅ@ÉLÉÉÉìÉZÉãÅ@Åñ".
           02  ERR-01.
               03  FILLER  PIC X(18) VALUE
               "ÅñÅ@ì¸óÕÉGÉâÅ[Å@Åñ".
           02  ERR-02.
               03  FILLER  PIC X(22) VALUE
               "ÅñÅ@ÉfÅ[É^Å@Ç»ÇµÅ@Å@Åñ".
           02  ERR-DIS.
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
           COPY LIBSCR.
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-1
       CALL "SD_Init" USING
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "14" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "1" "71" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-HIN" "9" "2" "6" "6" "ACP-ACT" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-HIN" BY REFERENCE W-HIN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SCD" "9" "2" "67" "1" "ACP-HIN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SCD" BY REFERENCE W-SCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ACP-AREA" " " "23" "0" "6" "ACP-SCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NEN" "9" "23" "23" "2" " " "04ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-NEN" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GET" "9" "23" "26" "2" "ACP-NEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-PEY" "9" "23" "29" "2" "ACP-GET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "225" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HINNM" "N" "2" "13" "48" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HINNM" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SNM" "N" "2" "69" "12" "DSP-HINNM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SNM" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID1" " " "3" "0" "30" "DSP-SNM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M10" "N" "3" "6" "6" " " "DSP-MID1"  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M10" BY REFERENCE W-M0 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M11" "N" "3" "13" "6" "DSP-M10" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M11" BY REFERENCE W-M1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M12" "N" "3" "20" "6" "DSP-M11" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M12" BY REFERENCE W-M2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M13" "N" "3" "27" "6" "DSP-M12" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M13" BY REFERENCE W-M3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M14" "N" "3" "34" "6" "DSP-M13" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M14" BY REFERENCE W-M4 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID2" " " "3" "0" "30" "DSP-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M20" "N" "3" "46" "6" " " "DSP-MID2"  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M20" BY REFERENCE W-M0 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M21" "N" "3" "53" "6" "DSP-M20" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M21" BY REFERENCE W-M1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M22" "N" "3" "60" "6" "DSP-M21" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M22" BY REFERENCE W-M2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M23" "N" "3" "67" "6" "DSP-M22" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M23" BY REFERENCE W-M3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-M24" "N" "3" "74" "6" "DSP-M23" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-M24" BY REFERENCE W-M4 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEISAI" " " "0" "0" "70" "DSP-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI1" " " "LIN1" "13" "35" " " "DSP-MEISAI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MLZ" "-------" "LIN1" "5" "7" " " "DSP-MEI1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MLZ" BY REFERENCE W-M1Z(1,1) "6" "2" BY REFERENCE P 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MLA" "-------" "LIN1" "12" "7" "DSP-MLZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MLA" BY REFERENCE W-M1A(1,1) "6" "2" BY REFERENCE P 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MLB" "-------" "LIN1" "19" "7" "DSP-MLA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MLB" BY REFERENCE W-M1B(1,1) "6" "2" BY REFERENCE P 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MLC" "-------" "LIN1" "26" "7" "DSP-MLB" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MLC" BY REFERENCE W-M1C(1,1) "6" "2" BY REFERENCE P 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MLD" "-------" "LIN1" "33" "7" "DSP-MLC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MLD" BY REFERENCE W-M1D(1,1) "6" "2" BY REFERENCE P 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI2" " " "LIN1" "13" "35" "DSP-MEI1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MRZ" "-------" "LIN1" "45" "7" " " "DSP-MEI2"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MRZ" BY REFERENCE W-M1Z(1,1) "6" "2" BY REFERENCE O 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MRA" "-------" "LIN1" "52" "7" "DSP-MRZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MRA" BY REFERENCE W-M1A(1,1) "6" "2" BY REFERENCE O 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MRB" "-------" "LIN1" "59" "7" "DSP-MRA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MRB" BY REFERENCE W-M1B(1,1) "6" "2" BY REFERENCE O 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MRC" "-------" "LIN1" "66" "7" "DSP-MRB" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MRC" BY REFERENCE W-M1C(1,1) "6" "2" BY REFERENCE O 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MRD" "-------" "LIN1" "73" "7" "DSP-MRC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MRD" BY REFERENCE W-M1D(1,1) "6" "2" BY REFERENCE O 300
            BY REFERENCE I 6 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEI" " " "24" "0" "35" "DSP-MEISAI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEIZ" "------9" "24" "45" "7" " " "DSP-KEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEIZ" BY REFERENCE W-KEIZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEIA" "------9" "24" "52" "7" "DSP-KEIZ" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEIA" BY REFERENCE W-KEIA "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEIB" "------9" "24" "59" "7" "DSP-KEIA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEIB" BY REFERENCE W-KEIB "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEIC" "------9" "24" "66" "7" "DSP-KEIB" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEIC" BY REFERENCE W-KEIC "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEID" "------9" "24" "73" "7" "DSP-KEIC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEID" BY REFERENCE W-KEID "6" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "52" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "23" "0" "52" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NGP" "X" "23" "8" "29" " " "01C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NGPC" "X" "23" "8" "23" "DSP-NGP" " " RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "174" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-HIN" "X" "2" "6" "7" " " "CLR-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-HNM" "X" "2" "13" "48" "CLR-HIN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-SCD" "X" "2" "67" "14" "CLR-HNM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-MEISAI" " " "LIN1" "0" "70" "CLR-SCD" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-MEI1" "X" "LIN1" "5" "35" " " "CLR-MEISAI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-MEI2" "X" "LIN1" "45" "35" "CLR-MEI1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-KEI" "X" "24" "45" "35" "CLR-MEISAI" " "
            RETURNING RESU.
      *EMSG-AREA
       CALL "SD_Init" USING 
            "EMSG-AREA" " " "0" "0" "82" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-02" "X" "24" "1" "18" " " "EMSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-03" "X" "24" "1" "32" "EMSG-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-04" "X" "24" "1" "32" "EMSG-03" " "  RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "319" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-01" " " "24" "0" "39" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-01" "X" "24" "1" "39" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE" " " "24" "0" "39" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "24" "1" "39" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
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
            "NOR-M01" " " "24" "0" "22" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-D01" " " "24" "0" "22" "NOR-M01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-D01" "X" "24" "1" "22" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M01" " " "24" "0" "22" "NOR-D01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-M01" "X" "24" "1" "22" " " "INV-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D01" " " "24" "0" "22" "INV-M01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-D01" "X" "24" "1" "22" " " "INV-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "OK-01" " " "24" "0" "14" "INV-D01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01OK-01" "X" "24" "1" "14" " " "OK-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CAN-01" " " "24" "0" "18" "OK-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CAN-01" "X" "24" "1" "18" " " "CAN-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-01" " " "24" "0" "18" "CAN-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-01" "X" "24" "1" "18" " " "ERR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-02" " " "24" "0" "22" "ERR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-02" "X" "24" "1" "22" " " "ERR-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "24" "0" "71" "ERR-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-DIS" "X" "24" "2" "5" " " "ERR-DIS"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-DIS" "X" "24" "7" "12" "01ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ERR-DIS" "X" "24" "19" "1" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ERR-DIS" "X" "24" "20" "11" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05ERR-DIS" "X" "24" "31" "2" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06ERR-DIS" "X" "24" "33" "5" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07ERR-DIS" "X" "24" "38" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08ERR-DIS" "X" "24" "43" "30" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MEIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  ACT-RTN    THRU  ACT-EX.
       MR999.
           PERFORM  END-RTN    THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
      ******************************
      *    ÇhÇmÇhÅ|ÇqÇsÇm          *
      *          Å`èâä˙èàóùÅ`      *
      ******************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               MOVE 0 TO JS-SIGN
           END-IF
           ACCEPT W-JS FROM ARGUMENT-VALUE.
           IF  W-JS > 1
               MOVE 0 TO W-JS
           END-IF
           CALL "SD_Screen_Output" USING "SJ251R" RETURNING RESU.
           IF  W-JS = 1
               CALL "DB_F_Open" USING
                "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST
                "1" "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               CALL "DB_F_Open" USING
                "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST
                "1" "HI-KEY2" BY REFERENCE HI-KEY2
               CALL "DB_F_Open" USING
                "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST
                "1" "JCON3-KEY" BY REFERENCE JCON3-KEY
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST
                "1" "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               CALL "DB_F_Open" USING
                "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST
                "1" "HI-KEY2" BY REFERENCE HI-KEY2
               CALL "DB_F_Open" USING
                "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST
                "1" "JMST3-KEY" BY REFERENCE JMST3-KEY
               CALL "DB_F_Open" USING
                "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
                "JCON3-KEY" BY REFERENCE JCON3-KEY
               CALL "DB_F_Open" USING
                "INPUT" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
                "JNYZ-KEY" BY REFERENCE JNYZ-KEY
               CALL "DB_F_Open" USING
                "INPUT" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
                "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
                HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3
           END-IF
      *
           MOVE     SPACE      TO    WORK-AREA  WORK-MEISAI.
           INITIALIZE                WORK-AREA  WORK-MEISAI.
           COPY LIBCPR.
       INI-EX.
           EXIT.
      ******************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm          *
      *          Å`èIóπèàóùÅ`      *
      ******************************
       END-RTN.
           IF  W-JS = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
           ELSE
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JMST3_IDLST JMST3_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNYZ_IDLST JNYZ_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HSHF_IDLST HSHF_PNAME1
           END-IF
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *************************************
      *    Ç`ÇbÇsÅ|ÇqÇsÇm                 *
      *          Å`âÊñ ì¸óÕÅñçXêVèàóùÅ`   *
      *************************************
       ACT-RTN.
           IF  W-JS       =  1
               MOVE  0             TO  W-ACT
               CALL "SD_Output" USING
                "ACP-ACT" ACP-ACT "p" RETURNING RESU
               GO  TO  ACT-005
           END-IF
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-RTN
           END-IF
           IF  W-ACT  NOT =  0  AND  1
               GO  TO  ACT-RTN
           END-IF.
      **
       ACT-005.
           IF  W-ACT      =  0
               MOVE  "ëOåééc"    TO  W-M0
               MOVE  "ì¸å…êî"    TO  W-M1
               MOVE  "èoå…êî"    TO  W-M2
               MOVE  "éwê}êî"    TO  W-M3
               MOVE  "é¿ç›å…"    TO  W-M4
           ELSE
               MOVE  "é¿ç›å…"    TO  W-M0
               MOVE  "óaÇËêî"    TO  W-M1
               MOVE  "éÊÇÊÇØ"    TO  W-M2
               MOVE  "óLå¯êî"    TO  W-M3
               MOVE  "éÛíçêî"    TO  W-M4
           END-IF
           CALL "SD_Output" USING
            "DSP-MID1" DSP-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MID2" DSP-MID2 "p" RETURNING RESU.
      ***
           MOVE  ZERO          TO  W-HIN  W-SCD.
           CALL "SD_Output" USING "CLR-HIN" CLR-HIN "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-HNM" CLR-HNM "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-SCD" CLR-SCD "p" RETURNING RESU.
           PERFORM  CLR-RTN    THRU  CLR-EX.
       ACT-010.
           MOVE  W-HIN         TO  O-HIN.
           CALL "SD_Output" USING "CLR-HIN" CLR-HIN "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-HNM" CLR-HNM "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-SCD" CLR-SCD "p" RETURNING RESU.
       ACT-011.
           CALL "SD_Accept" USING BY REFERENCE ACP-HIN "ACP-HIN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT      =  "09"
               IF  W-JS       =  0
                   GO  TO  ACT-RTN
               END-IF
           END-IF
           IF  ESTAT      =  "P6"
               GO  TO  ACT-012
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-011
           END-IF
           CALL "SD_Output" USING "ACP-HIN" ACP-HIN "p" RETURNING RESU.
           GO  TO  ACT-015.
       ACT-012.
      *           READ     HI2-M      NEXT UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST
                "1" "HI-KEY2" BY REFERENCE HI-KEY2
               GO  TO  ACT-012
           END-IF
           MOVE     HI-MHCD    TO    W-HIN.
           CALL "SD_Output" USING "ACP-HIN" ACP-HIN "p" RETURNING RESU.
           GO  TO  ACT-016.
       ACT-015.
           MOVE     W-HIN      TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST
                "1" "HI-KEY2" BY REFERENCE HI-KEY2
               CALL "SD_Output" USING
                "EMSG-02" EMSG-02 "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF.
       ACT-016.
           CALL "SD_Output" USING
            "DSP-HINNM" DSP-HINNM "p" RETURNING RESU.
           IF  W-ACT      =  1     MOVE  "ëSÅ@é–"    TO  JCON3-03
               MOVE  9             TO  W-SCD
               CALL "SD_Output" USING
                "ACP-SCD" ACP-SCD "p" RETURNING RESU
               GO  TO  ACT-025
           END-IF
           IF  W-JS       =  1     MOVE  4             TO  W-SCD
               CALL "SD_Output" USING
                "ACP-SCD" ACP-SCD "p" RETURNING RESU
               GO  TO  ACT-021
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-SCD "ACP-SCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-020
           END-IF
           IF  W-SCD      >  9    OR  <  1
               GO  TO  ACT-020
           END-IF
           IF  W-SCD      =  9
               MOVE  "ëSÅ@é–"    TO  JCON3-03
           END-IF
           IF  W-SCD      =  9
               GO  TO  ACT-025
           END-IF.
       ACT-021.
           MOVE  "3"           TO  JCON3-01.
           MOVE  W-SCD         TO  JCON3-02.
      *           READ  JCON     UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "EMSG-03" EMSG-03 "p" RETURNING RESU
               GO  TO  ACT-020
           END-IF.
       ACT-025.
           CALL "SD_Output" USING "DSP-SNM" DSP-SNM "p" RETURNING RESU.
       ACT-030.
           PERFORM  CLR-RTN    THRU  CLR-EX.
           MOVE  ZERO          TO  W-DC W-DATE.
           PERFORM  KEI-RTN    THRU  KEI-EX.
           IF  ESTAT = "09"
               GO TO ACT-040
           END-IF
           PERFORM  DSP-RTN    THRU  DSP-EX.
           IF  1          =  NYZ-SW  AND  AZU-SW
               CALL "SD_Output" USING
                "EMSG-04" EMSG-04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
           END-IF.
       ACT-040.
           IF  W-ACT      =  1
               GO  TO  ACT-011
           END-IF
           IF  W-JS       =  1
               GO  TO  ACT-011
           END-IF
           GO  TO  ACT-020.
       ACT-EX.
           EXIT.
      *************************************
      *    ÇjÇdÇhÅ|ÇqÇsÇm                 *
      *          Å`ñæç◊åvéZèàóùÅ`         *
      *************************************
       KEI-RTN.
       KEI-010.
           MOVE  W-SCD         TO  NJZAI-01.
           MOVE  W-HIN         TO  NJZAI-02.
           MOVE  0             TO  NJZAI-03.
      *           START  NJZAI   KEY  NOT <  NJZAI-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-030
           END-IF.
       KEI-020.
      *           READ  NJZAI    NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-025
           END-IF
           IF  NJZAI-01     NOT =  W-SCD
               GO  TO  KEI-025
           END-IF
           IF  NJZAI-02     NOT =  W-HIN
               GO  TO  KEI-025
           END-IF
           MOVE  NJZAI-03      TO  P.
           PERFORM  JZAI-RTN   THRU  JZAI-EX
               VARYING  I    FROM  1  BY  1
               UNTIL  I     >    10.
           GO  TO  KEI-020.
       KEI-025.
           IF  W-ACT            =  0
                 GO  TO  KEI-200
           END-IF.
       KEI-030.
           MOVE  ZERO          TO  JMST3-KEY.
           MOVE  W-HIN         TO  JMST3-03.
      *           START  JMST3    KEY  NOT <  JMST3-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-060
           END-IF.
       KEI-040.
      *           READ  JMST3     NEXT  UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-060
           END-IF
           IF  W-HIN        NOT =  JMST3-03
               GO  TO  KEI-060
           END-IF
           IF  JS-SIGN = 0
               GO TO KEI-055
           END-IF
           IF  W-DC = 1
               IF  JMST3-06 > W-DATE
                   GO TO KEI-040
               ELSE
                   GO TO KEI-055
               END-IF
           END-IF
           MOVE 1 TO W-DC.
           PERFORM NGP-RTN THRU NGP-EX.
           IF  ESTAT = "09"
               GO TO KEI-EX
           END-IF
           IF  HSH-NDD > W-DATE
               GO TO KEI-040
           END-IF.
       KEI-055.
           MOVE  JMST3-09      TO    P.
           PERFORM  JMST1-RTN  THRU  JMST1-EX
               VARYING  I    FROM  1  BY  1
               UNTIL  I     >    10.
           GO  TO  KEI-040.
       KEI-060.
           MOVE  ZERO          TO  JNYZ-KEY.
           MOVE  W-HIN         TO  JNYZ-01.
      *           START  JNYZ     KEY  NOT <  JNYZ-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            JNYZ_PNAME1 "JNYZ-KEY" " NOT < " JNYZ-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-100
           END-IF.
       KEI-070.
      *           READ  JNYZ      NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNYZ_PNAME1 BY REFERENCE JNYZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-100
           END-IF
           IF  JNYZ-01      NOT =  W-HIN
               GO  TO  KEI-100
           END-IF
           MOVE  W-SCD         TO  NJZAI-01.
           MOVE  JNYZ-01       TO  NJZAI-02.
           MOVE  JNYZ-02       TO  NJZAI-03.
      *           READ  NJZAI           UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO        TO  NJZAI-0711(01)  NJZAI-0711(02)
                                     NJZAI-0711(03)  NJZAI-0711(04)
                                     NJZAI-0711(05)  NJZAI-0711(06)
                                     NJZAI-0711(07)  NJZAI-0711(08)
                                     NJZAI-0711(09)  NJZAI-0711(10)
           END-IF
           MOVE  JNYZ-02       TO  P.
           PERFORM  JNYZ-RTN   THRU  JNYZ-EX
               VARYING  I    FROM  1  BY  1
               UNTIL  I     >    10.
           GO  TO  KEI-070.
       KEI-100.
           MOVE  ZERO          TO  HSH-KEY2.
           MOVE  W-HIN         TO  HSH-HCD.
      *           START  HSHF     KEY  NOT <  HSH-KEY2 INVALID
      *///////////////
           CALL "DB_Start" USING
            HSHF_PNAME1 "HSH-KEY2" " NOT < " HSH-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-200
           END-IF.
       KEI-110.
      *           READ  HSHF      NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  KEI-200
           END-IF
           IF  HSH-HCD      NOT =  W-HIN
               GO  TO  KEI-200
           END-IF
           IF  HSH-ENGP     NOT =  ZERO
               GO  TO  KEI-110
           END-IF
      *
           IF  JS-SIGN = 0
               GO TO KEI-115
           END-IF
           IF  W-DC = 1
               IF  HSH-NDD > W-DATE
                   GO TO KEI-110
               ELSE
                   GO TO KEI-115
               END-IF
           END-IF
           MOVE 1 TO W-DC.
           PERFORM NGP-RTN THRU NGP-EX.
           IF  ESTAT = "09"
               GO TO KEI-EX
           END-IF
           IF  HSH-NDD > W-DATE
               GO TO KEI-110
           END-IF.
       KEI-115.
           MOVE  1             TO  P.
       KEI-120.
           PERFORM  HSHF-RTN   THRU  HSHF-EX
               VARYING  I    FROM  1  BY  1
               UNTIL  I     >    10.
           IF  P            NOT =  4
               ADD   1           TO  P
               GO  TO  KEI-120
           END-IF
           GO  TO  KEI-110.
       KEI-200.
           MOVE  1             TO    P.
       KEI-210.
           PERFORM  GYKEI-RTN  THRU  GYKEI-RTN
               VARYING  I    FROM  1  BY  1
               UNTIL  I     >    10.
           IF  P            NOT =  4
               ADD   1           TO  P
               GO  TO  KEI-210
           END-IF.
       KEI-EX.
           EXIT.
      *----ëqï ç›å…É}ÉXÉ^Å@ÇÊÇËÅ@äeêîó åvéZ----*
       JZAI-RTN.
           COMPUTE  WK-A(P I)  =  WK-A(P I)     + NJZAI-0411(I)
                                - NJZAI-0511(I) + NJZAI-0611(I)
                                + NJZAI-1111(I).
           COMPUTE  WK-B(P I)  =  WK-B(P I) + NJZAI-0711(I).
           COMPUTE  WK-C(P I)  =  WK-C(P I) + NJZAI-0811(I).
           COMPUTE  WK-D(P I)  =  WK-D(P I) + NJZAI-0911(I).
           COMPUTE  WK-E(P I)  =  WK-A(P I) + WK-B(P I)
                                - WK-C(P I) - WK-D(P I).
       JZAI-EX.
           EXIT.
      *----éÛíçÉ}ÉXÉ^Å@ÇÊÇËÅ@äeêîó åvéZ----*
       JMST1-RTN.
           IF  JMST3-01        =  0
               COMPUTE  WK-L(P I)  =  WK-L(P I) + JMST3-1111(I)
                       - JMST3-151(I) - JMST3-141(I) - JMST3-1211(I)
           END-IF
           IF  JMST3-01        =  5
               COMPUTE  WK-F(P I)  =  WK-F(P I) + JMST3-1111(I)
                         - JMST3-151(I) - JMST3-141(I) - JMST3-1211(I)
           END-IF
           IF  JMST3-01        =  6
               COMPUTE  WK-G(P I)  =  WK-G(P I) + JMST3-1111(I)
                       - JMST3-151(I) - JMST3-141(I) - JMST3-1211(I)
           END-IF
           IF  WK-F(P I)  NOT  =  ZERO
               IF  AZU-SW          =  0
                   MOVE  1          TO  AZU-SW
               END-IF
           END-IF.
       JMST1-EX.
           EXIT.
      *----ì¸å…ó\íËécÉtÉ@ÉCÉãÇÊÇËÅ@äeêîó åvéZ----*
       JNYZ-RTN.
           MOVE  JNYZ-0311(I)   TO  WK-H(P I).
           COMPUTE  WK-H(P I)  =  WK-H(P I) - NJZAI-0711(I).
           IF  WK-H(P I)  NOT  =  ZERO
               IF  NYZ-SW          =  0
                   MOVE  1          TO  NYZ-SW
               END-IF
           END-IF.
       JNYZ-EX.
           EXIT.
       HSHF-RTN.
           COMPUTE  WK-H(P I)  =  WK-H(P I) + HSH-HSU(P I) -
                                  HSH-NSU(P I) - HSH-ISU(P I).
           IF  WK-H(P I)  NOT  =  ZERO
               IF  NYZ-SW          =  0
                   MOVE  1          TO  NYZ-SW
               END-IF
           END-IF.
       HSHF-EX.
           EXIT.
      *----åªç›écÅEóLå¯êîÅ@äeêîó åvéZ----*
       GYKEI-RTN.
           COMPUTE  WK-K(P I)  =  WK-E(P I) - WK-F(P I) - WK-G(P I).
           IF  NYZ-SW          =  1
               COMPUTE  WK-K(P I)  =  WK-K(P I) + WK-H(P I)
           END-IF.
       GYKEI-EX.
           EXIT.
      *************************************
      *    ÇcÇrÇoÅ|ÇqÇsÇm                 *
      *          Å`ñæç◊ï\é¶èàóùÅ`         *
      *************************************
       DSP-RTN.
           MOVE  1             TO  P.
       DSP-000.
           PERFORM  SET-RTN    THRU  SET-EX
               VARYING   I   FROM  1  BY  1
               UNTIL   I    >   10.
           IF  P            NOT =  4
               ADD   1           TO  P
               GO  TO  DSP-000
           END-IF
           MOVE  4             TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE  2             TO    P.
           MOVE  4             TO    O.
       DSP-010.
           PERFORM  DSP-MEI-RTN  THRU  DSP-MEI-EX
               VARYING   I   FROM  1  BY  1
               UNTIL   I    >   10.
           IF  P                =  3
               CALL "SD_Output" USING
                "DSP-KEI" DSP-KEI "p" RETURNING RESU
               GO  TO  DSP-EX
           END-IF
           MOVE  14            TO  LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE  3             TO  P.
           MOVE  1             TO  O.
           GO  TO  DSP-010.
       DSP-EX.
           EXIT.
      *-----âÊñ ï\é¶ÉèÅ[ÉNÅ@ÉZÉbÉgÅ@ÉãÅ[É`Éì----*
       SET-RTN.
           IF  W-ACT            =  0
               MOVE  WK-A(P I)     TO  W-M1Z(P I)
               MOVE  WK-B(P I)     TO  W-M1A(P I)
               MOVE  WK-C(P I)     TO  W-M1B(P I)
               MOVE  WK-D(P I)     TO  W-M1C(P I)
               MOVE  WK-E(P I)     TO  W-M1D(P I)
           END-IF
           IF  W-ACT            =  1
               MOVE  WK-E(P I)     TO  W-M1Z(P I)
               MOVE  WK-G(P I)     TO  W-M1B(P I)
               MOVE  WK-K(P I)     TO  W-M1C(P I)
               MOVE  WK-L(P I)     TO  W-M1D(P I)
               IF  NYZ-SW           =  0
                   MOVE  "óaÇËêî"    TO  W-M1
                   MOVE  WK-F(P I)   TO  W-M1A(P I)
               ELSE
                   MOVE  "ì¸ó\íË"    TO  W-M1
                   MOVE  WK-H(P I)   TO  W-M1A(P I)
               END-IF
           END-IF
           CALL "SD_Output" USING
            "DSP-MID1" DSP-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-MID2" DSP-MID2 "p" RETURNING RESU.
           ADD   W-M1Z(P I)    TO  W-KEIZ.
           ADD   W-M1A(P I)    TO  W-KEIA.
           ADD   W-M1B(P I)    TO  W-KEIB.
           ADD   W-M1C(P I)    TO  W-KEIC.
           ADD   W-M1D(P I)    TO  W-KEID.
       SET-EX.
           EXIT.
      *-----âÊñ ï\é¶ÉãÅ[É`Éì----*
       DSP-MEI-RTN.
           IF  LIN1 NOT = 23
               CALL "SD_Output" USING
                "DSP-MEI1" DSP-MEI1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DSP-MEI2" DSP-MEI2 "p" RETURNING RESU
           ELSE
               IF  W-DC = 1
                   CALL "SD_Output" USING
                    "DSP-MEI2" DSP-MEI2 "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "DSP-MEI1" DSP-MEI1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DSP-MEI2" DSP-MEI2 "p" RETURNING RESU
               END-IF
           END-IF
           ADD   1             TO  LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
       DSP-MEI-EX.
           EXIT.
      *----ÇbÇkÇqÅ|ÇqÇsÇm              *
      *      Å`ÉèÅ[ÉNÅEâÊñ ÉNÉäÉAÅ`----*
       CLR-RTN.
           INITIALIZE          WORK-MEISAI.
           MOVE  4             TO  LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
       CLR-010.
           IF  LIN1            >   24
               CALL "SD_Output" USING
                "CLR-KEI" CLR-KEI "p" RETURNING RESU
               GO  TO  CLR-EX
           END-IF
           CALL "SD_Output" USING
            "CLR-MEI1" CLR-MEI1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "CLR-MEI2" CLR-MEI2 "p" RETURNING RESU.
           ADD   1             TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           GO  TO  CLR-010.
       CLR-EX.
           EXIT.
       NGP-RTN.
           CALL "SD_Output" USING "DSP-NGP" DSP-NGP "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-NEN" ACP-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-GET" ACP-GET "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-PEY" ACP-PEY "p" RETURNING RESU.
       NGP-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-NEN "ACP-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO NGP-EX
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO NGP-010
           END-IF.
       NGP-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-GET "ACP-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO NGP-RTN
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO NGP-020
           END-IF
           IF  W-GET > 00 AND < 13
               GO TO NGP-030
           END-IF
           IF  W-GET = 99
               IF  W-NEN NOT = 99
                   GO TO NGP-020
               END-IF
           END-IF
           IF  W-GET = 00
               IF  W-NEN NOT = 00
                   GO TO NGP-020
               END-IF
           END-IF.
       NGP-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-PEY "ACP-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO TO NGP-020
           END-IF
           IF  ESTAT NOT = "00" AND "01" AND "06"
               GO TO NGP-030
           END-IF
           IF  W-PEY > 00 AND < 32
               GO TO NGP-040
           END-IF
           IF  W-PEY = 99
               IF  W-GET NOT = 99
                   GO TO NGP-030
               END-IF
           END-IF
           IF  W-PEY = 00
               IF  W-GET NOT = 00
                   GO TO NGP-030
               END-IF
           END-IF.
       NGP-040.
           IF  W-GET = 00 OR 99
               MOVE 99999999 TO W-DATE
               CALL "SD_Output" USING
                "ACP-NEN" ACP-NEN "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACP-GET" ACP-GET "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACP-PEY" ACP-PEY "p" RETURNING RESU
               GO TO NGP-EX
           END-IF
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       NGP-EX.
           EXIT.
      ***
           COPY    LPMSG.
      ***
