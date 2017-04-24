       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT050L.
       AUTHOR.                        E-SHIGIHARA.
      **********************************************************
      *    PROGRAM        : â◊éDÅEì¸ì˙ãLÅ@Å@Å@Å@Å@Å@           *
      *    DATA WRITTEN   : 87/08/19                           *
      *    SCREEN USED    : UNUSED                             *
      *    FORM   USED    : UNUSED                             *
      *    PRINTER TYPE   : JIPS                               *
      *    COMPILE TYPE   : COBOL                              *
      *    JS-W           : 0=ñ{é– , 1=ëIë , 2=ã ìá , 3=ëÅìá  *
      **********************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  JS-SIGN                   PIC X(01).
       77  JS-W                      PIC X(01).
       77  W-JS                      PIC X(01).
       77  STR-SW                    PIC X(02).
       01  W-P                       PIC X(175).
       01  R1-R.
           02  R1-K1                 PIC X(05).
           02  FILLER                PIC X(01).
           02  R1-01                 PIC N(14).
           02  R1-K2                 PIC X(05).
       01  R2-R.
           02  R2-K1                 PIC X(05).
           02  FILLER                PIC X(01).
           02  R2-01                 PIC N(14).
           02  FILLER                PIC X(01).
           02  R2-02A.
             03  R2-02        OCCURS   25.
                 04  R2-021          PIC ZZZ.
           02  R2-02B   REDEFINES R2-02A.
             03  R2-B02        OCCURS   25.
                 04  R2-B020         PIC X.
                 04  R2-B021         PIC ZZ.
           02  R2-03                 PIC ZZZZ.
           02  R2-K2                 PIC X(05).
       01  MID0-R.
           02  M0-K1                 PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC X(01)  VALUE   SPACE.
           02  M0-01                 PIC N(05)  VALUE   SPACE.
       01  MID1-R.
           02  M1-K2                 PIC X(05)  VALUE X"1A24212474".
           02  FILLER                PIC X(08)    VALUE   SPACE.
           02  M1-01                 PIC X(08).
           02  FILLER                PIC X(31)    VALUE   SPACE.
           02  M1-S                  PIC N(01)    VALUE   SPACE.
           02  FILLER                PIC X(08)    VALUE   SPACE.
           02  M1-06                 PIC 9(06).
           02  FILLER                PIC X(09)    VALUE   SPACE.
           02  M1-03                 PIC 9(02).
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  M1-04                 PIC Z9.
           02  FILLER                PIC X(03)    VALUE   SPACE.
           02  M1-05                 PIC Z9.
           02  FILLER                PIC X(11)    VALUE   SPACE.
           02  M1-02                 PIC 9(06).
       01  MID2A-R.
           02  M2A-K1                PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC X(51)    VALUE   SPACE.
           02  M2A-UM                PIC N(02)  VALUE "Å@Åß".
           02  M2A-UNO               PIC X(08)    VALUE   SPACE.
           02  F                     PIC X(02)    VALUE   SPACE.
           02  M2A-JSO               PIC N(06).
       01  MID2-R.
           02  M2-K1                 PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC X(06)    VALUE   SPACE.
           02  M2-01                 PIC N(20).
           02  F                     PIC X(16)    VALUE   SPACE.
           02  M2-JSN                PIC N(14).
           02  F                     PIC X(25)    VALUE   SPACE.
           02  M2-02                 PIC ZZZ.
       01  MID3-R.
           02  M3-K1                 PIC X(05)  VALUE X"1A24212078".
           02  FILLER                PIC X(13)    VALUE   SPACE.
           02  M3-01                 PIC N(20).
           02  F                     PIC X(09)    VALUE   SPACE.
           02  M3-JS                 PIC N(14)    VALUE   SPACE.
           02  FILLER                PIC X(28)    VALUE   SPACE.
           02  M3-K2                 PIC X(05)  VALUE X"1A24212474".
           02  M3-02                 PIC N(01).
       01  MID4-R.
           02  FILLER                PIC X(08)    VALUE   SPACE.
           02  M5-K1                 PIC X(05)  VALUE X"1A24212078".
           02  M5-01                 PIC N(26).
           02  FILLER                PIC X(05)    VALUE   SPACE.
           02  M5-K2            PIC X(08) VALUE  X"1A26222166212078".
           02  M4-NA                 PIC N(14)    VALUE   SPACE.
           02  F                     PIC X(02)    VALUE   SPACE.
           02  M5-K2A                PIC X(05)  VALUE X"1A24212474".
           02  M5-02                 PIC N(06).
           02  M4-K4            PIC X(08) VALUE  X"1A26212068212078".
           02  FILLER                PIC X(03)   VALUE   SPACE.
           02  M4-K3            PIC X(08) VALUE  X"1A26222166222176".
           02  M4-01                 PIC N(03).
           02  M4-K5            PIC X(08) VALUE  X"1A26212068212078".
       01  MID5-R.
           02  F                     PIC X(54)   VALUE   SPACE.
           02  M5-TM                 PIC X(03)   VALUE   "TEL".
           02  F                     PIC X(01)   VALUE   SPACE.
           02  M5-TEL                PIC X(14)   VALUE   SPACE.
       01  MEI                           PIC X(1080).
       01  MEIR      REDEFINES       MEI.
           02  MEI-R      OCCURS            6.
               03  MEI1-R                PIC X(50).
               03  MEI2-R                PIC X(130).
       01  R3-R.
           02  R3-K1                 PIC X(05).
           02  FILLER                PIC X(24).
           02  R3-K30                PIC X(08).
           02  R3-01                 PIC N(28).
           02  R3-K20                PIC X(08).
           02  F                     PIC X(26).
           02  R3-02                 PIC N(06).
           02  R3-K2                 PIC X(05).
       01  ACT-WORK.
           02  W-SEN                 PIC 9(01).
           02  W-TST                 PIC 9(01).
           02  W-FROM                PIC 9(06).
           02  W-TO                  PIC 9(06).
           02  W-OK                  PIC 9(01).
           02  I                     PIC 9(02).
           02  W-T                   PIC S9(03).
           02  OLD1                  PIC 9(06).
           02  W-CNT                 PIC 9(03).
           02  A                     PIC X(03).
           02  AR     REDEFINES      A.
               03  AAR               PIC ZZ9.
           02  W-TEKI.
               03  W-TEKI1           PIC N(04).
               03  W-TEKI2           PIC N(19).
           02  P-TEKI.
               03  P-TEKI1           PIC N(19).
               03  P-TEKI2           PIC N(09).
           02  SV-GYO                PIC 9(01).
           02  SV-TCD                PIC 9(04).
           02  W-OKNO                PIC 9(06).
           02  W-NO                  PIC 9(03).
           02  W-ANAME.
             03  W-NNA               PIC N(24).
             03  W-NNMD  REDEFINES  W-NNA.
               04  W-NNM             PIC N(01)  OCCURS  24.
             03  W-PNA   REDEFINES  W-NNA.
               04  W-PNA1            PIC N(14).
               04  W-PNA2            PIC N(10).
             03  W-ONA               PIC N(14).
             03  W-ONMD  REDEFINES  W-ONA.
               04  W-ONM             PIC N(01)  OCCURS  14.
             03  W-UNA               PIC N(14).
             03  W-UNMD  REDEFINES  W-UNA.
               04  W-UNM             PIC N(01)  OCCURS  14.
           02  W-C.
             03  W-C1                PIC 9(02).
             03  W-C2                PIC 9(02).
             03  W-C3                PIC 9(02).
             03  W-C4                PIC 9(02).
           02  W-NGP.
             03  F                   PIC 9(02).
             03  W-NGPS              PIC 9(06).
           02  W-NGPL              REDEFINES  W-NGP.
             03  W-NEN               PIC 9(04).
             03  W-NENL            REDEFINES  W-NEN.
               04  W-NEN1            PIC 9(02).
               04  W-NEN2            PIC 9(02).
             03  F                   PIC 9(04).
           02  W-SDATE               PIC 9(08).
           02  W-EDATE               PIC 9(08).
           02  W-SNGP.
             03  W-SNEN              PIC 9(04).
             03  W-SNENL           REDEFINES  W-SNEN.
               04  W-SNEN1           PIC 9(02).
               04  W-SNEN2           PIC 9(02).
             03  W-SGET              PIC 9(02).
             03  W-SPEY              PIC 9(02).
           02  W-ENGP.
             03  W-ENEN              PIC 9(04).
             03  W-ENENL           REDEFINES  W-ENEN.
               04  W-ENEN1           PIC 9(02).
               04  W-ENEN2           PIC 9(02).
             03  W-EGET              PIC 9(02).
             03  W-EPEY              PIC 9(02).
           02  W-POC                 PIC 9(01).
           02  W-FOC                 PIC 9(01).
           02  W-KEI                 PIC 9(04)   VALUE  0.
       01  W-AREA.
           02  LCNT                  PIC 9(02)   VALUE  90.
       01  SOUKO                   PIC N(06).
       01  K-1                     PIC X(05)  VALUE  X"1A24212078".
       01  K-2                     PIC X(05)  VALUE  X"1A24212474".
       COPY    LWMSG.
      *
           COPY  L-JNIF-RYO.
           COPY  LIHIM2.
           COPY  LITCM.
           COPY  L-JCON.
      *FD  P-F
       77  P-R                       PIC X(180).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-02.
           02  CLE-JS    PIC  X(01) VALUE " ".
           02  CLE-021   PIC  X(01) VALUE " ".
           02  CLE-022   PIC  X(01) VALUE " ".
           02  CLE-023   PIC  X(08) VALUE "        ".
           02  CLE-024   PIC  X(08) VALUE "        ".
           02  CLE-025   PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(17) VALUE  "                 ".
               03  FILLER  PIC  X(02) VALUE  "â◊".
               03  FILLER  PIC  X(06) VALUE  "éDÅEì¸".
               03  FILLER  PIC  X(02) VALUE  "ì˙".
               03  FILLER  PIC  X(02) VALUE  "ãL".
           02  DSP-00.
               03  FILLER  PIC  X(06) VALUE  "ã≥Å@àÁ".
               03  FILLER  PIC  X(05) VALUE  "=0 , ".
               03  FILLER  PIC  X(06) VALUE  "àÍÅ@î ".
               03  FILLER  PIC  X(05) VALUE  "=1 , ".
               03  FILLER  PIC  X(06) VALUE  "Ç`ÇkÇk".
               03  FILLER  PIC  X(06) VALUE  "=9    ".
           02  DSP-02.
               03  FILLER  PIC  X(10) VALUE  "ÇPÅ@î≠Å@çs".
           02  DSP-03.
               03  FILLER  PIC  X(18) VALUE  "ÇQÅ@çƒî≠çsÅ@Å@ëIë".
               03  FILLER  PIC  X(03) VALUE  "[ ]".
           02  DSP-05.
               03  FILLER  PIC  X(18) VALUE  "ÉeÉXÉgÉvÉäÉìÉgàÛéö".
               03  FILLER  PIC  X(16) VALUE  "(YES=1,NO=2) [ ]".
           02  DSP-07.
               03  FILLER  PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  DSP-08.
               03  FILLER  PIC  X(04) VALUE  "ÇsÇn".
           02  DSP-09.
               03  FILLER  PIC  X(06) VALUE  "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "Åj".
               03  FILLER  PIC  X(09) VALUE  "---> ÿ¿∞›".
       01  DSP-AREA1.
           02  DISP-15  PIC  X(39)  VALUE
                "ñ{é–=0 , ã ìá=1 , í√éR=2 , ëÅìá=3 ...  ".
           02  DSP-DNOM.
             03  FILLER  PIC  X(06) VALUE  "ì`ï[áÇ".
             03  FILLER  PIC  X(08) VALUE  "        ".
             03  FILLER  PIC  X(08) VALUE  "        ".
           02  DSP-DATM.
             03  FILLER  PIC  X(06) VALUE  "ì˙Å@ït".
             03  FILLER  PIC  X(08) VALUE  "  /  /  ".
             03  FILLER  PIC  X(08) VALUE  "  /  /  ".
       01  ACP-AREA.
           02  ACP-SIGN    PIC 9(01).
           02  ACP-JS      PIC 9(01).
           02  ACP-SEN     PIC 9(01).
           02  ACP-TST     PIC 9(01).
           02  ACP-FROM    PIC 9(06).
           02  ACP-TO      PIC 9(06).
           02  ACP-SNGP.
             03  ACP-SNEN  PIC 9(02).
             03  ACP-SGET  PIC 9(02).
             03  ACP-SPEY  PIC 9(02).
           02  ACP-ENGP.
             03  ACP-ENEN  PIC 9(02).
             03  ACP-EGET  PIC 9(02).
             03  ACP-EPEY  PIC 9(02).
           02  ACP-OK      PIC 9(01).
      *
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-02
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-JS" "X" "5" "54" "1" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-021" "X" "9" "45" "1" "CLE-JS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-022" "X" "12" "55" "1" "CLE-021" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-023" "X" "17" "31" "8" "CLE-022" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-024" "X" "19" "31" "8" "CLE-023" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-025" "X" "23" "62" "1" "CLE-024" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "166" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "29" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "25" "17" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "26" "2" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-01" "X" "1" "29" "6" "02DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "X" "1" "36" "2" "03DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-01" "X" "1" "39" "2" "04DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-00" " " "5" "0" "34" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-00" "X" "5" "21" "6" " " "DSP-00" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-00" "X" "5" "27" "5" "01DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-00" "X" "5" "32" "6" "02DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-00" "X" "5" "38" "5" "03DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-00" "X" "5" "43" "6" "04DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-00" "X" "5" "49" "6" "05DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "7" "0" "10" "DSP-00" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "7" "25" "10" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "9" "0" "21" "DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "9" "25" "18" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-03" "X" "9" "44" "3" "01DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-05" " " "12" "0" "34" "DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-05" "X" "12" "21" "18" " " "DSP-05" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-05" "X" "12" "41" "16" "01DSP-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "17" "0" "8" "DSP-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "17" "21" "8" " " "DSP-07" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" " " "19" "0" "4" "DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-08" "X" "19" "21" "4" " " "DSP-08" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" " " "23" "0" "26" "DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-09" "X" "23" "41" "6" " " "DSP-09" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-09" "X" "23" "47" "9" "01DSP-09" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-09" "X" "23" "56" "2" "02DSP-09" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-09" "X" "23" "58" "9" "03DSP-09" " " RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "83" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-15" "X" "3" "13" "39" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DNOM" " " "0" "0" "22" "DISP-15" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-DNOM" "X" "15" "31" "6" " " "DSP-DNOM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DNOM" "X" "17" "31" "8" "01DSP-DNOM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-DNOM" "X" "19" "31" "8" "02DSP-DNOM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DATM" " " "0" "0" "22" "DSP-DNOM" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01DSP-DATM" "X" "15" "31" "6" " " "DSP-DATM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DATM" "X" "17" "31" "8" "01DSP-DATM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-DATM" "X" "19" "31" "8" "02DSP-DATM" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SIGN" "9" "3" "51" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SIGN" BY REFERENCE JS-SIGN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JS" "9" "5" "54" "1" "ACP-SIGN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "9" "45" "1" "ACP-JS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TST" "9" "12" "55" "1" "ACP-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TST" BY REFERENCE W-TST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FROM" "9" "17" "31" "6" "ACP-TST" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TO" "9" "19" "31" "6" "ACP-FROM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNGP" " " "17" "0" "6" "ACP-TO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SNEN" "9" "17" "31" "2" " " "ACP-SNGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SNEN" BY REFERENCE W-SNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SGET" "9" "17" "34" "2" "ACP-SNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SGET" BY REFERENCE W-SGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SPEY" "9" "17" "37" "2" "ACP-SGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENGP" " " "19" "0" "6" "ACP-SNGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ENEN" "9" "19" "31" "2" " " "ACP-ENGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ENEN" BY REFERENCE W-ENEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EGET" "9" "19" "34" "2" "ACP-ENEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EGET" BY REFERENCE W-EGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-EPEY" "9" "19" "37" "2" "ACP-EGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "62" "1" "ACP-ENGP" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
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
           IF  JS-W    NOT =  1
               GO  TO  MR004
           END-IF
           CALL "SD_Output" USING "DISP-15" DISP-15 "p" RETURNING RESU.
       MR003.
           CALL "SD_Accept" USING BY REFERENCE ACP-SIGN "ACP-SIGN"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR003
           END-IF
           CALL "SD_Output" USING
            "ACP-SIGN" ACP-SIGN "p" RETURNING RESU.
           IF  JS-SIGN  NOT  =  0  AND  1  AND  2  AND  3
               GO  TO  MR003
           END-IF.
       MR004.
           IF (JS-W          =  2  OR  3)  OR
             ((JS-W          =  1)  AND  (JS-SIGN   NOT =  0))
               MOVE  9        TO  W-JS
               CALL "SD_Output" USING "ACP-JS" ACP-JS "p" RETURNING RESU
               GO  TO  MR010
           END-IF.
       MR005.
           CALL "SD_Accept" USING BY REFERENCE ACP-JS "ACP-JS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  =  "09"
               IF  JS-SIGN  NOT =  0
                   GO  TO  MR003
               END-IF
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR005
           END-IF
           CALL "SD_Output" USING "ACP-JS" ACP-JS "p" RETURNING RESU.
           IF  W-JS   NOT  =  0  AND  1  AND  9
               GO  TO  MR005
           END-IF.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  =  "09"
               IF (JS-W         =  0)  OR
                 ((JS-W          =  1)  AND  (JS-SIGN       =  0))
                   GO  TO  MR005
               ELSE
                   IF  JS-W          =  1
                       GO  TO  MR003
                   END-IF
               END-IF
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT  =  1   AND    2
               GO  TO  MR010
           END-IF
           IF  W-SEN =  2
               MOVE  "çƒ"    TO  M1-S
               MOVE  ZERO      TO  W-SNGP
               MOVE  99999999  TO  W-ENGP
               CALL "SD_Output" USING
                "DSP-DNOM" DSP-DNOM "p" RETURNING RESU
               GO  TO  MR020
           END-IF
      *
           PERFORM  SEL-RTN  THRU  SEL-EX.
           IF  W-SNGP = ZERO
               CALL "SD_Output" USING "ERR-02" ERR-02 "p" RETURNING RESU
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING
            "DSP-DATM" DSP-DATM "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-SNGP" ACP-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-ENGP" ACP-ENGP "p" RETURNING RESU.
           MOVE  ZERO    TO  W-FROM.
           MOVE  999999  TO  W-TO.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TST "ACP-TST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR020
           END-IF
           CALL "SD_Output" USING "ACP-TST" ACP-TST "p" RETURNING RESU.
           IF  W-TST  NOT  =  1   AND    2
               GO  TO  MR020
           END-IF
           IF  W-TST    =  1
               PERFORM    TST-RTN   THRU     TST-EX
               GO  TO  MR020
           END-IF
           IF  W-SEN       =   1
               GO  TO  MR050
           END-IF.
       MR030.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM "ACP-FROM"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR030
           END-IF
           CALL "SD_Output" USING
            "ACP-FROM" ACP-FROM "p" RETURNING RESU.
       MR040.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO "ACP-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR030
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR040
           END-IF
           CALL "SD_Output" USING "ACP-TO" ACP-TO "p" RETURNING RESU.
           IF  W-FROM   >  W-TO
               GO  TO  MR030
           END-IF
           GO  TO  MR050.
       MR042.
           CALL "SD_Accept" USING BY REFERENCE ACP-SNEN "ACP-SNEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MR042
           END-IF
           MOVE  20    TO  W-SNEN1.
       MR043.
           CALL "SD_Accept" USING BY REFERENCE ACP-SGET "ACP-SGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MR042
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MR043
           END-IF
           IF  W-SGET < 1  OR  > 12
               GO  TO  MR043
           END-IF.
       MR044.
           CALL "SD_Accept" USING BY REFERENCE ACP-SPEY "ACP-SPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MR043
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MR044
           END-IF
           IF  W-SGET < 1  OR  > 31
               GO  TO  MR044
           END-IF
           IF  W-SNGP < W-SDATE  OR  > W-EDATE
               GO  TO  MR042
           END-IF.
       MR046.
           CALL "SD_Accept" USING BY REFERENCE ACP-ENEN "ACP-ENEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MR044
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MR046
           END-IF
           MOVE  20    TO  W-ENEN1.
       MR047.
           CALL "SD_Accept" USING BY REFERENCE ACP-EGET "ACP-EGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MR046
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MR047
           END-IF
           IF  W-EGET < 1  OR  > 12
               GO  TO  MR047
           END-IF.
       MR048.
           CALL "SD_Accept" USING BY REFERENCE ACP-EPEY "ACP-EPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MR047
           END-IF
           IF  ESTAT NOT = "00"  AND  "01"  AND  "06"
               GO  TO  MR048
           END-IF
           IF  W-SPEY < 1  OR  > 31
               GO  TO  MR048
           END-IF
           IF  W-SNGP > W-ENGP
               GO  TO  MR046
           END-IF
           IF  W-SNGP < W-SDATE  OR  > W-EDATE
               GO  TO  MR046
           END-IF.
       MR050.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  W-SEN =  1
                   GO  TO  MR042
               ELSE
                   GO  TO  MR040
               END-IF
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR050
           END-IF
           IF  W-OK   NOT  =  "1"   AND      "9"
               GO  TO  MR050
           END-IF
           IF  W-OK        =  "9"
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               INITIALIZE   ACT-WORK
               GO  TO  MR010
           END-IF
      *
           IF  W-FOC     =  0
               MOVE  1       TO  W-FOC
               CALL "DB_F_Open" USING
                "I-O" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
                "JNIF1-KEY" BY REFERENCE JNIF1-KEY
               CALL "DB_F_Open" USING
                "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST
                "1" "HI-KEY2" BY REFERENCE HI-KEY2
               CALL "DB_F_Open" USING
                "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
                "TC-KEY" BY REFERENCE TC-KEY
               CALL "DB_F_Open" USING
                "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
                "JCON2-KEY" BY REFERENCE JCON2-KEY
           END-IF
           IF  W-POC     =  0
               MOVE  1       TO  W-POC
               CALL "PR_Open" RETURNING RESP
           END-IF
           MOVE    W-FROM   TO  JNIF1-01.
           MOVE    ZERO     TO  JNIF1-02.
       MR055.
      *           START   JNIF     KEY  NOT  <  JNIF1-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JNIF_PNAME1 "JNIF1-KEY" " NOT < " JNIF1-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           MOVE   "ON"      TO         STR-SW.
       MR060.
      *           READ    JNIF     NEXT       AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR990
           END-IF
           IF  JNIF1-02    =    7
               IF  OLD1        =    JNIF1-01
                   GO  TO  MR065
               ELSE
                   GO  TO  MR060
               END-IF
           END-IF
           MOVE     JNIF1-07    TO       SV-GYO.
           MOVE     JNIF1-051   TO       SV-TCD.
           IF  JS-SIGN  =  0
               IF  JNIF1-07    NOT  =  1
                   GO  TO  MR060
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JNIF1-07    NOT  =  6
                   GO  TO  MR060
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JNIF1-07    NOT  =  7
                   GO  TO  MR060
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JNIF1-07    NOT  =  4
                   GO  TO  MR060
               END-IF
           END-IF
           IF  W-JS      NOT =  9
               IF  JNIF1-13A   NOT =  W-JS
                   GO  TO  MR060
               END-IF
           END-IF
           MOVE  ZERO      TO  W-NGP.
           MOVE  JNIF1-04  TO  W-NGPS.
           MOVE  20        TO  W-NEN1.
           IF  W-NGP < W-SNGP  OR  > W-ENGP
               GO  TO  MR060
           END-IF
           MOVE   "OF"      TO         STR-SW.
           GO  TO   MR066.
       MR065.
           IF  JS-SIGN  =  0
               IF  SV-GYO      NOT  =  1
                   GO  TO  MR060
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  SV-GYO      NOT  =  6
                   GO  TO  MR060
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  SV-GYO      NOT  =  7
                   GO  TO  MR060
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  SV-GYO      NOT  =  4
                   GO  TO  MR060
               END-IF
           END-IF
           IF  W-JS      NOT =  9
               IF  JNIF2-07A   NOT =  W-JS
                   GO  TO  MR060
               END-IF
           END-IF
           MOVE   "OF"      TO         STR-SW.
       MR066.
           IF  W-SEN    =  1
               IF  (JNIF1-10    =   0)   AND     (JNIF1-11     =    1)
                   CONTINUE
               ELSE
                   GO  TO  MR060
               END-IF
           END-IF
           IF  W-SEN    =   2
               IF  JNIF1-10    =   1
                   CONTINUE
               ELSE
                   GO  TO  MR060
               END-IF
           END-IF
           IF  W-FROM   >  JNIF1-01
               GO  TO  MR060
           END-IF
           IF  W-TO     <  JNIF1-01
               MOVE  SPACE  TO  P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               GO  TO  MR999
           END-IF
           IF  JNIF1-14  NOT  =    W-OKNO
               MOVE   JNIF1-14     TO   W-OKNO
               MOVE   ZERO         TO   W-NO
           END-IF
           IF  JNIF1-01  NOT  =    OLD1
               PERFORM  HED-RTN  THRU   HED-EX
               MOVE   JNIF1-13     TO   W-CNT
               MOVE   JNIF1-01     TO      OLD1
           END-IF
           IF  JNIF1-02        =   7
               GO  TO  MR080
           END-IF
           PERFORM     MOV-RTN    THRU   MOV-EX.
           GO  TO  MR060.
       MR080.
           MOVE  SPACE     TO     R3-R.
           MOVE     K-1    TO     R3-K1.
           MOVE  M5-K2     TO     R3-K30.
           MOVE  M4-K5     TO     R3-K20.
           MOVE  JNIF2-03  TO     W-TEKI.
           MOVE  W-TEKI2   TO     P-TEKI1.
           MOVE  JNIF2-02A TO     P-TEKI2.
           MOVE  P-TEKI    TO     R3-01.
           MOVE   SOUKO    TO     R3-02.
           MOVE     K-2    TO     R3-K2.
           IF  W-SEN   =    1
               PERFORM    RW2-RTN     THRU     RW2-EX
           END-IF
           PERFORM     PRI-RTN    THRU     PRI-EX
               W-CNT       TIMES.
           INITIALIZE  R1-R     R2-R      MEI.
           MOVE  0  TO  SV-GYO.
           MOVE  ZERO TO  SV-TCD.
           GO  TO  MR060.
       MR990.
           IF  STR-SW  =  "ON"
               GO  TO  MR999
           END-IF
           MOVE  SPACE  TO  P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           ACCEPT        JS-W FROM ARGUMENT-VALUE.
           IF  JS-W       >  3
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  JS-W       =  0
               MOVE  0       TO  JS-SIGN
           END-IF
           IF  JS-W       =  2
               MOVE  1       TO  JS-SIGN
           END-IF
           IF  JS-W       =  3
               MOVE  3       TO  JS-SIGN
           END-IF
      *
           INITIALIZE     ACT-WORK.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           IF  W-FOC     =  1
               CALL "DB_F_Close" USING
                BY REFERENCE JNIF_IDLST JNIF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI2-M_IDLST HI2-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
           END-IF
           IF  W-POC     =  1
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      **********************************************
      *    ÇgÇdÇcÅ|ÇqÇsÇm                          *
      **********************************************
       HED-RTN.
           MOVE    JNIF1-05    TO  TC-KEY.
      *           READ    TC-M    WITH UNLOCK     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   SPACE   TO  TC-UNO
               MOVE  ALL "Å@"  TO  TC-NAME
               MOVE  ALL "Å@"  TO  TC-JSU
               MOVE  ALL "Å@"  TO  TC-JSS
           END-IF
           MOVE   TC-UNO    TO  M1-01.
           MOVE   JNIF1-01  TO  M1-02.
           MOVE   JNIF1-041 TO  M1-03.
           MOVE   JNIF1-042 TO  M1-04.
           MOVE   JNIF1-043 TO  M1-05.
           MOVE   JNIF1-14  TO  M1-06.
           MOVE   TC-JSU   TO  M2-01.
           MOVE   TC-JSS   TO  M3-01.
           IF  JNIF1-08    <   0
               MOVE      "Å|"      TO       M3-02
           ELSE
               MOVE      "Å@"      TO       M3-02
           END-IF
           IF  JNIF1-08  =  ZERO
               MOVE  "***"  TO    A
           ELSE
               MOVE   JNIF1-08 TO  AAR
           END-IF
           MOVE   A        TO  M4-01.
           MOVE   TC-NAME  TO  M5-01.
           MOVE   JNIF1-06 TO  JCON2-02.
           MOVE   "2"      TO  JCON2-01.
      *           READ   JCON     WITH UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  JCON2-03
           END-IF
           MOVE   JCON2-03 TO  M5-02.
           MOVE   JNIF1-07 TO  JCON3-02.
           MOVE   "3"      TO  JCON3-01.
      *           READ   JCON     WITH UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO  JCON3-03
           END-IF
           MOVE   JCON3-03 TO  SOUKO.
           IF  JNIF1-07    =   1
               IF  JNIF1-051   =   2654  OR  2656  OR  2657  OR  4038
                                         OR  4054  OR  4056  OR  4057
                   MOVE      "ïxÅ@émÅ@Å@Å@"  TO       SOUKO
               END-IF
           END-IF
           MOVE   SPACE    TO  M0-01.
           IF  JNIF1-051   =   2650  OR  2651  OR  2652
               IF  JNIF1-052 NOT =   001
                   MOVE   "ÅôÅôÅôÅôÅô"    TO  M0-01
               END-IF
           END-IF
           IF  JNIF1-051   =   7040
               MOVE   "ÅùÅùÅùÅùÅù"    TO  M0-01
           END-IF
           IF  JNIF1-051       =  4038  OR  4054  OR  4056  OR  4057
               MOVE  "542-0081"       TO  M2A-UNO
               MOVE  "Å@Å@Å@Å@Å@Å@" TO  M2A-JSO
               MOVE  "ëÂç„ésíÜâõãÊìÏëDèÍÅ@Å@Å@Å@Å@" TO  M2-JSN
               MOVE  "Å@Å@Å@Å@Å@Å@Å@ÇPíöñ⁄ÇPÇPÅ|ÇX" TO  M3-JS
               MOVE  "êÁã»éYã∆äîéÆâÔé–Å@Å@" TO  M4-NA
               MOVE  "06-6268-4561  " TO  M5-TEL
               GO  TO HED-EX
           END-IF
           IF  JNIF1-051       =  2654  OR  2656  OR  2657
               MOVE  "520-2132"       TO  M2A-UNO
               MOVE  "Å@Å@Å@Å@Å@Å@" TO  M2A-JSO
               MOVE  "é†âÍåßëÂí√ésê_óÃÇPÅ|ÇXÅ|ÇRÅ@" TO  M2-JSN
               MOVE  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@" TO  M3-JS
               MOVE  "äîéÆâÔé–êVì˙ñ{ã≥àÁÉVÉÖÅ[ÉYÅ@" TO  M4-NA
               MOVE  "077-543-1331  " TO  M5-TEL
               GO  TO HED-EX
           END-IF
           IF  JNIF1-052   NOT =  001
               IF  JNIF1-051       =  0459
                   MOVE  "663-8033"       TO  M2A-UNO
                   MOVE  "ï∫å…åßêºã{ésçÇñÿìåí¨ÇPÇTÅ|ÇP" TO  M2-JSN
                   MOVE  "äîéÆâÔé–è„éRõâã∆Å@Å@" TO  M4-NA
                   MOVE  "0798-64-5111  " TO  M5-TEL
                   GO  TO HED-EX.
           IF  JNIF1-052   NOT =  001
               IF  JNIF1-051       =  0460
                   MOVE  "663-8033"       TO  M2A-UNO
                   MOVE  "ï∫å…åßêºã{ésçÇñÿìåí¨ÇPÇTÅ|ÇP" TO  M2-JSN
                   MOVE  "äîéÆâÔé–ÉEÉGÉÑÉ}Å@Å@" TO  M4-NA
                   MOVE  "0798-67-0810  " TO  M5-TEL
                   GO  TO HED-EX
               END-IF
           END-IF
           IF  JNIF1-051       =  3015
               MOVE  "114-0034"       TO  M2A-UNO
               MOVE  "ìåãûìsñkãÊè„è\èÇSÅ|ÇPÅ|ÇTÅ@" TO  M2-JSN
               MOVE  "äîéÆâÔé–ÉXÉMÉÑÉ}Å@Å@" TO  M4-NA
               MOVE  "03-3909-8883  " TO  M5-TEL
               GO  TO HED-EX
           END-IF
           IF  JS-SIGN         =  0
               MOVE  "700-0975"       TO  M2A-UNO
               MOVE  "Å@Å@Å@Å@Å@Å@" TO  M2A-JSO
               MOVE  "â™éRésñkãÊç°ÇWíöñ⁄ÇPÇUÅ|ÇPÇV" TO  M2-JSN
               MOVE  "Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@" TO  M3-JS
               MOVE  "ì˙êiÉSÉÄäîéÆâÔé–Å@Å@" TO  M4-NA
           ELSE
               IF  JS-SIGN         =  1
                   MOVE  "713-8103"       TO  M2A-UNO
                   MOVE  "â™éRåßëqï~és" TO  M2A-JSO
                   MOVE  "ã ìáâ≥ìáéöêVñ©ÇWÇQÇUÇQÅ|ÇPÅ@" TO  M2-JSN
                   MOVE  "Å@Å@êÖìáç`çëç€ï®ó¨ÉZÉìÉ^Å[ì‡" TO  M3-JS
                   MOVE  "ì˙êiÉSÉÄáäÅ@ã ìáï®ó¨ÉZÉìÉ^Å[" TO  M4-NA
               ELSE
                   IF  JS-SIGN         =  2
                       MOVE  "709-3717"       TO  M2A-UNO
                       MOVE  "Å@Å@Å@Å@Å@Å@" TO  M2A-JSO
                       MOVE  "â™éRåßãvïƒåSÅ@Å@Å@Å@Å@Å@Å@Å@" TO  M2-JSN
                       MOVE  "Å@Å@î¸çÁí¨å¥ìcÇRÇQÇQÇVÅ|ÇPÅ@" TO  M3-JS
                       MOVE  "ì˙êiÉSÉÄáäÅ@í√éRï®ó¨ÉZÉìÉ^Å[" TO  M4-NA
                   ELSE
                       IF  JS-SIGN         =  3
                       MOVE  "701-0304"       TO  M2A-UNO
                       MOVE  "â™éRåßìsåEåS" TO  M2A-JSO
                       MOVE  "ëÅìáí¨ëÅìáÇSÇTÇOÇVÅ|ÇRÇVÅ@Å@" TO  M2-JSN
                       MOVE  "óºîıÇgÇcíÜélçëï®ó¨ÉZÉìÉ^Å[ì‡" TO  M3-JS
                       MOVE  "ì˙êiÉSÉÄÅ@ëÅìáîzëóÉZÉìÉ^Å[Å@" TO  M4-NA
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE  "086-243-2456  " TO  M5-TEL.
       HED-EX.
           EXIT.
      **********************************************
      *    ÇoÇqÇhÅ|ÇqÇsÇm                          *
      **********************************************
       PRI-RTN.
           MOVE SPACE  TO  P-R.
           IF  LCNT    NOT =  90
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE    SPACE  TO  W-P.
           MOVE    MID0-R TO  W-P.
           MOVE    W-P    TO  P-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    SPACE  TO  W-P.
           MOVE    MID1-R TO  W-P.
           MOVE    W-P    TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           ADD     1     TO  W-NO.
           MOVE    W-NO  TO  M2-02.
       PRI-005.
           MOVE    SPACE   TO  W-P.
           MOVE    MID2A-R TO  W-P.
           MOVE    W-P     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    SPACE   TO  W-P.
           MOVE    MID2-R  TO  W-P.
           MOVE    W-P     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    SPACE   TO  W-P.
           MOVE    MID3-R  TO  W-P.
           MOVE    W-P     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    SPACE   TO  W-P.
           MOVE    MID4-R  TO  W-P.
           MOVE    W-P     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    SPACE   TO  W-P.
           MOVE  MID5-R        TO  W-P.
           MOVE  W-P           TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  W-P.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    0    TO    I.
       PRI-010.
           ADD 1    TO    I.
           IF  I     >    6
               GO  TO  PRI-020
           END-IF
           MOVE    MEI1-R(I)     TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           MOVE    MEI2-R(I)     TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           GO  TO  PRI-010.
       PRI-020.
           MOVE    SPACE         TO    W-P.
           MOVE    R3-R          TO    W-P.
           MOVE    W-P           TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
      *
           MOVE    SPACE TO  R2-R.
           MOVE   K-1    TO  R2-K1.
           MOVE    SPACE TO  R2-01.
           MOVE   K-2    TO  R2-K2.
           MOVE   W-KEI  TO  R2-03.
           MOVE    SPACE         TO    W-P.
           MOVE    R2-R          TO    W-P.
           MOVE    W-P           TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
      *
           MOVE    0     TO  W-KEI.
           MOVE    7     TO  LCNT.
       PRI-EX.
           EXIT.
      ***************************************************
      *    ÇsÇrÇsÅ|ÇqÇsÇm                               *
      ***************************************************
       TST-RTN.
           IF  W-POC     =  0
               MOVE  1       TO  W-POC
               CALL "PR_Open" RETURNING RESP
           END-IF
           MOVE    SPACE   TO  P-R.
           IF  LCNT   NOT   =     90
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE   ALL "X" TO  M1-01.
           MOVE   ALL "9" TO  M1-02.
           MOVE   "99"    TO  M1-03.
           MOVE   "99"    TO  M1-04.
           MOVE   "99"    TO  M1-05.
           MOVE   ALL "9" TO  M1-06.
           MOVE   SPACE   TO  W-P.
           MOVE   MID1-R  TO  W-P.
           MOVE   W-P     TO  P-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO  P-R.
           MOVE   SPACE   TO  M2A-JSO.
           MOVE  ALL "Çm"   TO    M2-JSN.
           MOVE  ALL "Çm"   TO    M2-01.
           MOVE   ALL "X" TO  M2A-UNO.
           MOVE    SPACE   TO    W-P.
           MOVE    MID2A-R TO    W-P.
           MOVE    W-P     TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE   TO    P-R.
           MOVE    SPACE   TO    W-P.
           MOVE    MID2-R  TO    W-P.
           MOVE    W-P     TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE   TO    P-R.
           MOVE  ALL "Çm"   TO    M3-01.
           MOVE  ALL "Çm"   TO    M3-02.
           MOVE  ALL "Çm"   TO    M3-JS.
           MOVE  SPACE       TO    W-P.
           MOVE  MID3-R      TO    W-P.
           MOVE  W-P         TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE       TO    P-R.
           MOVE  ALL "Çm"   TO    M4-01.
           MOVE  ALL "Çm"   TO    M5-01.
           MOVE  ALL "Çm"   TO    M5-02.
           MOVE  ALL "Çm"   TO    M4-NA.
           MOVE  SPACE       TO    W-P.
           MOVE  MID4-R      TO    W-P.
           MOVE  W-P         TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE      TO    P-R.
           MOVE  ALL "X"       TO  M5-TEL.
           MOVE  MID5-R        TO  W-P.
           MOVE  W-P           TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    0         TO    LCNT.
       TST-010.
           MOVE    K-1     TO    R1-K1.
           MOVE  ALL "Çm" TO    R1-01.
           MOVE    K-2     TO    R1-K2.
           MOVE    SPACE   TO    W-P.
           MOVE    R1-R    TO    W-P.
           MOVE    W-P     TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE   TO    P-R.
           MOVE    K-1     TO    R2-K1.
           MOVE  ALL "Çm" TO    R2-01.
           MOVE    0       TO    I.
       TST-020.
           ADD 1       TO    I.
           IF  I       >     25
               GO  TO  TST-030
           END-IF
           MOVE  ALL "9"   TO    R2-021(I).
           GO  TO  TST-020.
       TST-030.
           MOVE  ALL "9"   TO    R2-03.
           MOVE  SPACE     TO    W-P.
           MOVE  R2-R      TO    W-P.
           MOVE  W-P       TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE      TO  P-R.
           ADD     1       TO  LCNT.
           IF  LCNT   NOT   =    6
               GO  TO  TST-010
           END-IF
           MOVE    K-1     TO   R3-K1.
           MOVE    M5-K2   TO   R3-K30.
           MOVE    M4-K5   TO   R3-K20.
           MOVE  ALL "Çm" TO   R3-01.
           MOVE  ALL "Çm" TO   R3-02.
           MOVE    K-2     TO   R3-K2.
           MOVE    SPACE   TO   W-P.
           MOVE    R3-R    TO   W-P.
           MOVE    W-P     TO   P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE      TO  P-R.
           ADD     1       TO  LCNT.
       TST-EX.
           EXIT.
      ***************************************************
      *    ÇlÇnÇuÅ|ÇqÇsÇm                               *
      ***************************************************
       MOV-RTN.
           MOVE   SPACE    TO  R1-R  R2-R  MEI1-R(JNIF1-02)
                                           MEI2-R(JNIF1-02).
           MOVE   K-1      TO  R1-K1.
           MOVE   JNIF1-03 TO  HI-MHCD HI-HCD.
      *           READ   HI2-M    WITH UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ZERO          TO  HI-ASSD
               MOVE   ZERO          TO  HI-HKB
               MOVE   ALL "Å@"  TO  HI-NAME HI-SMS
           END-IF
           MOVE   ALL "Å@"     TO  W-NNA  W-ONA  W-UNA.
           IF  SPACE         =   HI-NAME   AND  HI-SMS
               GO  TO  MOV-008
           END-IF
           IF  (HI-SMS    NOT =   SPACE)  AND  (JNIF1-07      =  6)
               MOVE   HI-SMS  TO  W-NNA
           ELSE
               MOVE   HI-NAME TO  W-NNA
           END-IF
           IF  JNIF1-051     =   0013  OR  0459  OR  0460
               MOVE  SPACE   TO  W-NNM(24)
           END-IF
           MOVE   25      TO  W-C1.
       MOV-001.
           SUBTRACT    1    FROM   W-C1.
           IF  W-C1          =    ZERO
               GO  TO  MOV-008
           END-IF
           IF  W-NNM(W-C1)   =   SPACE
               GO  TO  MOV-001
           END-IF
           IF  W-C1          <   15
               MOVE   W-NNA      TO  W-UNA
               GO  TO  MOV-008
           END-IF
           MOVE   ZERO    TO  W-C2.
       MOV-002.
           ADD         1    TO     W-C2.
           IF  W-C2    >    14
               MOVE   W-PNA1         TO  W-ONA
               MOVE   W-PNA2         TO  W-UNA
               GO  TO  MOV-006
           END-IF
           MOVE  W-NNM(W-C2)  TO   W-ONM(W-C2).
           IF  W-NNM(W-C2)  NOT =    SPACE
               GO  TO  MOV-002
           END-IF
           COMPUTE  W-C3   =  W-C1      -     W-C2.
           IF  W-C3    >    14
               GO  TO  MOV-002
           END-IF
      *
           MOVE  14           TO   W-C4.
       MOV-003.
           IF  W-C4          =    ZERO
               MOVE  "ÇcÇ`ÇsÇ`Å@ÉGÉâÅ["  TO  ERR-MSGN
               CALL "SD_Output" USING
                "DISP-MSG-01" DISP-MSG-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  MOV-006
           END-IF
           MOVE  W-NNM(W-C1)  TO   W-UNM(W-C4).
           SUBTRACT    1    FROM   W-C1  W-C4.
           IF  W-C1     NOT  =    W-C2
               GO  TO  MOV-003
           END-IF.
       MOV-006.
           IF  W-ONA         =    SPACE
               GO  TO  MOV-008
           END-IF
           IF  W-UNA         =    SPACE
               GO  TO  MOV-008
           END-IF
           IF  W-UNM(14) NOT =    SPACE
               GO  TO  MOV-008
           END-IF
           MOVE   15      TO  W-C1.
           MOVE   14      TO  W-C2.
       MOV-007.
           SUBTRACT    1    FROM   W-C1  W-C2.
           IF  W-C2     NOT  =    ZERO
               MOVE  W-UNM(W-C2)   TO  W-UNM(W-C1)
               GO  TO  MOV-007
           END-IF
           MOVE  SPACE         TO  W-UNM(W-C1).
           IF  W-UNM(14)     =    SPACE
               GO  TO  MOV-006
           END-IF.
       MOV-008.
           MOVE   W-ONA   TO  R1-01.
           MOVE   K-2     TO  R1-K2.
           MOVE   SPACE   TO  W-P.
           MOVE   R1-R    TO  W-P.
           MOVE   W-P     TO  MEI1-R(JNIF1-02).
           MOVE   K-1     TO  R2-K1.
           MOVE   W-UNA   TO  R2-01.
       MOV-010.
           COMPUTE W-T  = JNIF1-091(01) + JNIF1-091(02) + JNIF1-091(03)
                        + JNIF1-091(04) + JNIF1-091(05) + JNIF1-091(06)
                        + JNIF1-091(07) + JNIF1-091(08) + JNIF1-091(09)
                        + JNIF1-091(10) + JNIF1-091(11) + JNIF1-091(12)
                        + JNIF1-091(13) + JNIF1-091(14) + JNIF1-091(15)
                        + JNIF1-091(16) + JNIF1-091(17) + JNIF1-091(18)
                        + JNIF1-091(19) + JNIF1-091(20) + JNIF1-091(21)
                        + JNIF1-091(22) + JNIF1-091(23) + JNIF1-091(24)
                        + JNIF1-091(25) + JNIF1-091(26) + JNIF1-091(27).
           IF  (HI-S1(01) NOT = 0)  OR (HI-S1(02) NOT = 0) OR
               (HI-S1(03) NOT = 0)  OR (HI-S1(04) NOT = 0) OR
               (HI-S1(05) NOT = 0)  OR (HI-S1(06) NOT = 0) OR
               (HI-S1(07) NOT = 0)
               GO  TO  MOV-015
           END-IF
           IF  HI-HKB        = 0
               MOVE    JNIF1-091(02)    TO   R2-021(01)
               MOVE    JNIF1-091(04)    TO   R2-021(02)
               MOVE    JNIF1-091(05)    TO   R2-021(03)
               MOVE    JNIF1-091(06)    TO   R2-021(04)
               MOVE    JNIF1-091(07)    TO   R2-021(05)
               MOVE    JNIF1-091(08)    TO   R2-021(06)
               MOVE    JNIF1-091(09)    TO   R2-021(07)
               MOVE    JNIF1-091(10)    TO   R2-021(08)
               MOVE    JNIF1-091(11)    TO   R2-021(09)
               MOVE    JNIF1-091(12)    TO   R2-021(10)
               MOVE    JNIF1-091(13)    TO   R2-021(11)
               MOVE    JNIF1-091(14)    TO   R2-021(12)
               MOVE    JNIF1-091(15)    TO   R2-021(13)
               MOVE    JNIF1-091(16)    TO   R2-021(14)
               MOVE    JNIF1-091(17)    TO   R2-021(15)
               MOVE    JNIF1-091(18)    TO   R2-021(16)
               MOVE    JNIF1-091(19)    TO   R2-021(17)
               MOVE    JNIF1-091(20)    TO   R2-021(18)
               MOVE    JNIF1-091(21)    TO   R2-021(19)
               MOVE    JNIF1-091(22)    TO   R2-021(20)
               MOVE    JNIF1-091(23)    TO   R2-021(21)
               MOVE    JNIF1-091(24)    TO   R2-021(22)
               MOVE    JNIF1-091(25)    TO   R2-021(23)
               MOVE    JNIF1-091(26)    TO   R2-021(24)
               MOVE    JNIF1-091(27)    TO   R2-021(25)
               GO  TO  MOV-020
           END-IF
           MOVE    JNIF1-091(02)    TO   R2-B021(01).
           IF  JNIF1-091(03)  NOT  =  ZERO
               MOVE    JNIF1-091(03)    TO   R2-B021(01)
           END-IF
           MOVE    JNIF1-091(04)    TO   R2-B021(02).
           MOVE    JNIF1-091(05)    TO   R2-B021(03).
           MOVE    JNIF1-091(06)    TO   R2-B021(04).
           MOVE    JNIF1-091(07)    TO   R2-B021(05).
           MOVE    JNIF1-091(08)    TO   R2-B021(06).
           MOVE    JNIF1-091(09)    TO   R2-B021(07).
           MOVE    JNIF1-091(10)    TO   R2-B021(08).
           MOVE    JNIF1-091(11)    TO   R2-B021(09).
           MOVE    JNIF1-091(12)    TO   R2-B021(10).
           MOVE    JNIF1-091(13)    TO   R2-B021(11).
           MOVE    JNIF1-091(14)    TO   R2-B021(12).
           MOVE    JNIF1-091(15)    TO   R2-B021(13).
           MOVE    JNIF1-091(16)    TO   R2-B021(14).
           MOVE    JNIF1-091(17)    TO   R2-B021(15).
           MOVE    JNIF1-091(18)    TO   R2-B021(16).
           MOVE    JNIF1-091(19)    TO   R2-B021(17).
           MOVE    JNIF1-091(20)    TO   R2-B021(18).
           MOVE    JNIF1-091(21)    TO   R2-B021(19).
           MOVE    JNIF1-091(22)    TO   R2-B021(20).
           MOVE    JNIF1-091(23)    TO   R2-B021(21).
           MOVE    JNIF1-091(24)    TO   R2-B021(22).
           MOVE    JNIF1-091(25)    TO   R2-B021(23).
           MOVE    JNIF1-091(26)    TO   R2-B021(24).
           MOVE    JNIF1-091(27)    TO   R2-B021(25).
           IF (JNIF1-091(02)   NOT =  ZERO)  OR
             ((JNIF1-091(03)   NOT =  ZERO)  AND
               (HI-HKB   NOT  = 0))
               MOVE    "*"              TO   R2-B020(01)
           END-IF
           IF  JNIF1-091(04)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(02)
           END-IF
           IF  JNIF1-091(05)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(03)
           END-IF
           IF  JNIF1-091(06)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(04)
           END-IF
           IF  JNIF1-091(07)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(05)
           END-IF
           IF  JNIF1-091(08)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(06)
           END-IF
           IF  JNIF1-091(09)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(07)
           END-IF
           IF  JNIF1-091(10)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(08)
           END-IF
           IF  JNIF1-091(11)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(09)
           END-IF
           IF  JNIF1-091(12)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(10)
           END-IF
           IF  JNIF1-091(13)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(11)
           END-IF
           IF  JNIF1-091(14)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(12)
           END-IF
           IF  JNIF1-091(15)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(13)
           END-IF
           IF  JNIF1-091(16)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(14)
           END-IF
           IF  JNIF1-091(17)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(15)
           END-IF
           IF  JNIF1-091(18)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(16)
           END-IF
           IF  JNIF1-091(19)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(17)
           END-IF
           IF  JNIF1-091(20)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(18)
           END-IF
           IF  JNIF1-091(21)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(19)
           END-IF
           IF  JNIF1-091(22)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(20)
           END-IF
           IF  JNIF1-091(23)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(21)
           END-IF
           IF  JNIF1-091(24)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(22)
           END-IF
           IF  JNIF1-091(25)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(23)
           END-IF
           IF  JNIF1-091(26)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(24)
           END-IF
           IF  JNIF1-091(27)   NOT =  ZERO
               MOVE    "*"              TO   R2-B020(25)
           END-IF
           GO  TO  MOV-020.
       MOV-015.
           MOVE    JNIF1-091(01)    TO   R2-021(01).
           MOVE    JNIF1-091(02)    TO   R2-021(02).
           MOVE    JNIF1-091(03)    TO   R2-021(03).
           MOVE    JNIF1-091(04)    TO   R2-021(04).
           MOVE    JNIF1-091(05)    TO   R2-021(05).
           MOVE    JNIF1-091(06)    TO   R2-021(06).
           MOVE    JNIF1-091(07)    TO   R2-021(07).
           MOVE    JNIF1-091(25)    TO   R2-021(23).
           MOVE    JNIF1-091(26)    TO   R2-021(24).
           MOVE    JNIF1-091(27)    TO   R2-021(25).
       MOV-020.
           MOVE    W-T     TO    R2-03.
           MOVE    SPACE   TO    W-P.
           MOVE    R2-R    TO    W-P.
           MOVE    W-P     TO    MEI2-R(JNIF1-02).
           ADD     W-T     TO  W-KEI.
           MOVE    0       TO  W-T.
           IF  W-SEN    =    1
               PERFORM      RW1-RTN        THRU        RW1-EX
           END-IF.
       MOV-EX.
           EXIT.
      ***************************************************
      *    ÇqÇvÇPÅ[ÇqÇsÇm                               *
      ***************************************************
       RW1-RTN.
           MOVE   "1"     TO  JNIF1-10.
      *           REWRITE        JNIF1-R       INVALID
      *///////////////
           CALL "DB_Update" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               MOVE       "JNIF"  TO    ERR-F
               MOVE       "R"     TO    ERR-M
               MOVE    JNIF1-KEY  TO    ERR-K
               PERFORM   ERR-RTN   THRU   ERR-EX
           END-IF.
       RW1-EX.
      ***************************************************
      *    ÇqÇvÇQÅ[ÇqÇsÇm                               *
      ***************************************************
       RW2-RTN.
           MOVE   "1"     TO  JNIF2-04.
      *           REWRITE        JNIF2-R       INVALID
      *///////////////
           CALL "DB_Update" USING
            JNIF_PNAME1 JNIF_LNAME JNIF2-R RETURNING RET.
           IF  RET = 1
               MOVE       "JNIF"  TO    ERR-F
               MOVE       "R"     TO    ERR-M
               MOVE    JNIF2-KEY  TO    ERR-K
               PERFORM   ERR-RTN   THRU   ERR-EX
           END-IF.
       RW2-EX.
           EXIT.
      ***************************************************
      *    À¬ﬁπ æØƒ                ROUTIN               *
      ***************************************************
       SEL-RTN.
           MOVE  ZERO  TO  W-SDATE  W-EDATE  W-SNGP  W-ENGP.
           CALL "DB_F_Open" USING
            "INPUT" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
       SEL-010.
      *           READ  JNIF  NEXT  RECORD  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JNIF_PNAME1 BY REFERENCE JNIF-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SEL-090
           END-IF
           IF  JNIF1-10 = 1
               GO  TO  SEL-010
           END-IF
           IF  JNIF1-02    =    7
               GO  TO  SEL-010
           END-IF
           IF  JS-SIGN  =  0
               IF  JNIF1-07    NOT =  1
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JNIF1-07    NOT =  6
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  2
               IF  JNIF1-07    NOT =  7
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  JS-SIGN  =  3
               IF  JNIF1-07    NOT =  4
                   GO  TO  SEL-010
               END-IF
           END-IF
           IF  W-JS      NOT =  9
               IF  JNIF1-13A   NOT =  W-JS
                   GO  TO  SEL-010
               END-IF
           END-IF
           MOVE  ZERO     TO  W-NGP.
           MOVE  JNIF1-04  TO  W-NGPS.
           MOVE  20       TO  W-NEN1.
           IF  W-SDATE = ZERO
               MOVE  W-NGP  TO  W-SDATE
           END-IF
           IF  W-SDATE > W-NGP
               MOVE  W-NGP  TO  W-SDATE
           END-IF
           IF  W-EDATE < W-NGP
               MOVE  W-NGP  TO  W-EDATE
           END-IF
           GO  TO  SEL-010.
       SEL-090.
           CALL "DB_F_Close" USING BY REFERENCE JNIF_IDLST JNIF_PNAME1.
           MOVE  W-SDATE  TO  W-SNGP.
           MOVE  W-EDATE  TO  W-ENGP.
       SEL-EX.
           EXIT.
       COPY    LPMSG.
