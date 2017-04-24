       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS60I.
      *************************************************************
      *    PROGRAM         :  ìùàÍì`ï[ì¸óÕÅiê‘ÇøÇ·ÇÒñ{ï‹ÇeÇ`Çwï™Åj*
      *    PRINTER TYPE    :  JIPS                                *
      *    SCREEN          :  SJH60I                              *
      *    COMPILE TYPE    :  COBOL                               *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       01  W-CRT.
           02  W-HNO          PIC  9(009).
           02  W-HNGP         PIC  9(006).
           02  W-HNGPD REDEFINES W-HNGP.
             03  W-HNEN       PIC  9(002).
             03  W-HGET       PIC  9(002).
             03  W-HPEY       PIC  9(002).
           02  W-NNGP         PIC  9(006).
           02  W-NNGPD REDEFINES W-NNGP.
             03  W-NNEN       PIC  9(002).
             03  W-NGET       PIC  9(002).
             03  W-NPEY       PIC  9(002).
           02  W-BI           PIC  X(010).
           02  W-SNGP         PIC  9(008).
           02  W-SNGPL REDEFINES W-SNGP.
             03  F            PIC  9(002).
             03  W-SNGPS      PIC  9(006).
           02  W-HNA          PIC  X(006).
           02  W-ZON          PIC  X(004).
           02  W-DNGP         PIC  9(008).
           02  W-DNGPL REDEFINES W-DNGP.
             03  W-DNEN1      PIC  9(002).
             03  W-DNGPS      PIC  9(006).
           02  W-MEI.
             03  W-MEID  OCCURS   9.
               04  W-HCD      PIC  9(006).
               04  W-S        PIC  9(003).
               04  W-JAN      PIC  X(013).
               04  W-SU       PIC  9(005).
               04  W-GTN      PIC  9(007).
               04  W-UTN      PIC  9(007).
               04  W-GKIN     PIC  9(009).
               04  W-UKIN     PIC  9(009).
               04  W-SHM      PIC  X(013).
               04  W-MSB      PIC  X(010).
               04  W-COR      PIC  N(004).
               04  W-SZM      PIC  X(004).
               04  W-SIZM     PIC  N(004).
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-STC          PIC  9(007).
           02  W-DNO          PIC  9(007).
           02  W-NRC          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-SDATE        PIC  9(006).
           02  W-EDATE        PIC  9(006).
           02  W-NGP.
             03  W-NENL       PIC  9(004).
             03  W-NEND  REDEFINES W-NENL.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  W-L1           PIC  9(002).
           02  W-L2           PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-SC           PIC  9(001).
           02  W-SNO          PIC  9(002).
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   4.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-DSZ          PIC  X(004).
           02  W-DSZD  REDEFINES W-DSZ.
             03  F            PIC  X(003).
             03  W-DSZH       PIC  X(001).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5        ".
           02  W-ASZD.
             03  W-ASZ   OCCURS   4.
               04  W-SZD   OCCURS  10.
                 05  W-SZ     PIC  X(003).
           02  W-MSZ.
             03  F            PIC  X(030) VALUE
                  "                     280290300".
             03  F            PIC  X(030) VALUE
                  "125130135140150160170180190200".
             03  F            PIC  X(030) VALUE
                  "210215220225230235240245250   ".
             03  F            PIC  X(030) VALUE
                  "240245250255260265270275      ".
           02  W-AHN          PIC  N(024).
           02  W-AHND  REDEFINES W-AHN.
             03  W-HND   OCCURS  24.
               04  W-HN       PIC  N(001).
           02  W-CORD         PIC  N(004).
           02  W-ANAD  REDEFINES W-CORD.
             03  W-NAD  OCCURS   4.
               04  W-NA       PIC  N(001).
           02  W-C1           PIC  9(002).
           02  W-C2           PIC  9(002).
           COPY LSTAT.
      *
           COPY LITDNA.
           COPY LIAHNH.
           COPY LRCODE.
           COPY L-JCON.
           COPY LIDTHT.
           COPY LIHIM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  FILLER.
             03  A-ACT   PIC  9(001).
             03  A-DNO   PIC  9(007).
             03  A-HNO   PIC  9(009).
           02  FILLER.
             03  A-STC   PIC  9(007).
             03  A-HNGP  PIC  9(006).
             03  A-NNGP  PIC  9(006).
           02  FILLER.
             03  A-HCD   PIC  9(006).
             03  A-S     PIC  9(003).
           02  FILLER.
             03  A-SU    PIC  9(005).
             03  A-MSB   PIC  X(010).
           02  FILLER.
             03  A-BI    PIC  X(010).
             03  A-SNGP  PIC  9(006).
           02  FILLER.
             03  A-HNA   PIC  X(006).
             03  A-ZON   PIC  X(004).
             03  A-DNGP  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DNOC  PIC  X(007) VALUE
                "       ".
           02  D-NHSN  PIC  N(016).
           02  FILLER.
             03  D-HNA   PIC  N(024).
             03  D-SIZM  PIC  N(004).
           02  FILLER.
             03  D-SU    PIC ZZZZ9 .
             03  D-GTN   PIC ZZZZ,ZZ9 .
             03  D-GKIN  PIC ZZ,ZZZ,ZZ9 .
             03  D-UTN   PIC ZZZZ,ZZ9 .
             03  D-UKIN  PIC ZZ,ZZZ,ZZ9 .
           02  D-TOT.
             03  FILLER  PIC ZZ,ZZ9 .
             03  FILLER  PIC ZZ,ZZZ,ZZ9 .
             03  FILLER  PIC ZZ,ZZZ,ZZ9 .
       01  C-SPC.
           02  FILLER.
             03  FILLER  PIC  X(050) VALUE
                  "                                                  ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(003) VALUE "   ".
             03  FILLER  PIC  X(020) VALUE "                    ".
           02  FILLER.
             03  FILLER  PIC  X(050) VALUE
                  "                                                  ".
             03  FILLER  PIC  X(004) VALUE "    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ≈º  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  –ƒ≥€∏  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  º¨√› ≈º  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  À›“≤ ≈º  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  ª≤Ωﬁ ≈º  ***".
             03  E-ME6   PIC  X(019) VALUE
                  "***  JAN –ƒ≥ƒ∏  ***".
             03  E-ME7   PIC  X(034) VALUE
                  "***  ºﬁ≠º›√ﬁ∞¿ º≠≥æ≤•ª∏ºﬁÆ Ã∂  ***".
             03  E-ME8   PIC  X(021) VALUE
                  "***  PROGRAM ¥◊∞  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  DATA ¥◊∞  ***".
             03  E-ME10  PIC  X(017) VALUE
                  "***  JAN ¥◊∞  ***".
             03  E-ME11  PIC  X(016) VALUE
                  "***  ¿›∂ ≈º  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  JAN ∂≈“≤ ≈º  ***".
             03  E-ME13  PIC  X(024) VALUE
                  "***  ì`ï[î≠çsÅ@çœÇ›  ***".
             03  E-ME20  PIC  X(025) VALUE
                  "***  TDNAF WRITE ¥◊∞  ***".
             03  E-ME21  PIC  X(027) VALUE
                  "***  TDNAF REWRITE ¥◊∞  ***".
             03  E-ME22  PIC  X(026) VALUE
                  "***  TDNAF DELETE ¥◊∞  ***".
             03  E-ME24  PIC  X(017) VALUE
                  "***  JCON ≈º  ***".
             03  E-ME25  PIC  X(026) VALUE
                  "***  JCON REWRITE ¥◊∞  ***".
             03  E-TDNA  PIC  X(016).
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "93" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "1" "0" "17" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "1" "47" "1" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "1" "57" "7" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HNO" "9" "1" "72" "9" "A-DNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HNO" BY REFERENCE W-HNO "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "2" "0" "19" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STC" "9" "2" "10" "7" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STC" BY REFERENCE W-STC "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HNGP" "9" "2" "61" "6" "A-STC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HNGP" BY REFERENCE W-HNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NNGP" "9" "2" "75" "6" "A-HNGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NNGP" BY REFERENCE W-NNGP "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "W-L1" "0" "9" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-L1" "1" "6" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD(1) "6" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S" "9" "W-L1" "57" "3" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S" BY REFERENCE W-S(1) "3" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "W-L2" "0" "15" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "9" "W-L2" "27" "5" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU(1) "5" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MSB" "X" "W-L2" "71" "10" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MSB" BY REFERENCE W-MSB(1) "10" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "22" "0" "16" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BI" "X" "22" "1" "10" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BI" BY REFERENCE W-BI "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SNGP" "9" "22" "13" "6" "A-BI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SNGP" BY REFERENCE W-SNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "23" "0" "16" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HNA" "X" "23" "8" "6" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HNA" BY REFERENCE W-HNA "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZON" "X" "23" "20" "4" "A-HNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZON" BY REFERENCE W-ZON "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNGP" "9" "23" "32" "6" "A-ZON" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNGP" BY REFERENCE W-DNGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "162" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNOC" "X" "1" "57" "7" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NHSN" "N" "2" "18" "32" "D-DNOC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NHSN" BY REFERENCE AHNH-NHSN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-L1" "0" "56" "D-NHSN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "W-L1" "8" "48" " " "03C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZM" "N" "W-L1" "61" "8" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZM" BY REFERENCE W-SIZM(1) "8" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L2" "0" "41" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZ9" "W-L2" "27" "5" " " "04C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU(1) "5" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
           "D-GTN" "ZZZZ,ZZ9" "W-L2" "32" "8" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GTN" BY REFERENCE W-GTN(1) "7" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKIN" "ZZ,ZZZ,ZZ9" "W-L2" "41" "10" "D-GTN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GKIN" BY REFERENCE W-GKIN(1) "9" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UTN" "ZZZZ,ZZ9" "W-L2" "51" "8" "D-GKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTN" BY REFERENCE W-UTN(1) "7" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UKIN" "ZZ,ZZZ,ZZ9" "W-L2" "60" "10" "D-UTN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UKIN" BY REFERENCE W-UKIN(1) "9" "1" BY REFERENCE CNT 102
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TOT" " " "22" "0" "26" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TOT" "ZZ,ZZ9" "22" "26" "6" " " "D-TOT" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TOT" BY REFERENCE WT-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TOT" "ZZ,ZZZ,ZZ9" "22" "41" "10" "01D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TOT" BY REFERENCE WT-GKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TOT" "ZZ,ZZZ,ZZ9" "22" "60" "10" "02D-TOT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TOT" BY REFERENCE WT-UKIN "9" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "132" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-SPC" " " "W-L1" "0" "78" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-SPC" "X" "W-L1" "1" "50" " " "01C-SPC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-SPC" "X" "W-L1" "51" "5" "0101C-SPC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301C-SPC" "X" "W-L1" "57" "3" "0201C-SPC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0401C-SPC" "X" "W-L1" "61" "20" "0301C-SPC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-SPC" " " "W-L2" "0" "54" "01C-SPC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-SPC" "X" "W-L2" "27" "50" " " "02C-SPC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-SPC" "X" "W-L2" "77" "4" "0102C-SPC" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "390" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "390" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "19" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "34" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "21" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "17" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "16" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "21" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "24" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "25" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "27" "E-ME20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "26" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME24" "X" "24" "15" "17" "E-ME22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" "X" "24" "15" "26" "E-ME24" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TDNA" "X" "24" "43" "16" "E-ME25" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TDNA" BY REFERENCE TDNA-KEY "16" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH60I" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE W-MSIZ TO W-ASIZD.
           MOVE W-MSZ TO W-ASZD.
           MOVE ZERO TO W-STC.
           ACCEPT W-NGPS FROM DATE.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN2
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NGPS TO W-SDATE.
           ACCEPT W-NGPS FROM DATE.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN2
               MOVE 1 TO W-GET
           END-IF
           MOVE W-NGPS TO W-EDATE.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT = 9
               GO TO M-90
           END-IF
           IF  W-ACT < 1 OR > 3
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJH60I" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           IF  W-STC NOT = ZERO
               CALL "SD_Output" USING "A-STC" A-STC "p" RETURNING RESU
               CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-STC "A-STC" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           MOVE W-STC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-20
           END-IF
           CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "SD_Output" USING "D-DNOC" D-DNOC "p" RETURNING RESU
               GO TO M-30
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           MOVE SPACE TO TDNA-KEY.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
      *           START TDNAF KEY NOT < TDNA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNAF_PNAME1 "TDNA-KEY" " NOT < " TDNA-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF (W-STC NOT = TDNA-STC) OR (W-DNO NOT = TDNA-DNO)
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF
           IF  TDNA-PC = 9
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-NRC = 0
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-25
           END-IF.
       M-30.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-20
               ELSE
                   GO TO M-25
               END-IF
           END-IF
           IF  W-DMM = 9
               IF  W-ACT = 3
                   GO TO M-25
               END-IF
           END-IF
           PERFORM UPD-RTN THRU UPD-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-90
           END-IF
           GO TO M-15.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCON_IDLST JCON_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       DSP-RTN.
           MOVE ZERO TO W-CRT W-NRC.
           MOVE SPACE TO W-BI W-HNA W-ZON
                         W-JAN(1) W-JAN(2) W-JAN(3) W-JAN(4) W-JAN(5)
                         W-JAN(6) W-JAN(7) W-JAN(8) W-JAN(9)
                         W-SHM(1) W-SHM(2) W-SHM(3) W-SHM(4) W-SHM(5)
                         W-SHM(6) W-SHM(7) W-SHM(8) W-SHM(9)
                         W-MSB(1) W-MSB(2) W-MSB(3) W-MSB(4) W-MSB(5)
                         W-MSB(6) W-MSB(7) W-MSB(8) W-MSB(9)
                         W-SZM(1) W-SZM(2) W-SZM(3) W-SZM(4) W-SZM(5)
                         W-SZM(6) W-SZM(7) W-SZM(8) W-SZM(9)
                      W-SIZM(1) W-SIZM(2) W-SIZM(3) W-SIZM(4) W-SIZM(5)
                      W-SIZM(6) W-SIZM(7) W-SIZM(8) W-SIZM(9).
           MOVE TDNA-NRC TO W-NRC.
           MOVE TDNA-HNO TO W-HNO.
           MOVE TDNA-HNGP TO W-HNGP.
           MOVE TDNA-NNGP TO W-NNGP.
           MOVE TDNA-BI TO W-BI.
           MOVE TDNA-SNGP TO W-SNGP.
           MOVE TDNA-HNA TO W-HNA.
           MOVE TDNA-ZON TO W-ZON.
           MOVE TDNA-DNGP TO W-DNGP.
      *
           MOVE ZERO TO CNT WT-D.
       DSP-010.
           ADD 1 TO CNT.
           IF  CNT > 9
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
           MOVE TDNA-JAN TO W-JAN(CNT).
           MOVE TDNA-SU TO W-SU(CNT).
           MOVE TDNA-GTN TO W-GTN(CNT).
           MOVE TDNA-GKIN TO W-GKIN(CNT).
           MOVE TDNA-UTN TO W-UTN(CNT).
           MOVE TDNA-UKIN TO W-UKIN(CNT).
           MOVE TDNA-SHM TO W-SHM(CNT).
           MOVE TDNA-MSB TO W-MSB(CNT).
      *
           ADD TDNA-SU TO WT-SU.
           ADD TDNA-GKIN TO WT-GKIN.
           ADD TDNA-UKIN TO WT-UKIN.
      *
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
      *           SELECT CODEF WHERE 0000 = CODE-TCD AND TDNA-JAN = CODE-JAN.
      *///////////////
           CALL "DB_Select" USING
            CODEF_PNAME1 "WHERE" 
            "CODE-TCD" "=" "0000" "AND"
            "CODE-JAN" "=" TDNA-JAN RETURNING RET.
      *           READ CODEF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING CODEF_PNAME1
               MOVE ZERO TO W-HCD(CNT)
               MOVE SPACE TO W-SIZM(CNT) W-SZM(CNT)
               GO TO DSP-020
           END-IF
           IF (CODE-SIZ < 1 OR > 4) OR (CODE-SNO < 1 OR > 10)
               MOVE ZERO TO W-HCD(CNT)
               MOVE SPACE TO W-SIZM(CNT) W-SZM(CNT)
               GO TO DSP-020
           END-IF
           MOVE W-SIZ(CODE-SIZ,CODE-SNO) TO W-DSZ.
           IF  HI-HKB = 1
               MOVE 5 TO W-DSZH
           END-IF
           MOVE W-SZ(CODE-SIZ,CODE-SNO) TO W-S(CNT).
           MOVE W-DSZ TO W-SIZM(CNT) W-SZM(CNT).
           MOVE CODE-HCD TO W-HCD(CNT).
       DSP-020.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO DSP-030
           END-IF
           IF (W-STC NOT = TDNA-STC) OR (W-DNO NOT = TDNA-DNO)
               GO TO DSP-030
           END-IF
           GO TO DSP-010.
       DSP-030.
           CALL "SD_Output" USING "D-NHSN" D-NHSN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HNO" A-HNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HNGP" A-HNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-NNGP" A-NNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BI" A-BI "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SNGP" A-SNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HNA" A-HNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-ZON" A-ZON "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DNGP" A-DNGP "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TOT" D-TOT "p" RETURNING RESU.
      *
           MOVE ZERO TO CNT.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
       DSP-040.
           ADD 1 TO CNT.
           IF  CNT > 9
               GO TO DSP-EX
           END-IF
           ADD 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1 > 20
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
      *
           IF  W-SU(CNT) = ZERO
               CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU
               GO TO DSP-040
           END-IF
           MOVE W-HCD(CNT) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "Å@ïiñºÇ»ÇµÅ@" TO HI-NAME
           END-IF
      *
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S" A-S "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SIZM" D-SIZM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           CALL "SD_Output" USING "D-GTN" D-GTN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTN" D-UTN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-MSB" A-MSB "p" RETURNING RESU.
           GO TO DSP-040.
       DSP-EX.
           EXIT.
       ACP-RTN.
           IF  W-ACT = 3
               GO TO ACP-280
           END-IF.
       ACP-005.
           CALL "SD_Accept" USING BY REFERENCE A-HNO "A-HNO" "9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-005
           END-IF.
       ACP-010.
           CALL "SD_Accept" USING BY REFERENCE A-HNGP "A-HNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-005
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-010
           END-IF
           IF  W-HNGP = ZERO
               GO TO ACP-020
           END-IF
           IF  W-HNEN < 09
               GO TO ACP-010
           END-IF
           IF  W-HGET < 01 OR > 12
               GO TO ACP-010
           END-IF
           IF  W-HPEY < 01 OR > 31
               GO TO ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-NNGP "A-NNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-010
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-NNGP = ZERO
               GO TO ACP-030
           END-IF
           IF  W-NNEN < 09
               GO TO ACP-020
           END-IF
           IF  W-NGET < 01 OR > 12
               GO TO ACP-020
           END-IF
           IF  W-NPEY < 01 OR > 31
               GO TO ACP-020
           END-IF
           IF  W-HNGP > W-NNGP
               GO TO ACP-020
           END-IF.
       ACP-030.
           MOVE ZERO TO CNT.
           MOVE 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
       ACP-040.
           ADD 1 TO CNT.
           ADD 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  CNT > 9
               GO TO ACP-150
           END-IF
           IF  W-L1 > 20
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO ACP-EX
           END-IF.
       ACP-050.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-130
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-050
           END-IF
           MOVE W-HCD(CNT) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-050
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           PERFORM COR-RTN THRU COR-EX.
           MOVE W-CORD TO W-COR(CNT).
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-S "A-S" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-050
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           PERFORM SIZ-RTN THRU SIZ-EX.
           IF  W-SC = 5
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           CALL "SD_Output" USING "D-SIZM" D-SIZM "p" RETURNING RESU.
           IF  HI-S(W-SC,W-SNO) = 0
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
      *           SELECT CODEF WHERE 0000 = CODE-TCD AND W-HCD(CNT) = CODE-HCD
      *                               AND W-SC = CODE-SIZ AND W-SNO = CODE-SNO.
      *///////////////
           CALL "DB_Select" USING
            CODEF_PNAME1 "WHERE" 
            "CODE-TCD" "=" "0000" "AND"
            "CODE-HCD" "=" W-HCD(CNT) "AND"
            "CODE-SIZ" "=" W-SC "AND"
            "CODE-SNO" "=" W-SNO RETURNING RET.
      *           READ CODEF AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING CODEF_PNAME1
               MOVE SPACE TO CODE-JAN CODE-NAME
           END-IF
           IF  CODE-JAN = SPACE
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           IF  CODE-NAME = SPACE
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           MOVE CODE-JAN TO W-JAN(CNT).
           MOVE CODE-NAME TO W-SHM(CNT).
      *
           MOVE 0077 TO THT-TCD.
           MOVE W-HCD(CNT) TO THT-HCD.
           MOVE W-SC TO THT-SIZ.
      *           READ THTM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-070
           END-IF
           MOVE THT-T TO W-GTN(CNT).
           MOVE THT-TT TO W-UTN(CNT).
           GO TO ACP-080.
       ACP-070.
           MOVE 0077 TO THT-TCD.
           MOVE W-HCD(CNT) TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           MOVE THT-T TO W-GTN(CNT).
           MOVE THT-TT TO W-UTN(CNT).
       ACP-080.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF  W-SU(CNT) = ZERO
               GO TO ACP-080
           END-IF
           COMPUTE W-GKIN(CNT) = W-SU(CNT) * W-GTN(CNT).
           COMPUTE W-UKIN(CNT) = W-SU(CNT) * W-UTN(CNT).
           CALL "SD_Output" USING "D-GTN" D-GTN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTN" D-UTN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-MSB "A-MSB" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           GO TO ACP-040.
       ACP-120.
           SUBTRACT 1 FROM CNT.
           SUBTRACT 2 FROM W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  CNT = 0
               GO TO ACP-020
           END-IF
           IF  W-HCD(CNT) = ZERO
               GO TO ACP-120
           END-IF
           GO TO ACP-050.
       ACP-130.
           MOVE ZERO TO W-MEID(CNT).
           MOVE SPACE TO W-JAN(CNT) W-SHM(CNT) W-MSB(CNT) W-SZM(CNT)
                         W-SIZM(CNT).
           CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU.
           ADD 1 TO CNT.
           ADD 2 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  CNT NOT = 10
               GO TO ACP-130
           END-IF.
       ACP-150.
           MOVE ZERO TO CNT WT-D.
       ACP-160.
           ADD 1 TO CNT.
           IF  CNT NOT = 10
               ADD W-SU(CNT) TO WT-SU
               ADD W-GKIN(CNT) TO WT-GKIN
               ADD W-UKIN(CNT) TO WT-UKIN
               GO TO ACP-160
           END-IF
           CALL "SD_Output" USING "D-TOT" D-TOT "p" RETURNING RESU.
       ACP-180.
           CALL "SD_Accept" USING BY REFERENCE A-BI "A-BI" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 10 TO CNT
               MOVE 22 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               GO TO ACP-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-180
           END-IF.
       ACP-190.
           CALL "SD_Accept" USING BY REFERENCE A-SNGP "A-SNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-190
           END-IF
           IF  W-SNGPS = ZERO
               MOVE ZERO TO W-SNGP
               GO TO ACP-200
           END-IF
           MOVE W-SNGP TO W-NGP.
           IF  W-NEN2 < 09
               GO TO ACP-190
           END-IF
           IF  W-GET < 01 OR > 12
               GO TO ACP-190
           END-IF
           IF  W-PEY < 01 OR > 31
               GO TO ACP-190
           END-IF
           MOVE 20 TO W-NEN1.
           MOVE W-NGP TO W-SNGP.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-HNA "A-HNA" "X" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-190
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-200
           END-IF.
       ACP-210.
           CALL "SD_Accept" USING BY REFERENCE A-ZON "A-ZON" "X" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-210
           END-IF.
       ACP-220.
           CALL "SD_Accept" USING BY REFERENCE A-DNGP "A-DNGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-210
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-220
           END-IF
           IF  W-DNGPS = ZERO
               ACCEPT W-DNGPS FROM DATE
               CALL "SD_Output" USING "A-DNGP" A-DNGP "p" RETURNING RESU
               MOVE 20 TO W-DNEN1
               GO TO ACP-280
           END-IF
           MOVE W-DNGP TO W-NGP.
           IF  W-NEN2 < 09
               GO TO ACP-220
           END-IF
           IF  W-GET < 01 OR > 12
               GO TO ACP-220
           END-IF
           IF  W-PEY < 01 OR > 31
               GO TO ACP-220
           END-IF
           MOVE 20 TO W-NEN1.
           MOVE W-NGP TO W-DNGP.
           IF  W-DNGPS < W-SDATE OR > W-EDATE
               GO TO ACP-220
           END-IF.
       ACP-280.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-EX
               ELSE
                   GO TO ACP-220
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-280
           END-IF
           IF  W-DMM = 9
               IF  W-ACT = 3
                   GO TO ACP-EX
               ELSE
                   GO TO ACP-005
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-280
           END-IF.
       ACP-EX.
           EXIT.
       UPD-RTN.
           IF  W-ACT NOT = 1
               GO TO UPD-100
           END-IF
           PERFORM CON-RTN THRU CON-EX.
           IF  COMPLETION_CODE = 255
               GO TO UPD-EX
           END-IF
           MOVE ZERO TO CNT.
       UPD-010.
           ADD 1 TO CNT.
           IF  CNT = 10
               GO TO UPD-EX
           END-IF
           IF  W-HCD(CNT) = ZERO
               GO TO UPD-010
           END-IF.
       UPD-020.
           INITIALIZE TDNA-R.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
           PERFORM MOV-RTN THRU MOV-EX.
      *           WRITE TDNA-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TDNA" E-TDNA "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-030
           END-IF
           GO TO UPD-010.
       UPD-030.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           MOVE "TDNAF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           GO TO UPD-020.
       UPD-100.
           MOVE SPACE TO TDNA-KEY.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
      *           START TDNAF KEY NOT < TDNA-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDNAF_PNAME1 "TDNA-KEY" " NOT < " TDNA-KEY RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           IF (TDNA-STC NOT = W-STC) OR (TDNA-DNO NOT = W-DNO)
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF.
       UPD-110.
      *           DELETE TDNAF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TDNAF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TDNA" E-TDNA "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
      *
      *           READ TDNAF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO UPD-120
           END-IF
           IF (TDNA-STC NOT = W-STC) OR (TDNA-DNO NOT = W-DNO)
               GO TO UPD-120
           END-IF
           GO TO UPD-110.
       UPD-120.
           IF  W-ACT = 3
               GO TO UPD-EX
           END-IF
      *
           MOVE ZERO TO CNT.
       UPD-130.
           ADD 1 TO CNT.
           IF  CNT = 10
               GO TO UPD-EX
           END-IF
           IF  W-HCD(CNT) = ZERO
               GO TO UPD-130
           END-IF.
       UPD-140.
           INITIALIZE TDNA-R.
           MOVE W-STC TO TDNA-STC.
           MOVE W-DNO TO TDNA-DNO.
           PERFORM MOV-RTN THRU MOV-EX.
      *           WRITE TDNA-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDNAF_PNAME1 TDNAF_LNAME TDNA-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TDNA" E-TDNA "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-150
           END-IF
           GO TO UPD-130.
       UPD-150.
           IF  ERR-STAT NOT = "24"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO UPD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           MOVE "TDNAF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           GO TO UPD-140.
       UPD-EX.
           EXIT.
       SIZ-RTN.
           MOVE ZERO TO W-SC.
       SIZ-010.
           ADD 1 TO W-SC.
           IF  W-SC > 4
               GO TO SIZ-EX
           END-IF
           MOVE ZERO TO W-SNO.
       SIZ-020.
           ADD 1 TO W-SNO.
           IF  W-SNO > 10
               GO TO SIZ-010
           END-IF
           IF  W-S(CNT) NOT = W-SZ(W-SC,W-SNO)
               GO TO SIZ-020
           END-IF
           MOVE W-SIZ(W-SC,W-SNO) TO W-DSZ.
           IF  HI-HKB = 1
               MOVE 5 TO W-DSZH
           END-IF
           MOVE W-DSZ TO W-SIZM(CNT) W-SZM(CNT).
       SIZ-EX.
           EXIT.
       CON-RTN.
           MOVE ZERO TO JCON1-R.
           MOVE 1 TO JCON1-01.
           MOVE 9 TO JCON1-02.
      *           READ JCON INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME24" E-ME24 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO CON-EX
           END-IF
           IF  JCON1-03 = 999999
               MOVE 000000 TO JCON1-03
           END-IF
           ADD 1 TO JCON1-03.
      *           REWRITE JCON1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME25" E-ME25 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO CON-EX
           END-IF
           MOVE JCON1-03 TO W-DNO.
       CON-EX.
           EXIT.
       MOV-RTN.
           MOVE CNT TO TDNA-DGN.
           MOVE W-JAN(CNT) TO TDNA-JAN.
           MOVE W-SU(CNT) TO TDNA-SU.
           MOVE W-GTN(CNT) TO TDNA-GTN.
           MOVE W-UTN(CNT) TO TDNA-UTN.
           MOVE W-GKIN(CNT) TO TDNA-GKIN.
           MOVE W-UKIN(CNT) TO TDNA-UKIN.
           MOVE 23 TO TDNA-DPM.
           MOVE 392 TO TDNA-CLS.
           MOVE W-SHM(CNT) TO TDNA-SHM.
           MOVE W-MSB(CNT) TO TDNA-MSB.
           MOVE W-HCD(CNT) TO TDNA-HCD.
           MOVE W-COR(CNT) TO TDNA-COR.
           MOVE W-SZM(CNT) TO TDNA-SIZ.
           MOVE AHNH-CCD TO TDNA-CCD.
           MOVE AHNH-NHSN TO TDNA-TNA.
           MOVE W-HNO TO TDNA-HNO.
           MOVE W-HNGP TO TDNA-HNGP.
           MOVE W-NNGP TO TDNA-NNGP.
           MOVE 519001 TO TDNA-THC.
           MOVE W-BI TO TDNA-BI.
           MOVE W-SNGP TO TDNA-SNGP.
           MOVE W-HNA TO TDNA-HNA.
           MOVE W-ZON TO TDNA-ZON.
           MOVE W-DNGP TO TDNA-DNGP.
           MOVE 20 TO TDNA-DC.
           MOVE 1 TO TDNA-NRC.
       MOV-EX.
           EXIT.
       COR-RTN.
           MOVE SPACE TO W-AHN W-CORD.
           MOVE HI-NAME TO W-AHN.
           MOVE ZERO TO W-C1.
       COR-010.
           ADD 1 TO W-C1.
           IF  W-C1 < 25
               IF  W-HN(W-C1) NOT = SPACE
                   GO TO COR-010
               END-IF
           END-IF
           ADD 1 TO W-C1.
           IF  W-C1 < 25
               IF  W-HN(W-C1) NOT = SPACE
                   GO TO COR-010
               END-IF
           END-IF.
       COR-020.
           ADD 1 TO W-C1.
           IF  W-C1 > 24
               GO TO COR-EX
           END-IF
           IF  W-HN(W-C1) = SPACE
               GO TO COR-020
           END-IF
           MOVE ZERO TO W-C2.
       COR-030.
           ADD 1 TO W-C2.
           IF  W-C2 > 4
               GO TO COR-EX
           END-IF
           MOVE W-HN(W-C1) TO W-NA(W-C2).
           ADD 1 TO W-C1.
           IF  W-C1 < 25
               GO TO COR-030
           END-IF.
       COR-EX.
           EXIT.
