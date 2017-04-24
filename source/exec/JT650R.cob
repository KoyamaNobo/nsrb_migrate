       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT650R.
      ***************************************************
      *    PROGRAM        : éÛíçë‰í†ñ‚çáÇπÅ@Å@Å@Å@Å@Å@Å@*
      *    DATA WRITTEN   : 97/12/05                    *
      *    SCREEN USED    : SJ650R                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL85 (74MODE)            *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       01  W-AREA.
           02  SW-END                PIC X(03).
      *    SW-END = " ON" ---> éÛíçÉ}ÉXÉ^Ç™ÇmÇdÇwÇsÅ@Ç`ÇsÅ@ÇdÇmÇc
           02  SW                    PIC X(03).
      *    SW  =  " ON" ---> É}ÉXÉ^ÇÇqÇdÇ`ÇcÇµÇƒÅCÇhÇmÇuÇ`ÇkÇhÇcÇÃéûÅB
      *    SW  =  "OFF" ---> É}ÉXÉ^ÇÇqÇdÇ`ÇcÇµÇƒÅCÇmÇnÇqÇlÇ`ÇkÇÃéûÅB
           02  NEW-SW                PIC X(03).
      *    NEW-SW   =  " ON" ---> ì¸óÕÇµÇƒÇPâÒñ⁄ÇÃÇqÇdÇ`ÇcÇ‹Ç≈ÅB
      *    éÛíçáÇÅ@Çì¸óÕÇµÇΩéûÇæÇØ  Åi"óaÇËãÊï™Å@ÉGÉâÅ[" Çï\é¶Ç∑ÇÈÇ©Åj
           02  NEW-SW1               PIC X(03).
      *    NEW-SW1  =  " ON" ---> ì¸óÕÇµÇƒÇPâÒñ⁄ÇÃÇqÇdÇ`ÇcÇ‹Ç≈ÅB
      *    Åi"ÉfÅ[É^Å@ñ¢ìoò^" Çï\é¶Ç∑ÇÈÇ©Åj
           02  LIN                   PIC 9(02).
           02  LIN1                  PIC 9(02).
      ***  DISPLAY  GAMEN-CLR Ç∑ÇÈéûÅCàÍí[ LIN ÇÇrÇ`ÇuÇdÇµÇƒÇ®Ç≠ÅB
           02 COLU                   PIC 9(02).
           02 G                      PIC 9(02).
           02 GG                     PIC 9(02).
           02 I                      PIC 9(02).
           02 AA                     PIC 9(02).
           02 A                      PIC 9(02).
           02 B                      PIC 9(02).
           02 C                      PIC 9(02).
           02  W-AREA1.
               03  W-TOK             PIC 9(04).
      *    ì¸óÕíl
               03  W-HIN             PIC 9(06).
      *    ì¸óÕíl
               03  W-TOK1            PIC 9(04).
      *    ï\é¶íl
               03  W-HIN1            PIC 9(06).
      *    ï\é¶íl
               03  W-JUNO.
                   04  W-JUNO1           PIC 9(06).
                   04  W-JUNO2           PIC 9(01).
               03  W-AREA2.
                   04  W-ZEN             PIC S9(04).
                   04  W-ZAN             PIC S9(04).
                   04  W-ZEN-KEI         PIC S9(06).
                   04  W-TOU-KEI         PIC S9(06).
                   04  W-ZAN-KEI         PIC S9(06).
                   04  W-DEN             PIC N(02).
                   04  W-SHOSAI.
                       05  W-GYO             PIC 9(02).
                       05  W-SHOSAI1  OCCURS 13.
                           06  W-SIZE1           PIC 9(01).
                           06  W-KEI             PIC S9(06).
                           06  W-SHU             PIC 9(06).
                           06  W-SIZE2           PIC S9(04)   OCCURS 10.
                       05  OKC               PIC 9(01).
       01  W-DATE.
           02  W-YMD                 PIC 9(06).
           02  W-YMDD  REDEFINES W-YMD.
               03  W-YY              PIC 9(02).
               03  W-MM              PIC 9(02).
               03  W-DD              PIC 9(02).
           02  W-KBN                 PIC N(03).
           02  W-NGP                 PIC 9(08).
           02  W-NGPD  REDEFINES W-NGP.
               03  F                 PIC 9(02).
               03  W-NEN             PIC 9(02).
               03  W-GET             PIC 9(02).
               03  W-PEY             PIC 9(02).
       01  W-TOTAL                   PIC S9(6).
      *
       COPY    LWMSG.
      *
           COPY  LJMSTD.
           COPY  L-JNSR.
           COPY  LIHIM2.
           COPY  LITCM.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLR-1.
           02  FILLER   PIC  X(12) VALUE  "CLEAR SCREEN".
       01  CLR-AREA.
           02  FILLER   PIC X(53)   VALUE " ".
           02  CLR-JU.
               03  CLR-JU1     PIC X(06)   VALUE " ".
               03  CLR-JU2     PIC X(01)   VALUE " ".
           02  CLR-HIN  PIC X(55)   VALUE " ".
           02  CLR-01.
               03  FILLER   PIC X(51)   VALUE " ".
               03  FILLER   PIC X(74)   VALUE " ".
               03  FILLER   PIC X(74)   VALUE " ".
               03  FILLER   PIC X(74)   VALUE " ".
               03  CLR-02.
                   04  FILLER   PIC X(77)   VALUE " ".
                   04  CLR-SHOSAI.
                       05  FILLER   PIC X(02)   VALUE " ".
                       05  FILLER   PIC X(49)   VALUE " ".
                       05  FILLER   PIC X(63)   VALUE " ".
                       05  FILLER   PIC X(48)   VALUE " ".
                       05  FILLER   PIC X(01)   VALUE " ".
      *
       01  DSP-AREA.
           02  DSP-TOK     PIC N(24).
           02  DSP-HIN     PIC N(24).
           02  DSP-TOK1    PIC 9(04).
           02  DSP-HIN1    PIC 9(06).
       01  DSP-HED.
           02  DSP-KBN     PIC N(03).
           02  DSP-HED-01  PIC 9(01).
           02  DSP-HED-02.
               03  DSP-HED-021  PIC ----- .
               03  DSP-HED-022  PIC ----- .
               03  DSP-HED-023  PIC ----- .
           02  DSP-HED-03.
               03  DSP-HED-031  PIC ----,--9 .
               03  DSP-HED-032  PIC ----,--9 .
               03  DSP-HED-033  PIC ----,--9 .
           02  DSP-HED-04  PIC ZZZZZ .
           02  DSP-HED-05.
               03  FILLER   PIC X(01)
                           VALUE "/".
               03  FILLER   PIC X(01)
                           VALUE "/".
               03  DSP-HED-051  PIC 9(2).
               03  DSP-HED-052  PIC Z9 .
               03  DSP-HED-053  PIC Z9 .
           02  DSP-HED-KIGOU1   PIC X(01)
                           VALUE "*".
       01  DSP-MEI-AREA.
           02  DSP-MEI.
               03  DSP-MEI-01.
                   04  DSP-MEI-011     PIC ZZ .
                   04  DSP-MEI-012     PIC X(01)
                                       VALUE "/".
                   04  DSP-MEI-013     PIC ZZ .
               03  DSP-MEI-02          PIC N(02).
               03  DSP-MEI-03          PIC N(24).
               03  DSP-MEI-04          PIC ----,--9 .
               03  DSP-MEI-05          PIC 9(06).
           02  DSP-MEI-KIGOU2 PIC X(01)
                                       VALUE  "*".
           02  DSP-SHOSAI.
               03  DSP-SHOSAI1.
                   04  DSP-HIM     PIC N(24).
                   04  DSP-SIZE1   PIC 9(01).
           02  DSP-1.
               03  FILLER   PIC X(04)   VALUE "ÇRçÜ".
               03  FILLER   PIC X(04)   VALUE "ÇQçÜ".
               03  FILLER   PIC X(04)   VALUE "ÇPçÜ".
               03  FILLER   PIC X(04)   VALUE "ÇOçÜ".
               03  FILLER   PIC X(04)   VALUE " íÜ ".
               03  FILLER   PIC X(04)   VALUE " ëÂ ".
               03  FILLER   PIC X(04)   VALUE "ì¡ëÂ".
               03  FILLER   PIC X(04)   VALUE "28.0".
               03  FILLER   PIC X(04)   VALUE "29.0".
               03  FILLER   PIC X(04)   VALUE "30.0".
           02  DSP-2.
               03  FILLER   PIC X(04)   VALUE "12.5".
               03  FILLER   PIC X(04)   VALUE "13.0".
               03  FILLER   PIC X(04)   VALUE "13.5".
               03  FILLER   PIC X(04)   VALUE "14.0".
               03  FILLER   PIC X(04)   VALUE "15.0".
               03  FILLER   PIC X(04)   VALUE "16.0".
               03  FILLER   PIC X(04)   VALUE "17.0".
               03  FILLER   PIC X(04)   VALUE "18.0".
               03  FILLER   PIC X(04)   VALUE "19.0".
               03  FILLER   PIC X(04)   VALUE "20.0".
           02  DSP-3.
               03  FILLER   PIC X(04)   VALUE "21.0".
               03  FILLER   PIC X(04)   VALUE "21.5".
               03  FILLER   PIC X(04)   VALUE "22.0".
               03  FILLER   PIC X(04)   VALUE "22.5".
               03  FILLER   PIC X(04)   VALUE "23.0".
               03  FILLER   PIC X(04)   VALUE "23.5".
               03  FILLER   PIC X(04)   VALUE "24.0".
               03  FILLER   PIC X(04)   VALUE "24.5".
               03  FILLER   PIC X(04)   VALUE "25.0".
               03  FILLER   PIC X(04)   VALUE "    ".
           02  DSP-4.
               03  FILLER   PIC X(04)   VALUE "24.0".
               03  FILLER   PIC X(04)   VALUE "24.5".
               03  FILLER   PIC X(04)   VALUE "25.0".
               03  FILLER   PIC X(04)   VALUE "25.5".
               03  FILLER   PIC X(04)   VALUE "26.0".
               03  FILLER   PIC X(04)   VALUE "26.5".
               03  FILLER   PIC X(04)   VALUE "27.0".
               03  FILLER   PIC X(04)   VALUE "27.5".
               03  FILLER   PIC X(04)   VALUE "    ".
               03  FILLER   PIC X(04)   VALUE "    ".
      *
       01  ACP-AREA.
           02  ACP-TOK   PIC 9(04).
           02  ACP-HIN   PIC 9(06).
           02  ACP-JUNO1 PIC 9(06).
           02  ACP-JUNO2 PIC 9(01).
           02  ACP-GYO   PIC 9(02).
           02  ACP-OKC   PIC 9(01).
      *
       01  DISP-MSG-SPACE1.
           02  FILLER   PIC X(39)     VALUE " ".
      *
       01  ERR-AREA.
           02  ERR-1    PIC X(16)   VALUE "óaÇËãÊï™Å@ÉGÉâÅ[".
      *
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-1
       CALL "SD_Init" USING
           "CLR-1" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-1" "X" "1" "0" "12" " " "CLR-01" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "628" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01CLR-AREA" "X" "1" "27" "53" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-JU" " " "2" "0" "7" "01CLR-AREA" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-JU1" "X" "2" "9" "6" " " "CLR-JU"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-JU2" "X" "2" "16" "1" "CLR-JU1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-HIN" "X" "2" "25" "55" "CLR-JU" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-01" " " "0" "0" "513" "CLR-HIN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "3" "6" "51" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-01" "X" "4" "7" "74" "01CLR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-01" "X" "5" "7" "74" "02CLR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-01" "X" "6" "7" "74" "03CLR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-02" " " "0" "27" "240" "04CLR-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-02" "X" "LIN" "4" "77" " " "CLR-02"  RETURNING RESU.
       CALL "SD_Init" USING 
           "CLR-SHOSAI" " " "0" "9" "163" "01CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-SHOSAI" "X" "22" "2" "2" " " "CLR-SHOSAI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-SHOSAI" "X" "21" "8" "49" "01CLR-SHOSAI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-SHOSAI" "X" "22" "7" "63" "02CLR-SHOSAI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-SHOSAI" "X" "23" "10" "48" "03CLR-SHOSAI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-SHOSAI" "X" "24" "71" "1" "04CLR-SHOSAI" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "106" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TOK" "N" "1" "32" "48" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TOK" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HIN" "N" "2" "32" "48" "DSP-TOK" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HIN" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TOK1" "9" "1" "27" "4" "DSP-HIN" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TOK1" BY REFERENCE W-TOK1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HIN1" "9" "2" "25" "6" "DSP-TOK1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HIN1" BY REFERENCE W-HIN1 "6" "0" RETURNING RESU.
      *DSP-HED
       CALL "SD_Init" USING 
            "DSP-HED" " " "0" "0" "60" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KBN" "RN" "6" "75" "6" " " "DSP-HED"  RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KBN" BY REFERENCE W-KBN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-01" "9" "3" "6" "1" "DSP-KBN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-01" BY REFERENCE JMSTD-09 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-02" " " "0" "0" "15" "DSP-HED-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-021" "-----" "A" "COLU" "5" " " "DSP-HED-02"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-021" BY REFERENCE W-ZEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-022" "-----" "B" "COLU" "5" "DSP-HED-021" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-022" BY REFERENCE JMSTD-1211(1) "6" "1"
            BY REFERENCE I 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-023" "-----" "C" "COLU" "5" "DSP-HED-022" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-023" BY REFERENCE W-ZAN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-03" " " "0" "0" "24" "DSP-HED-02" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-031" "----,--9" "A" "57" "8" " " "DSP-HED-03"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-031" BY REFERENCE W-ZEN-KEI "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-032" "----,--9" "B" "57" "8" "DSP-HED-031" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-032" BY REFERENCE W-TOU-KEI "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-033" "----,--9" "C" "57" "8" "DSP-HED-032" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-033" BY REFERENCE W-ZAN-KEI "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-04" "ZZZZZ" "4" "66" "5" "DSP-HED-03" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-04" BY REFERENCE JMSTD-17 "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "DSP-HED-05" " " "0" "0" "8" "DSP-HED-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-HED-05" "X" "4" "75" "1" " " "DSP-HED-05"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-HED-05" "X" "4" "78" "1" "01DSP-HED-05" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-051" "9" "4" "73" "2" "02DSP-HED-05" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "DSP-HED-051" BY REFERENCE JMSTD-0212 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-052" "Z9" "4" "76" "2" "DSP-HED-051" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-052" BY REFERENCE JMSTD-022 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-053" "Z9" "4" "79" "2" "DSP-HED-052" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HED-053" BY REFERENCE JMSTD-023 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HED-KIGOU1" "X" "4" "72" "1" "DSP-HED-05" " "
            RETURNING RESU.
      *DSP-MEI-AREA
       CALL "SD_Init" USING 
            "DSP-MEI-AREA" " " "0" "0" "281" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI" " " "LIN" "0" "71" " " "DSP-MEI-AREA"
             RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-01" " " "LIN" "0" "5" " " "DSP-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-011" "ZZ" "LIN" "4" "2" " " "DSP-MEI-01"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MEI-011" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-012" "X" "LIN" "6" "1" "DSP-MEI-011" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-013" "ZZ" "LIN" "7" "2" "DSP-MEI-012" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MEI-013" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-02" "N" "LIN" "10" "4" "DSP-MEI-01" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MEI-02" BY REFERENCE W-DEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-03" "N" "LIN" "15" "48" "DSP-MEI-02" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MEI-03" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-04" "----,--9" "LIN" "65" "8" "DSP-MEI-03" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MEI-04" BY REFERENCE W-TOTAL "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-05" "9" "LIN" "75" "6" "DSP-MEI-04" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MEI-05" BY REFERENCE JNSR-181 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MEI-KIGOU2" "X" "LIN" "74" "1" "DSP-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SHOSAI" " " "0" "0" "49" "DSP-MEI-KIGOU2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SHOSAI1" " " "0" "0" "49" " " "DSP-SHOSAI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-HIM" "N" "23" "10" "48" " " "DSP-SHOSAI1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-HIM" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SIZE1" "9" "22" "7" "1" "DSP-HIM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SIZE1" BY REFERENCE W-SIZE1(1) "1" "1" BY REFERENCE
            GG 53 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-1" " " "AA" "0" "40" "DSP-SHOSAI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-1" "X" "AA" "8" "4" " " "DSP-1"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-1" "X" "AA" "13" "4" "01DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-1" "X" "AA" "18" "4" "02DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-1" "X" "AA" "23" "4" "03DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-1" "X" "AA" "28" "4" "04DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-1" "X" "AA" "33" "4" "05DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-1" "X" "AA" "38" "4" "06DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-1" "X" "AA" "43" "4" "07DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-1" "X" "AA" "48" "4" "08DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-1" "X" "AA" "53" "4" "09DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-2" " " "AA" "0" "40" "DSP-1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-2" "X" "AA" "8" "4" " " "DSP-2"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-2" "X" "AA" "13" "4" "01DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-2" "X" "AA" "18" "4" "02DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-2" "X" "AA" "23" "4" "03DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-2" "X" "AA" "28" "4" "04DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-2" "X" "AA" "33" "4" "05DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-2" "X" "AA" "38" "4" "06DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-2" "X" "AA" "43" "4" "07DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-2" "X" "AA" "48" "4" "08DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-2" "X" "AA" "53" "4" "09DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-3" " " "AA" "0" "40" "DSP-2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-3" "X" "AA" "8" "4" " " "DSP-3"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-3" "X" "AA" "13" "4" "01DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-3" "X" "AA" "18" "4" "02DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-3" "X" "AA" "23" "4" "03DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-3" "X" "AA" "28" "4" "04DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-3" "X" "AA" "33" "4" "05DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-3" "X" "AA" "38" "4" "06DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-3" "X" "AA" "43" "4" "07DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-3" "X" "AA" "48" "4" "08DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-3" "X" "AA" "53" "4" "09DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-4" " " "AA" "0" "40" "DSP-3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-4" "X" "AA" "8" "4" " " "DSP-4"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-4" "X" "AA" "13" "4" "01DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-4" "X" "AA" "18" "4" "02DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-4" "X" "AA" "23" "4" "03DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-4" "X" "AA" "28" "4" "04DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-4" "X" "AA" "33" "4" "05DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-4" "X" "AA" "38" "4" "06DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-4" "X" "AA" "43" "4" "07DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-4" "X" "AA" "48" "4" "08DSP-4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-4" "X" "AA" "53" "4" "09DSP-4" " "  RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TOK" "9" "1" "27" "4" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TOK" BY REFERENCE W-TOK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-HIN" "9" "2" "25" "6" "ACP-TOK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-HIN" BY REFERENCE W-HIN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JUNO1" "9" "2" "9" "6" "ACP-HIN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JUNO1" BY REFERENCE W-JUNO1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JUNO2" "9" "2" "16" "1" "ACP-JUNO1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JUNO2" BY REFERENCE W-JUNO2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GYO" "9" "22" "2" "2" "ACP-JUNO2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-GYO" BY REFERENCE W-GYO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "71" "1" "ACP-GYO" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "24" "0" "39" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE1" "X" "24" "1" "39" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *ERR-AREA
       CALL "SD_Init" USING 
            "ERR-AREA" " " "0" "0" "16" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-1" "X" "24" "1" "16" " " "ERR-AREA"  RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       HAJIME.
           PERFORM   INI-RTN    THRU  INI-EX.
           PERFORM   MAIN-RTN   THRU  MAIN-EX.
           PERFORM   END-RTN    THRU  END-EX.
           CALL "DB_Close".
           STOP  RUN.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       MAIN-RTN.
           MOVE  "OFF"     TO  NEW-SW.
           CALL "SD_Accept" USING BY REFERENCE ACP-TOK "ACP-TOK" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MAIN-EX
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-RTN
           END-IF
           CALL "SD_Output" USING "ACP-TOK" ACP-TOK "p" RETURNING RESU.
      *
           IF  W-TOK  =  ZERO
               MOVE  SPACE     TO  TC-NAME
               CALL "SD_Output" USING
                "DSP-TOK" DSP-TOK "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-HIN" CLR-HIN "p" RETURNING RESU
               GO  TO  MAIN-020
           END-IF
      *
           MOVE  W-TOK    TO  TC-TCD.
           MOVE  "001"    TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           IF  SW  =  " ON"
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  MAIN-RTN
           END-IF
           CALL "SD_Output" USING "DSP-TOK" DSP-TOK "p" RETURNING RESU.
       MAIN-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-HIN "ACP-HIN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-RTN
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-010
           END-IF
           CALL "SD_Output" USING "ACP-HIN" ACP-HIN "p" RETURNING RESU.
      *
           IF  W-HIN  =  ZERO
               MOVE  SPACE     TO  HI-NAME
               CALL "SD_Output" USING
                "DSP-HIN" DSP-HIN "p" RETURNING RESU
               CALL "SD_Output" USING "CLR-JU" CLR-JU "p" RETURNING RESU
               GO  TO  MAIN-026
           END-IF
      *
           MOVE  W-HIN    TO  HI-MHCD HI-HCD.
           PERFORM  HIM-READ-RTN     THRU  HIM-READ-EX.
           IF  SW  =  " ON"
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  MAIN-010
           END-IF
           CALL "SD_Output" USING "DSP-HIN" DSP-HIN "p" RETURNING RESU.
           CALL "SD_Output" USING "CLR-JU" CLR-JU "p" RETURNING RESU.
           GO  TO  MAIN-026.
       MAIN-020.
           MOVE  " ON"     TO  NEW-SW.
           CALL "SD_Accept" USING BY REFERENCE ACP-JUNO1 "ACP-JUNO1"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-RTN
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-020
           END-IF
           CALL "SD_Output" USING
            "ACP-JUNO1" ACP-JUNO1 "p" RETURNING RESU.
      *
       MAIN-025.
           CALL "SD_Accept" USING BY REFERENCE ACP-JUNO2 "ACP-JUNO2"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-020
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-025
           END-IF
           CALL "SD_Output" USING
            "ACP-JUNO2" ACP-JUNO2 "p" RETURNING RESU.
      *
       MAIN-026.
           MOVE  7        TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           MOVE  ZERO     TO  G.
           MOVE  " ON"     TO  NEW-SW1.
      *
           IF  W-TOK  =  ZERO
               MOVE  W-JUNO1     TO  JMSTD-07
               MOVE  W-JUNO2     TO  JMSTD-08
      *               START  JMSTD  KEY  NOT <  JMSTD-KEY1  INVALID
      *///////////////
               CALL "DB_Start" USING
                JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
                RETURNING RET
               GO  TO  MAIN-040
           ELSE
               INITIALIZE  JMSTD-KEY2
               MOVE  W-TOK       TO  JMSTD-04
               MOVE  W-HIN       TO  JMSTD-05
      *               START  JMSTD  KEY  NOT <  JMSTD-KEY2  INVALID
      *///////////////
               CALL "DB_Start" USING
                JMSTD_PNAME1 "JMSTD-KEY2" " NOT < " JMSTD-KEY2
                RETURNING RET
               GO  TO  MAIN-040
           END-IF.
       MAIN-027.
      ***  éÛíçÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  JMSTD  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-028
           END-IF
           GO  TO  MAIN-029.
       MAIN-028.
           IF  NEW-SW1  =  " ON"
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               GO  TO  MAIN-RTN
           ELSE
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-029.
           IF  NEW-SW  =  " ON"
               IF  JMSTD-01  NOT =  ZERO  AND  2  AND  5  AND  6
                   CALL "SD_Output" USING
                    "ERR-1" ERR-1 "p" RETURNING RESU
                   GO  TO  MAIN-RTN
               END-IF
           END-IF
           IF  JMSTD-01  NOT =  ZERO  AND  2  AND  5  AND  6
               GO  TO  MAIN-027
           END-IF
           MOVE  "OFF"     TO  NEW-SW.
           IF  W-TOK  =  ZERO
               IF  W-JUNO  NOT =  JMSTD-KEY1
                   GO  TO  MAIN-027
               END-IF
           END-IF
           IF  W-TOK  NOT =  ZERO
               IF  W-TOK  NOT =  JMSTD-04
                   GO  TO  MAIN-027
               END-IF
           END-IF
           IF  W-TOK  NOT =  ZERO
               IF  W-HIN  NOT =  ZERO
                   IF  W-HIN  NOT =  JMSTD-05
                       GO  TO  MAIN-027
                   END-IF
               END-IF
           END-IF
      *
           MOVE  "éÛÅ@íç"      TO  W-KBN.
           IF  JMSTD-01   =  5
               MOVE  "óaÅ@ÇË"      TO  W-KBN
           END-IF
           IF  JMSTD-01   =  6
               MOVE  "éÊÇÊÇØ"      TO  W-KBN
           END-IF
           PERFORM  HED-RTN     THRU  HED-EX.
      *
           INITIALIZE  JNSR-R.
           MOVE  JMSTD-07     TO  JNSR-151.
           MOVE  JMSTD-08     TO  JNSR-152.
      *           START  JNSR  KEY  NOT  <  JNSR-KEY2  INVALID
      *///////////////
           CALL "DB_Start" USING
            JNSR_PNAME1 "JNSR-KEY2" " NOT < " JNSR-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-040
           END-IF.
       MAIN-030.
      ***  èoâ◊ó›åvÉtÉ@ÉCÉãÅ@ÇqÇdÇ`Çc
      *           READ  JNSR  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNSR_PNAME1 BY REFERENCE JNSR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-040
           END-IF
           MOVE  JNSR-16   TO  W-NGP.
           IF  W-GET       NOT =  W-MM
               GO  TO  MAIN-030
           END-IF
           IF (JMSTD-07    NOT =  JNSR-151)  OR
              (JMSTD-08    NOT =  JNSR-152)
               GO  TO  MAIN-040
           END-IF
           IF  JNSR-13  NOT =  ZERO  AND  2  AND  5  AND  6
               GO  TO  MAIN-030
           END-IF
           COMPUTE  W-TOTAL  =  JNSR-081(01)  +  JNSR-081(02) +
                                JNSR-081(03)  +  JNSR-081(04) +
                                JNSR-081(05)  +  JNSR-081(06) +
                                JNSR-081(07)  +  JNSR-081(08) +
                                JNSR-081(09)  +  JNSR-081(10).
       NEXT-DSP.
           ADD  1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           ADD  1     TO  G.
           IF  LIN  =  21
               SUBTRACT  1     FROM  G
               GO  TO  MAIN-OKC
           END-IF
           PERFORM  WORK-RTN     THRU  WORK-EX.
           PERFORM  DSP-RTN      THRU  DSP-EX.
           GO  TO  MAIN-030.
       MAIN-040.
           IF  LIN  =  7
               IF  NEW-SW1  =  " ON"
                   CALL "SD_Output" USING
                    "INV-D01" INV-D01 "p" RETURNING RESU
                   GO  TO  MAIN-RTN
               ELSE
                   ADD  1     TO  LIN
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
                   GO  TO  MAIN-OKC
               END-IF
           END-IF
           IF  LIN  NOT =  20
               ADD  1     TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
               MOVE  LIN     TO  LIN1
               PERFORM  GAMEN-CLR-RTN     THRU  GAMEN-CLR-EX
               MOVE  LIN1    TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
           END-IF.
       MAIN-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               PERFORM  SHOSAI-RTN     THRU  SHOSAI-EX
           ELSE
               GO  TO  MAIN-050
           END-IF
           IF  ESTAT  =  "09"
               GO  TO  MAIN-OKC
           END-IF.
       MAIN-050.
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-OKC
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT  =  "1" AND "9"
               GO  TO  MAIN-OKC
           END-IF
           IF  OKC  =  "9"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               PERFORM  CLR1-RTN     THRU  CLR1-EX
               GO  TO  MAIN-RTN
           END-IF
           MOVE  "OFF"     TO  NEW-SW1.
           IF  LIN  =  21
               PERFORM  CLR3-RTN     THRU  CLR3-EX
               MOVE  7        TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
               MOVE  ZERO     TO  G
               GO  TO  NEXT-DSP
           ELSE
               CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU
               PERFORM  CLR2-RTN     THRU  CLR2-EX
               MOVE  7        TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
               MOVE  ZERO     TO  G
               GO  TO  MAIN-027
           END-IF.
       MAIN-EX.
           EXIT.
      ************************************
      *    ÇgÇdÇcÅ|ÇqÇsÇmÅ@Å@Å@Å@        *
      ************************************
       HED-RTN.
           MOVE     JMSTD-04         TO    W-TOK1  TC-TCD.
           MOVE     "001"            TO    TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           MOVE     JMSTD-05         TO    W-HIN1  HI-MHCD HI-HCD.
           PERFORM  HIM-READ-RTN     THRU  HIM-READ-EX.
           MOVE  JMSTD-07            TO  W-JUNO1.
           MOVE  JMSTD-08            TO  W-JUNO2.
           CALL "SD_Output" USING
            "DSP-TOK1" DSP-TOK1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HIN1" DSP-HIN1 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-TOK" DSP-TOK "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-HIN" DSP-HIN "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-JUNO1" ACP-JUNO1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACP-JUNO2" ACP-JUNO2 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HED-01" DSP-HED-01 "p" RETURNING RESU.
      *
           MOVE  3     TO AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE  4     TO A.
           CALL "SD_Arg_Match_Line" USING "A" "2" A RETURNING RESU.
           MOVE  5     TO B.
           CALL "SD_Arg_Match_Line" USING "B" "2" B RETURNING RESU.
           MOVE  6     TO C.
           CALL "SD_Arg_Match_Line" USING "C" "2" C RETURNING RESU.
      *
           IF  JMSTD-09  =  1
               CALL "SD_Output" USING "DSP-1" DSP-1 "p" RETURNING RESU
           END-IF
           IF  JMSTD-09  =  2
               CALL "SD_Output" USING "DSP-2" DSP-2 "p" RETURNING RESU
           END-IF
           IF  JMSTD-09  =  3
               CALL "SD_Output" USING "DSP-3" DSP-3 "p" RETURNING RESU
           END-IF
           IF  JMSTD-09  =  4
               CALL "SD_Output" USING "DSP-4" DSP-4 "p" RETURNING RESU
           END-IF
           MOVE  1      TO  I.
           MOVE  7      TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           MOVE  ZERO   TO  W-ZEN-KEI W-TOU-KEI W-ZAN-KEI.
       HED-010.
           IF  I  >  10
               GO  TO  HED-020
           END-IF
           COMPUTE W-ZEN = JMSTD-1111(I) - JMSTD-141(I).
           COMPUTE W-ZAN = JMSTD-1111(I) - JMSTD-141(I) - JMSTD-1211(I).
           CALL "SD_Output" USING
            "DSP-HED-02" DSP-HED-02 "p" RETURNING RESU.
           ADD  W-ZEN             TO  W-ZEN-KEI.
           ADD  JMSTD-1211(I)     TO  W-TOU-KEI.
           ADD  W-ZAN             TO  W-ZAN-KEI.
           ADD  1     TO  I.
           ADD  5     TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           GO  TO  HED-010.
       HED-020.
           CALL "SD_Output" USING
            "DSP-HED-03" DSP-HED-03 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HED-04" DSP-HED-04 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-HED-05" DSP-HED-05 "p" RETURNING RESU.
           IF  JMSTD-01  =  2
               CALL "SD_Output" USING
                "DSP-HED-KIGOU1" DSP-HED-KIGOU1 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "DSP-KBN" DSP-KBN "p" RETURNING RESU.
       HED-EX.
           EXIT.
      ************************************
      *    ÇvÇnÇqÇjÅ|ÇqÇsÇmÅ@Å@Å@        *
      ************************************
       WORK-RTN.
           MOVE  JNSR-07         TO  W-SIZE1(G).
           MOVE  W-TOTAL         TO  W-KEI(G).
           MOVE  JNSR-04         TO  W-SHU(G).
           MOVE  1               TO  I.
       WORK-010.
           IF  I  >  10
               GO  TO  WORK-EX
           END-IF
           MOVE  JNSR-081 (I)    TO  W-SIZE2(G I).
           ADD  1     TO  I.
           GO  TO  WORK-010.
       WORK-EX.
           EXIT.
      ************************************
      *    ÇcÇrÇoÅ|ÇqÇsÇmÅ@Å@Å@Å@        *
      ************************************
       DSP-RTN.
           MOVE  SPACE           TO  W-DEN.
           IF  JNSR-10  =  0
               MOVE  "èoâ◊"    TO  W-DEN
           END-IF
           IF  JNSR-10  =  1
               MOVE  "ï‘ïi"    TO  W-DEN
           END-IF
           IF  JNSR-10  =  2
               MOVE  "ïsó«"    TO  W-DEN
           END-IF
           IF  JNSR-10  =  3
               MOVE  "í˘ê≥"    TO  W-DEN
           END-IF
           IF  JNSR-10  =  7
               MOVE  "ÉTèo"    TO  W-DEN
           END-IF
           MOVE  JNSR-11         TO  TC-KEY.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           CALL "SD_Output" USING "DSP-MEI" DSP-MEI "p" RETURNING RESU.
           IF  W-HIN1  NOT =  JNSR-01
               CALL "SD_Output" USING
                "DSP-MEI-KIGOU2" DSP-MEI-KIGOU2 "p" RETURNING RESU
           END-IF.
       DSP-EX.
           EXIT.
      ************************************
      *    ÇfÇ`ÇlÇdÇmÅ|ÇbÇkÇqÅ|ÇqÇsÇm    *
      ************************************
       GAMEN-CLR-RTN.
           IF  LIN  >  20
               GO  TO  GAMEN-CLR-EX
           END-IF
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           ADD  1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  GAMEN-CLR-RTN.
       GAMEN-CLR-EX.
           EXIT.
      ************************************
      *    ÇrÇgÇnÇrÇ`ÇhÅ|ÇqÇsÇmÅ@        *
      ************************************
       SHOSAI-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACP-GYO "ACP-GYO" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  SHOSAI-EX
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  SHOSAI-RTN
           END-IF
           IF  W-GYO  >  G
               GO  TO  SHOSAI-RTN
           END-IF
           IF  W-GYO  =  ZERO
               GO  TO  SHOSAI-RTN
           END-IF
           MOVE  W-GYO          TO  GG.
           MOVE  W-SHU(GG)      TO  HI-MHCD HI-HCD.
           PERFORM  HIM-READ-RTN     THRU  HIM-READ-EX.
           CALL "SD_Output" USING
            "DSP-SHOSAI1" DSP-SHOSAI1 "p" RETURNING RESU.
           MOVE  21     TO  AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
      *
           IF  W-SIZE1(GG)  =  1
               CALL "SD_Output" USING "DSP-1" DSP-1 "p" RETURNING RESU
           END-IF
           IF  W-SIZE1(GG)  =  2
               CALL "SD_Output" USING "DSP-2" DSP-2 "p" RETURNING RESU
           END-IF
           IF  W-SIZE1(GG)  =  3
               CALL "SD_Output" USING "DSP-3" DSP-3 "p" RETURNING RESU
           END-IF
           IF  W-SIZE1(GG)  =  4
               CALL "SD_Output" USING "DSP-4" DSP-4 "p" RETURNING RESU
           END-IF
      *
           IF  W-SIZE1(GG)  NOT =  1  AND  2  AND  3  AND  4
               CALL "SD_Output" USING
                "CLR-SHOSAI" CLR-SHOSAI "p" RETURNING RESU
               GO  TO  SHOSAI-020
           END-IF
      *
           MOVE  1     TO  I.
           MOVE  22    TO  A.
           CALL "SD_Arg_Match_Line" USING "A" "2" A RETURNING RESU.
           MOVE  7     TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
       SHOSAI-010.
           IF  I  >  10
               GO  TO  SHOSAI-020
           END-IF
           MOVE  W-SIZE2(GG I)     TO  W-ZEN.
           CALL "SD_Output" USING
            "DSP-HED-021" DSP-HED-021 "p" RETURNING RESU.
           ADD  1     TO  I.
           ADD  5     TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           GO  TO  SHOSAI-010.
       SHOSAI-020.
           MOVE  W-KEI(GG)     TO  W-ZEN-KEI.
           CALL "SD_Output" USING
            "DSP-HED-031" DSP-HED-031 "p" RETURNING RESU.
           GO  TO  SHOSAI-RTN.
       SHOSAI-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇmÇP                       *
      *********************************************
       CLR1-RTN.
           INITIALIZE  W-AREA.
           CALL "SD_Output" USING
            "CLR-AREA" CLR-AREA "p" RETURNING RESU.
           MOVE  8     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR1-010.
           IF  LIN  >  20
               GO  TO  CLR1-EX
           END-IF
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           ADD   1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  CLR1-010.
       CLR1-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇmÇQ                       *
      *********************************************
       CLR2-RTN.
           INITIALIZE  W-AREA2.
           CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU.
           MOVE  8     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR2-010.
           IF  LIN  >  20
               GO  TO  CLR2-EX
           END-IF
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           ADD   1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  CLR2-010.
       CLR2-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇmÇR                       *
      *********************************************
       CLR3-RTN.
           INITIALIZE  W-AREA2.
           MOVE  8     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR3-010.
           IF  LIN  >  20
               GO  TO  CLR3-EX
           END-IF
           CALL "SD_Output" USING "CLR-02" CLR-02 "p" RETURNING RESU.
           ADD   1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  CLR3-010.
       CLR3-EX.
           EXIT.
      *********************************************
      *    ÇhÇmÇhÅ|ÇqÇsÇm                         *
      *********************************************
       INI-RTN.
           CALL "SD_Output" USING "CLR-1" CLR-1 "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ650R" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           ACCEPT       W-YMD       FROM  DATE.
       INI-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "SD_Output" USING "CLR-1" CLR-1 "p" RETURNING RESU.
       END-EX.
           EXIT.
      ************************************
      *    íºëóêÊÉ}ÉXÉ^Å@ÇqÇdÇ`Çc        *
      ************************************
       TCM-READ-RTN.
           MOVE  "OFF"     TO  SW.
      ***  íºëóêÊÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  TC-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  " ON"     TO  SW
               MOVE  SPACE     TO   TC-NAME
           END-IF.
       TCM-READ-EX.
           EXIT.
      ************************************
      *    ïiñºÉ}ÉXÉ^Å@ÇqÇdÇ`Çc      *
      ************************************
       HIM-READ-RTN.
           MOVE  "OFF"     TO  SW.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  " ON"     TO  SW
               MOVE  SPACE     TO   HI-NAME
           END-IF.
       HIM-READ-EX.
           EXIT.
      *
       COPY LPMSG.
      *
