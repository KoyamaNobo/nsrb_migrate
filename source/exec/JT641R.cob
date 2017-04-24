       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT640R.
       AUTHOR.                        MAYUMI.I.
      ***************************************************
      *    PROGRAM        : ìæà”êÊï èoâ◊é¿ê—ñ‚çáÇπÅ@Å@Å@*
      *    DATA WRITTEN   : 91/10/31                    *
      *    SCREEN USED    : SJ640R                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL85 (74MODE)            *
      *    JS-SIGN        : 0=ìæà”êÊ , 1=ìæà”êÊíºëóêÊ   *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  JS-SIGN                   PIC 9(01).
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  JT-W170K1ID               PIC X(12).
       77  JT-W170K3ID               PIC X(12).
       77  W-DC                      PIC 9(01).
       01  STN-NO.
           02  STN-NO-01             PIC X(03).
           02  STN-NO-02             PIC X(03).
       01  WF1-ID.
           02  WORK1-ID-01           PIC X(09) VALUE "JT-W170K1".
           02  WORK1-ID-02           PIC X(03).
       01  WF2-ID.
           02  WORK2-ID-01           PIC X(09) VALUE "JT-W170K3".
           02  WORK2-ID-02           PIC X(03).
       01  W-AREA.
           02  SW                    PIC X(03).
           02  LIN                   PIC 9(02).
           02  LIN1                  PIC 9(02).
           02  COLU                  PIC 9(02).
           02 G                      PIC 9(02).
           02 GG                     PIC 9(02).
           02 I                      PIC 9(02).
           02  W-DATE                PIC 9(08).
           02  W-AREA1.
               03  W-TOK             PIC 9(04).
               03  W-CCD             PIC 9(03).
               03  W-AREA2.
                   04  W-FROM            PIC 9(08).
                   04  W-FROMD  REDEFINES  W-FROM.
                       05  W-FNEN        PIC 9(04).
                       05  W-FNENL  REDEFINES  W-FNEN.
                           06  W-FNEN1   PIC 9(02).
                           06  W-FNEN2   PIC 9(02).
                       05  W-FGET        PIC 9(02).
                       05  W-FPEY        PIC 9(02).
                   04  W-FROML  REDEFINES  W-FROM.
                       05  F             PIC 9(02).
                       05  W-FROMS       PIC 9(06).
                   04  W-TO              PIC 9(08).
                   04  W-TOD    REDEFINES  W-TO.
                       05  W-TNEN        PIC 9(04).
                       05  W-TNENL  REDEFINES  W-TNEN.
                           06  W-TNEN1   PIC 9(02).
                           06  W-TNEN2   PIC 9(02).
                       05  W-TGET        PIC 9(02).
                       05  W-TPEY        PIC 9(02).
                   04  W-TOL    REDEFINES  W-TO.
                       05  F             PIC 9(02).
                       05  W-TOS         PIC 9(06).
                   04  W-DEN             PIC N(02).
                   04  W-HIZ.
                       05  F             PIC 9(02).
                       05  W-HIZ1        PIC 9(02).
                       05  W-HIZ2        PIC 9(02).
                       05  W-HIZ3        PIC 9(02).
                   04  W-SOK             PIC S9(06).
                   04  W-KBN             PIC 9(01).
                   04  W-SAS             PIC 9(06).
                   04  W-GP              PIC 9(04).
                   04  W-GPD   REDEFINES W-GP.
                       05  W-GET         PIC 9(02).
                       05  W-PEY         PIC 9(02).
                   04  W-SHOSAI.
                       05  W-GYO             PIC 9(02).
                       05  W-SHOSAI1  OCCURS 17.
                           06  W-SIZE1       PIC 9(01).
                           06  W-CHO.
                               07  W-CHOT    PIC 9(04).
                               07  W-CHOC    PIC 9(03).
                           06  W-SIZE2       PIC S9(05)   OCCURS 10.
                           06  W-GNO         PIC 9(07).
                           06  W-GNOD   REDEFINES  W-GNO.
                               07  W-GNO1    PIC 9(06).
                               07  W-GNO2    PIC 9(01).
                           06  W-TEK1        PIC N(09).
                           06  W-TEK2        PIC N(21).
                           06  W-OKNO        PIC 9(06).
                           06  W-UNSO        PIC 9(01).
                           06  W-KURA        PIC 9(01).
                       05  OKC               PIC 9(01).
      *
       COPY    LWMSG.
      *
           COPY  LIBFDD.
           COPY  L-JNSR.
           COPY  LIHIM2.
           COPY  LITCM.
           COPY  LJT170.
           COPY  L-JCON.
           COPY  LJMSTD.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLR-1.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           02  FILLER  PIC X(46)   VALUE " ".
           02  CLR-01.
               03  FILLER  PIC X(06)   VALUE " ".
               03  FILLER  PIC X(06)   VALUE " ".
               03  CLR-02.
                   04  FILLER  PIC X(05)   VALUE " ".
                   04  FILLER  PIC X(02)   VALUE " ".
                   04  FILLER  PIC X(53)   VALUE " ".
                   04  FILLER  PIC X(07)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  CLR-03.
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                       05  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC  X(011) VALUE "CLEAR  DATA".
                   04  FILLER  PIC X(52)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(01)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(60)   VALUE " ".
                   04  FILLER  PIC X(01)   VALUE " ".
                   04  FILLER  PIC X(01)   VALUE " ".
                   04  FILLER  PIC X(01)   VALUE " ".
           02  CLR-SHOSAI.
               03  CLR-04.
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
                   04  FILLER  PIC X(04)   VALUE " ".
               03  CLR-05.
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
                   04  FILLER  PIC X(06)   VALUE " ".
           02  GAMEN-CLR.
               03  FILLER  PIC X(05)   VALUE " ".
               03  FILLER  PIC X(02)   VALUE " ".
               03  FILLER  PIC X(53)   VALUE " ".
               03  FILLER  PIC X(07)   VALUE " ".
               03  FILLER  PIC X(06)   VALUE " ".
      *
       01  DSP-AREA.
           02  FILLER.
               03  DSP-HIZ1   PIC ZZ .
               03  DSP-HIZ    PIC X(01)
                              VALUE "/".
               03  DSP-HIZ2   PIC ZZ .
               03  DSP-DEN    PIC N(01).
               03  DSP-HCD    PIC 9(06).
               03  DSP-HIN    PIC N(16).
               03  DSP-JGP    PIC 99/99 .
               03  DSP-SOK    PIC ---,--9 .
               03  DSP-SAS    PIC 9(06).
      *
       01  DSP-AREA1.
           02  DSP-TOK     PIC N(21).
           02  DSP-SHOSAI.
               03  DSP-SHOSAI1.
                   04  DSP-CHOC    PIC 9(03).
                   04  DSP-CHON    PIC N(24).
                   04  DSP-SIZE1   PIC 9(01).
                   04  DSP-GNO.
                       05  FILLER  PIC 9(06).
                       05  FILLER  PIC 9(01).
                   04  DSP-OKNO    PIC 9(06).
                   04  DSP-TEK1    PIC N(09).
                   04  DSP-TEK2    PIC N(21).
                   04  DSP-UNSO    PIC 9(01).
                   04  DSP-KURA    PIC 9(01).
               03  DSP-SHOSAI2.
                   04  DSP-SIZE2   PIC --,--- .
           02  DSP-1.
               03  FILLER  PIC X(04)   VALUE "    ".
               03  FILLER  PIC X(04)   VALUE "    ".
               03  FILLER  PIC X(04)   VALUE "ÇrÇr".
               03  FILLER  PIC X(04)   VALUE "Å@Çr".
               03  FILLER  PIC X(04)   VALUE "Å@Çl".
               03  FILLER  PIC X(04)   VALUE "  Çk".
               03  FILLER  PIC X(04)   VALUE "ÇkÇk".
               03  FILLER  PIC X(04)   VALUE "28.0".
               03  FILLER  PIC X(04)   VALUE "29.0".
               03  FILLER  PIC X(04)   VALUE "30.0".
           02  DSP-2.
               03  FILLER  PIC X(04)   VALUE "12.5".
               03  FILLER  PIC X(04)   VALUE "13.0".
               03  FILLER  PIC X(04)   VALUE "13.5".
               03  FILLER  PIC X(04)   VALUE "14.0".
               03  FILLER  PIC X(04)   VALUE "15.0".
               03  FILLER  PIC X(04)   VALUE "16.0".
               03  FILLER  PIC X(04)   VALUE "17.0".
               03  FILLER  PIC X(04)   VALUE "18.0".
               03  FILLER  PIC X(04)   VALUE "19.0".
               03  FILLER  PIC X(04)   VALUE "20.0".
           02  DSP-3.
               03  FILLER  PIC X(04)   VALUE "21.0".
               03  FILLER  PIC X(04)   VALUE "21.5".
               03  FILLER  PIC X(04)   VALUE "22.0".
               03  FILLER  PIC X(04)   VALUE "22.5".
               03  FILLER  PIC X(04)   VALUE "23.0".
               03  FILLER  PIC X(04)   VALUE "23.5".
               03  FILLER  PIC X(04)   VALUE "24.0".
               03  FILLER  PIC X(04)   VALUE "24.5".
               03  FILLER  PIC X(04)   VALUE "25.0".
               03  FILLER  PIC X(04)   VALUE "    ".
           02  DSP-4.
               03  FILLER  PIC X(04)   VALUE "24.0".
               03  FILLER  PIC X(04)   VALUE "24.5".
               03  FILLER  PIC X(04)   VALUE "25.0".
               03  FILLER  PIC X(04)   VALUE "25.5".
               03  FILLER  PIC X(04)   VALUE "26.0".
               03  FILLER  PIC X(04)   VALUE "26.5".
               03  FILLER  PIC X(04)   VALUE "27.0".
               03  FILLER  PIC X(04)   VALUE "27.5".
               03  FILLER  PIC X(04)   VALUE "    ".
               03  FILLER  PIC X(04)   VALUE "    ".
           02  DSP-CGP    PIC X(05)
                                 VALUE "     ".
      *
       01  ACP-AREA.
           02  ACP-TOK  PIC 9(04).
           02  ACP-CCD  PIC 9(03).
           02  ACP-FROM PIC 9(06).
           02  ACP-TO   PIC 9(06).
           02  ACP-GYO  PIC 9(02).
           02  ACP-OKC  PIC 9(01).
      *
       01  DISP-MSG-SPACE1.
           02  FILLER  PIC X(39)     VALUE " ".
      *
           COPY  LSMSG.
           COPY  LIBSCR.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLR-1
       CALL "SD_Init" USING
           "CLR-1" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-1" "X" "1" "0" "12" " " "CLR-1" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "483" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-AREA" "X" "1" "15" "46" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-01" " " "1" "15" "264" "01CLR-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-01" "X" "1" "68" "6" " " "CLR-01" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-01" "X" "1" "75" "6" "01CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-02" " " "1" "0" "252" "02CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-02" "X" "LIN" "4" "5" " " "CLR-02" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-02" "X" "LIN" "10" "2" "01CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03CLR-02" "X" "LIN" "13" "53" "02CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-02" "X" "LIN" "67" "7" "03CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-02" "X" "LIN" "75" "6" "04CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-03" " " "20" "0" "40" "05CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-03" "X" "20" "13" "4" " " "CLR-03" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-03" "X" "20" "20" "4" "01CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-03" "X" "20" "27" "4" "02CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-03" "X" "20" "34" "4" "03CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-03" "X" "20" "41" "4" "04CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-03" "X" "20" "48" "4" "05CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-03" "X" "20" "55" "4" "06CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-03" "X" "20" "62" "4" "07CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-03" "X" "20" "69" "4" "08CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-03" "X" "20" "76" "4" "09CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLEAR" "X" "21" "0" "11" "CLR-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-02" "X" "22" "6" "52" "CLEAR" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-02" "X" "22" "62" "6" "08CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-02" "X" "22" "69" "1" "09CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "11CLR-02" "X" "22" "74" "6" "10CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "12CLR-02" "X" "23" "6" "60" "11CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "13CLR-02" "X" "23" "72" "1" "12CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "14CLR-02" "X" "23" "79" "1" "13CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "15CLR-02" "X" "24" "70" "1" "14CLR-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-SHOSAI" " " "1" "0" "100" "CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-04" " " "20" "0" "40" " " "CLR-SHOSAI" RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-04" "X" "20" "13" "4" " " "CLR-04" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-04" "X" "20" "20" "4" "01CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-04" "X" "20" "27" "4" "02CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-04" "X" "20" "34" "4" "03CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-04" "X" "20" "41" "4" "04CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-04" "X" "20" "48" "4" "05CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-04" "X" "20" "55" "4" "06CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-04" "X" "20" "62" "4" "07CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-04" "X" "20" "69" "4" "08CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-04" "X" "20" "76" "4" "09CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-05" " " "21" "0" "60" "CLR-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-05" "X" "21" "11" "6" " " "CLR-05" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-05" "X" "21" "18" "6" "01CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-05" "X" "21" "25" "6" "02CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-05" "X" "21" "32" "6" "03CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-05" "X" "21" "39" "6" "04CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-05" "X" "21" "46" "6" "05CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-05" "X" "21" "53" "6" "06CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08CLR-05" "X" "21" "60" "6" "07CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09CLR-05" "X" "21" "67" "6" "08CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10CLR-05" "X" "21" "74" "6" "09CLR-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "GAMEN-CLR" " " "LIN" "0" "73" "CLR-SHOSAI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01GAMEN-CLR" "X" "LIN" "4" "5" " " "GAMEN-CLR"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02GAMEN-CLR" "X" "LIN" "10" "2" "01GAMEN-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03GAMEN-CLR" "X" "LIN" "13" "53" "02GAMEN-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04GAMEN-CLR" "X" "LIN" "67" "7" "03GAMEN-CLR" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05GAMEN-CLR" "X" "LIN" "75" "6" "04GAMEN-CLR" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" " " "LIN" "0" "63" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HIZ1" "ZZ" "LIN" "4" "2" " " "01DSP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-HIZ1" BY REFERENCE W-HIZ2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HIZ" "X" "LIN" "6" "1" "DSP-HIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HIZ2" "ZZ" "LIN" "7" "2" "DSP-HIZ" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-HIZ2" BY REFERENCE W-HIZ3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-DEN" "N" "LIN" "10" "2" "DSP-HIZ2" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-DEN" BY REFERENCE W-DEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HCD" "9" "LIN" "13" "6" "DSP-DEN" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-HCD" BY REFERENCE HI-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-HIN" "N" "LIN" "20" "32" "DSP-HCD" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-HIN" BY REFERENCE HI-SMS "32" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-JGP" "99/99" "LIN" "61" "5" "DSP-HIN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-JGP" BY REFERENCE W-GP "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SOK" "---,--9" "LIN" "67" "7" "DSP-JGP" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-SOK" BY REFERENCE W-SOK "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SAS" "9" "LIN" "75" "6" "DSP-SOK" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-SAS" BY REFERENCE W-SAS "6" "0" RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING
            "DSP-AREA1" " " "0" "0" "340" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TOK" "N" "1" "20" "42" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TOK" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "DSP-SHOSAI" " " "1" "20" "133" "DSP-TOK" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SHOSAI1" " " "1" "20" "127" " " "DSP-SHOSAI"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-CHOC" "9" "22" "6" "3" " " "DSP-SHOSAI1"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-CHOC" BY REFERENCE W-CHOC(1) "3" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-CHON" "N" "22" "10" "48" "DSP-CHOC" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-CHON" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SIZE1" "9" "21" "7" "1" "DSP-CHON" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-SIZE1" BY REFERENCE W-SIZE1(1) "1" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-GNO" " " "21" "0" "7" "DSP-SIZE1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-GNO" "9" "22" "62" "6" " " "DSP-GNO" RETURNING RESU.
       CALL "SD_From" USING
            "01DSP-GNO" BY REFERENCE W-GNO1(1) "6" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
           "02DSP-GNO" "9" "22" "69" "1" "01DSP-GNO" " " RETURNING RESU.
       CALL "SD_From" USING
            "02DSP-GNO" BY REFERENCE W-GNO2(1) "1" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-OKNO" "9" "22" "74" "6" "DSP-GNO" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-OKNO" BY REFERENCE W-OKNO(1) "6" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TEK1" "N" "23" "6" "18" "DSP-OKNO" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TEK1" BY REFERENCE W-TEK1(1) "18" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TEK2" "N" "23" "24" "42" "DSP-TEK1" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TEK2" BY REFERENCE W-TEK2(1) "42" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-UNSO" "9" "23" "72" "1" "DSP-TEK2" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-UNSO" BY REFERENCE W-UNSO(1) "1" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KURA" "9" "23" "79" "1" "DSP-UNSO" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KURA" BY REFERENCE W-KURA(1) "1" "1"
            BY REFERENCE GG 133 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SHOSAI2" " " "1" "20" "6" "DSP-SHOSAI1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-SIZE2" "--,---" "21" "COLU" "6" " " "DSP-SHOSAI2"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-SIZE2" BY REFERENCE W-SIZE2(1,1) "5" "2" BY REFERENCE
            GG 133 BY REFERENCE I 5 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-1" " " "20" "0" "40" "DSP-SHOSAI" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-1" "X" "20" "13" "4" " " "DSP-1" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-1" "X" "20" "20" "4" "01DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-1" "X" "20" "27" "4" "02DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-1" "X" "20" "34" "4" "03DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-1" "X" "20" "41" "4" "04DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-1" "X" "20" "48" "4" "05DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-1" "X" "20" "55" "4" "06DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-1" "X" "20" "62" "4" "07DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-1" "X" "20" "69" "4" "08DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-1" "X" "20" "76" "4" "09DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-2" " " "20" "0" "40" "DSP-1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-2" "X" "20" "13" "4" " " "DSP-2" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-2" "X" "20" "20" "4" "01DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-2" "X" "20" "27" "4" "02DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-2" "X" "20" "34" "4" "03DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-2" "X" "20" "41" "4" "04DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-2" "X" "20" "48" "4" "05DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-2" "X" "20" "55" "4" "06DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-2" "X" "20" "62" "4" "07DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-2" "X" "20" "69" "4" "08DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-2" "X" "20" "76" "4" "09DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-3" " " "20" "0" "40" "DSP-2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-3" "X" "20" "13" "4" " " "DSP-3" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-3" "X" "20" "20" "4" "01DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-3" "X" "20" "27" "4" "02DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-3" "X" "20" "34" "4" "03DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-3" "X" "20" "41" "4" "04DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-3" "X" "20" "48" "4" "05DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-3" "X" "20" "55" "4" "06DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-3" "X" "20" "62" "4" "07DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-3" "X" "20" "69" "4" "08DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-3" "X" "20" "76" "4" "09DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-4" " " "20" "0" "40" "DSP-3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-4" "X" "20" "13" "4" " " "DSP-4" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-4" "X" "20" "20" "4" "01DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-4" "X" "20" "27" "4" "02DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-4" "X" "20" "34" "4" "03DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-4" "X" "20" "41" "4" "04DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-4" "X" "20" "48" "4" "05DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-4" "X" "20" "55" "4" "06DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-4" "X" "20" "62" "4" "07DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09DSP-4" "X" "20" "69" "4" "08DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10DSP-4" "X" "20" "76" "4" "09DSP-4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-CGP" "X" "LIN" "61" "5" "DSP-4" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TOK" "9" "1" "15" "4" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TOK" BY REFERENCE W-TOK "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CCD" "9" "22" "6" "3" "ACP-TOK" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-CCD" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-FROM" "9" "1" "68" "6" "ACP-CCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-FROM" BY REFERENCE W-FROMS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TO" "9" "1" "75" "6" "ACP-FROM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TO" BY REFERENCE W-TOS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-GYO" "9" "21" "2" "2" "ACP-TO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-GYO" BY REFERENCE W-GYO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "9" "24" "70" "1" "ACP-GYO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING
            "DISP-MSG-SPACE1" " " "24" "0" "39" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-MSG-SPACE1" "X" "24" "1" "39" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
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
           MOVE  W-TOK    TO  TC-TCD.
           MOVE  "001"    TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           IF  SW  =  " ON"
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  MAIN-RTN
           END-IF
           CALL "SD_Output" USING "DSP-TOK" DSP-TOK "p" RETURNING RESU.
           IF  JS-SIGN    =  0
               GO  TO  MAIN-010
           END-IF.
       MAIN-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-CCD "ACP-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-RTN
           END-IF
           IF  ESTAT NOT  =  "01" AND "06"
               GO  TO  MAIN-005
           END-IF
           CALL "SD_Output" USING "ACP-CCD" ACP-CCD "p" RETURNING RESU.
           MOVE  W-TOK    TO  TC-TCD.
           MOVE  W-CCD    TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           IF  SW  =  " ON"
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  MAIN-005
           END-IF
           CALL "SD_Output" USING
            "DSP-CHON" DSP-CHON "p" RETURNING RESU.
       MAIN-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM "ACP-FROM" "9"
           "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  JS-SIGN  =  1
                   GO  TO  MAIN-005
               ELSE
                   GO  TO  MAIN-RTN
               END-IF
           END-IF
           IF  ESTAT NOT  =  "01" AND "04" AND "06"
               GO  TO  MAIN-010
           END-IF
           IF  ESTAT      =  "04"
               ACCEPT  W-FROMS  FROM  DATE
           END-IF
           CALL "SD_Output" USING
            "ACP-FROM" ACP-FROM "p" RETURNING RESU.
           IF  W-FROMS   =  ZERO
               MOVE  ZERO     TO  W-FROM
               GO  TO  MAIN-020
           END-IF
           IF  (W-FGET  <  1  OR  >  12)  OR
               (W-FPEY  <  1  OR  >  31)
               GO  TO  MAIN-010
           END-IF
           MOVE  ZERO      TO  W-FNEN1.
           IF  W-FNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-FNEN
           END-IF
           IF  W-FNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-FNEN
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO "ACP-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MAIN-010
           END-IF
           IF  ESTAT NOT  =  "01" AND "04" AND "06"
               GO  TO  MAIN-020
           END-IF
           IF  ESTAT      =  "04"
               ACCEPT  W-TOS  FROM  DATE
           END-IF
           IF  W-TOS     =  ZERO  OR  999999
               MOVE  99999999 TO  W-TO
               CALL "SD_Output" USING "ACP-TO" ACP-TO "p" RETURNING RESU
               GO  TO  MAIN-025
           END-IF
           CALL "SD_Output" USING "ACP-TO" ACP-TO "p" RETURNING RESU.
           IF  (W-TGET  <  1  OR  >  12)  OR
               (W-TPEY  <  1  OR  >  31)
               GO  TO  MAIN-020
           END-IF
           MOVE  ZERO      TO  W-TNEN1.
           IF  W-TNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-TNEN
           END-IF
           IF  W-TNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-TNEN
           END-IF
           IF  W-FROM  >  W-TO
               GO  TO  MAIN-020
           END-IF.
       MAIN-025.
           MOVE  2        TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           MOVE  ZERO     TO  G.
      *
           MOVE  0        TO  W-DC.
           INITIALIZE  JNSR-KEY3.
           MOVE  W-TOK    TO  JNSR-19.
           MOVE  W-FROM   TO  JNSR-20.
      *           IF  JS-SIGN    =  1
      *               SELECT  JNSR WHERE W-CCD = JNSR-112
      *           END-IF
      */////////////// add koyama 20161102
           IF  JS-SIGN =  1
               CALL "DB_Select" USING
                JNSR_PNAME1 "WHERE"
                W-CCD "=" "JNSR-112" RETURNING RET
           END-IF
      *           START  JNSR  KEY  NOT  <  JNSR-KEY3  INVALID
      *///////////////
           CALL "DB_Start" USING
            JNSR_PNAME1 "JNSR-KEY3" " NOT < " JNSR-KEY3 RETURNING RET.
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
           IF  W-TOK  NOT =  JNSR-19
               GO  TO  MAIN-040
           END-IF
           MOVE   JNSR-20    TO  W-DATE.
           IF  W-TO  <  W-DATE
               GO  TO  MAIN-040
           END-IF
           IF  JS-SIGN    =  1
               IF  W-CCD  NOT =  JNSR-112
                   GO  TO  MAIN-030
               END-IF
           END-IF
           MOVE   W-DATE     TO  W-HIZ.
           COMPUTE  W-SOK    =   JNSR-081(01)  +  JNSR-081(02) +
                                 JNSR-081(03)  +  JNSR-081(04) +
                                 JNSR-081(05)  +  JNSR-081(06) +
                                 JNSR-081(07)  +  JNSR-081(08) +
                                 JNSR-081(09)  +  JNSR-081(10).
           MOVE   JNSR-221   TO  W-SAS.
       NEXT-DSP.
           ADD  1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           ADD  1     TO  G.
           IF  LIN  =  20
               SUBTRACT  1     FROM  G
               GO  TO  MAIN-OKC
           END-IF
           PERFORM  WORK-RTN     THRU  WORK-EX.
           PERFORM  DSP-RTN      THRU  DSP-EX.
           GO  TO  MAIN-030.
       MAIN-040.
           MOVE  1        TO  W-DC.
           INITIALIZE  JTW-KEY3.
           MOVE  W-TOK    TO  JTW-19.
           MOVE  W-FROM   TO  JTW-20.
      *           IF  JS-SIGN    =  1
      *               SELECT  JT-W170 WHERE W-CCD = JTW-112
      *           END-IF
      */////////////// add koyama 20161102
           IF  JS-SIGN    =  1
               CALL "DB_Select" USING
                JT-W170_PNAME1 "WHERE"
                W-CCD "=" "JNSR-112" RETURNING RET
           END-IF
      *           START  JT-W170  KEY  NOT  <  JTW-KEY3  INVALID
      *///////////////
           CALL "DB_Start" USING
            JT-W170_PNAME1 "JTW-KEY3" " NOT < " JTW-KEY3 RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-047
           END-IF.
       MAIN-042.
      ***  èoâ◊ó›åvÉèÅ[ÉNÅ@ÇqÇdÇ`Çc
      *           READ  JT-W170 NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-W170_PNAME1 BY REFERENCE JTW-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-047
           END-IF
           IF  W-TOK  NOT =  JTW-19
               GO  TO  MAIN-047
           END-IF
           MOVE   JTW-20     TO  W-DATE.
           IF  W-TO  <  W-DATE
               GO  TO  MAIN-047
           END-IF
           IF  JS-SIGN    =  1
               IF  W-CCD  NOT =  JTW-112
                   GO  TO  MAIN-042
               END-IF
           END-IF
           MOVE   ZERO       TO  W-HIZ.
           COMPUTE  W-SOK    =   JTW-081(01)  +  JTW-081(02) +
                                 JTW-081(03)  +  JTW-081(04) +
                                 JTW-081(05)  +  JTW-081(06) +
                                 JTW-081(07)  +  JTW-081(08) +
                                 JTW-081(09)  +  JTW-081(10).
           MOVE   JTW-221    TO  W-SAS.
       MAIN-045.
           ADD  1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           ADD  1     TO  G.
           IF  LIN  =  20
               SUBTRACT  1     FROM  G
               GO  TO  MAIN-OKC
           END-IF
           PERFORM  WORK1-RTN    THRU  WORK1-EX.
           PERFORM  DSP1-RTN     THRU  DSP1-EX.
           GO  TO  MAIN-042.
       MAIN-047.
           IF  LIN  =  2
               CALL "SD_Output" USING
                "INV-D01" INV-D01 "p" RETURNING RESU
               GO  TO  MAIN-010
           END-IF
           IF  LIN  NOT =  19
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
               PERFORM  CLR1-RTN     THRU  CLR1-EX
               GO  TO  MAIN-RTN
           END-IF
           IF  LIN  =  20
               PERFORM  CLR3-RTN     THRU  CLR3-EX
               MOVE  2        TO  LIN
               CALL "SD_Arg_Match_Line" USING
                "LIN" "2" LIN RETURNING RESU
               MOVE  ZERO     TO  G
               IF  W-DC      =  0
                   GO  TO  NEXT-DSP
               ELSE
                   GO  TO  MAIN-045
               END-IF
           END-IF
           IF  LIN  NOT =  20
               CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU
               PERFORM  CLR1-RTN     THRU  CLR1-EX
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-EX.
           EXIT.
      ************************************
      *    ÇvÇnÇqÇjÅ|ÇqÇsÇmÅ@Å@Å@        *
      ************************************
       WORK-RTN.
           MOVE   0          TO  W-KBN.
           IF  JNSR-151         NOT  =  ZERO
               IF  JNSR-13               =  0
                   MOVE  3          TO  W-KBN
               ELSE
                   MOVE  JNSR-13    TO  W-KBN
               END-IF
           END-IF
           MOVE  JNSR-07         TO  W-SIZE1(G).
           MOVE  JNSR-11         TO  W-CHO(G).
           MOVE  JNSR-151        TO  W-GNO1(G).
           MOVE  JNSR-152        TO  W-GNO2(G).
           MOVE  JNSR-14         TO  W-UNSO(G).
           MOVE  JNSR-23         TO  W-TEK1(G).
           MOVE  JNSR-24         TO  W-TEK2(G).
           MOVE  JNSR-12         TO  W-OKNO(G).
           MOVE  JNSR-06         TO  W-KURA(G).
           MOVE  1               TO  I.
       WORK-010.
           IF  I  >  10
               GO  TO  WORK-EX
           END-IF
           MOVE  JNSR-081(I)     TO  W-SIZE2(G I).
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
               IF  W-KBN    =  3
                   MOVE  "éÛíç"    TO  W-DEN
               ELSE
                   IF  W-KBN    =  5
                       MOVE  "óaÇË"    TO  W-DEN
                   ELSE
                       IF  W-KBN    =  6
                           MOVE  "éÊÇË"    TO  W-DEN
                       ELSE
                           IF  W-KBN    =  0
                               MOVE  "èoâ◊"    TO  W-DEN
                           END-IF
                       END-IF
                   END-IF
               END-IF
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
           IF  JNSR-10  =  5
               MOVE  "ï‘ëº"    TO  W-DEN
           END-IF
           IF  JNSR-10  =  7
               MOVE  "ÉTèo"    TO  W-DEN
           END-IF
           MOVE  JNSR-01         TO  HI-MHCD HI-HCD.
           PERFORM  HIM-READ-RTN     THRU  HIM-READ-EX.
           MOVE  JNSR-151        TO  JMSTD-07.
           MOVE  JNSR-152        TO  JMSTD-08.
           PERFORM  JMS-READ-RTN     THRU  JMS-READ-EX.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           IF  W-GP     =  ZERO
               CALL "SD_Output" USING
                "DSP-CGP" DSP-CGP "p" RETURNING RESU
           END-IF.
       DSP-EX.
           EXIT.
      ************************************
      *    ÇvÇnÇqÇjÇPÅ|ÇqÇsÇmÅ@Å@        *
      ************************************
       WORK1-RTN.
           MOVE   0          TO  W-KBN.
           IF  JTW-151          NOT  =  ZERO
               IF  JTW-13                =  0
                   MOVE  3          TO  W-KBN
               ELSE
                   MOVE  JTW-13     TO  W-KBN
               END-IF
           END-IF
           MOVE  JTW-07          TO  W-SIZE1(G).
           MOVE  JTW-11          TO  W-CHO(G).
           MOVE  JTW-151         TO  W-GNO1(G).
           MOVE  JTW-152         TO  W-GNO2(G).
           MOVE  JTW-14          TO  W-UNSO(G).
           MOVE  JTW-23          TO  W-TEK1(G).
           MOVE  JTW-24          TO  W-TEK2(G).
           MOVE  JTW-12          TO  W-OKNO(G).
           MOVE  JTW-06          TO  W-KURA(G).
           MOVE  1               TO  I.
       WORK1-010.
           IF  I  >  10
               GO  TO  WORK1-EX
           END-IF
           MOVE  JTW-081(I)     TO  W-SIZE2(G I).
           ADD  1     TO  I.
           GO  TO  WORK1-010.
       WORK1-EX.
           EXIT.
      ************************************
      *    ÇcÇrÇoÇPÅ|ÇqÇsÇmÅ@Å@Å@        *
      ************************************
       DSP1-RTN.
           MOVE  SPACE           TO  W-DEN.
           IF  JTW-10   =  0
               IF  W-KBN    =  3
                   MOVE  "éÛíç"    TO  W-DEN
               ELSE
                   IF  W-KBN    =  5
                       MOVE  "óaÇË"    TO  W-DEN
                   ELSE
                       IF  W-KBN    =  6
                           MOVE  "éÊÇË"    TO  W-DEN
                       ELSE
                           IF  W-KBN    =  0
                               MOVE  "èoâ◊"    TO  W-DEN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  JTW-10   =  1
               MOVE  "ï‘ïi"    TO  W-DEN
           END-IF
           IF  JTW-10   =  2
               MOVE  "ïsó«"    TO  W-DEN
           END-IF
           IF  JTW-10   =  3
               MOVE  "í˘ê≥"    TO  W-DEN
           END-IF
           IF  JTW-10   =  7
               MOVE  "ÉTèo"    TO  W-DEN
           END-IF
           MOVE  JTW-01          TO  HI-MHCD HI-HCD.
           PERFORM  HIM-READ-RTN     THRU  HIM-READ-EX.
           MOVE  JTW-151         TO  JMSTD-07.
           MOVE  JTW-152         TO  JMSTD-08.
           PERFORM  JMS-READ-RTN     THRU  JMS-READ-EX.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           IF  W-GP     =  ZERO
               CALL "SD_Output" USING
                "DSP-CGP" DSP-CGP "p" RETURNING RESU
           END-IF.
       DSP1-EX.
           EXIT.
      ************************************
      *    ÇfÇ`ÇlÇdÇmÅ|ÇbÇkÇqÅ|ÇqÇsÇm    *
      ************************************
       GAMEN-CLR-RTN.
           IF  LIN  >  19
               GO  TO  GAMEN-CLR-EX
           END-IF
           CALL "SD_Output" USING
            "GAMEN-CLR" GAMEN-CLR "p" RETURNING RESU.
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
           MOVE  W-CHO(GG)      TO  TC-KEY.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           CALL "SD_Output" USING
            "DSP-SHOSAI1" DSP-SHOSAI1 "p" RETURNING RESU.
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
           MOVE  11    TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
       SHOSAI-010.
           IF  I  >  10
               GO  TO  SHOSAI-020
           END-IF
           CALL "SD_Output" USING
            "DSP-SIZE2" DSP-SIZE2 "p" RETURNING RESU.
           ADD  1     TO  I.
           ADD  7     TO  COLU.
           CALL "SD_Arg_Match_Col" USING "COLU" "2" COLU RETURNING RESU.
           GO  TO  SHOSAI-010.
       SHOSAI-020.
           GO  TO  SHOSAI-RTN.
       SHOSAI-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇmÇP                       *
      *********************************************
       CLR1-RTN.
           INITIALIZE  W-AREA1.
           MOVE  3     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR1-010.
           IF  LIN  >  19
               GO  TO  CLR1-EX
           END-IF
           CALL "SD_Output" USING
            "CLR-AREA" CLR-AREA "p" RETURNING RESU.
           ADD   1     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
           GO  TO  CLR1-010.
       CLR1-EX.
           EXIT.
      *********************************************
      *    ÇbÇkÇqÅ|ÇqÇsÇmÇR                       *
      *********************************************
       CLR3-RTN.
           INITIALIZE  W-SHOSAI  W-DEN.
           MOVE  3     TO  LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
       CLR3-010.
           IF  LIN  >  19
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
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN   NOT =  0    AND   1
               CALL "DB_Close"
               STOP  RUN
           END-IF
           CALL "SD_Output" USING "CLR-1" CLR-1 "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ640R" RETURNING RESU.
           CALL  "CBLSTNNO"  USING  STN-NO USER_ID.
           MOVE  STN-NO-02   TO     WORK1-ID-02  WORK2-ID-02.
           MOVE  WF1-ID      TO     JT-W170K1ID.
           MOVE  JT-W170K1ID TO     JT-W170_PNAME1.
           MOVE  WF2-ID      TO     JT-W170K3ID.
           MOVE  JT-W170K3ID TO     JT-W170_PNAME2.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2" BY
            REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE JMSTD-KEY3.
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
           CALL "DB_F_Open" USING
            "INPUT" JT-W170_PNAME1 "SHARED" BY REFERENCE JT-W170_IDLST
            "2" "JTW-KEY1" BY REFERENCE JTW-KEY1 "JTW-KEY3" BY REFERENCE
            JTW-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           COPY  LIBCPR.
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
           CALL "DB_F_Close" USING
            BY REFERENCE JT-W170_IDLST JT-W170_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
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
               MOVE  SPACE     TO   HI-SMS
           END-IF.
       HIM-READ-EX.
           EXIT.
      *
       JMS-READ-RTN.
           MOVE  ZERO         TO  W-GP.
      *           READ  JMSTD  UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  JMS-READ-EX
           END-IF
           MOVE  JMSTD-022    TO  W-GET.
           MOVE  JMSTD-023    TO  W-PEY.
       JMS-READ-EX.
           EXIT.
      *
       COPY LPMSG.
