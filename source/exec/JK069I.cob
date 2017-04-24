       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JK070I.
       AUTHOR.               IKUMI.N.
      **************************************************
      *    PROGRAM      :    èoâ◊í˘ê≥ì¸óÕ              *
      *    DATA WRITTEN :    90/04/20                  *
      *    SCREEN  USED :    SK070I                    *
      *    COMPILE TYPE :    COBOL                     *
      *    UPDATE       :    91/10/24                  *
      **************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT        PIC  X(02).
       77  DIS-SW          PIC  9(01).
       01  LCNT            PIC  9(02)  VALUE  90.
       01  PCNT            PIC  9(04)  VALUE  0.
       01  W-NEXT          PIC  9(01).
       01  ENDFLG          PIC  X(03)  VALUE  SPACE.
       01  SV-JSTR         PIC  X(256).
       01  W-AREA1.
           02  WYMD.
               03  WYY     PIC  9(02).
               03  WMM     PIC  9(02).
               03  WDD     PIC  9(02).
           02  N-24        PIC  N(24)   VALUE  ALL   "Å@".
       01  W-AREA2.
           02  O           PIC  9(01)   VALUE  ZERO.
           02  M           PIC  9(02)   VALUE  ZERO.
           02  A           PIC  9(01)   VALUE  ZERO.
           02  B           PIC  9(02)   VALUE  ZERO.
           02  C           PIC  9(01)   VALUE  ZERO.
           02  LIN1        PIC  9(02)   VALUE  ZERO.
           02  LIN2        PIC  9(02)   VALUE  ZERO.
           02  COL1        PIC  9(02)   VALUE  ZERO.
           02  WA          PIC  9(01)   VALUE  ZERO.
           02  WB          PIC  9(03)   VALUE  ZERO.
           02  AW          PIC  9(01).
           02  LIN2W       PIC  9(02).
       01  ACT-WORK1.
           02  W-ACT       PIC  9(01)   VALUE  ZERO.
           02  W-NAM       PIC  N(05)   VALUE  SPACE.
       01  ACT-WORK2.
           02  WK-TCCD     PIC  9(07).
           02  WK-TCCD-R   REDEFINES    WK-TCCD.
               03  WK-TCD  PIC  9(04).
               03  WK-CCD  PIC  9(03).
           02  W-1         PIC  9(06)   VALUE  ZERO.
           02  W-1R    REDEFINES   W-1.
               03  W1-1    PIC  9(01).
               03  W1-2    PIC  9(05).
           02  W-2.
               03  W-21    PIC  9(01)   VALUE  ZERO.
               03  W-22    PIC  N(02)   VALUE  SPACE.
           02  W-3.
               03  W-31    PIC  9(04)   VALUE  ZERO.
               03  W-31L    REDEFINES  W-31.
                   04  W-311    PIC  9(02).
                   04  W-312    PIC  9(02).
               03  W-32    PIC  9(02)   VALUE  ZERO.
               03  W-33    PIC  9(02)   VALUE  ZERO.
           02  W-3L    REDEFINES   W-3.
               03  F       PIC  9(02).
               03  W-3S    PIC  9(06).
           02  W-4.
               03  W-4A.
                   04  W-41        PIC  9(04)   VALUE  ZERO.
                   04  W-42        PIC  9(03)   VALUE  ZERO.
               03  W-43    PIC  N(24)   VALUE  SPACE.
               03  W-44    PIC  N(24)   VALUE  SPACE.
           02  W-5.
               03  W-51    PIC  9(01)   VALUE  ZERO.
               03  W-52    PIC  N(06)   VALUE  SPACE.
           02  W-5A        PIC  9(03)   VALUE  ZERO.
           02  W-6.
               03  W-6A    OCCURS   6.
                   04  W-61.
                       05  W-611   PIC  9(06).
                       05  W-612   PIC  9(01).
                   04  W-62.
                       05  W-621   PIC  9(06).
                       05  W-622   PIC  N(24).
                   04  W-63        PIC  9(01).
                   04  W-64A.
                       05  W-64    PIC  S9(04)  OCCURS  10.
                   04  W-65        PIC  S9(05).
           02  W-OKC       PIC  9(01)   VALUE  ZERO.
           02  W-KOS       PIC S9(03)   VALUE  ZERO.
           02  W-KEI       PIC S9(07)   VALUE  ZERO.
           02  W-OLD.
               03  CNT     PIC  9(01)   VALUE  ZERO.
               03  O-HIN   PIC  9(06)   VALUE  ZERO.
           02  W-KATON.
               03  W-KA            PIC  9(03)   OCCURS  6.
       01  SAVE-WORK.
           02  SV-1         PIC  9(06)   VALUE  ZERO.
           02  SV-3.
               03  SV-31    PIC  9(04)   VALUE  ZERO.
               03  SV-31L    REDEFINES  SV-31.
                   04  SV-311    PIC  9(02).
                   04  SV-312    PIC  9(02).
               03  SV-32    PIC  9(02)   VALUE  ZERO.
               03  SV-33    PIC  9(02)   VALUE  ZERO.
           02  SV-4.
               03  SV-4A.
                   04  SV-41        PIC  9(04)   VALUE  ZERO.
                   04  SV-42        PIC  9(03)   VALUE  ZERO.
           02  SV-5A        PIC  9(03).
           02  SV-6.
               03  SV-6A    OCCURS   6.
                   04  SV-62.
                       05  SV-621   PIC  9(06).
                   04  SV-63        PIC  9(01).
                   04  SV-64A.
                       05  SV-64    PIC  S9(04)  OCCURS  10.
                   04  SV-65        PIC  S9(05).
           02  SV-14B       PIC  9(06).
           02  SV-10        PIC S9(03).
           02  SV-KEI       PIC S9(07)   VALUE  ZERO.
       01  REV-AREA.
           02  REV-1.
               03  REV-1A          PIC  X(40)     VALUE
                   "1               SS     S    M     L     ".
               03  REV-1B          PIC  X(21)     VALUE
                   "LL  28.0  29.0  30.0 ".
           02  REV-2.
               03  REV-2A          PIC  X(40)     VALUE
                   "2 12.5  13.0  13.5  14.0  15.0  16.0  17".
               03  REV-2B          PIC  X(21)     VALUE
                   ".0  18.0  19.0  20.0 ".
           02  REV-3.
               03  REV-3A          PIC  X(40)     VALUE
                   "3 21.0  21.5  22.0  22.5  23.0  23.5  24".
               03  REV-3B          PIC  X(21)     VALUE
                   ".0  24.5  25.0  ---- ".
           02  REV-4.
               03  REV-4A          PIC  X(40)     VALUE
                   "4 24.0  24.5  25.0  25.5  26.0  26.5  27".
               03  REV-4B          PIC  X(21)     VALUE
                   ".0  27.5  ----  ---- ".
      *
           COPY     LWMSG.
      *
           COPY     L-JSTR.
           COPY     LIHIM2.
           COPY     LITCM.
           COPY     L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-ACT     PIC  9(01).
           02  DSP-NAM     PIC  N(05).
           02  ACP-1       PIC  9(06).
           02  ACP-2.
               03  A-21       PIC  9(01).
               03  D-22       PIC  N(02).
           02  ACP-3.
               03  A-31       PIC  9(02).
               03  A-32       PIC  9(02).
               03  A-33       PIC  9(02).
           02  ACP-4.
               03  A-41       PIC  9(04).
               03  A-42       PIC  9(03).
               03  D-43       PIC  N(24).
           02  D-44   PIC  N(24).
           02  ACP-5.
               03  A-51       PIC  9(01).
               03  D-52       PIC  N(06).
               03  A-53       PIC  9(03).
               03  D-54       PIC  ZZ9 .
           02  ACP-6A.
               03  A-611      PIC  9(06).
               03  A-612      PIC  9(01).
               03  A-613      PIC  X(01)  VALUE  "-".
               03  A-612A     PIC  X(08)  VALUE  " ".
               03  D-622      PIC  N(24).
           02  ACP-6B.
               03  A-621      PIC  9(06).
               03  A-63       PIC  9(01).
               03  A-64       PIC  S9(04).
               03  D-64       PIC  ZZZZ- .
               03  D-65       PIC ZZZ,ZZZ- .
           02  DSP-6B.
               03  DIS-1   PIC  ZZZZ- .
               03  DIS-2   PIC  ZZZZ- .
               03  DIS-3   PIC  ZZZZ- .
               03  DIS-4   PIC  ZZZZ- .
               03  DIS-5   PIC  ZZZZ- .
               03  DIS-6   PIC  ZZZZ- .
               03  DIS-7   PIC  ZZZZ- .
               03  DIS-8   PIC  ZZZZ- .
               03  DIS-9   PIC  ZZZZ- .
               03  DIS-10  PIC  ZZZZ- .
           02  DSP-KEI     PIC  ZZZ,ZZZ- .
           02  DSP-LINE    PIC  X(01)  VALUE  "-".
           02  ACP-KOS     PIC S9(03).
           02  DSP-KOS     PIC ZZ9- .
           02  ACP-OKC     PIC  9(01).
       01  DSP-CLE.
           02  CLE-01.
               03  C-11.
                   04  FILLER  PIC  X(06) VALUE "      ".
                   04  FILLER  PIC  X(01) VALUE " ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(02) VALUE "  ".
                   04  FILLER  PIC  X(02) VALUE "  ".
                   04  FILLER  PIC  X(02) VALUE "  ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(03) VALUE "   ".
                   04  C-111   PIC  N(24).
               03  C-12.
                   04  FILLER  PIC  X(01) VALUE " ".
                   04  FILLER  PIC  X(12) VALUE   "Å@Å@Å@Å@Å@Å@".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  C-121   PIC  N(24).
               03  C-13.
                   04  FILLER  PIC  X(01) VALUE " ".
                   04  FILLER  PIC  X(12) VALUE   "Å@Å@Å@Å@Å@Å@".
                   04  C-131.
                       05  FILLER  PIC  X(01) VALUE " ".
                       05  FILLER  PIC  X(06) VALUE "      ".
                       05  FILLER  PIC  X(02) VALUE "  ".
                   04  FILLER  PIC  X(08) VALUE "        ".
               03  C-14.
                   04  FILLER  PIC  X(04) VALUE  "    ".
               03  C-15    PIC  X(09) VALUE "         ".
               03  C-16    PIC  X(01) VALUE " ".
           02  CLE-02.
               03  CLE-21.
                   04  FILLER  PIC  X(07) VALUE "       ".
                   04  FILLER  PIC  X(01) VALUE " ".
                   04  FILLER  PIC  X(24)
                               VALUE "                        ".
                   04  FILLER  PIC  X(24)
                               VALUE "                        ".
               03  CLE-22.
                   04  FILLER  PIC  X(07) VALUE "       ".
                   04  FILLER  PIC  X(01) VALUE " ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(05) VALUE "     ".
                   04  FILLER  PIC  X(08) VALUE "        ".
           02  CLE-03      PIC  X(06) VALUE  "      ".
           02  CLE-04.
               03  C-41    PIC  X(08) VALUE  "        ".
               03  C-42    PIC  X(07) VALUE  "       ".
       01  DSP-REV-AREA.
           02  DSP-RR-ALL.
            03 DSP-RR1     PIC  X(61).
            03 DSP-RR2     PIC  X(61).
            03 DSP-RR3     PIC  X(61).
            03 DSP-RR4     PIC  X(61).
           02  DSP-RC-ALL.
            03 DSP-RC1     PIC  X(61).
            03 DSP-RC2     PIC  X(61).
            03 DSP-RC3     PIC  X(61).
            03 DSP-RC4     PIC  X(61).
       01  DSP-ERR.
           02  INV-M02     PIC  X(28) 
                           VALUE   "ÅñÅñïiñºÉ}ÉXÉ^Å[Å@ñ¢ìoò^ÅñÅñ".
           02  ERR-03      PIC  X(24) 
                           VALUE   "ÅñÅñÅ@äYìñì`ï[ñ≥ÇµÅ@ÅñÅñ".
           02  ERR-04      PIC  X(22) 
                           VALUE   "ÅñÅñÅ@ì`ãÊÉGÉâÅ[Å@ÅñÅñ".
           02  ERR-05      PIC  X(28) 
                           VALUE   "ÅñÅñÅ@ì¸êîÇ™ìØàÍÇ≈Ç»Ç¢Å@ÅñÅñ".
           02  ERR-06      PIC  X(32) 
                           VALUE   "ÅñÅ@égópÉTÉCÉYÅÅÇyÇdÇqÇnÇ≈Ç∑Å@Åñ".
           02  ERR-SET     PIC  X(26) 
               VALUE   "ÅñÅ@ÉZÉbÉgêîó Å@ÉGÉâÅ[Å@Åñ".
      **
      **   MESSEGE  AREA
      **
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(60).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4A02".
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4202".
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
      ************************
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "305" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "1" "67" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-NAM" "N" "1" "69" "10" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-NAM" BY REFERENCE W-NAM "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-1" "9" "3" "2" "6" "DSP-NAM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-1" BY REFERENCE W-1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-2" " " "3" "0" "5" "ACP-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-21" "9" "3" "9" "1" " " "ACP-2" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-21" BY REFERENCE W-21 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-22" "N" "3" "10" "4" "A-21" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-22" BY REFERENCE W-22 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-3" " " "3" "0" "6" "ACP-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-31" "9" "3" "15" "2" " " "ACP-3" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-31" BY REFERENCE W-312 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-32" "9" "3" "18" "2" "A-31" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-32" BY REFERENCE W-32 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-33" "9" "3" "21" "2" "A-32" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-33" BY REFERENCE W-33 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-4" " " "3" "0" "55" "ACP-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-41" "9" "3" "24" "4" " " "ACP-4" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-41" BY REFERENCE W-41 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-42" "9" "3" "29" "3" "A-41" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-42" BY REFERENCE W-42 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-43" "N" "3" "33" "48" "A-42" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-43" BY REFERENCE W-43 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-44" "N" "4" "33" "48" "ACP-4" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-44" BY REFERENCE W-44 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-5" " " "4" "0" "19" "D-44" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-51" "9" "4" "7" "1" " " "ACP-5" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-51" BY REFERENCE W-51 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-52" "N" "4" "9" "12" "A-51" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-52" BY REFERENCE W-52 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-53" "9" "4" "29" "3" "D-52" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-53" BY REFERENCE W-5A "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-54" "ZZ9" "4" "29" "3" "A-53" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-54" BY REFERENCE W-5A "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6A" " " "LIN1" "0" "64" "ACP-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-611" "9" "LIN1" "2" "6" " " "ACP-6A" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-611" BY REFERENCE W-611(1) "6" "1" BY REFERENCE A 107
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-612" "9" "LIN1" "9" "1" "A-611" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-612" BY REFERENCE W-612(1) "1" "1" BY REFERENCE A 107
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-613" "X" "LIN1" "8" "1" "A-612" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-612A" "X" "LIN1" "2" "8" "A-613" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-622" "N" "LIN1" "11" "48" "A-612A" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-622" BY REFERENCE W-622(1) "48" "1" BY REFERENCE A 107
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-6B" " " "LIN1 PLUS 1" "0" "24" "ACP-6A" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-621" "9" "LIN1 PLUS 1" "2" "6" " " "ACP-6B"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "A-621" BY REFERENCE W-621(1) "6" "1" BY REFERENCE A 107
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-63" "9" "LIN1 PLUS 1" "11" "1" "A-621" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "A-63" BY REFERENCE W-63(1) "1" "1" BY REFERENCE A 107
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-64" "S9" "LIN1 PLUS 1" "COL1" "4" "A-63" " "
            RETURNING RESU.
       CALL "SD_Into" USING 
            "A-64" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            BY REFERENCE B 2 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-64" "ZZZZ-" "LIN1 PLUS 1" "COL1" "5" "A-64" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-64" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            BY REFERENCE B 2 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-65" "ZZZ,ZZZ-" "LIN1 PLUS 1" "73" "8" "D-64" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-65" BY REFERENCE W-65(1) "5" "1" BY REFERENCE A 107
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-6B" " " "LIN1 PLUS 1" "0" "50" "ACP-6B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-1" "ZZZZ-" "LIN1 PLUS 1" "13" "5" " " "DSP-6B"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-1" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            "1" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-2" "ZZZZ-" "LIN1 PLUS 1" "19" "5" "DIS-1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-2" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            "2" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-3" "ZZZZ-" "LIN1 PLUS 1" "25" "5" "DIS-2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-3" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            "3" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-4" "ZZZZ-" "LIN1 PLUS 1" "31" "5" "DIS-3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-4" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            "4" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-5" "ZZZZ-" "LIN1 PLUS 1" "37" "5" "DIS-4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-5" BY REFERENCE W-64(1,1) "4" "2" BY REFERENCE A 107
            "5" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-6" "ZZZZ-" "LIN1 PLUS 1" "43" "5" "DIS-5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-6" BY REFERENCE W-64(1,1) "4" "2"  BY REFERENCE A 107
            "6" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-7" "ZZZZ-" "LIN1 PLUS 1" "49" "5" "DIS-6" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-7" BY REFERENCE W-64(1,1) "4" "2"  BY REFERENCE A 107
            "7" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-8" "ZZZZ-" "LIN1 PLUS 1" "55" "5" "DIS-7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-8" BY REFERENCE W-64(1,1) "4" "2"  BY REFERENCE A 107
            "8" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-9" "ZZZZ-" "LIN1 PLUS 1" "61" "5" "DIS-8" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-9" BY REFERENCE W-64(1,1) "4" "2"  BY REFERENCE A 107
            "9" 1 RETURNING RESU.
       CALL "SD_Init" USING 
            "DIS-10" "ZZZZ-" "LIN1 PLUS 1" "67" "5" "DIS-9" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DIS-10" BY REFERENCE W-64(1,1) "4" "2"  BY REFERENCE A 107
            "10" 2 RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KEI" "ZZZ,ZZZ-" "22" "73" "8" "DSP-6B" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KEI" BY REFERENCE W-KEI "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-LINE" "X" "3" "28" "1" "DSP-KEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KOS" "S9" "23" "7" "3" "DSP-LINE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KOS" BY REFERENCE W-KOS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-KOS" "ZZ9-" "23" "7" "4" "ACP-KOS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-KOS" BY REFERENCE W-KOS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "60" "1" "DSP-KOS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-CLE
       CALL "SD_Init" USING 
            "DSP-CLE" " " "0" "0" "324" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" " " "0" "0" "181" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-11" " " "3" "0" "72" " " "CLE-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-11" "X" "3" "2" "6" " " "C-11" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-11" "X" "3" "9" "1" "01C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-11" "X" "3" "10" "4" "02C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-11" "X" "3" "15" "2" "03C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-11" "X" "3" "18" "2" "04C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-11" "X" "3" "21" "2" "05C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-11" "X" "3" "24" "4" "06C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-11" "X" "3" "29" "3" "07C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-111" "N" "3" "33" "48" "08C-11" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-111" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-12" " " "4" "0" "65" "C-11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-12" "X" "4" "7" "1" " " "C-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-12" "X" "4" "9" "12" "01C-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-12" "X" "4" "29" "4" "02C-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-121" "N" "4" "33" "48" "03C-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-121" BY REFERENCE N-24 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-13" " " "22" "0" "30" "C-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-13" "X" "22" "7" "1" " " "C-13" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-13" "X" "22" "9" "12" "01C-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-131" " " "22" "0" "9" "02C-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-131" "X" "22" "31" "1" " " "C-131" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-131" "X" "22" "33" "6" "01C-131" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-131" "X" "22" "40" "2" "02C-131" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-13" "X" "22" "73" "8" "C-131" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-14" " " "23" "0" "4" "C-13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-14" "X" "23" "7" "4" " " "C-14" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-15" "X" "21" "73" "9" "C-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-16" "X" "24" "60" "1" "C-15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "122" "CLE-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-21" " " "LIN2" "0" "56" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-21" "X" "LIN2" "2" "7" " " "CLE-21" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-21" "X" "LIN2" "9" "1" "01CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-21" "X" "LIN2" "11" "24" "02CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-21" "X" "LIN2" "35" "24" "03CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-22" " " "LIN2 PLUS 1" "0" "66" "CLE-21" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-22" "X" "LIN2 PLUS 1" "2" "7" " " "CLE-22"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-22" "X" "LIN2 PLUS 1" "11" "1" "01CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-22" "X" "LIN2 PLUS 1" "13" "5" "02CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-22" "X" "LIN2 PLUS 1" "19" "5" "03CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-22" "X" "LIN2 PLUS 1" "25" "5" "04CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLE-22" "X" "LIN2 PLUS 1" "31" "5" "05CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07CLE-22" "X" "LIN2 PLUS 1" "37" "5" "06CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08CLE-22" "X" "LIN2 PLUS 1" "43" "5" "07CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09CLE-22" "X" "LIN2 PLUS 1" "49" "5" "08CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10CLE-22" "X" "LIN2 PLUS 1" "55" "5" "09CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11CLE-22" "X" "LIN2 PLUS 1" "61" "5" "10CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12CLE-22" "X" "LIN2 PLUS 1" "67" "5" "11CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13CLE-22" "X" "LIN2 PLUS 1" "73" "8" "12CLE-22" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "X" "3" "2" "6" "CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-04" " " "0" "0" "15" "CLE-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-41" "X" "3" "24" "8" " " "CLE-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-42" "X" "LIN1 PLUS 1" "2" "7" "C-41" " "
            RETURNING RESU.
      *DSP-REV-AREA
       CALL "SD_Init" USING 
            "DSP-REV-AREA" " " "0" "0" "488" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR-ALL" " " "0" "0" "244" " " "DSP-REV-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR1" "RX" "6" "11" "61" " " "DSP-RR-ALL"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR1" BY REFERENCE REV-1 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR2" "RX" "7" "11" "61" "DSP-RR1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR2" BY REFERENCE REV-2 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR3" "RX" "8" "11" "61" "DSP-RR2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR3" BY REFERENCE REV-3 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RR4" "RX" "9" "11" "61" "DSP-RR3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RR4" BY REFERENCE REV-4 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC-ALL" " " "0" "0" "244" "DSP-RR-ALL" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC1" "X" "6" "11" "61" " " "DSP-RC-ALL"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC1" BY REFERENCE REV-1 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC2" "X" "7" "11" "61" "DSP-RC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC2" BY REFERENCE REV-2 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC3" "X" "8" "11" "61" "DSP-RC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC3" BY REFERENCE REV-3 "61" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-RC4" "X" "9" "11" "61" "DSP-RC3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "DSP-RC4" BY REFERENCE REV-4 "61" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "160" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M02" "X" "24" "1" "28" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-03" "X" "24" "1" "24" "INV-M02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-04" "X" "24" "1" "22" "ERR-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-05" "X" "24" "1" "28" "ERR-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-06" "X" "24" "1" "32" "ERR-05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SET" "X" "24" "1" "26" "ERR-06" " " RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "341" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-01" " " "24" "0" "60" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-01" "X" "24" "1" "60" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE" " " "24" "0" "40" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "24" "1" "40" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-B" "X" "24" "40" "5" " " "DISP-BUZ-B"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-J" "X" "24" "40" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-M01" " " "24" "0" "22" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-D01" " " "24" "0" "22" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-D01" "X" "24" "1" "22" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M01" " " "24" "0" "22" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-M01" "X" "24" "1" "22" " " "INV-M01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D01" " " "24" "0" "22" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-D01" "X" "24" "1" "22" " " "INV-D01"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "OK-01" " " "24" "0" "14" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01OK-01" "X" "24" "1" "14" " " "OK-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "CAN-01" " " "24" "0" "18" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CAN-01" "X" "24" "1" "18" " " "CAN-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-01" " " "24" "0" "18" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-01" "X" "24" "1" "18" " " "ERR-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-02" " " "24" "0" "22" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-02" "X" "24" "1" "22" " " "ERR-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "24" "0" "71" "ERR-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-DIS" "X" "24" "2" "5" " " "ERR-DIS" RETURNING RESU.
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
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
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
           CALL "SD_Screen_Output" USING "SK070I" RETURNING RESU.
           ACCEPT   WYMD       FROM  DATE.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
      *
           MOVE     SPACE      TO    ACT-WORK1  ACT-WORK2  W-AREA2.
           INITIALIZE                ACT-WORK1  ACT-WORK2  W-AREA2.
       INI-EX.
           EXIT.
      ******************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm          *
      *          Å`èIóπèàóùÅ`      *
      ******************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *************************************
      *    Ç`ÇbÇsÅ|ÇqÇsÇm                 *
      *          Å`âÊñ ì¸óÕÅñçXêVèàóùÅ`   *
      *************************************
       ACT-RTN.
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
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p" RETURNING RESU.
           IF  W-ACT  NOT =  1  AND  2  AND  3
               GO  TO  ACT-RTN
           END-IF
           IF  W-ACT      =  1
               MOVE    "éÊè¡Å@Å@Å@"  TO  W-NAM
           END-IF
           IF  W-ACT      =  2
               MOVE    "èoâ◊ì˙í˘ê≥"  TO  W-NAM
           END-IF
           IF  W-ACT      =  3
               MOVE    "ñæç◊í˘ê≥Å@"  TO  W-NAM
           END-IF
           CALL "SD_Output" USING "DSP-NAM" DSP-NAM "p" RETURNING RESU.
       ACT-005.
           MOVE     1          TO    A.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           PERFORM  CR1-RTN    THRU  CR1-EX.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           INITIALIZE          ACT-WORK2.
       ACT-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-1 "ACP-1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-RTN
           END-IF
           IF  ESTAT      =  "02"
               PERFORM   NXT-RTN     THRU   NXT-EX
               GO  TO  ACT-011
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-010
           END-IF
           MOVE     0          TO    W-NEXT.
       ACT-011.
           IF  W-NEXT     =   9
               GO  TO  ACT-010
           END-IF
           CALL "SD_Output" USING "ACP-1" ACP-1 "p" RETURNING RESU.
           MOVE     0          TO    DIS-SW.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE     W-1        TO    JSTR-01.
           MOVE     1          TO    JSTR-02  A  CNT.
      *           START    JSTR KEY   NOT <  JSTR-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSTR_IDLST JSTR_PNAME1
               CALL "DB_F_Open" USING
                "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
                "JSTR-KEY" BY REFERENCE JSTR-KEY
               GO  TO   ACT-014
           END-IF.
       ACT-012.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  ACT-014
           END-IF
           MOVE     JSTR-02    TO    A.
           IF  W-1    NOT =  JSTR-01
               GO  TO  ACT-014
           END-IF
           IF  DIS-SW NOT =  0
               GO  TO  ACT-013
           END-IF
           PERFORM  CR1-RTN    THRU  CR1-EX.
           MOVE     JSTR-14A   TO    W-5A.
           MOVE     JSTR-03    TO    W-21.
           MOVE     JSTR-05    TO    W-3.
           IF  W-21       =  0
               MOVE       "èoâ◊"   TO    W-22
           END-IF
           IF  W-21       =  3
               MOVE       "í˘ê≥"   TO    W-22
           END-IF
           IF  W-21       =  7
               MOVE       "ÉTèo"   TO    W-22
           END-IF
           MOVE     JSTR-061   TO    W-41  TC-TCD.
           MOVE     1          TO    TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    W-43.
           MOVE     JSTR-062   TO    W-42  TC-CCD.
      *           READ     TC-M       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    TC-NAME
           END-IF
           MOVE     TC-NAME    TO    W-44.
           MOVE     3          TO    JCON3-01.
           MOVE     JSTR-07    TO    W-51  JCON3-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    JCON3-03
           END-IF
           MOVE     JCON3-03   TO    W-52.
           MOVE     JSTR-15A   TO    W-KOS.
           CALL "SD_Output" USING "ACP-2" ACP-2 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-3" ACP-3 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-41" A-41 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-42" A-42 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-43" D-43 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-44" D-44 "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-5" ACP-5 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-KOS" DSP-KOS "p" RETURNING RESU.
           MOVE  1     TO  DIS-SW.
       ACT-013.
           IF  JSTR-158  =  0     OR   JSTR-17    =   1
               CALL "SD_Output" USING
                "ERR-01" ERR-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           IF  JSTR-03  NOT =  0  AND  7
               CALL "SD_Output" USING
                "ERR-04" ERR-04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  ACT-010
           END-IF
           MOVE  JSTR-121(1)  TO  W-64(A 1).
           MOVE  JSTR-121(2)  TO  W-64(A 2).
           MOVE  JSTR-121(3)  TO  W-64(A 3).
           MOVE  JSTR-121(4)  TO  W-64(A 4).
           MOVE  JSTR-121(5)  TO  W-64(A 5).
           MOVE  JSTR-121(6)  TO  W-64(A 6).
           MOVE  JSTR-121(7)  TO  W-64(A 7).
           MOVE  JSTR-121(8)  TO  W-64(A 8).
           MOVE  JSTR-121(9)  TO  W-64(A 9).
           MOVE  JSTR-121(10) TO  W-64(A 10).
           MOVE  JSTR-122     TO  W-65(A).
           MOVE     JSTR-08    TO    W-61(A).
           MOVE     JSTR-09    TO    W-621(A)  HI-MHCD HI-HCD.
           MOVE     JSTR-10    TO    W-63(A).
      *           READ     HI2-M      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE      TO    HI-NAME
           END-IF
           MOVE     HI-NAME    TO    W-622(A).
       ACT-013A.
           IF  CNT NOT = A
               ADD   1   TO  CNT
               ADD   2   TO  LIN1
               CALL "SD_Arg_Match_Line" USING
                "LIN1" "2" LIN1 RETURNING RESU
               GO  TO  ACT-013A
           END-IF
           IF  W-61(A)        =  ZERO
               CALL "SD_Output" USING "A-612A" A-612A "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-611" A-611 "p" RETURNING RESU
               CALL "SD_Output" USING "A-612" A-612 "p" RETURNING RESU
               CALL "SD_Output" USING "A-613" A-613 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-6B" DSP-6B "p" RETURNING RESU.
           MOVE     A          TO    O.
           ADD      1          TO    CNT.
           ADD      2          TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           GO  TO   ACT-012.
       ACT-014.
           IF  CNT        =  1
               CALL "SD_Output" USING
                "ERR-03" ERR-03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-010
           END-IF
           COMPUTE  W-KEI =  W-65(1) + W-65(2) + W-65(3) + W-65(4) +
                             W-65(5) + W-65(6).
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
           IF  W-ACT      =  1
               PERFORM  SAV-RTN  THRU  SAV-EX
               GO  TO  ACT-150
           END-IF
           IF  W-ACT      =  2
               GO  TO  ACT-030
           END-IF
           IF  W-ACT      =  3
               GO  TO  ACT-055
           END-IF.
       ACT-030.
           CALL "SD_Accept" USING BY REFERENCE A-31 "A-31" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-030
           END-IF
           CALL "SD_Output" USING "A-31" A-31 "p" RETURNING RESU.
       ACT-031.
           CALL "SD_Accept" USING BY REFERENCE A-32 "A-32" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-030
           END-IF
           IF  ESTAT  NOT =  "00" AND "01" AND "06"
               GO  TO  ACT-031
           END-IF
           CALL "SD_Output" USING "A-32" A-32 "p" RETURNING RESU.
           IF  (W-32  <  1)  OR  (W-32  >  12)
               GO  TO  ACT-031
           END-IF.
       ACT-032.
           CALL "SD_Accept" USING BY REFERENCE A-33 "A-33" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-031
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-032
           END-IF
           CALL "SD_Output" USING "A-33" A-33 "p" RETURNING RESU.
           IF  (W-33  <  1)  OR  (W-33  >  31)
               GO  TO  ACT-032
           END-IF
           IF  W-312      <  95
               MOVE  20     TO  W-311
           ELSE
               MOVE  19     TO  W-311
           END-IF
           GO  TO  ACT-150.
       ACT-055.
           CALL "SD_Accept" USING BY REFERENCE A-53 "A-53" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-055
           END-IF
           CALL "SD_Output" USING "D-54" D-54 "p" RETURNING RESU.
       ACT-060.
           MOVE     10         TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           MOVE     1          TO    A  B  C.
       ACT-061.
           MOVE     W-621(A)   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE       HI-R
           END-IF
           MOVE     HI-ISU     TO    W-KA(A).
           IF  W-61(A)      NOT =    ZERO
               GO  TO  ACT-086
           END-IF.
       ACT-080.
           MOVE     W-621(A)   TO    O-HIN.
           CALL "SD_Accept" USING BY REFERENCE A-621 "A-621" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  A           =     1
                   GO  TO  ACT-055
               ELSE
                   SUBTRACT   1       FROM  A
                   SUBTRACT   2       FROM  LIN1
                   CALL "SD_Arg_Match_Line" USING
                    "LIN1" "2" LIN1 RETURNING RESU
                   GO  TO  ACT-080
               END-IF
           END-IF
           IF  ESTAT  NOT = "04"
               GO  TO  ACT-081
           END-IF
           IF  W-63(A)  NOT =  1  AND 2  AND  3  AND  4
               IF  O-HIN   NOT  =  ZERO
                   MOVE  O-HIN   TO  W-621(A)
                   CALL "SD_Output" USING
                    "A-621" A-621 "p" RETURNING RESU
                   GO  TO  ACT-080
               END-IF
           END-IF
           MOVE  O-HIN        TO  W-621(A).
           IF  O-HIN     NOT =    ZERO
               CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU
           ELSE
               GO  TO  ACT-081A
           END-IF
           IF  A             =    6
               GO  TO  ACT-110
           ELSE
               ADD   2     TO   LIN1
               CALL "SD_Arg_Match_Line" USING
                "LIN1" "2" LIN1 RETURNING RESU
               ADD   1     TO   A
               GO  TO  ACT-080
           END-IF.
       ACT-081.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-080
           END-IF
           CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU.
           IF  W-621(A) NOT =    ZERO
               GO  TO  ACT-082
           END-IF.
       ACT-081A.
           IF  A           <=   O
               CALL "SD_Output" USING "ERR-01" ERR-01 "p" RETURNING RESU
               MOVE  O-HIN   TO  W-621(A)
               CALL "SD_Output" USING "A-621" A-621 "p" RETURNING RESU
               GO  TO  ACT-080
           END-IF
           MOVE     A          TO    AW.
           MOVE     LIN2       TO    LIN2W.
           IF  AW           <   7
               MOVE   6        TO    A
               MOVE  20        TO    LIN2
               CALL "SD_Arg_Match_Line" USING
                "LIN2" "2" LIN2 RETURNING RESU
               INITIALIZE   W-64A(A)
               MOVE   ZERO     TO    W-621(A)  W-65(A)
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
           END-IF
           IF  AW           <   6
               MOVE   5        TO    A
               MOVE  18        TO    LIN2
               CALL "SD_Arg_Match_Line" USING
                "LIN2" "2" LIN2 RETURNING RESU
               INITIALIZE   W-64A(A)
               MOVE   ZERO     TO    W-621(A)  W-65(A)
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
           END-IF
           IF  AW           <   5
               MOVE   4        TO    A
               MOVE  16        TO    LIN2
               CALL "SD_Arg_Match_Line" USING
                "LIN2" "2" LIN2 RETURNING RESU
               INITIALIZE   W-64A(A)
               MOVE   ZERO     TO    W-621(A)  W-65(A)
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
           END-IF
           IF  AW           <   4
               MOVE   3        TO    A
               MOVE  14        TO    LIN2
               CALL "SD_Arg_Match_Line" USING
                "LIN2" "2" LIN2 RETURNING RESU
               INITIALIZE   W-64A(A)
               MOVE   ZERO     TO    W-621(A)  W-65(A)
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
           END-IF
           IF  AW           <   3
               MOVE   2        TO    A
               MOVE  12        TO    LIN2
               CALL "SD_Arg_Match_Line" USING
                "LIN2" "2" LIN2 RETURNING RESU
               INITIALIZE   W-64A(A)
               MOVE   ZERO     TO    W-621(A)  W-65(A)
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
           END-IF
           MOVE     AW         TO    A.
           MOVE     LIN2W      TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
           GO  TO  ACT-120.
       ACT-082.
           MOVE     W-621(A)   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M02" INV-M02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-080
           END-IF
           MOVE     HI-ISU     TO    W-KA(A).
           MOVE     HI-NAME    TO    W-622(A).
           CALL "SD_Output" USING "D-622" D-622 "p" RETURNING RESU.
       ACT-085.
           CALL "SD_Accept" USING BY REFERENCE A-63 "A-63" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT
               GO  TO  ACT-080
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  ACT-085
           END-IF
           CALL "SD_Output" USING "A-63" A-63 "p" RETURNING RESU.
           IF  W-63(A)  NOT  =  1  AND  2  AND  3  AND  4
               PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT
               GO  TO  ACT-085
           END-IF.
       ACT-086.
           PERFORM  REV-DSP-RTN  THRU  REV-DSP-EXT.
           IF  W-63(A)       =  1
               IF  HI-SS1       =    ZERO
                   CALL "SD_Output" USING
                    "ERR-06" ERR-06 "p" RETURNING RESU
                   PERFORM   REV-CLE-RTN  THRU   REV-CLE-EXT
                   GO  TO  ACT-085
               END-IF
           END-IF
           IF  W-63(A)       =  2
               IF  HI-SS2       =    ZERO
                   CALL "SD_Output" USING
                    "ERR-06" ERR-06 "p" RETURNING RESU
                   PERFORM   REV-CLE-RTN  THRU   REV-CLE-EXT
                   GO  TO  ACT-085
               END-IF
           END-IF
           IF  W-63(A)       =  3
               IF  HI-SS3       =    ZERO
                   CALL "SD_Output" USING
                    "ERR-06" ERR-06 "p" RETURNING RESU
                   PERFORM   REV-CLE-RTN  THRU   REV-CLE-EXT
                   GO  TO  ACT-085
               END-IF
           END-IF
           IF  W-63(A)       =  4
               IF  HI-SS4       =    ZERO
                   CALL "SD_Output" USING
                    "ERR-06" ERR-06 "p" RETURNING RESU
                   PERFORM   REV-CLE-RTN  THRU   REV-CLE-EXT
                   GO  TO  ACT-085
               END-IF
           END-IF.
       ACT-090.
           MOVE     1          TO    B.
           MOVE     13         TO    COL1.
           CALL "SD_Arg_Match_Col" USING "COL1" "2" COL1 RETURNING RESU.
       ACT-091.
           IF  W-63(A)    =  1
               IF  HI-S1(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  2
               IF  HI-S2(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  3
               IF  HI-S3(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  W-63(A)    =  4
               IF  HI-S4(B)     =    0
                   MOVE     ZERO       TO    W-64(A , B)
                   CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
                   GO  TO   ACT-092
               END-IF
           END-IF
           IF  (W-63(A)    =  4)  AND  (B   =   10)
               MOVE     ZERO       TO    W-64(A , B)
               CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU
               GO  TO   ACT-092
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-64 "A-64" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  (ESTAT  =  "09")  AND  (B  =  1)
               IF  W-611(A)  =  0
                   GO  TO  ACT-085
               ELSE
                   IF  A     =    1
                       GO  TO  ACT-055
                   ELSE
                       SUBTRACT   1       FROM  A
                       SUBTRACT   2       FROM  LIN1
                       CALL "SD_Arg_Match_Line" USING
                        "LIN1" "2" LIN1 RETURNING RESU
                       GO  TO  ACT-090
                   END-IF
               END-IF
           END-IF
           IF  ESTAT      =  "09"
               GO  TO  ACT-093
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-091
           END-IF
           CALL "SD_Output" USING "D-64" D-64 "p" RETURNING RESU.
           IF  B          >  7
               IF  W-64(A,B)  >  999  OR  < -999
                   GO  TO  ACT-091
               END-IF
           END-IF.
       ACT-092.
           IF  B      NOT =  10
               ADD      1          TO    B
               ADD      6          TO    COL1
               CALL "SD_Arg_Match_Col" USING
                "COL1" "2" COL1 RETURNING RESU
               GO  TO   ACT-091
           END-IF
           GO  TO   ACT-100.
       ACT-093.
           SUBTRACT 1          FROM  B.
           SUBTRACT 6          FROM  COL1.
           CALL "SD_Arg_Match_Col" USING "COL1" "2" COL1 RETURNING RESU.
           IF  B          =  ZERO
               IF  W-61(A)    =  ZERO
                   GO  TO  ACT-085
               ELSE
                   GO  TO   ACT-061
               END-IF
           END-IF
           IF  W-63(A)    =  1
               IF  HI-S1(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  2
               IF  HI-S2(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  3
               IF  HI-S3(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           IF  W-63(A)    =  4
               IF  HI-S4(B)     =    0
                   GO  TO  ACT-093
               END-IF
           END-IF
           GO  TO   ACT-091.
       ACT-100.
           COMPUTE  W-65(A) =  W-64(A , 1) + W-64(A , 2) + W-64(A , 3)
               + W-64(A , 4) + W-64(A , 5) + W-64(A , 6) + W-64(A , 7)
               + W-64(A , 8) + W-64(A , 9) + W-64(A , 10).
           CALL "SD_Output" USING "D-65" D-65 "p" RETURNING RESU.
           IF  W-5A      =   ZERO   OR  1
               GO  TO  ACT-110
           END-IF
           DIVIDE   W-5A      INTO  W-65(A)
                    GIVING  WA      REMAINDER  WB.
           IF  WB    NOT =   ZERO
               CALL "SD_Output" USING
                "ERR-SET" ERR-SET "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ACT-110.
           PERFORM  REV-CLE-RTN  THRU  REV-CLE-EXT.
           ADD      1          TO    A.
           ADD      2          TO    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           IF  A          >  6
               GO  TO  ACT-120
           END-IF
           GO  TO  ACT-061.
       ACT-120.
           COMPUTE  W-KEI   =  W-65(1) + W-65(2) + W-65(3) +
                               W-65(4) + W-65(5) + W-65(6).
           CALL "SD_Output" USING "DSP-KEI" DSP-KEI "p" RETURNING RESU.
       ACT-130.
           CALL "SD_Accept" USING BY REFERENCE ACP-KOS "ACP-KOS" "S9"
            "3" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  NOT =  "09"
               GO  TO  ACT-140
           END-IF
           SUBTRACT   2       FROM    LIN1.
           CALL "SD_Arg_Match_Line" USING
            "LIN1" "2" LIN1 RETURNING RESU.
           SUBTRACT   1       FROM    A.
           IF  W-621(A)   =   ZERO
               GO  TO  ACT-080
           END-IF
           MOVE     W-621(A)   TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO   ACT-080
           END-IF
           GO  TO  ACT-090.
       ACT-140.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-130
           END-IF
           CALL "SD_Output" USING "DSP-KOS" DSP-KOS "p" RETURNING RESU.
       ACT-150.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  (ESTAT  =  "09")  AND  (W-ACT  =  1)
               GO  TO  ACT-010
           END-IF
           IF  (ESTAT  =  "09")  AND  (W-ACT  =  2)
               GO  TO  ACT-030
           END-IF
           IF  (ESTAT  =  "09")  AND  (W-ACT  =  3)
               GO  TO  ACT-130
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-150
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  W-OKC  NOT =  1 AND 9
               GO  TO  ACT-150
           END-IF
           IF  W-OKC      =  9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               GO  TO   ACT-160
           END-IF.
       ACT-155.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
       ACT-160.
           GO  TO   ACT-005.
       ACT-EX.
           EXIT.
      *************************************
      *    ÇtÇoÇcÅ|ÇqÇsÇm                 *
      *          Å`ÉtÉ@ÉCÉãçXêVèàóùÅ`     *
      *************************************
       UPD-RTN.
      *****èoâ◊éwê}ÉgÉâÉìÅ@çXêV
           MOVE     SPACE      TO    ENDFLG.
           MOVE     0          TO    C  B.
           MOVE     W-1        TO    JSTR-01.
           MOVE     1          TO    JSTR-02.
      *           START    JSTR  KEY   NOT <  JSTR-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO   UPD-EX
           END-IF.
       UPD-015.
      *           READ     JSTR       NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "END"       TO  ENDFLG
               GO  TO   UPD-025
           END-IF
           IF  JSTR-01     NOT =   W-1
               MOVE  "END"       TO  ENDFLG
               GO  TO   UPD-025
           END-IF
           IF  JSTR-02  =  1
               MOVE   JSTR-R     TO  SV-JSTR
           END-IF
           IF  W-ACT      =   1
               GO  TO  UPD-030
           END-IF
           IF  W-ACT      =   2
               GO  TO  UPD-040
           END-IF.
       UPD-025.
           IF  ENDFLG     =   "END"
               IF  W-ACT      =    1  OR  2
                   GO  TO  UPD-EX
               END-IF
           END-IF
           ADD      1          TO    C  B.
           IF  B   >      6
               GO  TO  UPD-EX
           END-IF
           IF  W-621(B)   =    ZERO
               GO  TO  UPD-EX
           END-IF
           IF  ENDFLG     =    "END"
               MOVE       SV-JSTR       TO  JSTR-R
               INITIALIZE       JSTR-11
           END-IF
           MOVE     B          TO    JSTR-02.
           MOVE     W-21       TO    JSTR-03.
           MOVE     W-61(B)    TO    JSTR-08.
           MOVE     W-621(B)   TO    JSTR-09.
           MOVE     W-63(B)   TO    JSTR-10.
           MOVE     W-64(B 1) TO    JSTR-121(1).
           MOVE     W-64(B 2) TO    JSTR-121(2).
           MOVE     W-64(B 3) TO    JSTR-121(3).
           MOVE     W-64(B 4) TO    JSTR-121(4).
           MOVE     W-64(B 5) TO    JSTR-121(5).
           MOVE     W-64(B 6) TO    JSTR-121(6).
           MOVE     W-64(B 7) TO    JSTR-121(7).
           MOVE     W-64(B 8) TO    JSTR-121(8).
           MOVE     W-64(B 9) TO    JSTR-121(9).
           MOVE     W-64(B 10) TO   JSTR-121(10).
           MOVE     W-65(B)    TO    JSTR-122.
           MOVE     W-KOS      TO    JSTR-15A.
           MOVE     8          TO    JSTR-17.
           IF  ENDFLG   =  "END"
               PERFORM  WRI-RTN  THRU  WRI-EX
               GO  TO     UPD-025
           ELSE
               PERFORM  REW-RTN  THRU  REW-EX
           END-IF
           GO  TO   UPD-015.
       UPD-030.
           INITIALIZE                JSTR-12.
           MOVE     8          TO    JSTR-17.
           PERFORM  REW-RTN  THRU    REW-EX.
           GO  TO  UPD-015.
       UPD-040.
           MOVE     W-3        TO    JSTR-05.
           MOVE     8          TO    JSTR-17.
           PERFORM  REW-RTN  THRU    REW-EX.
           GO  TO  UPD-015.
       UPD-EX.
           EXIT.
      *----ÇvÇqÇhÅ|ÇqÇsÇm--------------*
       WRI-RTN.
           MOVE     JSTR-KEY    TO  ERR-K.
      *           WRITE   JSTR-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"      TO  ERR-F
               MOVE     "W"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *----ÇqÇdÇvÅ|ÇqÇsÇm--------------*
       REW-RTN.
           MOVE     JSTR-KEY    TO  ERR-K.
      *           REWRITE JSTR-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"      TO  ERR-F
               MOVE     "R"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       REW-EX.
           EXIT.
      *----ÇbÇqÇPÅ|ÇqÇsÇm              *
      *        Å`ÉNÉäÉAÅ@ÉãÅ[É`ÉìÅ`----*
       CR1-RTN.
           MOVE     LIN1       TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
       CR1-010.
           PERFORM  CR2-RTN    THRU  CR2-EX  VARYING  C  FROM  A
                    BY  1  UNTIL  C  >   6.
       CR1-EX.
           EXIT.
      *----ÇbÇqÇQÅ|ÇqÇsÇm              *
      *      Å`ÉèÅ[ÉNÅEâÊñ ÉNÉäÉAÅ`----*
       CR2-RTN.
           INITIALIZE                W-6A(C).
           CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU.
           ADD      2          TO    LIN2.
           CALL "SD_Arg_Match_Line" USING
            "LIN2" "2" LIN2 RETURNING RESU.
       CR2-EX.
           EXIT.
       REV-DSP-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
           IF  W-63(A)  =  1
               CALL "SD_Output" USING
                "DSP-RR1" DSP-RR1 "p" RETURNING RESU
           END-IF
           IF  W-63(A)  =  2
               CALL "SD_Output" USING
                "DSP-RR2" DSP-RR2 "p" RETURNING RESU
           END-IF
           IF  W-63(A)  =  3
               CALL "SD_Output" USING
                "DSP-RR3" DSP-RR3 "p" RETURNING RESU
           END-IF
           IF  W-63(A)  =  4
               CALL "SD_Output" USING
                "DSP-RR4" DSP-RR4 "p" RETURNING RESU
           END-IF.
       REV-DSP-EXT.
           EXIT.
       REV-CLE-RTN.
           CALL "SD_Output" USING
            "DSP-RC-ALL" DSP-RC-ALL "p" RETURNING RESU.
       REV-CLE-EXT.
           EXIT.
      ***********************************************
      *    ÉZÅ[ÉuÉèÅ[ÉNÉZÉbÉgÅ@ÉãÅ[É`Éì             *
      ***********************************************
       SAV-RTN.
           MOVE    SPACE           TO  SAVE-WORK.
           INITIALIZE                  SAVE-WORK.
           MOVE    1               TO  CNT  A.
      *
           MOVE    W-1             TO  JSTR-01.
           MOVE    1               TO  JSTR-02.
      *           START   JSTR  KEY  NOT <    JSTR-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  SAV-EX
           END-IF.
       SAV-010.
      *           READ    JSTR  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  SAV-EX
           END-IF
           IF  JSTR-01   NOT =  W-1
               GO  TO  SAV-EX
           END-IF
           IF  CNT   NOT =     1
               GO  TO  SAV-020
           END-IF.
       SAV-015.
           MOVE    JSTR-01      TO    SV-1.
           MOVE    JSTR-06      TO    SV-4.
           MOVE    JSTR-05      TO    SV-3.
           MOVE    JSTR-14B     TO    SV-14B.
           MOVE    JSTR-15A     TO    SV-10.
           MOVE    JSTR-14A     TO    SV-5A.
       SAV-020.
           MOVE    JSTR-09      TO    SV-621(A).
           MOVE    JSTR-10      TO    SV-63(A).
           MOVE    JSTR-121(1)  TO    SV-64(A , 1).
           MOVE    JSTR-121(2)  TO    SV-64(A , 2).
           MOVE    JSTR-121(3)  TO    SV-64(A , 3).
           MOVE    JSTR-121(4)  TO    SV-64(A , 4).
           MOVE    JSTR-121(5)  TO    SV-64(A , 5).
           MOVE    JSTR-121(6)  TO    SV-64(A , 6).
           MOVE    JSTR-121(7)  TO    SV-64(A , 7).
           MOVE    JSTR-121(8)  TO    SV-64(A , 8).
           MOVE    JSTR-121(9)  TO    SV-64(A , 9).
           MOVE    JSTR-121(10) TO    SV-64(A , 10).
           MOVE    JSTR-122     TO    SV-65(A).
           ADD     JSTR-122     TO    SV-KEI.
           ADD     1            TO    CNT  A.
           GO  TO  SAV-010.
       SAV-EX.
           EXIT.
       NXT-RTN.
           MOVE     0          TO    W-NEXT.
       NXT-010.
      *           READ     JSTR       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  NXT-020
           END-IF
           IF  JSTR-158  =  0     OR  JSTR-17    =   1
               GO  TO  NXT-010
           END-IF
           IF  JSTR-03  NOT =  0  AND  7
               GO  TO  NXT-010
           END-IF
           MOVE     JSTR-01    TO    W-1.
           GO  TO  NXT-EX.
       NXT-020.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           IF  W-NEXT          =    0
               MOVE   1             TO   W-NEXT
               GO  TO  NXT-010
           END-IF
           MOVE     9          TO     W-NEXT.
       NXT-EX.
           EXIT.
      ***
           COPY    LPMSG.
