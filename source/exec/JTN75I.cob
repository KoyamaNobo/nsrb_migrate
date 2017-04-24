       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JTN75I.
       AUTHOR.               --------.
      ******************************************************
      *    PROGRAM      :    ëqï ç›å…É}ÉXÉ^Å@ç›å…í≤êÆì¸óÕ  *
      *    DATA WRITTEN :    94/10/12                      *
      *    SCREEN  USED :    SJN75I                        *
      *    COMPILE TYPE :    COBOL                         *
      ******************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT        PIC  X(02).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(018) VALUE
                "ÅñÅñÅñÅ@Å@ç›å…í≤êÆÅ@ÉäÉXÉgÅ@Å@ÅñÅñÅñ".
           02  F              PIC  X(031) VALUE SPACE.
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "ëqå…".
           02  F              PIC  X(007) VALUE " ∫∞ƒﬁ  ".
           02  F              PIC  N(008) VALUE "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÇRçÜ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÇQçÜ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÇPçÜ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ÇOçÜ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@íÜ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "Å@ëÂ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "ì¡ëÂ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(047) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(047) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(010) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(047) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "    ".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "çáÅ@Å@åv".
       01  W-P.
           02  F              PIC  X(001).
           02  P-SOC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC  ---,---  OCCURS  10 TIMES.
           02  P-SUT          PIC --,---,--9.
       01  W-AREA.
           02  W-KEY.
               03  W-SOC   PIC  9(01).
               03  W-HCD   PIC  9(06).
               03  W-SC    PIC  9(01).
           02  W-OSUD.
               03  W-OSU   PIC S9(06)   OCCURS  10.
           02  W-NSUD.
               03  W-NSU   PIC S9(06)   OCCURS  10.
           02  W-SUD.
               03  W-SU    PIC S9(06)   OCCURS  10.
           02  W-SSUD.
               03  W-SSU   PIC S9(06)   OCCURS  10.
           02  W-OKC       PIC  9(01).
           02  W-C         PIC  9(02).
           02  WRI-SW      PIC  9(01).
           02  A           PIC  9(02).
           02  W-SD.
               03  W-S     PIC  9(01)   OCCURS  10.
           02  W-PAGE      PIC  9(03).
           02  CHK.
               03  CHK1    PIC  9(01).
               03  CHK2    PIC  9(01).
           02  W-SUT       PIC S9(07).
       01  W-ASIZ.
           02  W-SIZ1  PIC  X(40)   VALUE
               "ÇRçÜÇQçÜÇPçÜÇOçÜÅ@íÜÅ@ëÂì¡ëÂ28.029.030.0".
           02  W-SIZ2  PIC  X(40)   VALUE
               "12.513.013.514.015.016.017.018.019.020.0".
           02  W-SIZ3  PIC  X(40)   VALUE
               "21.021.522.022.523.023.524.024.525.0    ".
           02  W-SIZ4  PIC  X(40)   VALUE
               "24.024.525.025.526.026.527.027.5        ".
       01  W-SIZD.
           02  W-SIZ   PIC  X(04)   OCCURS  10.
      *
           COPY     LWMSG.
      *
           COPY     LIHIM2.
           COPY     L-JCON.
           COPY     LNJZAI.
      *FD  SP-F
       77  SP-R             PIC  X(206).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER      PIC  X(12) VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-1.
               03  A-SOC      PIC  9(01).
               03  D-SON      PIC  N(06).
               03  A-HCD      PIC  9(06).
               03  D-HNA      PIC  N(24).
           02  ACP-2.
               03  A-SC       PIC  9(01).
           02  ACP-3.
               03  D-SIZ01    PIC  X(04).
               03  D-SIZ02    PIC  X(04).
               03  D-SIZ03    PIC  X(04).
               03  D-SIZ04    PIC  X(04).
               03  D-SIZ05    PIC  X(04).
               03  D-SIZ06    PIC  X(04).
               03  D-SIZ07    PIC  X(04).
               03  D-SIZ08    PIC  X(04).
               03  D-SIZ09    PIC  X(04).
               03  D-SIZ10    PIC  X(04).
           02  ACP-4.
               03  D-OSU01    PIC ------- .
               03  D-OSU02    PIC ------- .
               03  D-OSU03    PIC ------- .
               03  D-OSU04    PIC ------- .
               03  D-OSU05    PIC ------- .
               03  D-OSU06    PIC ------- .
               03  D-OSU07    PIC ------- .
               03  D-OSU08    PIC ------- .
               03  D-OSU09    PIC ------- .
               03  D-OSU10    PIC ------- .
           02  ACP-5.
               03  A-NSU      PIC S9(06).
               03  D-NSU      PIC ------- .
           02  ACP-OKC     PIC  9(01).
       01  DSP-CLE.
           02  CLE-01.
               03  C-1.
                   04  FILLER  PIC  X(14) VALUE "              ".
                   04  FILLER  PIC  X(06) VALUE "      ".
                   04  FILLER  PIC  X(48) VALUE
                   "                                                ".
               03  C-2.
                   04  FILLER  PIC  X(01) VALUE " ".
               03  C-3.
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
                   04  FILLER  PIC  X(04) VALUE "    ".
               03  C-4.
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
               03  C-5.
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
                   04  FILLER  PIC  X(08) VALUE "        ".
       01  DSP-ERR.
           02  INV-M02     PIC  X(28)
                           VALUE "ÅñÅñïiñºÉ}ÉXÉ^Å[Å@ñ¢ìoò^ÅñÅñ".
           02  INV-M03     PIC  X(20)
                           VALUE "ÅñÅñÉTÉCÉYÅ@Ç»ÇµÅñÅñ".
           02  INV-D02     PIC  X(22)
                           VALUE "ÅñÅñëqå…ñºÅ@ñ¢ìoò^ÅñÅñ".
           COPY     LSMSG.
      **
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "192" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-1" " " "5" "0" "67" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SOC" "9" "5" "2" "1" " " "ACP-1" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SOC" BY REFERENCE W-SOC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SON" "N" "5" "4" "12" "A-SOC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SON" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "5" "17" "6" "D-SON" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "5" "24" "48" "A-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-2" " " "8" "0" "1" "ACP-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "8" "13" "1" " " "ACP-2" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-3" " " "11" "0" "40" "ACP-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ01" "X" "11" "4" "4" " " "ACP-3" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ01" BY REFERENCE W-SIZ(1) "4" "1" "01" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ02" "X" "11" "12" "4" "D-SIZ01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ02" BY REFERENCE W-SIZ(1) "4" "1" "02" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ03" "X" "11" "20" "4" "D-SIZ02" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ03" BY REFERENCE W-SIZ(1) "4" "1" "03" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ04" "X" "11" "28" "4" "D-SIZ03" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ04" BY REFERENCE W-SIZ(1) "4" "1" "04" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ05" "X" "11" "36" "4" "D-SIZ04" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ05" BY REFERENCE W-SIZ(1) "4" "1" "05" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ06" "X" "11" "44" "4" "D-SIZ05" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ06" BY REFERENCE W-SIZ(1) "4" "1" "06" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ07" "X" "11" "52" "4" "D-SIZ06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ07" BY REFERENCE W-SIZ(1) "4" "1" "07" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ08" "X" "11" "60" "4" "D-SIZ07" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ08" BY REFERENCE W-SIZ(1) "4" "1" "08" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ09" "X" "11" "68" "4" "D-SIZ08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ09" BY REFERENCE W-SIZ(1) "4" "1" "09" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZ10" "X" "11" "76" "4" "D-SIZ09" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZ10" BY REFERENCE W-SIZ(1) "4" "1" "10" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-4" " " "13" "0" "70" "ACP-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU01" "-------" "13" "2" "7" " " "ACP-4" RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU01" BY REFERENCE W-OSU(1) "6" "1" "01" 4
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU02" "-------" "13" "10" "7" "D-OSU01" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU02" BY REFERENCE W-OSU(1) "6" "1" "02" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU03" "-------" "13" "18" "7" "D-OSU02" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU03" BY REFERENCE W-OSU(1) "6" "1" "03" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU04" "-------" "13" "26" "7" "D-OSU03" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU04" BY REFERENCE W-OSU(1) "6" "1" "04" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU05" "-------" "13" "34" "7" "D-OSU04" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU05" BY REFERENCE W-OSU(1) "6" "1" "05" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU06" "-------" "13" "42" "7" "D-OSU05" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU06" BY REFERENCE W-OSU(1) "6" "1" "06" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU07" "-------" "13" "50" "7" "D-OSU06" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU07" BY REFERENCE W-OSU(1) "6" "1" "07" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU08" "-------" "13" "58" "7" "D-OSU07" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU08" BY REFERENCE W-OSU(1) "6" "1" "08" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU09" "-------" "13" "66" "7" "D-OSU08" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU09" BY REFERENCE W-OSU(1) "6" "1" "09" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-OSU10" "-------" "13" "74" "7" "D-OSU09" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-OSU10" BY REFERENCE W-OSU(1) "6" "1" "10" 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-5" " " "15" "0" "13" "ACP-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NSU" "S9" "15" "W-C" "6" " " "ACP-5" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NSU" BY REFERENCE W-NSU(1) "6" "1" BY REFERENCE A 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NSU" "-------" "15" "W-C" "7" "A-NSU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NSU" BY REFERENCE W-NSU(1) "6" "1" BY REFERENCE A 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "ACP-5" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-CLE
       CALL "SD_Init" USING 
            "DSP-CLE" " " "0" "0" "269" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" " " "0" "0" "269" " " "DSP-CLE" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-1" " " "5" "0" "68" " " "CLE-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-1" "X" "5" "2" "14" " " "C-1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-1" "X" "5" "17" "6" "01C-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-1" "X" "5" "24" "48" "02C-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-2" " " "8" "0" "1" "C-1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-2" "X" "8" "13" "1" " " "C-2" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-3" " " "11" "0" "40" "C-2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-3" "X" "11" "4" "4" " " "C-3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-3" "X" "11" "12" "4" "01C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-3" "X" "11" "20" "4" "02C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-3" "X" "11" "28" "4" "03C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-3" "X" "11" "36" "4" "04C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-3" "X" "11" "44" "4" "05C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-3" "X" "11" "52" "4" "06C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-3" "X" "11" "60" "4" "07C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-3" "X" "11" "68" "4" "08C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-3" "X" "11" "76" "4" "09C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-4" " " "13" "0" "80" "C-3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-4" "X" "13" "2" "8" " " "C-4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-4" "X" "13" "10" "8" "01C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-4" "X" "13" "18" "8" "02C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-4" "X" "13" "26" "8" "03C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-4" "X" "13" "34" "8" "04C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-4" "X" "13" "42" "8" "05C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-4" "X" "13" "50" "8" "06C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-4" "X" "13" "58" "8" "07C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-4" "X" "13" "66" "8" "08C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-4" "X" "13" "74" "8" "09C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-5" " " "15" "0" "80" "C-4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-5" "X" "15" "2" "8" " " "C-5" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-5" "X" "15" "10" "8" "01C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-5" "X" "15" "18" "8" "02C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-5" "X" "15" "26" "8" "03C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-5" "X" "15" "34" "8" "04C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-5" "X" "15" "42" "8" "05C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-5" "X" "15" "50" "8" "06C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-5" "X" "15" "58" "8" "07C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-5" "X" "15" "66" "8" "08C-5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-5" "X" "15" "74" "8" "09C-5" " " RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "70" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M02" "X" "24" "1" "28" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M03" "X" "24" "1" "20" "INV-M02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D02" "X" "24" "1" "22" "INV-M03" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MEIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  ACT-RTN    THRU  ACT-EX.
           IF  ESTAT      =  "P6"
               PERFORM  LST-RTN    THRU  LST-EX
           END-IF.
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
           CALL "SD_Screen_Output" USING "SJN75I" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           MOVE     SPACE      TO    W-AREA.
           INITIALIZE                W-AREA.
       INI-EX.
           EXIT.
      ******************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm          *
      *          Å`èIóπèàóùÅ`      *
      ******************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
       END-EX.
           EXIT.
      *************************************
      *    Ç`ÇbÇsÅ|ÇqÇsÇm                 *
      *          Å`âÊñ ì¸óÕÅñçXêVèàóùÅ`   *
      *************************************
       ACT-RTN.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           MOVE     ZERO       TO    W-OSUD  W-NSUD.
       ACT-010.
           CALL "SD_Accept" USING BY REFERENCE A-SOC "A-SOC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT      =  "P6"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-010
           END-IF
           CALL "SD_Output" USING "A-SOC" A-SOC "p" RETURNING RESU.
           IF  W-SOC      <  1  OR  >  8
               GO  TO  ACT-010
           END-IF
      *
           MOVE     3          TO    JCON3-01.
           MOVE     W-SOC      TO    JCON3-02.
      *           READ     JCON       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-D02" INV-D02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-010
           END-IF
           CALL "SD_Output" USING "D-SON" D-SON "p" RETURNING RESU.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-020
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
      *
           MOVE     W-HCD      TO    HI-MHCD HI-HCD.
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
               GO  TO   ACT-020
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
       ACT-030.
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-020
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-030
           END-IF
      *
           IF  W-SC       =   1
               MOVE  HI-SS1     TO  W-SD
               MOVE  W-SIZ1     TO  W-SIZD
           END-IF
           IF  W-SC       =   2
               MOVE  HI-SS2     TO  W-SD
               MOVE  W-SIZ2     TO  W-SIZD
           END-IF
           IF  W-SC       =   3
               MOVE  HI-SS3     TO  W-SD
               MOVE  W-SIZ3     TO  W-SIZD
           END-IF
           IF  W-SC       =   4
               MOVE  HI-SS4     TO  W-SD
               MOVE  0          TO  W-S(10)
               MOVE  W-SIZ4     TO  W-SIZD
           END-IF
           IF  W-SD       =   ZERO
               CALL "SD_Output" USING
                "INV-M03" INV-M03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO   ACT-020
           END-IF
           CALL "SD_Output" USING "ACP-3" ACP-3 "p" RETURNING RESU.
      *
           MOVE     ZERO       TO    W-OSUD.
           MOVE     W-SOC      TO    NJZAI-01.
           MOVE     W-HCD      TO    NJZAI-02.
           MOVE     W-SC       TO    NJZAI-03.
      *           READ     NJZAI      UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO ACT-035
           END-IF
           PERFORM  ODS-RTN    THRU  ODS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
       ACT-035.
           CALL "SD_Output" USING "ACP-4" ACP-4 "p" RETURNING RESU.
       ACT-040.
           MOVE     1          TO    A.
           MOVE     2          TO    W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       ACT-045.
           IF  W-S(A)          =     0
               GO  TO  ACT-055
           END-IF.
       ACT-050.
           CALL "SD_Accept" USING BY REFERENCE A-NSU "A-NSU" "S9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-060
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-050
           END-IF
           CALL "SD_Output" USING "D-NSU" D-NSU "p" RETURNING RESU.
       ACT-055.
           ADD      1          TO    A.
           ADD      8          TO    W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  A               >     10
               GO  TO  ACT-090
           END-IF
           GO  TO  ACT-045.
       ACT-060.
           SUBTRACT  1         FROM  A.
           IF  A               =     ZERO
               GO  TO  ACT-030
           END-IF
           SUBTRACT  8         FROM  W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-S(A)          =     0
               GO  TO  ACT-060
           END-IF
           GO  TO  ACT-050.
       ACT-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  ACT-060
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACT-090
           END-IF
           IF  W-OKC      =   9
               GO  TO  ACT-010
           END-IF
           IF  W-OKC  NOT =   1
               GO  TO  ACT-090
           END-IF.
      *
       ACT-100.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           GO  TO   ACT-RTN.
       ACT-EX.
           EXIT.
      *************************************
      *    ÇtÇoÇcÅ|ÇqÇsÇm                 *
      *          Å`ÉtÉ@ÉCÉãçXêVèàóùÅ`     *
      *************************************
       UPD-RTN.
           MOVE     W-SOC       TO    NJZAI-01.
           MOVE     W-HCD       TO    NJZAI-02.
           MOVE     W-SC        TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-010
           END-IF
      *
           PERFORM  ZDS-RTN     THRU  ZDS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-020.
       UPD-010.
           MOVE     SPACE       TO    NJZAI-R.
           INITIALIZE                 NJZAI-R.
           MOVE     W-SOC       TO    NJZAI-01.
           MOVE     W-HCD       TO    NJZAI-02.
           MOVE     W-SC        TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *
           PERFORM  ZDS-RTN     THRU  ZDS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-010
           END-IF.
       UPD-020.
           MOVE     9           TO    NJZAI-01.
           MOVE     W-HCD       TO    NJZAI-02.
           MOVE     W-SC        TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *           READ     NJZAI      INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-030
           END-IF
      *
           PERFORM  ZDS-RTN     THRU  ZDS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
      *
      *           REWRITE  NJZAI-R    INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "R"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           GO  TO  UPD-EX.
       UPD-030.
           MOVE     SPACE       TO    NJZAI-R.
           INITIALIZE                 NJZAI-R.
           MOVE     9           TO    NJZAI-01.
           MOVE     W-HCD       TO    NJZAI-02.
           MOVE     W-SC        TO    NJZAI-03.
           MOVE     NJZAI-KEY   TO    ERR-K.
      *
           PERFORM  ZDS-RTN     THRU  ZDS-EX
                    VARYING   A    FROM  1  BY  1
                    UNTIL     A    >   10.
      *
           PERFORM  NJW-RTN     THRU  NJW-EX.
           IF  WRI-SW           =  1
               GO  TO  UPD-030
           END-IF.
       UPD-EX.
           EXIT.
      ****************************************
      *    èCê≥ëOç›å…êîÅ@ÉZÉbÉg              *
      ****************************************
       ODS-RTN.
           COMPUTE  W-OSU(A)   =  NJZAI-0411(A)  -  NJZAI-0511(A)
                               +  NJZAI-0611(A)  +  NJZAI-1111(A).
       ODS-EX.
           EXIT.
      ****************************************
      *    èCê≥å„ç›å…êîÅ@ÉZÉbÉg              *
      ****************************************
       ZDS-RTN.
           IF  NJZAI-01        =  9
               GO  TO  ZDS-010
           END-IF
           COMPUTE  W-SU(A)    =  W-NSU(A)       -  NJZAI-0411(A)
                               +  NJZAI-0511(A)  -  NJZAI-0611(A).
           MOVE     W-SU(A)    TO    NJZAI-1111(A).
           GO  TO  ZDS-EX.
       ZDS-010.
           COMPUTE  W-SSU(A)   =  W-NSU(A)       -  W-OSU(A).
           ADD      W-SSU(A)   TO    NJZAI-1111(A).
       ZDS-EX.
           EXIT.
       NJW-RTN.
           MOVE     0          TO    WRI-SW.
      *           WRITE  NJZAI-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  NJW-010
           END-IF
           GO  TO  NJW-EX.
       NJW-010.
           IF  ERR-STAT      =  "24"
               GO  TO  NJW-020
           END-IF
           IF  ERR-STAT  NOT =  "00"
               MOVE    "W"         TO    ERR-M
               MOVE    "NJZAI"     TO    ERR-F
               PERFORM  ERR-RTN  THRU    ERR-EX
           END-IF
           MOVE     2          TO    WRI-SW.
           GO  TO  NJW-EX.
       NJW-020.
           MOVE     1          TO    WRI-SW.
           MOVE    "W"         TO    ERR-M.
           MOVE    "NJZAI"     TO    ERR-F.
           MOVE     NJZAI-KEY  TO    ERR-K.
           MOVE     ERR-STAT   TO    ERR-FLG.
           CALL "SD_Output" USING
            "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING
            " " "¥ÿ± ∂∏¡Æ≥∫ﬁ,ª≤∂≤∑∞ ¶ µΩ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       NJW-EX.
           EXIT.
      *************************************
      *    ç›å…í≤êÆÅ@ÉäÉXÉg               *
      *************************************
       LST-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       LST-020.
      *           READ     NJZAI      NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  LST-EX
           END-IF
           IF  NJZAI-01        =  9
               GO  TO  LST-EX
           END-IF
           IF  ZERO = NJZAI-1111(01) AND NJZAI-1111(02) AND
                      NJZAI-1111(03) AND NJZAI-1111(04) AND
                      NJZAI-1111(05) AND NJZAI-1111(06) AND
                      NJZAI-1111(07) AND NJZAI-1111(08) AND
                      NJZAI-1111(09) AND NJZAI-1111(10)
               GO  TO  LST-020
           END-IF
      *
           CALL "PR_Open" RETURNING RESP.
           MOVE     ZERO       TO    W-PAGE.
           ACCEPT   H-DATE     FROM  DATE.
           PERFORM  MID-020     THRU  MID-EX.
       LST-040.
           MOVE     NJZAI-01    TO    W-SOC.
           MOVE     ZERO       TO    CHK.
       LST-060.
           MOVE     NJZAI-02    TO    W-HCD.
           MOVE     ZERO       TO    CHK2.
           MOVE     W-HCD      TO    HI-MHCD HI-HCD.
      *           READ     HI2-M      UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE       TO  HI-NAME
               MOVE  "Å@É}ÉXÉ^Å[Å@Ç»ÇµÅ@Åñ" TO HI-NAME
           END-IF.
       LST-080.
           COMPUTE W-SUT = NJZAI-1111(01) + NJZAI-1111(02) +
                           NJZAI-1111(03) + NJZAI-1111(04) +
                           NJZAI-1111(05) + NJZAI-1111(06) +
                           NJZAI-1111(07) + NJZAI-1111(08) +
                           NJZAI-1111(09) + NJZAI-1111(10).
      *
           MOVE     SPACE      TO  W-P.
           MOVE     SPACE      TO  P-NAME.
           IF  CHK1      =  0
               MOVE  1             TO  CHK1
               MOVE  W-SOC         TO  P-SOC
           END-IF
           IF  CHK2      =  0
               MOVE  1             TO  CHK2
               MOVE  W-HCD         TO  P-HCD
               MOVE  HI-NAME       TO  P-NAME
           END-IF
           MOVE     NJZAI-03   TO  P-SIZ.
           MOVE     NJZAI-1111(01)  TO  P-SU(01).
           MOVE     NJZAI-1111(02)  TO  P-SU(02).
           MOVE     NJZAI-1111(03)  TO  P-SU(03).
           MOVE     NJZAI-1111(04)  TO  P-SU(04).
           MOVE     NJZAI-1111(05)  TO  P-SU(05).
           MOVE     NJZAI-1111(06)  TO  P-SU(06).
           MOVE     NJZAI-1111(07)  TO  P-SU(07).
           MOVE     NJZAI-1111(08)  TO  P-SU(08).
           MOVE     NJZAI-1111(09)  TO  P-SU(09).
           MOVE     NJZAI-1111(10)  TO  P-SU(10).
           MOVE     W-SUT           TO  P-SUT.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER  >  60
               MOVE  W-SOC         TO  P-SOC
               MOVE  W-HCD         TO  P-HCD
               MOVE  HI-NAME       TO  P-NAME
               PERFORM  MID-RTN  THRU    MID-EX
           END-IF
           MOVE     SPACE           TO  SP-R.
           MOVE     W-P             TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE     SPACE           TO  SP-R.
       LST-100.
      *           READ     NJZAI      NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  LST-900
           END-IF
           IF  NJZAI-01        =  9
               GO  TO  LST-900
           END-IF
           IF  ZERO = NJZAI-1111(01) AND NJZAI-1111(02) AND
                      NJZAI-1111(03) AND NJZAI-1111(04) AND
                      NJZAI-1111(05) AND NJZAI-1111(06) AND
                      NJZAI-1111(07) AND NJZAI-1111(08) AND
                      NJZAI-1111(09) AND NJZAI-1111(10)
               GO  TO  LST-100
           END-IF
           IF  W-SOC       NOT =  NJZAI-01
               GO  TO  LST-040
           END-IF
           IF  W-HCD       NOT =  NJZAI-02
               GO  TO  LST-060
           END-IF
           GO  TO  LST-080.
       LST-900.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      *************************************
      *    å©èoÇµàÛéö                     *
      *************************************
       MID-RTN.
           MOVE  SPACE        TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD   1            TO  W-PAGE.
           MOVE  W-PAGE       TO  H-PAGE.
           MOVE  SPACE        TO  SP-R.
           MOVE  HEAD1        TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE        TO  SP-R.
           MOVE  HEAD2        TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE        TO  SP-R.
           MOVE  HEAD3        TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE        TO  SP-R.
           MOVE  HEAD4        TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE        TO  SP-R.
           MOVE  HEAD5        TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE  SPACE        TO  SP-R.
       MID-EX.
           EXIT.
      *
           COPY    LPMSG.
