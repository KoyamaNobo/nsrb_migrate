       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT231R.
      ******************************************************************
      *    <<REMARKS>>                                                 *
      *    FUNCTION.......  ïiñºï éÛíçécñ‚çáÇπ                         *
      *    COMPILE MODE...  NORMAL                                     *
      *    SCREEN.........  SJ231R                                     *
      *    RELEASE DATE...  98/10/26                                   *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                 PIC  X(02).
       01  W-DATA.
           02  W-HCD                PIC  9(06).
           02  W-DMM                PIC  9(01).
           02  W-ZC                 PIC  9(01).
           02  W-END                PIC  9(01).
           02  CNT                  PIC  9(02).
           02  W-KBN                PIC  N(01).
           02  W-KEI                PIC S9(07).
           02  W-ASUD.
               03  W-SUD   OCCURS  10.
                   04  W-SU         PIC S9(05).
           02  WT-KEI               PIC S9(07).
           02  WT-ASUD.
               03  WT-ASU   OCCURS   4.
                   04  WT-SUD   OCCURS  10.
                       05  WT-SU    PIC S9(05).
           02  W-L1                 PIC  9(02).
           02  W-L2                 PIC  9(02).
           02  W-LT                 PIC  9(02).
      *
           COPY  LJMST3.
           COPY  LITCM.
           COPY  LIHIM2.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-HCD     PIC  9(06).
           02  A-DMM     PIC  9(01).
       01  C-DSP.
           02  D-NAME    PIC  N(24).
           02  D-MEI.
               03  FILLER.
                   04  FILLER  PIC  N(24).
                   04  FILLER  PIC  9(04).
                   04  FILLER  PIC  --,---,--- .
               03  FILLER.
                   04  FILLER  PIC  9(06).
                   04  FILLER  PIC  X(01)       VALUE "-".
                   04  FILLER  PIC  9(01).
                   04  FILLER  PIC  N(01).
                   04  FILLER  PIC  ZZ .
                   04  FILLER  PIC  ZZ .
                   04  FILLER  PIC  9(01).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
           02  D-TOTAL.
               03  D-TMEI.
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
                   04  FILLER  PIC  -(06).
               03  D-TKEI  PIC --,---,--- .
       01  D-SPACE.
           02  S-HCD.
               03  FILLER  PIC  X(06) VALUE   "      ".
               03  FILLER  PIC  X(48) VALUE
                     "                                                ".
           02  S-MEI.
               03  FILLER.
                   04  FILLER  PIC  X(48) VALUE
                     "                                                ".
                   04  FILLER  PIC  X(04) VALUE   "    ".
                   04  FILLER  PIC  X(10) VALUE   "          ".
               03  FILLER.
                   04  FILLER  PIC  X(08) VALUE   "        ".
                   04  FILLER  PIC  X(02) VALUE   "  ".
                   04  FILLER  PIC  X(02) VALUE   "  ".
                   04  FILLER  PIC  X(02) VALUE   "  ".
                   04  FILLER  PIC  X(31) VALUE
                          "                               ".
                   04  FILLER  PIC  X(30) VALUE
                          "                              ".
           02  S-TOTAL.
               03  S-TMEI.
                   04  FILLER  PIC  X(30) VALUE
                          "                              ".
                   04  FILLER  PIC  X(30) VALUE
                          "                              ".
               03  S-TKEI  PIC  X(10) VALUE
                   "          ".
           02  S-DMM     PIC  X(01) VALUE  " ".
       01  C-ERR.
           02  FILLER.
               03  E-ME01   PIC  N(06) VALUE
                   "ïiñºÅ@ñ¢ìoò^".
               03  E-ME02   PIC  N(07) VALUE
                   "ÇcÇ`ÇsÇ`Å@Ç»Çµ".
               03  E-ME03   PIC  N(08) VALUE
                   "ÇdÇmÇcÅ@ÇcÇ`ÇsÇ`".
               03  E-ME04   PIC  N(11) VALUE
                   "ÇoÇqÇnÇfÇqÇ`ÇlÅ@ÉGÉâÅ[".
               03  E-ME98   PIC  X(05) VALUE X"1B4A05".
               03  E-ME99   PIC  X(05) VALUE X"1B4205".
               03  E-CL     PIC  X(50) VALUE
                   "                                                  ".
       PROCEDURE           DIVISION.
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
            "C-ACP" " " "0" "0" "7" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "1" "23" "6" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "24" "76" "1" "A-HCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "255" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "1" "30" "48" " " "C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "0" "0" "137" "D-NAME" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEI" " " "W-L1" "30" "62" " " "D-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
           "0101D-MEI" "N" "W-L1" "1" "48" " " "01D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "0101D-MEI" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MEI" "9" "W-L1" "51" "4" "0101D-MEI" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-MEI" BY REFERENCE JMST3-04 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0301D-MEI" "--,---,---" "W-L1" "71" "10" "0201D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301D-MEI" BY REFERENCE W-KEI "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEI" " " "W-L2" "30" "75" "01D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MEI" "9" "W-L2" "2" "6" " " "02D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MEI" BY REFERENCE JMST3-07 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "0202D-MEI" "X" "W-L2" "8" "1" "0102D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0302D-MEI" "9" "W-L2" "9" "1" "0202D-MEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-MEI" BY REFERENCE JMST3-08 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-MEI" "N" "W-L2" "11" "2" "0302D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0402D-MEI" BY REFERENCE W-KBN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-MEI" "ZZ" "W-L2" "14" "2" "0402D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0502D-MEI" BY REFERENCE JMST3-062 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0602D-MEI" "ZZ" "W-L2" "17" "2" "0502D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0602D-MEI" BY REFERENCE JMST3-063 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0702D-MEI" "9" "W-L2" "20" "1" "0602D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0702D-MEI" BY REFERENCE JMST3-09 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0802D-MEI" "------" "W-L2" "21" "6" "0702D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0802D-MEI" BY REFERENCE W-SU(1) "5" "1" "01" 5 
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0902D-MEI" "------" "W-L2" "27" "6" "0802D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0902D-MEI" BY REFERENCE W-SU(1) "5" "1" "02" 5 
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1002D-MEI" "------" "W-L2" "33" "6" "0902D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1002D-MEI" BY REFERENCE W-SU(1) "5" "1" "03" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1102D-MEI" "------" "W-L2" "39" "6" "1002D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1102D-MEI" BY REFERENCE W-SU(1) "5" "1" "04" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1202D-MEI" "------" "W-L2" "45" "6" "1102D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1202D-MEI" BY REFERENCE W-SU(1) "5" "1" "05" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1302D-MEI" "------" "W-L2" "51" "6" "1202D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1302D-MEI" BY REFERENCE W-SU(1) "5" "1" "06" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1402D-MEI" "------" "W-L2" "57" "6" "1302D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1402D-MEI" BY REFERENCE W-SU(1) "5" "1" "07" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1502D-MEI" "------" "W-L2" "63" "6" "1402D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1502D-MEI" BY REFERENCE W-SU(1) "5" "1" "08" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1602D-MEI" "------" "W-L2" "69" "6" "1502D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1602D-MEI" BY REFERENCE W-SU(1) "5" "1" "09" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "1702D-MEI" "------" "W-L2" "75" "6" "1602D-MEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "1702D-MEI" BY REFERENCE W-SU(1) "5" "1" "10" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TOTAL" " " "0" "0" "70" "D-MEI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TMEI" " " "W-LT" "30" "60" " " "D-TOTAL" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TMEI" "------" "W-LT" "21" "6" " " "D-TMEI"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TMEI" BY REFERENCE W-SU(1) "5" "1" "01" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TMEI" "------" "W-LT" "27" "6" "01D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-TMEI" BY REFERENCE W-SU(1) "5" "1" "02" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TMEI" "------" "W-LT" "33" "6" "02D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-TMEI" BY REFERENCE W-SU(1) "5" "1" "03" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TMEI" "------" "W-LT" "39" "6" "03D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04D-TMEI" BY REFERENCE W-SU(1) "5" "1" "04" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-TMEI" "------" "W-LT" "45" "6" "04D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05D-TMEI" BY REFERENCE W-SU(1) "5" "1" "05" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-TMEI" "------" "W-LT" "51" "6" "05D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "06D-TMEI" BY REFERENCE W-SU(1) "5" "1" "06" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-TMEI" "------" "W-LT" "57" "6" "06D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "07D-TMEI" BY REFERENCE W-SU(1) "5" "1" "07" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-TMEI" "------" "W-LT" "63" "6" "07D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "08D-TMEI" BY REFERENCE W-SU(1) "5" "1" "08" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-TMEI" "------" "W-LT" "69" "6" "08D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "09D-TMEI" BY REFERENCE W-SU(1) "5" "1" "09" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-TMEI" "------" "W-LT" "75" "6" "09D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "10D-TMEI" BY REFERENCE W-SU(1) "5" "1" "10" 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TKEI" "--,---,---" "23" "9" "10" "D-TMEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-TKEI" BY REFERENCE WT-KEI "7" "0" RETURNING RESU.
      *D-SPACE
       CALL "SD_Init" USING 
            "D-SPACE" " " "0" "0" "262" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCD" " " "1" "0" "54" " " "D-SPACE"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-HCD" "X" "1" "23" "6" " " "S-HCD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-HCD" "X" "1" "30" "48" "01S-HCD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-MEI" " " "0" "0" "137" "S-HCD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-MEI" " " "W-L1" "0" "62" " " "S-MEI"  RETURNING RESU.
       CALL "SD_Init" USING 
           "0101S-MEI" "X" "W-L1" "1" "48" " " "01S-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201S-MEI" "X" "W-L1" "51" "4" "0101S-MEI" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0301S-MEI" "X" "W-L1" "71" "10" "0201S-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-MEI" " " "W-L2" "0" "75" "01S-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102S-MEI" "X" "W-L2" "2" "8" " " "02S-MEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202S-MEI" "X" "W-L2" "11" "2" "0102S-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302S-MEI" "X" "W-L2" "14" "2" "0202S-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402S-MEI" "X" "W-L2" "17" "2" "0302S-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0502S-MEI" "X" "W-L2" "20" "31" "0402S-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0602S-MEI" "X" "W-L2" "51" "30" "0502S-MEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "S-TOTAL" " " "0" "0" "70" "S-MEI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-TMEI" " " "W-LT" "0" "60" " " "S-TOTAL" RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-TMEI" "X" "W-LT" "21" "30" " " "S-TMEI" RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-TMEI" "X" "W-LT" "51" "30" "01S-TMEI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "S-TKEI" "X" "23" "9" "10" "S-TMEI" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "S-DMM" "X" "24" "76" "1" "S-TOTAL" " "  RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "124" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "124" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME01" "N" "24" "1" "12" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME02" "N" "24" "1" "14" "E-ME01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME03" "N" "24" "1" "16" "E-ME02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME04" "N" "24" "1" "22" "E-ME03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME04" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "1" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ231R" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT  =   "P9"
               GO  TO  M-95
           END-IF
           IF  ESTAT  NOT =   "01"  AND  "06"
               GO  TO  M-10
           END-IF
           MOVE      W-HCD      TO    HI-MHCD HI-KEY.
      *           READ      HI2-M            UNLOCK       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME01" E-ME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
      *
           MOVE      ZERO       TO    WT-ASUD  WT-KEI  W-ZC.
           MOVE      ZERO       TO    JMST3-KEY.
           MOVE      W-HCD      TO    JMST3-03.
      *           START     JMST3      KEY  NOT <  JMST3-KEY      INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME02" E-ME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-10
           END-IF.
       M-15.
      *           READ      JMST3      NEXT    UNLOCK       AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME02" E-ME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           IF  JMST3-03   NOT =   W-HCD
               CALL "SD_Output" USING
                "E-ME02" E-ME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-10
           END-IF.
       M-20.
           MOVE      ZERO       TO    W-ASUD  CNT.
       M-25.
           ADD       1          TO    CNT.
           IF  CNT            >   10
               GO  TO  M-30
           END-IF
           COMPUTE  W-SU(CNT)  =  JMST3-1111(CNT)  -  JMST3-1211(CNT)
                                                   -  JMST3-141(CNT).
           IF  W-SU(CNT)      =   ZERO
               GO  TO  M-25
           END-IF
           ADD      W-SU(CNT)   TO    WT-SU(JMST3-09,CNT)   WT-KEI.
           IF  W-ZC       NOT =   1
               MOVE   1             TO  W-ZC
           END-IF
           GO  TO  M-25.
       M-30.
      *           READ      JMST3      NEXT    UNLOCK       AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-35
           END-IF
           IF  JMST3-03   NOT =   W-HCD
               GO  TO  M-35
           END-IF
           GO  TO  M-20.
       M-35.
           IF  W-ZC       NOT =   1
               CALL "SD_Output" USING
                "E-ME02" E-ME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-10
           END-IF
           MOVE      19         TO    W-LT.
           CALL "SD_Arg_Match_Line" USING
            "W-LT" "2" W-LT RETURNING RESU.
           MOVE      ZERO       TO    CNT.
       M-40.
           ADD       1          TO    W-LT   CNT.
           CALL "SD_Arg_Match_Line" USING
            "W-LT" "2" W-LT RETURNING RESU.
           IF  CNT        NOT =   5
               MOVE  ZERO           TO  W-ASUD
               MOVE  WT-ASU(CNT)    TO  W-ASUD
               CALL "SD_Output" USING
                "D-TMEI" D-TMEI "p" RETURNING RESU
               GO  TO  M-40
           END-IF
           CALL "SD_Output" USING "D-TKEI" D-TKEI "p" RETURNING RESU.
      *
           MOVE      0          TO    W-END.
           MOVE      4          TO    W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE      5          TO    W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE      ZERO       TO    JMST3-KEY.
           MOVE      W-HCD      TO    JMST3-03.
      *           START     JMST3      KEY  NOT <  JMST3-KEY      INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME04" E-ME04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO  TO  M-65
           END-IF.
       M-45.
      *           READ      JMST3      NEXT    UNLOCK       AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      1          TO    W-END
               CALL "SD_Output" USING
                "E-ME03" E-ME03 "p" RETURNING RESU
               GO  TO  M-60
           END-IF
           IF  JMST3-03   NOT =   W-HCD
               MOVE      1          TO    W-END
               CALL "SD_Output" USING
                "E-ME03" E-ME03 "p" RETURNING RESU
               GO  TO  M-60
           END-IF
      *
           MOVE      ZERO       TO    W-ASUD  W-KEI   CNT   W-ZC.
       M-50.
           ADD       1          TO    CNT.
           IF  CNT            >   10
               GO  TO  M-55
           END-IF
           COMPUTE  W-SU(CNT)  =  JMST3-1111(CNT)  -  JMST3-1211(CNT)
                                                   -  JMST3-141(CNT).
           ADD      W-SU(CNT)   TO    W-KEI.
           IF  W-SU(CNT)  NOT =   ZERO
               IF  W-ZC           =   ZERO
                   MOVE   1           TO  W-ZC
               END-IF
           END-IF
           GO  TO  M-50.
       M-55.
           IF  W-ZC           =   0
               GO  TO  M-45
           END-IF
           MOVE      JMST3-04   TO    TC-TCD.
           MOVE      001        TO    TC-CCD.
      *           READ      TC-M             UNLOCK       INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE   TO   TC-NAME
           END-IF
           MOVE  "Å@"       TO  W-KBN.
           IF  JMST3-01       =   5
               MOVE  "óa"       TO  W-KBN
           END-IF
           IF  JMST3-01       =   6
               MOVE  "éÊ"       TO  W-KBN
           END-IF
           ADD       2          TO    W-L1  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1           <   19
               CALL "SD_Output" USING
                "D-MEI" D-MEI "p" RETURNING RESU
               GO  TO  M-45
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      NOT =  "01"
               GO  TO  M-60
           END-IF
           IF  W-DMM          =  9
               GO  TO  M-65
           END-IF
           IF  W-DMM      NOT =  1
               GO  TO  M-60
           END-IF
           IF  W-END      NOT =  0
               GO  TO  M-65
           END-IF
      *
           PERFORM   CMI-RTN    THRU  CMI-EX.
           CALL "SD_Output" USING "S-DMM" S-DMM "p" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           MOVE      6          TO    W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE      7          TO    W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
           GO  TO  M-45.
       M-65.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ231R" RETURNING RESU.
           GO  TO  M-10.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       CMI-RTN.
           MOVE      4          TO    W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE      5          TO    W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
       CMI-010.
           ADD       2          TO    W-L1  W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           IF  W-L1           <  19
               CALL "SD_Output" USING "S-MEI" S-MEI "p" RETURNING RESU
               GO  TO  CMI-010
           END-IF.
       CMI-EX.
           EXIT.
       CTT-RTN.
           MOVE      19         TO    W-LT.
           CALL "SD_Arg_Match_Line" USING
            "W-LT" "2" W-LT RETURNING RESU.
       CTT-010.
           ADD       1          TO    W-LT.
           CALL "SD_Arg_Match_Line" USING
            "W-LT" "2" W-LT RETURNING RESU.
           IF  W-LT           <  24
               CALL "SD_Output" USING
                "S-TOTAL" S-TOTAL "p" RETURNING RESU
               GO  TO  CTT-010
           END-IF.
       CTT-EX.
           EXIT.
