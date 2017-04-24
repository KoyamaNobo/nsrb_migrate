       IDENTIFICATION        DIVISION.
       PROGRAM-ID.           JT321U.
      **************************************************
      *    PROGRAM      :    é¿ÅEóLå¯ç›å…ÉfÅ[É^íäèo    *
      *    DATA WRITTEN :    00/07/21                  *
      *    SCREEN  USED :    ______                    *
      *    COMPILE TYPE :    CBL85                     *
      **************************************************
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                 PIC  X(02).
       77  WK0128ID                 PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1              PIC  X(003).
           02  STN-NO2              PIC  X(003).
       01  W-FID.
           02  W-FID1               PIC  X(006) VALUE "WK0128".
           02  W-FID2               PIC  X(003).
       01  I                        PIC  9(02).
       01  ZERO-SW                  PIC  9(01).
       01  DATA-SW                  PIC  9(01).
       01  WORK-AREA.
           02  W-JCC                PIC  9(01).
           02  W-KURA               PIC  9(01).
           02  W-HCD                PIC  9(06).
           02  W-SIZ                PIC  9(01).
           02  CHK                  PIC  9(01).
           02  W-AREA.
             03  W-ZCHK             PIC  9(01).
             03  W-SEN              PIC  9(01).
             03  W-FROM.
                 04  W-FK           PIC  9(01).
                 04  W-FH1          PIC  9(06).
                 04  W-FH2          PIC  9(06).
                 04  W-FH3          PIC  9(06).
                 04  W-FH4          PIC  9(06).
                 04  W-FH5          PIC  9(06).
             03  W-TO.
                 04  W-TK           PIC  9(01).
                 04  W-TH           PIC  9(06).
                 04  W-TH1          PIC  9(06).
                 04  W-TH2          PIC  9(06).
                 04  W-TH3          PIC  9(06).
                 04  W-TH4          PIC  9(06).
                 04  W-TH5          PIC  9(06).
             03  OKC                PIC  9(01).
      *
           COPY     LWMSG.
      *
           COPY     LNJZAI.
           COPY     LIHIM2.
           COPY     LJMST3.
           COPY     LJNYZ.
      *FD  JT-YZAI
       01  JT-YZAI_JT321U.
           02  JT-YZAI_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  JT-YZAI_LNAME        PIC  X(014) VALUE "JT-YZAI_JT321U".
           02  F                    PIC  X(001).
           02  JT-YZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_SORT         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_RES          USAGE  POINTER.
       01  YZAI-R.
           02   YZAI-01             PIC 9(1).
           02   YZAI-02             PIC 9(6).
           02   YZAI-03             PIC 9(1).
           02   YZAI-04.
                03   YZAI-041       OCCURS  10.
                    04   YZAI-0411  PIC S9(6).
           02   FILLER              PIC X(11).
           02   YZAI-88             PIC 9(01).
           02   YZAI-89             PIC 9(01).
           02   YZAI-90             PIC 9(01).
           02   YZAI-91.
                03   YZAI-911       PIC 9(01).
                03   YZAI-912       PIC 9(01).
           02   YZAI-92             PIC 9(01).
           02   YZAI-93             PIC 9(01).
           02   FILLER              PIC X(42).
       77  F                        PIC X(01).
      *
       77  ESTAT                    PIC  X(002).
       77  RESU                     PIC  9(001).
       77  RET                      PIC  9(001) VALUE ZERO.
       77  USER_ID                  PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE          PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL      PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLR-01.
           02  CLR-ZCHK  PIC  X(01) VALUE " ".
           02  CLR-SEN   PIC  X(01) VALUE " ".
           02  CLR-FK    PIC  X(01) VALUE " ".
           02  CLR-TK    PIC  X(01) VALUE " ".
           02  CLR-FH1   PIC  X(06) VALUE "      ".
           02  CLR-TH1   PIC  X(06) VALUE "      ".
           02  CLR-FH2   PIC  X(06) VALUE "      ".
           02  CLR-TH2   PIC  X(06) VALUE "      ".
           02  CLR-FH3   PIC  X(06) VALUE "      ".
           02  CLR-TH3   PIC  X(06) VALUE "      ".
           02  CLR-FH4   PIC  X(06) VALUE "      ".
           02  CLR-TH4   PIC  X(06) VALUE "      ".
           02  CLR-FH5   PIC  X(06) VALUE "      ".
           02  CLR-TH5   PIC  X(06) VALUE "      ".
      ***
       01  DSP-AREA.
           02  FILLER    PIC  X(22) VALUE
                "                      " .
           02  FILLER    PIC  X(20) VALUE
               "ëqï éÛï•ÉèÅ[ÉNÅ@íäèo".
           02  FILLER    PIC  X(17) VALUE "é¿ç›å…éÛï•    = 0".
           02  FILLER    PIC  X(23) VALUE "óLå¯ç›å…éÛï•  = 1 ...  ".
           02  FILLER    PIC  X(14) VALUE "ã≥Å@Å@àÁÅ@ = 0".
           02  FILLER    PIC  X(14) VALUE "ÉèÅ@Å[Å@ÉN = 1".
           02  FILLER    PIC  X(14) VALUE "àÍÅ@Å@î    = 2".
           02  FILLER    PIC  X(20) VALUE "ëSÅ@Å@åèÅ@ = 9 ...  ".
           02  FILLER    PIC  X(42) VALUE
               "ëq  ïiñºÇP      ÇQ      ÇR      ÇS      ÇT".
           02  FILLER    PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  FILLER    PIC  X(04) VALUE  "ÇsÇn".
           02  FILLER    PIC  X(25) VALUE  "ämîF(OK=1,NO=9)-->   ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-ZCHK  PIC 9(01).
           02  ACP-SEN   PIC 9(01).
           02  ACP-FK    PIC 9(01).
           02  ACP-TK    PIC 9(01).
           02  ACP-FH1   PIC 9(06).
           02  ACP-TH1   PIC 9(06).
           02  ACP-FH2   PIC 9(06).
           02  ACP-TH2   PIC 9(06).
           02  ACP-FH3   PIC 9(06).
           02  ACP-TH3   PIC 9(06).
           02  ACP-FH4   PIC 9(06).
           02  ACP-TH4   PIC 9(06).
           02  ACP-FH5   PIC 9(06).
           02  ACP-TH5   PIC 9(06).
           02  ACP-OKC   PIC 9(01).
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER    PIC X(39).
           02  DISP-MSG-SPACE.
               03  FILLER    PIC X(39).
           02  DISP-BUZ-B.
               03  FILLER    PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER    PIC X(05) VALUE X"1B4A01".
           02  ERR-02.
               03  FILLER    PIC X(22) VALUE
               "ÅñÅ@ÉfÅ[É^Å@Ç»ÇµÅ@Å@Åñ".
           02  ERR-DIS.
               03  FILLER    PIC X(05) VALUE
               "<<<  ".
               03  FILLER    PIC X(12).
               03  FILLER    PIC X(01).
               03  FILLER    PIC X(11) VALUE
               "¥◊∞ STATUS=".
               03  FILLER    PIC X(02).
               03  FILLER    PIC X(05) VALUE
               "  >>>".
               03  FILLER    PIC X(05) VALUE
               " KEY=".
               03  FILLER    PIC X(30).
      **
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *CLR-01
       CALL "SD_Init" USING 
            "CLR-01" " " "0" "0" "64" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-ZCHK" "X" "10" "36" "1" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-SEN" "X" "10" "54" "1" "CLR-ZCHK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FK" "X" "16" "23" "1" "CLR-SEN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TK" "X" "18" "23" "1" "CLR-FK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH1" "X" "16" "26" "6" "CLR-TK" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH1" "X" "18" "26" "6" "CLR-FH1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH2" "X" "16" "34" "6" "CLR-TH1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH2" "X" "18" "34" "6" "CLR-FH2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH3" "X" "16" "42" "6" "CLR-TH2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH3" "X" "18" "42" "6" "CLR-FH3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH4" "X" "16" "50" "6" "CLR-TH3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH4" "X" "18" "50" "6" "CLR-FH4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-FH5" "X" "16" "58" "6" "CLR-TH4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-TH5" "X" "18" "58" "6" "CLR-FH5" " "  RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "223" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "27" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "1" "28" "20" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "8" "13" "17" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "10" "13" "23" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "6" "44" "14" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "8" "44" "14" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "10" "44" "14" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "12" "44" "20" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "14" "22" "42" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-AREA" "X" "16" "13" "8" "09DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-AREA" "X" "18" "13" "4" "10DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12DSP-AREA" "X" "23" "41" "25" "11DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "65" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ZCHK" "9" "10" "35" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ZCHK" BY REFERENCE W-ZCHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "12" "63" "1" "ACP-ZCHK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FK" "9" "16" "23" "1" "ACP-SEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FK" BY REFERENCE W-FK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TK" "9" "18" "23" "1" "ACP-FK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TK" BY REFERENCE W-TK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH1" "9" "16" "26" "6" "ACP-TK" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH1" BY REFERENCE W-FH1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH1" "9" "18" "26" "6" "ACP-FH1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH1" BY REFERENCE W-TH1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH2" "9" "16" "34" "6" "ACP-TH1" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH2" BY REFERENCE W-FH2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH2" "9" "18" "34" "6" "ACP-FH2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH2" BY REFERENCE W-TH2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH3" "9" "16" "42" "6" "ACP-TH2" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH3" BY REFERENCE W-FH3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH3" "9" "18" "42" "6" "ACP-FH3" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH3" BY REFERENCE W-TH3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH4" "9" "16" "50" "6" "ACP-TH3" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH4" BY REFERENCE W-FH4 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH4" "9" "18" "50" "6" "ACP-FH4" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH4" BY REFERENCE W-TH4 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FH5" "9" "16" "58" "6" "ACP-TH4" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FH5" BY REFERENCE W-FH5 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TH5" "9" "18" "58" "6" "ACP-FH5" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TH5" BY REFERENCE W-TH5 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "ACP-TH5" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "181" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-01" " " "24" "0" "39" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-01" "X" "24" "1" "39" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING 
          "01DISP-MSG-01" BY REFERENCE ERR-MSGX "60" "0" RETURNING RESU.
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
          "DISP-BUZ-J" " " "24" "0" "5" "DISP-BUZ-B" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZ-J"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-02" " " "24" "0" "22" "DISP-BUZ-J" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-02" "X" "24" "1" "22" " " "ERR-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DIS" " " "24" "0" "71" "ERR-02" " "  RETURNING RESU.
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
       MEIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           IF  COMPLETION_CODE  = 255
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  UPD1-RTN   THRU  UPD1-EX.
           IF  W-ZCHK   =  0
               GO  TO  MR999
           END-IF
           PERFORM  UPD2-RTN   THRU  UPD2-EX.
           PERFORM  UPD3-RTN   THRU  UPD3-EX.
           PERFORM  UPD4-RTN   THRU  UPD4-EX.
       MR999.
           PERFORM  END-RTN    THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
      ******************************
      *    ÇhÇmÇhÅ|ÇqÇsÇm          *
      *          Å`èâä˙èàóùÅ`      *
      ******************************
       INI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           INITIALIZE  W-AREA.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-ZCHK "ACP-ZCHK"
            "9" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-010
           END-IF
           CALL "SD_Output" USING
            "ACP-ZCHK" ACP-ZCHK "p" RETURNING RESU.
           IF  W-ZCHK NOT =  0 AND 1
               GO  TO  INI-010
           END-IF.
       INI-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-020
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT =  0  AND 1 AND 2 AND  9
               GO  TO  INI-020
           END-IF
           IF  W-ZCHK     =  1
               MOVE   9        TO  W-FK  W-TK
               CALL "SD_Output" USING "CLR-FK" CLR-FK "p" RETURNING RESU
               CALL "SD_Output" USING "CLR-TK" CLR-TK "p" RETURNING RESU
               GO  TO  INI-055
           END-IF.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-FK "ACP-FK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-020
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-030
           END-IF
           CALL "SD_Output" USING "ACP-FK" ACP-FK "p" RETURNING RESU.
       INI-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-TK "ACP-TK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-030
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-050
           END-IF
           CALL "SD_Output" USING "ACP-TK" ACP-TK "p" RETURNING RESU.
           IF  W-FK  >  W-TK
               GO  TO  INI-050
           END-IF.
       INI-055.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH1 "ACP-FH1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               IF  W-ZCHK     =  1
                   GO  TO  INI-020
               ELSE
                   GO  TO  INI-050
               END-IF
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-055
           END-IF
           CALL "SD_Output" USING "ACP-FH1" ACP-FH1 "p" RETURNING RESU.
       INI-060.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH1 "ACP-TH1" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-055
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-060
           END-IF
           CALL "SD_Output" USING "ACP-TH1" ACP-TH1 "p" RETURNING RESU.
           IF  W-FH1   >  W-TH1
               GO  TO  INI-060
           END-IF
           IF  W-TH1 = 999999
               MOVE W-TH1 TO W-TH
               MOVE ZERO TO W-FH2 W-TH2 W-FH3 W-TH3 W-FH4 W-TH4
                                                    W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH2" CLR-FH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH2" CLR-TH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-070.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH2 "ACP-FH2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-060
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-070
           END-IF
           IF  W-FH2 = ZERO
               MOVE W-TH1 TO W-TH
               MOVE ZERO TO W-FH2 W-TH2 W-FH3 W-TH3 W-FH4 W-TH4
                                                    W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH2" CLR-FH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH2" CLR-TH2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH2" ACP-FH2 "p" RETURNING RESU.
           IF  W-FH2   <  W-TH1
               GO  TO  INI-070
           END-IF.
       INI-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH2 "ACP-TH2" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-070
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-080
           END-IF
           CALL "SD_Output" USING "ACP-TH2" ACP-TH2 "p" RETURNING RESU.
           IF  W-FH2   >  W-TH2
               GO  TO  INI-080
           END-IF
           IF  W-TH2 = 999999
               MOVE W-TH2 TO W-TH
               MOVE ZERO TO W-FH3 W-TH3 W-FH4 W-TH4 W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH3 "ACP-FH3" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-080
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-090
           END-IF
           IF  W-FH3 = ZERO
               MOVE W-TH2 TO W-TH
               MOVE ZERO TO W-FH3 W-TH3 W-FH4 W-TH4 W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH3" CLR-FH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH3" CLR-TH3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH3" ACP-FH3 "p" RETURNING RESU.
           IF  W-FH3   <  W-TH2
               GO  TO  INI-090
           END-IF.
       INI-100.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH3 "ACP-TH3" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-090
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-100
           END-IF
           CALL "SD_Output" USING "ACP-TH3" ACP-TH3 "p" RETURNING RESU.
           IF  W-FH3   >  W-TH3
               GO  TO  INI-100
           END-IF
           IF  W-TH3 = 999999
               MOVE W-TH3 TO W-TH
               MOVE ZERO TO W-FH4 W-TH4 W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-110.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH4 "ACP-FH4" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-100
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-110
           END-IF
           IF  W-FH4 = ZERO
               MOVE W-TH3 TO W-TH
               MOVE ZERO TO W-FH4 W-TH4 W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH4" CLR-FH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH4" CLR-TH4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH4" ACP-FH4 "p" RETURNING RESU.
           IF  W-FH4   <  W-TH3
               GO  TO  INI-110
           END-IF.
       INI-120.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH4 "ACP-TH4" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-110
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-120
           END-IF
           CALL "SD_Output" USING "ACP-TH4" ACP-TH4 "p" RETURNING RESU.
           IF  W-FH4   >  W-TH4
               GO  TO  INI-120
           END-IF
           IF  W-TH4 = 999999
               MOVE W-TH4 TO W-TH
               MOVE ZERO TO W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF.
       INI-130.
           CALL "SD_Accept" USING BY REFERENCE ACP-FH5 "ACP-FH5" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-120
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-130
           END-IF
           IF  W-FH5 = ZERO
               MOVE W-TH4 TO W-TH
               MOVE ZERO TO W-FH5 W-TH5
               CALL "SD_Output" USING
                "CLR-FH5" CLR-FH5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLR-TH5" CLR-TH5 "p" RETURNING RESU
               GO TO INI-510
           END-IF
           CALL "SD_Output" USING "ACP-FH5" ACP-FH5 "p" RETURNING RESU.
           IF  W-FH5   <  W-TH4
               GO  TO  INI-130
           END-IF.
       INI-140.
           CALL "SD_Accept" USING BY REFERENCE ACP-TH5 "ACP-TH5" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-130
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-140
           END-IF
           CALL "SD_Output" USING "ACP-TH5" ACP-TH5 "p" RETURNING RESU.
           IF  W-FH5   >  W-TH5
               GO  TO  INI-140
           END-IF
           MOVE W-TH5 TO W-TH.
       INI-510.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  NOT =  "09"
               GO TO INI-520
           END-IF
           IF  W-FH2 = ZERO
               GO  TO  INI-060
           END-IF
           IF  W-FH3 = ZERO
               GO  TO  INI-080
           END-IF
           IF  W-FH4 = ZERO
               GO  TO  INI-100
           END-IF
           IF  W-FH5 = ZERO
               GO  TO  INI-120
           END-IF
           GO  TO  INI-140.
       INI-520.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-510
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT =  "1"  AND  "9"
               GO  TO  INI-510
           END-IF
           IF  OKC  =  "9"
               GO  TO  INI-RTN
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-YZAI_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-YZAI_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-YZAI_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
            "JNYZ-KEY" BY REFERENCE JNYZ-KEY.
       INI-EX.
           EXIT.
      ******************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm          *
      *          Å`èIóπèàóùÅ`      *
      ******************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JNYZ_IDLST JNYZ_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ******************************
      ***   UPD1  RTN            ***
      ******************************
       UPD1-RTN.
           INITIALIZE                 NJZAI-KEY.
           MOVE  W-FK             TO  NJZAI-01.
           MOVE  W-FH1            TO  NJZAI-02.
      *
      *           START  NJZAI  KEY  NOT <  NJZAI-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF.
       UPD1-010.
      ***  ëqï ç›å…É}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  NJZAI  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF
           MOVE  NJZAI-01     TO  W-KURA.
           MOVE  NJZAI-02     TO  W-HCD.
           MOVE  NJZAI-03     TO  W-SIZ.
           MOVE  W-HCD        TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD1-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD1-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   >  19
                   GO  TO  UPD1-010
               END-IF
           END-IF
      *
           IF  W-KURA    >  W-TK
               GO  TO  UPD1-EX
           END-IF
           IF  W-KURA    >  W-TH
               GO  TO  UPD1-EX
           END-IF
           IF  W-HCD     >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD     >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD     >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD     >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD1-020
           END-IF
           IF  W-HCD     >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD1-020
           END-IF
           GO  TO  UPD1-010.
      *
       UPD1-020.
           PERFORM  ZC4-RTN      THRU  ZC4-EX.
           IF  ZERO-SW  =  1
               PERFORM  WRI-RTN      THRU  WRI-EX
           END-IF
      *
           IF  W-ZCHK   =  1
               GO  TO  UPD1-010
           END-IF
           PERFORM  ZC0-RTN      THRU  ZC0-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           PERFORM  ZC1-RTN      THRU  ZC1-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           PERFORM  ZC2-RTN      THRU  ZC2-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           PERFORM  ZC3-RTN      THRU  ZC3-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           GO  TO  UPD1-010.
       UPD1-EX.
           EXIT.
      ******************************
      ***   UPD2  RTN            ***
      ******************************
       UPD2-RTN.
           INITIALIZE                 JMST3-KEY.
           MOVE  W-FH1            TO  JMST3-03.
      *
      *           START  JMST3  KEY  NOT <  JMST3-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-EX
           END-IF.
       UPD2-010.
      *           READ  JMST3  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-EX
           END-IF
           MOVE  9            TO  W-KURA.
           MOVE  JMST3-03     TO  W-HCD.
           MOVE  JMST3-09     TO  W-SIZ.
           MOVE  W-HCD        TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD2-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD2-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   >  19
                   GO  TO  UPD2-010
               END-IF
           END-IF
      *
           IF  JMST3-01  NOT =  0 AND  5  AND  6
               GO  TO  UPD2-010
           END-IF
           IF  JMST3-03  >  W-TH
               GO  TO  UPD2-EX
           END-IF
           IF  JMST3-03  >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD2-020
           END-IF
           IF  JMST3-03  >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD2-020
           END-IF
           IF  JMST3-03  >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD2-020
           END-IF
           IF  JMST3-03  >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD2-020
           END-IF
           IF  JMST3-03  >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD2-020
           END-IF
           GO  TO  UPD2-010.
      *
       UPD2-020.
           PERFORM  ZC6-RTN      THRU  ZC6-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           GO  TO  UPD2-010.
       UPD2-EX.
           EXIT.
      ******************************
      ***   UPD3  RTN            ***
      ******************************
       UPD3-RTN.
           INITIALIZE                 JNYZ-KEY.
           MOVE  W-FH1            TO  JNYZ-01.
      *           START  JNYZ   KEY  NOT <  JNYZ-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JNYZ_PNAME1 "JNYZ-KEY" " NOT < " JNYZ-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-EX
           END-IF.
       UPD3-010.
      *           READ  JNYZ   NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JNYZ_PNAME1 BY REFERENCE JNYZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-EX
           END-IF
           MOVE  9            TO  W-KURA.
           MOVE  JNYZ-01      TO  W-HCD.
           MOVE  JNYZ-02      TO  W-SIZ.
           MOVE  W-HCD        TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD3-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD3-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   >  19
                   GO  TO  UPD3-010
               END-IF
           END-IF
      *
           IF  JNYZ-01   >  W-TH
               GO  TO  UPD3-EX
           END-IF
           IF  JNYZ-01   >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD3-020
           END-IF
           IF  JNYZ-01   >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD3-020
           END-IF
           IF  JNYZ-01   >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD3-020
           END-IF
           IF  JNYZ-01   >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD3-020
           END-IF
           IF  JNYZ-01   >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD3-020
           END-IF
           GO  TO  UPD3-010.
      *
       UPD3-020.
           MOVE  9             TO  NJZAI-01.
           MOVE  W-HCD         TO  NJZAI-02.
           MOVE  W-SIZ         TO  NJZAI-03.
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
           PERFORM  ZC5-RTN      THRU  ZC5-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           GO  TO  UPD3-010.
       UPD3-EX.
           EXIT.
      ******************************
      ***   UPD4  RTN            ***
      ******************************
       UPD4-RTN.
           INITIALIZE                 HI-KEY2.
           MOVE  W-FH1            TO  HI-MHCD HI-HCD.
      *           START  HI2-M  KEY  NOT <  HI-KEY2    INVALID
      *///////////////
           CALL "DB_Start" USING
            HI2-M_PNAME1 "HI-KEY2" " NOT < " HI-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  UPD4-EX
           END-IF.
       UPD4-010.
      *           READ  HI2-M  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD4-EX
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3   <  30  OR   >  39
                   GO  TO  UPD4-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3   <  20  OR   >  29
                   GO  TO  UPD4-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3   >  19
                   GO  TO  UPD4-010
               END-IF
           END-IF
           MOVE  9            TO  W-KURA.
           MOVE  HI-MHCD      TO  W-HCD.
           MOVE  ZERO         TO  W-SIZ   HI-S4(10).
      *
           IF  HI-MHCD   >  W-TH
               GO  TO  UPD4-EX
           END-IF
           IF  HI-MHCD   >= W-FH1 AND   <=  W-TH1
               GO  TO  UPD4-020
           END-IF
           IF  HI-MHCD   >= W-FH2 AND   <=  W-TH2
               GO  TO  UPD4-020
           END-IF
           IF  HI-MHCD   >= W-FH3 AND   <=  W-TH3
               GO  TO  UPD4-020
           END-IF
           IF  HI-MHCD   >= W-FH4 AND   <=  W-TH4
               GO  TO  UPD4-020
           END-IF
           IF  HI-MHCD   >= W-FH5 AND   <=  W-TH5
               GO  TO  UPD4-020
           END-IF
           GO  TO  UPD4-010.
       UPD4-020.
           ADD   1            TO  W-SIZ.
           IF  W-SIZ          =  5
               GO  TO  UPD4-010
           END-IF
           IF  W-SIZ          =  1
               IF  HI-SS1        =  ZERO
                   GO  TO  UPD4-020
               END-IF
           END-IF
           IF  W-SIZ          =  2
               IF  HI-SS2        =  ZERO
                   GO  TO  UPD4-020
               END-IF
           END-IF
           IF  W-SIZ          =  3
               IF  HI-SS3        =  ZERO
                   GO  TO  UPD4-020
               END-IF
           END-IF
           IF  W-SIZ          =  4
               IF  HI-SS4        =  ZERO
                   GO  TO  UPD4-020
               END-IF
           END-IF
      *
           MOVE  9             TO  NJZAI-01.
           MOVE  W-HCD         TO  NJZAI-02.
           MOVE  W-SIZ         TO  NJZAI-03.
      *           READ  NJZAI           UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO        TO  YZAI-R
               GO  TO  UPD4-025
           END-IF
           MOVE  0                TO  DATA-SW.
           PERFORM  SET-RTN      THRU  SET-EX.
      *
       UPD4-025.
           INITIALIZE                 JMST3-KEY.
           MOVE  W-HCD            TO  JMST3-03.
      *           START  JMST3  KEY  NOT <  JMST3-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD4-040
           END-IF.
       UPD4-030.
      *           READ  JMST3  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD4-040
           END-IF
           IF  W-HCD          NOT =  JMST3-03
               GO  TO  UPD4-040
           END-IF
           IF  W-SIZ          NOT =  JMST3-09
               GO  TO  UPD4-030
           END-IF
           IF  JMST3-01           =  0
               GO  TO  UPD4-030
           END-IF
           MOVE  1                TO  DATA-SW.
           PERFORM  SET-RTN      THRU  SET-EX.
           GO  TO  UPD4-030.
       UPD4-040.
           MOVE  W-HCD        TO  JNYZ-01.
           MOVE  W-SIZ        TO  JNYZ-02.
      *           READ  JNYZ UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JNYZ_PNAME1 BY REFERENCE JNYZ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD4-050
           END-IF
           MOVE  9             TO  NJZAI-01.
           MOVE  W-HCD         TO  NJZAI-02.
           MOVE  W-SIZ         TO  NJZAI-03.
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
           MOVE  2                TO  DATA-SW.
           PERFORM  SET-RTN      THRU  SET-EX.
       UPD4-050.
           PERFORM  ZC8-RTN      THRU  ZC8-EX.
           IF  ZERO-SW  =  1
               PERFORM WRI-RTN THRU WRI-EX
           END-IF
           GO  TO  UPD4-020.
       UPD4-EX.
           EXIT.
      *•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
      ********************************
      *    ëOåéécÅ@ÉZÉbÉgÅ@          *
      ********************************
       ZC0-RTN.
           MOVE  ZERO  TO  YZAI-R.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC0-010.
           IF  I  >  10
               GO  TO  ZC0-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  NJZAI-0411(I)  -  NJZAI-0511(I)
                                  +  NJZAI-0611(I)  +  NJZAI-1111(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE  0     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC0-010.
       ZC0-EX.
           EXIT.
      ********************************
      *    ì¸å…ÉZÉbÉgÅ@ Å@Å@         *
      ********************************
       ZC1-RTN.
           MOVE  ZERO  TO  YZAI-R.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC1-010.
           IF  I  >  10
               GO  TO  ZC1-EX
           END-IF
           MOVE  NJZAI-0711(I)  TO  YZAI-0411(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE  1     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC1-010.
       ZC1-EX.
           EXIT.
      ********************************
      *    èoå…ÉZÉbÉgÅ@ Å@Å@         *
      ********************************
       ZC2-RTN.
           MOVE  ZERO  TO  YZAI-R.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC2-010.
           IF  I  >  10
               GO  TO  ZC2-EX
           END-IF
           MOVE  NJZAI-0811(I)  TO  YZAI-0411(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE  2     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC2-010.
       ZC2-EX.
           EXIT.
      ********************************
      *    éwê}ÉZÉbÉgÅ@ Å@Å@         *
      ********************************
       ZC3-RTN.
           MOVE  ZERO  TO  YZAI-R.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC3-010.
           IF  I  >  10
               GO  TO  ZC3-EX
           END-IF
           MOVE  NJZAI-0911(I)  TO  YZAI-0411(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE  3     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC3-010.
       ZC3-EX.
           EXIT.
      ********************************
      *    é¿ç›å…ÉZÉbÉgÅ@ Å@         *
      ********************************
       ZC4-RTN.
           MOVE  ZERO  TO  YZAI-R.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC4-010.
           IF  I  >  10
               GO  TO  ZC4-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  NJZAI-0411(I)  -  NJZAI-0511(I)
                                  +  NJZAI-0611(I)  +  NJZAI-1111(I)
                                  +  NJZAI-0711(I)  -  NJZAI-0811(I)
                                  -  NJZAI-0911(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE  4     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC4-010.
       ZC4-EX.
           EXIT.
      ********************************
      *    ì¸ó\íËÉZÉbÉgÅ@ Å@         *
      ********************************
       ZC5-RTN.
           MOVE  ZERO  TO  YZAI-R.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC5-010.
           IF  I  >  10
               GO  TO  ZC5-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  JNYZ-0311(I)  -  NJZAI-0711(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE  5     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC5-010.
       ZC5-EX.
           EXIT.
      ********************************
      *    óaÇËÅEéÊÇËÇØÅEéÛíçÉZÉbÉg  *
      ********************************
       ZC6-RTN.
           MOVE  ZERO  TO  YZAI-R.
           IF  JMST3-01    =  0
               MOVE  9     TO  YZAI-90
           END-IF
           IF  JMST3-01    =  5
               MOVE  6     TO  YZAI-90
           END-IF
           IF  JMST3-01    =  6
               MOVE  7     TO  YZAI-90
           END-IF
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC6-010.
           IF  I  >  10
               GO  TO  ZC6-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  JMST3-1111(I)  -  JMST3-151(I)
                                  -  JMST3-141(I)   -  JMST3-1211(I).
           IF  (ZERO-SW   = 0)  AND  (YZAI-0411(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC6-010.
       ZC6-EX.
           EXIT.
      ********************************
      *    óLå¯ç›å…ÉZÉbÉg Å@Å@Å@Å@Å@ *
      ********************************
       ZC8-RTN.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC8-010.
           IF  I  >  10
               GO  TO  ZC8-EX
           END-IF
           IF  YZAI-0411(I)  NOT =  ZERO
               MOVE  8     TO  YZAI-90
               MOVE   1     TO  ZERO-SW
               GO  TO  ZC8-EX
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC8-010.
       ZC8-EX.
           EXIT.
      ********************************
      *    óLå¯ç›å…Å@åvéZ Å@Å@Å@Å@Å@ *
      ********************************
       SET-RTN.
           MOVE  1     TO  I.
           IF  DATA-SW         =  1
               GO  TO  SET-110
           END-IF
           IF  DATA-SW         =  2
               GO  TO  SET-210
           END-IF
           MOVE  ZERO  TO  YZAI-R.
       SET-010.
           IF  I  >  10
               GO  TO  SET-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  NJZAI-0411(I)  -  NJZAI-0511(I)
                                  +  NJZAI-0611(I)  +  NJZAI-1111(I)
                                  +  NJZAI-0711(I)  -  NJZAI-0811(I)
                                  -  NJZAI-0911(I).
           ADD  1     TO  I.
           GO  TO  SET-010.
       SET-110.
           IF  I  >  10
               GO  TO  SET-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  YZAI-0411(I)  -
                                    (JMST3-1111(I)  -  JMST3-151(I)
                                  -  JMST3-141(I)   -  JMST3-1211(I)).
           ADD  1     TO  I.
           GO  TO  SET-110.
       SET-210.
           IF  I  >  10
               GO  TO  SET-EX
           END-IF
           COMPUTE  YZAI-0411(I)  =  YZAI-0411(I)  +
                                    (JNYZ-0311(I)  -  NJZAI-0711(I)).
           ADD  1     TO  I.
           GO  TO  SET-210.
       SET-EX.
           EXIT.
      ************************************
      ***  ÇiÇsÅ|ÇxÇyÇ`ÇhÅ@ÇvÇqÇhÇsÇd  ***
      ************************************
       WRI-RTN.
           MOVE  W-KURA      TO  YZAI-01.
           MOVE  W-HCD       TO  YZAI-02.
           MOVE  W-SIZ       TO  YZAI-03.
           MOVE  W-ZCHK      TO  YZAI-88.
           IF  W-SIZ         =  1
               MOVE  4          TO  YZAI-89
           END-IF
           IF  W-SIZ         =  2
               MOVE  1          TO  YZAI-89
           END-IF
           IF  W-SIZ         =  3
               MOVE  2          TO  YZAI-89
           END-IF
           IF  W-SIZ         =  4
               MOVE  3          TO  YZAI-89
           END-IF
      *           WRITE    YZAI-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-YZAI_PNAME1 JT-YZAI_LNAME YZAI-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  "W"          TO  ERR-M
               MOVE  "WK0128"     TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      ***
           COPY    LPMSG.
