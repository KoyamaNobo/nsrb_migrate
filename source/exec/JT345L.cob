       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT340L.
       AUTHOR.          I.N.
      *********************************************************
      *    PROGRAM         :  受注・預り・取よけ台帳          *
      *                    :  JS-SIGN   0=当月  1=前月        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ***                             *
      *    DATA WRITTN     :  92/05/12                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM150.
       OBJECT-COMPUTER. NEAC-SYSTEM150.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT                 PIC X(2).
       01  JS-SIGN                  PIC 9(01).
       01  RED-SW                   PIC 9(01).
       01  END-SW                   PIC 9(01)  VALUE  0.
       01  CHK-SW                   PIC 9(01).
       01  MID-SW                   PIC 9(01).
       01  W-15K                    PIC X(05)  VALUE  X"1A24212078".
       01  W-20K                    PIC X(05)  VALUE  X"1A24212474".
       01  WK0128ID                 PIC X(09).
       01  STN-NO.
           02  STN-NO1              PIC X(03).
           02  STN-NO2              PIC X(03).
       01  W-FID.
           02  W-FID1               PIC X(06)  VALUE  "WK0128".
           02  W-FID2               PIC X(03).
       01  LCNT                     PIC 9(02).
       01  PCNT                     PIC 9(04).
       01  S-AREA.
           02  S-TUKI               PIC 9(02).
           02  S-SEN                PIC 9(01).
           02  S-KBN                PIC 9(01).
           02  S-HINF               PIC 9(06).
           02  S-HINT               PIC 9(06).
           02  S-TOKF               PIC 9(04).
           02  S-TOKT               PIC 9(04).
           02  S-OKC                PIC 9(01).
       01  W-AREA.
           02  I                    PIC 9(02).
           02  P                    PIC 9(02).
           02  SYS-DATE.
               03  SYS-YY           PIC 9(02).
               03  SYS-MM           PIC 9(02).
               03  SYS-DD           PIC 9(02).
           02  W-TUKI               PIC Z9.
           02  NEW-KEY.
               03  NEW-KEY1.
                   04  NEW-KEY2.
                       05  NEW-HCD  PIC 9(06).
                   04  NEW-TCD      PIC 9(04).
                   04  NEW-CCD      PIC 9(03).
               03  NEW-JNO.
                   04  NEW-JNO1     PIC 9(06).
                   04  NEW-JNO2     PIC 9(01).
           02  OLD-KEY.
               03  OLD-KEY1.
                   04  OLD-KEY2.
                       05  OLD-HCD  PIC 9(06).
                   04  OLD-TCD      PIC 9(04).
                   04  OLD-CCD      PIC 9(03).
               03  OLD-JNO.
                   04  OLD-JNO1     PIC 9(06).
                   04  OLD-JNO2     PIC 9(01).
           02  N-SNO                PIC 9(06).
           02  O-SNO                PIC 9(06).
       01  W-MEISAI.
           02  W-SIZ                PIC 9(01).
           02  W-ZEN.
               03  W-ZKEI-T       OCCURS  10.
                   04  W-ZSU        PIC S9(06).
               03  W-ZKEI           PIC S9(08).
               03  W-ZSET           PIC S9(04).
           02  W-KEI1.
               03  W-KEI1-T       OCCURS  10.
                   04  W-KSU1       PIC S9(06).
               03  W-KKEI1          PIC S9(08).
               03  W-KSET1          PIC S9(04).
           02  W-KEI2.
               03  W-KEI2-T       OCCURS  10.
                   04  W-KSU2       PIC S9(06).
               03  W-KKEI2          PIC S9(08).
               03  W-KSET2          PIC S9(04).
           02  W-ZAN.
               03  W-XKEI-T       OCCURS  10.
                   04  W-XSU        PIC S9(06).
               03  W-XKEI           PIC S9(08).
               03  W-XSET           PIC S9(04).
           02  W-MSW                PIC 9(01).
       01  W-SYOUKEI.
           02  WS-TBL             OCCURS  4.
               03  WS-SIZ           PIC 9(01).
               03  WS-ZEN.
                   04  WS-ZKEI-T      OCCURS  10.
                       05  WS-ZSU   PIC S9(06).
                   04  WS-ZKEI      PIC S9(08).
                   04  WS-ZSET      PIC S9(04).
               03  WS-KEI1.
                   04  WS-KEI1-T      OCCURS  10.
                       05  WS-KSU1  PIC S9(06).
                   04  WS-KKEI1     PIC S9(08).
                   04  WS-KSET1     PIC S9(04).
               03  WS-KEI2.
                   04  WS-KEI2-T      OCCURS  10.
                       05  WS-KSU2  PIC S9(06).
                   04  WS-KKEI2     PIC S9(08).
                   04  WS-KSET2     PIC S9(04).
               03  WS-ZAN.
                   04  WS-XKEI-T      OCCURS  10.
                       05  WS-XSU   PIC S9(06).
                   04  WS-XKEI      PIC S9(08).
                   04  WS-XSET      PIC S9(04).
               03  WS-MSW           PIC 9(01).
           02  WS-SSW               PIC 9(01).
       01  W-GOUKEI.
           02  WG-TBL             OCCURS  4.
               03  WG-SIZ           PIC 9(01).
               03  WG-ZEN.
                   04  WG-ZKEI-T      OCCURS  10.
                       05  WG-ZSU   PIC S9(06).
                   04  WG-ZKEI      PIC S9(08).
                   04  WG-ZSET      PIC S9(04).
               03  WG-KEI1.
                   04  WG-KEI1-T      OCCURS  10.
                       05  WG-KSU1  PIC S9(06).
                   04  WG-KKEI1     PIC S9(08).
                   04  WG-KSET1     PIC S9(04).
               03  WG-KEI2.
                   04  WG-KEI2-T      OCCURS  10.
                       05  WG-KSU2  PIC S9(06).
                   04  WG-KKEI2     PIC S9(08).
                   04  WG-KSET2     PIC S9(04).
               03  WG-ZAN.
                   04  WG-XKEI-T      OCCURS  10.
                       05  WG-XSU   PIC S9(06).
                   04  WG-XKEI      PIC S9(08).
                   04  WG-XSET      PIC S9(04).
               03  WG-MSW           PIC 9(01).
       01  PRN-WORK.
           02  W-HIZ.
               03  W-MM             PIC 99.
               03  FILLER           PIC X(01)  VALUE  "/".
               03  W-DD             PIC 99.
      *************
      ***    ﾐﾀﾞｼ
      ****************
       01  HEAD1.
           02  F                    PIC X(05) VALUE X"1A24212474".
           02  F                    PIC X(29) VALUE SPACE.
           02  M-TUKI               PIC N(2).
           02  F                    PIC N(2)  VALUE  "月度".
           02  F                    PIC X(6)  VALUE  SPACE.
           02  M-MID                PIC N(10).
           02  F                    PIC X(06) VALUE  SPACE.
           02  M-SEN                PIC N(05).
           02  F                    PIC X(20) VALUE  SPACE.
           02  M-YY                 PIC 9(02).
           02  F                    PIC N(1) VALUE "年".
           02  M-MM                 PIC Z9.
           02  F                    PIC N(1) VALUE "月".
           02  M-DD                 PIC Z9.
           02  F                    PIC N(1) VALUE "日".
           02  F                    PIC X(10) VALUE SPACE.
           02  F                    PIC X(05)  VALUE  "PAGE.".
           02  WPCNT                PIC ZZZ9.
       01  HEAD2.
           02  F                    PIC X(07)  VALUE  " ｺｰﾄﾞ  ".
           02  F                    PIC N(07) VALUE "品　　　　　名".
       01  HEAD3.
           02  F                    PIC X(11)  VALUE  "    ｺｰﾄﾞ   ".
           02  F                    PIC N(07) VALUE "得　意　先　名".
           02  F                    PIC X(28)  VALUE  SPACE.
           02  F                    PIC N(07) VALUE "直　送　先　名".
       01  HEAD4.
           02  F                    PIC X(15) VALUE SPACE.
           02  F                    PIC N(02) VALUE "区分".
           02  F                    PIC X(02) VALUE SPACE.
           02  F                    PIC N(02) VALUE "月日".
           02  F                    PIC X(01) VALUE SPACE.
           02  F                    PIC N(04) VALUE "受注№行".
           02  F                    PIC X(01) VALUE SPACE.
           02  F                    PIC X(01) VALUE "1".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "３号".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "２号".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "１号".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "０号".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "　中".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "　大".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "特大".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "28.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "29.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "30.0".
           02  F                    PIC X(09) VALUE SPACE.
           02  F                    PIC X(03) VALUE "ｾｯﾄ".
           02  F                    PIC N(01) VALUE "数".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC N(02) VALUE "単価".
           02  F                    PIC X(09) VALUE SPACE.
       01  HEAD5.
           02  F                    PIC X(35) VALUE SPACE.
           02  F                    PIC X(01) VALUE "2".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "12.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "13.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "13.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "14.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "15.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "16.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "17.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "18.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "19.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "20.0".
           02  F                    PIC X(09) VALUE SPACE.
           02  F                    PIC N(06) VALUE "出荷直送先名".
           02  F                    PIC X(09) VALUE SPACE.
       01  HEAD6.
           02  F                    PIC X(35) VALUE SPACE.
           02  F                    PIC X(01) VALUE "3".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "21.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "21.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "22.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "22.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "23.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "23.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "24.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "24.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "25.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "    ".
           02  F                    PIC X(30) VALUE SPACE.
       01  HEAD7.
           02  F                    PIC X(35) VALUE SPACE.
           02  F                    PIC X(01) VALUE "4".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "24.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "24.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "25.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "25.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "26.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "26.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "27.0".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "27.5".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "    ".
           02  F                    PIC X(03) VALUE SPACE.
           02  F                    PIC X(04) VALUE "    ".
           02  F                    PIC X(04) VALUE SPACE.
           02  F                    PIC N(02) VALUE "合計".
           02  F                    PIC X(22) VALUE SPACE.
       01  HEAD8.
           02  F                    PIC X(35) VALUE  SPACE.
           02  F                    PIC X(01) VALUE  "(".
           02  F                    PIC X(14) VALUE  SPACE.
           02  F                    PIC N(07) VALUE "出　荷　品　名".
           02  F                    PIC X(15) VALUE  SPACE.
           02  F                    PIC X(01) VALUE  ")".
           02  F                    PIC X(56) VALUE  SPACE.
      *****
       COPY  LWMSG.
      *****
           COPY   LTWK05.                                               
           COPY   LIHIM2.
           COPY   LITCM.
      ***
      *FD  P-F
       01  P-R.
           02  P1-R.
               03  P1-15K               PIC X(05).
               03  P1-HCD               PIC 9(06).
               03  F                    PIC X(01).
               03  P1-HNA               PIC N(24).
               03  P1-20K               PIC X(05).
               03  F                    PIC X(105).
           02  P2-R   REDEFINES P1-R.
               03  P2-15K               PIC X(05).
               03  F                    PIC X(02).
               03  P2-TCD               PIC 9(04).
               03  P2-V                 PIC X(01).
               03  P2-CCD               PIC 9(03).
               03  F                    PIC X(01).
               03  P2-TNA               PIC N(26).
               03  F                    PIC X(01).
               03  P2-CNA               PIC N(26).
               03  P2-20K               PIC X(05).
               03  F                    PIC X(44).
           02  P3-R   REDEFINES P1-R.
               03  P3-15K               PIC X(05).
               03  F                    PIC X(14).
               03  P3-FD                PIC X(01).
               03  P3-MID               PIC N(02).
               03  P3-RD                PIC X(01).
               03  F                    PIC X(01).
               03  P3-20K               PIC X(05).
               03  P3-KEIM              PIC N(07).
               03  P3-MEID  REDEFINES P3-KEIM.
                 04  P3-HIZ             PIC 99/99.
                 04  F                  PIC X(01).
                 04  P3-JNO             PIC 9(06).
                 04  P3-JX              PIC X(01).
                 04  P3-JGYO            PIC 9(01).
               03  F                    PIC X(01).
               03  P3-SIZ               PIC 9(01).
               03  P3-SUR         OCCURS  10.
                   04  P3-SUR1          PIC ---,---.
               03  P3-KEI               PIC ----,--9.
               03  F                    PIC X(01).
               03  P3-SET               PIC Z(05).
               03  F                    PIC X(01).
               03  P3-TAN               PIC ZZ,ZZZ.
               03  F                    PIC X(09).
               03  F                    PIC X(23).
           02  P4-R   REDEFINES P1-R.
               03  P4-15K1              PIC X(05).
               03  F                    PIC X(14).
               03  P4-FD                PIC X(01).
               03  P4-MID               PIC N(02).
               03  P4-RD                PIC X(01).
               03  F                    PIC X(01).
               03  P4-20K1              PIC X(05).
               03  P4-KEIM              PIC N(07).
               03  P4-MEID  REDEFINES P4-KEIM.
                 04  P4-HIZ             PIC 99/99.
                 04  F                  PIC X(01).
                 04  P4-JNO             PIC 9(06).
                 04  P4-JX              PIC X(01).
                 04  P4-JGYO            PIC 9(01).
               03  F                    PIC X(01).
               03  P4-SIZ               PIC 9(01).
               03  P4-SUR         OCCURS  10.
                   04  P4-SUR1          PIC ---,---.
               03  P4-KEI               PIC ----,--9.
               03  F                    PIC X(01).
               03  P4-15K2              PIC X(05).
               03  P4-CNA               PIC N(14).
               03  P4-20K2              PIC X(05).
               03  F                    PIC X(06).
           02  P5-R   REDEFINES P1-R.
               03  P5-15K               PIC X(05).
               03  F                    PIC X(35).
               03  P5-FD                PIC X(01).
               03  P5-HCD               PIC 9(06).
               03  F                    PIC X(01).
               03  P5-HNA               PIC N(24).
               03  P5-RD                PIC X(01).
               03  P5-20K               PIC X(05).
               03  F                    PIC X(68).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER  PIC X(12)  VALUE
                                    "            ".
           02  FILLER  PIC N(05)  VALUE  "受注台帳他".
           02  DSP-TUKI  PIC  Z9 .
           02  FILLER  PIC X(04)  VALUE  "月度".
           02  FILLER  PIC X(22)  VALUE  "教　育=0 , ワーク=1 , ".
           02  FILLER  PIC X(25)  VALUE  "一　般=2 , 全　件=9 ...  ".
           02  FILLER  PIC X(10)  VALUE  "受　注 = 0".
           02  FILLER  PIC X(10)  VALUE  "預　り = 5".
           02  FILLER  PIC X(18)  VALUE  "取よけ = 6 .....  ".
           02  FILLER  PIC X(20)  VALUE  "確認（OK=1,NO=9）-->".
           02  FILLER  PIC X(04)  VALUE  "ﾘﾀｰﾝ".
       01  DSP-AREA1.
           02  FILLER  PIC X(14)  VALUE  "品名C  得意先C".
           02  FILLER  PIC X(08)  VALUE  "ＦＲＯＭ".
           02  FILLER  PIC X(04)  VALUE  "ＴＯ".
       01  ACP-AREA.
           02  ACP-SEN  PIC 9(01).
           02  ACP-KBN  PIC 9(01).
           02  FILLER.
               03  ACP-HINF  PIC 9(06).
               03  ACP-TOKF  PIC 9(04).
           02  FILLER.
               03  ACP-HINT  PIC 9(06).
               03  ACP-TOKT  PIC 9(04).
           02  ACP-OKC  PIC 9(01).
       01  CLR-AREA.
           02  FILLER  PIC X(01)  VALUE  " ".
           02  FILLER  PIC X(01)  VALUE  " ".
           02  FILLER.
               03  FILLER  PIC X(06)  VALUE  "      ".
               03  FILLER  PIC X(04)  VALUE  "    ".
           02  FILLER.
               03  FILLER  PIC X(06)  VALUE  "      ".
               03  FILLER  PIC X(04)  VALUE  "    ".
           02  FILLER  PIC X(01)  VALUE  " ".
       01  DSP-ERR.
           02  ERR-MSG1   PIC  N(06)  VALUE
                "品名　なし".
      *****
       COPY  LSMSG.
      *****
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "137" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "26" "12" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "N" "1" "27" "10" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TUKI" "Z9" "6" "31" "2" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TUKI" BY REFERENCE S-TUKI "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "6" "34" "4" "DSP-TUKI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "8" "10" "22" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "8" "32" "25" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "11" "23" "10" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "13" "23" "10" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "15" "23" "18" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-AREA" "X" "23" "41" "20" "09DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-AREA" "X" "23" "62" "4" "10DSP-AREA" " "
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "26" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA1" "X" "11" "56" "14" " " "DSP-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA1" "X" "13" "46" "8" "01DSP-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA1" "X" "15" "46" "4" "02DSP-AREA1" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "23" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "8" "56" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-SEN" BY REFERENCE S-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KBN" "9" "15" "40" "1" "ACP-SEN" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-KBN" BY REFERENCE S-KBN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACP-AREA" " " "13" "0" "10" "ACP-KBN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-HINF" "9" "13" "56" "6" " " "03ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-HINF" BY REFERENCE S-HINF "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TOKF" "9" "13" "64" "4" "ACP-HINF" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TOKF" BY REFERENCE S-TOKF "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ACP-AREA" " " "15" "0" "10" "03ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-HINT" "9" "15" "56" "6" " " "04ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-HINT" BY REFERENCE S-HINT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TOKT" "9" "15" "64" "4" "ACP-HINT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TOKT" BY REFERENCE S-TOKT "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "04ACP-AREA" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OKC" BY REFERENCE S-OKC "1" "0" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING 
            "CLR-AREA" " " "0" "0" "23" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-AREA" "X" "8" "56" "1" " " "CLR-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-AREA" "X" "15" "40" "1" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-AREA" " " "13" "0" "10" "02CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103CLR-AREA" "X" "13" "56" "6" " " "03CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203CLR-AREA" "X" "13" "64" "4" "0103CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-AREA" " " "15" "0" "10" "03CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104CLR-AREA" "X" "15" "56" "6" " " "04CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204CLR-AREA" "X" "15" "64" "4" "0104CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-AREA" "X" "23" "61" "1" "04CLR-AREA" " "
            RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "24" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" "N" "24" "1" "12" " " "DSP-ERR" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
       PRO-005.
           ACCEPT    JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN          >  1
               GO  TO  PRO-005
           END-IF.
       PRO-010.
           PERFORM  INT-RTN    THRU  INT-EX.
           PERFORM  MAIN-RTN   THRU  MAIN-EX
                    UNTIL      NEW-KEY        =  HIGH-VALUE.
           PERFORM  END-RTN    THRU  END-EX.
           IF  END-SW          =  0
               IF  S-SEN      NOT  =  9
                   GO  TO  PRO-010
               END-IF
           END-IF
           CALL "DB_Close".
           STOP  RUN.
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
       INT-RTN.
           INITIALIZE          W-AREA.
           MOVE    0           TO  RED-SW.
           MOVE    90          TO  LCNT.
           MOVE    ZERO        TO  PCNT.
      *
           CALL  "CBLSTNNO"  USING   STN-NO USER_ID.
           MOVE    STN-NO2     TO  W-FID2.
           MOVE    W-FID       TO  WK0128ID.
           MOVE WK0128ID TO JT-WK05_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK05_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK05_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "PR_Open" RETURNING RESP.
      *
           PERFORM  RWK-RTN    THRU  RWK-EX.
           IF  NEW-KEY          =  HIGH-VALUE
               MOVE   9          TO  END-SW
               GO  TO  INT-EX
           END-IF
           MOVE  WK05-99       TO  S-TUKI  W-TUKI.
      ***  画面処理
           PERFORM  INP-RTN    THRU  INP-EX.
           IF  NEW-KEY          =  HIGH-VALUE
               MOVE   9          TO  END-SW
               GO  TO  INT-EX
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK05_IDLST JT-WK05_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK05_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK05_IDLST "0".
           PERFORM  RWK-RTN    THRU  RWK-EX.
           IF  NEW-KEY          =  HIGH-VALUE
               GO  TO  INT-EX
           END-IF
      *
           ACCEPT   SYS-DATE   FROM  DATE.
           MOVE  SYS-YY        TO  M-YY.
           MOVE  SYS-MM        TO  M-MM.
           MOVE  SYS-DD        TO  M-DD.
           MOVE  NEW-KEY       TO  OLD-KEY.
       INT-EX.
            EXIT.
      *********************************
      ***  画面処理　　　　         ***
      *********************************
       INP-RTN.
           CALL "SD_Output" USING "C-CL" C-CL "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           IF  JS-SIGN          =  0
               CALL "SD_Output" USING
                "DSP-AREA1" DSP-AREA1 "p" RETURNING RESU
           ELSE
               MOVE  ZERO       TO  S-HINF  S-TOKF
               MOVE  999999     TO  S-HINT
               MOVE  9999       TO  S-TOKT
           END-IF.
       INP-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "P9"
               MOVE  HIGH-VALUE    TO  NEW-KEY
               GO  TO  INP-EX
           END-IF
           IF  ESTAT       NOT  =  "01" AND "06"
               GO  TO  INP-005
           END-IF
           IF  S-SEN       NOT  =  0 AND 1 AND 2 AND 9
               GO  TO  INP-005
           END-IF.
       INP-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-KBN "ACP-KBN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "09"
               GO  TO  INP-005
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO  TO  INP-010
           END-IF
           IF  S-KBN        NOT =  0 AND 5 AND 6
               GO  TO  INP-010
           END-IF
           IF  JS-SIGN      NOT =  0
               GO  TO  INP-020
           END-IF.
       INP-012.
           CALL "SD_Accept" USING BY REFERENCE ACP-HINF "ACP-HINF"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "09"
               GO  TO  INP-010
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO  TO  INP-012
           END-IF.
       INP-014.
           CALL "SD_Accept" USING BY REFERENCE ACP-HINT "ACP-HINT"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "09"
               GO  TO  INP-012
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO  TO  INP-014
           END-IF
           IF  S-HINF           >  S-HINT
               GO  TO  INP-014
           END-IF.
       INP-016.
           CALL "SD_Accept" USING BY REFERENCE ACP-TOKF "ACP-TOKF"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "09"
               GO  TO  INP-014
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO  TO  INP-016
           END-IF.
       INP-018.
           CALL "SD_Accept" USING BY REFERENCE ACP-TOKT "ACP-TOKT"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "09"
               GO  TO  INP-016
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO  TO  INP-018
           END-IF
           IF  S-TOKF           >  S-TOKT 
               GO  TO  INP-018
           END-IF.
       INP-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT            =  "P9"
               MOVE  HIGH-VALUE    TO  NEW-KEY
               GO  TO  INP-EX
           END-IF
           IF  ESTAT            =  "09"
               IF  JS-SIGN      NOT =  0
                   GO  TO  INP-010
               ELSE
                   GO  TO  INP-018
               END-IF
           END-IF
           IF  ESTAT        NOT =  "01" AND "06"
               GO  TO  INP-020
           END-IF
           IF  S-OKC            =  9
               GO  TO  INP-005
           END-IF
           IF  S-OKC        NOT =  1
               GO  TO  INP-020
           END-IF
           IF  S-KBN            =  0
               MOVE  "受　　注　　台　　帳"  TO  M-MID
           END-IF
           IF  S-KBN            =  5
               MOVE  "預　　り　　台　　帳"  TO  M-MID
           END-IF
           IF  S-KBN            =  6
               MOVE  "取　よ　け　台　帳　"  TO  M-MID
           END-IF.
       INP-EX.
           EXIT.
      ******************************
      ***  メイン処理            ***
      ******************************
       MAIN-RTN.
           PERFORM  BREAK1-RTN THRU  BREAK1-EX
                    UNTIL      NEW-KEY      =  HIGH-VALUE.
       MAIN-EX.
           EXIT.
      ******************************
      ***  ＢＲＥＡＫ１－ＲＴＮ  ***
      ******************************
       BREAK1-RTN.
           INITIALIZE          W-GOUKEI.
           MOVE  NEW-KEY2      TO  OLD-KEY2.
           PERFORM  HNM-RTN    THRU  HNM-EX.
           PERFORM  BREAK2-RTN THRU  BREAK2-EX
                    UNTIL      NEW-KEY2  NOT =  OLD-KEY2.
      *
           PERFORM  GOK-RTN    THRU  GOK-EX.
       BREAK1-EX.
           EXIT.
      ******************************
      ***  ＢＲＥＡＫ２－ＲＴＮ  ***
      ******************************
       BREAK2-RTN.
           INITIALIZE          W-SYOUKEI.
           MOVE  NEW-KEY1      TO  OLD-KEY1.
           PERFORM  TOK-RTN    THRU  TOK-EX.
           PERFORM  BREAK3-RTN THRU  BREAK3-EX
                    UNTIL      NEW-KEY1 NOT =  OLD-KEY1.
      *
           IF  WS-SSW       NOT >  1
               GO  TO  BREAK2-EX
           END-IF
           PERFORM  SOK-RTN    THRU  SOK-EX.
       BREAK2-EX.
           EXIT.
      ******************************
      ***  ＢＲＥＡＫ３－ＲＴＮ  ***
      ******************************
       BREAK3-RTN.
           INITIALIZE          W-MEISAI.
           MOVE  NEW-KEY       TO  OLD-KEY.
           PERFORM  NOBREAK-RTN    THRU  NOBREAK-EX
                    UNTIL      NEW-KEY   NOT =  OLD-KEY.
      *
           IF  WS-SSW       NOT =  9
               ADD   1             TO  WS-SSW
           END-IF
           IF  W-MSW            =  ZERO
               GO  TO  BREAK3-010
           END-IF
           IF  LCNT         NOT <  60
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
               PERFORM  TOK-RTN    THRU  TOK-EX
           END-IF.
       BREAK3-010.
      ***  残数印字
           PERFORM  ZAN-RTN    THRU  ZAN-EX.
       BREAK3-EX.
           EXIT.
      ******************************
      ***  ＮＯＢＲＥＡＫ－ＲＴＮ***
      ******************************
       NOBREAK-RTN.
           IF  LCNT         NOT <  60
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
               PERFORM  TOK-RTN    THRU  TOK-EX
           END-IF
      *
      ***  明細印字
           IF  WK05-03          =  ZERO
               PERFORM  ZEN-RTN    THRU  ZEN-EX
           ELSE
               MOVE  1             TO  W-MSW
               PERFORM  MEI-RTN    THRU  MEI-EX
           END-IF
           MOVE  N-SNO         TO  O-SNO.
      ***  ワークファイル　リード
           PERFORM  RWK-RTN    THRU  RWK-EX.
       NOBREAK-EX.
           EXIT.
      *****************************
      ***    ﾐ ﾀﾞ ｼ  R T N      ***
      *****************************
      **
       HEAD-RTN.
           IF  LCNT             <  90
               MOVE  SPACE         TO  P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD   1             TO  PCNT.
           MOVE  PCNT          TO  WPCNT.
           MOVE  SYS-YY        TO  M-YY.
           MOVE  SYS-MM        TO  M-MM.
           MOVE  SYS-DD        TO  M-DD.
           MOVE  S-TUKI        TO  W-TUKI.
           MOVE  W-TUKI        TO  M-TUKI.
           MOVE  ALL "　"  TO  M-SEN.
           IF  S-SEN          =  0
               MOVE   "【教　育】"  TO  M-SEN
           END-IF
           IF  S-SEN          =  1
               MOVE   "【ワーク】"  TO  M-SEN
           END-IF
           IF  S-SEN          =  2
               MOVE   "【一　般】"  TO  M-SEN
           END-IF
           MOVE  HEAD1         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD2         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD3         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD4         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD5         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD6         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD7         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  HEAD8         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  9             TO  LCNT.
       HEAD-EX.
           EXIT.
      **************************
      **   前残印字処理       **
      **************************
       ZEN-RTN.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           MOVE  "<"           TO  P3-FD.
           MOVE  "前残"      TO  P3-MID.
           MOVE  ">"           TO  P3-RD.
           MOVE  WK05-962      TO  W-MM.
           MOVE  WK05-963      TO  W-DD.
           MOVE  W-HIZ         TO  P3-HIZ.
           MOVE  WK05-061      TO  P3-JNO.
           MOVE  "-"           TO  P3-JX.
           MOVE  WK05-062      TO  P3-JGYO.
           MOVE  WK05-97       TO  P3-TAN.
           MOVE  WK05-98       TO  P3-SET.
           MOVE  WK05-08       TO  P3-SIZ  W-SIZ.
           MOVE  W-SIZ         TO  P.
           PERFORM  ZSUR-RTN   THRU  ZSUR-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I             >  10.
           MOVE  WK05-092      TO  P3-KEI  W-ZKEI.
           ADD   WK05-092      TO  WS-ZKEI(P)  WG-ZKEI(P).
           MOVE  WK05-98       TO  W-ZSET.
           ADD   WK05-98       TO  WS-ZSET(P)  WG-ZSET(P).
           MOVE  W-SIZ         TO  WS-SIZ(P)  WG-SIZ(P).
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           ADD   1             TO  LCNT.
       ZEN-EX.
           EXIT.
      *********************************
      **   （前残）　数量印字処理     *
      *********************************
       ZSUR-RTN.
           MOVE  WK05-0911(I)  TO  P3-SUR1(I).
           ADD   WK05-0911(I)  TO  W-ZSU(I)  WS-ZSU(P , I)
                                             WG-ZSU(P , I).
       ZSUR-EX.
           EXIT.
      **************************
      **   前残印字処理       **
      **************************
       MEI-RTN.
           IF  N-SNO            =  O-SNO
               GO  TO  MEI-010
           END-IF
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P4-MID  P4-CNA.
           MOVE  "<"           TO  P4-FD.
           MOVE  ">"           TO  P4-RD.
           IF  WK05-10          =  0
               MOVE  "("           TO  P4-FD
               MOVE  ")"           TO  P4-RD
               MOVE  "出荷"      TO  P4-MID
           END-IF
           IF  WK05-10          =  3
               MOVE  "｢"           TO  P4-FD
               MOVE  "｣"           TO  P4-RD
               MOVE  "訂正"      TO  P4-MID
           END-IF
           MOVE  WK05-032      TO  W-MM.
           MOVE  WK05-033      TO  W-DD.
           MOVE  W-HIZ         TO  P4-HIZ.
           IF  WK05-042         =  WK05-94
                 MOVE  SPACE       TO  TC-NAME
                 GO  TO  MEI-005
           END-IF
           MOVE  WK05-041      TO  TC-TCD.
           MOVE  WK05-94       TO  TC-CCD.
      *           READ  TC-M          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE       TO  TC-NAME
           END-IF.
       MEI-005.
           MOVE  TC-NAME       TO  P4-CNA.
       MEI-010.
           MOVE  WK05-08       TO  P4-SIZ.
           PERFORM  MSUR-RTN   THRU  MSUR-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I             >  10.
           MOVE  WK05-092      TO  P4-KEI.
           IF  WK05-10          =  0
               ADD   WK05-092      TO  W-KKEI1
               ADD   WK05-092      TO  WS-KKEI1(P)  WG-KKEI1(P)
               ADD   WK05-98       TO  W-KSET1 WS-KSET1(P)  WG-KSET1(P)
           ELSE
               ADD   WK05-092      TO  W-KKEI2
               ADD   WK05-092      TO  WS-KKEI2(P)  WG-KKEI1(P)
               ADD   WK05-98       TO  W-KSET2 WS-KSET2(P)  WG-KSET2(P)
           END-IF
      *
           MOVE  W-15K         TO  P4-15K1  P4-15K2.
           MOVE  W-20K         TO  P4-20K1  P4-20K2.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           ADD   1             TO  LCNT.
           IF  WK05-95          =  WK05-07
               GO  TO  MEI-EX
           END-IF
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P5-HNA.
           MOVE  "("           TO  P5-FD.
           MOVE  ")"           TO  P5-RD.
           MOVE  WK05-07       TO  P5-HCD.
           MOVE  WK05-07       TO  HI-MHCD HI-HCD.
      *           READ  HI2-M         UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE         TO  HI-NAME
           END-IF
           MOVE  HI-NAME           TO  P5-HNA.
           MOVE  W-15K         TO  P5-15K.
           MOVE  W-20K         TO  P5-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           ADD   1             TO  LCNT.
       MEI-EX.
           EXIT.
      *********************************
      **   （明細）　数量印字処理     *
      *********************************
       MSUR-RTN.
           MOVE  WK05-0911(I)  TO  P4-SUR1(I).
           IF  WK05-10          =  0
               ADD   WK05-0911(I)  TO  W-KSU1(I)  WS-KSU1(P , I)
                                                  WG-KSU1(P , I)
           ELSE
               ADD   WK05-0911(I)  TO  W-KSU2(I)  WS-KSU2(P , I)
                                                  WG-KSU2(P , I)
           END-IF.
       MSUR-EX.
           EXIT.
      **************************
      **   残数印字処理       **
      **************************
       ZAN-RTN.
           IF  W-MSW          =  1
               MOVE  SPACE         TO  P-R
               MOVE  SPACE         TO  P3-MID
               MOVE  "["           TO  P3-FD
               MOVE  "残数"      TO  P3-MID
               MOVE  "]"           TO  P3-RD
               MOVE  W-SIZ         TO  P3-SIZ
           END-IF
           PERFORM  XSUR-RTN   THRU  XSUR-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I             >  10.
           COMPUTE  W-XKEI      =  W-ZKEI - W-KKEI1 - W-KKEI2.
           COMPUTE  W-XSET      =  W-ZSET - W-KSET1 - W-KSET2.
           IF  W-MSW          =  1
               MOVE  W-XKEI        TO  P3-KEI
               MOVE  W-XSET        TO  P3-SET
           END-IF
           ADD   W-XKEI        TO  WS-XKEI(P)  WG-XKEI(P).
           ADD   W-XSET        TO  WS-XSET(P)  WG-XSET(P).
           IF  W-MSW          =  ZERO
               GO  TO  ZAN-EX
           END-IF
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           ADD   1             TO  LCNT.
       ZAN-EX.
           EXIT.
      *********************************
      **   （前残）　数量印字処理     *
      *********************************
       XSUR-RTN.
           COMPUTE  W-XSU(I)    =  W-ZSU(I)  -  W-KSU1(I)  -  W-KSU2(I).
           IF  W-MSW          =  1
               MOVE  W-XSU(I)      TO  P3-SUR1(I)
           END-IF
           ADD   W-XSU(I)      TO  WS-XSU(P , I)  WG-XSU(P , I).
       XSUR-EX.
           EXIT.
      **************************
      **   小計印字処理       **
      **************************
       SOK-RTN.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           MOVE  "　〔　小計　〕"      TO  P3-KEIM.
           MOVE  "<"           TO  P3-FD.
           MOVE  "前残"      TO  P3-MID.
           MOVE  ">"           TO  P3-RD.
           MOVE  1             TO  P.
       SOK-010.
           IF  WS-SIZ(P)       =   ZERO
               GO  TO  SOK-020
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
               PERFORM  TOK-RTN    THRU  TOK-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "　〔　小計　〕"      TO  P3-KEIM
               MOVE  "<"           TO  P3-FD
               MOVE  "前残"      TO  P3-MID
               MOVE  ">"           TO  P3-RD
           END-IF
           MOVE  WS-SIZ(P)     TO  P3-SIZ.
           PERFORM  SZEN-RTN   THRU  SZEN-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WS-ZKEI(P)    TO  P3-KEI.
           MOVE  WS-ZSET(P)    TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  0             TO  MID-SW.
       SOK-020.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  SOK-010
           END-IF
           MOVE  "("           TO  P3-FD.
           MOVE  "出荷"      TO  P3-MID.
           MOVE  ")"           TO  P3-RD.
           MOVE  1             TO  P.
       SOK-030.
           IF  WS-SIZ(P)       =   ZERO
               GO  TO  SOK-040
           END-IF
           MOVE  0             TO  CHK-SW.
           PERFORM  CHK1-RTN   THRU  CHK1-EX
                 VARYING       I     FROM  1  BY  1
                 UNTIL         I     >    10.
           IF  CHK-SW          =   ZERO
               GO  TO  SOK-040
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
               PERFORM  TOK-RTN    THRU  TOK-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "　〔　小計　〕"      TO  P3-KEIM
               MOVE  "("           TO  P3-FD
               MOVE  "出荷"      TO  P3-MID
               MOVE  ")"           TO  P3-RD
           END-IF
           MOVE  WS-SIZ(P)     TO  P3-SIZ.
           PERFORM  SKE1-RTN   THRU  SKE1-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WS-KKEI1(P)   TO  P3-KEI.
           MOVE  WS-KSET1(P)   TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  1             TO  WS-MSW(P).
           MOVE  0             TO  MID-SW.
       SOK-040.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  SOK-030
           END-IF
      ***
           MOVE  "｢"           TO  P3-FD.
           MOVE  "訂正"      TO  P3-MID.
           MOVE  "｣"           TO  P3-RD.
           MOVE  1             TO  P.
       SOK-050.
           IF  WS-SIZ(P)       =   ZERO
               GO  TO  SOK-060
           END-IF
           MOVE  0             TO  CHK-SW.
           PERFORM  CHK2-RTN   THRU  CHK2-EX
                 VARYING       I     FROM  1  BY  1
                 UNTIL         I     >    10.
           IF  CHK-SW          =   ZERO
               GO  TO  SOK-060
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
               PERFORM  TOK-RTN    THRU  TOK-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "　〔　小計　〕"      TO  P3-KEIM
               MOVE  "｢"           TO  P3-FD
               MOVE  "訂正"      TO  P3-MID
               MOVE  "｣"           TO  P3-RD
           END-IF
           MOVE  WS-SIZ(P)     TO  P3-SIZ.
           PERFORM  SKE2-RTN   THRU  SKE2-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WS-KKEI2(P)   TO  P3-KEI.
           MOVE  WS-KSET2(P)   TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  1             TO  WS-MSW(P).
           MOVE  0             TO  MID-SW.
       SOK-060.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  SOK-050
           END-IF
           MOVE  SPACE         TO  P-R.
           MOVE  "["           TO  P3-FD.
           MOVE  "残数"      TO  P3-MID.
           MOVE  "]"           TO  P3-RD.
           MOVE  1             TO  P.
       SOK-070.
           IF  WS-SIZ(P)       =   ZERO
               GO  TO  SOK-080
           END-IF
           MOVE  0             TO  CHK-SW.
           PERFORM  CHK5-RTN   THRU  CHK5-EX
                 VARYING       I     FROM  1  BY  1
                 UNTIL         I     >    10.
           IF  CHK-SW          =   ZERO
               GO  TO  SOK-080
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
               PERFORM  TOK-RTN    THRU  TOK-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "　〔　小計　〕"      TO  P3-KEIM
               MOVE  "["           TO  P3-FD
               MOVE  "残数"      TO  P3-MID
               MOVE  "]"           TO  P3-RD
           END-IF
           MOVE  WS-SIZ(P)     TO  P3-SIZ.
           PERFORM  SZAN-RTN   THRU  SZAN-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WS-XKEI(P)    TO  P3-KEI.
           MOVE  WS-XSET(P)    TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  0             TO  MID-SW.
       SOK-080.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  SOK-070
           END-IF.
       SOK-EX.
           EXIT.
      ***************************************
      **   小計　（前残）　数量印字処理     *
      ***************************************
       SZEN-RTN.
           MOVE  WS-ZSU(P , I) TO  P3-SUR1(I).
       SZEN-EX.
           EXIT.
      ***************************************
      **   小計　（出荷）　数量印字処理     *
      ***************************************
       SKE1-RTN.
           MOVE  WS-KSU1(P , I)    TO  P3-SUR1(I).
       SKE1-EX.
           EXIT.
      ***************************************
      **   小計　（訂正）　数量印字処理     *
      ***************************************
       SKE2-RTN.
           MOVE  WS-KSU2(P , I)    TO  P3-SUR1(I).
       SKE2-EX.
           EXIT.
      ***************************************
      **   小計　（残数）　数量印字処理     *
      ***************************************
       SZAN-RTN.
           MOVE  WS-XSU(P , I) TO  P3-SUR1(I).
       SZAN-EX.
           EXIT.
      ***************************************
      **   小計　（出荷）　０チェック　     *
      ***************************************
       CHK1-RTN.
           IF  WS-KSU1(P , I)    NOT =  ZERO
               MOVE  1                  TO  CHK-SW
           END-IF.
       CHK1-EX.
           EXIT.
      ***************************************
      **   小計　（訂正）　０チェック　     *
      ***************************************
       CHK2-RTN.
           IF  WS-KSU2(P , I)    NOT =  ZERO
               MOVE  1                  TO  CHK-SW
           END-IF.
       CHK2-EX.
           EXIT.
      ***************************************
      **   小計　（残数）　０チェック　     *
      ***************************************
       CHK5-RTN.
           IF  WS-XSU(P , I)     NOT =  ZERO
               MOVE  1                  TO  CHK-SW
           END-IF.
       CHK5-EX.
           EXIT.
      **************************
      **   合計印字処理       **
      **************************
       GOK-RTN.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           MOVE  "｛　合計　｝　"      TO  P3-KEIM.
           MOVE  "<"           TO  P3-FD.
           MOVE  "前残"      TO  P3-MID.
           MOVE  ">"           TO  P3-RD.
           MOVE  1             TO  P.
       GOK-010.
           IF  WG-SIZ(P)       =   ZERO
               GO  TO  GOK-020
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "｛　合計　｝　"      TO  P3-KEIM
               MOVE  "<"           TO  P3-FD
               MOVE  "前残"      TO  P3-MID
               MOVE  ">"           TO  P3-RD
           END-IF
           MOVE  WG-SIZ(P)     TO  P3-SIZ.
           PERFORM  GZEN-RTN   THRU  GZEN-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WG-ZKEI(P)    TO  P3-KEI.
           MOVE  WG-ZSET(P)    TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  0             TO  MID-SW.
       GOK-020.
           ADD   1             TO  P.
           IF  P            NOT >  4 
               GO  TO  GOK-010
           END-IF
      ***
           MOVE  "("           TO  P3-FD.
           MOVE  "出荷"      TO  P3-MID.
           MOVE  ")"           TO  P3-RD.
           MOVE  1             TO  P.
       GOK-030.
           IF  WG-SIZ(P)       =   ZERO
               GO  TO  GOK-040
           END-IF
           MOVE  0             TO  CHK-SW.
           PERFORM  CHK3-RTN   THRU  CHK3-EX
                 VARYING       I     FROM  1  BY  1
                 UNTIL         I     >    10.
           IF  CHK-SW          =   ZERO
               GO  TO  GOK-040
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "｛　合計　｝　"      TO  P3-KEIM
               MOVE  "("           TO  P3-FD
               MOVE  "出荷"      TO  P3-MID
               MOVE  ")"           TO  P3-RD
           END-IF
           MOVE  WG-SIZ(P)     TO  P3-SIZ.
           PERFORM  GKE1-RTN   THRU  GKE1-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WG-KKEI1(P)   TO  P3-KEI.
           MOVE  WG-KSET1(P)   TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  1             TO  WG-MSW(P).
           MOVE  0             TO  MID-SW.
       GOK-040.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  GOK-030
           END-IF
      ***
           MOVE  "｢"           TO  P3-FD.
           MOVE  "訂正"      TO  P3-MID.
           MOVE  "｣"           TO  P3-RD.
           MOVE  1             TO  P.
       GOK-050.
           IF  WG-SIZ(P)       =   ZERO
               GO  TO  GOK-060
           END-IF
           MOVE  0             TO  CHK-SW.
           PERFORM  CHK4-RTN   THRU  CHK4-EX
                 VARYING       I     FROM  1  BY  1
                 UNTIL         I     >    10.
           IF  CHK-SW          =   ZERO
               GO  TO  GOK-060
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "｛　合計　｝　"      TO  P3-KEIM
               MOVE  "｢"           TO  P3-FD
               MOVE  "訂正"      TO  P3-MID
               MOVE  "｣"           TO  P3-RD
           END-IF
           MOVE  WG-SIZ(P)     TO  P3-SIZ.
           PERFORM  GKE2-RTN   THRU  GKE2-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WG-KKEI2(P)   TO  P3-KEI.
           MOVE  WG-KSET2(P)   TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  1             TO  WG-MSW(P).
           MOVE  0             TO  MID-SW.
       GOK-060.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  GOK-050
           END-IF
           MOVE  SPACE         TO  P-R.
           MOVE  "["           TO  P3-FD.
           MOVE  "残数"      TO  P3-MID.
           MOVE  "]"           TO  P3-RD.
           MOVE  1             TO  P.
       GOK-070.
           IF  WG-SIZ(P)       =   ZERO
               GO  TO  GOK-080
           END-IF
           MOVE  0             TO  CHK-SW.
           PERFORM  CHK6-RTN   THRU  CHK6-EX
                 VARYING       I     FROM  1  BY  1
                 UNTIL         I     >    10.
           IF  CHK-SW          =   ZERO
               GO  TO  GOK-080
           END-IF
           IF  LCNT         NOT <  60
               MOVE  1             TO  MID-SW
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
           END-IF
           IF  MID-SW          =   1
               MOVE  "｛　合計　｝　"      TO  P3-KEIM
               MOVE  "["           TO  P3-FD
               MOVE  "残数"      TO  P3-MID
               MOVE  "]"           TO  P3-RD
           END-IF
           MOVE  WG-SIZ(P)     TO  P3-SIZ.
           PERFORM  GZAN-RTN   THRU  GZAN-EX
                 VARYING       I   FROM  1  BY  1
                 UNTIL         I   >    10.
           MOVE  WG-XKEI(P)    TO  P3-KEI.
           MOVE  WG-XSET(P)    TO  P3-SET.
      *
           MOVE  W-15K         TO  P3-15K.
           MOVE  W-20K         TO  P3-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           MOVE  SPACE         TO  P3-MID.
           ADD   1             TO  LCNT.
           MOVE  0             TO  MID-SW.
       GOK-080.
           ADD   1             TO  P.
           IF  P            NOT >  4
               GO  TO  GOK-070
           END-IF.
       GOK-EX.
           EXIT.
      ***************************************
      **   合計　（前残）　数量印字処理     *
      ***************************************
       GZEN-RTN.
           MOVE  WG-ZSU(P , I) TO  P3-SUR1(I).
       GZEN-EX.
           EXIT.
      ***************************************
      **   合計　（出荷）　数量印字処理     *
      ***************************************
       GKE1-RTN.
           MOVE  WG-KSU1(P , I)    TO  P3-SUR1(I).
       GKE1-EX.
           EXIT.
      ***************************************
      **   合計　（訂正）　数量印字処理     *
      ***************************************
       GKE2-RTN.
           MOVE  WG-KSU2(P , I)    TO  P3-SUR1(I).
       GKE2-EX.
           EXIT.
      ***************************************
      **   合計　（残数）　数量印字処理     *
      ***************************************
       GZAN-RTN.
           MOVE  WG-XSU(P , I) TO  P3-SUR1(I).
       GZAN-EX.
           EXIT.
      ***************************************
      **   合計　（出荷）　０チェック　     *
      ***************************************
       CHK3-RTN.
           IF  WG-KSU1(P , I)    NOT =  ZERO
               MOVE  1                  TO  CHK-SW
           END-IF.
       CHK3-EX.
           EXIT.
      ***************************************
      **   合計　（訂正）　０チェック　     *
      ***************************************
       CHK4-RTN.
           IF  WG-KSU2(P , I)    NOT =  ZERO
               MOVE  1                  TO  CHK-SW
           END-IF.
       CHK4-EX.
           EXIT.
      ***************************************
      **   合計　（残数）　０チェック　     *
      ***************************************
       CHK6-RTN.
           IF  WG-XSU(P , I)     NOT =  ZERO
               MOVE  1                  TO  CHK-SW
           END-IF.
       CHK6-EX.
           EXIT.
      **************************
      **   品名　　印字処理   **
      **************************
       HNM-RTN.
           IF  LCNT         NOT <  58
               PERFORM  HEAD-RTN   THRU  HEAD-EX
           END-IF
      *
           MOVE  OLD-HCD       TO  HI-MHCD HI-HCD.
      *           READ  HI2-M         UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE       TO  HI-NAME
           END-IF
           MOVE  SPACE         TO  P-R.
           MOVE  OLD-HCD       TO  P1-HCD.
           MOVE  HI-NAME       TO  P1-HNA.
      *
           MOVE  W-15K         TO  P1-15K.
           MOVE  W-20K         TO  P1-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           ADD   1             TO  LCNT.
       HNM-EX.
           EXIT.
      **************************
      **   得意先　印字処理   **
      **************************
       TOK-RTN.
           IF  LCNT        NOT  <  59
               PERFORM  HEAD-RTN   THRU  HEAD-EX
               PERFORM  HNM-RTN    THRU  HNM-EX
           END-IF
           MOVE  OLD-TCD       TO  TC-TCD.
           MOVE  001           TO  TC-CCD.
      *           READ  TC-M          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE       TO  TC-NAME
           END-IF
           MOVE  SPACE         TO  P-R.
           MOVE  OLD-TCD       TO  P2-TCD.
           MOVE  "-"           TO  P2-V.
           MOVE  OLD-CCD       TO  P2-CCD.
           MOVE  TC-NAME       TO  P2-TNA.
           MOVE  SPACE         TO  P2-CNA.
           IF  OLD-CCD          =  001
               GO  TO  TOK-010
           END-IF
           MOVE  OLD-TCD       TO  TC-TCD.
           MOVE  OLD-CCD       TO  TC-CCD.
      *           READ  TC-M          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE       TO  TC-NAME
           END-IF
           MOVE  TC-NAME       TO  P2-CNA.
       TOK-010.
           MOVE  W-15K         TO  P2-15K.
           MOVE  W-20K         TO  P2-20K.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE         TO  P-R.
           ADD   1             TO  LCNT.
       TOK-EX.
           EXIT.
      *********************************
      *    ワークファイル　ＲＥＡＤ   *
      *********************************
       RWK-RTN.
      *           READ  JT-WK05       NEXT  AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK05_PNAME1 BY REFERENCE WK05-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  HIGH-VALUE  TO  NEW-KEY
               GO  TO  RWK-EX
           END-IF
           IF  RED-SW              =   0
               MOVE    1          TO  RED-SW
               GO  TO  RWK-EX
           END-IF
           IF  WK05-01        NOT  =   S-KBN
               GO  TO  RWK-RTN
           END-IF
           IF  WK05-07             <  S-HINF  OR  >  S-HINT
               GO  TO  RWK-RTN
           END-IF
           IF  WK05-041            <  S-TOKF  OR  >  S-TOKT
               GO  TO  RWK-RTN
           END-IF
           MOVE  WK05-07       TO   HI-MHCD HI-HCD.
      *           READ  HI2-M        UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO  RWK-RTN
           END-IF
           IF  S-SEN               =   0
               IF  HI-BC3           <  30  OR   >  39
                   GO  TO RWK-RTN
               END-IF
           END-IF
           IF  S-SEN               =   1
               IF  HI-BC3           <  20  OR   >  29
                   GO  TO RWK-RTN
               END-IF
           END-IF
           IF  S-SEN               =   2
               IF  HI-BC3           >  19
                   GO  TO RWK-RTN
               END-IF
           END-IF.
       RWK-010.
           MOVE  WK05-95       TO  NEW-HCD.
           MOVE  WK05-041      TO  NEW-TCD.
           MOVE  WK05-042      TO  NEW-CCD.
           MOVE  WK05-06       TO  NEW-JNO.
           MOVE  WK05-021      TO  N-SNO.
       RWK-EX.
           EXIT.
      **************************
      ***  ﾌ ｧ ｲ ﾙ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK05_IDLST JT-WK05_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
       COPY  LPMSG.
