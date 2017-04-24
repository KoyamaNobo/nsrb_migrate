       IDENTIFICATION         DIVISION.
       PROGRAM-ID.            JSS10I.
      *********************************************************
      *    PROGRAM         :  生協発注書入力　　              *
      *    PRINTER TYPE    :  UNUSED                          *
      *    SCREEN          :  SJS10I                          *
      *    DATE      　　  :  04/02/01                        *
      *    COMPILE TYPE    :  CBL85(74)                       *
      *********************************************************
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       EXPRESS5800.
       OBJECT-COMPUTER.       EXPRESS5800.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
       77  ERR-STAT             PIC  X(02).
       77  ACT                  PIC  9(01).
       77  ACTNM                PIC  N(02).
       77  OKC                  PIC  9(01).
       01  SYSYMD               PIC  9(08).
       01  SYSYMDR              REDEFINES  SYSYMD.
           02  SYSYY            PIC  9(04).
           02  SYSYYR           REDEFINES  SYSYY.
               03  SYSY1        PIC  9(02).
               03  SYSY2        PIC  9(02).
           02  SYSMM            PIC  9(02).
           02  SYSDD            PIC  9(02).
       01  WK-AREA.
           02  II               PIC  9(01).
           02  JJ               PIC  9(02).
           02  KK               PIC  9(02).
           02  LIN              PIC  9(02).
           02  SV-II            PIC  9(01).
           02  O-II             PIC  9(01).
           02  SW-HAT           PIC  9(01).
           02  SW-HIN           PIC  9(01).
           02  SW-HIN4          PIC  9(01).
           02  SW-CON           PIC  9(01).
           02  SW-JDCON         PIC  9(01).
           02  W-NGP.
             03  W-NEN          PIC  9(04).
             03  W-GET          PIC  9(02).
             03  W-PEY          PIC  9(02).
           02  W-FNGP           PIC  9(08).
           02  W-RNGP           PIC  9(08).
       01  G-AREA.
           02  G-HNO                PIC  9(06).
           02  G-HED.
               03  G-SYMD           PIC  9(08).
               03  G-SYMDR          REDEFINES  G-SYMD.
                   04  G-SYY        PIC  9(04).
                   04  G-SYYR       REDEFINES  G-SYY.
                       05  G-SY1    PIC  9(02).
                       05  G-SY2    PIC  9(02).
                   04  G-SMM        PIC  9(02).
                   04  G-SDD        PIC  9(02).
               03  G-NYMD           PIC  9(08).
               03  G-NYMDR          REDEFINES  G-NYMD.
                   04  G-NYY        PIC  9(04).
                   04  G-NYYR       REDEFINES  G-NYY.
                       05  G-NY1    PIC  9(02).
                       05  G-NY2    PIC  9(02).
                   04  G-NMM        PIC  9(02).
                   04  G-NDD        PIC  9(02).
               03  G-TCCD.
                   04  G-TCD        PIC  9(04).
                   04  G-CCD        PIC  9(03).
               03  G-TNM            PIC  N(26).
               03  G-CNM            PIC  N(26).
               03  G-HYMD           PIC  9(08).
               03  G-HYMDR          REDEFINES  G-HYMD.
                   04  G-HYY        PIC  9(04).
                   04  G-HYYR       REDEFINES  G-HYY.
                       05  G-HY1    PIC  9(02).
                       05  G-HY2    PIC  9(02).
                   04  G-HMM        PIC  9(02).
                   04  G-HDD        PIC  9(02).
               03  G-DKB            PIC  9(01).
               03  G-DNO            PIC  9(10).
           02  G-MEI.
               03  G-MEI-T          OCCURS   6.
                   04  G-HCD        PIC  9(06).
                   04  O-HCD        PIC  9(06).
                   04  G-HNM        PIC  N(24).
                   04  G-AHCD       PIC  X(13).
                   04  G-AHCD2      PIC  X(09).
                   04  G-SIZ        PIC  9(03).
                   04  G-SKB        PIC  9(01).
                   04  G-SUR        PIC S9(04).
                   04  G-GTAN       PIC  9(05).
                   04  G-GKIN       PIC S9(08).
                   04  G-UTAN       PIC  9(05).
                   04  G-UKIN       PIC S9(08).
           02  G-FOT.
               03  G-GSUR           PIC S9(05).
               03  G-GGKIN          PIC S9(08).
               03  G-GUKIN          PIC S9(08).
               03  G-UCD            PIC  9(01).
               03  O-UCD            PIC  9(01).
               03  G-UNM            PIC  N(06).
               03  G-HT             PIC  N(09).
               03  G-KK.
                   04  G-KKM        PIC  9(02).
                   04  G-KKS        PIC  9(02).
                   04  G-KKG        PIC  9(03).
               03  O-KK.
                   04  O-KKM        PIC  9(02).
                   04  O-KKS        PIC  9(02).
                   04  O-KKG        PIC  9(03).
               03  G-KKNM           PIC  N(23).
      *
       COPY LWMSG.
       COPY LWSIZC.
      *****
           COPY   LITM.
           COPY   LITCM.
           COPY   LIHIM2.
           COPY   LITHTM.
           COPY   LSTENM.
           COPY   L-JCON.
           COPY   LJDCON.
           COPY   LSKHAT.
           COPY   LIBFDD.
      *****
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      *
       01  DSP-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
      *
       01  A-AREA.
           02  A-L01.
               03  A-ACT     PIC  9(01).
           02  A-L02.
               03  A-HNO     PIC  9(06).
           02  A-L03.
               03  A-SYMD.
                   04  A-SY  PIC  9(02).
                   04  A-SM  PIC  9(02).
                   04  A-SD  PIC  9(02).
               03  A-NYMD.
                   04  A-NY  PIC  9(02).
                   04  A-NM  PIC  9(02).
                   04  A-ND  PIC  9(02).
               03  A-TCCD.
                   04  A-TCD PIC  9(04).
                   04  A-CCD PIC  9(03).
           02  A-L04.
               03  A-HYMD.
                   04  A-HY  PIC  9(02).
                   04  A-HM  PIC  9(02).
                   04  A-HD  PIC  9(02).
               03  A-DKB     PIC  9(01).
           02  A-L05.
               03  A-DNO     PIC  9(10).
           02  A-LLIN.
               03  A-HCD     PIC  9(06).
               03  A-AHCD    PIC  X(13).
               03  A-AHCD2   PIC  X(09).
               03  A-SIZ     PIC  9(03).
               03  A-SUR     PIC S9(04).
               03  A-GTAN    PIC S9(05).
               03  A-UTAN    PIC S9(05).
           02  A-LLINP1.
               03  A-HNM     PIC  N(24).
           02  A-L22.
               03  A-UCD     PIC  9(01).
               03  A-KKM     PIC  9(02).
               03  A-KKS     PIC  9(02).
               03  A-KKG     PIC  9(03).
           02  A-L23.
               03  A-HT      PIC  N(09).
               03  A-KKNM    PIC  N(23).
           02  A-L24.
               03  A-OKC     PIC  9(01).
      *
       01  DSP-AREA.
           02  D-L01.
               03  D-ACTNM   PIC N(02).
           02  D-L03.
               03  D-SYMD.
                   04 D-SY   PIC 9(02).
                   04 D-SM   PIC Z9 .
                   04 D-SD   PIC Z9 .
               03  D-NYMD.
                   04 D-NY   PIC 9(02).
                   04 D-NM   PIC Z9 .
                   04 D-ND   PIC Z9 .
               03  D-TNM     PIC N(26).
           02  D-L04.
               03  D-HYMD.
                   04 D-HY   PIC 9(02).
                   04 D-HM   PIC Z9 .
                   04 D-HD   PIC Z9 .
               03  D-CNM     PIC N(26).
           02  D-LLIN.
               03  D-SUR     PIC ZZZ9- .
               03  D-GTAN    PIC ZZZZ9 .
               03  D-GKIN    PIC ---,---,--9 .
               03  D-UTAN    PIC ZZZZ9 .
               03  D-UKIN    PIC ---,---,--9 .
           02  D-L20.
               03  D-GSUR    PIC ZZZZ9- .
               03  D-GGKIN   PIC ---,---,--9 .
               03  D-GUKIN   PIC ---,---,--9 .
           02  D-L22.
               03  D-UNM     PIC N(06).
      *
       01  C-AREA.
           02  C-L01.
               03  C-ACT     PIC Z .
               03  C-ACTNM   PIC N(02).
           02  C-L02.
               03  C-HNO     PIC Z(06).
           02  C-L03.
               03  C-SYMD.
                   04        PIC Z(02).
                   04        PIC Z(02).
                   04        PIC Z(02).
               03  C-NYMD.
                   04        PIC Z(02).
                   04        PIC Z(02).
                   04        PIC Z(02).
               03  C-TNM     PIC N(26).
               03  C-TCCD.
                   04  C-TCD PIC Z(04).
                   04  C-CCD PIC Z(03).
           02  C-L04.
               03  C-HYMD.
                   04        PIC Z(02).
                   04        PIC Z(02).
                   04        PIC Z(02).
               03  C-DKB     PIC Z .
               03  C-CNM     PIC N(26).
           02  C-L05.
               03  C-DNO     PIC Z(10).
           02  C-LLIN.
               03  C-HCD     PIC Z(06).
               03  C-AHCD    PIC X(13).
               03  C-AHCD2   PIC X(09).
               03  C-SIZ     PIC Z(03).
               03  C-SUR     PIC ZZZZ- .
               03  C-GTAN    PIC Z(05).
               03  C-GKIN    PIC ---,---,--- .
               03  C-UTAN    PIC Z(05).
               03  C-UKIN    PIC ---,---,--- .
           02  C-LLINP1.
               03  C-HNM     PIC N(24).
           02  C-L20.
               03  C-GSUR    PIC ------ .
               03  C-GGKIN   PIC ---,---,--- .
               03  C-GUKIN   PIC ---,---,--- .
           02  C-L22.
               03  C-UCD     PIC Z .
               03  C-UNM     PIC N(06).
               03  C-KKM     PIC Z(02).
               03  C-KKS     PIC Z(02).
               03  C-KKG     PIC Z(03).
           02  C-L23.
               03  C-HT      PIC N(09).
               03  C-KKNM    PIC N(23).
           02  C-L24.
               03  C-OKC     PIC Z .
      ****
      *
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER           PIC X(60).
           02  DISP-MSG-SPACE.
               03  FILLER           PIC X(60).
           02  DISP-MSG-SPACES.
               03  FILLER           PIC X(40).
           02  DISP-BUZ-B.
               03  FILLER           PIC X(05) VALUE X"1B4210".
           02  DISP-BUZ-J.
               03  FILLER           PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER           PIC X(22) VALUE
               "＊　マスタ　登録済　＊".
           02  NOR-D01.
               03  FILLER           PIC X(22) VALUE
               "＊　デ―タ　登録済　＊".
           02  INV-M01.
               03  FILLER           PIC X(22) VALUE
               "＊　マスタ　未登録　＊".
           02  INV-D01.
               03  FILLER           PIC X(22) VALUE
               "＊　デ―タ　未登録　＊".
           02  OK-01.
               03  FILLER           PIC X(14) VALUE
               "＊　Ｏ　Ｋ　＊".
           02  CAN-01.
               03  FILLER           PIC X(18) VALUE
               "＊　キャンセル　＊".
           02  ERR-01.
               03  FILLER           PIC X(18) VALUE
               "＊　入力エラ―　＊".
           02  ERR-02.
               03  FILLER            PIC X(22) VALUE
               "＊　データ　なし　　＊".
           02  ERR-DIS.
               03  FILLER            PIC X(05) VALUE
               "<<<  ".
               03  FILLER            PIC X(12).
               03  FILLER            PIC X(01).
               03  FILLER            PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  FILLER            PIC X(02).
               03  FILLER            PIC X(05) VALUE
               "  >>>".
               03  FILLER            PIC X(05) VALUE
               " KEY=".
               03  FILLER            PIC X(30).
           02  MSG-SPACE             PIC X(49) VALUE
                 "                                                 ".
           02  INV-TOK               PIC X(49) VALUE
                 "得意先マスタ　未登録                             ".
           02  INV-CHO               PIC X(49) VALUE
                 "直送先マスタ　未登録                             ".
           02  INV-STE               PIC X(49) VALUE
                 "社店マスタ　未登録                               ".
           02  ERR-HNO               PIC X(49) VALUE
                 "同一発注№使用中です                             ".
           02  ERR-AHCD1             PIC X(49) VALUE
                 "相手品名コードが入力されてません                 ".
           02  ERR-AHCD2             PIC X(49) VALUE
                 "同一コードが有ります                             ".
           02  ERR-SIZ1              PIC X(49) VALUE
                 "サイズが入力されてません                         ".
           02  ERR-SIZ2              PIC X(49) VALUE
                 "サイズ未登録                                     ".
           02  ERR-SIZ3              PIC X(49) VALUE
                 "同一サイズが有ります                             ".
           02  ERR-HIN               PIC X(49) VALUE
                 "品名ＩＮＤＥＸＥＤマスタ未登録                   ".
           02  ERR-ISU               PIC X(49) VALUE
                 "入数無し                                         ".
           02  ERR-GTAN              PIC X(49) VALUE
                 "原単価が違います                                 ".
           02  ERR-UTAN              PIC X(49) VALUE
                 "売単価が違います                                 ".
      *
           COPY  LIBSCR.
      *****
       PROCEDURE              DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLEAR
       CALL "SD_Init" USING
           "DSP-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
          "01DSP-CLEAR" "X" "1" "0" "12" " " "DSP-CLEAR" RETURNING RESU.
      *A-AREA
       CALL "SD_Init" USING 
            "A-AREA" " " "0" "0" "209" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L01" " " "1" "0" "1" " " "A-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "1" "70" "1" " " "A-L01" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L02" " " "2" "0" "6" "A-L01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HNO" "9" "2" "74" "6" " " "A-L02" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HNO" BY REFERENCE G-HNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L03" " " "3" "0" "19" "A-L02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SYMD" " " "3" "0" "6" " " "A-L03" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SY" "9" "3" "2" "2" " " "A-SYMD" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SY" BY REFERENCE G-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SM" "9" "3" "5" "2" "A-SY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SM" BY REFERENCE G-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SD" "9" "3" "8" "2" "A-SM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SD" BY REFERENCE G-SDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NYMD" " " "3" "0" "6" "A-SYMD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NY" "9" "3" "11" "2" " " "A-NYMD" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NY" BY REFERENCE G-NY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NM" "9" "3" "14" "2" "A-NY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NM" BY REFERENCE G-NMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ND" "9" "3" "17" "2" "A-NM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ND" BY REFERENCE G-NDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCCD" " " "3" "0" "7" "A-NYMD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "3" "20" "4" " " "A-TCCD" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE G-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CCD" "9" "3" "25" "3" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CCD" BY REFERENCE G-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L04" " " "4" "0" "7" "A-L03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HYMD" " " "4" "0" "6" " " "A-L04" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HY" "9" "4" "11" "2" " " "A-HYMD" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HY" BY REFERENCE G-HY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HM" "9" "4" "14" "2" "A-HY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HM" BY REFERENCE G-HMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HD" "9" "4" "17" "2" "A-HM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HD" BY REFERENCE G-HDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DKB" "9" "4" "26" "1" "A-HYMD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DKB" BY REFERENCE G-DKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L05" " " "5" "0" "10" "A-L04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "5" "11" "10" " " "A-L05" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE G-DNO "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-LLIN" " " "LIN" "0" "45" "A-L05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "LIN" "2" "6" " " "A-LLIN" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE G-HCD(1) "6" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-AHCD" "X" "LIN" "9" "13" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-AHCD" BY REFERENCE G-AHCD(1) "13" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-AHCD2" "X" "LIN" "23" "9" "A-AHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-AHCD2" BY REFERENCE G-AHCD2(1) "9" "1" BY REFERENCE
            II 116 RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SIZ" "9" "LIN" "33" "3" "A-AHCD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SIZ" BY REFERENCE G-SIZ(1) "3" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SUR" "S9" "LIN" "38" "4" "A-SIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SUR" BY REFERENCE G-SUR(1) "4" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GTAN" "S9" "LIN" "44" "5" "A-SUR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GTAN" BY REFERENCE G-GTAN(1) "5" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UTAN" "S9" "LIN" "63" "5" "A-GTAN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UTAN" BY REFERENCE G-UTAN(1) "5" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-LLINP1" " " "LIN PLUS 1" "0" "48" "A-LLIN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HNM" "N" "LIN PLUS 1" "9" "48" " " "A-LLINP1"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HNM" BY REFERENCE G-HNM(1) "48" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L22" " " "22" "0" "8" "A-LLINP1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UCD" "9" "22" "7" "1" " " "A-L22" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UCD" BY REFERENCE G-UCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KKM" "9" "22" "33" "2" "A-UCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KKM" BY REFERENCE G-KKM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KKS" "9" "22" "37" "2" "A-KKM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KKS" BY REFERENCE G-KKS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KKG" "9" "22" "41" "3" "A-KKS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KKG" BY REFERENCE G-KKG "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L23" " " "23" "0" "64" "A-L22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HT" "N" "23" "7" "18" " " "A-L23" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HT" BY REFERENCE G-HT "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KKNM" "N" "23" "33" "46" "A-HT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KKNM" BY REFERENCE G-KKNM "46" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-L24" " " "24" "0" "1" "A-L23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OKC" "9" "24" "72" "1" " " "A-L24" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "203" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-L01" " " "1" "0" "4" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ACTNM" "N" "1" "72" "4" " " "D-L01" RETURNING RESU.
       CALL "SD_From" USING 
            "D-ACTNM" BY REFERENCE ACTNM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-L03" " " "3" "0" "64" "D-L01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SYMD" " " "3" "0" "6" " " "D-L03" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SY" "9" "3" "2" "2" " " "D-SYMD" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SY" BY REFERENCE G-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SM" "Z9" "3" "5" "2" "D-SY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SM" BY REFERENCE G-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SD" "Z9" "3" "8" "2" "D-SM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SD" BY REFERENCE G-SDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NYMD" " " "3" "0" "6" "D-SYMD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NY" "9" "3" "11" "2" " " "D-NYMD" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NY" BY REFERENCE G-NY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "Z9" "3" "14" "2" "D-NY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NM" BY REFERENCE G-NMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ND" "Z9" "3" "17" "2" "D-NM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-ND" BY REFERENCE G-NDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNM" "N" "3" "29" "52" "D-NYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNM" BY REFERENCE G-TNM "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-L04" " " "4" "0" "58" "D-L03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HYMD" " " "4" "0" "6" " " "D-L04" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HY" "9" "4" "11" "2" " " "D-HYMD" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HY" BY REFERENCE G-HY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HM" "Z9" "4" "14" "2" "D-HY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HM" BY REFERENCE G-HMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HD" "Z9" "4" "17" "2" "D-HM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HD" BY REFERENCE G-HDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CNM" "N" "4" "29" "52" "D-HYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-CNM" BY REFERENCE G-CNM "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-LLIN" " " "LIN" "0" "37" "D-L04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SUR" "ZZZ9-" "LIN" "38" "5" " " "D-LLIN" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SUR" BY REFERENCE G-SUR(1) "4" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GTAN" "ZZZZ9" "LIN" "44" "5" "D-SUR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GTAN" BY REFERENCE G-GTAN(1) "5" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKIN" "---,---,--9" "LIN" "50" "11" "D-GTAN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GKIN" BY REFERENCE G-GKIN(1) "8" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UTAN" "ZZZZ9" "LIN" "63" "5" "D-GKIN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTAN" BY REFERENCE G-UTAN(1) "5" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UKIN" "---,---,--9" "LIN" "69" "11" "D-UTAN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UKIN" BY REFERENCE G-UKIN(1) "8" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-L20" " " "20" "0" "28" "D-LLIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GSUR" "ZZZZ9-" "20" "37" "6" " " "D-L20" RETURNING RESU.
       CALL "SD_From" USING 
            "D-GSUR" BY REFERENCE G-GSUR "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GGKIN" "---,---,--9" "20" "50" "11" "D-GSUR" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GGKIN" BY REFERENCE G-GGKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GUKIN" "---,---,--9" "20" "69" "11" "D-GGKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GUKIN" BY REFERENCE G-GUKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-L22" " " "22" "0" "12" "D-L20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UNM" "N" "22" "9" "12" " " "D-L22" RETURNING RESU.
       CALL "SD_From" USING 
            "D-UNM" BY REFERENCE G-UNM "12" "0" RETURNING RESU.
      *C-AREA
       CALL "SD_Init" USING 
            "C-AREA" " " "0" "0" "380" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L01" " " "1" "0" "5" " " "C-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-ACT" "Z" "1" "70" "1" " " "C-L01" RETURNING RESU.
       CALL "SD_From" USING 
            "C-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-ACTNM" "N" "1" "72" "4" "C-ACT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-ACTNM" BY REFERENCE ACTNM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L02" " " "2" "0" "6" "C-L01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HNO" "Z" "2" "74" "6" " " "C-L02" RETURNING RESU.
       CALL "SD_From" USING 
            "C-HNO" BY REFERENCE G-HNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L03" " " "3" "0" "71" "C-L02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SYMD" " " "3" "0" "6" " " "C-L03" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-SYMD" "Z" "3" "2" "2" " " "C-SYMD" RETURNING RESU.
       CALL "SD_From" USING 
            "01C-SYMD" BY REFERENCE G-SY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-SYMD" "Z" "3" "5" "2" "01C-SYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02C-SYMD" BY REFERENCE G-SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-SYMD" "Z" "3" "8" "2" "02C-SYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03C-SYMD" BY REFERENCE G-SDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-NYMD" " " "3" "0" "6" "C-SYMD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-NYMD" "Z" "3" "11" "2" " " "C-NYMD" RETURNING RESU.
       CALL "SD_From" USING 
            "01C-NYMD" BY REFERENCE G-NY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-NYMD" "Z" "3" "14" "2" "01C-NYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02C-NYMD" BY REFERENCE G-NMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-NYMD" "Z" "3" "17" "2" "02C-NYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03C-NYMD" BY REFERENCE G-NDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-TNM" "N" "3" "29" "52" "C-NYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-TNM" BY REFERENCE G-TNM "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-TCCD" " " "3" "0" "7" "C-TNM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-TCD" "Z" "3" "20" "4" " " "C-TCCD" RETURNING RESU.
       CALL "SD_From" USING 
            "C-TCD" BY REFERENCE G-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CCD" "Z" "3" "25" "3" "C-TCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-CCD" BY REFERENCE G-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L04" " " "4" "0" "59" "C-L03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HYMD" " " "4" "0" "6" " " "C-L04" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-HYMD" "Z" "4" "11" "2" " " "C-HYMD" RETURNING RESU.
       CALL "SD_From" USING 
            "01C-HYMD" BY REFERENCE G-HY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-HYMD" "Z" "4" "14" "2" "01C-HYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02C-HYMD" BY REFERENCE G-HMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-HYMD" "Z" "4" "17" "2" "02C-HYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03C-HYMD" BY REFERENCE G-HDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-DKB" "Z" "4" "26" "1" "C-HYMD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-DKB" BY REFERENCE G-DKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-CNM" "N" "4" "29" "52" "C-DKB" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-CNM" BY REFERENCE G-CNM "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L05" " " "5" "0" "10" "C-L04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-DNO" "Z" "5" "11" "10" " " "C-L05" RETURNING RESU.
       CALL "SD_From" USING 
            "C-DNO" BY REFERENCE G-DNO "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-LLIN" " " "LIN" "0" "68" "C-L05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HCD" "Z" "LIN" "2" "6" " " "C-LLIN" RETURNING RESU.
       CALL "SD_From" USING 
            "C-HCD" BY REFERENCE G-HCD(1) "6" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-AHCD" "X" "LIN" "9" "13" "C-HCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-AHCD" BY REFERENCE G-AHCD(1) "13" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-AHCD2" "X" "LIN" "23" "9" "C-AHCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-AHCD2" BY REFERENCE G-AHCD2(1) "9" "1" BY REFERENCE
            II 116 RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SIZ" "Z" "LIN" "33" "3" "C-AHCD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-SIZ" BY REFERENCE G-SIZ(1) "3" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-SUR" "ZZZZ-" "LIN" "38" "5" "C-SIZ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-SUR" BY REFERENCE G-SUR(1) "4" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-GTAN" "Z" "LIN" "44" "5" "C-SUR" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-GTAN" BY REFERENCE G-GTAN(1) "5" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-GKIN" "---,---,---" "LIN" "50" "11" "C-GTAN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "C-GKIN" BY REFERENCE G-GKIN(1) "8" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-UTAN" "Z" "LIN" "63" "5" "C-GKIN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-UTAN" BY REFERENCE G-UTAN(1) "5" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-UKIN" "---,---,---" "LIN" "69" "11" "C-UTAN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "C-UKIN" BY REFERENCE G-UKIN(1) "8" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-LLINP1" " " "LIN PLUS 1" "0" "48" "C-LLIN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HNM" "N" "LIN PLUS 1" "9" "48" " " "C-LLINP1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "C-HNM" BY REFERENCE G-HNM(1) "48" "1" BY REFERENCE II 116
            RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L20" " " "20" "0" "28" "C-LLINP1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-GSUR" "------" "20" "37" "6" " " "C-L20" RETURNING RESU.
       CALL "SD_From" USING 
            "C-GSUR" BY REFERENCE G-GSUR "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-GGKIN" "---,---,---" "20" "50" "11" "C-GSUR" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "C-GGKIN" BY REFERENCE G-GGKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-GUKIN" "---,---,---" "20" "69" "11" "C-GGKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "C-GUKIN" BY REFERENCE G-GUKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L22" " " "22" "0" "20" "C-L20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-UCD" "Z" "22" "7" "1" " " "C-L22" RETURNING RESU.
       CALL "SD_From" USING 
            "C-UCD" BY REFERENCE G-UCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-UNM" "N" "22" "9" "12" "C-UCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-UNM" BY REFERENCE G-UNM "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-KKM" "Z" "22" "33" "2" "C-UNM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-KKM" BY REFERENCE G-KKM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-KKS" "Z" "22" "37" "2" "C-KKM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-KKS" BY REFERENCE G-KKS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-KKG" "Z" "22" "41" "3" "C-KKS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-KKG" BY REFERENCE G-KKG "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L23" " " "23" "0" "64" "C-L22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-HT" "N" "23" "7" "18" " " "C-L23" RETURNING RESU.
       CALL "SD_From" USING 
            "C-HT" BY REFERENCE G-HT "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-KKNM" "N" "23" "33" "46" "C-HT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "C-KKNM" BY REFERENCE G-KKNM "46" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "C-L24" " " "24" "0" "1" "C-L23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "C-OKC" "Z" "24" "72" "1" " " "C-L24" RETURNING RESU.
       CALL "SD_From" USING 
            "C-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DISP-ERR-AREA
       CALL "SD_Init" USING 
            "DISP-ERR-AREA" " " "24" "0" "1087" " " " " RETURNING RESU.
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
            "DISP-MSG-SPACE" " " "24" "0" "60" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE" "X" "24" "1" "60" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "60" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACES" " " "24" "0" "40" "DISP-MSG-SPACE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACES" "X" "24" "1" "40" " " "DISP-MSG-SPACES"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DISP-MSG-SPACES" BY REFERENCE ERR-SPACE "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACES" " "
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
            "01NOR-M01" "X" "24" "1" "22" " " "NOR-M01" RETURNING RESU.
       CALL "SD_Init" USING 
            "NOR-D01" " " "24" "0" "22" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01NOR-D01" "X" "24" "1" "22" " " "NOR-D01" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-M01" " " "24" "0" "22" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-M01" "X" "24" "1" "22" " " "INV-M01" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-D01" " " "24" "0" "22" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01INV-D01" "X" "24" "1" "22" " " "INV-D01" RETURNING RESU.
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
       CALL "SD_Init" USING 
            "MSG-SPACE" "X" "24" "1" "49" "ERR-DIS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-TOK" "X" "24" "1" "49" "MSG-SPACE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-CHO" "X" "24" "1" "49" "INV-TOK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-STE" "X" "24" "1" "49" "INV-CHO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-HNO" "X" "24" "1" "49" "INV-STE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-AHCD1" "X" "24" "1" "49" "ERR-HNO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-AHCD2" "X" "24" "1" "49" "ERR-AHCD1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SIZ1" "X" "24" "1" "49" "ERR-AHCD2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SIZ2" "X" "24" "1" "49" "ERR-SIZ1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-SIZ3" "X" "24" "1" "49" "ERR-SIZ2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-HIN" "X" "24" "1" "49" "ERR-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-ISU" "X" "24" "1" "49" "ERR-HIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-GTAN" "X" "24" "1" "49" "ERR-ISU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-UTAN" "X" "24" "1" "49" "ERR-GTAN" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ******************************************************************
       MR-1.
           PERFORM INIT-RTN THRU INIT-EXT.
       MR-2.
           PERFORM SCRN-RTN THRU SCRN-EXT.
       MR-9.
           PERFORM END-RTN  THRU END-EX.
           CALL "DB_Close".
           STOP RUN.
      ******************************************************************
      *    初期処理
      ******************************************************************
       INIT-RTN.
           CALL "SD_Screen_Output" USING "SJS10I" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255
           ACCEPT  SYSYMD   FROM DATE.
           COPY  LIBCPR.
           IF  SYSY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1        TO SYSYY
           END-IF
           IF  SYSY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2        TO SYSYY
           END-IF
           MOVE SYSYMD TO W-NGP.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           MOVE W-NGP TO W-RNGP.
           MOVE SYSYMD TO W-NGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NGP TO W-FNGP.
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" STENM_PNAME1 "SHARED" BY REFERENCE STENM_IDLST "2"
            "STE-KEY1" BY REFERENCE STE-KEY1 "STE-KEY2" BY REFERENCE
            STE-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JDCON_PNAME1 "SHARED" BY REFERENCE JDCON_IDLST "1"
            "JDCON-KEY" BY REFERENCE JDCON-KEY.
           CALL "DB_F_Open" USING
            "I-O" SK-HAT_PNAME1 "SHARED" BY REFERENCE SK-HAT_IDLST "1"
            "HAT-KEY" BY REFERENCE HAT-KEY.
       INIT-EXT.
           EXIT.
      ******************************************************************
      *    画面処理
      ******************************************************************
       SCRN-RTN.
       SCRN-0.
           PERFORM CRE-RTN    THRU CRE-EXT.
       SCRN-1.
           PERFORM ACT-RTN    THRU ACT-EXT.
           IF  ESTAT = "P9"
               GO TO SCRN-EXT
           END-IF.
       SCRN-2.
           PERFORM AHNO-RTN   THRU AHNO-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-0
           END-IF
      *
           IF  ACT    NOT = 1
               PERFORM CRE-2      THRU CRE-EXT
               PERFORM DISP-RTN   THRU DISP-EXT
               IF  SW-HAT NOT = ZERO
                   CALL "SD_Output" USING
                    "INV-D01" INV-D01 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO TO SCRN-2
               ELSE
                   IF  ACT        = 3
                       GO TO SCRN-99
                   END-IF
               END-IF
           END-IF.
       SCRN-2A.
           PERFORM ASYMD-RTN  THRU ASYMD-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-2
           END-IF.
       SCRN-3.
           PERFORM ANYMD-RTN  THRU ANYMD-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-2A
           END-IF.
       SCRN-4.
           PERFORM ATCCD-RTN  THRU ATCCD-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-3
           END-IF.
       SCRN-5.
           PERFORM AHYMD-RTN  THRU AHYMD-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-4
           END-IF.
       SCRN-6.
           PERFORM ADKB-RTN   THRU ADKB-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-5
           END-IF.
       SCRN-7.
           PERFORM ADNO-RTN   THRU ADNO-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-6
           END-IF.
       SCRN-10.
           MOVE 1             TO II.
       SCRN-11.
           COMPUTE LIN = II * 2 + 6.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN RETURNING RESU.
      *
           PERFORM AHCD-RTN   THRU AHCD-EXT.
           IF  ESTAT = "09"
               IF  II     = 1
                   GO TO SCRN-7
               ELSE
                   COMPUTE II  = II  - 1
                   GO TO SCRN-11
               END-IF
           END-IF
           IF  ESTAT     = "04"
               MOVE II      TO SV-II
               PERFORM VARYING II FROM II BY 1 UNTIL II > 6
                       INITIALIZE G-MEI-T(II)
                       COMPUTE LIN = II * 2 + 6
                       CALL "SD_Arg_Match_Line" USING
                        "LIN" "2" LIN RETURNING RESU
                       CALL "SD_Output" USING
                        "C-LLIN" C-LLIN "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "C-LLINP1" C-LLINP1 "p" RETURNING RESU
               END-PERFORM
               MOVE ZERO    TO G-GSUR G-GGKIN G-GUKIN
               PERFORM VARYING JJ FROM 1 BY 1 UNTIL JJ > 6
                       ADD  G-SUR(JJ)     TO G-GSUR
                       ADD  G-GKIN(JJ)    TO G-GGKIN
                       ADD  G-UKIN(JJ)    TO G-GUKIN
               END-PERFORM
               CALL "SD_Output" USING "D-L20" D-L20 "p" RETURNING RESU
               GO TO SCRN-30
           END-IF.
       SCRN-12.
           PERFORM AHNM-RTN   THRU AHNM-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-11
           END-IF.
       SCRN-13.
           PERFORM AAHCD-RTN  THRU AAHCD-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-12
           END-IF.
       SCRN-13A.
           PERFORM AAHCD2-RTN THRU AAHCD2-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-13
           END-IF.
       SCRN-14.
           PERFORM ASIZ-RTN   THRU ASIZ-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-13A
           END-IF.
       SCRN-15.
           PERFORM ASUR-RTN   THRU ASUR-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-14
           END-IF.
       SCRN-16.
           PERFORM AGTAN-RTN  THRU AGTAN-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-15
           END-IF.
       SCRN-17.
           PERFORM AUTAN-RTN  THRU AUTAN-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-16
           END-IF.
       SCRN-18.
           MOVE II            TO SV-II.
      *
           IF II NOT = 6
              COMPUTE II  = II  + 1
              GO TO SCRN-11
           END-IF.
       SCRN-30.
           PERFORM AUCD-RTN   THRU AUCD-EXT.
           IF  ESTAT = "09"
               MOVE SV-II      TO II
               GO TO SCRN-11
           END-IF.
       SCRN-31.
           PERFORM AHT-RTN    THRU AHT-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-30
           END-IF.
       SCRN-32.
           PERFORM AKKM-RTN   THRU AKKM-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-31
           END-IF.
       SCRN-33.
           PERFORM AKKS-RTN   THRU AKKS-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-32
           END-IF.
       SCRN-34.
           PERFORM AKKG-RTN   THRU AKKG-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-33
           END-IF.
       SCRN-35.
           PERFORM HKKNM-RTN  THRU HKKNM-EXT.
       SCRN-36.
           PERFORM AKKNM-RTN  THRU AKKNM-EXT.
           IF  ESTAT = "09"
               GO TO SCRN-34
           END-IF.
       SCRN-99.
           PERFORM OKC-RTN    THRU OKC-EXT.
           IF  OKC   = 9
               GO TO SCRN-0
           END-IF
           IF  ESTAT = "09"
               IF  ACT   = 3
                   GO TO SCRN-2
               ELSE
                   GO TO SCRN-36
               END-IF
           END-IF.
       SCRN-UPD.
           PERFORM UPDT-RTN  THRU UPDT-EXT.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
      *
           PERFORM JDCON-DEL-RTN THRU JDCON-DEL-EXT.
      *
           IF  ACT = 1
               PERFORM VARYING II FROM 1 BY 1 UNTIL II > 6
                       MOVE SPACE          TO G-AHCD(II)
                       MOVE ZERO           TO G-UKIN(II)
                       COMPUTE LIN = II * 2 + 6
                       CALL "SD_Arg_Match_Line" USING
                        "LIN" "2" LIN RETURNING RESU
                       CALL "SD_Output" USING
                        "C-AHCD" C-AHCD "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "C-UKIN" C-UKIN "p" RETURNING RESU
               END-PERFORM
               MOVE ZERO          TO G-DNO G-GUKIN OKC
               CALL "SD_Output" USING "C-DNO" C-DNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-GUKIN" C-GUKIN "p" RETURNING RESU
               CALL "SD_Output" USING "C-OKC" C-OKC "p" RETURNING RESU
               IF  G-HCD(6) = ZERO
                   GO TO SCRN-3
               ELSE
                   GO TO SCRN-7
               END-IF
           ELSE
              PERFORM CRE-1     THRU CRE-EXT
              GO TO SCRN-2
           END-IF.
       SCRN-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    入力項目
      *-----------------------------------------------------------------
      *****
       ACT-RTN.
       ACT-0.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT     = "P9"
               GO TO ACT-EXT
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO ACT-0
           END-IF.
       ACT-CHK.
           IF  ACT   NOT = 1 AND 2 AND 3
               GO TO ACT-0
           END-IF
      *
           EVALUATE ACT
               WHEN 1       MOVE "追加"   TO ACTNM
               WHEN 2       MOVE "変更"   TO ACTNM
               WHEN 3       MOVE "取消"   TO ACTNM
               WHEN OTHER   MOVE SPACE    TO ACTNM
           END-EVALUATE.
           CALL "SD_Output" USING "D-ACTNM" D-ACTNM "p" RETURNING RESU.
       ACT-EXT.
           EXIT.
      *****
       AHNO-RTN.
           IF  ACT       = 1
               MOVE ZERO       TO G-HNO
               CALL "SD_Output" USING "C-HNO" C-HNO "p" RETURNING RESU
               GO TO AHNO-EXT
           END-IF
      *
           PERFORM JDCON-DEL-RTN THRU JDCON-DEL-EXT.
       AHNO-0.
           CALL "SD_Accept" USING BY REFERENCE A-HNO "A-HNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT     = "09"
               GO TO AHNO-EXT
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO AHNO-0
           END-IF.
       AHNO-CHK.
           PERFORM JDCON-WRI-RTN THRU JDCON-WRI-EXT.
           IF  SW-JDCON NOT = ZERO
               GO TO AHNO-0
           END-IF.
       AHNO-EXT.
           EXIT.
      *****
       ASYMD-RTN.
       ASYMD-0.
           CALL "SD_Accept" USING BY REFERENCE A-SY "A-SY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ASYMD-EXT
           END-IF
           IF  ESTAT  NOT = "00" AND "01" AND "06"
               GO TO ASYMD-0
           END-IF
           CALL "SD_Output" USING "D-SY" D-SY "p" RETURNING RESU.
       ASYMD-1.
           CALL "SD_Accept" USING BY REFERENCE A-SM "A-SM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ASYMD-0
           END-IF
           IF  ESTAT  NOT = "00" AND "01" AND "06"
               GO TO ASYMD-1
           END-IF
           CALL "SD_Output" USING "D-SM" D-SM "p" RETURNING RESU.
           IF  G-SMM      > 12
               GO TO ASYMD-1
           END-IF.
       ASYMD-2.
           CALL "SD_Accept" USING BY REFERENCE A-SD "A-SD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ASYMD-1
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ASYMD-2
           END-IF
           CALL "SD_Output" USING "D-SD" D-SD "p" RETURNING RESU.
           IF  G-SMM      = 2
               IF  G-SDD      > 29
                   GO TO ASYMD-2
               END-IF
           ELSE
              IF  G-SMM      = 4 OR 6 OR 9 OR 11
                  IF  G-SDD      > 30
                      GO TO ASYMD-2
                  END-IF
              ELSE
                 IF  G-SDD      > 31
                     GO TO ASYMD-2
                 END-IF
              END-IF
           END-IF.
       ASYMD-CHK.
           IF  ZERO = G-SY2 AND G-SMM AND G-SDD
               MOVE SYSYMD        TO G-SYMD
               CALL "SD_Output" USING "D-SYMD" D-SYMD "p" RETURNING RESU
               GO TO ASYMD-CHK-1
           END-IF
      *
           MOVE ZERO          TO G-SY1
           IF  G-SY2      >= DATE-NF1 AND <= DATE-NT1
               ADD  DATE-NC1      TO G-SYY
           END-IF
           IF  G-SY2      >= DATE-NF2 AND <= DATE-NT2
               ADD  DATE-NC2      TO G-SYY
           END-IF.
       ASYMD-CHK-1.
           IF  G-SYMD  < W-FNGP OR > W-RNGP
               GO TO ASYMD-0
           END-IF
           IF  G-SMM      < 1 OR > 12
               GO TO ASYMD-1
           END-IF
           IF  G-SDD      < 1
               GO TO ASYMD-2
           END-IF
           IF  G-SMM      = 2
               IF  G-SDD      > 29
                   GO TO ASYMD-2
               END-IF
           ELSE
               IF  G-SMM      = 4 OR 6 OR 9 OR 11
                   IF  G-SDD      > 30
                       GO TO ASYMD-2
                   END-IF
               ELSE
                   IF  G-SDD      > 31
                       GO TO ASYMD-2
                   END-IF
               END-IF
           END-IF.
       ASYMD-EXT.
           EXIT.
      *****
       ANYMD-RTN.
       ANYMD-0.
           CALL "SD_Accept" USING BY REFERENCE A-NY "A-NY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ANYMD-EXT
           END-IF
           IF  ESTAT  NOT = "00" AND "01" AND "06"
               GO TO ANYMD-0
           END-IF
           CALL "SD_Output" USING "D-NY" D-NY "p" RETURNING RESU.
       ANYMD-1.
           CALL "SD_Accept" USING BY REFERENCE A-NM "A-NM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ANYMD-0
           END-IF
           IF  ESTAT  NOT = "00" AND "01" AND "06"
               GO TO ANYMD-1
           END-IF
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           IF  G-NMM      > 12
               GO TO ANYMD-1
           END-IF.
       ANYMD-2.
           CALL "SD_Accept" USING BY REFERENCE A-ND "A-ND" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ANYMD-1
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ANYMD-2
           END-IF
           CALL "SD_Output" USING "D-ND" D-ND "p" RETURNING RESU.
           IF  G-NMM      = 2
               IF  G-NDD      > 29
                   GO TO ANYMD-2
               END-IF
           ELSE
               IF  G-NMM      = 4 OR 6 OR 9 OR 11
                   IF  G-NDD      > 30
                       GO TO ANYMD-2
                   END-IF
               ELSE
                  IF  G-NDD      > 31
                      GO TO ANYMD-2
                  END-IF
               END-IF
           END-IF.
       ANYMD-CHK.
           IF  ZERO = G-NY2 AND G-NMM AND G-NDD
               MOVE SYSYMD        TO G-NYMD
               CALL "SD_Output" USING "D-NYMD" D-NYMD "p" RETURNING RESU
               GO TO ANYMD-CHK-1
           END-IF
      *
           MOVE ZERO          TO G-NY1
           IF  G-NY2      >= DATE-NF1 AND <= DATE-NT1
               ADD  DATE-NC1      TO G-NYY
           END-IF
           IF  G-NY2      >= DATE-NF2 AND <= DATE-NT2
               ADD  DATE-NC2      TO G-NYY
           END-IF.
       ANYMD-CHK-1.
           IF  G-NYMD  < W-FNGP OR > W-RNGP
               GO TO ANYMD-0
           END-IF
           IF  G-NMM      < 1 OR > 12
               GO TO ANYMD-1
           END-IF
           IF  G-NDD      < 1
               GO TO ANYMD-2
           END-IF
           IF  G-NMM      = 2
               IF  G-NDD      > 29
                   GO TO ANYMD-2
               END-IF
           ELSE
               IF G-NMM      = 4 OR 6 OR 9 OR 11
                   IF  G-NDD      > 30
                       GO TO ANYMD-2
                   END-IF
               ELSE
                   IF  G-NDD      > 31
                       GO TO ANYMD-2
                   END-IF
               END-IF
           END-IF
      *
           MOVE SPACE         TO G-HT.
           MOVE G-NMM         TO G-HT(1:2).
           MOVE "／"          TO G-HT(3:1).
           MOVE G-NDD         TO G-HT(4:2).
           MOVE "配達指定"    TO G-HT(6:4).
           CALL "SD_Output" USING "A-HT" A-HT "p" RETURNING RESU.
       ANYMD-EXT.
           EXIT.
      *****
       ATCCD-RTN.
       ATCCD-T.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ATCCD-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ATCCD-T
           END-IF.
       ATCCD-T-CHK.
           MOVE G-TCD         TO T-KEY.
      *           READ T-M      UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-TOK" INV-TOK "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO ATCCD-T
           END-IF
           MOVE T-NAME        TO G-TNM.
           CALL "SD_Output" USING "D-TNM" D-TNM "p" RETURNING RESU.
       ATCCD-C.
           CALL "SD_Accept" USING BY REFERENCE A-CCD "A-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ATCCD-T
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ATCCD-C
           END-IF.
       ATCCD-C-CHK.
           MOVE G-TCD         TO TC-TCD.
           MOVE G-CCD         TO TC-CCD.
      *           READ TC-M     UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-CHO" INV-CHO "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO ATCCD-C
           END-IF
           MOVE TC-NAME       TO G-CNM.
           CALL "SD_Output" USING "D-CNM" D-CNM "p" RETURNING RESU.
       ATCCD-S-CHK.
           MOVE TC-STC        TO STE-KEY1.
      *           READ STENM    UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" STENM_PNAME1 BY REFERENCE STE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-STE" INV-STE "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO ATCCD-C
           END-IF.
       ATCCD-EXT.
           EXIT.
      *****
       AHYMD-RTN.
           IF  STE-09     = ZERO
               MOVE ZERO       TO G-HYMD
               CALL "SD_Output" USING "C-HYMD" C-HYMD "p" RETURNING RESU
               GO TO AHYMD-EXT
           END-IF.
       AHYMD-0.
           CALL "SD_Accept" USING BY REFERENCE A-HY "A-HY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AHYMD-EXT
           END-IF
           IF  ESTAT  NOT = "00" AND "01" AND "06"
               GO TO AHYMD-0
           END-IF
           CALL "SD_Output" USING "D-HY" D-HY "p" RETURNING RESU.
       AHYMD-1.
           CALL "SD_Accept" USING BY REFERENCE A-HM "A-HM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AHYMD-0
           END-IF
           IF  ESTAT  NOT = "00" AND "01" AND "06"
               GO TO AHYMD-1
           END-IF
           CALL "SD_Output" USING "D-HM" D-HM "p" RETURNING RESU.
           IF  G-HMM      > 12
               GO TO AHYMD-1
           END-IF.
       AHYMD-2.
           CALL "SD_Accept" USING BY REFERENCE A-HD "A-HD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AHYMD-1
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AHYMD-2
           END-IF
           CALL "SD_Output" USING "D-HD" D-HD "p" RETURNING RESU.
           IF  G-HMM      = 2
               IF  G-HDD      > 29
                   GO TO AHYMD-2
               END-IF
           ELSE
               IF  G-HMM      = 4 OR 6 OR 9 OR 11
                   IF  G-HDD      > 30
                       GO TO AHYMD-2
                   END-IF
               ELSE
                   IF  G-HDD      > 31
                       GO TO AHYMD-2
                   END-IF
               END-IF
           END-IF.
       AHYMD-CHK.
           IF  ZERO       = G-HY2 AND G-HMM AND G-HDD
               MOVE ZERO          TO G-HY1
               GO TO AHYMD-EXT
           END-IF
      *
           MOVE ZERO          TO G-HY1
           IF  G-HY2      >= DATE-NF1 AND <= DATE-NT1
               ADD  DATE-NC1      TO G-HYY
           END-IF
           IF  G-HY2      >= DATE-NF2 AND <= DATE-NT2
               ADD  DATE-NC2      TO G-HYY
           END-IF.
       AHYMD-CHK-1.
           IF  G-HMM      < 1 OR > 12
               GO TO AHYMD-0
           END-IF
           IF  G-HDD      < 1
               GO TO AHYMD-0
           END-IF
           IF  G-HMM      = 2
               IF  G-HDD      > 29
                   GO TO AHYMD-0
               END-IF
           ELSE
               IF  G-HMM      = 4 OR 6 OR 9 OR 11
                   IF  G-HDD      > 30
                       GO TO AHYMD-0
                   END-IF
               ELSE
                   IF  G-HDD      > 31
                       GO TO AHYMD-0
                   END-IF
               END-IF
           END-IF.
       AHYMD-EXT.
           EXIT.
      *****
       ADKB-RTN.
       ADKB-0.
           CALL "SD_Accept" USING BY REFERENCE A-DKB "A-DKB" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ADKB-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ADKB-0
           END-IF.
       ADKB-CHK.
           IF  G-DKB  NOT = 0 AND 5 AND 6
               GO TO ADKB-0
           END-IF.
       ADKB-EXT.
           EXIT.
      *****
       ADNO-RTN.
           IF  STE-11     = ZERO
               MOVE ZERO       TO G-DNO
               CALL "SD_Output" USING "C-DNO" C-DNO "p" RETURNING RESU
               GO TO ADNO-EXT
           END-IF.
       ADNO-0.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ADNO-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ADNO-0
           END-IF.
       ADNO-CHK.
           IF  G-DNO  NOT = ZERO
               GO TO ADNO-EXT
           END-IF
      *
           PERFORM DNO-NO-RTN THRU DNO-NO-EXT.
           MOVE STE-083        TO G-DNO.
           CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU.
       ADNO-EXT.
           EXIT.
      *****
       AHCD-RTN.
           MOVE II            TO O-II.
           MOVE ZERO          TO SW-HIN
                                 SW-HIN4.
       AHCD-0.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AHCD-EXT
           END-IF
           IF  II NOT = 1
               IF  ESTAT      = "04"
                   GO TO AHCD-EXT
               END-IF
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AHCD-0
           END-IF.
       AHCD-CHK.
           IF  G-HCD(II) = ZERO
               GO TO AHCD-0
           END-IF
           IF  G-HCD(II) = O-HCD(II)
               GO TO AHCD-EXT
           END-IF
      *
           MOVE G-HCD(II)     TO HI-MHCD HI-HCD.
      *           READ HI2-M    UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO AHCD-0
           END-IF
      *           READ HI2-M    UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO AHCD-0
           END-IF
      *
           MOVE HI-NAME        TO G-HNM(II).
           CALL "SD_Output" USING "A-HNM" A-HNM "p" RETURNING RESU.
           IF  HI-ISU = ZERO
               IF  G-HCD(II)   <  999900
                   CALL "SD_Output" USING
                    "ERR-ISU" ERR-ISU "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                   GO TO AHCD-0
               END-IF
           END-IF
      *
           IF  (ACT     = 1) AND (II NOT = 6)
               IF  (II     = 1) OR
                  ((II NOT = 1) AND (G-HCD(II) NOT = G-HCD(II - 1)))
                   ADD  1              TO II
                   PERFORM VARYING II FROM II BY 1 UNTIL II > 6
                           MOVE G-HCD(O-II)    TO G-HCD(II)
                                                  O-HCD(II)
                           MOVE G-HNM(O-II)    TO G-HNM(II)
                           COMPUTE LIN = II * 2 + 6
                           CALL "SD_Arg_Match_Line" USING
                            "LIN" "2" LIN RETURNING RESU
                           CALL "SD_Output" USING
                            "A-HCD" A-HCD "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "A-HNM" A-HNM "p" RETURNING RESU
                   END-PERFORM
                   MOVE O-II           TO II
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
               END-IF
           END-IF
      *
           IF  ACT     = 1
               PERFORM VARYING KK FROM 1 BY 1
                 UNTIL (KK > 6) OR (KK = II)
                       IF  G-HCD(KK)(1:4) = G-HCD(II)(1:4)
                           MOVE G-GTAN(KK)    TO G-GTAN(II)
                           MOVE G-UTAN(KK)    TO G-UTAN(II)
                           CALL "SD_Output" USING
                            "D-GTAN" D-GTAN "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "D-UTAN" D-UTAN "p" RETURNING RESU
                           MOVE 1             TO SW-HIN4
                           EXIT PERFORM
                       END-IF
               END-PERFORM
           END-IF
      *
           MOVE 1             TO SW-HIN.
           MOVE G-HCD(II)     TO O-HCD(II).
       AHCD-EXT.
           EXIT.
      *****
       AHNM-RTN.
       AHNM-0.
           CALL "SD_Accept" USING BY REFERENCE A-HNM "A-HNM" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AHNM-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AHNM-0
           END-IF.
       AHNM-CHK.
           IF  (ACT     = 1) AND (II NOT = 6)
               IF  (II     = 1) OR
                  ((II NOT = 1) AND (G-HCD(II) NOT = G-HCD(II - 1)))
                   ADD  1              TO II
                   PERFORM VARYING II FROM II BY 1 UNTIL II > 6
                           MOVE G-HNM(O-II)    TO G-HNM(II)
                           COMPUTE LIN = II * 2 + 6
                           CALL "SD_Arg_Match_Line" USING
                            "LIN" "2" LIN RETURNING RESU
                           CALL "SD_Output" USING
                            "A-HNM" A-HNM "p" RETURNING RESU
                   END-PERFORM
                   MOVE O-II           TO II
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
               END-IF
           END-IF.
       AHNM-EXT.
           EXIT.
      *****
       AAHCD-RTN.
           IF  STE-10     = ZERO
               MOVE SPACE      TO G-AHCD(II)
               CALL "SD_Output" USING "C-AHCD" C-AHCD "p" RETURNING RESU
               GO TO AAHCD-EXT
           END-IF.
       AAHCD-0.
           CALL "SD_Accept" USING BY REFERENCE A-AHCD "A-AHCD" "X" "13"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AAHCD-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AAHCD-0
           END-IF.
       AAHCD-CHK.
           IF  G-AHCD(II) = SPACE
               CALL "SD_Output" USING
                "ERR-AHCD1" ERR-AHCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO AAHCD-EXT
           END-IF
      *
           PERFORM VARYING KK FROM 1 BY 1
             UNTIL (KK > 6) OR (KK = II)
                   IF  G-HCD(KK) = G-HCD(II)
                       IF  G-AHCD(KK) = G-AHCD(II)
                           CALL "SD_Output" USING
                            "ERR-AHCD2" ERR-AHCD2 "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                           EXIT PERFORM
                       END-IF
                   END-IF
           END-PERFORM.
           IF  (KK NOT = II) AND (KK NOT > 6)
               GO TO AAHCD-0
           END-IF.
       AAHCD-EXT.
           EXIT.
      *****
       AAHCD2-RTN.
           IF  STE-10     NOT = 2
               MOVE SPACE      TO G-AHCD2(II)
               CALL "SD_Output" USING
                "C-AHCD2" C-AHCD2 "p" RETURNING RESU
               GO TO AAHCD2-EXT
           END-IF.
       AAHCD2-0.
           CALL "SD_Accept" USING BY REFERENCE A-AHCD2 "A-AHCD2" "X" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AAHCD2-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AAHCD2-0
           END-IF.
       AAHCD2-CHK.
           IF  G-AHCD2(II) = SPACE
               CALL "SD_Output" USING
                "ERR-AHCD1" ERR-AHCD1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO AAHCD2-EXT
           END-IF
      *
           PERFORM VARYING KK FROM 1 BY 1
             UNTIL (KK > 6) OR (KK = II)
                   IF  G-HCD(KK) = G-HCD(II)
                       IF  G-AHCD2(KK) = G-AHCD2(II)
                           CALL "SD_Output" USING
                            "ERR-AHCD2" ERR-AHCD2 "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                           EXIT PERFORM
                       END-IF
                   END-IF
           END-PERFORM.
           IF  (KK NOT = II) AND (KK NOT > 6)
               GO TO AAHCD2-0
           END-IF.
       AAHCD2-EXT.
           EXIT.
      *****
       ASIZ-RTN.
       ASIZ-0.
           CALL "SD_Accept" USING BY REFERENCE A-SIZ "A-SIZ" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ASIZ-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ASIZ-0
           END-IF.
       ASIZ-CHK.
           IF  G-SIZ(II)  = ZERO
               MOVE ZERO          TO G-SKB(II)
               CALL "SD_Output" USING
                "ERR-SIZ1" ERR-SIZ1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO ASIZ-0
           END-IF
      *
           MOVE G-HCD(II)     TO SIZE-WK-HIN.
           MOVE G-SIZ(II)     TO SIZE-WK-CD.
           PERFORM SIZE-RTN THRU SIZE-EX.
           IF  SIZE-WK-SW  = 1
               CALL "SD_Output" USING
                "ERR-SIZ1" ERR-SIZ1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
           END-IF
           IF  SIZE-WK-SW  = 2
               CALL "SD_Output" USING
                "ERR-SIZ2" ERR-SIZ2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO ASIZ-0
           END-IF
           IF  SIZE-WK-SW  = 9
               CALL "SD_Output" USING
                "ERR-HIN" ERR-HIN "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO ASIZ-0
           END-IF
           MOVE SIZE-WK-KB    TO G-SKB(II).
      *
           PERFORM VARYING KK FROM 1 BY 1
             UNTIL (KK > 6) OR (KK = II)
                   IF  G-HCD(KK) = G-HCD(II)
                       IF  G-SIZ(KK) = G-SIZ(II)
                           CALL "SD_Output" USING
                            "ERR-SIZ3" ERR-SIZ3 "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                           EXIT PERFORM
                       END-IF
                   END-IF
           END-PERFORM.
           IF  (KK NOT = II) AND (KK NOT > 6)
               GO TO ASIZ-0
           END-IF.
       ASIZ-CHK-1.
           IF  (SW-HIN    = ZERO) OR (SW-HIN4     = 1)
               GO TO ASIZ-EXT
           END-IF
      *
           MOVE G-TCD         TO THT-TCD.
           MOVE G-HCD(II)     TO THT-HCD.
           MOVE G-SKB(II)     TO THT-SIZ.
      *           READ THTM     UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO          TO THT-T
           END-IF
           MOVE THT-T         TO G-GTAN(II).
           CALL "SD_Output" USING "D-GTAN" D-GTAN "p" RETURNING RESU.
      *
           IF  (ACT     = 1) AND (II NOT = 6)
               IF  (II     = 1) OR
                  ((II NOT = 1) AND (G-HCD(II) NOT = G-HCD(II - 1)))
                   ADD  1              TO II
                   PERFORM VARYING II FROM II BY 1 UNTIL II > 6
                           MOVE G-GTAN(O-II)   TO G-GTAN(II)
                           COMPUTE LIN = II * 2 + 6
                           CALL "SD_Arg_Match_Line" USING
                            "LIN" "2" LIN RETURNING RESU
                           CALL "SD_Output" USING
                            "D-GTAN" D-GTAN "p" RETURNING RESU
                   END-PERFORM
                   MOVE O-II           TO II
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
               END-IF
           END-IF.
       ASIZ-EXT.
           EXIT.
      *****
       ASUR-RTN.
       ASUR-0.
           CALL "SD_Accept" USING BY REFERENCE A-SUR "A-SUR" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO ASUR-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO ASUR-0
           END-IF
           CALL "SD_Output" USING "D-SUR" D-SUR "p" RETURNING RESU.
       ASUR-CHK.
           IF  G-SUR(II)  = ZERO
               GO TO ASUR-0
           END-IF
      *
           MOVE ZERO          TO G-GSUR.
           PERFORM VARYING JJ FROM 1 BY 1 UNTIL JJ > 6
                   ADD  G-SUR(JJ)     TO G-GSUR
           END-PERFORM.
           CALL "SD_Output" USING "D-GSUR" D-GSUR "p" RETURNING RESU.
       ASUR-EXT.
           EXIT.
      *****
       AGTAN-RTN.
       AGTAN-0.
           CALL "SD_Accept" USING BY REFERENCE A-GTAN "A-GTAN" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AGTAN-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AGTAN-0
           END-IF
           CALL "SD_Output" USING "D-GTAN" D-GTAN "p" RETURNING RESU.
       AGTAN-CHK.
           PERFORM VARYING JJ FROM 1 BY 1
             UNTIL (JJ > 6) OR (II = JJ)
                   IF  G-HCD(II) = G-HCD(JJ)
                       IF  G-GTAN(II) NOT = G-GTAN(JJ)
                           CALL "SD_Output" USING
                            "ERR-GTAN" ERR-GTAN "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                       END-IF
                       EXIT PERFORM
                   END-IF
           END-PERFORM.
      *
           IF  (ACT     = 1) AND (II NOT = 6)
              IF  (II     = 1) OR
                 ((II NOT = 1) AND (G-HCD(II) NOT = G-HCD(II - 1)))
                   ADD  1              TO II
                   PERFORM VARYING II FROM II BY 1 UNTIL II > 6
                           MOVE G-GTAN(O-II)   TO G-GTAN(II)
                           COMPUTE LIN = II * 2 + 6
                           CALL "SD_Arg_Match_Line" USING
                            "LIN" "2" LIN RETURNING RESU
                           CALL "SD_Output" USING
                            "D-GTAN" D-GTAN "p" RETURNING RESU
                   END-PERFORM
                   MOVE O-II           TO II
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
               END-IF
           END-IF
      *
           COMPUTE G-GKIN(II) = G-SUR(II) * G-GTAN(II).
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
      *
           MOVE ZERO          TO G-GGKIN.
           PERFORM VARYING JJ FROM 1 BY 1 UNTIL JJ > 6
                   ADD  G-GKIN(JJ)    TO G-GGKIN
           END-PERFORM.
           CALL "SD_Output" USING "D-GGKIN" D-GGKIN "p" RETURNING RESU.
       AGTAN-EXT.
           EXIT.
      *****
       AUTAN-RTN.
       AUTAN-0.
           CALL "SD_Accept" USING BY REFERENCE A-UTAN "A-UTAN" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AUTAN-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AUTAN-0
           END-IF
           CALL "SD_Output" USING "D-UTAN" D-UTAN "p" RETURNING RESU.
       AUTAN-CHK.
           PERFORM VARYING JJ FROM 1 BY 1
             UNTIL (JJ > 6) OR (II = JJ)
                   IF  G-HCD(II) = G-HCD(JJ)
                       IF  G-UTAN(II) NOT = G-UTAN(JJ)
                           CALL "SD_Output" USING
                            "ERR-UTAN" ERR-UTAN "p" RETURNING RESU
                           CALL "SD_Output" USING
                            "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                       END-IF
                       EXIT PERFORM
                   END-IF
           END-PERFORM.
      *
           IF  (ACT     = 1) AND (II NOT = 6)
              IF  (II     = 1) OR
                 ((II NOT = 1) AND (G-HCD(II) NOT = G-HCD(II - 1)))
                   ADD  1              TO II
                   PERFORM VARYING II FROM II BY 1 UNTIL II > 6
                           MOVE G-UTAN(O-II)   TO G-UTAN(II)
                           COMPUTE LIN = II * 2 + 6
                           CALL "SD_Arg_Match_Line" USING
                            "LIN" "2" LIN RETURNING RESU
                           CALL "SD_Output" USING
                            "D-UTAN" D-UTAN "p" RETURNING RESU
                   END-PERFORM
                   MOVE O-II           TO II
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
               END-IF
           END-IF
      *
           COMPUTE G-UKIN(II) = G-SUR(II) * G-UTAN(II).
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
      *
           MOVE ZERO          TO G-GUKIN.
           PERFORM VARYING JJ FROM 1 BY 1 UNTIL JJ > 6
                   ADD  G-UKIN(JJ)    TO G-GUKIN
           END-PERFORM.
           CALL "SD_Output" USING "D-GUKIN" D-GUKIN "p" RETURNING RESU.
       AUTAN-EXT.
           EXIT.
      *****
       AUCD-RTN.
           IF  G-DKB      = 5  OR  6
               INITIALIZE            G-UCD O-UCD G-UNM
               CALL "SD_Output" USING "C-UCD" C-UCD "p" RETURNING RESU
               CALL "SD_Output" USING "C-UNM" C-UNM "p" RETURNING RESU
               GO TO AUCD-EXT
           END-IF.
       AUCD-0.
           CALL "SD_Accept" USING BY REFERENCE A-UCD "A-UCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AUCD-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AUCD-0
           END-IF
           IF  G-UCD    =  0 OR 6
               GO TO AUCD-0
           END-IF.
       AUCD-CHK.
           IF  G-UCD      = O-UCD
               GO TO AUCD-EXT
           END-IF
      *
           MOVE SPACE         TO JCON2-KEY.
           MOVE 2             TO JCON2-01.
           MOVE G-UCD         TO JCON2-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-M01" INV-M01 "p" RETURNING RESU
               GO  TO  AUCD-0
           END-IF
           MOVE JCON2-03      TO G-UNM.
           CALL "SD_Output" USING "D-UNM" D-UNM "p" RETURNING RESU.
      *
           MOVE G-UCD         TO O-UCD.
       AUCD-EXT.
           EXIT.
       AHT-RTN.
           IF  G-DKB      = 5  OR  6
               INITIALIZE            G-HT
               CALL "SD_Output" USING "C-HT" C-HT "p" RETURNING RESU
               GO TO AHT-EXT
           END-IF.
       AHT-0.
           CALL "SD_Accept" USING BY REFERENCE A-HT "A-HT" "N" "18"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AHT-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AHT-0
           END-IF.
       AHT-CHK.
       AHT-EXT.
           EXIT.
      *****
       AKKM-RTN.
           IF  G-DKB      = 5  OR  6
               INITIALIZE            G-KKM O-KKM
               CALL "SD_Output" USING "C-KKM" C-KKM "p" RETURNING RESU
               GO TO AKKM-EXT
           END-IF.
       AKKM-0.
           CALL "SD_Accept" USING BY REFERENCE A-KKM "A-KKM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AKKM-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AKKM-0
           END-IF.
       AKKM-CHK.
           IF  G-KKM      > 12
               GO TO AKKM-0
           END-IF.
       AKKM-EXT.
           EXIT.
      *****
       AKKS-RTN.
           IF  G-DKB      = 5  OR  6
               INITIALIZE            G-KKS O-KKS
               CALL "SD_Output" USING "C-KKS" C-KKS "p" RETURNING RESU
               GO TO AKKS-EXT
           END-IF.
       AKKS-0.
           CALL "SD_Accept" USING BY REFERENCE A-KKS "A-KKS" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AKKS-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AKKS-0
           END-IF.
       AKKS-CHK.
       AKKS-EXT.
           EXIT.
      *****
       AKKG-RTN.
           IF  G-DKB      = 5  OR  6
               INITIALIZE            G-KKG O-KKG
               CALL "SD_Output" USING "C-KKG" C-KKG "p" RETURNING RESU
               GO TO AKKG-EXT
           END-IF.
       AKKG-0.
           CALL "SD_Accept" USING BY REFERENCE A-KKG "A-KKG" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AKKG-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AKKG-0
           END-IF.
       AKKG-CHK.
       AKKG-EXT.
           EXIT.
      *****
       HKKNM-RTN.
           IF  G-DKB     = 5  OR  6
               GO  TO  HKKNM-EXT
           END-IF
           IF  G-KK      = O-KK
               GO  TO  HKKNM-EXT
           END-IF
      *
           MOVE SPACE         TO G-KKNM.
           IF  G-KKM NOT = ZERO
               MOVE G-KKM         TO G-KKNM(1:2)
               MOVE "月"          TO G-KKNM(3:1)
           END-IF
           IF  G-KKS NOT = ZERO
               MOVE G-KKS         TO G-KKNM(4:2)
               MOVE "週"          TO G-KKNM(6:1)
           END-IF
           IF  G-KKG NOT = ZERO
               MOVE G-KKG         TO G-KKNM(7:3)
               MOVE "号"          TO G-KKNM(10:1)
           END-IF
           CALL "SD_Output" USING "A-KKNM" A-KKNM "p" RETURNING RESU.
           MOVE G-KK          TO O-KK.
       HKKNM-EXT.
           EXIT.
      *****
       AKKNM-RTN.
       AKKNM-0.
           CALL "SD_Accept" USING BY REFERENCE A-KKNM "A-KKNM" "N" "46"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO AKKNM-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO AKKNM-0
           END-IF.
       AKKNM-CHK.
       AKKNM-EXT.
           EXIT.
      *****
       OKC-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-OKC "A-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "MSG-SPACE" MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT      = "09"
               GO TO OKC-EXT
           END-IF
           IF  ESTAT  NOT = "01" AND "06"
               GO TO OKC-RTN
           END-IF.
       OKC-CHK.
           IF  OKC    NOT = 1 AND 9
               GO TO OKC-RTN
           END-IF.
       OKC-EXT.
           EXIT.
      ******************************************************************
      *    画面　表示処理
      ******************************************************************
       DISP-RTN.
           MOVE 1             TO SW-HAT.
           MOVE SPACE         TO HAT-KEY.
           MOVE G-HNO         TO HAT-01.
      *           START SK-HAT KEY NOT < HAT-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            SK-HAT_PNAME1 "HAT-KEY" " NOT < " HAT-KEY RETURNING RET.
           IF  RET = 1
               GO TO DISP-EXT
           END-IF.
       DISP-010.
      *           READ  SK-HAT NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DISP-EXT
           END-IF
           IF  HAT-01 NOT = G-HNO
               GO TO DISP-EXT
           END-IF
           IF  HAT-02     = ZERO OR > 6
               GO TO DISP-010
           END-IF
           MOVE ZERO          TO SW-HAT.
       DISP-020.
           MOVE HAT-25        TO G-SYMD.
           MOVE HAT-03        TO G-NYMD.
           MOVE HAT-10        TO G-HYMD.
           MOVE HAT-04        TO G-TCCD.
           MOVE G-TCD         TO T-KEY.
      *           READ T-M      UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE T-R
           END-IF
           MOVE T-NAME        TO G-TNM.
           MOVE G-TCD         TO TC-TCD.
           MOVE G-CCD         TO TC-CCD.
      *           READ TC-M     UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE TC-R
           END-IF
           MOVE TC-NAME       TO G-CNM.
           MOVE TC-STC        TO STE-KEY1.
      *           READ STENM    UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" STENM_PNAME1 BY REFERENCE STE-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-STE" INV-STE "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               INITIALIZE STE-R
           END-IF
           MOVE HAT-16        TO G-UCD.
      *
           MOVE SPACE         TO JCON2-KEY.
           MOVE 2             TO JCON2-01.
           MOVE G-UCD         TO JCON2-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE STE-R
           END-IF
           MOVE JCON2-03      TO G-UNM.
      *
           MOVE HAT-17        TO G-HT.
           MOVE HAT-18        TO G-KK.
           MOVE HAT-19        TO G-KKNM.
           MOVE HAT-23        TO G-DNO.
           MOVE HAT-24        TO G-DKB.
       DISP-030.
           MOVE HAT-07        TO G-HCD(HAT-02)
                                 O-HCD(HAT-02).
           MOVE HAT-08        TO G-SIZ(HAT-02).
           MOVE HAT-09        TO G-HNM(HAT-02).
           MOVE HAT-11        TO G-SUR(HAT-02)
           MOVE HAT-13        TO G-GTAN(HAT-02).
           COMPUTE G-GKIN(HAT-02) = G-SUR(HAT-02) * G-GTAN(HAT-02).
           MOVE HAT-14        TO G-UTAN(HAT-02).
           COMPUTE G-UKIN(HAT-02) = G-SUR(HAT-02) * G-UTAN(HAT-02).
           MOVE HAT-15        TO G-AHCD(HAT-02).
           MOVE HAT-26        TO G-AHCD2(HAT-02).
       DISP-040.
      *           READ  SK-HAT NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DISP-050
           END-IF
           IF  HAT-01 NOT = G-HNO
               GO TO DISP-050
           END-IF
           IF  HAT-02     = ZERO OR > 6
               GO TO DISP-040
           END-IF
           GO TO DISP-030.
       DISP-050.
           CALL "SD_Output" USING "D-L03" D-L03 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCCD" A-TCCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-L04" D-L04 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DKB" A-DKB "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-L22" A-L22 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-L22" D-L22 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-L23" A-L23 "p" RETURNING RESU.
      *
           PERFORM VARYING II FROM 1 BY 1
             UNTIL (II > 6) OR (G-HCD(II) = ZERO)
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
                   CALL "SD_Output" USING
                    "D-LLIN" D-LLIN "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-HCD" A-HCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-AHCD" A-AHCD "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-AHCD2" A-AHCD2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-SIZ" A-SIZ "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-LLINP1" A-LLINP1 "p" RETURNING RESU
           END-PERFORM.
      *
           MOVE ZERO          TO G-GSUR G-GGKIN G-GUKIN.
           PERFORM VARYING JJ FROM 1 BY 1 UNTIL JJ > 6
                   ADD  G-SUR(JJ)     TO G-GSUR
                   ADD  G-GKIN(JJ)    TO G-GGKIN
                   ADD  G-UKIN(JJ)    TO G-GUKIN
           END-PERFORM.
           CALL "SD_Output" USING "D-L20" D-L20 "p" RETURNING RESU.
       DISP-EXT.
           EXIT.
      ******************************************************************
      *    画面クリア処理
      ******************************************************************
       CRE-RTN.
           MOVE ZERO          TO ACT.
           MOVE SPACE         TO ACTNM.
           CALL "SD_Output" USING "C-L01" C-L01 "p" RETURNING RESU.
       CRE-1.
           MOVE ZERO          TO G-HNO.
           CALL "SD_Output" USING "C-HNO" C-HNO "p" RETURNING RESU.
       CRE-2.
           INITIALIZE            G-HED.
           CALL "SD_Output" USING "C-SYMD" C-SYMD "p" RETURNING RESU.
           CALL "SD_Output" USING "C-NYMD" C-NYMD "p" RETURNING RESU.
           CALL "SD_Output" USING "C-TNM" C-TNM "p" RETURNING RESU.
           CALL "SD_Output" USING "C-TCCD" C-TCCD "p" RETURNING RESU.
           CALL "SD_Output" USING "C-L04" C-L04 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-L05" C-L05 "p" RETURNING RESU.
       CRE-3.
           PERFORM VARYING II FROM 1 BY 1 UNTIL II > 6
                   INITIALIZE G-MEI-T(II)
                   COMPUTE LIN = II * 2 + 6
                   CALL "SD_Arg_Match_Line" USING
                    "LIN" "2" LIN RETURNING RESU
                   CALL "SD_Output" USING
                    "C-LLIN" C-LLIN "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "C-LLINP1" C-LLINP1 "p" RETURNING RESU
           END-PERFORM.
       CRE-4.
           INITIALIZE            G-FOT OKC.
           CALL "SD_Output" USING "C-L20" C-L20 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-L22" C-L22 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-L23" C-L23 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-L24" C-L24 "p" RETURNING RESU.
       CRE-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    伝票№付番処理
      *-----------------------------------------------------------------
       DNO-NO-RTN.
           MOVE TC-STC        TO STE-KEY1.
      *           READ STENM           INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" STENM_PNAME1 BY REFERENCE STE-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "INV-STE" INV-STE "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               INITIALIZE STE-R
           END-IF
      *
           IF  STE-083 = STE-082
               MOVE STE-081       TO STE-083
           ELSE
               ADD  1             TO STE-083
           END-IF
      *
           MOVE STE-083       TO G-DNO.
      *
      *           REWRITE STE-R        INVALID
      *///////////////
           CALL "DB_Update" USING
           STENM_PNAME1 STENM_LNAME STE-R RETURNING RET.
           IF  RET = 1
               MOVE "R"          TO   ERR-M
               MOVE  "STENM"     TO   ERR-F
               MOVE  STE-KEY1    TO   ERR-K
               PERFORM ERR-RTN   THRU ERR-EX
           END-IF
      *
           CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU.
       DNO-NO-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    排他制御Ｆ　追加
      *-----------------------------------------------------------------
       JDCON-WRI-RTN.
           IF  ACT = 1
               GO TO JDCON-WRI-EXT
           END-IF
      *
           MOVE ZERO          TO SW-JDCON.
           MOVE 5             TO JDCON-01.
           MOVE G-HNO         TO JDCON-02.
      *           WRITE JDCON-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JDCON_PNAME1 JDCON_LNAME JDCON-R RETURNING RET.
           IF  RET = 1
               MOVE 1             TO SW-JDCON
               CALL "SD_Output" USING
                "ERR-HNO" ERR-HNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO TO JDCON-WRI-EXT
           END-IF
      *
           PERFORM CLS-JDCON-RTN THRU CLS-JDCON-EXT.
       JDCON-WRI-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    排他制御Ｆ　削除
      *-----------------------------------------------------------------
       JDCON-DEL-RTN.
           IF  ACT = 1
               GO TO JDCON-DEL-EXT
           END-IF
      *
           MOVE 5             TO JDCON-01.
           MOVE G-HNO         TO JDCON-02.
      *           READ JDCON        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JDCON_PNAME1 BY REFERENCE JDCON-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO JDCON-DEL-EXT
           END-IF
      *           DELETE JDCON      INVALID
      *///////////////
           CALL "DB_Delete" USING JDCON_PNAME1 RETURNING RET.
           IF  RET = 1
               GO TO JDCON-DEL-EXT
           END-IF
      *
           PERFORM CLS-JDCON-RTN THRU CLS-JDCON-EXT.
       JDCON-DEL-EXT.
           EXIT.
      *
       CLS-JDCON-RTN.
       CLS-JDCON-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    終了　処理　
      *-----------------------------------------------------------------
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE STENM_IDLST STENM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JDCON_IDLST JDCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SK-HAT_IDLST SK-HAT_PNAME1.
       END-EX.
           EXIT.
      *-----------------------------------------------------------------
      *    更新　処理　
      *-----------------------------------------------------------------
       UPDT-RTN.
           IF  ACT NOT = 1
               PERFORM HAT-DEL-RTN THRU HAT-DEL-EXT
           END-IF
      *
           IF  ACT NOT = 3
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  000
               PERFORM CON-UPD-RTN THRU CON-UPD-EXT
               PERFORM HAT-UPD-RTN THRU HAT-UPD-EXT
                       VARYING II FROM 1 BY 1
                         UNTIL (II > 6) OR (G-HCD(II) = ZERO)
           END-IF.
       UPDT-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    コントロールＦ　更新
      *-----------------------------------------------------------------
       CON-UPD-RTN.
           IF  ACT NOT =  1
               GO TO CON-UPD-EXT
           END-IF.
       CON-UPD-010.
           MOVE ZERO          TO SW-CON.
           MOVE 1             TO JCON1-01.
           MOVE 1             TO JCON1-02.
      *           READ JCON        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               INITIALIZE JCON1-R
               MOVE 1             TO JCON1-01
               MOVE 1             TO JCON1-02
               MOVE 1             TO SW-CON
           END-IF.
       CON-UPD-020.
           IF  SW-CON NOT = ZERO
               MOVE 100001        TO JCON1-03
               MOVE 300001        TO JCON1-04
               MOVE 500001        TO JCON1-05
           ELSE
               IF  JCON1-05 = 599999
                   MOVE 500001        TO JCON1-05
               ELSE
                   ADD  1             TO JCON1-05
               END-IF
           END-IF
      *
           MOVE JCON1-05      TO G-HNO.
      *
           IF  SW-CON NOT = ZERO
               PERFORM WRI-CON-RTN THRU WRI-CON-EXT
           ELSE
               PERFORM REW-CON-RTN THRU REW-CON-EXT
           END-IF.
       CON-UPD-030.
           MOVE SPACE         TO HAT-KEY.
           MOVE G-HNO         TO HAT-01.
      *           START SK-HAT KEY NOT < HAT-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            SK-HAT_PNAME1 "HAT-KEY" " NOT < " HAT-KEY RETURNING RET.
           IF  RET = 1
               GO TO CON-UPD-EXT
           END-IF
      *           READ  SK-HAT NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO CON-UPD-EXT
           END-IF
           IF  HAT-01     = G-HNO
               GO TO CON-UPD-010
           END-IF.
       CON-UPD-EXT.
           EXIT.
      *
       REW-CON-RTN.
      *           REWRITE JCON1-R        INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE "R"           TO ERR-M
               MOVE "JCON"        TO ERR-F
               MOVE JCON1-KEY     TO ERR-K
               PERFORM ERR-RTN  THRU ERR-EX
           END-IF.
       REW-CON-EXT.
           EXIT.
      *
       WRI-CON-RTN.
      *           WRITE   JCON1-R        INVALID
      *//////////////
           CALL "DB_Insert" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE "W"           TO ERR-M
               MOVE "JCON"        TO ERR-F
               MOVE JCON1-KEY     TO ERR-K
               PERFORM ERR-RTN  THRU ERR-EX
           END-IF
      *
           PERFORM CLS-CON-RTN THRU CLS-CON-EXT.
       WRI-CON-EXT.
           EXIT.
      *
       CLS-CON-RTN.
       CLS-CON-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    生協発注データ　削除処理
      *-----------------------------------------------------------------
       HAT-DEL-RTN.
           MOVE SPACE         TO HAT-KEY.
           MOVE G-HNO         TO HAT-01.
      *           START SK-HAT KEY NOT < HAT-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            SK-HAT_PNAME1 "HAT-KEY" " NOT < " HAT-KEY RETURNING RET.
           IF  RET = 1
               GO TO HAT-DEL-EXT
           END-IF.
       HAT-DEL-010.
      *           READ  SK-HAT NEXT        AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SK-HAT_PNAME1 BY REFERENCE HAT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO HAT-DEL-EXT
           END-IF
           IF  HAT-01 NOT = G-HNO
               GO TO HAT-DEL-EXT
           END-IF
      *
           PERFORM DEL-HAT-RTN THRU DEL-HAT-EXT.
           GO TO HAT-DEL-010.
       HAT-DEL-EXT.
           EXIT.
      *-----------------------------------------------------------------
      *    生協発注データ　更新処理
      *-----------------------------------------------------------------
       HAT-UPD-RTN.
           MOVE SPACE         TO HAT-R.
           INITIALIZE            HAT-R.
      *
           MOVE G-HNO         TO HAT-01.
           MOVE II            TO HAT-02.
           MOVE G-SYMD        TO HAT-25.
           MOVE G-NYMD        TO HAT-03.
           MOVE G-TCCD        TO HAT-04.
           MOVE 1             TO HAT-05.
           MOVE TC-STC        TO HAT-06.
           MOVE G-HCD(II)     TO HAT-07.
           MOVE G-SIZ(II)     TO HAT-08.
           MOVE G-HNM(II)     TO HAT-09.
           MOVE G-HYMD        TO HAT-10.
           MOVE G-SUR(II)     TO HAT-11.
           MOVE G-GTAN(II)    TO HAT-13.
           MOVE G-UTAN(II)    TO HAT-14.
           MOVE G-AHCD(II)    TO HAT-15.
           MOVE G-AHCD2(II)   TO HAT-26.
           MOVE G-UCD         TO HAT-16.
           MOVE G-HT          TO HAT-17.
           MOVE G-KK          TO HAT-18.
           MOVE G-KKNM        TO HAT-19.
           MOVE G-DNO         TO HAT-23.
           MOVE G-DKB         TO HAT-24.
           MOVE SYSYMD        TO HAT-21.
           MOVE ACT           TO HAT-22.
           IF  STE-12       =  0
               MOVE  0            TO HAT-97
           END-IF
           IF  STE-12       =  1
               MOVE  2            TO HAT-97
           END-IF
           IF  STE-12       =  2
               IF  G-DKB        =  0
                   MOVE  0            TO HAT-97
               ELSE
                   MOVE  1            TO HAT-97
               END-IF
           END-IF
           IF  STE-12       =  3
               IF  G-DKB        =  0
                   MOVE  1            TO HAT-97
               ELSE
                   MOVE  0            TO HAT-97
               END-IF
           END-IF
      *
           PERFORM WRI-HAT-RTN THRU WRI-HAT-EXT.
       HAT-UPD-EXT.
           EXIT.
      *
       DEL-HAT-RTN.
      *           DELETE  SK-HAT         INVALID
      *///////////////
           CALL "DB_Delete" USING SK-HAT_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "D"           TO ERR-M
               MOVE "SK-HAT"      TO ERR-F
               MOVE HAT-KEY       TO ERR-K
               PERFORM ERR-RTN  THRU ERR-EX
           END-IF
      *
           PERFORM CLS-HAT-RTN THRU CLS-HAT-EXT.
       DEL-HAT-EXT.
           EXIT.
      *
       REW-HAT-RTN.
      *           REWRITE HAT-R          INVALID
      *///////////////
           CALL "DB_Update" USING
            SK-HAT_PNAME1 SK-HAT_LNAME HAT-R RETURNING RET.
           IF  RET = 1
               MOVE "R"           TO ERR-M
               MOVE "SK-HAT"      TO ERR-F
               MOVE HAT-KEY       TO ERR-K
               PERFORM ERR-RTN  THRU ERR-EX
           END-IF.
       REW-HAT-EXT.
           EXIT.
      *
       WRI-HAT-RTN.
      *           WRITE   HAT-R          INVALID
      *//////////////
           CALL "DB_Insert" USING
            SK-HAT_PNAME1 SK-HAT_LNAME HAT-R RETURNING RET.
           IF  RET = 1
               MOVE "W"           TO ERR-M
               MOVE "SK-HAT"      TO ERR-F
               MOVE HAT-KEY       TO ERR-K
               PERFORM ERR-RTN  THRU ERR-EX
           END-IF
      *
           PERFORM CLS-HAT-RTN THRU CLS-HAT-EXT.
       WRI-HAT-EXT.
           EXIT.
      *
       CLS-HAT-RTN.
       CLS-HAT-EXT.
           EXIT.
      *
       COPY LPMSG.
       COPY LPSIZC.
