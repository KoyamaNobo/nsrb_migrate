       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR445L.
       AUTHOR.                     AKEMI.K.
      *===============================================================*
      *    補助　元帳                                                 *
      *                            --- 91/03/08 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       INPUT-OUTPUT               SECTION.
       DATA                       DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
       77  PCNT                PIC  9(05).
       77  PRINT-CNT           PIC  9(05).
       77  K20                 PIC  X(05) VALUE X"1A24212474".
       77  K15                 PIC  X(05) VALUE X"1A24212078".
       77  WKZE                PIC  9(10) VALUE ZERO.
       77  WKSP                PIC  X(10) VALUE SPACE.
       77  WKNSP               PIC  N(20) VALUE SPACE.
       01  CRT-WK.
           02  W-121F.
               03  W-11F       PIC  9(04).
               03  W-21F       PIC  9(04).
           02  W-121T.
               03  W-11T       PIC  9(04).
               03  W-21T       PIC  9(04).
           02  W-122F.
               03  W-12F       PIC  9(04).
               03  W-22F       PIC  9(04).
           02  W-122T.
               03  W-12T       PIC  9(04).
               03  W-22T       PIC  9(04).
           02  W-123F.
               03  W-13F       PIC  9(04).
               03  W-23F       PIC  9(04).
           02  W-123T.
               03  W-13T       PIC  9(04).
               03  W-23T       PIC  9(04).
           02  W-124F.
               03  W-14F       PIC  9(04).
               03  W-24F       PIC  9(04).
           02  W-124T.
               03  W-14T       PIC  9(04).
               03  W-24T       PIC  9(04).
           02  W-125F.
               03  W-15F       PIC  9(04).
               03  W-25F       PIC  9(04).
           02  W-125T.
               03  W-15T       PIC  9(04).
               03  W-25T       PIC  9(04).
           02  W-126F.
               03  W-16F       PIC  9(04).
               03  W-26F       PIC  9(04).
           02  W-126T.
               03  W-16T       PIC  9(04).
               03  W-26T       PIC  9(04).
           02  W-03F.
             03  W-03FY        PIC  9(04).
             03  W-03FYL  REDEFINES W-03FY.
               04  W-03FY1     PIC  9(02).
               04  W-03FY2     PIC  9(02).
             03  W-03FM        PIC  9(02).
             03  W-03FD        PIC  9(02).
           02  W-03FR REDEFINES W-03F     PIC  9(08).
           02  W-03T.
             03  W-03TY        PIC  9(04).
             03  W-03TYL  REDEFINES W-03TY.
               04  W-03TY1     PIC  9(02).
               04  W-03TY2     PIC  9(02).
             03  W-03TM        PIC  9(02).
             03  W-03TD        PIC  9(02).
           02  W-03TR REDEFINES W-03T     PIC  9(08).
           02  W-03TL REDEFINES W-03T.
             03  F             PIC  9(02).
             03  W-03TS        PIC  9(06).
           02  W-OKC           PIC  X(01).
       01  WORK-AREA.
           02  I               PIC  9(02).
           02  SOE             PIC  9(02).
           02  FI              PIC  9(02).
           02  TI              PIC  9(02).
           02  ERR-SW          PIC  9(01).
           02  INV-SW          PIC  9(01).
           02  W-ZENKI         PIC S9(11).
           02  W-ZAN           PIC S9(11).
           02  W-KASI          PIC S9(11).
           02  W-KARI          PIC S9(11).
           02  FROM-YMD.
             03  FROM-YM.
               04  FROM-Y      PIC  9(04).
               04  FROM-M      PIC  9(02).
             03  FROM-D        PIC  9(02).
           02  TO-YMD.
             03  TO-YM.
               04  TO-Y        PIC  9(04).
               04  TO-M        PIC  9(02).
             03  TO-D          PIC  9(02).
           02  TO-YMD1.
             03  TO-YM1.
               04  TO-Y1       PIC  9(04).
               04  TO-M1       PIC  9(02).
             03  TO-D1         PIC  9(02).
           02  W-YMD.
             03  W-Y           PIC  9(04).
             03  W-MD.
               04  W-M         PIC  9(02).
               04  W-D         PIC  9(02).
           02  SYSDATE.
             03  SYS-YY        PIC  9(02).
             03  SYS-MM        PIC  9(02).
             03  SYS-DD        PIC  9(02).
           02  WK-YMD.
             03  WK-YY         PIC  9(04).
             03  WK-YYL  REDEFINES WK-YY.
               04  WK-YY1      PIC  9(02).
               04  WK-YY2      PIC  9(02).
             03  WK-MM         PIC  9(02).
             03  WK-DD         PIC  9(02).
           02  WK-ZZ           PIC  Z9.
           02  WK-ZZZZZ        PIC  ZZZZ9.
           02  WK-9999         PIC  9(04).
       01  OLD-KEY.
           02  OHTRDATE3.
             03  OHTRDATE3YY   PIC  9(04).
             03  OHTRDATE3MM   PIC  9(02).
             03  OHTRDATE3DD   PIC  9(02).
           02  OHJUNLNO3       PIC  9(06).
           02  OZI             PIC  9(02).
       01  TOT-WK.
           02  TOT-ITEM OCCURS 3.
             03  TOT-KR        PIC S9(12).
             03  TOT-KS        PIC S9(12).
       01  MID03.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  M3-YY           PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  M3-MM           PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  M3-DD           PIC N(02).
           02  FILLER          PIC N(03) VALUE "日作成".
           02  FILLER          PIC X(23) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC0".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC N(09)
               VALUE "補　助　元　帳".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC1".
           02  FILLER          PIC X(53) VALUE SPACE.
           02  M3-PCNT         PIC N(05).
           02  FILLER          PIC N(01) VALUE "頁".
       01  MID05.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(28) VALUE SPACE.
           02  FILLER          PIC N(03) VALUE "（　自".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-YY1          PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  FILLER          PIC X(01).
           02  M5-MM1          PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  FILLER          PIC X(01).
           02  M5-DD1          PIC N(02).
           02  FILLER          PIC N(04) VALUE "日　　至".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-YY2          PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-MM2          PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M5-DD2          PIC N(02).
           02  FILLER          PIC N(03) VALUE "日　）".
       01  MID06.
           02  FILLER          PIC X(02) VALUE X"1AC0".
           02  FILLER          PIC X(96) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC1".
       01  MID07.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(06) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
       01  MID08.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "勘定科目".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M8-01           PIC N(10).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "（".
           02  M8-02           PIC N(04).
           02  FILLER          PIC N(01) VALUE "）".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "補助科目".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M8-03           PIC N(10).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "（".
           02  M8-04           PIC N(04).
           02  FILLER          PIC N(01) VALUE "）".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M8-05           PIC N(02).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
       01  MID09.
           02  FILLER          PIC X(04) VALUE X"1AC01AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(06) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(36) VALUE SPACE.
           02  FILLER          PIC X(04) VALUE X"1AC1".
       01  MID10.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(11) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(24) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(32) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(04) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
       01  MID11.
           02  FILLER          PIC X(04) VALUE X"1AC01AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "日".
           02  FILLER          PIC X(03) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "付".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "伝票番号".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(07) VALUE "取　引　先　名".
           02  FILLER          PIC X(03) VALUE SPACE.
           02  FILLER          PIC X(06) VALUE "(部門)".
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(07) VALUE "借　方　金　額".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(07) VALUE "貸　方　金　額".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(07) VALUE "残　高　金　額".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(09) VALUE SPACE.
           02  FILLER          PIC N(07) VALUE "摘　　　　　要".
           02  FILLER          PIC X(09) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "税".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(04) VALUE X"1AC11AC2".
       01  YOKO.
           02  FILLER          PIC X(02) VALUE X"1AC0".
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(11) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(24) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(32) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(04) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(02) VALUE X"1AC1".
       01  TATEP1.
           02  FILLER          PIC X(05) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(11) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(29) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(42) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(04) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
       01  TATEP2.
           02  FILLER          PIC X(05) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(11) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(29) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(17) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(34) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(04) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(05) VALUE SPACE.
      *********
       COPY LWMSG_PR.
       COPY LIBFDD.
       COPY KANGEL.
       COPY ACCUNT.
       COPY LHOZAN.
       COPY FCTL.
       01  SDH_PR445L.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F              PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY SIWAKH.
       77  F                   PIC  X(001).
       01  P-F.
           02  P-R                 PIC X(200).
           02  P1-R  REDEFINES  P-R.
               03  P1-K15          PIC X(05).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P1-01.
                 04  P1-01Y        PIC 9(2).
                 04  P1-01YH       PIC X(01).
                 04  P1-01M        PIC Z9.
                 04  P1-01MH       PIC X(01).
                 04  P1-01D        PIC Z9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P1-02           PIC 9(06).
               03  P1-02H          PIC X(01).
               03  P1-03           PIC 9(02).
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P1-04           PIC N(10).
               03  FILLER          PIC X(02).
               03  P1-05F          PIC X(01).
               03  P1-05           PIC 9(04).
               03  P1-05T          PIC X(01).
               03  FILLER          PIC X(02).
               03  P1-06           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  P1-07           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  P1-08           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P1-09           PIC N(20).
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(02).
               03  P1-10           PIC X(01).
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  P1-K20          PIC X(05).
           02  P2-R  REDEFINES  P-R.
               03  P2-K15          PIC X(05).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(10).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(11).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P2-04           PIC N(10).
               03  FILLER          PIC X(08).
               03  FILLER          PIC X(02).
               03  P2-06           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  P2-07           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  P2-08           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P2-09           PIC N(04).
               03  FILLER          PIC X(08).
               03  P2-99           PIC ----,---,---,--9.
               03  FILLER          PIC X(01).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(04).
               03  FILLER          PIC X(02).
               03  P2-K20          PIC X(05).
       77  F                       PIC  X(001).
      *********
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  FILLER.
               03  ACP-W121F.
                   04  ACP-W11F              PIC 9(04).
                   04  ACP-W21F              PIC 9(04).
               03  ACP-W121T.
                   04  ACP-W11T              PIC 9(04).
                   04  ACP-W21T              PIC 9(04).
           02  FILLER.
               03  ACP-W122F.
                   04  ACP-W12F              PIC 9(04).
                   04  ACP-W22F              PIC 9(04).
               03  ACP-W122T.
                   04  ACP-W12T              PIC 9(04).
                   04  ACP-W22T              PIC 9(04).
           02  FILLER.
               03  ACP-W123F.
                   04  ACP-W13F              PIC 9(04).
                   04  ACP-W23F              PIC 9(04).
               03  ACP-W123T.
                   04  ACP-W13T              PIC 9(04).
                   04  ACP-W23T              PIC 9(04).
           02  FILLER.
               03  ACP-W124F.
                   04  ACP-W14F              PIC 9(04).
                   04  ACP-W24F              PIC 9(04).
               03  ACP-W124T.
                   04  ACP-W14T              PIC 9(04).
                   04  ACP-W24T              PIC 9(04).
           02  FILLER.
               03  ACP-W125F.
                   04  ACP-W15F              PIC 9(04).
                   04  ACP-W25F              PIC 9(04).
               03  ACP-W125T.
                   04  ACP-W15T              PIC 9(04).
                   04  ACP-W25T              PIC 9(04).
           02  FILLER.
               03  ACP-W126F.
                   04  ACP-W16F              PIC 9(04).
                   04  ACP-W26F              PIC 9(04).
               03  ACP-W126T.
                   04  ACP-W16T              PIC 9(04).
                   04  ACP-W26T              PIC 9(04).
           02  ACP-W03F.
               03  ACP-W03FY                 PIC 9(02).
               03  ACP-W03FM                 PIC 9(02).
               03  ACP-W03FD                 PIC 9(02).
           02  ACP-W03T.
               03  ACP-W03TY                 PIC 9(02).
               03  ACP-W03TM                 PIC 9(02).
               03  ACP-W03TD                 PIC 9(02).
           02  ACP-WOKC                      PIC X(01).
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER    PIC X(10).
               03  FILLER    PIC N(04) VALUE "補助元帳".
           02  DSP-04.
               03  FILLER    PIC N(04) VALUE "ＦＲＯＭ".
               03  FILLER    PIC N(02) VALUE "ＴＯ".
           02  DSP-06.
               03  FILLER    PIC N(02) VALUE "科目".
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(05) VALUE "補助コード".
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(01) VALUE   "-".
           02  DSP-08.
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(01) VALUE   "-".
           02  DSP-10.
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(01) VALUE   "-".
           02  DSP-12.
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(01) VALUE   "-".
           02  DSP-14.
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(01) VALUE   "-".
           02  DSP-16.
               03  FILLER    PIC X(01) VALUE   "-".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(01) VALUE   "-".
           02  DSP-18.
               03  FILLER    PIC N(05) VALUE "日　　　付".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(04) VALUE "/  /".
               03  FILLER    PIC X(04) VALUE "/  /".
           02  DSP-24.
               03  FILLER    PIC X(18) VALUE "確認 OK=1,NO=9 ( )".
       01  DSP-NEN.
           02  DSP-TSUKI.
               03  FILLER    PIC  N(01)  VALUE  "年".
               03  FILLER    PIC  N(02)  VALUE  "月度".
               03  FILLER    PIC  9(02).
               03  FILLER    PIC  9(02).
       01  MG-AREA.
           02  MG-01         PIC N(07)
               VALUE "日付期間エラー".
       01  SP-AREA.
           02  SP-06.
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-08.
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-10.
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-12.
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-14.
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-16.
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-20.
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
           02  SP-24.
               03  FILLER   PIC X(01).
       COPY LSMSG_PR.
       COPY LIBSCR.
      **********
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C"  RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "6" "0" "16" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W121F" " " "6" "0" "8" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W11F" "9" "6" "27" "4" " " "ACP-W121F" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W11F" BY REFERENCE W-11F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W21F" "9" "6" "32" "4" "ACP-W11F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W21F" BY REFERENCE W-21F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W121T" " " "6" "0" "8" "ACP-W121F" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W11T" "9" "6" "47" "4" " " "ACP-W121T" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W11T" BY REFERENCE W-11T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W21T" "9" "6" "52" "4" "ACP-W11T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W21T" BY REFERENCE W-21T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "8" "0" "16" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W122F" " " "8" "0" "8" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W12F" "9" "8" "27" "4" " " "ACP-W122F" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W12F" BY REFERENCE W-12F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W22F" "9" "8" "32" "4" "ACP-W12F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W22F" BY REFERENCE W-22F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W122T" " " "8" "0" "8" "ACP-W122F" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W12T" "9" "8" "47" "4" " " "ACP-W122T" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W12T" BY REFERENCE W-12T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W22T" "9" "8" "52" "4" "ACP-W12T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W22T" BY REFERENCE W-22T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACP-AREA" " " "10" "0" "16" "02ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W123F" " " "10" "0" "8" " " "03ACP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W13F" "9" "10" "27" "4" " " "ACP-W123F"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W13F" BY REFERENCE W-13F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W23F" "9" "10" "32" "4" "ACP-W13F" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W23F" BY REFERENCE W-23F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W123T" " " "10" "0" "8" "ACP-W123F" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W13T" "9" "10" "47" "4" " " "ACP-W123T"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W13T" BY REFERENCE W-13T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W23T" "9" "10" "52" "4" "ACP-W13T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W23T" BY REFERENCE W-23T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ACP-AREA" " " "12" "0" "16" "03ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W124F" " " "12" "0" "8" " " "04ACP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W14F" "9" "12" "27" "4" " " "ACP-W124F"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W14F" BY REFERENCE W-14F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W24F" "9" "12" "32" "4" "ACP-W14F" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W24F" BY REFERENCE W-24F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W124T" " " "12" "0" "8" "ACP-W124F" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W14T" "9" "12" "47" "4" " " "ACP-W124T"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W14T" BY REFERENCE W-14T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W24T" "9" "12" "52" "4" "ACP-W14T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W24T" BY REFERENCE W-24T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05ACP-AREA" " " "14" "0" "16" "04ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W125F" " " "14" "0" "8" " " "05ACP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W15F" "9" "14" "27" "4" " " "ACP-W125F"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W15F" BY REFERENCE W-15F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W25F" "9" "14" "32" "4" "ACP-W15F" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W25F" BY REFERENCE W-25F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W125T" " " "14" "0" "8" "ACP-W125F" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W15T" "9" "14" "47" "4" " " "ACP-W125T"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W15T" BY REFERENCE W-15T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W25T" "9" "14" "52" "4" "ACP-W15T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W25T" BY REFERENCE W-25T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06ACP-AREA" " " "16" "0" "16" "05ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W126F" " " "16" "0" "8" " " "06ACP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W16F" "9" "16" "27" "4" " " "ACP-W126F"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W16F" BY REFERENCE W-16F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W26F" "9" "16" "32" "4" "ACP-W16F" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W26F" BY REFERENCE W-26F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W126T" " " "16" "0" "8" "ACP-W126F" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W16T" "9" "16" "47" "4" " " "ACP-W126T"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W16T" BY REFERENCE W-16T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W26T" "9" "16" "52" "4" "ACP-W16T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W26T" BY REFERENCE W-26T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03F" " " "18" "0" "6" "06ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03FY" "9" "18" "27" "2" " " "ACP-W03F"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W03FY" BY REFERENCE W-03FY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03FM" "9" "18" "30" "2" "ACP-W03FY" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W03FM" BY REFERENCE W-03FM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03FD" "9" "18" "33" "2" "ACP-W03FM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W03FD" BY REFERENCE W-03FD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03T" " " "18" "0" "6" "ACP-W03F" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03TY" "9" "18" "47" "2" " " "ACP-W03T"
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W03TY" BY REFERENCE W-03TY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03TM" "9" "18" "50" "2" "ACP-W03TY" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W03TM" BY REFERENCE W-03TM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W03TD" "9" "18" "53" "2" "ACP-W03TM" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W03TD" BY REFERENCE W-03TD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-WOKC" "X" "24" "77" "1" "ACP-W03T" " "
            RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-WOKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "18" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "36" "10" " " "DSP-01" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-01" BY REFERENCE WKSP "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "N" "1" "37" "8" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "4" "0" "12" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "N" "4" "27" "8" " " "DSP-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-04" "N" "4" "49" "4" "01DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06" " " "6" "0" "19" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06" "N" "6" "7" "4" " " "DSP-06" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-06" "X" "6" "11" "1" "01DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-06" "N" "6" "12" "10" "02DSP-06" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-06" "X" "6" "31" "1" "03DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-06" "N" "6" "40" "2" "04DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-06" "X" "6" "51" "1" "05DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" " " "8" "0" "4" "DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-08" "X" "8" "31" "1" " " "DSP-08" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-08" "N" "8" "40" "2" "01DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-08" "X" "8" "51" "1" "02DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" " " "10" "0" "4" "DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-10" "X" "10" "31" "1" " " "DSP-10" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-10" "N" "10" "40" "2" "01DSP-10" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-10" "X" "10" "51" "1" "02DSP-10" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" " " "12" "0" "4" "DSP-10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-12" "X" "12" "31" "1" " " "DSP-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-12" "N" "12" "40" "2" "01DSP-12" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-12" "X" "12" "51" "1" "02DSP-12" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-14" " " "14" "0" "4" "DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-14" "X" "14" "31" "1" " " "DSP-14" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-14" "N" "14" "40" "2" "01DSP-14" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-14" "X" "14" "51" "1" "02DSP-14" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-16" " " "16" "0" "4" "DSP-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-16" "X" "16" "31" "1" " " "DSP-16" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-16" "N" "16" "40" "2" "01DSP-16" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-16" "X" "16" "51" "1" "02DSP-16" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-18" " " "18" "0" "20" "DSP-16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-18" "N" "18" "11" "10" " " "DSP-18" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-18" "N" "18" "40" "2" "01DSP-18" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-18" "X" "18" "29" "4" "02DSP-18" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-18" "X" "18" "49" "4" "03DSP-18" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-24" " " "24" "0" "18" "DSP-18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-24" "X" "24" "61" "18" " " "DSP-24" RETURNING RESU.
      *DSP-NEN
       CALL "SD_Init" USING 
            "DSP-NEN" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TSUKI" " " "1" "0" "10" " " "DSP-NEN" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-TSUKI" "N" "1" "4" "2" " " "DSP-TSUKI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-TSUKI" "N" "1" "8" "4" "01DSP-TSUKI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-TSUKI" "9" "1" "2" "2" "02DSP-TSUKI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-TSUKI" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-TSUKI" "9" "1" "6" "2" "03DSP-TSUKI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-TSUKI" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
      *MG-AREA
       CALL "SD_Init" USING
            "MG-AREA" " " "0" "0" "14" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "MG-01" "N" "24" "2" "14" " " "MG-AREA"  RETURNING RESU.
      *SP-AREA
       CALL "SD_Init" USING 
            "SP-AREA" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-06" " " "6" "0" "16" " " "SP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-06" "Z" "6" "27" "4" " " "SP-06" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-06" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-06" "Z" "6" "32" "4" "01SP-06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-06" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-06" "Z" "6" "47" "4" "02SP-06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-06" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-06" "Z" "6" "52" "4" "03SP-06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-06" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-08" " " "8" "0" "16" "SP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-08" "Z" "8" "27" "4" " " "SP-08" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-08" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-08" "Z" "8" "32" "4" "01SP-08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-08" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-08" "Z" "8" "47" "4" "02SP-08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-08" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-08" "Z" "8" "52" "4" "03SP-08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-08" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-10" " " "10" "0" "16" "SP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-10" "Z" "10" "27" "4" " " "SP-10" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-10" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-10" "Z" "10" "32" "4" "01SP-10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-10" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-10" "Z" "10" "47" "4" "02SP-10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-10" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-10" "Z" "10" "52" "4" "03SP-10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-10" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-12" " " "12" "0" "16" "SP-10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-12" "Z" "12" "27" "4" " " "SP-12" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-12" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-12" "Z" "12" "32" "4" "01SP-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-12" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-12" "Z" "12" "47" "4" "02SP-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-12" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-12" "Z" "12" "52" "4" "03SP-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-12" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-14" " " "14" "0" "16" "SP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-14" "Z" "14" "27" "4" " " "SP-14" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-14" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-14" "Z" "14" "32" "4" "01SP-14" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-14" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-14" "Z" "14" "47" "4" "02SP-14" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-14" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-14" "Z" "14" "52" "4" "03SP-14" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-14" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-16" " " "16" "0" "16" "SP-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-16" "Z" "16" "27" "4" " " "SP-16" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-16" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-16" "Z" "16" "32" "4" "01SP-16" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-16" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-16" "Z" "16" "47" "4" "02SP-16" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-16" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-16" "Z" "16" "52" "4" "03SP-16" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-16" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-20" " " "20" "0" "12" "SP-16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-20" "Z" "20" "27" "2" " " "SP-20" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-20" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-20" "Z" "20" "30" "2" "01SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-20" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-20" "Z" "20" "33" "2" "02SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-20" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-20" "Z" "20" "47" "2" "03SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-20" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05SP-20" "Z" "20" "50" "2" "04SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05SP-20" BY REFERENCE WKZE "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06SP-20" "Z" "20" "53" "2" "05SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "06SP-20" BY REFERENCE WKZE "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-24" " " "24" "0" "1" "SP-20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-24" "X" "24" "77" "1" " " "SP-24" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-24" BY REFERENCE WKSP "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR000.
           PERFORM INI-RTN THRU INI-EX.
           IF  ERR-SW = 1
               GO TO MR999
           END-IF.
       MR010.
           PERFORM W10-RTN THRU W10-EX.
           IF  ESTAT = "P9"
               GO TO MR999
           END-IF.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-W121F" ACP-W121F "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-W121T" ACP-W121T "p"
                                         RETURNING RESU.
           IF  W-12F NOT = ZERO
               CALL "SD_Output" USING "ACP-W122F" ACP-W122F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W122T" ACP-W122T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-13F NOT = ZERO
               CALL "SD_Output" USING "ACP-W123F" ACP-W123F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W123T" ACP-W123T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-14F NOT = ZERO
               CALL "SD_Output" USING "ACP-W124F" ACP-W124F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W124T" ACP-W124T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-15F NOT = ZERO
               CALL "SD_Output" USING "ACP-W125F" ACP-W125F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W125T" ACP-W125T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-16F NOT = ZERO
               CALL "SD_Output" USING "ACP-W126F" ACP-W126F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W126T" ACP-W126T "p"
                                         RETURNING RESU
           END-IF.
       MR050.
           PERFORM W03F-RTN THRU W03F-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
       MR060.
           PERFORM W03T-RTN THRU W03T-EX.
           IF  ESTAT = "09"
               GO TO MR050
           END-IF.
       MR090.
           PERFORM WOKC-RTN THRU WOKC-EX.
           IF  ESTAT = "09"
               GO TO MR060
           END-IF.
           IF  W-OKC = "9"
               CALL "SD_Output" USING "SP-AREA" SP-AREA "p"
                                          RETURNING RESU
               GO TO MR010
           END-IF.
           PERFORM OPEN-RTN THRU OPEN-EX.
           MOVE W-03FY2    TO M5-YY1.
           MOVE W-03FM     TO WK-ZZ.
           MOVE WK-ZZ      TO M5-MM1.
           MOVE W-03FD     TO WK-ZZ.
           MOVE WK-ZZ      TO M5-DD1.
           MOVE W-03TY2    TO M5-YY2.
           MOVE W-03TM     TO WK-ZZ.
           MOVE WK-ZZ      TO M5-MM2.
           MOVE W-03TD     TO WK-ZZ.
           MOVE WK-ZZ      TO M5-DD2.
           MOVE W-11F     TO AM-KEY.
      *           START AM KEY NOT LESS AM-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            AM_PNAME1 "AM-KEY" " NOT LESS " AM-KEY RETURNING RET.
           IF  RET = 1
               GO TO MR900
           END-IF.
       MR095.
      *           READ AM NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR150
           END-IF.
           IF  HOJYO  NOT =  1
               GO  TO  MR095
           END-IF.
           IF  AM-KEY >= W-11F AND <= W-11T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-12F AND <= W-12T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-13F AND <= W-13T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-14F AND <= W-14T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-15F AND <= W-15T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-16F AND <= W-16T
               GO TO MR110
           END-IF.
           GO TO MR095.
       MR100.
      *           READ AM NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR900
           END-IF.
           IF  HOJYO  NOT =  1
               GO  TO  MR100
           END-IF.
           IF  AM-KEY >= W-11F AND <= W-11T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-12F AND <= W-12T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-13F AND <= W-13T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-14F AND <= W-14T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-15F AND <= W-15T
               GO TO MR110
           END-IF.
           IF  AM-KEY >= W-16F AND <= W-16T
               GO TO MR110
           END-IF.
           GO TO MR100.
       MR110.
           MOVE AM-KEY     TO HZM-KMCD.
           MOVE ZERO       TO HZM-HOCD.
      *           START HZM-F     KEY IS NOT <  HZM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            HZM-F_PNAME1 "HZM-KEY" " NOT < " HZM-KEY RETURNING RET.
           IF  RET = 1
               GO  TO    MR100
           END-IF.
       MR120.
      *           READ  HZM-F     NEXT  UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HZM-F_PNAME1 BY REFERENCE HZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO    MR100
           END-IF.
           IF  HZM-KMCD  NOT =  AM-KEY
               GO  TO      MR100
           END-IF.
           IF  HZM-KEY >= W-121F AND <= W-121T
               GO TO MR130
           END-IF.
           IF  HZM-KEY >= W-122F AND <= W-122T
               GO TO MR130
           END-IF.
           IF  HZM-KEY >= W-123F AND <= W-123T
               GO TO MR130
           END-IF.
           IF  HZM-KEY >= W-124F AND <= W-124T
               GO TO MR130
           END-IF.
           IF  HZM-KEY >= W-125F AND <= W-125T
               GO TO MR130
           END-IF.
           IF  HZM-KEY >= W-126F AND <= W-126T
               GO TO MR130
           END-IF.
           GO TO MR120.
       MR130.
           MOVE AM-KEY     TO K-ACCD.
           MOVE ZERO       TO K-HOCD.
           PERFORM KNGG-RTN THRU KNGG-EX.
           MOVE KNGNMN     TO M8-01.
           MOVE AM-KEY     TO WK-9999.
           MOVE WK-9999    TO M8-02.
           MOVE AM-KEY     TO K-ACCD.
           MOVE HZM-HOCD   TO K-HOCD.
           PERFORM KNGG-RTN THRU KNGG-EX.
           MOVE KNGNMN     TO M8-03.
           MOVE HZM-HOCD   TO WK-9999.
           MOVE WK-9999    TO M8-04.
           IF  DR-CR = 1
               MOVE "借方"     TO M8-05
           ELSE
               MOVE "貸方"     TO M8-05
           END-IF.
           MOVE ZERO        TO TOT-WK W-ZAN W-KARI W-KASI ERR-SW.
           MOVE HZM-ZAN     TO W-ZENKI.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
           IF (W-ZAN  = ZERO) AND
              (W-KARI = ZERO) AND
              (W-KASI = ZERO)
               MOVE 1     TO ERR-SW
           END-IF.
           MOVE ZERO         TO SH-KEY3.
           MOVE AM-KEY       TO HACCNTCD.
           MOVE HZM-HOCD     TO HHOACCNT.
           MOVE Z-TOUF(TI)   TO HTRDATE.
      *           START SDH KEY NOT LESS SH-KEY3 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY3" " NOT LESS " SH-KEY3 RETURNING RET.
           IF  RET = 1
               GO TO MR150
           END-IF.
       MR140.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR150
           END-IF.
           IF  HZM-KEY NOT = HKACD1
               GO TO MR150
           END-IF.
           IF  W-03T < HTRDATE
               GO TO MR150
           END-IF.
           IF  W-03F > HTRDATE
               PERFORM SET-RTN THRU SET-EX
               PERFORM ADD-RTN THRU ADD-EX
               GO TO MR140
           END-IF.
           MOVE HTRDATE      TO ZYMD.
           PERFORM Z-RTN THRU Z-EXT.
           PERFORM ZAN-OUT-RTN THRU ZAN-OUT-EX.
           GO TO MR170.
       MR150.
           IF  ERR-SW = 0
               PERFORM ZAN-OUT-RTN THRU ZAN-OUT-EX
               MOVE YOKO TO P-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
               MOVE SPACE     TO P-R
               PERFORM TOT2-RTN THRU TOT2-EX
           END-IF.
           GO TO MR120.
       MR160.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR180
           END-IF.
           IF  HZM-KEY NOT = HKACD1
               GO TO MR180
           END-IF.
           IF  W-03T < HTRDATE
               GO TO MR180
           END-IF.
           MOVE HTRDATE2     TO ZYMD.
           PERFORM Z-RTN THRU Z-EXT.
           IF  ZI NOT = OZI
               PERFORM TOT1-RTN THRU TOT1-EX
           END-IF.
       MR170.
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM ADD-RTN THRU ADD-EX.
           MOVE HTRDATE      TO OHTRDATE3.
           MOVE HJUNLNO      TO OHJUNLNO3.
           MOVE ZI           TO OZI.
           GO TO MR160.
       MR180.
           PERFORM TOT1-RTN THRU TOT1-EX.
           PERFORM TOT2-RTN THRU TOT2-EX.
           MOVE ZERO     TO OLD-KEY.
           GO TO MR120.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
       INI-RTN.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           ACCEPT SYSDATE     FROM DATE.
           MOVE SYS-YY     TO M3-YY.
           MOVE SYS-MM     TO WK-ZZ.
           MOVE WK-ZZ      TO M3-MM.
           MOVE SYS-DD     TO WK-ZZ.
           MOVE WK-ZZ      TO M3-DD.
           MOVE 0     TO ERR-SW.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "FCTL-F"     TO ERR-F
               MOVE "G"          TO ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE FCTL-REC1     TO Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "SD_Output" USING "DSP-NEN" DSP-NEN "p"
                                         RETURNING RESU.
           COPY LIBCPR.
           MOVE Z-TOUT(Z-KSMM)     TO TO-YMD.
           MOVE Z-TOUT(15)         TO TO-YMD1.
           MOVE Z-UPDYM            TO FROM-YM.
           MOVE 01                 TO FROM-D.
           MOVE 1     TO I.
       INI-000.
           IF (Z-TOUF(I) NOT > FROM-YMD) AND
              (Z-TOUT(I) NOT < FROM-YMD)
               GO TO INI-100
           END-IF.
           IF  I NOT = 15
               ADD 1     TO I
               GO TO INI-000
           END-IF.
           IF  Z-KSMM = 12
               MOVE 1     TO I
           ELSE
               COMPUTE I = Z-KSMM + 1
           END-IF.
           MOVE Z-TOUF(I)              TO FROM-YMD.
           GO TO INI-EX.
       INI-100.
           IF  I = Z-KSMM
               MOVE 13     TO I
           ELSE
               IF  I = 12
                   MOVE 1     TO I
               ELSE
                   ADD 1     TO I
           END-IF.
           IF  I > 15
               CALL "SD_Output" USING "MG-01" MG-01 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               MOVE 1     TO ERR-SW
               GO TO INI-EX
           END-IF.
           MOVE Z-TOUF(I)     TO FROM-YMD.
       INI-EX.
           EXIT.
      *********
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY3" BY REFERENCE SH-KEY3.
           CALL "PR_Open" RETURNING RESP.
       OPEN-EX.
           EXIT.
       W10-RTN.
       W10-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W121F "ACP-W121F" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W10-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-RTN
           END-IF.
       W10-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W121T "ACP-W121T" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-020
           END-IF.
           IF  W-121F > W-121T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-020
           END-IF.
       W10-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W122F "ACP-W122F" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-030
           END-IF.
           IF  W-12F = ZERO
               MOVE ZERO TO W-22F W-12T W-22T W-13F W-23F W-13T W-23T
                                  W-14F W-24F W-14T W-24T W-15F W-25F
                                  W-15T W-25T W-16F W-26F W-16T W-26T
               CALL "SD_Output" USING "SP-08" SP-08 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-10" SP-10 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-12" SP-12 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-122F < W-121T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-030
           END-IF.
       W10-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W122T "ACP-W122T" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-030
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-040
           END-IF.
           IF  W-122F > W-122T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-040
           END-IF.
       W10-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W123F "ACP-W123F" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-040
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-050
           END-IF.
           IF  W-13F = ZERO
               MOVE ZERO TO W-23F W-13T W-23T W-14F W-24F W-14T W-24T
                                  W-15F W-25F W-15T W-25T W-16F W-26F
                                                          W-16T W-26T
               CALL "SD_Output" USING "SP-10" SP-10 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-12" SP-12 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-123F < W-122T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-050
           END-IF.
       W10-060.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W123T "ACP-W123T" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-050
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-060
           END-IF.
           IF  W-123F > W-123T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-060
           END-IF.
       W10-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W124F "ACP-W124F" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-060
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-070
           END-IF.
           IF  W-14F = ZERO
               MOVE ZERO TO W-24F W-14T W-24T W-15F W-25F W-15T W-25T
                                  W-16F W-26F W-16T W-26T
               CALL "SD_Output" USING "SP-12" SP-12 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-124F < W-123T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-070
           END-IF.
       W10-080.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W124T "ACP-W124T" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-070
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-080
           END-IF.
           IF  W-124F > W-124T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-080
           END-IF.
       W10-090.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W125F "ACP-W125F" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-080
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-090
           END-IF.
           IF  W-15F = ZERO
               MOVE ZERO TO W-25F W-15T W-25T W-16F W-26F W-16T W-26T
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-125F < W-124T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-090
           END-IF.
       W10-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W125T "ACP-W125T" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-090
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-100
           END-IF.
           IF  W-125F > W-125T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-100
           END-IF.
       W10-110.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W126F "ACP-W126F" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-100
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-110
           END-IF.
           IF  W-16F = ZERO
               MOVE ZERO TO W-26F W-16T W-26T
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-126F < W-125T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-110
           END-IF.
       W10-120.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W126T "ACP-W126T" " " "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-110
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-120
           END-IF.
           IF  W-126F > W-126T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-120
           END-IF.
       W10-EX.
           EXIT.
       W03F-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W03F "ACP-W03F" " " "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W03F-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W03F-RTN
           END-IF.
           MOVE ZERO TO W-03FY1.
           IF  W-03F = ZERO
               MOVE FROM-YMD     TO W-03F
               CALL "SD_Output" USING "ACP-W03F" ACP-W03F "p"
                                         RETURNING RESU
               GO TO W03F-050
           END-IF.
           IF  W-03FY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-03FY
           END-IF.
           IF  W-03FY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-03FY
           END-IF.
       W03F-050.
           IF  W-03F < FROM-YMD OR > TO-YMD1
               CALL "SD_Output" USING "MG-01" MG-01 "p" RETURNING RESU
               GO TO W03F-RTN
           END-IF.
           MOVE 1     TO I.
       W03F-100.
           IF (Z-TOUF(I) NOT > W-03F) AND
              (Z-TOUT(I) NOT < W-03F)
               CONTINUE
           ELSE
               IF  I NOT = 15
                   ADD 1     TO I
                   GO TO W03F-100
               ELSE
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
                   GO TO W03F-RTN
               END-IF
           END-IF.
           MOVE I     TO TI.
           IF  Z-KSMM = 12
               MOVE 1     TO FI
           ELSE
               COMPUTE FI = Z-KSMM + 1
           END-IF.
           IF  TI > 12
               MOVE 13     TO FI
           END-IF.
       W03F-EX.
           EXIT.
      *********
       W03T-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W03T "ACP-W03T" " " "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W03T-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W03T-RTN
           END-IF.
           MOVE ZERO TO W-03TY1.
           IF  W-03TS = 999999
               IF  TI < 13
                   MOVE TO-YMD     TO W-03T
               ELSE
                   MOVE TO-YMD1    TO W-03T
               END-IF
           END-IF.
           IF  W-03T = TO-YMD OR TO-YMD1
               GO TO W03T-100
           END-IF.
           IF  W-03TY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-03TY
           END-IF.
           IF  W-03TY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-03TY
           END-IF.
       W03T-100.
           IF  TI < 13
               IF  W-03T > TO-YMD
                   CALL "SD_Output" USING "MG-01" MG-01 "p"
                                                  RETURNING RESU
                   GO TO W03T-RTN
               END-IF
           END-IF.
           IF  TI > 12
               IF  W-03T > TO-YMD1
                   CALL "SD_Output" USING "MG-01" MG-01 "p"
                                                  RETURNING RESU
                   GO TO W03T-RTN
               END-IF
           END-IF.
           IF  W-03F > W-03T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W03T-RTN
           END-IF.
       W03T-EX.
           EXIT.
      *********
       WOKC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-WOKC "ACP-WOKC" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO WOKC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO WOKC-RTN
           END-IF.
           IF  W-OKC NOT = "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO WOKC-RTN
           END-IF.
       WOKC-EX.
           EXIT.
      *********
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF.
           MOVE FI     TO I.
       ZAN-SET-000.
           ADD HZM-TJKR(I)     TO W-KARI TOT-KR(3).
           ADD HZM-TJKS(I)     TO W-KASI TOT-KS(3).
           IF  I = TI
               SUBTRACT HZM-TJKR(I)     FROM TOT-KR(3)
               SUBTRACT HZM-TJKS(I)     FROM TOT-KS(3)
               GO TO ZAN-SET-900
           END-IF.
           IF  I = 12
               MOVE 1     TO I
               GO TO ZAN-SET-000
           END-IF.
           ADD 1     TO I.
           GO TO ZAN-SET-000.
       ZAN-SET-500.
           IF  BS-PL = 0
               MOVE 1      TO I
           ELSE
               MOVE 13     TO I
           END-IF.
       ZAN-SET-600.
           ADD HZM-TJKR(I)     TO W-KARI TOT-KR(3).
           ADD HZM-TJKS(I)     TO W-KASI TOT-KS(3).
           IF  I = 12
               IF  DR-CR = 1
                   COMPUTE W-ZENKI = HZM-ZAN + W-KARI - W-KASI
               ELSE
                   COMPUTE W-ZENKI = HZM-ZAN + W-KASI - W-KARI
               END-IF
           END-IF.
           IF  I = 12
               MOVE W-ZENKI     TO HZM-ZAN
               MOVE ZERO        TO W-KARI W-KASI TOT-ITEM(3)
           END-IF.
           IF  I = TI
               SUBTRACT HZM-TJKR(I)     FROM TOT-KR(3)
               SUBTRACT HZM-TJKS(I)     FROM TOT-KS(3)
               GO TO ZAN-SET-900
           END-IF.
           IF  I = 15
               GO TO ZAN-SET-900
           END-IF.
           ADD 1     TO I.
           GO TO ZAN-SET-600.
       ZAN-SET-900.
           IF  DR-CR = 1
               COMPUTE W-ZAN = HZM-ZAN + (W-KARI - HZM-TJKR(TI)) -
                                         ( W-KASI - HZM-TJKS(TI))
           ELSE
               COMPUTE W-ZAN = HZM-ZAN + (W-KASI - HZM-TJKS(TI)) -
                                         ( W-KARI - HZM-TJKR(TI))
           END-IF.
       ZAN-SET-EX.
           EXIT.
      *********
       SET-RTN.
           IF  DR-CR = HDR-CR
               COMPUTE W-ZAN = W-ZAN + HAMOUNT
           ELSE
               COMPUTE W-ZAN = W-ZAN - HAMOUNT
           END-IF.
       SET-EX.
           EXIT.
      *********
       ZAN-OUT-RTN.
           PERFORM MID-RTN THRU MID-EX.
           MOVE TATEP1           TO P-R.
           MOVE K15              TO P1-K15.
           MOVE SPACE            TO P1-04.
           MOVE W-ZAN            TO P1-08.
           MOVE "前日残高"     TO P1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       ZAN-OUT-EX.
           EXIT.
      *********
       PRI-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
               MOVE TATEP1       TO P-R
               MOVE K15          TO P1-K15
               MOVE K20          TO P1-K20
               MOVE HTRDATE      TO WK-YMD
               GO   TO    PRI-000
           END-IF.
           MOVE TATEP1       TO P-R.
           MOVE K15          TO P1-K15.
           MOVE K20          TO P1-K20.
           MOVE HTRDATE      TO WK-YMD.
           IF  HTRDATE  = OHTRDATE3
               GO TO PRI-030
           END-IF.
           IF  WK-YY NOT = OHTRDATE3YY
               GO TO PRI-000
           END-IF.
           IF  WK-MM NOT = OHTRDATE3MM
               GO TO PRI-010
           END-IF.
           IF  WK-DD NOT = OHTRDATE3DD
               GO TO PRI-020
           END-IF.
       PRI-000.
           MOVE WK-YY2   TO P1-01Y.
           MOVE "."      TO P1-01YH.
       PRI-010.
           MOVE WK-MM    TO P1-01M.
           MOVE "."      TO P1-01MH.
       PRI-020.
           MOVE WK-DD    TO P1-01D.
           GO TO PRI-040.
       PRI-030.
           IF  HJUNLNO  = OHJUNLNO3
               GO TO PRI-050
           END-IF.
       PRI-040.
           MOVE HJUNLNO    TO P1-02.
           MOVE "-"        TO P1-02H.
       PRI-050.
           MOVE HLINENO    TO P1-03.
           IF  HCUSTCD = ZERO
               MOVE SPACE       TO P1-04
           ELSE
               MOVE HNAMEN      TO P1-04
           END-IF.
           IF  HSECTCD NOT = ZERO
               MOVE "("        TO P1-05F
               MOVE HSECTCD    TO P1-05
               MOVE ")"        TO P1-05T
           END-IF.
           IF  HDR-CR  = 1
               MOVE HAMOUNT     TO P1-06
           ELSE
               MOVE HAMOUNT     TO P1-07
           END-IF.
           PERFORM SET-RTN THRU SET-EX.
           MOVE W-ZAN      TO P1-08.
           MOVE HTEKIYO    TO P1-09.
           IF  HTAXKB     = "1" OR "5"
               MOVE "*"       TO P1-10
           END-IF.
           IF  HTAXKB     = "3" OR "7"
               MOVE "#"       TO P1-10
           END-IF.
           IF  HTAXKB     = " "
               IF  HETAX    = "1" OR "5"
                   MOVE "*"       TO P1-10
               END-IF
           END-IF.
           IF  HTAXKB     = " "
               IF  HETAX    = "3" OR "7"
                   MOVE "#"       TO P1-10
               END-IF
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       PRI-EX.
           EXIT.
       ADD-RTN.
           IF  HDR-CR2 = 1
               ADD HAMOUNT     TO TOT-KR(2) TOT-KR(3)
           ELSE
               ADD HAMOUNT     TO TOT-KS(2) TOT-KS(3)
           END-IF.
       ADD-EX.
           EXIT.
       MID-RTN.
           IF  PCNT NOT = ZERO
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD 1     TO PCNT.
           MOVE PCNT     TO WK-ZZZZZ.
           MOVE WK-ZZZZZ     TO M3-PCNT.
           MOVE MID03 TO P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID05 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID06 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID07 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID08 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID09 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID10 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID11 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE TATEP1     TO P-R.
           MOVE K15        TO P1-K15.
           MOVE K20        TO P1-K20.
           MOVE SPACE      TO P1-04 P1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       MID-EX.
           EXIT.
       TOT1-RTN.
           IF  PCNT = ZERO
               GO TO TOT1-EX
           END-IF.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE TOT-ITEM(2)     TO TOT-ITEM(1).
           MOVE TATEP1     TO P-R.
           MOVE K15        TO P1-K15.
           MOVE K20        TO P1-K20.
           MOVE "当　月　合　計"     TO P1-04.
           MOVE SPACE      TO P1-09.
           PERFORM OUT-RTN THRU OUT-EX.
           MOVE YOKO TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
           MOVE ZERO     TO TOT-KR(2) TOT-KS(2).
       TOT1-EX.
           EXIT.
       TOT2-RTN.
           IF  PCNT = ZERO
               GO TO TOT2-EX
           END-IF.
           PERFORM SPO-RTN THRU SPO-EX.
           MOVE TOT-ITEM(3)     TO TOT-ITEM(1).
           MOVE TATEP2     TO P-R.
           MOVE K15        TO P2-K15.
           MOVE K20        TO P2-K20.
           MOVE "当　期　累　計"     TO P2-04.
           MOVE "前期繰越"           TO P2-09.
           MOVE W-ZENKI                TO P2-99.
           PERFORM OUT-RTN THRU OUT-EX.
           MOVE YOKO TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
           MOVE ZERO     TO TOT-KR(3) TOT-KS(3).
       TOT2-EX.
           EXIT.
       OUT-RTN.
           MOVE TOT-KR(1)     TO P1-06.
           MOVE TOT-KS(1)     TO P1-07.
           MOVE W-ZAN         TO P1-08.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       OUT-EX.
           EXIT.
      ***
       SPO-RTN.
           MOVE TATEP1     TO P-R.
           MOVE K15        TO P1-K15.
           MOVE K20        TO P1-K20.
           MOVE SPACE      TO P1-04 P1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       SPO-EX.
           EXIT.
      ***
       KNGG-RTN.
           MOVE 0     TO INV-SW.
      *           READ KNG UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1         TO INV-SW
               MOVE SPACE     TO KNGNMN
           END-IF.
       KNGG-EX.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
       CLSE-EXT.
           EXIT.
      *********
       COPY LPMSG_PR.
      *********
