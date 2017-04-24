       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PR440L.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    総勘定元帳                                                 *
      *                            --- 90/01/21 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       INPUT-OUTPUT               SECTION.
       DATA                        DIVISION.
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
           02  W-11F           PIC  9(04).
           02  W-11T           PIC  9(04).
           02  W-12F           PIC  9(04).
           02  W-12T           PIC  9(04).
           02  W-13F           PIC  9(04).
           02  W-13T           PIC  9(04).
           02  W-14F           PIC  9(04).
           02  W-14T           PIC  9(04).
           02  W-15F           PIC  9(04).
           02  W-15T           PIC  9(04).
           02  W-16F           PIC  9(04).
           02  W-16T           PIC  9(04).
           02  W-17F           PIC  9(04).
           02  W-17T           PIC  9(04).
           02  W-02F.
             03  W-02FY        PIC  9(04).
             03  W-02FYL  REDEFINES W-02FY.
               04  W-02FY1     PIC  9(02).
               04  W-02FY2     PIC  9(02).
             03  W-02FM        PIC  9(02).
             03  W-02FD        PIC  9(02).
           02  W-02FR REDEFINES W-02F     PIC  9(08).
           02  W-02T.
             03  W-02TY        PIC  9(04).
             03  W-02TYL  REDEFINES W-02TY.
               04  W-02TY1     PIC  9(02).
               04  W-02TY2     PIC  9(02).
             03  W-02TM        PIC  9(02).
             03  W-02TD        PIC  9(02).
           02  W-02TR REDEFINES W-02T     PIC  9(08).
           02  W-02TL REDEFINES W-02T.
             03  F             PIC  9(02).
             03  W-02TS        PIC  9(06).
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
             03 SYS-YY         PIC  9(02).
             03 SYS-MM         PIC  9(02).
             03 SYS-DD         PIC  9(02).
           02  WK-YMD.
             03 WK-YY          PIC  9(04).
             03 WK-YYL  REDEFINES WK-YY.
               04 WK-YY1       PIC  9(02).
               04 WK-YY2       PIC  9(02).
             03 WK-MM          PIC  9(02).
             03 WK-DD          PIC  9(02).
           02  WK-ZZ           PIC  Z9.
           02  WK-ZZZZZ        PIC  ZZZZ9.
           02  WK-9999         PIC  9(04).
           02  W-TEKIYO        PIC  N(20).
           02  W-TEKITOD  REDEFINES W-TEKIYO.
             03  W-TEKIYO1     PIC  N(19).
             03  W-TEKIYO2     PIC  N(01).
       01  OLD-KEY.
           02  OHTRDATE2.
             03  OHTRDATE2YY   PIC  9(04).
             03  OHTRDATE2MM   PIC  9(02).
             03  OHTRDATE2DD   PIC  9(02).
           02  OHJUNLNO2       PIC  9(06).
           02  OZI             PIC  9(02).
       01  TOT-WK.
           02  TOT-ITEM OCCURS 3.
             03  TOT-KR        PIC S9(12).
             03  TOT-KS        PIC S9(12).
       01  MID03.
           02  FILLER          PIC X(05) VALUE X"1A24212474".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  M1-YY           PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  M1-MM           PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  M1-DD           PIC N(02).
           02  FILLER          PIC N(03) VALUE "日作成".
           02  FILLER          PIC X(23) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC0".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC N(09)
               VALUE "総　勘　定　元　帳".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC1".
           02  FILLER          PIC X(49) VALUE SPACE.
           02  M1-PCNT         PIC N(05).
           02  FILLER          PIC N(01) VALUE "頁".
       01  MID04.
           02  FILLER          PIC X(02) VALUE X"1AC0".
           02  FILLER          PIC X(51) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC1".
       01  MID05.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(06) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
       01  MID06.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "勘定科目".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-01           PIC N(10).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "（".
           02  M6-02           PIC N(04).
           02  FILLER          PIC N(01) VALUE "）".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-03           PIC N(02).
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC N(03) VALUE "（　自".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-YYF          PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-MMF          PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-DDF          PIC N(02).
           02  FILLER          PIC N(01) VALUE "日".
           02  FILLER          PIC X(05) VALUE SPACE.
           02  FILLER          PIC N(01) VALUE "至".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-YYT          PIC N(02).
           02  FILLER          PIC N(01) VALUE "年".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-MMT          PIC N(02).
           02  FILLER          PIC N(01) VALUE "月".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  M6-DDT          PIC N(02).
           02  FILLER          PIC N(01) VALUE "日".
           02  FILLER          PIC N(02) VALUE "　）".
       01  MID07.
           02  FILLER          PIC X(04) VALUE X"1AC01AC2".
           02  FILLER          PIC X(10) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(35) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(06) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(81) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC1".
       01  MID08.
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
       01  MID09.
           02  FILLER          PIC X(04) VALUE X"1AC01AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "日　　付".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(04) VALUE "伝票番号".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE X"1AC2".
           02  FILLER          PIC X(01) VALUE SPACE.
           02  FILLER          PIC N(07) VALUE "取　引　先　名".
           02  FILLER          PIC X(02) VALUE SPACE.
           02  FILLER          PIC X(02) VALUE " (".
           02  FILLER          PIC N(02) VALUE "部門".
           02  FILLER          PIC X(01) VALUE ")".
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
           COPY LKAZAN.
           COPY FCTL.
       01  SDH_PR441L.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F            PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY SIWAKH.
       77  F                   PIC  X(001).
      **
       01  P-F.
           02  P-R                 PIC X(200).
           02  P1-R  REDEFINES  P-R.
               03  P1-K15          PIC X(05).
               03  FILLER          PIC X(02).
               03  FILLER          PIC X(01).
               03  P1-01.
                 04  P1-01Y        PIC 9(02).
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
       77  F                   PIC  X(001).
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
           02  ACP-W11.
               03  ACP-W11F              PIC 9(04).
               03  ACP-W11T              PIC 9(04).
           02  ACP-W12.
               03  ACP-W12F              PIC 9(04).
               03  ACP-W12T              PIC 9(04).
           02  ACP-W13.
               03  ACP-W13F              PIC 9(04).
               03  ACP-W13T              PIC 9(04).
           02  ACP-W14.
               03  ACP-W14F              PIC 9(04).
               03  ACP-W14T              PIC 9(04).
           02  ACP-W15.
               03  ACP-W15F              PIC 9(04).
               03  ACP-W15T              PIC 9(04).
           02  ACP-W16.
               03  ACP-W16F              PIC 9(04).
               03  ACP-W16T              PIC 9(04).
           02  ACP-W17.
               03  ACP-W17F              PIC 9(04).
               03  ACP-W17T              PIC 9(04).
           02  ACP-W02F.
               03  ACP-W02FY             PIC 9(02).
               03  ACP-W02FM             PIC 9(02).
               03  ACP-W02FD             PIC 9(02).
           02  ACP-W02T.
               03  ACP-W02TY             PIC 9(02).
               03  ACP-W02TM             PIC 9(02).
               03  ACP-W02TD             PIC 9(02).
           02  ACP-WOKC                  PIC X(01).
       01  DSP-AREA.
           02  DSP-01 .
               03  FILLER  PIC  N(01)  VALUE  "年".
               03  FILLER  PIC  N(02)  VALUE  "月度".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  9(02).
               03  FILLER    PIC X(12).
               03  FILLER    PIC N(05) VALUE "総勘定元帳".
           02  DSP-04 .
               03  FILLER    PIC N(04) VALUE "ＦＲＯＭ".
               03  FILLER    PIC N(02) VALUE "ＴＯ".
           02  DSP-06 .
               03  FILLER    PIC N(05) VALUE "科目コード".
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-06A.
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-06B.
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-06C.
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-06D.
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-06E.
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-06F.
               03  FILLER    PIC N(01) VALUE "～".
           02  DSP-08 .
               03  FILLER    PIC N(05) VALUE "日　　　付".
               03  FILLER    PIC N(01) VALUE "～".
               03  FILLER    PIC X(04) VALUE "/  /".
               03  FILLER    PIC X(04) VALUE "/  /".
           02  DSP-24 .
               03  FILLER    PIC X(18) VALUE "確認 OK=1,NO=9 ( )".
       01  MG-AREA.
           02  MG-01         PIC N(07)
               VALUE "日付期間エラー".
       01  SP-AREA.
           02  SP-06  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-08  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-10  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-12  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-14  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-16  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-18  .
               03  FILLER   PIC Z(04).
               03  FILLER   PIC Z(04).
           02  SP-20  .
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
               03  FILLER   PIC Z(02).
           02  SP-24  .
               03  FILLER   PIC X(01).
      *********
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
            "ACP-AREA" " " "0" "0" "69" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W11" " " "6" "0" "8" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W11F" "9" "6" "31" "4" " " "ACP-W11" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W11F" BY REFERENCE W-11F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W11T" "9" "6" "51" "4" "ACP-W11F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W11T" BY REFERENCE W-11T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W12" " " "8" "0" "8" "ACP-W11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W12F" "9" "8" "31" "4" " " "ACP-W12" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W12F" BY REFERENCE W-12F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W12T" "9" "8" "51" "4" "ACP-W12F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W12T" BY REFERENCE W-12T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W13" " " "10" "0" "8" "ACP-W12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W13F" "9" "10" "31" "4" " " "ACP-W13" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W13F" BY REFERENCE W-13F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W13T" "9" "10" "51" "4" "ACP-W13F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W13T" BY REFERENCE W-13T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W14" " " "12" "0" "8" "ACP-W13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W14F" "9" "12" "31" "4" " " "ACP-W14" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W14F" BY REFERENCE W-14F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W14T" "9" "12" "51" "4" "ACP-W14F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W14T" BY REFERENCE W-14T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W15" " " "14" "0" "8" "ACP-W14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W15F" "9" "14" "31" "4" " " "ACP-W15" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W15F" BY REFERENCE W-15F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W15T" "9" "14" "51" "4" "ACP-W15F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W15T" BY REFERENCE W-15T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W16" " " "16" "0" "8" "ACP-W15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W16F" "9" "16" "31" "4" " " "ACP-W16" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W16F" BY REFERENCE W-16F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W16T" "9" "16" "51" "4" "ACP-W16F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W16T" BY REFERENCE W-16T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W17" " " "18" "0" "8" "ACP-W16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W17F" "9" "18" "31" "4" " " "ACP-W17" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W17F" BY REFERENCE W-17F "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W17T" "9" "18" "51" "4" "ACP-W17F" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W17T" BY REFERENCE W-17T "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W02F" " " "20" "0" "6" "ACP-W17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W02FY" "9" "20" "27" "2" " " "ACP-W02F" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W02FY" BY REFERENCE W-02FY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-W02FM" "9" "20" "30" "2" "ACP-W02FY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W02FM" BY REFERENCE W-02FM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-W02FD" "9" "20" "33" "2" "ACP-W02FM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W02FD" BY REFERENCE W-02FD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W02T" " " "20" "0" "6" "ACP-W02F" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-W02TY" "9" "20" "47" "2" " " "ACP-W02T" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W02TY" BY REFERENCE W-02TY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-W02TM" "9" "20" "50" "2" "ACP-W02TY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W02TM" BY REFERENCE W-02TM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-W02TD" "9" "20" "53" "2" "ACP-W02TM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-W02TD" BY REFERENCE W-02TD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-WOKC" "X" "24" "77" "1" "ACP-W02T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-WOKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "106" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "32" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "N" "1" "4" "2" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "N" "1" "8" "4" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-01" "9" "1" "2" "2" "02DSP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-01" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "9" "1" "6" "2" "03DSP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-01" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-01" "RX" "1" "35" "12" "04DSP-01" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05DSP-01" BY REFERENCE WKSP "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-01" "N" "1" "36" "10" "05DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "4" "0" "12" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "N" "4" "27" "8" " " "DSP-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-04" "N" "4" "49" "4" "01DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06" " " "6" "0" "12" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06" "N" "6" "11" "10" " " "DSP-06" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-06" "N" "6" "40" "2" "01DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06A" " " "8" "0" "2" "DSP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06A" "N" "8" "40" "2" " " "DSP-06A" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06B" " " "10" "0" "2" "DSP-06A" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06B" "N" "10" "40" "2" " " "DSP-06B"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06C" " " "12" "0" "2" "DSP-06B" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06C" "N" "12" "40" "2" " " "DSP-06C"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06D" " " "14" "0" "2" "DSP-06C" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06D" "N" "14" "40" "2" " " "DSP-06D"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06E" " " "16" "0" "2" "DSP-06D" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06E" "N" "16" "40" "2" " " "DSP-06E"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-06F" " " "18" "0" "2" "DSP-06E" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-06F" "N" "18" "40" "2" " " "DSP-06F"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" " " "20" "0" "20" "DSP-06F" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-08" "N" "20" "11" "10" " " "DSP-08" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-08" "N" "20" "40" "2" "01DSP-08" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-08" "X" "20" "29" "4" "02DSP-08" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-08" "X" "20" "49" "4" "03DSP-08" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-24" " " "24" "0" "18" "DSP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-24" "X" "24" "61" "18" " " "DSP-24" RETURNING RESU.
      *MG-AREA
       CALL "SD_Init" USING
            "MG-AREA" " " "0" "0" "14" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "MG-01" "N" "24" "2" "14" " " "MG-AREA"  RETURNING RESU.
      *SP-AREA
       CALL "SD_Init" USING 
            "SP-AREA" " " "0" "0" "69" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-06" " " "6" "0" "8" " " "SP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-06" "Z" "6" "31" "4" " " "SP-06" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-06" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-06" "Z" "6" "51" "4" "01SP-06" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-06" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-08" " " "8" "0" "8" "SP-06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-08" "Z" "8" "31" "4" " " "SP-08" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-08" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-08" "Z" "8" "51" "4" "01SP-08" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-08" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-10" " " "10" "0" "8" "SP-08" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-10" "Z" "10" "31" "4" " " "SP-10" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-10" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-10" "Z" "10" "51" "4" "01SP-10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-10" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-12" " " "12" "0" "8" "SP-10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-12" "Z" "12" "31" "4" " " "SP-12" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-12" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-12" "Z" "12" "51" "4" "01SP-12" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-12" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-14" " " "14" "0" "8" "SP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-14" "Z" "14" "31" "4" " " "SP-14" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-14" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-14" "Z" "14" "51" "4" "01SP-14" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-14" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-16" " " "16" "0" "8" "SP-14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-16" "Z" "16" "31" "4" " " "SP-16" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-16" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-16" "Z" "16" "51" "4" "01SP-16" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-16" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-18" " " "18" "0" "8" "SP-16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-18" "Z" "18" "31" "4" " " "SP-18" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-18" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-18" "Z" "18" "51" "4" "01SP-18" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-18" BY REFERENCE WKZE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "SP-20" " " "20" "0" "12" "SP-18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SP-20" "Z" "20" "27" "2" " " "SP-20" RETURNING RESU.
       CALL "SD_From" USING 
            "01SP-20" BY REFERENCE WKZE "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SP-20" "Z" "20" "30" "2" "01SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02SP-20" BY REFERENCE WKZE "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03SP-20" "Z" "20" "33" "2" "02SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03SP-20" BY REFERENCE WKZE "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04SP-20" "Z" "20" "47" "2" "03SP-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04SP-20" BY REFERENCE WKZE "2" "0" RETURNING RESU.
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
               GO TO MR900
           END-IF.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-W11F" ACP-W11F "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-W11T" ACP-W11T "p"
                                         RETURNING RESU.
           IF  W-12F NOT = ZERO
               CALL "SD_Output" USING "ACP-W12F" ACP-W12F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W12T" ACP-W12T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-13F NOT = ZERO
               CALL "SD_Output" USING "ACP-W13F" ACP-W13F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W13T" ACP-W13T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-14F NOT = ZERO
               CALL "SD_Output" USING "ACP-W14F" ACP-W14F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W14T" ACP-W14T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-15F NOT = ZERO
               CALL "SD_Output" USING "ACP-W15F" ACP-W15F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W15T" ACP-W15T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-16F NOT = ZERO
               CALL "SD_Output" USING "ACP-W16F" ACP-W16F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W16T" ACP-W16T "p"
                                         RETURNING RESU
           END-IF.
           IF  W-17F NOT = ZERO
               CALL "SD_Output" USING "ACP-W17F" ACP-W17F "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "ACP-W17T" ACP-W17T "p"
                                         RETURNING RESU
           END-IF.
       MR030.
           PERFORM W02F-RTN THRU W02F-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
       MR040.
           PERFORM W02T-RTN THRU W02T-EX.
           IF  ESTAT = "09"
               GO TO MR030
           END-IF.
       MR090.
           PERFORM WOKC-RTN THRU WOKC-EX.
           IF  ESTAT = "09"
               GO TO MR040
           END-IF.
           IF  W-OKC = "9"
               CALL "SD_Output" USING "SP-AREA" SP-AREA "p"
                                          RETURNING RESU
               GO TO MR010
           END-IF.
           MOVE W-02FY2    TO M6-YYF.
           MOVE W-02FM     TO WK-ZZ.
           MOVE WK-ZZ      TO M6-MMF.
           MOVE W-02FD     TO WK-ZZ.
           MOVE WK-ZZ      TO M6-DDF.
           MOVE W-02TY2    TO M6-YYT.
           MOVE W-02TM     TO WK-ZZ.
           MOVE WK-ZZ      TO M6-MMT.
           MOVE W-02TD     TO WK-ZZ.
           MOVE WK-ZZ      TO M6-DDT.
           MOVE W-11F     TO AM-KEY.
      *           START AM KEY NOT LESS AM-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            AM_PNAME1 "AM-KEY" " NOT LESS " AM-KEY RETURNING RET.
           IF  RET = 1
               GO TO MR900
           END-IF
      *           READ AM NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR150
           END-IF.
           IF  AM-KEY > W-11T
               GO TO MR900
           END-IF.
           GO TO MR110.
       MR100.
      *           READ AM NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                GO TO MR900
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
           IF  AM-KEY >= W-17F AND <= W-17T
               GO TO MR110
           END-IF.
           GO TO MR100.
       MR110.
           PERFORM KZMG-RTN THRU KZMG-EX.
           IF  INV-SW = 1
               GO TO MR100
           END-IF.
           MOVE AM-KEY     TO K-ACCD.
           MOVE ZERO       TO K-HOCD.
           PERFORM KNGG-RTN THRU KNGG-EX.
           MOVE KNGNMN     TO M6-01.
           MOVE AM-KEY     TO WK-9999.
           MOVE WK-9999    TO M6-02.
           IF  DR-CR = 1
               MOVE "借方"     TO M6-03
           ELSE
               MOVE "貸方"     TO M6-03
           END-IF.
           MOVE ZERO        TO TOT-WK W-ZAN W-KARI W-KASI ERR-SW.
           MOVE KZM-ZAN     TO W-ZENKI.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
           IF (W-ZAN  = ZERO) AND
              (W-KARI = ZERO) AND
              (W-KASI = ZERO)
               MOVE 1     TO ERR-SW
           END-IF.
           MOVE ZERO         TO SH-KEY2.
           MOVE AM-KEY       TO HACCNTCD2.
           MOVE Z-TOUF(TI)   TO HTRDATE2.
      *           START SDH KEY NOT LESS SH-KEY2 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY2" " NOT LESS " SH-KEY2 RETURNING RET.
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
           IF  AM-KEY NOT = HACCNTCD2
               GO TO MR150
           END-IF.
           IF  W-02T < HTRDATE2
               GO TO MR150
           END-IF.
           IF  W-02F > HTRDATE2
               PERFORM SET-RTN THRU SET-EX
               PERFORM ADD-RTN THRU ADD-EX
               GO TO MR140
           END-IF.
           MOVE HTRDATE2     TO ZYMD.
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
           GO TO MR100.
       MR160.
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO MR180
           END-IF.
           IF  AM-KEY NOT = HACCNTCD2
               GO TO MR180
           END-IF.
           IF  W-02T < HTRDATE2
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
           MOVE HTRDATE2     TO OHTRDATE2.
           MOVE HJUNLNO2     TO OHJUNLNO2.
           MOVE ZI           TO OZI.
           GO TO MR160.
       MR180.
           PERFORM TOT1-RTN THRU TOT1-EX.
           PERFORM TOT2-RTN THRU TOT2-EX.
           MOVE ZERO     TO OLD-KEY.
           GO TO MR100.
       MR900.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       MR999.
           CALL "DB_Close".
           STOP RUN.
      ************************
       INI-RTN.
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
           PERFORM OPEN-RTN THRU OPEN-EX.
           COPY LIBCPR.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           ACCEPT SYSDATE     FROM DATE.
           MOVE SYS-YY     TO M1-YY.
           MOVE SYS-MM     TO WK-ZZ.
           MOVE WK-ZZ      TO M1-MM.
           MOVE SYS-DD     TO WK-ZZ.
           MOVE WK-ZZ      TO M1-DD.
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
               COMPUTE I = Z-KSMM + 1.
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
               END-IF
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
            "INPUT" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY2" BY REFERENCE SH-KEY2.
           CALL "PR_Open" RETURNING RESP.
       OPEN-EX.
           EXIT.
       W10-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W11F "ACP-W11F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO W10-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-RTN
           END-IF.
       W10-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W11T "ACP-W11T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-010
           END-IF.
           IF  W-11F > W-11T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-010
           END-IF.
       W10-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W12F "ACP-W12F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-010
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-020
           END-IF.
           IF  W-12F = ZERO
               MOVE ZERO TO W-12T W-13F W-13T W-14F W-14T W-15F W-15T
                                                          W-16F W-16T
               CALL "SD_Output" USING "SP-08" SP-08 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-10" SP-10 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-12" SP-12 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-11T > W-12F
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-020
           END-IF.
       W10-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W12T "ACP-W12T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-030
           END-IF.
           IF  W-12F > W-12T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-030
           END-IF.
       W10-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W13F "ACP-W13F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-030
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-040
           END-IF.
           IF  W-13F = ZERO
               MOVE ZERO TO W-13T W-14F W-14T W-15F W-15T W-16F W-16T
               CALL "SD_Output" USING "SP-10" SP-10 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-12" SP-12 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-12T > W-13F
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-040
           END-IF.
       W10-050.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W13T "ACP-W13T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-040
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-050
           END-IF.
           IF  W-13F > W-13T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-050
           END-IF.
       W10-060.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W14F "ACP-W14F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-050
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-060
           END-IF.
           IF  W-14F = ZERO
               MOVE ZERO TO W-14T W-15F W-15T W-16F W-16T
               CALL "SD_Output" USING "SP-12" SP-12 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-13T > W-14F
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-060
           END-IF.
       W10-070.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W14T "ACP-W14T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-060
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-070
           END-IF.
           IF  W-14F > W-14T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-070
           END-IF.
       W10-080.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W15F "ACP-W15F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-070
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-080
           END-IF.
           IF  W-15F = ZERO
               MOVE ZERO TO W-15T W-16F W-16T
               CALL "SD_Output" USING "SP-14" SP-14 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-14T > W-15F
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-080
           END-IF.
       W10-090.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W15T "ACP-W15T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-080
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-090
           END-IF
           IF  W-15F > W-15T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-090
           END-IF.
       W10-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W16F "ACP-W16F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-090
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-100
           END-IF.
           IF  W-16F = ZERO
               MOVE ZERO TO W-16T W-17F W-17T
               CALL "SD_Output" USING "SP-16" SP-16 "p" RETURNING RESU
               CALL "SD_Output" USING "SP-18" SP-18 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-15T > W-16F
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-100
           END-IF.
       W10-110.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W16T "ACP-W16T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-100
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-110
           END-IF.
           IF  W-16F > W-16T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-110
           END-IF.
       W10-120.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W17F "ACP-W17F" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-110
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-120
           END-IF.
           IF  W-17F = ZERO
               MOVE ZERO TO W-17T
               CALL "SD_Output" USING "SP-18" SP-18 "p" RETURNING RESU
               GO TO W10-EX
           END-IF.
           IF  W-16T > W-17F
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-120
           END-IF.
       W10-130.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W17T "ACP-W17T" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W10-120
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W10-130
           END-IF.
           IF  W-17F > W-17T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W10-130
           END-IF.
       W10-EX.
           EXIT.
      *********
       W02F-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W02F "ACP-W02F" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W02F-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W02F-RTN
           END-IF.
           MOVE ZERO TO W-02FY1.
           IF  W-02F = ZERO
               MOVE FROM-YMD     TO W-02F
               CALL "SD_Output" USING "ACP-W02F" ACP-W02F
                          "p" RETURNING RESU
               GO TO W02F-050
           END-IF.
           IF  W-02FY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-02FY
           END-IF.
           IF  W-02FY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-02FY
           END-IF.
       W02F-050.
           IF  W-02F < FROM-YMD OR > TO-YMD1
               CALL "SD_Output" USING "MG-01" MG-01 "p" RETURNING RESU
               GO TO W02F-RTN
           END-IF.
           MOVE 1     TO I.
       W02F-100.
           IF (Z-TOUF(I) NOT > W-02F) AND
              (Z-TOUT(I) NOT < W-02F)
               CONTINUE
           ELSE
               IF  I NOT = 15
                   ADD 1     TO I
                   GO TO W02F-100
               ELSE
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
                   GO TO W02F-RTN
               END-IF
           END-IF.
           MOVE I     TO TI.
      *
           IF  Z-KSMM = 12
               MOVE 1     TO FI
           ELSE
               COMPUTE FI = Z-KSMM + 1
           END-IF.
           IF  TI > 12
               MOVE 13     TO FI
           END-IF.
       W02F-EX.
           EXIT.
      *********
       W02T-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-W02T "ACP-W02T" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO W02T-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO W02T-RTN
           END-IF.
           MOVE ZERO TO W-02TY1.
           IF  W-02TS = 999999
               IF  TI < 13
                   MOVE TO-YMD     TO W-02T
               ELSE
                   MOVE TO-YMD1    TO W-02T
               END-IF
           END-IF.
           IF  W-02T = TO-YMD OR TO-YMD1
               CALL "SD_Output" USING "ACP-W02T" ACP-W02T "p"
                                         RETURNING RESU
               GO TO W02T-100
           END-IF.
           IF  W-02TY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-02TY
           END-IF.
           IF  W-02TY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-02TY
           END-IF.
       W02T-100.
           IF TI < 13
              IF  W-02T > TO-YMD
                  CALL "SD_Output" USING "MG-01" MG-01 "p"
                                          RETURNING RESU
                  GO TO W02T-RTN
              END-IF
           END-IF.
           IF  TI > 12
               IF  W-02T > TO-YMD1
                   GO TO W02T-RTN
               END-IF
           END-IF.
           IF  W-02F > W-02T
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
               GO TO W02T-RTN
           END-IF.
       W02T-EX.
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
       KZMG-RTN.
           MOVE 0          TO INV-SW.
           MOVE AM-KEY     TO KZM-KEY.
      *           READ KZM-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KZM-F_PNAME1 BY REFERENCE KZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
           END-IF.
       KZMG-EX.
           EXIT.
      *********
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF.
           MOVE FI     TO I.
       ZAN-SET-000.
           ADD KZM-TJKR(I)     TO W-KARI TOT-KR(3).
           ADD KZM-TJKS(I)     TO W-KASI TOT-KS(3).
           IF  I = TI
               SUBTRACT KZM-TJKR(I)     FROM TOT-KR(3)
               SUBTRACT KZM-TJKS(I)     FROM TOT-KS(3)
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
           ADD KZM-TJKR(I)     TO W-KARI TOT-KR(3).
           ADD KZM-TJKS(I)     TO W-KASI TOT-KS(3).
           IF  I = 12
               IF  DR-CR = 1
                   COMPUTE W-ZENKI = KZM-ZAN + W-KARI - W-KASI
               ELSE
                   COMPUTE W-ZENKI = KZM-ZAN + W-KASI - W-KARI
               END-IF
           END-IF.
           IF  I = 12
               MOVE W-ZENKI     TO KZM-ZAN
               MOVE ZERO        TO W-KARI W-KASI TOT-ITEM(3)
           END-IF.
           IF  I = TI
               SUBTRACT KZM-TJKR(I)     FROM TOT-KR(3)
               SUBTRACT KZM-TJKS(I)     FROM TOT-KS(3)
               GO TO ZAN-SET-900
           END-IF.
           IF  I = 15
               GO TO ZAN-SET-900
           END-IF.
           ADD 1     TO I.
           GO TO ZAN-SET-600.
       ZAN-SET-900.
           IF  DR-CR = 1
               COMPUTE W-ZAN = KZM-ZAN + (W-KARI - KZM-TJKR(TI)) -
                                         ( W-KASI - KZM-TJKS(TI))
           ELSE
               COMPUTE W-ZAN = KZM-ZAN + (W-KASI - KZM-TJKS(TI)) -
                                         ( W-KARI - KZM-TJKR(TI))
           END-IF.
       ZAN-SET-EX.
           EXIT.
      *********
       SET-RTN.
           IF  DR-CR = HDR-CR2
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
               MOVE HTRDATE2     TO WK-YMD
               GO   TO   PRI-000
           END-IF.
           MOVE TATEP1       TO P-R.
           MOVE K15          TO P1-K15.
           MOVE K20          TO P1-K20.
           MOVE HTRDATE2     TO WK-YMD.
           IF  HTRDATE2 = OHTRDATE2
               GO TO PRI-030
           END-IF.
           IF  WK-YY NOT = OHTRDATE2YY
               GO TO PRI-000
           END-IF.
           IF  WK-MM NOT = OHTRDATE2MM
               GO TO PRI-010
           END-IF.
           IF  WK-DD NOT = OHTRDATE2DD
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
           IF  HJUNLNO2 = OHJUNLNO2
               GO TO PRI-050
           END-IF.
       PRI-040.
           MOVE HJUNLNO2   TO P1-02.
           MOVE "-"        TO P1-02H.
       PRI-050.
           MOVE HLINENO2   TO P1-03.
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
           IF  HDR-CR2 = 1
               MOVE HAMOUNT     TO P1-06
           ELSE
               MOVE HAMOUNT     TO P1-07
           END-IF.
           PERFORM SET-RTN THRU SET-EX.
           MOVE W-ZAN      TO P1-08.
           IF (HACCNTCD2 NOT = 0400) OR (HJUNLNO2 < 300000 OR > 399999)
               MOVE HTEKIYO    TO P1-09
               GO  TO  PRI-090
           END-IF.
           MOVE HTEKIYO    TO W-TEKIYO.
           IF  HOPPCD > 5999 AND < 7000
               MOVE "製" TO W-TEKIYO2
           END-IF.
           IF  HOPPCD > 6999 AND < 8000
               MOVE "一" TO W-TEKIYO2
           END-IF.
           MOVE W-TEKIYO   TO P1-09.
       PRI-090.
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
      ***
       ADD-RTN.
           IF  HDR-CR2 = 1
               ADD HAMOUNT     TO TOT-KR(2) TOT-KR(3)
           ELSE
               ADD HAMOUNT     TO TOT-KS(2) TOT-KS(3)
           END-IF.
       ADD-EX.
           EXIT.
      ***
       MID-RTN.
           IF  PCNT NOT = ZERO
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD 1     TO PCNT.
           MOVE PCNT     TO WK-ZZZZZ.
           MOVE WK-ZZZZZ     TO M1-PCNT.
           MOVE MID03 TO P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE MID04 TO P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
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
           MOVE TATEP1     TO P-R.
           MOVE K15        TO P1-K15.
           MOVE K20        TO P1-K20.
           MOVE SPACE      TO P1-04 P1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE SPACE     TO P-R.
       MID-EX.
           EXIT.
      ***
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
      ***
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
      ***
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
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
       CLSE-EXT.
           EXIT.
      *********
       COPY LPMSG_PR.
      *********
