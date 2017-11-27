       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR060L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  部門名マスタリスト　　　　　  *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/19                      *
      *    COMPILE TYPE  :  COBOL                         *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.     SYSTEM3100.
       OBJECT-COMPUTER.     SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC  X(02).
       77  C2                      PIC  X(05)  VALUE  X"1A24212474".
       77  LCNT                    PIC  9(02).
       77  PCNT                    PIC  9(05).
       77  I                       PIC  9(01).
       77  RTN-SW                  PIC  9(01).
      ***  RTN-SW = 1 の時，MAIN-RTN へ戻る。
       01  HIZUKE                  PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  W-AREA.
           02  W-BUMONCD-FROM      PIC  9(04).
           02  W-BUMONCD-TO        PIC  9(04).
           02  W-KAKU              PIC  X(01).
      *
       01  PRINTR1.
           02  C-2B                PIC  X(05).
           02  F                   PIC  X(01)  VALUE  SPACE.
           02  P1-01               PIC  9(04).
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  P1-02               PIC  N(10).
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  P1-03               PIC  9(01).
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  F                   PIC  N(01)  VALUE  "頁".
           02  F                   PIC  X(03)  VALUE  SPACE.
           02  FF     OCCURS 3.
               03  P1-04               PIC  Z9.
               03  F                   PIC  X(06)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFF    OCCURS 6.
               03  P1-05               PIC  Z9.
               03  F                   PIC  X(06)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFFF   OCCURS 3.
               03  P1-06               PIC  Z9.
               03  F                   PIC  X(06)  VALUE  SPACE.
      *
       01  PRINTR2.
           02  F                   PIC  X(35)  VALUE  SPACE.
           02  F                   PIC  N(01)  VALUE  "列".
           02  F                   PIC  X(04)  VALUE  SPACE.
           02  FFFFF     OCCURS 3.
               03  P2-01               PIC  9(01).
               03  F                   PIC  X(07)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFFFFF    OCCURS 6.
               03  P2-02               PIC  9(01).
               03  F                   PIC  X(07)  VALUE  SPACE.
           02  F                   PIC  X(02)  VALUE  SPACE.
           02  FFFFFFF   OCCURS 3.
               03  P2-03               PIC  9(01).
               03  F                   PIC  X(07)  VALUE  SPACE.
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(17) VALUE
               "部　門　名　マ　ス　タ　リ　ス　ト".
           02  F                   PIC  X(36) VALUE  SPACE.
           02  M-YY                PIC  Z9.
           02  F                   PIC  N(01) VALUE  "年".
           02  M-MM                PIC  Z9.
           02  F                   PIC  N(01) VALUE  "月".
           02  M-DD                PIC  Z9.
           02  F                   PIC  N(03) VALUE  "日作成".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  M-PCNT              PIC  ZZZZ9.
           02  F                   PIC  N(01) VALUE  "頁".
      *
       01  MID-02.
           02  F                   PIC  N(03) VALUE  "部　門".
           02  F                   PIC  X(06) VALUE  SPACE.
           02  F                   PIC  X(12) VALUE  "部 　門   名".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(02) VALUE  "部門".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  X(08) VALUE  "<------ ".
           02  F                   PIC  N(03) VALUE  "損益用".
           02  F                   PIC  X(08) VALUE  " ------>".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  X(20) VALUE
               "<------------------ ".
           02  F                   PIC  N(03) VALUE  "経費用".
           02  F                   PIC  X(20) VALUE
               " ------------------>".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  X(08) VALUE  "<------ ".
           02  F                   PIC  N(03) VALUE  "製造用".
           02  F                   PIC  X(08) VALUE  " ------>".
      *
       01  MID-03.
           02  F                   PIC  N(03) VALUE  "コード".
           02  F                   PIC  X(23) VALUE  SPACE.
           02  F                   PIC  N(02) VALUE  "区分".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "明　細".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計①".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計②".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "明　細".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計①".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計②".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計③".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計④".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計⑤".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "明　細".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計①".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "合計②".
      ***
      ***************************************
      *    ｴﾗｰ DISPLAY (ﾜｰｸ)                *
      ***************************************
       01  DISP-ERR-WORK.
           02  DISP-MSG.
               03  ERR-MSGX.
                   04  ERR-MSGN     PIC N(25).
               03  ERR-SPACE        PIC X(50).
               03  ERR-F            PIC X(12).
               03  ERR-M            PIC X(01).
               03  ERR-K            PIC X(30).
               03  ERR-FLG          PIC X(02).
      *******************************
      *    プリンタ№変更ワーク     *
      *******************************
       01  ASNPRN.
           03  ASNPRN1              PIC  X(03)   VALUE  "PRN".
           03  ASNPRN2              PIC  9(03).
       01  PMEDIA                   PIC  X(06)   VALUE  SPACE.
      *******************************
      *    該当月取込み処理ワーク   *
      *******************************
       01  ZYMD                     PIC  9(08).
       01  ZI                       PIC  9(02).
       01  Z-R.
           02  Z-KEY1               PIC  X(06).
           02  Z-KSMM               PIC  9(02).
           02  Z-KONYMD.
               03  Z-KONYY          PIC  9(04).
               03  Z-KONYYL  REDEFINES Z-KONYY.
                 04  Z-KONYY1       PIC  9(02).
                 04  Z-KONYY2       PIC  9(02).
               03  Z-KONMM          PIC  9(02).
               03  Z-KONDD          PIC  9(02).
           02  Z-ZENYMD.
               03  Z-ZENYY          PIC  9(04).
               03  Z-ZENMM          PIC  9(02).
               03  Z-ZENDD          PIC  9(02).
           02  Z-GESYMD.
               03  Z-GESYY          PIC  9(04).
               03  Z-GESYYL  REDEFINES Z-GESYY.
                 04  Z-GESYY1       PIC  9(02).
                 04  Z-GESYY2       PIC  9(02).
               03  Z-GESMM          PIC  9(02).
               03  Z-GESDD          PIC  9(02).
           02  Z-GEMYMD.
               03  Z-GEMYY          PIC  9(04).
               03  Z-GEMYYL  REDEFINES Z-GEMYY.
                 04  Z-GEMYY1       PIC  9(02).
                 04  Z-GEMYY2       PIC  9(02).
               03  Z-GEMMM          PIC  9(02).
               03  Z-GEMDD          PIC  9(02).
           02  Z-ACEPSIN            PIC  9(01).
           02  Z-TOUKI.
             03  Z-TOU     OCCURS 15.
               04  Z-TOUF.
                 05  Z-TOUFYY       PIC  9(04).
                 05  Z-TOUFYYL  REDEFINES Z-TOUFYY.
                   06  Z-TOUFYY1    PIC  9(02).
                   06  Z-TOUFYY2    PIC  9(02).
                 05  Z-TOUFMM       PIC  9(02).
                 05  Z-TOUFDD       PIC  9(02).
               04  Z-TOUT.
                 05  Z-TOUTYY       PIC  9(04).
                 05  Z-TOUTYYL  REDEFINES Z-TOUTYY.
                   06  Z-TOUTYY1    PIC  9(02).
                   06  Z-TOUTYY2    PIC  9(02).
                 05  Z-TOUTMM       PIC  9(02).
                 05  Z-TOUTDD       PIC  9(02).
           02  Z-UPDYM.
             03  Z-UPDYY            PIC  9(04).
             03  Z-UPDMM            PIC  9(02).
           02  Z-SIMEBI             PIC  9(02).
           02  FILLER               PIC  X(223).
      ***
      ***  部門名マスタ
           COPY  BUMONF.
      ***  プリンター
      *       FD  PRINTF
       77  PRINTR                  PIC  X(250).
      **
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
      ******************************
      *　　画面クリアー項目　　    *
      ******************************
       01  DSP-CLR.
           03  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(001) VALUE " ".
      *******************
      *    画面表示     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC X(020) VALUE
               " 部門名マスタリスト ".
           03  FILLER  PIC X(008) VALUE  "ＦＲＯＭ".
           03  FILLER  PIC X(004) VALUE  "ＴＯ".
           03  FILLER  PIC X(010) VALUE  "部門コード".
           03  FILLER  PIC X(002) VALUE  "～".
           03  FILLER  PIC X(018)
               VALUE  "確認 OK=1,NO=9 ( )".
      ***********************
      *    画面入力         *
      ***********************
       01  ACP-AREA.
           03  ACP-BUMONCD-FROM    PIC 9(04).
           03  ACP-BUMONCD-TO      PIC 9(04).
           03  ACP-KAKU            PIC X(01).
      ***
      **
      **   MESSEGE  AREA
      **
       01  DISP-ERR-AREA.
           02  DISP-MSG-01.
               03  FILLER  PIC X(50).
           02  DISP-MSG-SPACE.
               03  FILLER  PIC X(50).
           02  DISP-BUZ-B.
               03  FILLER  PIC X(05) VALUE X"1B4210"..
           02  DISP-BUZ-J.
               03  FILLER  PIC X(05) VALUE X"1B4A01".
           02  NOR-M01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "＊　マスタ　登録済　＊".
           02  NOR-D01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "＊　データ　登録済　＊".
           02  INV-M01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "＊　マスタ　未登録　＊".
           02  INV-D01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "＊　データ　未登録　＊".
           02  OK-01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(14) VALUE
               "＊　Ｏ　Ｋ　＊".
           02  CAN-01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(18) VALUE
               "＊　キャンセル　＊".
           02  ERR-01.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(18) VALUE
               "＊　入力エラー　＊".
           02  INV-MCT.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(28) VALUE
               "＊　コントロールＭ未登録　＊".
           02  INV-CON.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(42) VALUE
               "＊　コントロールＦ未登録　処理続行不可　＊".
           02  ERR-YMD.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(22) VALUE
               "＊　日付入力エラー　＊".
           02  ERR-DIS.
               03  FILLER  PIC X(50).
               03  FILLER  PIC X(05) VALUE
               "<<<  ".
               03  FILLER  PIC X(12).
               03  FILLER  PIC X(01).
               03  FILLER  PIC X(11) VALUE
               "ｴﾗｰ STATUS=".
               03  FILLER  PIC X(02).
               03  FILLER  PIC X(05) VALUE
               "  >>>".
               03  FILLER  PIC X(05) VALUE
               " KEY=".
               03  FILLER  PIC X(30).
      ***
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "6" "33" "4" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "51" "4" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "24" "77" "1" "02CLR-AREA" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "RN" "1" "31" "20" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "X" "4" "31" "8" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA2" "X" "4" "51" "4" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA2" "X" "6" "11" "10" "03CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA2" "X" "6" "43" "2" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA2" "X" "24" "61" "18" "05CLR-AREA2" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUMONCD-FROM" "9" "6" "33" "4" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUMONCD-FROM" BY REFERENCE W-BUMONCD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUMONCD-TO" "9" "6" "51" "4" "ACP-BUMONCD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUMONCD-TO" BY REFERENCE W-BUMONCD-TO "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-BUMONCD-TO" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
      *DISP-ERR-AREA
       CALL "SD_Init" USING
            "DISP-ERR-AREA" " " "24" "0" "961" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-MSG-01" " " "24" "0" "50" " " "DISP-ERR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-MSG-01" "X" "24" "2" "50" " " "DISP-MSG-01"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-MSG-01" BY REFERENCE ERR-MSGX "50" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-MSG-SPACE" " " "24" "0" "50" "DISP-MSG-01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-MSG-SPACE" "X" "24" "2" "50" " " "DISP-MSG-SPACE"
            RETURNING RESU.
       CALL "SD_From" USING
            "01DISP-MSG-SPACE" BY REFERENCE ERR-SPACE "50" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-B" " " "24" "0" "5" "DISP-MSG-SPACE" " "
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
            "NOR-M01" " " "24" "0" "72" "DISP-BUZ-J" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01NOR-M01" "X" "24" "2" "50" " " "NOR-M01"
            RETURNING RESU.
       CALL "SD_From" USING
            "01NOR-M01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02NOR-M01" "X" "24" "2" "22" "01NOR-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "NOR-D01" " " "24" "0" "72" "NOR-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01NOR-D01" "X" "24" "2" "50" " " "NOR-D01"
            RETURNING RESU.
       CALL "SD_From" USING
            "01NOR-D01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02NOR-D01" "X" "24" "2" "22" "01NOR-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "INV-M01" " " "24" "0" "72" "NOR-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-M01" "X" "24" "2" "50" " " "INV-M01" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-M01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-M01" "X" "24" "2" "22" "01INV-M01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "INV-D01" " " "24" "0" "72" "INV-M01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-D01" "X" "24" "2" "50" " " "INV-D01" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-D01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-D01" "X" "24" "2" "22" "01INV-D01" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "OK-01" " " "24" "0" "64" "INV-D01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01OK-01" "X" "24" "2" "50" " " "OK-01" RETURNING RESU.
       CALL "SD_From" USING
            "01OK-01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02OK-01" "X" "24" "2" "14" "01OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "CAN-01" " " "24" "0" "68" "OK-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CAN-01" "X" "24" "2" "50" " " "CAN-01" RETURNING RESU.
       CALL "SD_From" USING
            "01CAN-01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02CAN-01" "X" "24" "2" "18" "01CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-01" " " "24" "0" "68" "CAN-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01ERR-01" "X" "24" "2" "50" " " "ERR-01" RETURNING RESU.
       CALL "SD_From" USING
            "01ERR-01" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02ERR-01" "X" "24" "2" "18" "01ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "INV-MCT" " " "24" "0" "78" "ERR-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-MCT" "X" "24" "2" "50" " " "INV-MCT" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-MCT" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-MCT" "X" "24" "2" "28" "01INV-MCT" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "INV-CON" " " "24" "0" "92" "INV-MCT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01INV-CON" "X" "24" "2" "50" " " "INV-CON" RETURNING RESU.
       CALL "SD_From" USING
            "01INV-CON" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02INV-CON" "X" "24" "2" "42" "01INV-CON" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-YMD" " " "24" "0" "72" "INV-CON" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01ERR-YMD" "X" "24" "2" "50" " " "ERR-YMD" RETURNING RESU.
       CALL "SD_From" USING
            "01ERR-YMD" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02ERR-YMD" "X" "24" "2" "22" "01ERR-YMD" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "ERR-DIS" " " "24" "0" "121" "ERR-YMD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01ERR-DIS" "X" "24" "2" "50" " " "ERR-DIS" RETURNING RESU.
       CALL "SD_From" USING
            "01ERR-DIS" BY REFERENCE ERR-SPACE "50" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02ERR-DIS" "X" "24" "2" "5" "01ERR-DIS" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03ERR-DIS" "X" "24" "7" "12" "02ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "03ERR-DIS" BY REFERENCE ERR-F "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "04ERR-DIS" "X" "24" "19" "1" "03ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "04ERR-DIS" BY REFERENCE ERR-M "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "05ERR-DIS" "X" "24" "20" "11" "04ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06ERR-DIS" "X" "24" "31" "2" "05ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06ERR-DIS" BY REFERENCE ERR-FLG "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "07ERR-DIS" "X" "24" "33" "5" "06ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08ERR-DIS" "X" "24" "38" "5" "07ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09ERR-DIS" "X" "24" "43" "30" "08ERR-DIS" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "09ERR-DIS" BY REFERENCE ERR-K "30" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           PERFORM  MAIN-RTN    THRU  MAIN-EX.
           PERFORM  CLSE-ENT     THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP  RUN.
      **************************
      *    初期処理            *
      **************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           ACCEPT  HIZUKE  FROM  DATE.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           MOVE  90     TO  LCNT.
       INI-EX.
           EXIT.
      *****************************
      *    ＭＡＩＮ　処理　　　　 *
      *****************************
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-BUMONCD-FROM "ACP-BUMONCD-FROM"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO  TO  MAIN-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-BUMONCD-TO "ACP-BUMONCD-TO" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-BUMONCD-TO = ZERO
               MOVE  ALL "9"     TO  W-BUMONCD-TO
           END-IF.
           IF  W-BUMONCD-FROM > W-BUMONCD-TO
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU = 9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               INITIALIZE  W-AREA
               GO  TO  MAIN-RTN
           END-IF.
           IF  W-KAKU NOT = 1
               GO  TO  MAIN-020
           END-IF.
           PERFORM  LST-RTN     THRU  LST-EX.
           IF  RTN-SW = 1
               MOVE  ZERO     TO  RTN-SW
               GO  TO  MAIN-RTN
           END-IF.
       MAIN-EX.
           EXIT.
      ************************
      *    終了処理          *
      ************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
       CLSE-EXT.
           EXIT.
      **************************
      *    ＬＳＴ－ＲＴＮ      *
      **************************
       LST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-BUMONCD-FROM     TO  BNM-KEY.
      *           START  BNM  KEY  NOT < BNM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            BNM_PNAME1 "BNM-KEY" " NOT < " BNM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                             RETURNING RESU
      **  データ未登録　表示
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      **
      ***  部門名マスタ　ＲＥＡＤ
       LST-010.
      *           READ  BNM  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-999.
           IF  BNM-KEY > W-BUMONCD-TO
               IF  LCNT = 90
                   CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                             RETURNING RESU
                   CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                         RETURNING RESU
      ***  データ未登録　表示
                   MOVE  1     TO  RTN-SW
                   GO  TO  LST-999
               ELSE
                   GO  TO  LST-999
               END-IF
           END-IF.
           IF  LCNT NOT < 62
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF.
           PERFORM  MEI-RTN     THRU  MEI-EX.
           GO  TO  LST-010.
       LST-999.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      ****************************
      *    ＭＩＤ－ＲＴＮ　      *
      ****************************
       MID-RTN.
           IF  LCNT NOT = 90
               MOVE  SPACE     TO  PRINTR
               CALL "PR_Write" USING PRINTR RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD  1     TO  PCNT.
           MOVE  PCNT   TO  M-PCNT.
           MOVE  YY     TO  M-YY.
           MOVE  MM     TO  M-MM.
           MOVE  DD     TO  M-DD.
           MOVE  MID-01 TO PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  MID-02 TO PRINTR.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  MID-03 TO PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  5     TO  LCNT.
       MID-EX.
           EXIT.
      ***************************
      *    ＭＥＩ－ＲＴＮ 　　  *
      ***************************
       MEI-RTN.
           MOVE  C2               TO  C-2B.
           MOVE  BNM-KEY          TO  P1-01.
           MOVE  BNMNMN           TO  P1-02.
           MOVE  BNM-BUMONKBN     TO  P1-03.
      ***  頁  ************************************
           MOVE  BNM-PLPG(1)     TO  P1-04(1).
           MOVE  BNM-PLPG(2)     TO  P1-04(2).
           MOVE  BNM-PLPG(3)     TO  P1-04(3).
      *
           MOVE  1     TO  I.
       MEI-030.
           IF  I NOT < 7
               GO  TO  MEI-040
           END-IF.
           MOVE  BNM-KHPG(I)     TO  P1-05(I).
           ADD  1     TO  I.
           GO  TO  MEI-030.
       MEI-040.
           MOVE  BNM-GNPG(1)     TO  P1-06(1).
           MOVE  BNM-GNPG(2)     TO  P1-06(2).
           MOVE  BNM-GNPG(3)     TO  P1-06(3).
           MOVE  PRINTR1 TO PRINTR.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE            TO  PRINTR.
      *
      ***  列  ************************************
           MOVE  BNM-PLLN(1)     TO  P2-01(1).
           MOVE  BNM-PLLN(2)     TO  P2-01(2).
           MOVE  BNM-PLLN(3)     TO  P2-01(3).
      *
           MOVE  1     TO  I.
       MEI-010.
           IF  I NOT < 7
               GO  TO  MEI-020
           END-IF.
           MOVE  BNM-KHLN(I)     TO  P2-02(I).
           ADD  1     TO  I.
           GO  TO  MEI-010.
       MEI-020.
           MOVE  BNM-GNLN(1)     TO  P2-03(1).
           MOVE  BNM-GNLN(2)     TO  P2-03(2).
           MOVE  BNM-GNLN(3)     TO  P2-03(3).
      *           WRITE  PRINTR  FROM  PRINTR2  AFTER 1.
           MOVE  PRINTR2         TO  PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE            TO  PRINTR.
           ADD  3     TO  LCNT.
       MEI-EX.
           EXIT.
      **
      *****************************
      *    ｴﾗｰ DISPLAY (ﾒｲﾝ)      *
      *****************************
       ERR-ENT.
           MOVE    ERR-STAT  TO  ERR-FLG.
           PERFORM CLSE-ENT THRU CLSE-EXT.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-010.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
           "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           GO TO ERR-010.
       ERR-EXT.
           EXIT.
      *****************************
      *    該当月取込み処理       *
      *****************************
       Z-RTN.
           MOVE    1         TO  ZI.
       Z-010.
           IF  ZI  >  15
               MOVE  99      TO  ZI
               GO    TO      Z-EXT
           END-IF.
           IF  Z-TOUF(ZI)  >  ZYMD
               ADD   1       TO  ZI
               GO    TO      Z-010
           END-IF.
           IF  Z-TOUT(ZI)  <  ZYMD
               ADD   1       TO  ZI
               GO    TO      Z-010
           END-IF.
       Z-EXT.
           EXIT.
