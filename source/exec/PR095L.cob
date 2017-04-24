       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR095L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  補助残高マスタリスト　　　　  *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/12/10                      *
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
       77  H                       PIC  9(01).
      ***  リスト上の 借方 OR 貸方 のどちらかの添字 ( 1 OR 2 )
       77  I                       PIC  9(02).
      ***  リスト上の添字   ( 当期１２箇月 と 翌期３箇月
      ***                         合わせて １５の添字 )
       77  J                       PIC  9(02).
      ***  ファイル上の添字 ( 決算月 )
       77  W-Z9                    PIC  Z9.
      ***  編集項目
       77  C2                      PIC  X(05)  VALUE  X"1A24212474".
       77  C15                     PIC  X(05)  VALUE  X"1A24212078".
       77  LCNT                    PIC  9(02).
       77  PCNT                    PIC  9(05).
       77  RTN-SW                  PIC  9(01).
      ***  RTN-SW = 1 の時，MAIN-RTN へ戻る。
       77  SW                      PIC  9(01).
      ***  SW = 0 の時，１回もＲＥＡＤできない。
      ***  SW = 1 の時，１回ＲＥＡＤした。
      ***  SW = 2 の時，２回ＲＥＡＤした。
       01  W-OWARI                 PIC  X(05).
      ***  SPACEじゃなかったら，コントロールファイルのINVALIDで STOP RUN
       01  W-CONTROL-TSUKI         PIC  9(02).
      ***  決算月をたしていくのに使用。
       01  KETSAN                  PIC  9(02).
      ***  決算月を保存しておく。
       01  WRITE-CNT               PIC  9(01).
      ***  WRITE-CNT=3 になったら，改頁する。
       01  HIZUKE                  PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  W-AREA.
           02  W-KACD-FROM         PIC  9(04).
           02  W-KACD-TO           PIC  9(04).
           02  W-HOCD-FROM         PIC  9(04).
           02  W-HOCD-TO           PIC  9(04).
           02  W-KAKU              PIC  X(01).
           02  W-AREA1.
               03  W-AREA2     OCCURS 2.
                   04  W-KACD      PIC  9(04).
                   04  W-KAMEI     PIC  N(10).
                   04  W-HOCD      PIC  9(04).
                   04  W-HOMEI     PIC  N(10).
                   04  W-ZMZ       PIC S9(11).
                   04  W-AREA3     OCCURS 15.
                       05  W-KARI       PIC S9(11).
                       05  W-KASHI      PIC S9(11).
      *
       01  W-AREA10.
           02  W-AREA11     OCCURS 15.
               03  W-01            PIC  X(02).
      ***  コントロールＦから決算月をためておく。
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(19) VALUE
               "補　助　残　高　マ　ス　タ　リ　ス　ト".
           02  F                   PIC  X(32) VALUE  SPACE.
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
           02  FFFF     OCCURS 2.
               03  F                   PIC  N(03) VALUE  "科　目".
               03  F                   PIC  X(19) VALUE  SPACE.
               03  F                   PIC  N(03) VALUE  "補　助".
               03  F                   PIC  X(38) VALUE  SPACE.
      *
       01  MID-03.
           02  FFFFF     OCCURS 2.
               03  F                   PIC  N(03) VALUE  "コード".
               03  F                   PIC  X(04) VALUE  SPACE.
               03  F                   PIC  N(05) VALUE  "科　目　名".
               03  F                   PIC  X(05) VALUE  SPACE.
               03  F                   PIC  N(03) VALUE  "コード".
               03  F                   PIC  X(04) VALUE  SPACE.
               03  F                   PIC  N(05) VALUE  "補　助　名".
               03  F                   PIC  X(10) VALUE  SPACE.
               03  F                   PIC  N(05) VALUE  "前期末残高".
               03  F                   PIC  X(04) VALUE  SPACE.
      *
       01  MID-04.
           02  F                   PIC  X(23) VALUE  SPACE.
           02  FFFFFF    OCCURS 2.
               03  F                   PIC  X(07) VALUE  "借 　方".
               03  F                   PIC  X(04) VALUE  SPACE.
               03  F                   PIC  N(01) VALUE  "／".
               03  F                   PIC  X(04) VALUE  SPACE.
               03  F                   PIC  X(07) VALUE  "貸 　方".
               03  F                   PIC  X(45) VALUE  SPACE.
      ***
           COPY  LWMSG_PR.
      ***  補助残高マスタ
           COPY  LHOZAN.
      ***  コントロールファイル
           COPY  FCTL.
      ***  漢字科目マスタ
           COPY  KANGEL.
      ***  プリンター
       01  PRINTF.
           02  PRINTR              PIC  X(250).
           02  PRINTR1             REDEFINES  PRINTR.
               03  C-15B           PIC  X(05).
               03  F               PIC  X(01).
               03  FF     OCCURS 2.
                   04  P1-01           PIC  9(04).
                   04  F               PIC  X(03).
                   04  P1-02           PIC  N(10).
                   04  F               PIC  X(03).
                   04  P1-03           PIC  9(04).
                   04  F               PIC  X(03).
                   04  P1-04           PIC  N(10).
                   04  F               PIC  X(02).
                   04  P1-05           PIC  ---,---,---,--9.
                   04  F               PIC  X(05).
               03  C-2B1           PIC  X(05).
           02  PRINTR2             REDEFINES  PRINTR.
               03  C-2B2           PIC  X(05).
               03  F               PIC  X(08).
               03  FFF    OCCURS 2.
                   04  PP2-01          PIC  N(02).
                   04  P2-01           PIC  N(02).
                   04  PP2-02          PIC  N(01).
                   04  F               PIC  X(01).
                   04  P2-02           PIC  ---,---,---,--9.
                   04  PP2-03          PIC  N(01).
                   04  P2-03           PIC  ---,---,---,--9.
                   04  F               PIC  X(26).
       77  F                  PIC  X(001).
      **
       77  SP-R               PIC  X(204).
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
           03  FILLER  PIC X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(004) VALUE "    ".
           03  FILLER  PIC X(001) VALUE " ".
      *******************
      *    画面表示     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC N(011) VALUE
               " 補助残高マスタリスト ".
           03  FILLER  PIC N(004) VALUE  "ＦＲＯＭ".
           03  FILLER  PIC N(002) VALUE  "ＴＯ".
           03  FILLER  PIC N(005) VALUE  "科目コード".
           03  FILLER  PIC N(005) VALUE  "補助コード".
           03  FILLER  PIC N(001) VALUE  "～".
           03  FILLER  PIC N(001) VALUE  "～".
           03  FILLER  PIC X(018) VALUE
               "確認 OK=1,NO=9 ( )".
      ***********************
      *    画面入力         *
      ***********************
       01  ACP-AREA.
           03  ACP-KACD-FROM        PIC 9(04).
           03  ACP-KACD-TO          PIC 9(04).
           03  ACP-HOCD-FROM        PIC 9(04).
           03  ACP-HOCD-TO          PIC 9(04).
           03  ACP-KAKU             PIC X(01).
       COPY  LSMSG_PR.
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
            "CLR-AREA" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "6" "33" "4" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "51" "4" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "8" "33" "4" "02CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA" "X" "8" "51" "4" "03CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA" "X" "24" "77" "1" "04CLR-AREA" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "30" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "N" "4" "31" "8" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "N" "4" "51" "4" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" "N" "6" "11" "10" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-AREA" "N" "8" "11" "10" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "N" "6" "43" "2" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-AREA" "N" "8" "43" "2" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DSP-AREA" "N" "24" "61" "18" "07DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD-FROM" "9" "6" "33" "4" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD-FROM" BY REFERENCE W-KACD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD-TO" "9" "6" "51" "4" "ACP-KACD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD-TO" BY REFERENCE W-KACD-TO "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-HOCD-FROM" "9" "8" "33" "4" "ACP-KACD-TO" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-HOCD-FROM" BY REFERENCE W-HOCD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-HOCD-TO" "9" "8" "51" "4" "ACP-HOCD-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-HOCD-TO" BY REFERENCE W-HOCD-TO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" "ACP-HOCD-TO" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           IF  W-OWARI NOT = SPACE
               GO  TO  PROCE-010
           END-IF.
           PERFORM  MAIN-RTN    THRU  MAIN-EX.
       PROCE-010.
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
            "INPUT" HZM-F_PNAME1 "SHARED" BY REFERENCE HZM-F_IDLST "1"
            "HZM-KEY" BY REFERENCE HZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE  90     TO  LCNT.
           MOVE  "DATE  "     TO  FCTL-KEY.
      ***  コントロールファイル　ＲＥＡＤ
      *           READ  FCTL-F  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-MCT" INV-MCT "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                  RETURNING RESU
               MOVE  "OWARI"     TO  W-OWARI
               GO  TO  INI-EX
           END-IF.
           MOVE  FCTL-KSMM     TO  KETSAN.
           ADD  1     TO  KETSAN.
           IF  KETSAN = 13
               MOVE  1     TO  KETSAN
           END-IF.
      ***  保存しておく。
           MOVE  KETSAN     TO  W-CONTROL-TSUKI.
           MOVE  1        TO  I.
       INI-010.
           IF  I NOT < 13
               GO  TO  INI-EX
           END-IF.
           MOVE  W-CONTROL-TSUKI     TO  W-Z9.
           MOVE  W-Z9                TO  W-01(I).
           IF  I = 1
               MOVE  W-Z9     TO  W-01(13)
           END-IF.
           IF  I = 2
               MOVE  W-Z9     TO  W-01(14)
           END-IF.
           IF  I = 3
               MOVE  W-Z9     TO  W-01(15)
           END-IF.
           ADD  1     TO I.
           ADD  1     TO  W-CONTROL-TSUKI.
           IF  W-CONTROL-TSUKI = 13
               MOVE  1     TO  W-CONTROL-TSUKI
           END-IF.
           GO  TO  INI-010.
       INI-EX.
           EXIT.
      *****************************
      *    ＭＡＩＮ　処理　　　　 *
      *****************************
       MAIN-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KACD-FROM "ACP-KACD-FROM"
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
                 BY REFERENCE ACP-KACD-TO "ACP-KACD-TO"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-KACD-TO = ZERO
               MOVE  ALL "9"     TO  W-KACD-TO
           END-IF.
           IF  W-KACD-FROM > W-KACD-TO
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-HOCD-FROM "ACP-HOCD-FROM"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
       MAIN-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-HOCD-TO "ACP-HOCD-TO"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-030
           END-IF.
           IF  W-HOCD-TO = ZERO
               MOVE  ALL "9"     TO  W-HOCD-TO
           END-IF.
           IF  W-HOCD-FROM > W-HOCD-TO
               GO  TO  MAIN-030
           END-IF.
       MAIN-040.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
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
               GO  TO  MAIN-040
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
           CALL "DB_F_Close" USING
            BY REFERENCE HZM-F_IDLST HZM-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    ＬＳＴ－ＲＴＮ     *
      *************************
       LST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-KACD-FROM     TO  HZM-KMCD.
           MOVE  W-HOCD-FROM     TO  HZM-HOCD.
      *           START  HZM-F  KEY  NOT < HZM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            HZM-F_PNAME1 "HZM-KEY" " NOT < " HZM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                              RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                              RETURNING RESU
      *      ***  データ未登録　表示
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      *
       LST-010.
      ***  SW  ******************
           MOVE  ZERO     TO  SW.
       LST-020.
      ***  補助残高マスタ　ＲＥＡＤ
      *           READ  HZM-F  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HZM-F_PNAME1 BY REFERENCE HZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-030
           END-IF.
           GO  TO  LST-040.
       LST-030.
      ***  SW  ******************
           IF  SW = 1
               GO  TO  LST-050
           ELSE
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
       LST-040.
      ***  科目コードと補助コードでよみ飛ばし
           IF  HZM-KMCD > W-KACD-TO
      ***  SW  ******************
               IF  SW = 1
                   GO  TO  LST-050
               ELSE
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
               END-IF
           END-IF.
           IF  HZM-HOCD < W-HOCD-FROM
               GO  TO  LST-020
           END-IF.
           IF  HZM-HOCD > W-HOCD-TO
               GO  TO  LST-020
           END-IF.
      ***  SW  ******************
           IF  SW = ZERO
               MOVE  1     TO  SW
               MOVE  1     TO  H
               PERFORM  SAVE-RTN     THRU  SAVE-EX
               GO  TO  LST-020
           ELSE
               MOVE  2     TO  SW
               MOVE  2     TO  H
               PERFORM  SAVE-RTN     THRU  SAVE-EX
           END-IF.
       LST-050.
           IF  ( LCNT = 90 )  OR  ( WRITE-CNT = 3 )
               MOVE  ZERO     TO  WRITE-CNT
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF.
           PERFORM  HEAD-RTN    THRU  HEAD-EX.
           PERFORM  MEI-RTN     THRU  MEI-EX.
           ADD  1     TO  WRITE-CNT.
      ***  SW  ******************
           IF  SW = 1
               GO  TO  LST-999
           END-IF.
           GO  TO  LST-010.
       LST-999.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      **************************
      *    ＳＡＶＥ－ＲＴＮ    *
      **************************
       SAVE-RTN.
           MOVE  1     TO  I.
           MOVE  KETSAN     TO J.
           MOVE  HZM-KMCD     TO  W-KACD(H).
           MOVE  HZM-HOCD     TO  W-HOCD(H).
           MOVE  HZM-ZAN      TO  W-ZMZ(H).
           MOVE  HZM-KMCD     TO  K-ACCD.
           MOVE  ZERO         TO  K-HOCD.
      ***  漢字科目マスタ　ＲＥＡＤ
      *           READ  KNG  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  KNGNMN
           END-IF.
           MOVE  KNGNMN     TO  W-KAMEI(H).
           MOVE  HZM-KMCD     TO  K-ACCD.
           MOVE  HZM-HOCD     TO  K-HOCD.
      ***  漢字科目マスタ　ＲＥＡＤ
      *           READ  KNG  WITH  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  KNGNMN
           END-IF.
           MOVE  KNGNMN     TO  W-HOMEI(H).
       SAVE-010.
           IF  I NOT < 13
               GO  TO  SAVE-020
           END-IF.
           MOVE  HZM-TJKR(J)     TO  W-KARI(H I).
           MOVE  HZM-TJKS(J)     TO  W-KASHI(H I).
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  SAVE-010.
       SAVE-020.
           IF  I NOT < 16
               GO  TO  SAVE-EX
           END-IF.
           MOVE  HZM-TJKR(I)     TO  W-KARI(H I).
           MOVE  HZM-TJKS(I)     TO  W-KASHI(H I).
           ADD  1     TO  I.
           GO  TO  SAVE-020.
       SAVE-EX.
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
           MOVE  MID-04 TO PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  6         TO  LCNT.
       MID-EX.
           EXIT.
      ***************************
      *    ＨＥＡＤ－ＲＴＮ     *
      ***************************
       HEAD-RTN.
           MOVE  1     TO  H.
       HEAD-010.
           IF  H NOT < 3
               GO  TO  HEAD-020
           END-IF.
           MOVE  C15   TO  C-15B.
           MOVE  W-KACD(H)     TO  P1-01(H).
           MOVE  W-KAMEI(H)    TO  P1-02(H).
           MOVE  W-HOCD(H)     TO  P1-03(H).
           MOVE  W-HOMEI(H)    TO  P1-04(H).
           MOVE  W-ZMZ(H)      TO  P1-05(H).
           MOVE  C2    TO  C-2B1.
      ***  SW  ******************
           IF  SW = 1
               GO  TO  HEAD-020
           END-IF.
           ADD  1     TO  H.
           GO  TO  HEAD-010.
       HEAD-020.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
       HEAD-EX.
           EXIT.
      ***************************
      *    ＭＥＩ－ＲＴＮ 　　  *
      ***************************
       MEI-RTN.
           MOVE  1     TO  I.
           MOVE  1     TO  H.
       MEI-010.
           IF  H NOT < 3
               GO  TO  MEI-020
           END-IF.
           IF  I NOT < 13
               GO  TO  MEI-030
           END-IF.
           MOVE  C2     TO  C-2B2.
           MOVE  "当期"     TO  PP2-01(H).
           MOVE  W-01(I)      TO  P2-01(H).
           MOVE  "月"       TO  PP2-02(H).
           MOVE  W-KARI(H I)  TO  P2-02(H).
           MOVE  "／"       TO  PP2-03(H).
           MOVE  W-KASHI(H I)  TO  P2-03(H).
      ***  SW  ******************
           IF  SW = 1
               GO  TO  MEI-020
           END-IF.
           ADD  1     TO  H.
           GO  TO  MEI-010.
       MEI-020.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  1     TO  H.
           ADD  1     TO  I.
           GO  TO  MEI-010.
       MEI-030.
           IF  H NOT < 3
               GO  TO  MEI-040
           END-IF.
           IF  I NOT < 16
               GO  TO  MEI-EX
           END-IF.
           MOVE  C2     TO  C-2B2.
           MOVE  "翌期"     TO  PP2-01(H).
           MOVE  W-01(I)      TO  P2-01(H).
           MOVE  "月"       TO  PP2-02(H).
           MOVE  W-KARI(H I)  TO  P2-02(H).
           MOVE  "／"       TO  PP2-03(H).
           MOVE  W-KASHI(H I)  TO  P2-03(H).
      ***  SW  ******************
           IF  SW = 1
               GO  TO  MEI-040
           END-IF.
           ADD  1     TO  H.
           GO  TO  MEI-030.
       MEI-040.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  1     TO  H.
           ADD  1     TO  I.
           GO  TO  MEI-030.
       MEI-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
