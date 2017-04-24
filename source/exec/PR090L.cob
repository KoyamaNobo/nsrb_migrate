       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR090L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  科目残高マスタリスト　　　　  *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/12/05                      *
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
       77  I                       PIC  9(02).
      ***  リスト上の添字
       77  J                       PIC  9(02).
      ***  ファイル上の添字
       77  Y                       PIC  9(02).
      ***  ファイル項目の翌期実績の添字 ( 13 OR 14 OR 15 の 時がある。)
       77  W-Z9                    PIC  Z9.
      ***  編集項目
       77  C2                      PIC  X(05)  VALUE  X"1A24212474".
       77  LCNT                    PIC  9(02).
       77  PCNT                    PIC  9(05).
       77  RTN-SW                  PIC  9(01).
      ***  RTN-SW = 1 の時，MAIN-RTN へ戻る。
       01  W-OWARI                 PIC  X(05).
      ***  SPACEじゃなかったら，コントロールファイルのINVALIDでSTOP RUN
       01  W-CONTROL-TSUKI         PIC  9(02).
      ***  決算月をたしていくのに使用。
       01  KETSAN                  PIC  9(02).
      ***  決算月を保存しておく。
       01  WRITE-CNT               PIC  9(01).
      ***  WRITE-CNT=4 になったら，改頁する。
       01  HIZUKE                  PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  W-AREA.
           02  W-KACD-FROM         PIC  9(04).
           02  W-KACD-TO           PIC  9(04).
           02  W-KAKU              PIC  X(01).
      *
      *
       01  W-AREA10.
           02  W-AREA11     OCCURS 12.
               03  W-01            PIC  X(02).
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(19) VALUE
               "科　目　残　高　マ　ス　タ　リ　ス　ト".
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
           02  F                   PIC  N(03) VALUE  "科　目".
           02  F                   PIC  X(06) VALUE  SPACE.
           02  F                   PIC  X(12) VALUE  "科   目   名".
           02  F                   PIC  X(12) VALUE  SPACE.
           02  F                   PIC  N(05) VALUE  "前期末残高".
      *
       01  MID-03.
           02  F                   PIC  N(03) VALUE  "コード".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  X(16) VALUE
                                   "<-------------- ".
           02  F                   PIC  N(03) VALUE  "前　期".
           02  F                   PIC  X(16) VALUE
                                   " -------------->".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  X(16) VALUE
                                   "<-------------- ".
           02  F                   PIC  N(03) VALUE  "当　期".
           02  F                   PIC  X(16) VALUE
                                   " -------------->".
           02  F                   PIC  X(02) VALUE  SPACE.
           02  F                   PIC  X(16) VALUE
                                   "<-------------- ".
           02  F                   PIC  N(03) VALUE  "翌　期".
           02  F                   PIC  X(16) VALUE
                                   " -------------->".
      *
       01  MID-04.
           02  F                   PIC  X(18) VALUE  SPACE.
           02  FFF     OCCURS 3.
               03  F                   PIC  X(07) VALUE  "借   方".
               03  F                   PIC  X(04) VALUE  SPACE.
               03  F                   PIC  N(01) VALUE  "／".
               03  F                   PIC  X(04) VALUE  SPACE.
               03  F                   PIC  X(07) VALUE  "貸   方".
               03  F                   PIC  X(16) VALUE  SPACE.
      ***
           COPY  LWMSG_PR.
      ***  科目残高マスタ
           COPY  LKAZAN.
      ***  コントロールファイル
           COPY  FCTL.
      ***  漢字科目マスタ　　　
           COPY  KANGEL.
      ***  プリンター
       01  PRINTF.
           02  PRINTR              PIC  X(250).
           02  PRINTR1             REDEFINES  PRINTR.
               03  C-2B            PIC  X(05).
               03  F               PIC  X(01).
               03  P-01            PIC  9(04).
               03  F               PIC  X(03).
               03  P-02            PIC  N(10).
               03  F               PIC  X(03).
               03  P-03            PIC  ---,---,---,--9.
           02  PRINTR2             REDEFINES  PRINTR.
               03  F               PIC  X(08).
               03  FF     OCCURS 3.
                   04  P2-01           PIC  N(02).
                   04  PP2-01          PIC  N(01).
                   04  P2-02           PIC  ---,---,---,--9.
                   04  PP2-02          PIC  N(01).
                   04  P2-03           PIC  ---,---,---,--9.
                   04  F               PIC  X(02).
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
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           03  FILLER   PIC  X(004) VALUE "    ".
           03  FILLER   PIC  X(004) VALUE "    ".
           03  FILLER   PIC  X(001) VALUE " ".
      *******************
      *    画面表示     *
      *******************
       01  DSP-AREA.
           03  FILLER  PIC X(022) VALUE
               " 科目残高マスタリスト ".
           03  FILLER  PIC X(008) VALUE  "ＦＲＯＭ".
           03  FILLER  PIC X(004) VALUE  "ＴＯ".
           03  FILLER  PIC X(010) VALUE  "科目コード".
           03  FILLER  PIC X(002) VALUE  "～".
           03  FILLER  PIC X(018) VALUE  "確認 OK=1,NO=9 ( )".
      ***********************
      *    画面入力         *
      ***********************
       01  ACP-AREA.
           03  ACP-KACD-FROM        PIC 9(04).
           03  ACP-KACD-TO          PIC 9(04).
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
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "32" "22" " " "DSP-AREA"
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
            "05DSP-AREA" "N" "6" "43" "2" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "N" "24" "61" "18" "05DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
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
            "ACP-KAKU" "X" "24" "77" "1" "ACP-GINCD-TO" " "
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
               GO  TO  PROCE-010.
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
            "INPUT" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           MOVE  90     TO  LCNT.
      *
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
      ***  足しておく。
           MOVE  1        TO  I.
       INI-010.
           IF  I NOT < 13
               GO  TO  INI-EX
           END-IF.
           MOVE  W-CONTROL-TSUKI     TO  W-Z9.
           MOVE  W-Z9                TO  W-01(I).
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
                 BY REFERENCE ACP-KAKU "ACP-KAKU"
                 "X" "1" BY REFERENCE ESTAT RETURNING RESU.
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
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
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
           MOVE  W-KACD-FROM     TO  KZM-KEY.
      *           START  KZM-F  KEY  NOT < KZM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            KZM-F_PNAME1 "KZM-KEY" " NOT < " KZM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                              RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                              RETURNING RESU
      *      ***  データ未登録　表示
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      **
       LST-010.
      ***  科目残高マスタ　ＲＥＡＤ
      *           READ  KZM-F  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" KZM-F_PNAME1 BY REFERENCE KZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-999
           END-IF.
      ***  科目コードでよみ飛ばし
           IF  KZM-KMCD > W-KACD-TO
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
           IF  ( LCNT NOT < 62 )  OR  ( WRITE-CNT = 4 )
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
      *
           MOVE  ZERO     TO  WRITE-CNT.
      *
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
      *
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
      *    ＭＥＩ－ＲＴＮ 　　  *
      ***************************
       MEI-RTN.
      *
           ADD  1     TO  WRITE-CNT.
      *
           MOVE  KZM-KMCD     TO  K-ACCD.
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
           MOVE  C2     TO  C-2B.
           MOVE  KZM-KMCD     TO  P-01.
           MOVE  KNGNMN       TO  P-02.
           MOVE  KZM-ZAN      TO  P-03.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
      *
           MOVE  1          TO  I.
           MOVE  KETSAN     TO  J.
           MOVE  13         TO  Y.
      *
       MEI-010.
           IF  I NOT < 13
               GO  TO  MEI-EX
           END-IF.
           MOVE  "月"     TO  PP2-01(1)  PP2-01(2).
           MOVE  "／"     TO  PP2-02(1)  PP2-02(2).
           MOVE  W-01(I)    TO  P2-01(1)  P2-01(2).
           MOVE  KZM-ZJKR(J)     TO  P2-02(1).
           MOVE  KZM-ZJKS(J)     TO  P2-03(1).
           MOVE  KZM-TJKR(J)     TO  P2-02(2).
           MOVE  KZM-TJKS(J)     TO  P2-03(2).
           IF  I  NOT > 3
               MOVE  "月"     TO  PP2-01(3)
               MOVE  "／"     TO  PP2-02(3)
               MOVE  W-01(I)    TO  P2-01(3)
               MOVE  KZM-TJKR(Y)     TO  P2-02(3)
               MOVE  KZM-TJKS(Y)     TO  P2-03(3)
               ADD  1     TO  Y
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
      *
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  MEI-010.
       MEI-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
