       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR080L.
       AUTHOR.           MAYUMI.I.
      *****************************************************
      *    PROGRAM       :  部残マスタリスト　　　　　　  *
      *    PRINTER TYPE  :  JIPS                          *
      *    DATA WRITTEN  :  90/11/28                      *
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
       77  I-Y                     PIC  9(02).
      ***  ファイル上の添字
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
       01  SW                      PIC  9(01).
      ***  (SW = 1,部門ＣＤが変わった)
      ***  (SW = 2,科目ＣＤが変わった)
       01  WRITE-CNT               PIC  9(01).
      ***  WRITE-CNT=4 になったら，改頁する。
       01  HIZUKE                  PIC  9(06).
       01  HIZUKER  REDEFINES  HIZUKE.
           02  YY                  PIC  9(02).
           02  MM                  PIC  9(02).
           02  DD                  PIC  9(02).
       01  OLD-NO-BUCD             PIC  9(04).
       01  OLD-NO-KACD             PIC  9(04).
       01  W-AREA.
           02  W-BUCD-FROM         PIC  9(04).
           02  W-BUCD-TO           PIC  9(04).
           02  W-KACD-FROM         PIC  9(04).
           02  W-KACD-TO           PIC  9(04).
           02  W-KAKU              PIC  X(01).
      *
       01  W-AREA10.
           02  W-AREA11     OCCURS 12.
               03  W-01            PIC  X(02).
      *
       01  MID-01.
           02  F                   PIC  X(05) VALUE  X"1A24212474".
           02  F                   PIC  X(39) VALUE  SPACE.
           02  F                   PIC  N(15) VALUE
               "部　残　マ　ス　タ　リ　ス　ト".
           02  F                   PIC  X(40) VALUE  SPACE.
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
           02  F                   PIC  X(24) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "科　目".
       01  MID-03.
           02  F                   PIC  N(03) VALUE  "コード".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(07) VALUE
                                   "部　　門　　名".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(03) VALUE  "コード".
           02  F                   PIC  X(05) VALUE  SPACE.
           02  F                   PIC  N(07) VALUE
                                   "科　　目　　名".
       01  MID-04.
           02  F                   PIC  X(08) VALUE  SPACE.
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
       01  MID-05.
           02  F                   PIC  X(18) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "借　　方".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "／".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "貸　　方".
           02  F                   PIC  X(15) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "借　　方".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "／".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "貸　　方".
           02  F                   PIC  X(15) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "借　　方".
           02  F                   PIC  X(03) VALUE  SPACE.
           02  F                   PIC  N(01) VALUE  "／".
           02  F                   PIC  X(04) VALUE  SPACE.
           02  F                   PIC  N(04) VALUE  "貸　　方".
      ***
           COPY  LWMSG_PR.
      ***  部残マスタ
           COPY  LBUZAN.
      ***  コントロールファイル
           COPY  FCTL.
      ***  部門名マスタ
           COPY  BUMONF.
      ***  漢字科目マスタ
           COPY  KANGEL.
      ***  プリンター
       01  PRINTF.
      *           LABEL       RECORD  OMITTED
      *           LINAGE          IS       66.
           02  PRINTR                  PIC  X(250).
           02  PRINTR1        REDEFINES   PRINTR.
               03  C-2B            PIC  X(05).
               03  F               PIC  X(01).
               03  P-01            PIC  9(04).
               03  F               PIC  X(03).
               03  P-BNAM          PIC  N(10).
               03  F               PIC  X(03).
               03  P-02            PIC  9(04).
               03  F               PIC  X(03).
               03  P-KNAM          PIC  N(10).
           02  PRINTR2        REDEFINES   PRINTR.
               03  F                   PIC  X(06).
               03  FF     OCCURS 3.
                   04  F               PIC  X(02).
                   04  P-03            PIC  N(02).
                   04  PP-01           PIC  N(01).
                   04  P-04            PIC  ---,---,---,--9.
                   04  PP-02           PIC  N(01).
                   04  P-05            PIC  ---,---,---,--9.
       77  F                  PIC  X(001).
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
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
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
           03  FILLER  PIC N(009) VALUE
               " 部残マスタリスト ".
           03  FILLER  PIC N(004) VALUE  "ＦＲＯＭ".
           03  FILLER  PIC N(002) VALUE  "ＴＯ".
           03  FILLER  PIC N(005) VALUE  "部門コード".
           03  FILLER  PIC N(005) VALUE  "科目コード".
           03  FILLER  PIC N(001) VALUE  "～".
           03  FILLER  PIC N(001) VALUE  "～".
           03  FILLER  PIC X(018) VALUE
               "確認 OK=1,NO=9 ( )".
      ***********************
      *    画面入力         *
      ***********************
       01  ACP-AREA.
           03  ACP-BUCD-FROM        PIC 9(04).
           03  ACP-BUCD-TO          PIC 9(04).
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
            "DSP-AREA" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "32" "18" " " "DSP-AREA"
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
            "ACP-BUCD-FROM" "9" "6" "33" "4" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUCD-FROM" BY REFERENCE W-BUCD-FROM "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-BUCD-TO" "9" "6" "51" "4" " " " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-BUCD-TO" BY REFERENCE W-BUCD-TO "4" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD-FROM" "9" "8" "33" "4" " " " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD-FROM" BY REFERENCE W-KACD-FROM "4" "0"
             RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KACD-TO" "9" "8" "51" "4" " " " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KACD-TO" BY REFERENCE W-KACD-TO "4" "0"
             RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" " " " " RETURNING RESU.
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
            "INPUT" BZM-F_PNAME1 "SHARED" BY REFERENCE BZM-F_IDLST "1"
            "BZM-KEY" BY REFERENCE BZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
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
                 BY REFERENCE ACP-BUCD-FROM "ACP-BUCD-FROM"
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
                 BY REFERENCE ACP-BUCD-TO "ACP-BUCD-TO"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-010
           END-IF.
           IF  W-BUCD-TO = ZERO
               MOVE  ALL "9"     TO  W-BUCD-TO
           END-IF.
           IF  W-BUCD-FROM > W-BUCD-TO
               GO  TO  MAIN-010
           END-IF.
       MAIN-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KACD-FROM "ACP-KACD-FROM"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-020
           END-IF.
       MAIN-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KACD-TO "ACP-KACD-TO"
                 "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "09"
               GO  TO  MAIN-020
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO  TO  MAIN-030
           END-IF.
           IF  W-KACD-TO = ZERO
               MOVE  ALL "9"     TO  W-KACD-TO
           END-IF.
           IF  W-KACD-FROM > W-KACD-TO
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
            BY REFERENCE BZM-F_IDLST BZM-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    ＬＳＴ－ＲＴＮ     *
      *************************
       LST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE  W-BUCD-FROM     TO  BZM-BMON.
           MOVE  W-KACD-FROM     TO  BZM-KMCD.
      *           START  BZM-F  KEY  NOT < BZM-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            BZM-F_PNAME1 "BZM-KEY" " NOT < " BZM-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                  RETURNING RESU
      ***  データ未登録　表示
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           END-IF.
      **
       LST-010.
      ***  部残マスタ　ＲＥＡＤ
      *           READ  BZM-F  NEXT  WITH  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BZM-F_PNAME1 BY REFERENCE BZM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-020
           END-IF.
           GO  TO  LST-030.
       LST-020.
           IF  LCNT = 90
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
      ***  データ未登録　表示
               MOVE  1     TO  RTN-SW
               GO  TO  LST-999
           ELSE
               GO  TO  LST-999
           END-IF.
       LST-030.
      ***  部門コードと科目コードでよみ飛ばし
           IF  BZM-BMON > W-BUCD-TO
               IF  LCNT = 90
                   CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                      RETURNING RESU
                   CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
      ***  データ未登録　表示
                   MOVE  1     TO  RTN-SW
                   GO  TO  LST-999
               ELSE
                   GO  TO  LST-999
               END-IF
           END-IF.
           IF  BZM-KMCD < W-KACD-FROM
               GO  TO  LST-010
           END-IF.
           IF  BZM-KMCD > W-KACD-TO
               GO  TO  LST-010
           END-IF.
           IF  ( LCNT NOT < 63 )  OR  ( WRITE-CNT = 4 )
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF.
      *
           IF  LCNT = 7
               MOVE  1     TO  SW
               GO  TO  LST-040
           END-IF.
           IF  BZM-BMON NOT = OLD-NO-BUCD
               MOVE  1     TO  SW
               GO  TO  LST-040
           END-IF.
           IF  BZM-KMCD NOT = OLD-NO-KACD
               MOVE  2     TO  SW
               GO  TO  LST-040
           END-IF.
      *
       LST-040.
           PERFORM  HEAD-RTN    THRU  HEAD-EX.
           PERFORM  MEI-RTN     THRU  MEI-EX.
           MOVE  BZM-BMON    TO  OLD-NO-BUCD.
           MOVE  BZM-KMCD    TO  OLD-NO-KACD.
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
           MOVE  MID-05 TO PRINTR.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           MOVE  5         TO  LCNT.
       MID-EX.
           EXIT.
      ***************************
      *    ＨＥＡＤ－ＲＴＮ     *
      ***************************
       HEAD-RTN.
      *
           ADD   1     TO  WRITE-CNT.
      *
           MOVE  C2         TO  C-2B.
           MOVE  BZM-BMON   TO  P-01.
           PERFORM BNAM-RTN THRU  BNAM-EX.
           MOVE  BZM-KMCD   TO  P-02.
           PERFORM KNAM-RTN THRU  KNAM-EX.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE      TO  PRINTR.
           MOVE  "月"     TO  PP-01(1).
           MOVE  "／"     TO  PP-02(1).
           MOVE  "月"     TO  PP-01(2).
           MOVE  "／"     TO  PP-02(2).
           MOVE  "月"     TO  PP-01(3).
           MOVE  "／"     TO  PP-02(3).
       HEAD-010.
      *
           MOVE  ZERO     TO  SW.
      *
           MOVE  KETSAN      TO  J.
           MOVE  W-01(1)         TO  P-03(1).
           MOVE  BZM-ZJKR(J)     TO  P-04(1).
           MOVE  BZM-ZJKS(J)     TO  P-05(1).
           MOVE  W-01(1)         TO  P-03(2).
           MOVE  BZM-TJKR(J)     TO  P-04(2).
           MOVE  BZM-TJKS(J)     TO  P-05(2).
           MOVE  W-01(1)         TO  P-03(3).
           MOVE  BZM-TJKR(13)    TO  P-04(3).
           MOVE  BZM-TJKS(13)    TO  P-05(3).
           ADD  1     TO  J.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
       HEAD-EX.
           EXIT.
      ***************************
      *    ＭＥＩ－ＲＴＮ 　　  *
      ***************************
       MEI-RTN.
           MOVE  2    TO  I.
           MOVE  2    TO  I-Y.
       MEI-010.
           IF  I NOT < 13
               GO  TO  MEI-EX
           END-IF.
           MOVE  "月"     TO  PP-01(1).
           MOVE  "／"     TO  PP-02(1).
           MOVE  "月"     TO  PP-01(2).
           MOVE  "／"     TO  PP-02(2).
           MOVE  W-01(I)         TO  P-03(1).
           MOVE  BZM-ZJKR(J)     TO  P-04(1).
           MOVE  BZM-ZJKS(J)     TO  P-05(1).
           MOVE  W-01(I)         TO  P-03(2).
           MOVE  BZM-TJKR(J)     TO  P-04(2).
           MOVE  BZM-TJKS(J)     TO  P-05(2).
           IF  I  NOT >  3
               COMPUTE  I-Y  =  12 + I
               MOVE  "月"     TO  PP-01(3)
               MOVE  "／"     TO  PP-02(3)
               MOVE  W-01(I)         TO  P-03(3)
               MOVE  BZM-TJKR(I-Y)   TO  P-04(3)
               MOVE  BZM-TJKS(I-Y)   TO  P-05(3)
               ADD   1          TO  I-Y
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRINTR2 RETURNING RESP.
           MOVE  SPACE     TO  PRINTR.
           ADD  1     TO  I  J.
           IF  J = 13
               MOVE  1     TO  J
           END-IF.
           GO  TO  MEI-010.
       MEI-EX.
           EXIT.
      ***************************
      *    部門名　取得   　　  *
      ***************************
       BNAM-RTN.
           MOVE  BZM-BMON   TO  BNM-KEY.
      *           READ  BNM        UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE    TO  BNMNMN
           END-IF.
           MOVE  BNMNMN     TO  P-BNAM.
       BNAM-EX.
           EXIT.
      ***************************
      *    科目名　取得   　　  *
      ***************************
       KNAM-RTN.
           MOVE  BZM-KMCD   TO  K-ACCD.
           MOVE  ZERO       TO  K-HOCD.
      *           READ  KNG        UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE    TO  KNGNMN
           END-IF.
           MOVE  KNGNMN     TO  P-KNAM.
       KNAM-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
