       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR625L.
       AUTHOR.           KOMATSUBARA.
      *****************************************************
      *    振替伝票（消費税振替）                 　　　  *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.     SYSTEM3100.
       OBJECT-COMPUTER.     SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC  X(02).
       77  CHK                     PIC  9(01).
       01  GAMEN-AREA.
           02  W-TEST              PIC  9(01).
       01  SW-AREA.
           02  END-SW              PIC  9(01)  VALUE 0.
           02  FST-SW              PIC  9(01)  VALUE 1.
       01  OLD-NO                  PIC  9(06)  VALUE ZERO.
       01  SOEJI-AREA.
           02  I                   PIC  9(01).
       01  PRINT-AREA.
           02  W-CNT               PIC  9(02).
           02  W-KRKEI             PIC S9(10).
           02  W-KSKEI             PIC S9(10).
           02  W-YMD.
               03  W-YY            PIC  9(04).
               03  W-YYL  REDEFINES W-YY.
                 04  W-YY1         PIC  9(02).
                 04  W-YY2         PIC  9(02).
               03  W-MM            PIC  9(02).
               03  W-DD            PIC  9(02).
           02  W-KARINAM.
               03  W-KARI1         PIC  N(08).
               03  W-KARI2         PIC  N(02).
           02  W-KASINAM.
               03  W-KASI1         PIC  N(08).
               03  W-KASI2         PIC  N(02).
       01  HEAD1.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(008) VALUE X"1A26212068222176".
           02  F            PIC  X(041) VALUE SPACE.
           02  F            PIC  N(008) VALUE "　振　替　伝　票".
           02  F            PIC  X(033) VALUE SPACE.
           02  F            PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
       01  HEAD2.
           02  F            PIC  X(040) VALUE SPACE.
           02  F            PIC  N(002) VALUE "平成".
           02  F            PIC  X(002) VALUE SPACE.
           02  H-NEN        PIC  N(002).
           02  F            PIC  N(001) VALUE "年".
           02  F            PIC  X(001) VALUE SPACE.
           02  H-GET        PIC  N(002).
           02  F            PIC  N(001) VALUE "月".
           02  F            PIC  X(001) VALUE SPACE.
           02  H-PEY        PIC  N(002).
           02  F            PIC  N(001) VALUE "日".
           02  F            PIC  X(019) VALUE SPACE.
           02  F            PIC  N(001) VALUE "№".
           02  F            PIC  X(003) VALUE SPACE.
           02  H-NO         PIC  9(006).
           02  F            PIC  X(002) VALUE SPACE.
       01  HEAD3.
           02  F            PIC  X(014) VALUE SPACE.
           02  F            PIC  N(005) VALUE "科　目　名".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(001) VALUE "金".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(001) VALUE "額".
           02  F            PIC  X(010) VALUE SPACE.
           02  F            PIC  N(001) VALUE "摘".
           02  F            PIC  X(010) VALUE SPACE.
           02  F            PIC  N(001) VALUE "要".
           02  F            PIC  X(011) VALUE SPACE.
           02  F            PIC  N(005) VALUE "科　目　名".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(001) VALUE "金".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(001) VALUE "額".
           02  F            PIC  X(007) VALUE SPACE.
       01  HEAD9.
           02  F            PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(043) VALUE SPACE.
           02  F            PIC  N(016) VALUE
                "日　進　ゴ　ム　株　式　会　社　".
           02  F            PIC  X(031) VALUE SPACE.
           02  F            PIC  X(005) VALUE X"1A24212474".
       01  W-PD.
           02  F            PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(092) VALUE SPACE.
           02  P-IM         PIC  N(004).
           02  F            PIC  X(005) VALUE X"1A24212474".
       01  P-R4.
           02  P4-KCD1         PIC  X(05).
           02  FILLER          PIC  X(13).
           02  P4-KRKA         PIC  N(08).
           02  P4-KRKIN        PIC  -(11).
           02  FILLER          PIC  X(02).
           02  P4-F1           PIC  X(01).
           02  P4-KRTEK        PIC  9(04).
           02  P4-KRTEKR       REDEFINES  P4-KRTEK
                               PIC  X(04).
           02  P4-F2           PIC  X(01).
           02  FILLER          PIC  X(03).
           02  P4-NAME         PIC  N(08).
           02  FILLER          PIC  X(01).
           02  P4-F3           PIC  X(01).
           02  P4-KSTEK        PIC  9(04).
           02  P4-KSTEKR       REDEFINES  P4-KSTEK
                               PIC  X(04).
           02  P4-F4           PIC  X(01).
           02  FILLER          PIC  X(03).
           02  P4-KSKA         PIC  N(08).
           02  P4-KSKIN        PIC  -(11).
           02  FILLER          PIC  X(05).
           02  P4-KCD2         PIC  X(05).
       01  P-R5.
           02  FILLER          PIC  X(25).
           02  P5-KRKEI        PIC  -(11).
           02  FILLER          PIC  X(45).
           02  P5-KSKEI        PIC  -(11).
           02  FILLER          PIC  X(05).
      ***
           COPY  LWMSG_PR.
           COPY  LIBFDD.
           COPY  LNSSIW.
           COPY  KANGEL.
      *
       77  P-R                     PIC  X(250).
      ***
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      ******************************
      *　　画面クリアー項目　　    *
      ******************************
       01  DSP-CLR.
           03  FILLER     PIC  X(12)  VALUE "CLEAR SCREEN".
      *******************
      *    画面表示     *
      *******************
       01  DSP-AREA.
           03  FILLER     PIC X(20) VALUE  "振替伝票(消費税振替)".
           03  FILLER     PIC X(16) VALUE  "ﾃｽﾄﾌﾟﾘﾝﾄ.....( )".
           03  FILLER     PIC X(4)  VALUE  "9:ｽﾙ".
           03  FILLER     PIC X(5)  VALUE  "1:ｼﾅｲ".
      ***********************
      *    画面入力         *
      ***********************
       01  ACP-AREA.
           03  ACP-TEST   PIC 9(01).
       COPY  LSMSG_PR.
       COPY  LIBSCR.
      ***
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999-FTG710" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "CLEAR" "X" "0" "0" "12" " " "DSP-CLR"  RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "45" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RX" "1" "21" "20" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "X" "5" "21" "16" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "X" "5" "39" "4" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" "X" "6" "39" "5" "03DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEST" "9" "5" "35" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-TEST" BY REFERENCE W-TEST "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           PERFORM  MAIN-RTN    THRU  MAIN-EX
                    UNTIL       END-SW  =  1.
           PERFORM  CLSE-ENT    THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP  RUN.
      **************************
      *    初期処理            *
      **************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NS-SIW_PNAME1 "EXCLUSIVE" BY REFERENCE 
            NS-SIW_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           COPY LIBCPR.
       INI-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TEST "ACP-TEST" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INI-010
           END-IF.
           IF  W-TEST  NOT =  1 AND 9
               GO  TO  INI-010
           END-IF.
           IF  W-TEST  =  9
               PERFORM  TEST-RTN  THRU  TEST-EX
               GO  TO  INI-010
           END-IF.
       INI-EX.
           EXIT.
      *****************************
      *    ＭＡＩＮ　処理　　　　 *
      *****************************
       MAIN-RTN.
      *           READ  NS-SIW  AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NS-SIW_PNAME1 BY REFERENCE SIW-R " " RETURNING RET.
           IF  RET = 1
               MOVE  1      TO  END-SW
               PERFORM  KEI-RTN  THRU  KEI-EX
               GO  TO  MAIN-EX
           END-IF.
           IF  SIW-02  NOT =  OLD-NO
               MOVE  SIW-02   TO  OLD-NO
               PERFORM  KEI-RTN   THRU  KEI-EX
               PERFORM  HEAD-RTN  THRU  HEAD-EX
           END-IF.
           PERFORM  MEISAI-RTN  THRU  MEISAI-EX.
       MAIN-EX.
           EXIT.
      ************************
      *    終了処理          *
      ************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-SIW_IDLST NS-SIW_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
      *************************
      *    テスト印字         *
      *************************
       TEST-RTN.
           MOVE  99           TO  H-NEN  H-GET  H-PEY  H-NO.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD1        TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD2        TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  "承認印　" TO  P-IM.
           MOVE  W-PD         TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD3        TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  SPACE        TO  P-R4.
           MOVE  W-15K        TO  P4-KCD1.
           MOVE  W-20K        TO  P4-KCD2.
           MOVE  ALL "　"   TO  P4-KRKA  P4-KSKA  P4-NAME.
           MOVE  ALL "Ｎ"   TO  P4-KRKA   P4-KSKA   P4-NAME.
           MOVE  "9999"       TO  P4-KRTEK  P4-KSTEK.
           MOVE  9999999999   TO  P4-KRKIN  P4-KSKIN.
           MOVE  "("          TO  P4-F1     P4-F3.
           MOVE  ")"          TO  P4-F2     P4-F4.
           MOVE  P-R4         TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           PERFORM  TEST-LOOP-RTN  THRU  TEST-LOOP-EX
                    VARYING   I    FROM  1  BY  1
                    UNTIL     I    >     5.
           MOVE  SPACE        TO  P-R5.
           MOVE  9999999999   TO  P5-KRKEI  P5-KSKEI.
           MOVE  P-R5         TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD9        TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       TEST-EX.
           EXIT.
      ****************************
      *    合計処理      　      *
      ****************************
       KEI-RTN.
           IF  FST-SW  =  1
               GO  TO  KEI-EX
           END-IF.
           PERFORM  SPAMEI-RTN  THRU  SPAMEI-EX.
           MOVE  SPACE        TO  P-R5.
           MOVE  W-KRKEI      TO  P5-KRKEI.
           MOVE  W-KSKEI      TO  P5-KSKEI.
           MOVE  SPACE        TO  P-R.
           MOVE  P-R5         TO  P-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD9        TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  0            TO  W-CNT.
           MOVE  ZERO         TO  W-KRKEI.
           MOVE  ZERO         TO  W-KSKEI.
       KEI-EX.
           EXIT.
      ***************************
      *    ＨＥＡＤ－ＲＴＮ     *
      ***************************
       HEAD-RTN.
           IF  FST-SW  =  0
               MOVE  SPACE    TO  P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           ELSE
               MOVE  0        TO  FST-SW
           END-IF.
           MOVE  SIW-01       TO  W-YMD.
           SUBTRACT  DATE-YC1 FROM W-YY.
           MOVE  W-YY2        TO  H-NEN.
           MOVE  W-MM         TO  H-GET.
           MOVE  W-DD         TO  H-PEY.
           MOVE  SIW-02       TO  H-NO.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD1        TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD2        TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  "承認印　" TO  P-IM.
           MOVE  W-PD         TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           MOVE  HEAD3        TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
       HEAD-EX.
           EXIT.
      ***************************
      *    明細印字       　　  *
      ***************************
       MEISAI-RTN.
           MOVE  SIW-041      TO  KNG-KEY.
      *           READ  KNG          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "？" TO  KNGNMN
           END-IF.
           MOVE  KNGNMN       TO  W-KARINAM.
           MOVE  SIW-051      TO  KNG-KEY.
      *           READ  KNG          UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "？" TO  KNGNMN
           END-IF.
           MOVE  KNGNMN       TO  W-KASINAM.
           MOVE  SPACE        TO  P-R4.
           MOVE  W-15K        TO  P4-KCD1.
           MOVE  W-20K        TO  P4-KCD2.
           MOVE  ALL "　"   TO  P4-KRKA  P4-KSKA  P4-NAME.
           MOVE  W-KARI1      TO  P4-KRKA.
           IF  SIW-042  NOT =  ZERO
               MOVE  "("      TO  P4-F1
               MOVE  SIW-042  TO  P4-KRTEK
               MOVE  ")"      TO  P4-F2
           ELSE
               MOVE  SPACE    TO  P4-F1  P4-F2  P4-KRTEKR
               IF  SIW-90     =   "1" OR "5"
                   MOVE  "*"      TO  P4-KRTEKR
               ELSE
                   MOVE  "#"      TO  P4-KRTEKR
               END-IF
           END-IF.
           MOVE  SIW-045      TO  P4-KRKIN.
           MOVE  "　消費税振替　　"    TO  P4-NAME.
           MOVE  W-KASI1      TO  P4-KSKA.
           IF  SIW-052  NOT =  ZERO
               MOVE  "("      TO  P4-F3
               MOVE  SIW-052  TO  P4-KSTEK
               MOVE  ")"      TO  P4-F4
           ELSE
               MOVE  SPACE    TO  P4-F3  P4-F4  P4-KSTEKR
               IF  SIW-90     =   "1" OR "5"
                   MOVE  "*"      TO  P4-KSTEKR
               ELSE
                   MOVE  "#"      TO  P4-KSTEKR
               END-IF
           END-IF.
           MOVE  SIW-055      TO  P4-KSKIN.
           MOVE  P-R4         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           ADD   SIW-045      TO  W-KRKEI.
           ADD   SIW-055      TO  W-KSKEI.
           ADD   1            TO  W-CNT.
           IF  W-CNT    =  2  OR  4  OR  6
               MOVE  SPACE        TO  P-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
               GO  TO  MEISAI-EX
           END-IF.
           MOVE  SPACE        TO  P-IM.
           IF  W-CNT    =  1
               MOVE  "検　印　"   TO  P-IM
           END-IF.
           IF  W-CNT    =  3
               MOVE  "記帳印　"   TO  P-IM
           END-IF.
           IF  W-CNT    =  5
               MOVE  "入力印　"   TO  P-IM
           END-IF.
           MOVE  SPACE        TO  P-R.
           MOVE  W-PD         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
       MEISAI-EX.
           EXIT.
      ***************************
      *    △明細印字     　　  *
      ***************************
       SPAMEI-RTN.
           IF  W-CNT    =  5
               GO  TO  SPAMEI-EX
           END-IF.
           MOVE  SPACE        TO  P-R4.
           MOVE  W-15K        TO  P4-KCD1.
           MOVE  W-20K        TO  P4-KCD2.
           MOVE  ALL "　"   TO  P4-KRKA  P4-KSKA  P4-NAME.
           MOVE  P-R4         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           ADD   1            TO  W-CNT.
           IF  W-CNT    =  2  OR  4
               MOVE  SPACE        TO  P-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
               GO  TO  SPAMEI-RTN
           END-IF.
           MOVE  SPACE        TO  P-IM.
           IF  W-CNT    =  1
               MOVE  "検　印　"   TO  P-IM
           END-IF.
           IF  W-CNT    =  3
               MOVE  "記帳印　"   TO  P-IM
           END-IF.
           IF  W-CNT    =  5
               MOVE  "入力印　"   TO  P-IM
           END-IF.
           MOVE  SPACE        TO  P-R.
           MOVE  W-PD         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           IF  W-CNT  NOT  =  5
               GO  TO  SPAMEI-RTN
           END-IF.
       SPAMEI-EX.
           EXIT.
      ***************************
      *    テスト明細 (OCCURS 5)*
      ***************************
       TEST-LOOP-RTN.
           MOVE  SPACE        TO  P-R4.
           MOVE  W-15K        TO  P4-KCD1.
           MOVE  W-20K        TO  P4-KCD2.
           MOVE  ALL "　"   TO  P4-KRKA  P4-KSKA  P4-NAME.
           MOVE  ALL "Ｎ"   TO  P4-KRKA   P4-KSKA   P4-NAME.
           MOVE  "9999"       TO  P4-KRTEK  P4-KSTEK.
           MOVE  9999999999   TO  P4-KRKIN  P4-KSKIN.
           MOVE  "("          TO  P4-F1    P4-F3.
           MOVE  ")"          TO  P4-F2    P4-F4.
           MOVE  P-R4         TO  P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
       TEST-LOOP-EX.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
