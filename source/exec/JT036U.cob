       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      JT036U.
      **************************************************
      **************************************************
      **                                              **
      **         荷  札  ト  ラ  ン  更  新           **
      **                                              **
      **        USER  NAME : 日進ゴム.                **
      **        DATE       : 1988･10･18               **
      **        TYPE       : COBOL                    **
      **        PROGRAM-ID : JT036U                   **
      **        SCREEN-ID  : ------.                  **
      **        AUTHOR     : HAJIME  MIZUNO           **
      **************************************************
      **************************************************
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       SOURCE-COMPUTER.                 SYSTEM150.
       OBJECT-COMPUTER.                 SYSTEM150.
      ******************************************************************
      *                                                                *
      *                 DATA              DIVISION                     *
      *                                                                *
      ******************************************************************
       DATA                             DIVISION.
      ******************************************************************
      *            WORKING     STORAGE     SECTION                     *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  INV-SW                 PIC  9(01).
       77  OKC                    PIC  9(01).
       77  ERR-STAT               PIC  X(02).
       77  JS-SIGN                PIC  9(01).
       77  ERR-SW                 PIC  9(01)  VALUE  0.
       01  WORK-AREA.
           03  ENDFLG             PIC  X(03).
           03  DENNO              PIC  9(06).
           03  DENNO1             PIC  9(06).
           03  DENNO2             PIC  9(06).
           03  DENNO3             PIC  9(06).
           03  WK1-11             PIC S9(03).
           03  I                  PIC  9(02).
           03  KEY-WORK.
               05  NEW-KEY.
                   07  NEW-01     PIC  9(06).
                   07  NEW-02     PIC  9(02).
                   07  NEW-03     PIC  9(03).
               05  OLD-KEY.
                   07  OLD-01     PIC  9(06).
                   07  OLD-02     PIC  9(02).
                   07  OLD-03     PIC  9(03).
           03  XX-12              PIC  9(01).
       01  NF-WK-ID               PIC X(17).
       01  NF-WK-IDR  REDEFINES  NF-WK-ID.
           02  W-ID               PIC X(06).
       01  KBN                        PIC N(03).
       01  SYUKA                      PIC 9(06).
      *
           COPY     LWMSG.
      *
           COPY     LNF-WK.
           COPY     L-JCON.
           COPY     L-JNIF.
           COPY     L-JSTR.
           COPY     LOKJF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      *************************
      *    DISPLAY SECTION    *
      *************************
       01  DSIP-AREA.
           03  CRT-CLR PIC  X(12) VALUE "CLEAR SCREEN".
           03  CRT-01.
               05  FILLER  PIC  X(14) VALUE  "荷札トラン更新".
               05  FILLER  PIC  X(02) VALUE  "（".
               05  DSP-01  PIC  N(03).
               05  FILLER  PIC  X(02) VALUE "）".
           03  DSP-INI3.
               05  FILLER  PIC  X(20) VALUE   "確認（OK=1,NO=9）-->".
               05  FILLER  PIC  X(04) VALUE "ﾘﾀｰﾝ".
       01  APT-AREA.
           03  APT-OKC PIC 9(01).
       01  MSG-AREA.
           02  ERR-OKJ.
               04  FILLER  PIC N(07)  VALUE "ＯＫＪＦ　なし".
               04  FILLER  PIC 9(06).
           COPY     LSMSG.
      ******************************************************************
      *                                                                *
      *                 PROCEDURE         DIVISION                     *
      *                                                                *
      ******************************************************************
       PROCEDURE        DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSIP-AREA
       CALL "SD_Init" USING 
            "DSIP-AREA" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-CLR" "X" "1" "0" "12" " " "DSIP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-01" " " "1" "0" "24" "CRT-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CRT-01" "RX" "1" "30" "14" " " "CRT-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CRT-01" "X" "1" "45" "2" "01CRT-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "N" "1" "47" "6" "02CRT-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-01" BY REFERENCE KBN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04CRT-01" "X" "1" "53" "2" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-INI3" " " "23" "0" "24" "CRT-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI3" "X" "23" "41" "20" " " "DSP-INI3"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-INI3" "X" "23" "62" "4" "01DSP-INI3" " "
            RETURNING RESU.
      *APT-AREA
       CALL "SD_Init" USING 
            "APT-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "APT-OKC" "9" "23" "61" "1" " " "APT-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "APT-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *MSG-AREA
       CALL "SD_Init" USING 
            "MSG-AREA" " " "24" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-OKJ" " " "24" "0" "20" " " "MSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-OKJ" "N" "24" "1" "14" " " "ERR-OKJ" RETURNING RESU.
       CALL "SD_Init" USING 
           "02ERR-OKJ" "9" "24" "17" "6" "01ERR-OKJ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-OKJ" BY REFERENCE NF1-01 "6" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       PROGRM-START.
           PERFORM  INITIAL-RTN   THRU   INITIAL-EXT.
           PERFORM  MAIN-RTN      THRU   MAIN-EXT
                                 UNTIL   ENDFLG  =  "END".
       PROGRAM-ENDING.
           PERFORM  END-RTN       THRU   END-EX.
           CALL "DB_Close".
           STOP     RUN.
      ******************************************************************
      *    INITIAL            初期処理
      ******************************************************************
       INITIAL-RTN.
           PERFORM  JS-ACP-RTN     THRU  JS-ACP-EX.
           IF  IPN-KYO-KBN  =  0
               MOVE  "NF-WK "      TO  W-ID
               MOVE  W-ID          TO  NF-WK_PNAME1
               MOVE  "教　育"      TO  KBN
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE  "NF-WKI"      TO  W-ID
               MOVE  W-ID          TO  NF-WK_PNAME1
               MOVE  "一　般"      TO  KBN
           END-IF
           CALL "SD_Output" USING "CRT-CLR" CRT-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "CRT-01" CRT-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-INI3" DSP-INI3 "p" RETURNING RESU.
      *
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE APT-OKC "APT-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  NOT  =  "01"
               GO  TO  INI-010
           END-IF
           IF  OKC  =  9
               PERFORM  UN-RTN      THRU  UN-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP   RUN
           END-IF
           IF  OKC    NOT =  1
               GO  TO  INI-010
           END-IF
      **
           CALL "DB_F_Open" USING
            "INPUT" NF-WK_PNAME1 " " BY REFERENCE NF-WK_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JNIF_PNAME1 "SHARED" BY REFERENCE JNIF_IDLST "1"
            "JNIF1-KEY" BY REFERENCE JNIF1-KEY.
      *
           MOVE     SPACE  TO    WORK-AREA.
           INITIALIZE            WORK-AREA.
           MOVE     SPACE  TO    KEY-WORK.
      *
           MOVE     1           TO          JCON1-01.
           MOVE     3           TO          JCON1-02.
      *           READ     JCON        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NF-WK_IDLST NF-WK_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNIF_IDLST JNIF_PNAME1
               CALL "DB_Close"
               STOP     RUN
           END-IF.
       INITIAL-EXT.
           EXIT.
      ******************************************************************
      *    END-RTN            終了処理
      ******************************************************************
       END-RTN.
           MOVE     JCON1-03    TO          DENNO1.
           MOVE     JCON1-04    TO          DENNO2.
           MOVE     JCON1-05    TO          DENNO3.
           MOVE     1           TO          JCON1-01.
           MOVE     3           TO          JCON1-02.
      *           READ     JCON        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NF-WK_IDLST NF-WK_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNIF_IDLST JNIF_PNAME1
               CALL "DB_Close"
               STOP     RUN
           END-IF
           MOVE     DENNO1      TO          JCON1-03.
           MOVE     DENNO2      TO          JCON1-04.
           MOVE     DENNO3      TO          JCON1-05.
      *           REWRITE  JCON1-R           INVALID    KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"      TO  ERR-F
               MOVE     "R"         TO  ERR-M
               MOVE     JCON1-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1           TO  ERR-SW
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK_IDLST NF-WK_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JNIF_IDLST JNIF_PNAME1.
      **
           IF  ERR-SW  =  1
               GO  TO  END-EX
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" NF-WK_PNAME1 " " BY REFERENCE NF-WK_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK_IDLST NF-WK_PNAME1.
       END-EX.
           EXIT.
      ******************************************************************
      *    MAIN               メイン  処理
      ******************************************************************
       MAIN-RTN.
      *           READ     NF-WK       AT    END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NF-WK_PNAME1 BY REFERENCE NF-R " " RETURNING RET.
           IF  RET = 1
               MOVE  "END"  TO  ENDFLG
               GO     TO        MAIN-EXT
           END-IF
           MOVE     NF1-01      TO     NEW-01.
           MOVE     NF1-021     TO     NEW-02.
           MOVE     NF1-022     TO     NEW-03.
           IF  NEW-KEY     NOT =      OLD-KEY
               PERFORM    MAIN2-RTN  THRU  MAIN2-EXT
           END-IF.
       MAIN-01.
           IF  NF1-03  =   1 OR 2 OR 3 OR 4 OR 5 OR 6
               PERFORM  PROCES1-RTN  THRU  PROCES1-EXT
           ELSE
               PERFORM  PROCES2-RTN  THRU  PROCES2-EXT
           END-IF
           IF  ERR-SW  =  1
               MOVE  "END"        TO       ENDFLG
               GO  TO  MAIN-EXT
           END-IF
           MOVE     NEW-KEY     TO       OLD-KEY.
       MAIN-EXT.
           EXIT.
      ******************************************************************
      *    MAIN2              メイン２処理
      ******************************************************************
       MAIN2-RTN.
           IF  NF1-12   =  ZERO
               ADD      1          TO   JCON1-03
           END-IF
           IF  NF1-12   =  1
               ADD      1          TO   JCON1-04
           END-IF
           IF  NF1-12   =  2
               ADD      1          TO   JCON1-05
           END-IF
      *
           IF  NF1-12   =  ZERO
               IF  JCON1-03   =  100000
                   MOVE       1        TO      JCON1-03
               END-IF
           END-IF
           IF  NF1-12   =  1
               IF  JCON1-04   =  200000
                   MOVE       100001   TO      JCON1-04
               END-IF
           END-IF
           IF  NF1-12   =  2
               IF  JCON1-05   =  300000
                   MOVE       200001   TO      JCON1-05
               END-IF
           END-IF
      *
           MOVE     ZERO   TO   JNIF1-02.
           IF  NF1-12   =  ZERO
               MOVE  JCON1-03   TO   JNIF1-01 DENNO
           END-IF
           IF  NF1-12   =  1
               MOVE  JCON1-04   TO   JNIF1-01 DENNO
           END-IF
           IF  NF1-12   =  2
               MOVE  JCON1-05   TO   JNIF1-01 DENNO
           END-IF.
       MAIN2-01.
      *           START    JNIF   KEY  NOT  <  JNIF1-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            JNIF_PNAME1 "JNIF1-KEY" " NOT < " JNIF1-KEY RETURNING RET.
           IF  RET = 1
               GO     TO   MAIN2-EXT
           END-IF
      *           READ     JNIF        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JNIF_PNAME1 BY REFERENCE JNIF-R " " RETURNING RET.
           IF  RET = 1
               GO     TO   MAIN2-EXT
           END-IF
           IF  DENNO       NOT  =  JNIF1-01
               GO     TO   MAIN2-EXT
           END-IF
           MOVE     ZERO   TO   JNIF1-KEY.
           IF  NF1-12   =  ZERO
               ADD      1          TO   JCON1-03
               MOVE     JCON1-03   TO   JNIF1-01 DENNO
           END-IF
           IF  NF1-12   =  1
               ADD      1          TO   JCON1-04
               MOVE     JCON1-04   TO   JNIF1-01 DENNO
           END-IF
           IF  NF1-12   =  2
               ADD      1          TO   JCON1-05
               MOVE     JCON1-05   TO   JNIF1-01 DENNO
           END-IF
           GO       TO         MAIN2-01.
      *
       MAIN2-EXT.
           EXIT.
      ******************************************************************
      *    PROCES1            １～６行処理
      ******************************************************************
       PROCES1-RTN.
           MOVE      SPACE      TO     JNIF1-R.
           INITIALIZE                  JNIF1-R.
      *
           IF  NF1-12   =  ZERO
               MOVE     JCON1-03   TO   JNIF1-01
           END-IF
           IF  NF1-12   =  1
               MOVE     JCON1-04   TO   JNIF1-01
           END-IF
           IF  NF1-12   =  2
               MOVE     JCON1-05   TO   JNIF1-01
           END-IF
           MOVE      NF1-01     TO     JNIF1-14.
           MOVE      NF1-03     TO     JNIF1-02.
           MOVE      NF1-04     TO     JNIF1-04.
           MOVE      NF1-05     TO     JNIF1-03.
           MOVE      NF1-061    TO     JNIF1-051.
           MOVE      NF1-062    TO     JNIF1-052.
           MOVE      NF1-07     TO     JNIF1-06.
           MOVE      NF1-08     TO     JNIF1-07.
           MOVE      NF1-09     TO     JNIF1-08.
           MOVE      1          TO            I.
       P-11.
           MOVE      NF1-101(I) TO     JNIF1-091(I).
           ADD       1          TO     I.
           IF  I  NOT =   28
               GO    TO   P-11
           END-IF
           MOVE      NF1-11     TO     JNIF1-13   WK1-11.
      *
           MOVE      ZERO       TO     JNIF1-10.
           MOVE      1          TO     JNIF1-11.
           MOVE      ZERO       TO     JNIF1-12.
           MOVE      NF1-12     TO     JNIF1-13A.
      *
      *           WRITE     JNIF1-R           INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JNIF_PNAME1 JNIF_LNAME JNIF1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JNIF"      TO  ERR-F
               MOVE     "W"         TO  ERR-M
               MOVE     JNIF1-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE     1           TO  ERR-SW
               GO  TO  PROCES1-EXT
           END-IF
           PERFORM  CBL-RTN  THRU  CBL-EX.
           MOVE     NF1-12       TO    XX-12.
       PROCES1-EXT.
           EXIT.
      ******************************************************************
      *    PROCES2                ７行処理
      ******************************************************************
       PROCES2-RTN.
           MOVE      SPACE      TO     JNIF2-R.
           INITIALIZE                  JNIF2-R.
      *
           IF  NF2-12    =  ZERO
               MOVE     JCON1-03   TO   JNIF2-01
           END-IF
           IF  NF2-12    =  1
               MOVE     JCON1-04   TO   JNIF2-01
           END-IF
           IF  NF2-12    =  2
               MOVE     JCON1-05   TO   JNIF2-01
           END-IF
           MOVE      NF2-03     TO     JNIF2-02.
           MOVE      NF2-01     TO     JNIF2-08.
           MOVE      NF2-04     TO     JNIF2-02A.
           MOVE      NF2-05     TO     JNIF2-03.
      *
           MOVE      ZERO       TO     JNIF2-04.
           MOVE      1          TO     JNIF2-05.
           MOVE      ZERO       TO     JNIF2-06.
           MOVE      NF2-12     TO     JNIF2-07A.
      *
      *           WRITE     JNIF2-R           INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JNIF_PNAME1 JNIF_LNAME JNIF2-R RETURNING RET.
           IF  RET = 1
               MOVE     "JNIF"      TO  ERR-F
               MOVE     "W"         TO  ERR-M
               MOVE     JNIF2-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1           TO  ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  PROCES2-EXT
           END-IF
           PERFORM  CBL-RTN  THRU  CBL-EX.
       PROCES2-EXT.
           EXIT.
      ******************************************************************
      *    CBL-RTN            仮クローズ処理
      ******************************************************************
       CBL-RTN.
       CBL-EX.
           EXIT.
      ******************************************************************
      *    UN-RTN             送り状データ自動生成差し戻し処理
      ******************************************************************
       UN-RTN.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE    SPACE  TO  JCON7-KEY.
           MOVE    7      TO  JCON7-01.
      *           READ    JCON           INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               MOVE   "JCON"       TO  ERR-F
               MOVE   "A"          TO  ERR-M
               MOVE    JCON1-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               GO  TO  UN-EX
           END-IF
           IF  IPN-KYO-KBN  =  0
               MOVE    0        TO  JCON7-06
               MOVE    JCON7-05 TO  SYUKA
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE    0        TO  JCON7-08
               MOVE    JCON7-07 TO  SYUKA
           END-IF
      *           REWRITE  JCON1-R           INVALID    KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               MOVE     "JCON"      TO  ERR-F
               MOVE     "R"         TO  ERR-M
               MOVE     JCON1-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1           TO  ERR-SW
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
       UN-010.
      *           READ     JSTR   NEXT     AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UN-090
           END-IF
           IF  JSTR-16  NOT =    IPN-KYO-KBN
               GO  TO  UN-010
           END-IF
           IF  JSTR-04S NOT =    SYUKA
               GO  TO  UN-010
           END-IF
           IF  JSTR-05  NOT =    ZERO
               GO  TO  UN-010
           END-IF
           IF  JSTR-03  NOT =    0 AND 7
               GO  TO  UN-010
           END-IF
           IF  JSTR-14      =    9
               GO  TO  UN-010
           END-IF
           IF  JSTR-4012     =    0
               GO  TO  UN-010
           END-IF
           IF  JSTR-17  NOT =    9
               GO  TO  UN-010
           END-IF.
       UN-020.
           MOVE     ZERO         TO  JSTR-15A.
      *           REWRITE  JSTR-R           INVALID    KEY
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"      TO  ERR-F
               MOVE     "R"         TO  ERR-M
               MOVE     JSTR-KEY    TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1           TO  ERR-SW
               GO  TO  UN-090
           END-IF.
       UN-030.
           MOVE     JSTR-14B     TO  OKJF-01.
      *           READ     OKJF    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" OKJF_PNAME1 BY REFERENCE OKJF-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"      TO  ERR-F
               MOVE     "A"         TO  ERR-M
               MOVE     OKJF-KEY    TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1           TO  ERR-SW
               GO  TO  UN-090
           END-IF
           MOVE     ZERO         TO  OKJF-07  OKJF-10.
      *           REWRITE  OKJF-R           INVALID    KEY
      *///////////////
           CALL "DB_Update" USING
            OKJF_PNAME1 OKJF_LNAME OKJF-R RETURNING RET.
           IF  RET = 1
               MOVE     "OKJF"      TO  ERR-F
               MOVE     "R"         TO  ERR-M
               MOVE     OKJF-KEY    TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1           TO  ERR-SW
               GO  TO  UN-090
           END-IF
           GO  TO  UN-010.
       UN-090.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
       UN-EX.
           EXIT.
      *****************************
      *    ｴﾗｰ DISPLAY (ﾒｲﾝ)      *
      *****************************
       ERR-RTN.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-010.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-EX.
           EXIT.
           COPY    LPACPT.
      *
      *******************    E N D    O F    P R O G R A M    **********
