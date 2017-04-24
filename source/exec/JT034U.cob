       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT034U.
       AUTHOR.             I.NAKANISHI.
      ***********************************************************
      *    PROGRAM      :  送り状データファイル生成４           *
      *    DATA WRITTEN :  63/09/30                             *
      *    SCREEN USED  :  UNUSED                               *
      *    FORM   USED  :  UNUSED                               *
      *    PRINTER TYPE :  UNUSED                               *
      *    COMPILE TYPE :  COBOL                                *
      ***********************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT                PIC X(02)   VALUE  SPACE.
       77  ERR-SW                  PIC 9(01)   VALUE  0.
       77  END-SW                  PIC 9(01)   VALUE  0.
       77  JS-SIGN                 PIC 9(01).
       01  CNT                     PIC 9(03).
       01  WK-AREA.
           03  I                   PIC 9(02).
           03  P                   PIC 9(02).
           03  W-DENNO             PIC 9(06).
           03  W-GYO               PIC 9(01).
           03  W-SYU               PIC 9(03).
           03  W-SET               PIC 9(03).
           03  W-SYUSZ.
               05  W-SYUSU   OCCURS   27     PIC S9(03).
           03  W-SOEJI             PIC 9(02).
           03  OLD-KEY.
               05  OLD-01          PIC 9(06).
               05  OLD-02.
                   07  OLD-021     PIC 9(02).
                   07  OLD-022     PIC 9(03).
               05  OLD-04          PIC 9(06).
           03  W-SIZTBL.
               05  W-SIZCD   OCCURS   4.
                   07  W-SIZ    OCCURS  10   PIC 9(03).
           03  W-SOETBL.
               05  W-SOECD   OCCURS   4.
                   07  W-SOE    OCCURS  10   PIC 9(02).
           03  KBN                 PIC N(03).
       01  W-CODE.
           03  W-COD1        PIC X(30)
                    VALUE   "063072081090201301401000000000".
           03  W-COD2        PIC X(30)
                    VALUE   "125130135140150160170180190200".
           03  W-COD3        PIC X(30)
                    VALUE   "210215220225230235240245250255".
           03  W-COD4        PIC X(30)
                    VALUE   "260265270275280290300000000000".
       01  W-XXX.
           03  W-XX1         PIC X(20)
                    VALUE   "01020304050607000000".
           03  W-XX2         PIC X(20)
                    VALUE   "01020304050607080910".
           03  W-XX3         PIC X(20)
                    VALUE   "11121314151617181920".
           03  W-XX4         PIC X(20)
                    VALUE   "21222324252627000000".
       01  NF-WK-ID                   PIC X(17).
       01  NF-WK-IDR     REDEFINES    NF-WK-ID.
           02  W-ID                   PIC X(06).
       01  NF-WK1-ID                  PIC X(17).
       01  NF-WK1-IDR    REDEFINES    NF-WK1-ID.
           02  W-ID1                  PIC X(07).
      ***
       COPY  LWMSG.
      *
           COPY    LNFWK1.
           COPY    L-JSTR.
           COPY    LOKJF.
           COPY    LNF-WK.
           COPY    L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-INI.
           02  FILLER  PIC X(28) VALUE " ".
           02  FILLER  PIC X(26) VALUE "送り状データファイル生成４".
           02  FILLER  PIC X(02) VALUE "（".
           02  DSP-01  PIC N(03).
           02  FILLER  PIC X(02) VALUE "）".
       01  DSP-END.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-ERR.
           03  ERR-DAT.
               05  FILLER  PIC X(22) VALUE  "＊　該当データ無し　＊".
               05  FILLER  PIC 9(06).
      *
       COPY  LSMSG.
      *
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-CLR
       CALL "SD_Init" USING
           "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-INI
       CALL "SD_Init" USING 
            "DSP-INI" " " "1" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INI" "RX" "1" "20" "28" " " "DSP-INI"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-INI" "X" "1" "21" "26" "01DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-INI" "X" "1" "49" "2" "02DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "N" "1" "51" "6" "03DSP-INI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-01" BY REFERENCE KBN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-INI" "X" "1" "57" "2" "DSP-01" " " RETURNING RESU.
      *DSP-END
       CALL "SD_Init" USING
           "DSP-END" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-END" "X" "1" "0" "12" " " "DSP-END" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "28" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-DAT" " " "24" "0" "28" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-DAT" "X" "24" "1" "22" " " "ERR-DAT" RETURNING RESU.
       CALL "SD_Init" USING 
           "02ERR-DAT" "9" "24" "24" "6" "01ERR-DAT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-DAT" BY REFERENCE W-DENNO "6" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *************************
      *    メイン　ルーチン   *
      *************************
       MAIN.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  UPD-RTN    THRU  UPD-EX.
           IF  ERR-SW  NOT =  1
               PERFORM  UPD2-RTN  THRU  UPD2-EX
           END-IF
           PERFORM  END-RTN    THRU  END-EX.
           CALL "SD_Output" USING "DSP-END" DSP-END "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *----------------------*
      *    ＩＮＩ－ＲＴＮ    *
      *----------------------*
       INI-RTN.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN  =  0
               MOVE  "教　育"  TO  KBN
               MOVE "NF-WK "     TO  W-ID
               MOVE  W-ID        TO  NF-WK_PNAME1
               MOVE "NF-WK1 "    TO  W-ID1
               MOVE  W-ID1       TO  NF-WK1_PNAME1
           END-IF
           IF  JS-SIGN  =  1
               MOVE  "一　般"  TO  KBN
               MOVE "NF-WKI"     TO  W-ID
               MOVE  W-ID        TO  NF-WK_PNAME1
               MOVE "NF-WK1I"    TO  W-ID1
               MOVE  W-ID1       TO  NF-WK1_PNAME1
           END-IF
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-INI" DSP-INI "p" RETURNING RESU.
      *
           INITIALIZE       WK-AREA.
      **
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" NF-WK1_PNAME1 
            "EXCLUSIVE" BY REFERENCE NF-WK1_IDLST
            "1" "WK1-KEY" BY REFERENCE WK1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" OKJF_PNAME1 "SHARED" BY REFERENCE OKJF_IDLST "1"
            "OKJF-KEY" BY REFERENCE OKJF-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" NF-WK_PNAME1 "EXCLUSIVE" BY REFERENCE
            NF-WK_IDLST "0".
      **
       INI-EX.
           EXIT.
      *----------------------*
      *    ＥＮＤ－ＲＴＮ    *
      *----------------------*
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK1_IDLST NF-WK1_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE OKJF_IDLST OKJF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NF-WK_IDLST NF-WK_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       END-EX.
           EXIT.
      *--------------------------------*
      *    データ　　抽出　            *
      *--------------------------------*
       UPD-RTN.
      *           READ     NF-WK1   NEXT  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NF-WK1_PNAME1 BY REFERENCE WK1-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     1        TO  END-SW
               GO       TO       UPD-010
           END-IF
           IF  WK1-01   NOT =    OLD-01
               GO  TO  UPD-010
           END-IF
           IF  WK1-02   NOT =    OLD-02
               GO  TO  UPD-010
           END-IF
           IF  WK1-04   NOT =    OLD-04
               GO  TO  UPD-020
           END-IF
           GO    TO       UPD-030.
       UPD-010.
           IF  OLD-01   NOT =    ZERO
               ADD      1        TO    CNT
               PERFORM  TEK-RTN  THRU  TEK-EX
           END-IF
           IF  ERR-SW   =        1
               GO       TO       UPD-EX
           END-IF.
       UPD-020.
           IF  OLD-04   NOT =    ZERO
               ADD      1        TO    W-GYO
               PERFORM  DAT-RTN  THRU  DAT-EX
           END-IF
           IF  ERR-SW   =        1
               GO       TO       UPD-EX
           END-IF
           IF  END-SW   =        1
               GO  TO  UPD-EX
           END-IF
           INITIALIZE        W-SYUSZ.
           IF  (OLD-01   NOT =    WK1-01)
               OR      (OLD-02   NOT =    WK1-02)
               MOVE     0        TO     W-GYO
           END-IF
           IF  OLD-01   =        WK1-01
               GO       TO        UPD-030
           END-IF
           MOVE     WK1-01   TO       OKJF-KEY.
      *           READ     OKJF     INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R " " RETURNING RET.
           IF  RET = 1
               MOVE    "OKJF"     TO    ERR-F
               MOVE     OKJF-KEY  TO    ERR-K
               MOVE    "A"        TO    ERR-M
               PERFORM  ERR-RTN   THRU  ERR-EX
               MOVE     1         TO    ERR-SW
               GO       TO        UPD-EX
           END-IF
           MOVE     0        TO       CNT.
       UPD-030.
           COMPUTE  W-SYU    =    1000    -     WK1-05.
           MOVE     W-CODE   TO       W-SIZTBL.
           MOVE     W-XXX    TO       W-SOETBL.
           MOVE     1        TO       I   P.
       UPD-040.
           IF  I        >        4
               GO  TO  UPD-050
           END-IF
           IF  W-SYU    =        W-SIZ(I , P)
               MOVE     W-SOE(I , P)  TO  W-SOEJI
               GO       TO        UPD-050
           END-IF
           ADD      1        TO       P.
           IF  P        >        10
               ADD      1         TO       I
               MOVE     1         TO       P
           END-IF
           GO       TO       UPD-040.
       UPD-050.
           MOVE     WK1-11   TO       W-SYUSU(W-SOEJI).
           MOVE     WK1-06   TO       W-SET.
           MOVE     WK1-01   TO       OLD-01.
           MOVE     WK1-02   TO       OLD-02.
           MOVE     WK1-04   TO       OLD-04.
           MOVE     WK1-08   TO       W-DENNO.
           GO       TO       UPD-RTN.
       UPD-EX.
           EXIT.
      ***************************************
      *    データ部更新　処理ルーチン       *
      ***************************************
       DAT-RTN.
           MOVE     SPACE    TO       NF-R1.
           INITIALIZE                 NF-R1.
           MOVE     OLD-01   TO       NF1-01.
           MOVE     OLD-02   TO       NF1-02.
           MOVE     W-GYO    TO       NF1-03.
           MOVE     OKJF-03  TO       NF1-04.
           MOVE     OLD-04   TO       NF1-05.
           MOVE     OKJF-05  TO       NF1-06  NF1-13.
           MOVE     OKJF-02  TO       NF1-07.
           MOVE     OKJF-04  TO       NF1-08.
           IF  W-SET  =  1
               IF  CNT   NOT   >   1
                   MOVE  1        TO  NF1-09
               ELSE
                   MOVE  0        TO  NF1-09
               END-IF
           ELSE
               MOVE     OKJF-07  TO       NF1-09
           END-IF
           IF  NF1-09              =  1
               IF  OKJF-07       NOT  =  NF1-09
                   MOVE     OKJF-07  TO       NF1-09
               END-IF
           END-IF
           MOVE     W-SYUSZ  TO       NF1-10.
           IF  W-SET    =        0
               MOVE     1         TO     NF1-11
           ELSE
               MOVE     W-SET    TO        NF1-11
           END-IF
           PERFORM  JSTR-RTN THRU     JSTR-EX.
           IF  ERR-SW   =        1
               GO       TO        DAT-EX
           END-IF
           MOVE     JS-SIGN  TO       NF1-12.
      *
      *           WRITE    NF-R1.
      *//////////////
           CALL "DB_Insert" USING
            NF-WK_PNAME1 NF-WK_LNAME NF-R1 RETURNING RET.
           IF  ERR-STAT NOT =   "00"
               MOVE    "NF-WK"    TO   ERR-F
               MOVE     NF-R1     TO   ERR-K
               MOVE    "W"        TO   ERR-M
               PERFORM  ERR-RTN   THRU     ERR-EX
               MOVE     1         TO   ERR-SW
               GO       TO        DAT-EX
           END-IF.
       DAT-EX.
           EXIT.
      ***************************************
      *    摘要データ更新処理ルーチン       *
      ***************************************
       TEK-RTN.
           MOVE     SPACE    TO       NF-R2.
           INITIALIZE                 NF-R2.
           MOVE     OLD-01   TO       NF2-01.
           MOVE     OLD-02   TO       NF2-02.
           MOVE     7        TO       NF2-03.
           PERFORM  JSTR-RTN THRU     JSTR-EX.
           IF  ERR-SW   =        1
               GO       TO        TEK-EX
           END-IF
           MOVE     JSTR-14D TO       NF2-04.
           MOVE     JSTR-15  TO       NF2-05.
           MOVE     JS-SIGN  TO       NF2-12.
           MOVE     JSTR-06  TO       NF2-99.
      *
      *           WRITE    NF-R2.
      *//////////////
           CALL "DB_Insert" USING
            NF-WK_PNAME1 NF-WK_LNAME NF-R2 RETURNING RET.
           IF  ERR-STAT NOT =   "00"
               MOVE    "NF-WK"    TO   ERR-F
               MOVE     NF-R2     TO   ERR-K
               MOVE    "W"        TO   ERR-M
               PERFORM  ERR-RTN   THRU     ERR-EX
               MOVE     1         TO   ERR-SW
               GO       TO        TEK-EX
           END-IF.
       TEK-EX.
           EXIT.
      ***************************************
      *    出荷指図トラン　ＲＥＡＤ         *
      ***************************************
       JSTR-RTN.
           MOVE     ZERO     TO       JSTR-KEY.
           MOVE     W-DENNO  TO       JSTR-01.
      *           START    JSTR     KEY      NOT <     JSTR-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO       TO        JSTR-999
           END-IF.
       JSTR-010.
      *           READ     JSTR     NEXT     AT    END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO       TO        JSTR-999
           END-IF
           IF  W-DENNO  NOT =    JSTR-01
               GO       TO         JSTR-999
           END-IF
           IF  JSTR-14B NOT =    OLD-01
               GO       TO         JSTR-010
           END-IF
           IF  JSTR-14C NOT =    OLD-021
               GO       TO         JSTR-010
           END-IF
           GO       TO       JSTR-EX.
       JSTR-999.
           CALL "SD_Output" USING "ERR-DAT" ERR-DAT "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           MOVE     1        TO       ERR-SW.
       JSTR-EX.
           EXIT.
      ***************************************
      *    仮クローズ　処理　　　　         *
      ***************************************
       CBL-RTN.
       CBL-EX.
           EXIT.
      ***************************************
      *    出荷指図トラン　更新　　         *
      ***************************************
       UPD2-RTN.
           MOVE     SPACE    TO       JCON7-KEY.
           MOVE     7        TO       JCON7-01.
      *           READ     JCON     UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    "JCON"    TO    ERR-F
               MOVE    "A"       TO    ERR-M
               MOVE     JCON1-KEY  TO    ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1         TO   ERR-SW
               GO       TO        UPD2-EX
           END-IF.
       UPD2-010.
           MOVE     SPACE    TO       JSTR-KEY.
      *           START    JSTR  KEY   NOT <  JSTR-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO       TO         UPD2-EX
           END-IF.
       UPD2-020.
      *           READ     JSTR     NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO       TO         UPD2-EX
           END-IF
           IF  JSTR-03  NOT =  0 AND 7
               GO  TO  UPD2-020
           END-IF
           MOVE     1        TO       I.
       UPD2-030.
           IF  I        >        10
               GO  TO  UPD2-040
           END-IF
           IF  JSTR-111(I)  <  0
               GO  TO  UPD2-020
           END-IF
           ADD      1        TO       I.
           GO  TO  UPD2-030.
       UPD2-040.
           IF  JS-SIGN  =  0
               IF  JCON7-05  NOT =  JSTR-04S
                   GO  TO  UPD2-020
               END-IF
           END-IF
           IF  JS-SIGN  =  1
               IF  JCON7-07  NOT =  JSTR-04S
                   GO  TO  UPD2-020
               END-IF
           END-IF
           IF  JSTR-05   NOT =  ZERO
               GO  TO  UPD2-020
           END-IF
           IF  JSTR-16   NOT =  JS-SIGN
               GO  TO  UPD2-020
           END-IF
           IF  JSTR-14       =  9
               GO  TO  UPD2-020
           END-IF
           IF  JSTR-4012     =  0
               GO  TO  UPD2-020
           END-IF.
       UPD2-050.
           MOVE  JSTR-14B       TO  OKJF-01.
      *           READ  OKJF  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OKJF_PNAME1 BY REFERENCE OKJF-R " " RETURNING RET.
           IF  RET = 1
               MOVE    "OKJF"    TO    ERR-F
               MOVE    "A"       TO    ERR-M
               MOVE     OKJF-KEY TO    ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1         TO   ERR-SW
               GO       TO        UPD2-EX
           END-IF.
       UPD2-060.
           MOVE     OKJF-07      TO   JSTR-15A.
      *           REWRITE  JSTR-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE    "JSTR"    TO    ERR-F
               MOVE    "R"       TO    ERR-M
               MOVE     JSTR-KEY TO    ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               MOVE     1         TO   ERR-SW
               GO       TO        UPD2-EX
           END-IF
           GO  TO  UPD2-020.
       UPD2-EX.
           EXIT.
      ***
      *****************************
      *    ｴﾗｰ DISPLAY (ﾒｲﾝ)      *
      *****************************
       ERR-RTN.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
       ERR-EX.
           EXIT.
