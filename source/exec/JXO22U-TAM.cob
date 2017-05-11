       IDENTIFICATION   DIVISION.
      ******************************************************************
      *    新倉庫向け送信確認                                          *
      *                          ０５／０９／０１　                    *
      *            [JXO22U]      T.ISHISHITA                           *
      *    CALL   "SJXO22".                                            *
      *    JS-SIGN  :  0=伝送 , 1=生成(NEW)                            *
      *    W-JS     :  1=藤田 , 2=津山 , 3=早島                        *
      ******************************************************************
       PROGRAM-ID.            JTO22U.
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       SYSTEM3100.
       OBJECT-COMPUTER.       SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE  SECTION.
       01  JS-SIGN                 PIC  9(01).
       01  W-JS                    PIC  9(01).
       01  ERR-STAT                PIC  X(02).
       01  WORK-ARIA.
           02  W-OKURI.
             03  F                 PIC  9(02).
             03  W-OKURISAKI       PIC  9(01).
           02  LIN-W               PIC  9(02).
           02  MM                  PIC  9(02).
           02  NN                  PIC  9(02).
           02  NA                  PIC  9(02).
           02  SW                  PIC  X(01).
           02  STR-TIME            PIC  9(08).
           02  STR-TIMER           REDEFINES  STR-TIME.
               03  STR-JF          PIC  9(04).
               03  F               PIC  X(04).
           02  END-TIME            PIC  9(08).
           02  END-TIMER           REDEFINES  END-TIME.
               03  END-JF          PIC  9(04).
               03  F               PIC  X(04).
           02  JOJF-RECW           PIC  X(512).
           02  TAMEKOMI-WORK       OCCURS  13.
               03  STR-CODE        PIC  X(10).
               03  END-CODE        PIC  X(10).
               03  KEN-W           PIC  N(01).
               03  MSG-N           PIC  X(22).
               03  JOJF-08W        PIC  9(02).
           02  W-SNM               PIC  N(06).
           02  W-ACD               PIC  9(03).
      *
       01  W-DATE.
           02  W-YY                PIC  9(02).
           02  W-MD                PIC  9(04).
           02  W-MDR               REDEFINES  W-MD.
               03  W-MM            PIC  9(02).
               03  W-DD            PIC  9(02).
      *----SCREEN WORK
       01  SCREEN-ARIA.
           02  KAKU-W              PIC  X(01).
           02  DKS-W.
               03  DKS01           PIC  9(06).
               03  DKS02           PIC  9(06).
               03  DKS03           PIC  9(06).
               03  DKS04           PIC  9(06).
               03  DKS05           PIC  9(06).
               03  DKS06           PIC  9(06).
               03  DKS07           PIC  9(06).
               03  DKS08           PIC  9(06).
               03  DKS09           PIC  9(06).
               03  DKS10           PIC  9(06).
               03  DKS11           PIC  9(06).
               03  DKS99           PIC  9(06).
           02  DKS-WR              REDEFINES  DKS-W.
               03  DKS-T           OCCURS 12  PIC  9(06).
      *
           02  SKS-W.
               03  SKS01           PIC  9(06).
               03  SKS02           PIC  9(06).
               03  SKS03           PIC  9(06).
               03  SKS04           PIC  9(06).
               03  SKS05           PIC  9(06).
               03  SKS06           PIC  9(06).
               03  SKS07           PIC  9(06).
               03  SKS08           PIC  9(06).
               03  SKS09           PIC  9(06).
               03  SKS10           PIC  9(06).
               03  SKS11           PIC  9(06).
               03  SKS99           PIC  9(06).
           02  SKS-WR              REDEFINES  SKS-W.
               03  SKS-T           OCCURS 12  PIC  9(06).
           02  MSG-W2              PIC  N(20).
      *
           02  W-JKYO              PIC  N(04).
       01  SW-AREA.
           02  END-SW                  PIC  9(01).
           02  ERR-SW                  PIC  9(01).
           02  GAMEN-M.
               03  F         PIC X(22) VALUE  "Ｃ／Ｆ　　　　        ".
               03  F         PIC X(22) VALUE  "直　送　先　　        ".
               03  F         PIC X(22) VALUE  "品　　　名　　        ".
               03  F         PIC X(22) VALUE  "ワークマン店名        ".
               03  F         PIC X(22) VALUE  "出　荷　指　図        ".
               03  F         PIC X(22) VALUE  "荷　　　札　　        ".
               03  F         PIC X(22) VALUE  "送　り　状　　        ".
               03  F         PIC X(22) VALUE  "ワークマン　　        ".
               03  F         PIC X(22) VALUE  "ナ　フ　コ　　        ".
               03  F         PIC X(22) VALUE  "トラスコ他　　        ".
               03  F         PIC X(22) VALUE  "赤ちゃん本舗　        ".
               03  F         PIC X(22) VALUE  "合　　　計 (ﾚｺｰﾄﾞ件数)".
           02  GAMEN-MR      REDEFINES  GAMEN-M.
               03  MSG-W     PIC  X(22)    OCCURS 12.
      *
           02  MSG-WORK.
               03  F         PIC N(07) VALUE  "送信データ無し".
               03  F         PIC N(07) VALUE  "送　　信　　中".
               03  F         PIC N(07) VALUE  "正常　終了　　".
               03  F         PIC N(07) VALUE  "通信不可　　　".
               03  F         PIC N(07) VALUE  "送信先エラー　".
               03  F         PIC N(07) VALUE  "区分エラー　　".
               03  F         PIC N(07) VALUE  "取　　消　　中".
           02  MSG-WORKR     REDEFINES  MSG-WORK.
               03  MSG-M     OCCURS  7  PIC  N(07).
      ***
       COPY    LWMSG.
      ***
           COPY    L-JOS2.
           COPY    L-JOSF-TAM.
           COPY    L-JOJF.
           COPY    L-JCON.
      *
      *-----------------------------------------------------------------
      *----            << SCREEN  SECTION >>                        ----
      *-----------------------------------------------------------------
      *
       77  END-STS            PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  ACP-AREA.
           02  ACP-KAKU   PIC  X(01).
           02  DSP-DKS.
               03  FILLER  PIC  X(22).
               03  FILLER  PIC  ZZZ,ZZ9 .
               03  FILLER  PIC  N(02).
           02  DSP-SNM    PIC  N(06).
           02  DSP-MSG.
               03  DSP-MSG1 PIC  N(20).
               03  FILLER  PIC  X(03)        VALUE X"1B4203".
               03  FILLER  PIC  X(40)        VALUE " ".
      ***
       COPY    LSERR.
      ***
       PROCEDURE        DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "129" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" "X" "24" "77" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-DKS" " " "0" "0" "33" "ACP-KAKU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-DKS" "X" "LIN-W" "2" "22" " " "DSP-DKS"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-DKS" BY REFERENCE MSG-N(1) "22" "1" 
            BY REFERENCE NN 46 RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-DKS" "ZZZ,ZZ9" "LIN-W" "36" "7" "01DSP-DKS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02DSP-DKS" BY REFERENCE DKS-T(1) "72" "1" BY REFERENCE NN 6
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-DKS" "N" "LIN-W" "44" "4" "02DSP-DKS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-DKS" BY REFERENCE KEN-W(1) "2" "1" BY REFERENCE NN 46
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SNM" "N" "1" "59" "12" "DSP-DKS" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SNM" BY REFERENCE W-SNM "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG" " " "23" "0" "83" "DSP-SNM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MSG1" "N" "23" "10" "40" " " "DSP-MSG" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-MSG1" BY REFERENCE MSG-M(1) "98" "1" BY REFERENCE MM 14
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MSG" "X" "23" "41" "3" "DSP-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MSG" "X" "23" "10" "40" "02DSP-MSG" " "
            RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ******************************************************************
      *    ＭＡＩＮ　ＲＯＵＴＩＮＥ　　　　　　                        *
      ******************************************************************
       MAIN.
           PERFORM  INI-RTN        THRU  INI-EX.
           IF  ERR-SW  =  1
               GO  TO  MR999
           END-IF
      **
           PERFORM  UPD-RTN        THRU  UPD-EX.
       MR999.
           PERFORM  END-RTN        THRU  END-EX.
           CALL "DB_Close".
           STOP  RUN.
      ******************************************************************
      *    ＩＮＩ－ＲＴＮ　　（初期＆画面処理）            　
      ******************************************************************
       INI-RTN.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN      >  1
               MOVE  6       TO  MM
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1       TO  ERR-SW
               GO  TO  INI-EX
           END-IF
           ACCEPT W-JS FROM ARGUMENT-VALUE.
      *
           IF  ERR-SW  =  1
               MOVE  ZERO     TO  ERR-SW
               GO  TO  INI-100
           END-IF
      *
           CALL "SD_Screen_Output" USING "SJXO22" RETURNING RESU.
           IF  JS-SIGN      =  0
               CALL "DB_F_Open" USING
                "INPUT" JOLSF2_PNAME1 " " BY REFERENCE JOLSF2_IDLST "0"
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON4-KEY" BY REFERENCE JCON4-KEY.
           CALL "DB_F_Open" USING
            "I-O" JOJF_PNAME1 "SHARED" BY REFERENCE JOJF_IDLST "1"
            "JOJF-KEY" BY REFERENCE JOJF-KEY.
      *
           MOVE  0001     TO  JOJF-01.
      *----O/Lﾌｧｲﾙ READ
      *           READ   JOJF        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1       TO  MM
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1       TO  ERR-SW
               GO  TO  INI-EX
           END-IF
           MOVE  JOJF-REC     TO  JOJF-RECW.
           MOVE  JOJF-07  TO  W-OKURI.
           IF  W-JS    NOT  =  W-OKURISAKI
               MOVE  5       TO  MM
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1       TO  ERR-SW
               GO  TO  INI-EX
           END-IF
           MOVE  4        TO  JCON4-01.
           MOVE  JOJF-07  TO  JCON4-02  W-ACD.
      *           READ  JCON     UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  1       TO  MM
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1       TO  ERR-SW
               GO  TO  INI-EX
           END-IF
           MOVE  JCON4-03 TO  W-SNM.
           CALL "SD_Output" USING "DSP-SNM" DSP-SNM "p" RETURNING RESU.
      *
           IF (JOJF-08(1) = ZERO) AND (JOJF-08(2) = ZERO)  AND
              (JOJF-08(3) = ZERO) AND (JOJF-08(4) = ZERO)  AND
              (JOJF-08(5) = ZERO) AND (JOJF-08(6) = ZERO)  AND
              (JOJF-08(7) = ZERO) AND (JOJF-08(8) = ZERO)  AND
              (JOJF-08(9) = ZERO) AND (JOJF-08(10) = ZERO) AND
              (JOJF-08(11) = ZERO) AND (JOJF-08(12) = ZERO)
               MOVE  1       TO  MM
               CALL "SD_Output" USING
                "DSP-MSG" DSP-MSG "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1       TO  ERR-SW
               GO  TO  INI-EX
           END-IF
      *
           MOVE  1        TO  NN  NA.
       INI-010.
           IF  JOJF-08(NN)  =  01
               MOVE  01           TO  JOJF-08W(NA)
               MOVE  MSG-W(01)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               ADD   1            TO  NA
               GO  TO  INI-020
           END-IF
           IF  JOJF-08(NN)  =  02
               MOVE  02           TO  JOJF-08W(NA)
               MOVE  MSG-W(02)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               ADD   1            TO  NA
               GO  TO  INI-020
           END-IF
           IF  JOJF-08(NN)  =  03
               MOVE  03           TO  JOJF-08W(NA)
               MOVE  MSG-W(03)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               ADD   1            TO  NA
               GO  TO  INI-020
           END-IF
           IF  JOJF-08(NN)  =  04
               MOVE  04           TO  JOJF-08W(NA)
               MOVE  MSG-W(04)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               ADD   1            TO  NA
               GO  TO  INI-020
           END-IF
           IF  JOJF-08(NN)  =  11
               MOVE  11           TO  JOJF-08W(NA)
               MOVE  MSG-W(05)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
               GO  TO  INI-020
           END-IF
           IF  JOJF-08(NN)  =  12
               MOVE  12           TO  JOJF-08W(NA)
               MOVE  MSG-W(06)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
               GO  TO  INI-020
           END-IF
           IF  JOJF-08(NN)  =  13
               MOVE  13           TO  JOJF-08W(NA)
               MOVE  MSG-W(07)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
           END-IF
           IF  JOJF-08(NN)  =  14
               MOVE  14           TO  JOJF-08W(NA)
               MOVE  MSG-W(08)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
           END-IF
           IF  JOJF-08(NN)  =  15
               MOVE  15           TO  JOJF-08W(NA)
               MOVE  MSG-W(09)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
           END-IF
           IF  JOJF-08(NN)  =  16
               MOVE  16           TO  JOJF-08W(NA)
               MOVE  MSG-W(10)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
           END-IF
           IF  JOJF-08(NN)  =  17
               MOVE  17           TO  JOJF-08W(NA)
               MOVE  MSG-W(11)    TO  MSG-N(NA)
               MOVE  "件"       TO  KEN-W(NA)
               MOVE  JOJF-09(NN)  TO  DKS-T(NA)
               MOVE  JOJF-11(NN)  TO  STR-CODE(NA)
               MOVE  JOJF-12(NN)  TO  END-CODE(NA)
               ADD   1            TO  NA
           END-IF.
       INI-020.
           IF  NA  =  12
               GO  TO  INI-030
           END-IF
           IF  NN  NOT  =  12
               ADD   1            TO  NN
               GO  TO  INI-010
           END-IF.
       INI-030.
           COMPUTE  DKS99 = DKS01 + DKS02 + DKS03 + DKS04 +
                 DKS05 + DKS06 + DKS07 + DKS08 + DKS09 + DKS10 + DKS11.
           MOVE  "件"       TO  KEN-W(12).
           MOVE  MSG-W(12)    TO  MSG-N(12).
           MOVE  1            TO  NN.
       INI-040.
           COMPUTE  LIN-W  =  NN  +  3.
           CALL "SD_Arg_Match_Line" USING
            "LIN-W" "2" LIN-W RETURNING RESU.
           IF  DKS-T(NN)  =  ZERO
               GO  TO  INI-050
           END-IF
           CALL "SD_Output" USING "DSP-DKS" DSP-DKS "p" RETURNING RESU.
       INI-050.
           IF  NN  NOT  = 11  ADD   1       TO  NN
               GO  TO  INI-040
           END-IF
           MOVE  12           TO  NN.
           MOVE  21           TO  LIN-W.
           CALL "SD_Arg_Match_Line" USING
            "LIN-W" "2" LIN-W RETURNING RESU.
           CALL "SD_Output" USING "DSP-DKS" DSP-DKS "p" RETURNING RESU.
      *----確認入力
       INI-100.
           CALL "SD_Accept" USING BY REFERENCE ACP-KAKU "ACP-KAKU"
            "X" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS  NOT  =  "01"
               GO  TO  INI-100
           END-IF
           IF  KAKU-W  NOT  =  1  AND  9
               GO  TO  INI-100
           END-IF
           IF  JS-SIGN =  1
               IF  KAKU-W  =  9
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  100
                   GO  TO  INI-EX
               END-IF
           END-IF
           IF  KAKU-W  =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  1     TO  ERR-SW
           END-IF.
       INI-EX.
           EXIT.
      ******************************************************************
      *    ＵＰＤ－ＲＴＮ　　（Ｏ／Ｌ状況Ｆ更新）　　　　　　　        *
      ******************************************************************
       UPD-RTN.
           IF  JS-SIGN =  1
               IF  KAKU-W  =  9
                   GO  TO  UPD-020
               END-IF
           END-IF
           MOVE  3            TO  MM.
           CALL "SD_Output" USING "DSP-MSG" DSP-MSG "p" RETURNING RESU.
      *
           MOVE  JOJF-RECW    TO  JOJF-REC.
           MOVE  JOJF-90      TO  JOJF-01.
           ACCEPT  W-DATE     FROM  DATE.
           MOVE  W-MD         TO  JOJF-02.
           ACCEPT  STR-TIME   FROM  TIME.
           MOVE  STR-JF       TO  JOJF-03.
           ACCEPT  END-TIME   FROM  TIME.
           MOVE  END-JF       TO  JOJF-04.
           MOVE  1            TO  JOJF-061.
           MOVE  SPACE        TO  JOJF-062.
           MOVE  ZERO         TO  JOJF-063.
           MOVE  1            TO  NA.
       UPD-010.
           MOVE  SKS-T(NA)    TO  JOJF-10(NA).
           MOVE  STR-CODE(NA) TO  JOJF-11(NA).
           MOVE  END-CODE(NA) TO  JOJF-12(NA).
           IF  NA  NOT  =  11
               ADD   1       TO  NA
               GO TO  UPD-010
           END-IF
           MOVE  ZERO         TO  JOJF-90.
           MOVE  W-ACD        TO  JOJF-07.
      *
      *           WRITE  JOJF-REC    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"     TO  ERR-F
               MOVE  JOJF-KEY   TO  ERR-K
               MOVE  "W"        TO  ERR-M
               MOVE  1          TO  ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               PERFORM  ERR-RTN  THRU  ERR-EX
               GO  TO  UPD-EX
           END-IF.
       UPD-020.
           MOVE  0001         TO  JOJF-01.
      *           READ  JOJF         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JOJF_PNAME1 BY REFERENCE JOJF-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"     TO  ERR-F
               MOVE  JOJF-KEY   TO  ERR-K
               MOVE  "A"        TO  ERR-M
               MOVE  1          TO  ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               PERFORM  ERR-RTN  THRU  ERR-EX
               GO  TO  UPD-EX
           END-IF
      *
           IF  KAKU-W  NOT =  9
               ADD   1            TO  JOJF-90
           END-IF
           MOVE  ZERO         TO  JOJF-02  JOJF-03  JOJF-04 JOJF-05
                                  JOJF-061 JOJF-063 JOJF-07.
           MOVE  SPACE        TO  JOJF-062.
           INITIALIZE             JOJF-TBL.
      *
      *           REWRITE  JOJF-REC  INVALID
      *///////////////
           CALL "DB_Update" USING
            JOJF_PNAME1 JOJF_LNAME JOJF-REC RETURNING RET.
           IF  RET = 1
               MOVE  "JOJF"     TO  ERR-F
               MOVE  JOJF-KEY   TO  ERR-K
               MOVE  "R"        TO  ERR-M
               MOVE  1          TO  ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           IF  KAKU-W      =  9
               CALL "DB_F_Open" USING
                "OUTPUT" JOLSF_PNAME1 " " BY REFERENCE JOLSF_IDLST "0"
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF_IDLST JOLSF_PNAME1
           END-IF.
       UPD-EX.
           EXIT.
      ******************************************************************
      *    ＥＮＤ－ＲＴＮ　　（終了処理）                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JOJF_IDLST JOJF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           IF  JS-SIGN      =  0
               CALL "DB_F_Close" USING
                BY REFERENCE JOLSF2_IDLST JOLSF2_PNAME1
           END-IF.
       END-EX.
           EXIT.
      ***
       COPY    LPERR.
      ***
