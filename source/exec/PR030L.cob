       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       PR030L.
      *****************************************************
      *    PROGRAM ........ 貸借マスタリスト　　　　      *
      *    AUTHOR  ........ ﾜﾀﾅﾍﾞ ｹﾝｼﾞ                    *
      *    SCREEN  USED ...                               *
      *    DATA WRITTEN ... 90/11/15                      *
      *****************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       INPUT-OUTPUT      SECTION.
       DATA              DIVISION.
       WORKING-STORAGE     SECTION.
      *
       01  GAM-AREA.
           03  LFROM       PIC  9(03).
           03  LTO         PIC  9(03).
           03  OKC         PIC  X(01).
       01  WORK-AREA.
           03  LCNT        PIC  9(02)  VALUE  90.                       ﾗｲﾝｶｳﾝﾄ
           03  PCNT        PIC  9(05)  VALUE  ZERO.                     ﾍﾟｰｼﾞ
           03  FST-SW      PIC  9(01)  VALUE  ZERO.
       01  HIZUKE          PIC  9(06).
       01  HIZUKER         REDEFINES   HIZUKE.
           03  H-YY        PIC  9(02).
           03  H-MM        PIC  9(02).
           03  H-DD        PIC  9(02).
       01  ERR-STAT        PIC  X(02).
      *
       01  MID-1.
           03  F           PIC  X(05)  VALUE  X"1A24212474".
           03  F           PIC  X(39)  VALUE  SPACE.
           03  F           PIC  N(15)  VALUE
                           "貸　借　マ　ス　タ　リ　ス　ト".
           03  F           PIC  X(40)  VALUE  SPACE.
           03  M1-01       PIC  Z9.                                     ﾈﾝ
           03  F           PIC  N(01)  VALUE  "年".
           03  M1-02       PIC  Z9.                                     ﾂｷ
           03  F           PIC  N(01)  VALUE  "月".
           03  M1-03       PIC  Z9.                                     ﾋ
           03  F           PIC  N(03)  VALUE  "日作成".
           03  F           PIC  X(04)  VALUE  SPACE.
           03  M1-04       PIC  ZZZZ9.                                  ﾍﾟｰｼﾞ
           03  F           PIC  N(01)  VALUE  "頁".
       01  MID-2.
           03  F           PIC  X(18)  VALUE  SPACE.
           03  F           PIC  X(17)  VALUE  "<--------------- ".
           03  F           PIC  N(03)  VALUE  "借　方".
           03  F           PIC  X(17)  VALUE  " --------------->".
           03  F           PIC  X(04)  VALUE  SPACE.
           03  F           PIC  X(17)  VALUE  "<--------------- ".
           03  F           PIC  N(03)  VALUE  "貸　方".
           03  F           PIC  X(17)  VALUE  " --------------->".
       01  MID-3.
           03  F           PIC  N(04)  VALUE  "ライン№".
           03  F           PIC  X(02)  VALUE  SPACE.
           03  F           PIC  N(03)  VALUE  "改行数".
           03  F           PIC  X(02)  VALUE  SPACE.
           03  F           PIC  N(04)  VALUE  "合計区分".
           03  F           PIC  X(02)  VALUE  SPACE.
           03  F           PIC  N(04)  VALUE  "印字区分".
           03  F           PIC  X(04)  VALUE  SPACE.
           03  F           PIC  N(08)  VALUE  "科目名（項目名）".
           03  F           PIC  X(06)  VALUE  SPACE.
           03  F           PIC  N(04)  VALUE  "合計区分".
           03  F           PIC  X(02)  VALUE  SPACE.
           03  F           PIC  N(04)  VALUE  "印字区分".
           03  F           PIC  X(04)  VALUE  SPACE.
           03  F           PIC  N(08)  VALUE  "科目名（項目名）".
           03  F           PIC  X(04)  VALUE  SPACE.
           03  F           PIC  N(07)  VALUE  "当期末処分利益".
      *
           COPY   LWMSG_PR.
      *
           COPY   BS-LIB.
      *
       01  P-F.
           02  P-R             PIC  X(200).
           02  R1-R REDEFINES P-R.
               03  F           PIC  X(02).
               03  R1-01       PIC  9(03)BBBBBBB.                       ﾗｲﾝ№
               03  R1-02       PIC  9(01)BBBBBBBB.                      改行数
               03  R1-03       PIC  9(01)BBBBBBBBB.                     合計区分
               03  R1-04       PIC  9(01)BBBBBB.                        印字区分
               03  R1-05       PIC  N(10).                              科目名
               03  F           PIC  X(07).
               03  R1-06       PIC  9(01)BBBBBBBBB.                     合計区分
               03  R1-07       PIC  9(01)BBBBBB.                        印字区分
               03  R1-08       PIC  N(10)BBBB.                          科目名
               03  R1-09       PIC  9(01).                              当期末処
       77  F                   PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           03  FILLER   PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
      *    <  画面クリア項目  >
           03  FILLER  PIC X(003) VALUE  "   ".
           03  FILLER  PIC X(003) VALUE  "   ".
           03  FILLER  PIC X(001) VALUE  " ".
      *    <  画面入力項目  >
       01  ACP-AREA.
           03  ACP-LFROM      PIC  9(03).
           03  ACP-LTO        PIC  9(03).
           03  ACP-OKC        PIC  X(01).
      *    <  画面表示項目  >
       01  DSP-AREA.
           03  FILLER  PIC N(008) VALUE
                         "貸借マスタリスト".
           03  FILLER  PIC N(004) VALUE  "ＦＲＯＭ".
           03  FILLER  PIC N(002) VALUE  "ＴＯ".
           03  FILLER  PIC N(004) VALUE  "ライン№".
           03  FILLER  PIC N(001) VALUE  "～".
           03  FILLER  PIC N(002) VALUE  "確認".
           03  FILLER  PIC X(013) VALUE  "OK=1,NO=9 ( )".
      *
           COPY  LSMSG_PR.
      *------------------------------------------------------------------------*
       PROCEDURE          DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "6" "34" "3" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA" "X" "6" "52" "3" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA" "X" "24" "77" "1" "02CLR-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-LFROM" "9" "6" "34" "3" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-LFROM" BY REFERENCE LFROM "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-LTO" "9" "6" "52" "3" "ACP-LFROM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-LTO" BY REFERENCE LTO "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "X" "24" "77" "1" "ACP-LTO" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "55" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" "RN" "1" "33" "16" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" "N" "4" "31" "8" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "N" "4" "51" "4" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DSP-AREA" "N" "6" "11" "8" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DSP-AREA" "N" "6" "43" "2" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DSP-AREA" "N" "24" "61" "4" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DSP-AREA" "X" "24" "66" "13" "06DSP-AREA" " "
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                  RETURNING RESU.
           ACCEPT     HIZUKE     FROM     DATE.
       PRO-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-LFROM "ACP-LFROM" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT       =  "P9"
               GO   TO   OWARI
           END-IF.
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-000
           END-IF.
       PRO-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-LTO "ACP-LTO" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-000
           END-IF.
           IF  ESTAT  NOT  =  "01" AND "06"
               GO   TO   PRO-010
           END-IF.
           IF  LFROM       >  LTO
               GO   TO   PRO-010
           END-IF.
       PRO-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT       =  "09"
               GO   TO   PRO-000
           END-IF.
           IF  OKC    NOT  =  "1"  AND "9"
               GO   TO   PRO-020
           END-IF.
           IF  OKC         =  "9"
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                RETURNING RESU
               GO          TO          PRO-000
           END-IF.
           PERFORM    LST-RTN     THRU        LST-EX.
           IF  FST-SW      =  ZERO
               CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                RETURNING RESU
               GO          TO          PRO-000
               GO          TO          PRO-000
           END-IF.
       OWARI.
           CALL "DB_Close".
           STOP       RUN.
      *----------------------------------*
      *    作表処理  ［条件］ <LST-RTN>  *
      *----------------------------------*
       LST-RTN.
      *    < ﾌｧｲﾙ OPEN >
           CALL "DB_F_Open" USING
            "INPUT" BS_PNAME1 "SHARED" BY REFERENCE BS_IDLST "1"
            "BS-KEY" BY REFERENCE BS-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE       LFROM       TO        BS-KEY.
      *          START      BS  KEY   NOT <  BS-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            BS_PNAME1 "BS-KEY" " NOT < " BS-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  LST-010
           END-IF.
       LST-000.
      *****貸借マスタ　ＲＥＡＤ
      *           READ       BS        NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BS_PNAME1 BY REFERENCE BS-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  LST-010
           END-IF.
           IF  BS-KEY    >     LTO
               GO  TO  LST-010
           END-IF.
           MOVE       1           TO        FST-SW.
           PERFORM    PRN-RTN     THRU      PRN-EX.
           GO         TO          LST-000.
       LST-010.
      *    < ﾌｧｲﾙ CLOSE >
           CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      *------------------------------------*
      *    作表処理　［印字］ < PRN-RTN >  *
      *------------------------------------*
       PRN-RTN.
           IF  (LCNT  =  90)  OR  (LCNT  NOT  <  62)
               PERFORM     MID-RTN    THRU    MID-EX
           END-IF.
           MOVE       BS-KEY      TO         R1-01.
           MOVE       BS-LIN      TO         R1-02.
           MOVE       BS-GKBDR    TO         R1-03.
           MOVE       BS-PKBDR    TO         R1-04.
           MOVE       BS-NAMDR1   TO         R1-05.
           MOVE       BS-GKBCR    TO         R1-06.
           MOVE       BS-PKBCR    TO         R1-07.
           MOVE       BS-NAMCR1   TO         R1-08.
           MOVE       BS-RKB      TO         R1-09.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE       TO         P-R.
           ADD        1           TO         LCNT.
       PRN-EX.
           EXIT.
      *-----------------------------------*
      *    見出し処理  < MID-RTN >        *
      *-----------------------------------*
       MID-RTN.
           IF  LCNT        NOT  =     90
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD        1           TO         PCNT.
           MOVE       H-YY        TO         M1-01.
           MOVE       H-MM        TO         M1-02.
           MOVE       H-DD        TO         M1-03.
           MOVE       PCNT        TO         M1-04.
           MOVE       MID-1       TO         P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE       TO         P-R.
           MOVE       MID-2       TO         P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE       TO         P-R.
           MOVE       MID-3       TO         P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       SPACE       TO         P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE       6           TO         LCNT.
       MID-EX.
           EXIT.
      *
      ************ E N D     O F     P R O G R A M *****************************
