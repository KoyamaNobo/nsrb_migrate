       IDENTIFICATION                    DIVISION.
       PROGRAM-ID.                       PR035M.
      *******************************************************************
      *****                 日　進　ゴ　ム　                        *****
      *****　　　　    損益マスタメンテナンス  　　　　　　 　　    *****
      *****            COMPILE MODE .... CBL85                      *****
      *****            SCREEN  USED .... GR0350                     *****
      *****            DATA WRITTEN .... 90/11/14                   *****
      *****                  AUTHOR .... MAYUKO.M.                  *****
      *******************************************************************
       ENVIRONMENT                       DIVISION.
       CONFIGURATION                     SECTION.
       SOURCE-COMPUTER.                  SYSTEM3100.
       OBJECT-COMPUTER.                  SYSTEM3100.
       INPUT-OUTPUT                      SECTION.
       DATA                              DIVISION.
       WORKING-STORAGE                   SECTION.
       77  ERR-STAT                PIC  X(02).
       01  W-SPACE                 PIC  N(10)   VALUE
                                        "　　　　　　　　　　".         科目名
       01  WORK-AREA.
           02  ACT                 PIC  9(01).                          ｱｸｼｮﾝ
           02  GM-WORK.
               03  GW-LINENO       PIC  9(03).                          ライン№
               03  GW-LIN          PIC  9(01).                          改行数
               03  GW-GKB          PIC  9(01).                          合計区分
               03  GW-NAM          PIC  N(10).                          科目名
               03  GW-URIKB        PIC  X(01).                          売上区分
               03  GW-PKB          PIC  9(01).                          印字区分
               03  OKC             PIC  X(01).                          確認
      *****
           COPY    LWMSG_PR.
      *****
           COPY    PL-LIB.
      *****
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      *--------------------------*
      *    画　面　ク　リ　ア    *
      *--------------------------*
       01  DSP-CLR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  CLR-AREA.
           02  FILLER  PIC X(001) VALUE " ".
           02  CLR-AREA2.
               03  FILLER  PIC X(004) VALUE "    ".
               03  FILLER  PIC X(002) VALUE "  ".
               03  FILLER  PIC X(002) VALUE "  ".
               03  FILLER  PIC X(002) VALUE "  ".
               03  FILLER  PIC X(002) VALUE "  ".
               03  FILLER  PIC N(10).                                   科目名
               03  FILLER  PIC X(001) VALUE " ".
      *------------------------------*
      *    画　面　入　力　項　目    *
      *------------------------------*
       01  ACP-AREA.
           03  ACP-ACT       PIC 9(01).                                 ACT
           03  ACP-LINENO    PIC 9(03).                                 ライン№
           03  ACP-LIN       PIC 9(01).                                 改行数
           03  ACP-GKB       PIC 9(01).                                 合計区分
           03  ACP-NAM       PIC N(10).                                 科目名
           03  ACP-URIKB     PIC X(01).                                 売上区分
           03  ACP-PKB       PIC 9(01).                                 印字区分
           03  ACP-OKC       PIC X(01).                                 確認
      *****
       COPY    LSMSG_PR.
      *****
       PROCEDURE                         DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *CLR-AREA
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA" "X" "3" "67" "1" " " "CLR-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLR-AREA2" " " "0" "0" "33" "01CLR-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "01CLR-AREA2" "X" "5" "34" "4" " " "CLR-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02CLR-AREA2" "X" "6" "37" "2" "01CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLR-AREA2" "X" "7" "37" "2" "02CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLR-AREA2" "X" "7" "75" "2" "03CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLR-AREA2" "X" "8" "75" "2" "04CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06CLR-AREA2" "N" "8" "18" "20" "05CLR-AREA2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "06CLR-AREA2" BY REFERENCE W-SPACE "20" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "07CLR-AREA2" "X" "24" "77" "1" "06CLR-AREA2" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "3" "67" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT"BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-LINENO" "9" "5" "35" "3" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-LINENO"BY REFERENCE GW-LINENO "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-LIN" "9" "6" "37" "1" "ACP-LINENO" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-LIN"BY REFERENCE GW-LIN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-GKB" "9" "7" "37" "1" "ACP-LIN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-GKB"BY REFERENCE GW-GKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-NAM" "N" "8" "18" "20" "ACP-GKB" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-NAM"BY REFERENCE GW-NAM "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-URIKB" "X" "7" "75" "1" "ACP-NAM" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-URIKB"BY REFERENCE GW-URIKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-PKB" "9" "8" "75" "1" "ACP-URIKB" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-PKB"BY REFERENCE GW-PKB "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "X" "24" "77" "1" "ACP-PKB" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-OKC"BY REFERENCE OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM     INI-RTN     THRU     INI-EX.
           PERFORM     MAIN-RTN    THRU     MAIN-EX.
           PERFORM     CLSE-ENT    THRU     CLSE-EXT.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "DB_Close".
           STOP    RUN.
      *
      *-----------------------*
      *    初　期　処　理     *
      *     << INI-RTN >>     *
      *-----------------------*
       INI-RTN.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Screen_Output" USING "GR0350" RETURNING RESU.
           INITIALIZE  GM-WORK.
           CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                  RETURNING RESU.
      *
      * << FILE OPEN >>
           CALL "DB_F_Open" USING
            "I-O" PL_PNAME1 "SHARED" BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
       INI-EX.
           EXIT.
      *
      *-----------------------------------*
      *    Ｍ　Ａ　Ｉ　Ｎ　　処　　理     *
      *         << MAIN-RTN >>            *
      *-----------------------------------*
       MAIN-RTN.
      * << ｱｸｼｮﾝ >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           INITIALIZE  GM-WORK.
           CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "P9"
               GO  TO  MAIN-EX
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ACT    NOT =   1    AND   2    AND  3
               GO  TO  MAIN-RTN
           END-IF.
      *
       MAIN-010.
      * << ライン№ >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-LINENO "ACP-LINENO" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  MAIN-RTN
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  MAIN-010
           END-IF.
      *
           MOVE      GW-LINENO     TO   PL-KEY.                         ｺｰﾄﾞ
      ***  損益マスタ　ＲＥＡＤ
      *           READ      PL       INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" PL_PNAME1 BY REFERENCE PL-REC " " RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-020
           END-IF.
      * << NORMAL >>
           IF  ACT  =  1
               CALL "SD_Output" USING "NOR-M01" NOR-M01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           END-IF.
           MOVE    PL-KEY    TO  GW-LINENO.
           MOVE    PL-LIN    TO  GW-LIN.
           MOVE    PL-GKB    TO  GW-GKB.
           MOVE    PL-NAMN   TO  GW-NAM.
           MOVE    PL-URIKB  TO  GW-URIKB.
           MOVE    PL-PKB    TO  GW-PKB.
           CALL "SD_Output" USING "ACP-LINENO" ACP-LINENO "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-LIN" ACP-LIN "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-GKB" ACP-GKB "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-NAM" ACP-NAM "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-URIKB" ACP-URIKB "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "ACP-PKB" ACP-PKB "p"
                                  RETURNING RESU.
      **
           IF  ACT  =  2
               GO  TO  MAIN-030
           END-IF.
           IF  ACT  =  3
               GO  TO  MAIN-OKC
           END-IF.
       MAIN-020.
      * << INVALID >>
           IF  ACT  =  2  OR  3
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                  RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                  RETURNING RESU
               GO  TO  MAIN-010
           END-IF.
       MAIN-030.
      * << 改行数 >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-LIN "ACP-LIN" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  MAIN-010
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  MAIN-030
           END-IF.
       MAIN-040.
      * << 合計区分 >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-GKB "ACP-GKB" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  MAIN-030
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  MAIN-040
           END-IF.
       MAIN-050.
      * << 科目名 >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-NAM "ACP-NAM" "N" "20"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  MAIN-040
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  MAIN-050
           END-IF.
       MAIN-060.
      * << 売上区分 >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-URIKB "ACP-URIKB" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
               GO  TO  MAIN-050
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  MAIN-060
           END-IF.
       MAIN-070.
      * << 印字区分 >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-PKB "ACP-PKB" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
                GO  TO  MAIN-060
           END-IF.
           IF  ESTAT  NOT =  "01"  AND  "06"
                GO  TO  MAIN-070
           END-IF.
       MAIN-OKC.
      * << 確認 >>
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                  RETURNING RESU.
           IF  ESTAT      =  "09"
               IF  ACT    =  3
                   GO  TO  MAIN-010
               ELSE
                   GO  TO  MAIN-070
               END-IF
           END-IF.
           IF  OKC        =  9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                              RETURNING RESU
               CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                              RETURNING RESU
               INITIALIZE  GM-WORK
               GO  TO  MAIN-010
           END-IF.
           IF  OKC    NOT =  1
               GO  TO  MAIN-OKC
           END-IF.
      *
           PERFORM    UPD-RTN   THRU   UPD-EX.
      *
           CALL "SD_Output" USING "OK-01" OK-01 "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "CLR-AREA2" CLR-AREA2 "p"
                                  RETURNING RESU.
           INITIALIZE  GM-WORK.
           GO         TO        MAIN-010.
       MAIN-EX.
           EXIT.
      *
      *----------------------------*
      *    ファイル　ＣＬＯＳＥ    *
      *       << CLSE-ENT >>       *
      *----------------------------*
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
      *
       CLSE-EXT.
           EXIT.
      *
      *----------------------------*
      *    更　　新　　処　　理    *
      *       << UPD-RTN >>        *
      *----------------------------*
       UPD-RTN.
           IF  ACT  =  1
               PERFORM     WRI-RTN     THRU     WRI-EX
           END-IF.
           IF  ACT  =  2
               PERFORM     REW-RTN     THRU     REW-EX
           END-IF.
           IF  ACT  =  3
               PERFORM     DEL-RTN     THRU     DEL-EX
           END-IF.
       UPD-EX.
           EXIT.
      *
      *----------------------------*
      *    ＷＲＩＴＥ　　処　理    *
      *       << WRI-RTN >>        *
      *----------------------------*
       WRI-RTN.
           MOVE    SPACE        TO  PL-REC.
           INITIALIZE               PL-REC.
           MOVE    GW-LINENO    TO  PL-KEY.
           MOVE    GW-LIN       TO  PL-LIN.
           MOVE    GW-GKB       TO  PL-GKB.
           MOVE    GW-NAM       TO  PL-NAMN.
           MOVE    GW-URIKB     TO  PL-URIKB.
           MOVE    GW-PKB       TO  PL-PKB.
           MOVE    PL-KEY       TO  ERR-K.
      *           WRITE   PL-REC       INVALID
      *///////////////
           CALL "DB_Insert" USING
            PL_PNAME1 PL_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               MOVE    "PL"      TO  ERR-F
               MOVE    "W"       TO  ERR-M
               PERFORM  ERR-ENT  THRU  ERR-EXT
           END-IF.
       WRI-EX.
           EXIT.
      *
      *------------------------------*
      *    ＲＥＷＲＩＴＥ　処　理    *
      *        << REW-RTN >>         *
      *------------------------------*
       REW-RTN.
           MOVE    GW-LIN       TO  PL-LIN.
           MOVE    GW-GKB       TO  PL-GKB.
           MOVE    GW-NAM       TO  PL-NAMN.
           MOVE    GW-URIKB     TO  PL-URIKB.
           MOVE    GW-PKB       TO  PL-PKB.
           MOVE    PL-KEY       TO  ERR-K.
      *           REWRITE  PL-REC      INVALID
      *///////////////
           CALL "DB_Update" USING
            PL_PNAME1 PL_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               MOVE    "PL"      TO  ERR-F
               MOVE    "R"       TO  ERR-M
               PERFORM  ERR-ENT  THRU  ERR-EXT
           END-IF.
      *
       REW-EX.
           EXIT.
      *
      *------------------------------*
      *    ＤＥＬＥＴＥ　　処　理    *
      *        << DEL-RTN >>         *
      *------------------------------*
       DEL-RTN.
           MOVE  PL-KEY        TO  ERR-K.
      *           DELETE  PL          INVALID
      *///////////////
           CALL "DB_Delete" USING PL_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "PL"      TO  ERR-F
               MOVE    "D"       TO  ERR-M
               PERFORM  ERR-ENT  THRU  ERR-EXT
           END-IF.
       DEL-EX.
           EXIT.
      *****
           COPY    LPMSG_PR.
      *****------------<<  PROGRAM  END  >>------------------------*****
