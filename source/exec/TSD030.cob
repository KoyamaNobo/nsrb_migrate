       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     TSD030.
       AUTHOR.         A.KOMATSUBARA.
      ***************************************
      *    手形決済情報接続  [仕訳処理]     *
      *          (手形 --> 財務)            *
      ***************************************
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       INPUT-OUTPUT    SECTION.
       DATA    DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT              PIC  X(02).
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-AREA.
           02  END-SW            PIC  9(01)    VALUE 0.
           02  W-CNT             PIC  9(01).
           02  WTK-KEY           PIC  9(05).
           02  NEW-KEY.
               03  NEW-KEY1      PIC  9(01).
               03  NEW-KEY2      PIC  9(08).
               03  NEW-KEY3      PIC  9(04).
               03  NEW-KEY4      PIC  9(04).
           02  OLD-KEY.
               03  OLD-KEY1      PIC  9(01).
               03  OLD-KEY2      PIC  9(08).
               03  OLD-KEY3      PIC  9(04).
               03  OLD-KEY4      PIC  9(04).
           02  WDNO2-05.
               03  WDNO2-051     OCCURS  3.
                   04  WDNO2-0511.
                       05  WDNO2-05111    PIC  9(04).
                       05  WDNO2-05112    PIC  9(04).
                   04  WDNO2-0512.
                       05  WDNO2-05121    PIC  9(04).
                       05  WDNO2-05122    PIC  9(04).
           02  WDNO2-06.
               03  WDNO2-061     OCCURS  3.
                   04  WDNO2-0611         PIC  9(03).
                   04  WDNO2-0612         PIC  9(03).
           02  WDNO1-023         PIC  9(06).
      *
       COPY    LWMSG_PR.
       COPY    LNSW01.
       01  SDH_TSD010.
           02  SDH_PNAME1           PIC  X(009)  VALUE "SIWAKE-H1".
           02  F                    PIC  X(001).
           02  SDH_LNAME            PIC  X(003)  VALUE "SDH".
           02  F                    PIC  X(001).
           02  SDH_KEY1             PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2             PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3             PIC  X(100)  VALUE SPACE.
           02  SDH_KEY4             PIC  X(100)  VALUE SPACE.
           02  SDH_SORT             PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST            PIC  X(100)  VALUE SPACE.
           02  SDH_RES              USAGE  POINTER.
       COPY    SIWAKH.
       77  F                        PIC  X(001).
       COPY    SIWAID.
       COPY    LNSDNO.
       COPY    ACCUNT.
       COPY    TKLIB.
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-AREA.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　割手・支手決済　仕訳処理　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　（手形－－－＞財務）　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
      **
       COPY    LSMSG_PR.
      **
       PROCEDURE   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING
            "DISP-AREA" " " "0" "0" "308" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-AREA" "N" "3" "10" "44" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DISP-AREA" "N" "4" "10" "44" "01DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DISP-AREA" "N" "5" "10" "44" "02DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DISP-AREA" "N" "6" "10" "44" "03DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DISP-AREA" "N" "7" "10" "44" "04DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DISP-AREA" "N" "8" "10" "44" "05DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DISP-AREA" "N" "9" "10" "44" "06DISP-AREA" " "
            RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *************************
      *    ＭＡＩＮ処理       *
      *************************
       HAJIME.
           PERFORM     INI-RTN   THRU   INI-EX.
           PERFORM     UPD-RTN   THRU   UPD-EX
                       UNTIL     END-SW  =  1.
           PERFORM     CLSE-ENT  THRU   CLSE-EXT.
       OWARI.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *************************
      *    初期処理           *
      *************************
       INI-RTN.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-AREA" DISP-AREA "p"
                                         RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO NS-W01_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NS-W01_PNAME1 "EXCLUSIVE" BY REFERENCE
            NS-W01_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
           CALL "DB_F_Open" USING
            "I-O" SDI_PNAME1 "EXCLUSIVE" BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
           CALL "DB_F_Open" USING
            "I-O" NS-DNO_PNAME1 "EXCLUSIVE" BY REFERENCE NS-DNO_IDLST
            "1" "DNO1-KEY" BY REFERENCE DNO1-KEY.
           MOVE  "20"            TO   DNO1-KEY  ERR-K.
      *           READ  NS-DNO          INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NS-DNO_PNAME1 BY REFERENCE DNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "NS-DNO"  TO   ERR-F
               MOVE  "G"       TO   ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                       "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE  DNO2-05         TO   WDNO2-05.
           MOVE  DNO2-06         TO   WDNO2-06.
           MOVE  "13"            TO   DNO1-KEY  ERR-K.
      *           READ  NS-DNO          INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NS-DNO_PNAME1 BY REFERENCE DNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "NS-DNO"  TO   ERR-F
               MOVE  "G"       TO   ERR-M
               CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE
                                       "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           INITIALIZE  OLD-KEY.
           MOVE 1     TO W-CNT.
       INI-EX.
           EXIT.
      *************************k
      *    更新処理           *
      *************************
       UPD-RTN.
      *           READ  NS-W01          AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" NS-W01_PNAME1 BY REFERENCE W01-R " " RETURNING RET.
           IF  RET = 1
               MOVE  1         TO   END-SW
               GO  TO  UPD-EX
           END-IF.
           MOVE  W01-01          TO   NEW-KEY1.
           MOVE  W01-03          TO   NEW-KEY3.
           IF  NEW-KEY3   =   2199
               MOVE  2200            TO   NEW-KEY3
           END-IF.
           IF  W01-01  =  3
               MOVE  W01-04          TO   NEW-KEY4
           END-IF.
           IF  W01-01  =  1      IF  W01-03  =  2200
               MOVE   2201           TO   NEW-KEY4
           END-IF.
           IF  W01-01  =  1      IF  W01-03  =  3200
               MOVE   3201           TO   NEW-KEY4
           END-IF.
           IF  W01-01  =  2
               MOVE  W01-09      TO   NEW-KEY2
           ELSE
               MOVE  W01-02      TO   NEW-KEY2
           END-IF.
           IF  NEW-KEY  NOT =  OLD-KEY
               MOVE  NEW-KEY     TO   OLD-KEY
               MOVE  1           TO   W-CNT
           END-IF.
           IF  W-CNT  =  1
               PERFORM  DNO-RTN  THRU DNO-EX
           END-IF.
           PERFORM  SDI-RTN      THRU SDI-EX.
           IF  W-CNT  NOT =  5
               ADD  1            TO   W-CNT
           ELSE
               MOVE 1            TO   W-CNT
           END-IF.
       UPD-EX.
           EXIT.
      *************************
      *    終了処理           *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-W01_IDLST NS-W01_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-DNO_IDLST NS-DNO_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
       CLSE-EXT.
           EXIT.
      *************************
      *    伝票№Ｆ　更新     *
      *************************
       DNO-RTN.
           MOVE  "13"            TO   DNO1-KEY  ERR-K.
      *           READ  NS-DNO          INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NS-DNO_PNAME1 BY REFERENCE DNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "NS-DNO"  TO   ERR-F
               MOVE  "G"       TO   ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       DNO-010.
           IF  DNO1-023  =  DNO1-022
               MOVE  DNO1-021    TO   DNO1-023
           ELSE
               ADD   1           TO   DNO1-023
           END-IF.
           MOVE  NEW-KEY2        TO   SDIYMD.
           MOVE  DNO1-023        TO   SDIJNO.
           MOVE  ZERO            TO   SDILNO.
      *           START SDI  KEY IS   NOT <  SDI-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            SDI_PNAME1 "SDI-KEY" " NOT < " SDI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  DNO-020
           END-IF
      *           READ  SDI  NEXT UNLOCK AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDI_PNAME1 BY REFERENCE SDI-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  DNO-020
           END-IF.
           IF  (NEW-KEY2  =  SDIYMD) AND (DNO1-023  =  SDIJNO)
               GO  TO  DNO-010
           END-IF.
       DNO-020.
           MOVE  NEW-KEY2        TO   HTRDATE.
           MOVE  DNO1-023        TO   HJUNLNO.
           MOVE  ZERO            TO   HLINENO  HDR-CR.
      *           START SDH  KEY IS     NOT <  SH-KEY1  INVALID
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT < " SH-KEY1 RETURNING RET.
           IF  RET = 1
               GO  TO  DNO-030
           END-IF
      *           READ  SDH  NEXT   UNLOCK   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  DNO-030
           END-IF.
           IF  (NEW-KEY2  =  HTRDATE) AND (DNO1-023  =  HJUNLNO)
               GO  TO  DNO-010
           END-IF.
       DNO-030.
           MOVE  DNO1-023        TO   WDNO1-023.
      *           REWRITE DNO1-R        INVALID
      *///////////////
           CALL "DB_Update" USING
            NS-DNO_PNAME1 NS-DNO_LNAME DNO1-R RETURNING RET.
           IF  RET = 1
               MOVE  "NS-DNO"  TO   ERR-F
               MOVE  "R"       TO   ERR-M
               PERFORM ERR-ENT      THRU  ERR-EXT
           END-IF.
       DNO-EX.
           EXIT.
      **************************
      *    仕訳インプット更新  *
      **************************
       SDI-RTN.
           MOVE  SPACE           TO   SDI-REC.
           INITIALIZE            SDI-REC.
           MOVE  NEW-KEY2        TO   SDIYMD.
           MOVE  WDNO1-023       TO   SDIJNO.
           MOVE  W-CNT           TO   SDILNO.
           MOVE  WDNO2-05111(NEW-KEY1)
                                 TO   KRCDM.
           IF  NEW-KEY1 NOT = 2
               MOVE  WDNO2-05112(NEW-KEY1)
                                 TO   KRCDS
           ELSE
               MOVE  W01-03      TO   KRCDS
           END-IF.
           IF  KRCDS         =    2199
               MOVE  2200        TO   KRCDS
           END-IF.
           MOVE  WDNO2-0611(NEW-KEY1)
                                 TO   KRSKN.
           MOVE  W01-08          TO   KRKIN.
           MOVE  KRCDM           TO   AM-KEY.
      *           READ  AM    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO      TO   TEG-BAN
           END-IF.
           MOVE  TEG-BAN         TO   KR-TB.
           MOVE  WDNO2-05121(NEW-KEY1)
                                 TO   KSCDM.
           IF  NEW-KEY1  =  1
               MOVE  W01-03      TO   KSCDS
           ELSE
               MOVE  WDNO2-05122(NEW-KEY1)
                                 TO   KSCDS
           END-IF.
           MOVE  WDNO2-0612(NEW-KEY1)
                                 TO   KSSKN.
           MOVE  W01-08          TO   KSKIN.
           MOVE  KSCDM           TO   AM-KEY.
      *           READ  AM    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO      TO   TEG-BAN
           END-IF.
           MOVE  TEG-BAN         TO   KS-TB.
           IF  NEW-KEY1  =  3
               GO    TO     SDI-010
           END-IF.
           ADD  30000  TO  NEW-KEY4  GIVING  SDICUST.
           COMPUTE   WTK-KEY     =    W01-04    +    20000.
           MOVE  WTK-KEY         TO   TK-KEY.
      *           READ  TK    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO   TK-NAMEN
           END-IF.
           MOVE  TK-NAMEN        TO   SDITEKI.
           GO    TO         SDI-020.
       SDI-010.
           ADD  10000  TO  NEW-KEY4  GIVING  SDICUST.
           IF  W01-03    =  2199  OR  2200
               MOVE      "割手決済　商工中金"     TO   SDITEKI
           END-IF.
           IF  W01-03    =  2422
               MOVE      "割手決済　住友信託"     TO   SDITEKI
           END-IF.
           IF  W01-03    =  3200
               MOVE      "割手決済　中国銀行"     TO   SDITEKI
           END-IF.
       SDI-020.
           MOVE  SDICUST         TO   TK-KEY.
      *           READ  TK    UNLOCK    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TK_PNAME1 BY REFERENCE TK-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO   TK-NAMEN
           END-IF.
           MOVE  TK-NAMEN        TO   SDINAMEN.
           MOVE  SDI-KEY         TO   ERR-K.
      *           WRITE SDI-REC         INVALID
      *///////////////
           CALL "DB_Insert" USING
            SDI_PNAME1 SDI_LNAME SDI-REC RETURNING RET.
           IF  RET = 1
               MOVE "SDI"      TO   ERR-F
               MOVE  "W"       TO   ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       SDI-EX.
           EXIT.
      *************************
      *    仕訳接続ワーク更新 *
      *************************
           COPY    LPMSG_PR.
