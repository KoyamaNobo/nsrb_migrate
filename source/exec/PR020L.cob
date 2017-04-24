       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR020L.
       AUTHOR.         A.KOMATSUBARA.
      *****************************************
      *    科目マスタ（２）　リスト           *
      *    02/11/16                           *
      *****************************************
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.    SYSTEM100.
       OBJECT-COMPUTER.    SYSTEM100.
       INPUT-OUTPUT    SECTION.
       DATA    DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(02).
       01  WORK-AREA.
           02  END-SW       PIC  9(01).
           02  PCNT         PIC  9(04).
           02  LCNT         PIC  9(02).
           02  I            PIC  9(02).
           02  DATA-KBN     PIC  9(01).
           02  HIZUKE.                                                  ﾋｽﾞｹ
               03  W-YY     PIC 9(02).                                   ﾈﾝ
               03  W-MM     PIC 9(02).                                   ﾂｷ
               03  W-DD     PIC 9(02).                                   ﾋ
       01  GAMEN-AREA.
           02  W-FROM       PIC 9(04).                                  FROM
           02  W-TO         PIC 9(04).                                  TO
           02  W-OKC        PIC X(01).                                  確認
      *
       01  MID1.
           02  F       PIC  X(05)  VALUE  X"1A24212474".
           02  F       PIC  X(39)  VALUE  SPACE.
           02  F   PIC  N(19)      VALUE
               "科　目　マ　ス　タ　（２）　リ　ス　ト".
           02  F       PIC  X(32)  VALUE  SPACE.
           02  M1-YY   PIC  Z9.
           02  F       PIC  N(01)  VALUE  "年".
           02  M1-MM   PIC  Z9.
           02  F       PIC  N(01)  VALUE  "月".
           02  M1-DD   PIC  Z9.
           02  F       PIC  N(03)  VALUE  "日作成".
           02  F       PIC  X(04)  VALUE  SPACE.
           02  M1-PCNT PIC  ZZZZ9.
           02  F       PIC  N(01)  VALUE  "頁".
       01  MID2.
           02  F       PIC  X(05)  VALUE  X"1A24212474".
           02  F       PIC  N(05)  VALUE  "科目コード".
       01  MID3.
           02  F       PIC  X(05)  VALUE  X"1A24212474".
           02  F       PIC  N(03)  VALUE  "科目名".
           02  F       PIC  X(20)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計①".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計②".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計③".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計④".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑤".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑥".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑦".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑧".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑨".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑩".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑪".
           02  F       PIC  X(03)  VALUE  SPACE.
           02  F       PIC  N(03)  VALUE  "合計⑫".
      *
       COPY    LWMSG_PR.
      *
       COPY    ACCUNT.
       COPY    KANGEL.
      *
       01  PRINTF.
           02  P-R              PIC  X(160).
           02  PRN-1            REDEFINES  P-R.
               03  P1-KCD       PIC  X(05).
               03  P1-ACCTCD    PIC  9(04).
               03  P1-ACCTCDR   REDEFINES  P1-ACCTCD
                                PIC  X(04).
               03  FILLER       PIC  X(13).
               03  P1-FIL1      PIC  N(02).
               03  P1-FIL2      PIC  X(04).
               03  FILLER       PIC  X(03).
               03  P1-TBL       OCCURS  12.
                   04  P1-DATA  PIC  9(03).
                   04  P1-DATAR REDEFINES  P1-DATA
                                PIC  X(03).
                   04  FILLER   PIC  X(06).
           02  PRN-2            REDEFINES  P-R.
               03  P2-KCD1      PIC  X(05).
               03  P2-KNGNMN    PIC  N(10).
               03  FILLER       PIC  X(02).
               03  P2-KCD2      PIC  X(05).
               03  P2-FIL       PIC  N(04).
               03  FILLER       PIC  X(03).
               03  P2-TBL       OCCURS  12.
                   04  FILLER   PIC  X(02).
                   04  P2-DATA  PIC  9(01).
                   04  P2-DATAR REDEFINES  P2-DATA
                                PIC  X(01).
                   04  FILLER   PIC  X(06).
           02  PRN-3            REDEFINES  P-R.
               03  FILLER       PIC  X(17).
               03  P3-FIL1      PIC  N(03).
               03  P3-FIL2      PIC  X(04).
               03  FILLER       PIC  X(01).
               03  P3-DATA      PIC  9(03).
           02  PRN-4            REDEFINES  P-R.
               03  FILLER       PIC  X(17).
               03  P4-FIL       PIC  N(04).
               03  FILLER       PIC  X(05).
               03  P4-DATA      PIC  9(01).
       77  F                         PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-AREA.
           02  FILLER.
               03  FILLER      PIC  X(24)  VALUE " ".
               03  FILLER      PIC  N(11)  VALUE
                   "科目マスタ（２）リスト".
           02  FILLER.
               03  FILLER      PIC  N(04)  VALUE "ＦＲＯＭ".
               03  FILLER      PIC  N(02)  VALUE "ＴＯ".
           02  FILLER.
               03  FILLER      PIC  N(05)  VALUE
                   "科目コード".
               03  FILLER      PIC  N(01)  VALUE "～".
           02  FILLER.
               03  FILLER      PIC  N(02)  VALUE "確認".
               03  FILLER      PIC  X(13)  VALUE
                   "OK=1,NO=9 ( )".
       01  ACEP-AREA.
           02  ACEP-FROM       PIC  9(04).
           02  ACEP-TO         PIC  9(04).
           02  ACEP-OKC        PIC  X(01).
       01  CLE-AREA.
           02  FILLER.
               03  FILLER      PIC  X(04) VALUE "    ".
               03  FILLER      PIC  X(04) VALUE "    ".
           02  FILLER.
               03  FILLER      PIC  X(01)  VALUE " ".
      *
       COPY    LSMSG_PR.
      *
       PROCEDURE   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *DISP-AREA
       CALL "SD_Init" USING
            "DISP-AREA" " " "0" "0" "87" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-AREA" " " "1" "0" "46" " " "DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DISP-AREA" "RX" "1" "29" "24" " " "01DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DISP-AREA" "N" "1" "30" "22" "02DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04DISP-AREA" " " "4" "0" "12" "01DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05DISP-AREA" "N" "4" "31" "8" " " "04DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "06DISP-AREA" "N" "4" "51" "4" "05DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07DISP-AREA" " " "6" "0" "12" "04DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08DISP-AREA" "N" "6" "11" "10" " " "07DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "09DISP-AREA" "N" "6" "43" "2" "08DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10DISP-AREA" " " "24" "0" "17" "07DISP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11DISP-AREA" "N" "24" "61" "4" " " "10DISP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "12DISP-AREA" "X" "24" "66" "13" "11DISP-AREA" " "
            RETURNING RESU.
      *ACEP-AREA
       CALL "SD_Init" USING
            "ACEP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACEP-FROM" "9" "6" "33" "4" " " "ACEP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-FROM" BY REFERENCE W-FROM "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACEP-TO" "9" "6" "51" "4" "ACEP-FROM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACEP-TO" BY REFERENCE W-TO "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACEP-OKC" "9" "24" "77" "1" "ACEP-TO" " "
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACEP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING
            "CLE-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01CLE-AREA" " " "6" "0" "8" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "02CLE-AREA" "X" "6" "33" "4" " " "01CLE-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03CLE-AREA" "X" "6" "51" "4" "02CLE-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04CLE-AREA" " " "24" "0" "1" "01CLE-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05CLE-AREA" "X" "24" "77" "1" " " "04CLE-AREA"
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
           PERFORM     ACP-RTN   THRU  ACP-EX.
           IF  ESTAT  =  "P9"
               GO  TO  MR-EX
           END-IF.
           PERFORM     INI-RTN   THRU  INI-EX.
           PERFORM     PRN-RTN   THRU  PRN-EX
                       UNTIL     END-SW  =  1.
           PERFORM     CLSE-ENT  THRU  CLSE-EXT.
       MR-EX.
           CALL "DB_Close".
           STOP RUN.
      *************************
      *    画面処理           *
      *************************
       ACP-RTN.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-AREA" DISP-AREA "p"
                                         RETURNING RESU.
           MOVE    0       TO     END-SW.
       ACP-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-FROM "ACEP-FROM" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  ACP-EX
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-010
           END-IF.
       ACP-020.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-TO "ACEP-TO" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-010
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-020
           END-IF.
           IF  W-FROM  >  W-TO
               GO  TO  ACP-020
           END-IF.
       ACP-030.
           CALL "SD_Accept" USING
                 BY REFERENCE ACEP-OKC "ACEP-OKC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACP-010
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ACP-030
           END-IF.
           IF  W-OKC  NOT =  "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                         RETURNING RESU
               GO  TO  ACP-030
           END-IF.
           IF  W-OKC  =  "9"
               INITIALIZE GAMEN-AREA
               CALL "SD_Output" USING "CLE-AREA" CLE-AREA "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                         RETURNING RESU
               GO  TO  ACP-010
           END-IF.
           IF  W-OKC  =  "1"
               CALL "DB_F_Open" USING
                "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
                "AM-KEY" BY REFERENCE AM-KEY
               PERFORM  STRT-RTN  THRU  STRT-EX
               CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1
           END-IF
           IF  END-SW  =  1
               MOVE     0         TO    END-SW
               GO  TO  ACP-010
           END-IF.
       ACP-EX.
           EXIT.
      *************************
      *    初期処理           *
      *************************
       INI-RTN.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "PR_Open" RETURNING RESP.
           INITIALIZE      WORK-AREA.
           ACCEPT  HIZUKE  FROM  DATE.
           MOVE    W-YY    TO    M1-YY.
           MOVE    W-MM    TO    M1-MM.
           MOVE    W-DD    TO    M1-DD.
           MOVE    90      TO    LCNT.
       INI-EX.
           EXIT.
      *************************
      *    出力処理           *
      *************************
       PRN-RTN.
           IF  LCNT  =  90
               PERFORM  STRT-RTN  THRU  STRT-EX
           END-IF.
      *           READ  AM  NEXT  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1   TO END-SW
               GO  TO    PRN-EX
           END-IF.
           IF  W-TO  <  AM-KEY
               MOVE    1   TO END-SW
               GO  TO      PRN-EX
           END-IF.
           IF  LCNT  >  4
               PERFORM HEAD-RTN  THRU HEAD-EX
           END-IF.
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212474"
                           TO    P1-KCD.
           MOVE  AM-KEY    TO    P1-ACCTCD.
           MOVE  "貸借"  TO    P1-FIL1.
           MOVE  "ｺｰﾄﾞ"    TO    P1-FIL2.
           MOVE  1         TO    DATA-KBN.
           PERFORM  P1-LOOP-RTN  THRU  P1-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   6.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-1 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212078"
                           TO    P2-KCD1.
           MOVE  ZERO      TO    KNG-KEY.
           MOVE  AM-KEY    TO    K-ACCD.
      *           READ  KNG       UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  KNGNMN
           END-IF.
           MOVE  KNGNMN    TO    P2-KNGNMN.
           MOVE  X"1A24212474"
                           TO    P2-KCD2.
           MOVE  "貸借区分"
                           TO    P2-FIL.
           PERFORM  P2-LOOP-RTN  THRU  P2-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   6.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-2 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212078"
                           TO    P2-KCD1.
           MOVE  "　"    TO    P2-KNGNMN.
           MOVE  X"1A24212474"
                           TO    P2-KCD2.
           MOVE  "計算区分"
                           TO    P2-FIL.
           PERFORM  P2-LOOP-RTN  THRU  P2-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   6.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-2 RETURNING RESP.
      *****
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212474"
                           TO    P1-KCD.
           MOVE  SPACE     TO    P1-ACCTCDR.
           MOVE  "損益"  TO    P1-FIL1.
           MOVE  "ｺｰﾄﾞ"    TO    P1-FIL2.
           MOVE  2         TO    DATA-KBN.
           PERFORM  P1-LOOP-RTN  THRU  P1-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   12.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-1 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212078"
                           TO    P2-KCD1.
           MOVE  "　"    TO    P2-KNGNMN.
           MOVE  X"1A24212474"
                           TO    P2-KCD2.
           MOVE  "計算区分"
                           TO    P2-FIL.
           PERFORM  P2-LOOP-RTN  THRU  P2-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   12.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-2 RETURNING RESP.
      *****
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212474"
                           TO    P1-KCD.
           MOVE  SPACE     TO    P1-ACCTCDR.
           MOVE  "原価"  TO    P1-FIL1.
           MOVE  "ｺｰﾄﾞ"    TO    P1-FIL2.
           MOVE  3         TO    DATA-KBN.
           PERFORM  P1-LOOP-RTN  THRU  P1-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   12.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-1 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE  X"1A24212078"
                           TO    P2-KCD1.
           MOVE  "　"    TO    P2-KNGNMN.
           MOVE  X"1A24212474"
                           TO    P2-KCD2.
           MOVE  "計算区分"
                           TO    P2-FIL.
           PERFORM  P2-LOOP-RTN  THRU  P2-LOOP-EX
               VARYING  I  FROM  1  BY  1
               UNTIL    I    >   12.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-2 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE "資金繰" TO    P3-FIL1.
           MOVE  "ｺｰﾄﾞ"    TO    P3-FIL2.
           IF  SKNKEY NOT = ZERO
               MOVE  SKNKEY    TO    P3-DATA
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-3 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE "計算区分"
                           TO    P4-FIL.
           IF  SKNKEY NOT = ZERO
               MOVE  SKNCOM    TO    P4-DATA
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-4 RETURNING RESP.
           MOVE  SPACE     TO    P-R.
           MOVE "発生区分"
                           TO    P4-FIL.
           IF  SKNKEY NOT = ZERO
               MOVE  SKNHAT    TO    P4-DATA
           END-IF.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-4 RETURNING RESP.
           ADD     1       TO    LCNT.
       PRN-EX.
           EXIT.
      *************************
      *    終了　処理         *
      *************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
      *************************
      *    位置付け処理       *
      *************************
       STRT-RTN.
           MOVE    W-FROM  TO     AM-KEY.
      *           START   AM      KEY IS NOT <   AM-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            AM_PNAME1 "AM-KEY" " NOT < " AM-KEY RETURNING RET.
           IF  RET = 1
               INITIALIZE  GAMEN-AREA
               CALL "SD_Output" USING "CLE-AREA" CLE-AREA "p"
                                         RETURNING RESU
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                         RETURNING RESU
               MOVE    1        TO   END-SW
           END-IF.
       STRT-EX.
           EXIT.
      *************************
      *    見出し処理         *
      *************************
       HEAD-RTN.
           IF  LCNT  NOT =  90
               MOVE  SPACE  TO     P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD     1       TO     PCNT.
           MOVE    PCNT    TO     M1-PCNT.
           MOVE    MID1    TO     P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE   TO     P-R.
           MOVE    MID2    TO     P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE   TO     P-R.
           MOVE    MID3    TO     P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE   TO     P-R.
           MOVE    0       TO     LCNT.
       HEAD-EX.
           EXIT.
      *************************
      *    Ｐ１　データセット *
      *************************
       P1-LOOP-RTN.
           IF  DATA-KBN  =  1
               IF  BSKEY (I)  =  ZERO
                   MOVE  SPACE     TO     P1-DATAR (I)
               ELSE
                   MOVE  BSKEY (I) TO     P1-DATA (I)
               END-IF
           END-IF.
           IF  DATA-KBN  =  2
               IF  PLKEY (I)  =  ZERO
                   MOVE  SPACE     TO     P1-DATAR (I)
               ELSE
                   MOVE  PLKEY (I) TO     P1-DATA (I)
               END-IF
           END-IF.
           IF  DATA-KBN  =  3
               IF  GNKEY (I)  =  ZERO
                   MOVE  SPACE     TO     P1-DATAR (I)
               ELSE
                   MOVE  GNKEY (I) TO     P1-DATA (I)
               END-IF
           END-IF.
       P1-LOOP-EX.
           EXIT.
      *************************
      *    Ｐ２　データセット *
      *************************
       P2-LOOP-RTN.
           IF  DATA-KBN  =  1
               IF  BSKEY (I)  =  ZERO
                   MOVE  SPACE     TO     P2-DATAR (I)
               ELSE
                   IF  P2-FIL  =  "貸借区分"
                       MOVE  BSDR-CR (I)  TO     P2-DATA (I)
                   ELSE
                       MOVE  BSCOM (I)    TO     P2-DATA (I)
                   END-IF
               END-IF
           END-IF.
           IF  DATA-KBN  =  2
               IF  PLKEY (I)  =  ZERO
                   MOVE  SPACE     TO     P2-DATAR (I)
               ELSE
                   MOVE  PLCOM (I) TO     P2-DATA (I)
               END-IF
           END-IF.
           IF  DATA-KBN  =  3
               IF  GNKEY (I)  =  ZERO
                   MOVE  SPACE     TO     P2-DATAR (I)
               ELSE
                   MOVE  GNCOM (I) TO     P2-DATA (I)
               END-IF
           END-IF.
       P2-LOOP-EX.
           EXIT.
      *******
           COPY    LPMSG_PR.
