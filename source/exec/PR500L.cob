       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        PR500L.
      *>=========================================================<*
      *>                                                         <*
      *>       USER     NAME.....                                <*
      *>       PROGRAM  NAME.....                                <*
      *>       PROGRAM  TITLE....                                <*
      *>       AUTHOR   .........                                <*
      *>       DATE     WRITTEN.. 83/08/24                       <*
      *>                                                         <*
      *>=========================================================<*
      *
       ENVIRONMENT                        DIVISION.
       CONFIGURATION                      SECTION.
       SOURCE-COMPUTER.                   SYSTEM100.
       OBJECT-COMPUTER.                   SYSTEM100.
       DATA                  DIVISION.
       WORKING-STORAGE                SECTION.
       77  ERR-STAT          PIC X(02).
       01  H1.
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
           02  FILLER        PIC X(02).
           02  H1-YY         PIC N(02).
           02  FILLER        PIC N(01)   VALUE   "年".
           02  H1-MM         PIC N(02).
           02  FILLER        PIC N(01)   VALUE   "月".
           02  H1-DD         PIC N(02).
           02  FILLER        PIC N(03)   VALUE   "日作成".
           02  FILLER        PIC X(16)   VALUE   SPACE.
           02  FILLER        PIC X(02)   VALUE   X"1AC0".
           02  FILLER        PIC N(22)   VALUE
                     "　部　門　別　経　費　明　細　表　（月次）　".
           02  FILLER        PIC X(03)   VALUE   X"1AC1".
           02  FILLER        PIC N(07)   VALUE   "　＜部門管理＞".
           02  FILLER        PIC X(27)   VALUE   SPACE.
           02  H1-PAGE       PIC N(03).
           02  FILLER        PIC N(01)   VALUE   "頁".
       01  H2.
           02  FILLER        PIC X(52)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "（".
           02  H2-YY         PIC N(02).
           02  FILLER        PIC N(01)   VALUE   "年".
           02  H2-MM         PIC N(02).
           02  FILLER        PIC N(03)   VALUE   "月分）".
       01  H3.
           02  FILLER        PIC X(137)  VALUE   X"1AC0".
           02  FILLER        PIC X(02)   VALUE   X"1AC1".
       01  H4.
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(23).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(01).
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  W-BNMNM       OCCURS  7   TIMES.
               03  WK-BNMNM  PIC N(10).
               03  P1        PIC X(02).
               03  P2        PIC X(01).
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
       01  H5.
           02  FILLER        PIC X(04)   VALUE   X"1AC01AC2".
           02  FILLER        PIC X(05).
           02  FILLER        PIC N(01)   VALUE "項".
           02  FILLER        PIC X(03).
           02  FILLER        PIC N(01)   VALUE "目".
           02  FILLER        PIC X(03).
           02  FILLER        PIC N(01)   VALUE "名".
           02  FILLER        PIC X(06).
           02  W-BNM         OCCURS  7   TIMES.
               03  P3        PIC X(02).
               03  P4        PIC X(12).
               03  WK-BNM    PIC X(04).
           02  FILLER        PIC X(04)   VALUE   X"1AC11AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  H6.
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(23).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  H7.
           02  FILLER        PIC X(04)   VALUE   X"1AC01AC2".
           02  FILLER        PIC X(23).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(16).
           02  FILLER        PIC X(04)   VALUE   X"1AC11AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  W-PRNREC1.
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC X(01)   VALUE   SPACE.
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  W-KNG1        PIC X(24).
           02  W-KNG2  REDEFINES   W-KNG1.
               03  NAME1     PIC X(20).
               03  FILLER1   PIC N(02).
           02  W-KNG3  REDEFINES   W-KNG1.
               03  FILLER2   PIC N(02).
               03  NAME2     PIC X(20).
           02  W-KNG4  REDEFINES   W-KNG1.
               03  FILLER3   PIC N(08).
               03  NAME3     PIC N(04).
           02  FILLER        PIC X(04)   VALUE   SPACE.
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
           02  WK-KINGAKU    OCCURS      7    TIMES.
               03  F1        PIC X(02).
               03  W-RETSU   PIC ---,---,---,--9.
               03  F2        PIC X(01).
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  WR-PRNREC1        REDEFINES   W-PRNREC1.
           02  FILLER        PIC X(41).
           02  W-KINGAKU     OCCURS      7    TIMES.
               03  FILLER    PIC X(02).
               03  WR-P1     PIC X(15).
               03  FILLER    PIC X(01).
           02  FILLER        PIC X(04).
       01  W-PRNREC2.
           02  FILLER        PIC X(04)   VALUE  X"1AC01AC2".
           02  FILLER        PIC X(06)   VALUE  SPACE.
           02  FILLER        PIC N(01)   VALUE "合".
           02  FILLER        PIC X(07)   VALUE  SPACE.
           02  FILLER        PIC N(01)   VALUE "計".
           02  FILLER        PIC X(06)   VALUE  SPACE.
           02  WK-GOKEI      OCCURS      7    TIMES.
               03  F3        PIC X(02).
               03  W-KEI     PIC ---,---,---,--9.
               03  F4        PIC X(01).
           02  FILLER        PIC X(04)   VALUE  X"1AC11AC2".
           02  FILLER        PIC N(01)   VALUE  "　".
       01  WR-PRNREC2        REDEFINES   W-PRNREC2.
           02  FILLER        PIC X(27).
           02  W-GOKEI       OCCURS      7    TIMES.
               03  FILLER    PIC X(02).
               03  WR-P2     PIC X(15).
               03  FILLER    PIC X(01).
           02  FILLER        PIC X(06).
       01  CRNT-DATE.
           02  CRNT-YY       PIC 9(02).
           02  CRNT-MM       PIC 9(02).
           02  CRNT-DD       PIC 9(02).
       01  W-MM              PIC Z9.
       01  W-DD              PIC Z9.
       01  W-Y               PIC N(02).
       01  W-M               PIC N(02).
       01  W-D               PIC N(02).
       01  W-K-MM            PIC Z9.
       01  W-K-Y             PIC N(02).
       01  W-K-M             PIC N(02).
       01  W-FLG1            PIC 9(01).
       01  W-FLG2            PIC 9(01).
       01  W-FLG4            PIC 9(01).
       01  W-FLG5            PIC 9(01).
       01  WKKEI.
           02  W-KEIII       OCCURS    7   TIMES.
               03  WK-KEI    PIC S9(11).
       01  SWKKEI.
           02  SW-KEIII      OCCURS    7   TIMES.
               03  SWK-KEI   PIC S9(11).
       01  OLD-GOCD          PIC 9(03).
       01  SW-PRN            PIC 9(01).
       01  W-PAGE            PIC 9(03).
       01  WK-PAGE           PIC ZZ9.
       01  WKS-PAGE          PIC N(03).
       01  W-LINE            PIC 9(02).
       01  W-OLDKEY          PIC 9(02).
       01  SOEJI1            PIC 9(01).
       01  SOJI1             PIC 9(01).
       01  SOEJI2            PIC 9(01).
       01  SOEJI3            PIC 9(01).
       01  SOEJI4            PIC 9(01).
       01  SOEJI5            PIC 9(01).
       01  W-KUBUN.
           02  ARINASHI      OCCURS      7    TIMES.
               03  ARNS      PIC X(01).
       01  GAMEN-AREA.
           02  W-OKC         PIC X(01).
           02  WKSP          PIC X(26)  VALUE SPACE.
       01  ERR-SW            PIC 9(01).
       COPY              LWMSG_PR.
       COPY              KEIHI.
       COPY              BUMONF.
       COPY              BKHHAI.
       COPY              FCTL.
       COPY              BKHPRN.
       COPY              KANGEL.
       77  PRN-REC           PIC X(230).
       77  USER_ID           PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE   PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER     PIC  9(003).
       77  ESTAT             PIC  X(002).
       77  RESU              PIC  9(001).
       77  RESP              PIC  9(001).
       77  RET               PIC  9(001) VALUE ZERO.
      *
       01  CLR-AREA.
           02  FILLER        PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-WOKC      PIC X(01).
       01  DSP-INI.
           02  FILLER.
               03  FILLER    PIC 9(02).
               03  FILLER    PIC N(01) VALUE "年".
               03  FILLER    PIC 9(02).
               03  FILLER    PIC N(02) VALUE "月度".
               03  FILLER    PIC X(26).
               03  FILLER    PIC N(12) VALUE
                   "部門別経費管理表（月次）".
           02  FILLER.
               03  FILLER  PIC X(18) VALUE "確認 OK=1,NO=9 ( )".
       01  DISP-BUZZER.
           02  DISP-BUZ-B01   PIC X(05) VALUE X"1B4201".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-011    PIC  X(18)
                              VALUE  "BKH-PRN KEY  ｱﾘﾏｾﾝ".
               05  DSP-012    PIC  X(19)
                              VALUE  "ｺﾝﾄﾛｰﾙ ﾌｧｨﾙ ﾐﾂｶﾘﾏｾﾝ".
               05  DSP-013    PIC  X(19)
                              VALUE  "BKHPRN-REC  ﾗｲﾄ ｴﾗｰ".
               05  DSP-014    PIC  X(20)
                              VALUE  "BKHPRN-REC  ﾘﾗｲﾄ ｴﾗｰ".
               05  DSP-015    PIC  X(29)
                              VALUE  "ﾌﾞﾓﾝﾍﾞﾂ ｹｲﾋ ﾊｲﾚﾂ ﾌｧｨﾙ ﾐﾂｶﾘﾏｾﾝ".
               05  DSP-016    PIC  X(17)
                              VALUE  "ﾌﾞﾓﾝﾒｲﾏｽﾀ ﾐﾂｶﾘﾏｾﾝ".
               05  DSP-017    PIC  X(21)
                              VALUE  "ｶﾝｼﾞｶﾓｸ ﾏｽﾀ   ﾐﾂｶﾘﾏｾﾝ".
       COPY  LSMSG_PR.
       PROCEDURE                      DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "PR-999" RETURNING RESP.
      *       01  CLR-AREA.
       CALL "SD_Init" USING
            "CLR-AREA" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "CLEAR" "X" "0" "0" "12" " " "CLR-AREA"  RETURNING RESU.
      *       01  ACP-AREA.
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-WOKC" "X" "24" "77" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-WOKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *       01  DSP-INI.
       CALL "SD_Init" USING
            "DSP-INI" " " "0" "0" "78" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-INI" " " "1" "0" "60" " " "DSP-INI"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0101DSP-INI" "9" "1" "2" "2" " " "01DSP-INI"
            RETURNING RESU.
       CALL "SD_From" USING
            "0101DSP-INI" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0201DSP-INI" "N" "1" "4" "2" "0101DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0301DSP-INI" "9" "1" "6" "2" "0201DSP-INI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301DSP-INI" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0401DSP-INI" "N" "1" "8" "4" "0301DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0501DSP-INI" "RX" "1" "28" "26" "0401DSP-INI" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0501DSP-INI" BY REFERENCE WKSP "26" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0601DSP-INI" "N" "1" "29" "24" "0501DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-INI" " " "24" "0" "18" "01DSP-INI" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0102DSP-INI" "X" "24" "61" "18" " " "02DSP-INI"
            RETURNING RESU.
      *       01  DISP-BUZZER    LINE  24.
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-B01" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *       01  DSP-AREA.
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "143" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "2" "0" "143" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "X" "2" "5" "18" " " "DSP-010"  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-012" "X" "2" "5" "19" "DSP-011" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-013" "X" "2" "5" "19" "DSP-012" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-014" "X" "2" "5" "20" "DSP-013" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-015" "X" "2" "5" "29" "DSP-014" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-016" "X" "2" "5" "17" "DSP-015" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-017" "X" "2" "5" "21" "DSP-016" " "  RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      *
       ENTRY-000.
           PERFORM INIT-000 THRU INIT-999.
           PERFORM MAIN-000 THRU MAIN-999    UNTIL   W-FLG1    =   3.
           PERFORM CLSE-ENT THRU CLSE-EXT.
       ENTRY-999.
           CALL "DB_Close".
           STOP  RUN.
       INIT-000.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE   "DATE  "  TO   FCTL-KEY1.
      *           READ    FCTL-F      INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_Close"
               STOP RUN
           END-IF.
           MOVE    FCTL-REC1   TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           MOVE    Z-KONYMD    TO  ZYMD.
           PERFORM Z-RTN     THRU  Z-EXT.
           IF  ZI  >  15
               CALL "DB_Close"
               STOP RUN
           END-IF.
      *
           CALL "SD_Output" USING "CLR-AREA" CLR-AREA "p"
                                          RETURNING RESU.
           CALL "SD_Output" USING "DSP-INI" DSP-INI "p"
                                          RETURNING RESU.
           CALL "DB_F_Open" USING
            "OUTPUT" BKH-PRN_PNAME1 " " BY REFERENCE BKH-PRN_IDLST "1"
            "BKHPRN-KEY" BY REFERENCE BKHPRN-KEY.
           CALL "PR_Open" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE BKH-PRN_IDLST BKH-PRN_PNAME1.
           ACCEPT    CRNT-DATE   FROM   DATE.
           CALL "DB_F_Open" USING "INPUT SEQUENTIAL" HH-F_PNAME1 
            "SHARED" BY REFERENCE HH-F_IDLST "1"
            "HH-KEY" BY REFERENCE HH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BKHHAI-K_PNAME1 "SHARED" BY REFERENCE 
            BKHHAI-K_IDLST "1" "BKHHAI-KEY" BY REFERENCE BKHHAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "I-O" BKH-PRN_PNAME1 " " BY REFERENCE BKH-PRN_IDLST "1"
            "BKHPRN-KEY" BY REFERENCE BKHPRN-KEY.
       INIT-002.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-WOKC "ACP-WOKC" "X" "1"
               BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  = "P9"
               PERFORM  CLSE-ENT THRU  CLSE-EXT
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  INIT-002
           END-IF.
           IF  W-OKC  NOT =  "9" AND "1"
               GO  TO  INIT-002
           END-IF.
           IF  W-OKC  =  "9"
               PERFORM  CLSE-ENT THRU  CLSE-EXT
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE    1   TO  W-FLG1.
           MOVE   99   TO   W-LINE.
           MOVE    1   TO  W-FLG5.
       INIT-999.
           EXIT.
       MAIN-000.
           PERFORM INP-000 THRU INP-999.
           PERFORM OUT-000 THRU OUT-999.
       MAIN-999.
           EXIT.
       INP-000.
           IF  W-FLG1  =   1
               PERFORM   INP1-000 THRU INP1-999
           END-IF.
           IF  W-FLG1  =   2
               PERFORM   INP6-000 THRU INP6-999
           END-IF.
       INP-999.
           EXIT.
       INP1-000.
      *           READ    HH-F    AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" HH-F_PNAME1 BY REFERENCE HH-R " " RETURNING RET.
           IF  RET = 1
               PERFORM   STA-000 THRU STA-999
               MOVE    2   TO  W-FLG1
           END-IF.
       INP1-999.
           EXIT.
       STA-000.
           MOVE    0          TO   ERR-SW.
           MOVE    LOW-VALUE  TO   BKHPRN-KEY.
      *           START   BKH-PRN  KEY  NOT   <  BKHPRN-KEY INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            BKH-PRN_PNAME1 "BKHPRN-KEY" " NOT < " BKHPRN-KEY
            RETURNING RET.
           IF  RET = 1
               PERFORM   ERR-000 THRU ERR-999
               MOVE  1   TO  ERR-SW
           END-IF.
       STA-999.
           EXIT.
       INP6-000.
           IF  ERR-SW  =  1
               MOVE  3             TO  W-FLG1
               GO TO  INP6-999
           END-IF.
      *           READ    BKH-PRN  NEXT   AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BKH-PRN_PNAME1 BY REFERENCE BKHPRN-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  3  TO  W-FLG1
           END-IF.
       INP6-999.
           EXIT.
       OUT-000.
           IF  W-FLG1  =  1
               PERFORM   KOS-000   THRU KOS-999
           END-IF.
           IF  W-FLG1  =  2
               PERFORM   LIS-000   THRU LIS-999
           END-IF.
           IF  W-FLG1  =  3
               PERFORM   GOKEI-000 THRU GOKEI-999
           END-IF.
       OUT-999.
           EXIT.
       KOS-000.
           PERFORM     RAN2-000 THRU RAN2-999.
           PERFORM     KOS6-000 THRU KOS6-999.
       KOS-999.
           EXIT.
       RAN2-000.
           MOVE    HH-BUCD   TO    BNM-KEY.
      *           READ  BNM     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC " "
            RETURNING RET.
           IF  RET = 1
               PERFORM   ERR5-000 THRU ERR-999
           END-IF.
       RAN2-999.
           EXIT.
       KOS6-000.
           MOVE   1   TO   SOEJI1.
       KOS6-001.
           IF  SOEJI1  >   6
               GO   TO   KOS6-999
           END-IF.
           IF  BNM-KHPG(SOEJI1)   =   00
               GO    TO   KOS6-005
           END-IF.
           PERFORM     RAN6-000  THRU RAN6-999.
           PERFORM     EDIT6-000 THRU EDIT6-999.
           PERFORM     SYU6-000  THRU SYU6-999.
       KOS6-005.
           ADD   1   TO   SOEJI1.
           GO   TO   KOS6-001.
       KOS6-999.
           EXIT.
       RAN6-000.
           MOVE   BNM-KHPG(SOEJI1)   TO   BKHPRN-PG.
           MOVE   HH-KACD     TO   BKHPRN-ACM.
           MOVE   HH-HOCD     TO   BKHPRN-ACS.
      *           READ    BKH-PRN     INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BKH-PRN_PNAME1 BY REFERENCE BKHPRN-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE   1   TO  W-FLG2
               GO  TO  RAN6-999
           END-IF.
           MOVE   0   TO   W-FLG2.
       RAN6-999.
           EXIT.
       EDIT6-000.
           IF  W-FLG2  NOT  =   1
               GO  TO  EDIT6-002
           END-IF.
           INITIALIZE  BKHPRN-REC.
           MOVE   BNM-KHPG(SOEJI1)    TO   BKHPRN-PG.
           MOVE   HH-KACD    TO    BKHPRN-ACM.
           MOVE   HH-HOCD    TO    BKHPRN-ACS.
           MOVE    HH-GOCD   TO   BKHPRN-GCD.
           MOVE    HH-GYO    TO   BKHPRN-GYO.
       EDIT6-002.
           MOVE   BNM-KHLN(SOEJI1)   TO   SOJI1.
           ADD HH-GEL(ZI) TO   BKHPRN-GI(SOJI1).
       EDIT6-999.
           EXIT.
       SYU6-000.
           IF  W-FLG2  NOT  =   1
               GO  TO  SYU6-002
           END-IF.
      *           WRITE   BKHPRN-REC  INVALID  KEY
      *///////////////
           CALL "DB_Insert" USING
            BKH-PRN_PNAME1 BKH-PRN_LNAME BKHPRN-REC  RETURNING RET.
           IF  RET = 1
               PERFORM  ERR2-000 THRU ERR2-999
           END-IF.
           GO  TO  SYU6-999.
       SYU6-002.
      *           REWRITE BKHPRN-REC  INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            BKH-PRN_PNAME1 BKH-PRN_LNAME BKHPRN-REC RETURNING RET.
           IF  RET = 1
               PERFORM  ERR3-000 THRU ERR3-999
           END-IF.
       SYU6-999.
           EXIT.
       LIS-000.
           IF  W-FLG4   =  0
               MOVE  BKHPRN-PG  TO  W-OLDKEY
               MOVE  BKHPRN-GCD TO  OLD-GOCD
               MOVE    0   TO  SW-PRN
               MOVE    1   TO  W-FLG4
           END-IF.
           IF  W-OLDKEY  NOT  =  BKHPRN-PG
               PERFORM   GOKEI-000 THRU GOKEI-999
               MOVE    BKHPRN-GCD  TO  OLD-GOCD
           END-IF.
           IF  OLD-GOCD  NOT  =  BKHPRN-GCD
               MOVE  1  TO  SW-PRN
               PERFORM   SYOKEI-000 THRU SYOKEI-999
           END-IF.
           PERFORM     HEAD-000 THRU HEAD-999.
           PERFORM     MEI-000  THRU MEI-999.
           PERFORM     SYU-000  THRU SYU-999.
           MOVE    BKHPRN-PG   TO  W-OLDKEY.
           MOVE    BKHPRN-GCD  TO  OLD-GOCD.
       LIS-999.
           EXIT.
       SYOKEI-000.
           IF  SW-PRN   =   0
               GO  TO  SYOKEI-007
           END-IF.
           MOVE   1   TO   SOEJI4.
       SYOKEI-0009.
           IF  SOEJI4   >   7
               GO   TO  SYOKEI-004
           END-IF.
           IF  ARNS(SOEJI4)   =   "A"
               MOVE  SWK-KEI(SOEJI4)   TO   W-RETSU(SOEJI4)
               GO     TO  SYOKEI-003
           END-IF.
           MOVE    SPACE   TO   WR-P1(SOEJI4).
       SYOKEI-003.
           MOVE   X"1AC2"   TO   F1(SOEJI4).
           MOVE    SPACE     TO   F2(SOEJI4).
           ADD   1   TO   SOEJI4.
           GO   TO   SYOKEI-0009.
       SYOKEI-004.
           MOVE    "小　計　"            TO  NAME3.
           MOVE    "　　　　　　　　"    TO  FILLER3.
           MOVE    W-PRNREC1   TO  PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           ADD    1       TO   W-LINE.
       SYOKEI-007.
           MOVE   ZERO    TO   SWKKEI.
       SYOKEI-999.
           EXIT.
       GOKEI-000.
           PERFORM       SYOKEI-000 THRU SYOKEI-999.
           MOVE    0     TO    SW-PRN.
           MOVE    H7    TO    PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE    H6    TO    PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
       GOKEI-0000.
           MOVE   1   TO   SOEJI4.
       GOKEI-0009.
           IF  SOEJI4   >   7
               GO   TO   GOKEI-004
           END-IF.
           IF  ARNS(SOEJI4)   =   "A"
               MOVE   WK-KEI(SOEJI4)   TO   W-KEI(SOEJI4)
               GO     TO   GOKEI-003
           END-IF.
           MOVE    SPACE   TO   WR-P2(SOEJI4).
       GOKEI-003.
           MOVE   X"1AC2"   TO   F3(SOEJI4).
           MOVE    SPACE     TO   F4(SOEJI4).
           ADD   1   TO   SOEJI4.
           GO   TO   GOKEI-0009.
       GOKEI-004.
           MOVE    W-PRNREC2   TO  PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE   ZERO    TO   WKKEI.
           MOVE    70   TO    W-LINE.
       GOKEI-999.
           EXIT.
       HEAD-000.
           IF  W-LINE  =  70
               GO   TO   HEAD-003
           END-IF.
           IF  W-LINE  =  99
               GO  TO  HEAD-004
           END-IF.
           IF  W-LINE  >  59
               MOVE H7 TO PRN-REC
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-REC RETURNING RESP
               GO   TO   HEAD-003
           ELSE
               GO   TO   HEAD-999
           END-IF.
       HEAD-003.
           MOVE   SPACE   TO   PRN-REC.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HEAD-004.
           MOVE    0    TO   W-LINE.
           ADD     1    TO   W-PAGE.
           MOVE    CRNT-MM   TO   W-MM.
           MOVE    CRNT-DD   TO   W-DD.
           MOVE    CRNT-YY   TO   W-Y.
           MOVE    W-MM      TO   W-M.
           MOVE    W-DD      TO   W-D.
           MOVE  W-Y   TO  H1-YY.
           MOVE  W-M   TO  H1-MM.
           MOVE  W-D   TO  H1-DD.
           MOVE  W-PAGE    TO   WK-PAGE.
           MOVE  WK-PAGE   TO   WKS-PAGE.
           MOVE  WKS-PAGE  TO   H1-PAGE.
           MOVE    H1   TO   PRN-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE    Z-GEMMM  TO  W-K-MM.
           MOVE    Z-GEMYY2 TO  W-K-Y.
           MOVE  W-K-MM   TO   W-K-M.
           MOVE  W-K-Y   TO  H2-YY.
           MOVE  W-K-M   TO  H2-MM.
           MOVE    H2   TO   PRN-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE    H3   TO   PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE   1   TO   SOEJI2.
       HEAD-016.
           IF  SOEJI2   >   7
               GO  TO  HEAD-025
           END-IF.
           MOVE    BKHPRN-PG   TO  BKHHAI-PG.
           MOVE    SOEJI2    TO  BKHHAI-LN.
      *           READ    BKHHAI-K    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BKHHAI-K_PNAME1 BY REFERENCE BKHHAI-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "　"     TO  WK-BNMNM(SOEJI2)
               MOVE   SPACE   TO   WK-BNM(SOEJI2)
               MOVE    "N"     TO   ARNS(SOEJI2)
               GO  TO  HEAD-022
           END-IF.
           MOVE    BKHHAI-BUCD   TO   BNM-KEY.
      *           READ    BNM         INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  ALL "　"     TO  WK-BNMNM(SOEJI2)
               MOVE   SPACE   TO   WK-BNM(SOEJI2)
               MOVE    "N"    TO   ARNS(SOEJI2)
               GO  TO  HEAD-022
           END-IF.
           MOVE    "A"    TO    ARNS(SOEJI2).
           MOVE    BNM-KEY   TO   WK-BNM(SOEJI2).
           MOVE    BNMNMN  TO  WK-BNMNM(SOEJI2).
       HEAD-022.
           MOVE  X"1AC2"  TO  P1(SOEJI2).
           MOVE    SPACE   TO  P2(SOEJI2).
           MOVE  X"1AC2"  TO  P3(SOEJI2).
           MOVE    SPACE   TO  P4(SOEJI2).
           ADD    1    TO   SOEJI2.
           GO  TO  HEAD-016.
       HEAD-025.
           MOVE    H4  TO   PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE    H5   TO  PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           MOVE    H6   TO   PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           ADD    9    TO   W-LINE.
       HEAD-999.
           EXIT.
       MEI-000.
           MOVE    BKHPRN-ACCD   TO    KNG-KEY.
      *           READ    KNG    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM   ERR6-000 THRU ERR6-999
           END-IF.
           MOVE    KNGNM   TO  NAME1.
           MOVE   "　　"       TO   FILLER1.
           MOVE    W-KNG2  TO  W-KNG1.
           MOVE   1   TO   SOEJI5.
       MEI-0039.
           IF  SOEJI5   >   7
               GO  TO  MEI-008
           END-IF.
           IF  ARNS(SOEJI5)   =   "A"
               MOVE   BKHPRN-GI(SOEJI5)   TO  W-RETSU(SOEJI5)
               GO   TO   MEI-006
           END-IF.
           MOVE    SPACE   TO   WR-P1(SOEJI5).
       MEI-006.
           MOVE   X"1AC2"   TO   F1(SOEJI5).
           MOVE    SPACE     TO   F2(SOEJI5).
           ADD   1   TO   SOEJI5.
           GO   TO   MEI-0039.
       MEI-008.
           IF  BKHPRN-GYO  >   1
               MOVE H6 TO PRN-REC
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-REC RETURNING RESP
               ADD   1   TO   W-LINE
               PERFORM   HEAD-000 THRU HEAD-999
               COMPUTE   BKHPRN-GYO   =   BKHPRN-GYO   -   1
               GO   TO   MEI-008
           END-IF.
           MOVE    W-PRNREC1   TO   PRN-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-REC RETURNING RESP.
           ADD   1   TO   W-LINE.
       MEI-999.
           EXIT.
       SYU-000.
           IF  W-FLG5  =   1
               MOVE    0     TO     W-FLG5
           END-IF.
           PERFORM     ADD-000 THRU ADD-999.
       SYU-999.
           EXIT.
       ADD-000.
           MOVE   1   TO   SOEJI3.
       ADD-001.
           IF  SOEJI3   >   7
               GO  TO  ADD-999
           END-IF.
           ADD  BKHPRN-GI(SOEJI3)
                        TO   WK-KEI(SOEJI3).
           ADD  BKHPRN-GI(SOEJI3)
                        TO  SWK-KEI(SOEJI3).
           ADD   1   TO   SOEJI3.
           GO  TO  ADD-001.
       ADD-999.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BKHHAI-K_IDLST BKHHAI-K_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BKH-PRN_IDLST BKH-PRN_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
       ERR-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-011" DSP-011 "p"
                                                  RETURNING RESU. 
       ERR-999.
           EXIT.
       ERR1-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-012" DSP-012 "p"
                                                  RETURNING RESU. 
       ERR1-999.
           EXIT.
       ERR2-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-013" DSP-013 "p"
                                                  RETURNING RESU. 
       ERR2-999.
           EXIT.
       ERR3-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-014" DSP-014 "p"
                                                  RETURNING RESU. 
       ERR3-999.
           EXIT.
       ERR4-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-015" DSP-015 "p"
                                                  RETURNING RESU. 
       ERR4-999.
           EXIT.
       ERR5-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-016" DSP-016 "p"
                                                  RETURNING RESU. 
       ERR5-999.
           EXIT.
       ERR6-000.
           CALL "SD_Output" USING "DISP-BUZ-B01" DISP-BUZ-B01 "p"
                                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-017" DSP-017 "p"
                                                  RETURNING RESU. 
       ERR6-999.
           EXIT.
       COPY LPMSG_PR.
