      *********************************************
      *    合計残高試算表                         *
      *    BASE  :  ZA0201                        *
      *    DATE  :  91/01/07                      *
      *    AUTHOR : MAYUMI.I                      *
      *********************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR400L.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       01  H1.
           02  FILLER          PIC X(02)   VALUE SPACE.
           02  H1-YY           PIC N(02).
           02  FILLER          PIC N(01)   VALUE "年".
           02  H1-MM           PIC N(02).
           02  FILLER          PIC N(01)   VALUE "月".
           02  H1-DD           PIC N(02).
           02  FILLER          PIC N(03)   VALUE "日作成".
           02  FILLER          PIC X(25)   VALUE SPACE.
           02  FILLER          PIC X(2)    VALUE X"1AC0".
           02  FILLER          PIC N(15)   VALUE
                               "　合　計　残　高　試　算　表　".
           02  FILLER          PIC X(42)   VALUE X"1AC1".
           02  H1-PAGE         PIC N(05).
           02  FILLER          PIC N(01)   VALUE "頁".
       01  H2.
           02  FILLER          PIC X(51)   VALUE SPACE.
           02  FILLER          PIC N(1)    VALUE "（".
           02  H2-YY           PIC N(2).
           02  FILLER          PIC N(1)    VALUE "年".
           02  H2-MM           PIC N(2).
           02  FILLER          PIC N(1)    VALUE "月".
           02  H2-DD           PIC N(2).
           02  FILLER          PIC N(4)    VALUE "日現在）".
       01  H3.
           02  FILLER          PIC X(137)  VALUE X"1AC0".
           02  FILLER          PIC X(2)    VALUE X"1AC1".
       01  H4.
           02  FILLER          PIC X(3)    VALUE X"1AC2".
           02  FILLER          PIC N(1)    VALUE "借".
           02  FILLER          PIC X(1)    VALUE SPACE.
           02  FILLER          PIC X(30)      VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(18)   VALUE X"1AC01AC2".
           02  FILLER          PIC N(1)    VALUE "期".
           02  FILLER          PIC X(13)   VALUE SPACE.
           02  FILLER          PIC N(1)    VALUE "欄".
           02  FILLER          PIC X(14)   VALUE SPACE.
           02  FILLER          PIC X(16)   VALUE X"1AC2".
           02  FILLER          PIC N(1)    VALUE "月".
           02  FILLER          PIC X(12)   VALUE SPACE.
           02  FILLER          PIC N(1)    VALUE "欄".
           02  FILLER          PIC X(13)   VALUE SPACE.
           02  FILLER          PIC X(4)    VALUE X"1AC21AC1".
           02  FILLER          PIC X(5)    VALUE X"1A24212078".
           02  FILLER          PIC N(1)    VALUE "　".
           02  FILLER          PIC X(5)    VALUE X"1A24212474".
       01  H5.
           02  FILLER          PIC X(5)    VALUE X"1AC21AC0".
           02  FILLER          PIC N(1)    VALUE "貸".
           02  FILLER          PIC X(1)    VALUE SPACE.
           02  FILLER          PIC X(2)    VALUE X"1AC2".
           02  FILLER          PIC N(6)    VALUE "　科　目　名".
           02  FILLER          PIC X(9)    VALUE SPACE.
           02  FILLER          PIC N(3)    VALUE "コード".
           02  FILLER          PIC X(1)    VALUE SPACE.
           02  FILLER          PIC X(5)    VALUE X"1AC2".
           02  FILLER          PIC N(1)    VALUE "残".
           02  FILLER          PIC X(5)    VALUE SPACE.
           02  FILLER          PIC N(1)    VALUE "高".
           02  FILLER          PIC X(3)    VALUE SPACE.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC N(4)    VALUE "前期繰越".
           02  FILLER          PIC X(3)    VALUE SPACE.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC N(4)    VALUE "当期借方".
           02  FILLER          PIC X(3)    VALUE SPACE.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC N(4)    VALUE "当期貸方".
           02  FILLER          PIC X(3)    VALUE SPACE.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC N(4)    VALUE "前月繰越".
           02  FILLER          PIC X(3)    VALUE SPACE.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC N(4)    VALUE "当月借方".
           02  FILLER          PIC X(2)    VALUE SPACE.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC N(4)    VALUE "当月貸方".
           02  FILLER          PIC X(2)    VALUE SPACE.
           02  FILLER          PIC X(4)    VALUE X"1AC21AC1".
           02  FILLER          PIC X(5)    VALUE X"1A24212078".
           02  FILLER          PIC N(1)    VALUE "　".
           02  FILLER          PIC X(5)    VALUE X"1A24212474".
       01  H6.
           02  FILLER          PIC X(6)    VALUE X"1AC2".
           02  FILLER          PIC X(30)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(16)   VALUE X"1AC2".
           02  FILLER          PIC X(16)   VALUE X"1AC2".
           02  FILLER          PIC X(2)    VALUE X"1AC2".
           02  FILLER          PIC X(5)    VALUE X"1A24212078".
           02  FILLER          PIC N(1)    VALUE "　".
           02  FILLER          PIC X(5)    VALUE X"1A24212474".
       01  H7.
           02  FILLER          PIC X(8)    VALUE X"1AC21AC0".
           02  FILLER          PIC X(30)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(17)   VALUE X"1AC2".
           02  FILLER          PIC X(16)   VALUE X"1AC2".
           02  FILLER          PIC X(16)   VALUE X"1AC2".
           02  FILLER          PIC X(4)    VALUE X"1AC21AC1".
           02  FILLER          PIC X(5)    VALUE X"1A24212078".
           02  FILLER          PIC N(1)    VALUE "　".
           02  FILLER          PIC X(5)    VALUE X"1A24212474".
       01  H8.
           02  H8-MSG1.
             03  FILLER        PIC N(10)   VALUE
                               "借　　方　　合　　計".
           02  H8-MSG2.
             03  FILLER        PIC N(10)   VALUE
                               "貸　　方　　合　　計".
       01  M1.
           02  FILLER          PIC X(3).
           02  M1-DRCR         PIC N(1).
           02  FILLER          PIC X(4).
           02  M1-ACCUTNM      PIC X(20).
           02  M1-FIL1         PIC X(1).
           02  M1-ACCUTCD      PIC X(4).
           02  M1-FIL2         PIC X(2).
           02  FILLER          PIC X(2).
           02  M1-ZAN          PIC ------,---,--9.
           02  M1-FIL4         PIC X(3).
           02  M1-BFCTZN       PIC ------,---,--9.
           02  FILLER          PIC X(3).
           02  M1-CTDR         PIC ------,---,--9.
           02  FILLER          PIC X(3).
           02  M1-CTCR         PIC ------,---,--9.
           02  M1-FIL5         PIC X(3).
           02  M1-BFMZN        PIC ------,---,--9.
           02  FILLER          PIC X(3).
           02  M1-MDR          PIC -----,---,--9.
           02  FILLER          PIC X(3).
           02  M1-MCR          PIC -----,---,--9.
           02  M1-FIL6         PIC X(15).
       01  W1.
           02  PCNT            PIC 9(5).
           02  LINCNT          PIC 9(2)    VALUE 70.
           02  PRINT-SW        PIC 9(1).
           02  I               PIC 9(1).
           02  J               PIC 9(2).
           02  W-ZZZZ9         PIC ZZZZ9.
           02  W-Z9            PIC Z9.
           02  W-KAKU          PIC X(1).
      *
       01  W2.
           02  W2-TOTAL    OCCURS  6.
             03  W2-ZAN        PIC S9(11).
             03  W2-BFCTZN     PIC S9(11).
             03  W2-CTDR       PIC S9(11).
             03  W2-CTCR       PIC S9(11).
             03  W2-BFMZN      PIC S9(11).
             03  W2-MDR        PIC S9(10).
             03  W2-MCR        PIC S9(10).
             03  W2-ADDCNT     PIC 9(4).
      *
       01  WORK-AREA.
           02  SET-WORK.
             03  W-ZENKI       PIC S9(11).
             03  W-ZAN         PIC S9(11).
             03  W-KARI        PIC S9(11).
             03  W-KASI        PIC S9(11).
           02  SOE             PIC  9(02).
           02  FI              PIC  9(02).
           02  TI              PIC  9(02).
      *
       01  HIZUKE              PIC 9(06).
       01  W-HIZUKE  REDEFINES  HIZUKE.
           02  W-YY                PIC 9(02).
           02  W-MM                PIC 9(02).
           02  W-DD                PIC 9(02).
      *
       COPY    LWMSG_PR.
       COPY    ACCUNT.
       COPY    LKAZAN.
       COPY    KANGEL.
       COPY    FCTL.
      *
       77  F5-REC      PIC X(200).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER.
               03      FILLER  PIC  N(01)  VALUE  "年".
               03      FILLER  PIC  N(02)  VALUE  "月度".
               03      FILLER  PIC  9(02).
               03      FILLER  PIC  9(02).
           02  FILLER  PIC  X(16)  VALUE  " 合計残高試算表 ".
           02  FILLER  PIC  X(18)  VALUE  "確認 OK=1,NO=9 ( )".
       01  ACP-AREA.
           02  ACP-KAKU    PIC X(01).
      *
           COPY  LSMSG_PR.
      ********************
      ********************
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "SPOUT1-999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "44" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "10" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-AREA" "N" "1" "4" "2" " " "01DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-AREA" "N" "1" "8" "4" "0101DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "9" "1" "2" "2" "0201DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-AREA" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-AREA" "9" "1" "6" "2" "0301DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-AREA" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "RX" "1" "33" "16" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "24" "61" "18" "02DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
      *
           CALL "DB_F_Open" USING
            "INPUT SEQUENTIAL" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "PR_Open" RETURNING RESP.
      *
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE   "DATE  "    TO   FCTL-KEY1.
      *           READ  FCTL-F  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "INV-MCT" INV-MCT "p"
                                          RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                          RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               GO  TO  END-99
           END-IF.
           MOVE  FCTL-REC       TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                         RETURNING RESU.
           MOVE       ZERO         TO     W2.
       ST-05.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               GO  TO  END-99
           END-IF.
           IF  W-KAKU NOT = "1" AND "9"
               GO  TO  ST-05
           END-IF.
           IF  ESTAT NOT = "01"
               GO  TO  ST-05
           END-IF.
           IF  W-KAKU = "9"
               GO  TO  END-99
           END-IF.
      ***
           ACCEPT  HIZUKE  FROM  DATE.
           MOVE  W-YY     TO  H1-YY.
           MOVE  W-MM     TO  W-Z9.
           MOVE  W-Z9     TO  H1-MM.
           MOVE  W-DD     TO  W-Z9.
           MOVE  W-Z9     TO  H1-DD.
           MOVE  Z-KONYY2    TO  H2-YY.
           MOVE  Z-KONMM     TO  W-Z9.
           MOVE  W-Z9        TO  H2-MM.
           MOVE  Z-KONDD     TO  W-Z9.
           MOVE  W-Z9        TO  H2-DD.
      *
           MOVE  Z-KONYMD     TO  ZYMD.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               GO TO END-99
           END-IF.
           MOVE ZI     TO TI.
           IF  TI > 12
               MOVE 13     TO FI
           ELSE
               IF  Z-KSMM = 12
                   MOVE 1     TO FI
               ELSE
                   COMPUTE FI = Z-KSMM + 1
               END-IF
           END-IF.
      *
       ST-10.
      *           READ AM NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO END-99
           END-IF.
           MOVE ZERO     TO SET-WORK.
           MOVE AM-KEY   TO KZM-KEY.
      *           READ KZM-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KZM-F_PNAME1 BY REFERENCE KZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE KZM-R
           END-IF.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
           MOVE       1            TO     PRINT-SW.
           PERFORM    PRINT-RTN    THRU   PRINT-EXT.
           PERFORM    ADD-RTN      THRU   ADD-EXT.
           GO TO      ST-10.
       CLSE-ENT.
       END-99.
           IF  LINCNT       NOT =  70
               MOVE     5            TO     PRINT-SW
               PERFORM  PRINT-RTN    THRU   PRINT-EXT
           END-IF.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_Close".
           STOP       RUN.
       CLSE-EXT.
      *****
      *****
       PRINT-RTN.
           MOVE       H6           TO     M1.
           IF  PRINT-SW     =      1
               GO TO PR-50
           END-IF.
           IF  PRINT-SW     =      2
               GO TO PR-50
           END-IF.
           IF  PRINT-SW     =      3
               GO TO PR-50
           END-IF.
           IF  PRINT-SW     =      4
               GO TO PR-50
           END-IF.
           MOVE       H6           TO     M1.
           MOVE       H7           TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           ADD        W2-CTDR  (5) TO     W2-CTDR  (4).
           MOVE       0            TO     W2-CTDR  (5).
           ADD        W2-MDR   (5) TO     W2-MDR   (4).
           MOVE       0            TO     W2-MDR   (5).
           ADD        W2-CTCR  (4) TO     W2-CTCR  (5).
           MOVE       0            TO     W2-CTCR  (4).
           ADD        W2-MCR   (4) TO     W2-MCR   (5).
           MOVE       0            TO     W2-MCR   (4).
           MOVE       H8-MSG1      TO     M1-ACCUTNM.
           MOVE       W2-TOTAL (4) TO     W2-TOTAL (6).
           PERFORM    PRI-RTN      THRU   PRI-EXT.
           MOVE       H7           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H6           TO     M1.
           MOVE       H8-MSG2      TO     M1-ACCUTNM.
           MOVE       W2-TOTAL (5) TO     W2-TOTAL (6).
           PERFORM    PRI-RTN      THRU   PRI-EXT.
           MOVE       H7           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           GO TO      PRINT-EXT.
       PR-50.
      *PR-60.
           MOVE       H6           TO     M1.
           IF  DR-CR OF AM-REC  =      1
               MOVE     "借"       TO     M1-DRCR
           ELSE
               MOVE     "貸"       TO     M1-DRCR
           END-IF.
           MOVE       ZERO         TO     KNG-KEY.
           MOVE       AM-KEY       TO     K-ACCD.
      *           READ       KNG          INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE        TO     KNGNM
           END-IF.
           MOVE       KNGNM        TO     M1-ACCUTNM.
           MOVE       "("          TO     M1-FIL1.
           MOVE       ")"          TO     M1-FIL2.
           MOVE       AM-KEY       TO     M1-ACCUTCD.
           IF  DR-CR OF AM-REC = 1
               COMPUTE W2-ZAN(6) = W-ZENKI + W-KARI - W-KASI
           ELSE
               COMPUTE W2-ZAN(6) = W-ZENKI + W-KASI - W-KARI
           END-IF.
           MOVE W-ZENKI          TO W2-BFCTZN(6).
           MOVE W-KARI           TO W2-CTDR(6)
           MOVE W-KASI           TO W2-CTCR(6).
           MOVE W-ZAN            TO W2-BFMZN(6).
           MOVE KZM-TJKR(TI)     TO W2-MDR(6).
           MOVE KZM-TJKS(TI)     TO W2-MCR(6).
           PERFORM    PRI-RTN      THRU   PRI-EXT.
       PRINT-EXT.
           EXIT.
      *****
       PRI-RTN.
           IF  LINCNT       NOT <  50
               PERFORM  HEAD-RTN     THRU   HEAD-EXT
           END-IF.
           MOVE       W2-ZAN    (6) TO    M1-ZAN.
           MOVE       W2-BFCTZN (6) TO    M1-BFCTZN.
           MOVE       W2-CTDR   (6) TO    M1-CTDR.
           MOVE       W2-CTCR   (6) TO    M1-CTCR.
           MOVE       W2-BFMZN  (6) TO    M1-BFMZN.
           MOVE       W2-MDR    (6) TO    M1-MDR.
           MOVE       W2-MCR    (6) TO    M1-MCR.
           MOVE       M1           TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           ADD        1            TO     LINCNT.
       PRI-EXT.
           EXIT.
      *****
       ADD-RTN.
           IF  DR-CR = 1
               MOVE 4     TO I
           ELSE
               MOVE 5     TO I
           END-IF.
           ADD W2-ZAN(6)        TO W2-ZAN(I).
           ADD W2-BFCTZN(6)     TO W2-BFCTZN(I).
           ADD W2-CTDR(6)       TO W2-CTDR(I).
           ADD W2-CTCR(6)       TO W2-CTCR(I).
           ADD W2-BFMZN(6)      TO W2-BFMZN(I).
           ADD W2-MDR(6)        TO W2-MDR(I).
           ADD W2-MCR(6)        TO W2-MCR(I).
       ADD-EXT.
           EXIT.
      *****
       HEAD-RTN.
           IF  LINCNT NOT   =      70
               MOVE     H7           TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               MOVE     SPACE        TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
      *
           ADD  1     TO  PCNT.
           MOVE  PCNT     TO  W-ZZZZ9.
           MOVE  W-ZZZZ9     TO  H1-PAGE.
      *
           MOVE     H1           TO     F5-REC .
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H2           TO     F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H3           TO     F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H4           TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H5           TO     F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H6           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       0            TO     LINCNT.
       HEAD-EXT.
           EXIT.
      *****
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF.
           MOVE KZM-ZAN     TO W-ZENKI.
           MOVE FI          TO SOE.
       ZAN-SET-000.
           ADD KZM-TJKR(SOE)     TO W-KARI.
           ADD KZM-TJKS(SOE)     TO W-KASI.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  SOE = 12
               MOVE 1     TO SOE
               GO TO ZAN-SET-000
           END-IF.
           ADD 1     TO SOE.
           GO TO ZAN-SET-000.
       ZAN-SET-500.
           IF  BS-PL = 0
               MOVE 1      TO SOE
           ELSE
               MOVE 13     TO SOE
           END-IF.
       ZAN-SET-600.
           ADD KZM-TJKR(SOE)     TO W-KARI.
           ADD KZM-TJKS(SOE)     TO W-KASI.
           IF  SOE = 12
               IF  DR-CR = 1
                   COMPUTE W-ZENKI = KZM-ZAN + W-KARI - W-KASI
               ELSE
                   COMPUTE W-ZENKI = KZM-ZAN + W-KASI - W-KARI
               END-IF
           END-IF.
           IF  SOE = 12
               MOVE W-ZENKI     TO KZM-ZAN
               MOVE ZERO        TO W-KARI W-KASI
           END-IF.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF.
           IF  SOE = 15
               GO TO ZAN-SET-900
           END-IF.
           ADD 1     TO SOE.
           GO TO ZAN-SET-600.
       ZAN-SET-900.
           IF  DR-CR = 1
               COMPUTE W-ZAN = KZM-ZAN + (W-KARI - KZM-TJKR(TI)) -
                               ( W-KASI - KZM-TJKS(TI))
           ELSE
               COMPUTE W-ZAN = KZM-ZAN + (W-KASI - KZM-TJKS(TI)) -
                               ( W-KARI - KZM-TJKR(TI))
           END-IF.
       ZAN-SET-EX.
           EXIT.
      *********
           COPY  LPMSG_PR.
      *
