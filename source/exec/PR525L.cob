       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR525L.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       01  H1.
           02  P-K             PIC X(05)  VALUE    X"1A24212474".
           02  FILLER          PIC X(02)   VALUE   SPACE.
           02  H1-SYY          PIC N(02).
           02  FILLER          PIC N(01)   VALUE   "年".
           02  H1-SMM          PIC N(02).
           02  FILLER          PIC N(01)   VALUE   "月".
           02  H1-SDD          PIC N(02).
           02  FILLER          PIC N(03)   VALUE   "日作成".
           02  FILLER          PIC X(15)   VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC0".
           02  FILLER          PIC N(14)   VALUE
               "製　造　原　価　報　告　書　".
           02  FILLER          PIC N(05)   VALUE   "（期末）".
           02  FILLER          PIC X(20)   VALUE   X"1AC1".
           02  H1-PAGE         PIC N(04).
           02  FILLER          PIC N(01)   VALUE   "頁".
       01  H2.
           02  FILLER          PIC X(53)   VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "（".
           02  H1-YY           PIC N(2).
           02  FILLER          PIC N(1)    VALUE   "年".
           02  H1-MM           PIC N(2).
           02  FILLER          PIC N(3)    VALUE   "月分）".
       01  H3.
           02  FILLER          PIC X(114)  VALUE   X"1AC0".
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
       01  H4.
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(8)    VALUE   X"1AC01AC2".
           02  FILLER          PIC N(1)    VALUE   "当".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "年".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "欄".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "前".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "年".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "欄".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "差".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "額".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "欄".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC11AC2".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H5.
           02  FILLER          PIC X(9)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "項".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "目".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "名".
           02  FILLER          PIC X(13)   VALUE   SPACE.
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H6.
           02  FILLER          PIC X(38)   VALUE   X"1AC01AC2".
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "金".
           02  FILLER          PIC X(5)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "額".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC2".
           02  FILLER          PIC N(3)    VALUE   "構成比".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "金".
           02  FILLER          PIC X(5)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "額".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC2".
           02  FILLER          PIC N(3)    VALUE   "構成比".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "金".
           02  FILLER          PIC X(5)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "額".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC2".
           02  FILLER          PIC N(3)    VALUE   "比　率".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC21AC1".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H7.
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H8.
           02  FILLER          PIC X(38)   VALUE   X"1AC01AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(4)    VALUE   X"1AC21AC1".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  M1.
           02  FILLER          PIC X(3)    VALUE   SPACE.
           02  M1-NAME1.
               03  FILLER      PIC X(2).
               03  M1-NAME2    PIC X(31).
           02  M1-ITEM1        REDEFINES   M1-NAME1.
               03  FILLER      PIC X(6).
               03  M1-NAME3    PIC X(27).
           02  M1-ITEM2        REDEFINES   M1-NAME1.
               03  FILLER      PIC X(10).
               03  M1-NAME4    PIC X(23).
           02  M1-ITEM3        REDEFINES   M1-NAME1.
               03  FILLER      PIC X(12).
               03  M1-NAME5    PIC X(21).
           02  FILLER          PIC X(2)    VALUE   SPACE.
           02  M1-TOKIN        PIC ----,---,---,--9.
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  M1-TORIT        PIC ----.99.
           02  FILLER          PIC X(3)    VALUE   SPACE.
           02  M1-ZENKIN       PIC ----,---,---,--9.
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  M1-ZENRIT       PIC ----.99.
           02  FILLER          PIC X(3)    VALUE   SPACE.
           02  M1-SA           PIC ----,---,---,--9.
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  M1-SARIT        PIC ----.99.
           02  FILLER          PIC X(5)    VALUE   SPACE.
       01  HYMDIN-W.
           02  HMMIN-W         PIC Z(2).
           02  HDDIN-W         PIC Z(2).
       01  HPAGE               PIC 9(4)    VALUE 0.
       01  HPAGER              PIC ZZZ9.
       01  W1.
           02  LINCNT          PIC 9(2)    VALUE 99.
           02  W1-KIN.
             03  W1-KINTO      PIC S9(11).
             03  W1-KINZE      PIC S9(11).
           02  W1-RIT          PIC S9(4)V9(4).
           02  W1-SA           PIC S9(11).
           02  W1-YMIN.
             03  W1-YYIN       PIC 9(2).
             03  W1-MMIN       PIC 9(2).
           02  W1-IN           PIC X(1).
       01  SYMD.
           02  SYY             PIC 9(02).
           02  SMM             PIC 9(02).
           02  SDD             PIC 9(02).
       01  SOEJI.
           02  I               PIC 9(02).
       COPY    LWMSG_PR.
       COPY    LGENKF.
       COPY    FCTL.
      ***  損益マスタ     (85/3)
       01  PL_PR525L.
           02  PL_PNAME1       PIC  X(004)  VALUE "PL-K".
           02  F               PIC  X(001).
           02  PL_LNAME        PIC  X(009)  VALUE "PL_PR525L".
           02  F              PIC  X(001).
           02  PL_KEY1         PIC  X(100)  VALUE SPACE.
           02  PL_KEY2         PIC  X(100)  VALUE SPACE.
           02  PL_SORT         PIC  X(100)  VALUE SPACE.
           02  PL_IDLST        PIC  X(100)  VALUE SPACE.
           02  PL_RES          USAGE  POINTER.
       01  PL1REC.
           02  PL1KEY          PIC X(3).
           02  PL1LIN          PIC 9(1).
           02  PL1GKB          PIC 9(1).
           02  PL1NAM          PIC X(20).
           02  PL1NAMN     REDEFINES   PL1NAM   PIC N(10).
           02  PL1YY.
             03  PL1ZENKI      PIC S9(11).
             03  PL1TOUKI      PIC S9(11).
           02  PL1MM.
             03  PL1ZENMM      PIC S9(11).
             03  PL1TOUMM      PIC S9(11).
           02  PL1URIKB        PIC X(1).
           02  PL1PKB          PIC 9(1).
           02  PL1TANA         PIC 9(1).
           02  PL1YM.
             03  PL1YYWK       PIC 9(2).
             03  PL1MMWK       PIC 9(2).
           02  FILLER          PIC X(9).
       77  F                   PIC X(001).
      **
       77  F5-REC                  PIC  X(136).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DISP-BUZZER.
           02  DISP-BUZ-J-09       PIC X(05) VALUE X"1B4A09".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-011         PIC X(06) VALUE  "ﾂｷ ｴﾗｰ".
       01  ACP-AREA.
           03  ACP-010.
               05  ACP-011         PIC  9(02).
       COPY  LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "F5-999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-BUZZER
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J-09" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "6" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "22" "0" "6" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "X" "22" "10" "6" " " "DSP-010"  RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" " " "22" "0" "2" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-011" "9" "22" "50" "2" " " "ACP-010"  RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-011" BY REFERENCE LINCNT "2" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           ACCEPT     SYMD         FROM    DATE.
           MOVE       SMM          TO      HMMIN-W.
           MOVE       SDD          TO      HDDIN-W.
           MOVE       SYY          TO      H1-SYY.
           MOVE       HMMIN-W      TO      H1-SMM.
           MOVE       HDDIN-W      TO      H1-SDD.
           CALL "DB_F_Open" USING
            "INPUT" PL_PNAME1 "SHARED" BY REFERENCE PL_IDLST "1"
            "PL1KEY" BY REFERENCE PL1KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY1" BY REFERENCE FCTL-KEY1.
           CALL "DB_F_Open" USING
            "INPUT" GEN_PNAME1 " " BY REFERENCE GEN_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE       "DATE  "     TO     FCTL-KEY1.
      *           READ       FCTL-F       UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               PERFORM  CLSE-ENT   THRU   CLSE-EXT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE       FCTL-REC1    TO     Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           MOVE       Z-KONYMD     TO     ZYMD.
           PERFORM    Z-RTN        THRU   Z-EXT.
           MOVE       ZI           TO     I.
           IF  I  >  15
               PERFORM  CLSE-ENT   THRU   CLSE-EXT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE       Z-TOUTMM(I)  TO     HMMIN-W.
           MOVE       Z-TOUTYY2(I) TO     H1-YY.
           MOVE       HMMIN-W      TO     H1-MM.
           MOVE       ZERO         TO     W1-KIN.
       ST-20.
      *           READ       PL           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" PL_PNAME1 BY REFERENCE PL1REC " " RETURNING RET.
           IF  RET = 1
               GO  TO  ST-30
           END-IF.
           IF  PL1URIKB     NOT = "U"
               GO  TO  ST-20
           END-IF.
           ADD        PL1ZENKI     TO     W1-KINZE.
           ADD        PL1TOUKI     TO     W1-KINTO.
           GO TO      ST-20.
       ST-30.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
       ST-40.
      *           READ       GEN          AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" GEN_PNAME1 BY REFERENCE PL-REC " " RETURNING RET.
           IF  RET = 1
               GO TO ST-END
           END-IF.
           IF  PL-MMWK      =     0
               CALL "SD_Output" USING "DSP-011" DSP-011 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J-09" DISP-BUZ-J-09 "p"
                                               RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-011 "ACP-011" "9" "2"
                   BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF.
       ST-50.
           PERFORM  HEAD-RTN     THRU   HEAD-EXT.
           IF  PL-LIN       >      1
               MOVE    H7          TO      F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               ADD      1            TO     LINCNT
               PERFORM  HEAD-RTN     THRU   HEAD-EXT
               SUBTRACT 1            FROM   PL-LIN
               GO TO    ST-50
           END-IF.
           MOVE        H7          TO      M1.
           IF  PL-GKB       =      1
               MOVE     PL-NAM       TO     M1-NAME1
           END-IF.
           IF  PL-GKB       =      2
               MOVE     PL-NAM       TO     M1-NAME2
           END-IF.
           IF  PL-GKB       =      3
               MOVE     PL-NAM       TO     M1-NAME3
           END-IF.
           IF  PL-GKB     =      4
               MOVE     PL-NAM       TO     M1-NAME4
           END-IF.
           IF  PL-GKB       =      5
               MOVE     PL-NAM       TO     M1-NAME5
           END-IF.
           IF  PL-PKB       =      1
               GO TO  ST-60
           END-IF.
           MOVE       PL-ZENKI     TO     M1-ZENKIN.
           MOVE       PL-TOUKI     TO     M1-TOKIN.
           COMPUTE    W1-SA        =      PL-TOUKI  -  PL-ZENKI.
           MOVE       W1-SA        TO     M1-SA.
           IF  W1-KINZE     NOT =  0
               DIVIDE   W1-KINZE     INTO  PL-ZENKI
                                   GIVING  W1-RIT  ROUNDED
               MULTIPLY 100          BY    W1-RIT GIVING M1-ZENRIT
           END-IF.
           IF  W1-KINTO     NOT =  0
               DIVIDE   W1-KINTO     INTO  PL-TOUKI
                                   GIVING W1-RIT   ROUNDED
               MULTIPLY  100         BY     W1-RIT  GIVING M1-TORIT
           END-IF.
           IF  PL-TOUKI     NOT >  0
               GO  TO  ST-60
           END-IF.
           IF  PL-ZENKI     >     0
               DIVIDE   PL-ZENKI     INTO   PL-TOUKI
                                   GIVING W1-RIT   ROUNDED
               MULTIPLY 100          BY     W1-RIT  GIVING M1-SARIT
           END-IF.
       ST-60.
           MOVE       M1           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           ADD        1            TO     LINCNT.
           GO TO      ST-40.
       ST-END.
           MOVE        H8          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           PERFORM    CLSE-ENT     THRU   CLSE-EXT.
           CALL "DB_Close".
           STOP       RUN.
       HEAD-RTN.
           IF  LINCNT       <      50
               GO  TO  HEAD-EXT
           END-IF.
           IF  LINCNT       NOT =  99
               MOVE   H8           TO     F5-REC
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING F5-REC RETURNING RESP
               MOVE   SPACE        TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           ADD        1            TO     HPAGE.
           MOVE       HPAGE        TO     HPAGER.
           MOVE       HPAGER       TO     H1-PAGE.
           MOVE       H1           TO     F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H2           TO     F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H3           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H4          TO      F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H5          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H6          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H7          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       0            TO     LINCNT.
       HEAD-EXT.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
       COPY                        LPMSG_PR.
