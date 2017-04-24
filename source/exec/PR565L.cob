       IDENTIFICATION                     DIVISION.
       PROGRAM-ID.                        PR565L.
      *>=========================================================<*
      *>                                                         <*
      *>       USER     NAME.....                                <*
      *>       PROGRAM  NAME.....PR495L                          <*
      *>       PROGRAM  TITLE....部門別損益管理表（期末）        <*
      *>       AUTHOR   .........                                <*
      *>       DATE     WRITTEN.. 91/01/14                       <*
      *>                                                         <*
      *>=========================================================<*
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       SOURCE-COMPUTER.      SYSTEM100.
       OBJECT-COMPUTER.      SYSTEM100.
       INPUT-OUTPUT          SECTION.
       DATA                  DIVISION.
      ******************************************************
       WORKING-STORAGE                SECTION.
      ******************************************************
       77  ERR-STAT          PIC X(02).
       01  FLG.
           02  SYORI-FLG     PIC 9(01).
           02  READ-FLG      PIC 9(01).
           02  WRITE-FLG     PIC 9(01).
           02  INIT-FLG      PIC 9(01).
       01  W1.
           02  W-EDIT.
             03  W-EDIT-YMD.
               04  W-EDIT-MM PIC Z(02).
               04  W-EDIT-DD PIC Z(02).
             03  W-EDIT-PAGE PIC Z(04).
           02  W-YMD.
             03  W-YY        PIC 9(02).
             03  W-MM        PIC 9(02).
             03  W-DD        PIC 9(02).
           02  W-PAGE        PIC 9(04).
           02  W-PRKBN.
             03  W-PRSIN     PIC 9(01)
                 OCCURS      5   TIMES.
           02  L-CNT         PIC 9(02).
           02  IX1           PIC 9(02).
           02  IX2           PIC 9(02).
           02  MSG-IT.
             03  ERR-MSG     PIC X(30).
             03  ERR-CD      PIC X(10).
             03  ERR-ACT     PIC X(01).
           02  W-KEY.
             03  W-NEWKEY    PIC 9(02).
             03  W-OLDKEY    PIC 9(02).
       01  I                 PIC 9(02).
       01  H1.
           02  FILLER        PIC X(05)   VALUE  X"1A24212474".
           02  FILLER        PIC X(02).
           02  H1-YY         PIC N(02).
           02  FILLER        PIC N(01)   VALUE   "年".
           02  H1-MM         PIC N(02).
           02  FILLER        PIC N(01)   VALUE   "月".
           02  H1-DD         PIC N(02).
           02  FILLER        PIC N(03)   VALUE   "日作成".
           02  FILLER        PIC X(16)   VALUE   SPACE.
           02  FILLER        PIC X(02)   VALUE   X"1AC0".
           02  FILLER        PIC N(26)   VALUE
           "　部　門　別　製　造　原　価　報　告　書　（期末）　".
           02  FILLER        PIC X(03)   VALUE   X"1AC1".
           02  FILLER        PIC N(07)   VALUE  "　〈部門管理〉".
           02  FILLER        PIC X(14)   VALUE  SPACE.
           02  H1-PAGE       PIC N(04).
           02  FILLER        PIC N(01)   VALUE   "頁".
       01  H2.
           02  FILLER        PIC X(52)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "（".
           02  H2-YY         PIC N(02).
           02  FILLER        PIC N(01)   VALUE   "年".
           02  H2-MM         PIC N(02).
           02  FILLER        PIC N(03)   VALUE   "月分）".
       01  H3.
           02  FILLER        PIC X(135)  VALUE   X"1AC0".
           02  FILLER        PIC X(02)   VALUE   X"1AC1".
       01  H4.
           02  FILLER        PIC X(25)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1AC01AC2".
           02  H4-BNM1       PIC X(21).
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  H4-BNM2       PIC X(21).
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  H4-BNM3       PIC X(21).
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  H4-BNM4       PIC X(21).
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  H4-BNM5       PIC X(21).
           02  FILLER        PIC X(04)   VALUE   X"1AC11AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  H4-A    REDEFINES     H4.
           02  FILLER        PIC X(30).
           02  H4-BNMIT-A    OCCURS  5     TIMES.
             03  H4-BNM-A    PIC X(20).
             03  FILLER      PIC X(04).
           02  FILLER        PIC X(04).
       01  H5.
           02  FILLER        PIC X(09)   VALUE   X"1AC21AC0".
           02  FILLER        PIC N(01)   VALUE   "項".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "目".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "名".
           02  FILLER        PIC X(06)   VALUE   SPACE.
           02  FILLER        PIC X(05)   VALUE   X"1AC2".
           02  FILLER        PIC N(01)   VALUE   "金".
           02  FILLER        PIC X(05)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "額".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  FILLER        PIC N(04)   VALUE   "部門比　".
           02  FILLER        PIC X(05)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
           02  FILLER        PIC N(01)   VALUE   "金".
           02  FILLER        PIC X(05)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "額".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  FILLER        PIC N(04)   VALUE   "部門比　".
           02  FILLER        PIC X(05)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
           02  FILLER        PIC N(01)   VALUE   "金".
           02  FILLER        PIC X(05)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "額".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  FILLER        PIC N(04)   VALUE   "部門比　".
           02  FILLER        PIC X(05)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
           02  FILLER        PIC N(01)   VALUE   "金".
           02  FILLER        PIC X(05)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "額".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  FILLER        PIC N(04)   VALUE   "部門比　".
           02  FILLER        PIC X(05)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212474".
           02  FILLER        PIC N(01)   VALUE   "金".
           02  FILLER        PIC X(05)   VALUE   SPACE.
           02  FILLER        PIC N(01)   VALUE   "額".
           02  FILLER        PIC X(03)   VALUE   SPACE.
           02  FILLER        PIC X(03)   VALUE   X"1AC2".
           02  FILLER        PIC X(05)   VALUE   X"1A24212078".
           02  FILLER        PIC N(04)   VALUE   "部門比　".
           02  FILLER        PIC X(07)   VALUE   X"1AC21A24212474".
           02  FILLER        PIC X(02)   VALUE   X"1AC1".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  H6.
           02  FILLER        PIC X(08)   VALUE   X"1AC21A24212078".
           02  FILLER        PIC N(14)   VALUE   ALL "　".
           02  FILLER        PIC X(01)   VALUE   SPACE.
           02  FILLER        PIC X(22)   VALUE   X"1A242124741AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(02)   VALUE   X"1AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  H7.
           02  FILLER        PIC X(27)   VALUE   X"1AC01AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(17)   VALUE   X"1AC2".
           02  FILLER        PIC X(09)   VALUE   X"1AC2".
           02  FILLER        PIC X(04)   VALUE   X"1AC11AC2".
           02  FILLER        PIC N(01)   VALUE   "　".
       01  ME1.
           02  FILLER        PIC X(08).
           02  ME1-NMIT1.
             03  ME1-NM1     PIC N(10).
             03  FILLER      PIC X(08).
           02  ME1-NMIT2     REDEFINES   ME1-NMIT1.
             03  FILLER      PIC X(02).
             03  ME1-NM2     PIC N(10).
             03  FILLER      PIC X(06).
           02  ME1-NMIT3     REDEFINES   ME1-NMIT1.
             03  FILLER      PIC X(04).
             03  ME1-NM3     PIC N(10).
             03  FILLER      PIC X(04).
           02  ME1-NMIT4     REDEFINES   ME1-NMIT1.
             03  FILLER      PIC X(06).
             03  ME1-NM4     PIC N(10).
             03  FILLER      PIC X(02).
           02  ME1-NMIT5     REDEFINES   ME1-NMIT1.
             03  FILLER      PIC X(08).
             03  ME1-NM5     PIC N(10).
           02  FILLER        PIC X(06).
           02  ME1-AMTIT   OCCURS  5   TIMES.
             03  FILLER      PIC X(02).
             03  ME1-AMT     PIC ---,---,---,--9.
             03  FILLER      PIC X(02).
             03  ME1-AMTRT   PIC ----.99.
           02  FILLER        PIC X(04).
       COPY                  LWMSG_PR.
       COPY                  BUGEN.
       COPY                  BUMONF.
       COPY                  BGNHAI.
       COPY                  FCTL.
       COPY                  BGNPRN.
       COPY                  LGENKF.
       77  PR-REC            PIC  X(250).
       77  USER_ID           PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE   PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER     PIC  9(003).
       77  ESTAT             PIC  X(002).
       77  RESU              PIC  9(001).
       77  RESP              PIC  9(001).
       77  RET               PIC  9(001) VALUE ZERO.
       01  DISP-BUZZER.
           02  DISP-BUZ-J-05 PIC X(05) VALUE X"1B4A05".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-011   PIC  X(01) VALUE "(".
               05  DSP-012   PIC  X(10).
               05  DSP-013   PIC  X(04) VALUE ")   ".
               05  DSP-014   PIC  X(30).
       01  ACP-AREA.
           03  ACP-010.
               05  ACP-011   PIC  X(01).
       COPY                  LSMSG_PR.
      ******************************************************
       PROCEDURE                      DIVISION.
      ******************************************************
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "PR-999" RETURNING RESP.
      *       01  DISP-BUZZER    LINE  24.
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "5" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J-05" "X" "24" "80" "5" " " "DISP-BUZZER"
      *       01  DSP-AREA.
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "45" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "1" "0" "45" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "X" "1" "1" "1" " " "DSP-010"  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-012" "X" "1" "2" "10" "DSP-011" " "  RETURNING RESU.
       CALL "SD_From" USING
            "DSP-012" BY REFERENCE ERR-CD "10" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-013" "X" "1" "12" "4" "DSP-012" " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-014" "X" "1" "16" "30" "DSP-013" " "  RETURNING RESU.
       CALL "SD_From" USING
            "DSP-014" BY REFERENCE ERR-MSG "30" "0" RETURNING RESU.
      *        01  ACP-AREA.
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" " " "1" "0" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-011" "X" "1" "50" "1" " " "ACP-010"  RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-011" BY REFERENCE ERR-ACT "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ENTRY-RTN                      SECTION.
         ENTRY-000.
           PERFORM INIT-RTN.
           PERFORM MAIN-RTN        UNTIL   SYORI-FLG   =   0   OR  3.
           PERFORM CLSE-ENT        THRU    CLSE-EXT.
           CALL "DB_Close".
           STOP    RUN.
         ENTRY-999.
           EXIT.
       INIT-RTN                    SECTION.
         INIT-000.
           INITIALIZE      W1.
           CALL "DB_F_Open" USING
            "OUTPUT" BGNPRN_PNAME1 " " BY REFERENCE BGNPRN_IDLST "1"
            "BPLPRN-KEY" BY REFERENCE BPLPRN-KEY.
           CALL "DB_F_Open" USING
            "I-O" GEN_PNAME1 "SHARED" BY REFERENCE GEN_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           MOVE    0       TO      READ-FLG.
           PERFORM PLCLR-RTN       UNTIL   READ-FLG    =   1.
           CALL "DB_F_Open" USING
            "INPUT SEQUENTIAL" BUGEN-F_PNAME1 "SHARED" BY REFERENCE 
            BUGEN-F_IDLST "1" "BU-KEY" BY REFERENCE BU-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BGNHAI_PNAME1 "SHARED" BY REFERENCE BGNHAI_IDLST "1"
            "BGNHAI-KEY" BY REFERENCE BGNHAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           CALL "DB_F_Open" USING
            "I-O" BGNPRN_PNAME1 " " BY REFERENCE BGNPRN_IDLST "1"
            "BPLPRN-KEY" BY REFERENCE BPLPRN-KEY.
           CALL "DB_F_Open" USING
            "I-O" GEN_PNAME1 "SHARED" BY REFERENCE GEN_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           CALL "PR_Open" RETURNING RESP.
           IF  WRITE-FLG   =   1
               PERFORM ERR-RTN
               GO  TO  INIT-999
           END-IF.
           PERFORM FCTL-READ-RTN.
           MOVE    FCTL-REC1       TO    Z-R.
           MOVE    Z-KONYMD        TO    ZYMD.
           PERFORM Z-RTN           THRU  Z-EXT.
           MOVE    ZI              TO    I.
           IF  I  >  15
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               PERFORM  CLSE-ENT   THRU  CLSE-EXT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "INV-CON" INV-CON "p" RETURNING RESU
               CALL "SD_Output" USING 
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           ACCEPT  W-YMD   FROM    DATE.
           MOVE    99      TO      L-CNT.
           MOVE    0       TO      W-PAGE.
           MOVE    1       TO      SYORI-FLG.
           MOVE    1       TO      INIT-FLG.
         INIT-999.
           EXIT.
       PLCLR-RTN                   SECTION.
         PLCLR-000.
      *           READ    GEN     NEXT    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" GEN_PNAME1 BY REFERENCE PL-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE    1   TO      READ-FLG
               GO  TO  PLCLR-999
           END-IF.
           MOVE    ZERO    TO      PL-YY   PL-MM.
      *           REWRITE PL-REC  INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            GEN_PNAME1 GEN_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               MOVE    "PL-K REWRITE ﾌﾉｳ"  TO  ERR-MSG
               MOVE    PL-KEY      TO   ERR-CD
               MOVE    1      TO   WRITE-FLG   READ-FLG
           END-IF.
         PLCLR-999.
           EXIT.
       FCTL-READ-RTN               SECTION.
         FCTL-READ-000.
           MOVE    "DATE  "    TO  FCTL-KEY.
      *           READ    FCTL-F      INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               PERFORM CLSE-ENT   THRU      CLSE-EXT
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "INV-CON" INV-CON "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
         FCTL-READ-999.
           EXIT.
       MAIN-RTN                    SECTION.
         MAIN-000.
           PERFORM INP-RTN.
           PERFORM OUT-RTN.
         MAIN-999.
           EXIT.
       INP-RTN                     SECTION.
         INP-000.
           IF  SYORI-FLG   =   1
               PERFORM BUPL-READ-RTN
           END-IF.
           IF  SYORI-FLG   =   2
               PERFORM BPRN-SREAD-RTN
           END-IF.
         INP-999.
           EXIT.
       BUPL-READ-RTN               SECTION.
         BUPL-READ-000.
      *           READ    BUGEN-F    AT  END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" BUGEN-F_PNAME1 BY REFERENCE BU-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE    2      TO   SYORI-FLG
               PERFORM BPRN-START-RTN
           END-IF.
         BUPL-READ-999.
           EXIT.
       BPRN-START-RTN              SECTION.
         BPRN-START-000.
           MOVE    LOW-VALUE  TO   BPLPRN-KEY.
      *           START   BGNPRN     KEY  NOT <  BPLPRN-KEY INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            BGNPRN_PNAME1 "BPLPRN-KEY" " NOT < " BPLPRN-KEY
            RETURNING RET.
           IF  RET = 1
               MOVE  "ｻｸﾋｮｳ ﾌｧｲﾙ START INVALID" TO ERR-MSG
               MOVE  SPACE    TO   ERR-CD
               PERFORM ERR-RTN
               MOVE  0        TO   SYORI-FLG
           END-IF.
         BPRN-START-999.
           EXIT.
       BPRN-SREAD-RTN              SECTION.
         BPRN-SREAD-000.
      *           READ    BGNPRN  NEXT    AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BGNPRN_PNAME1 BY REFERENCE BPLPRN-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE    BPLPRN-PG   TO      W-NEWKEY
           END-IF.
         BPRN-SREAD-999.
           EXIT.
       OUT-RTN                     SECTION.
         OUT-000.
           IF  SYORI-FLG   =  1
               PERFORM UPDATE-RTN
           END-IF.
           IF  SYORI-FLG   =  2
               PERFORM LIST-RTN
           END-IF.
           IF  SYORI-FLG   =  3
               IF  L-CNT   NOT =  99
                   MOVE H7 TO PR-REC
                   CALL "PR_LineFeed" USING "1" RETURNING RESP
                   CALL "PR_Write" USING PR-REC RETURNING RESP
               END-IF
           END-IF.
         OUT-999.
           EXIT.
       UPDATE-RTN                  SECTION.
         UPDATE-000.
           PERFORM PL-UP-RTN.
           IF  WRITE-FLG       =   2
               MOVE    3       TO  SYORI-FLG
               GO  TO  UPDATE-999
           END-IF.
           MOVE    BU-BUMN     TO  BNM-KEY.
           PERFORM BNM-READ-RTN.
           IF  READ-FLG   =    1
               GO  TO  UPDATE-999
           END-IF.
           PERFORM BPRN-UP-RTN     VARYING  IX1 FROM 1 BY 1
                   UNTIL IX1  >    3.
         UPDATE-999.
           EXIT.
       PL-UP-RTN                   SECTION.
         PL-UP-000.
           MOVE  0            TO   READ-FLG WRITE-FLG.
           MOVE  BU-LINNO     TO   PL-KEY.
      *           READ  GEN     INVALID   KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" GEN_PNAME1 BY REFERENCE PL-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  "ｿﾝｴｷ ﾌｧｲﾙ INVALID" TO  ERR-MSG
               MOVE  PL-KEY   TO   ERR-CD
               PERFORM ERR-RTN
               MOVE 2         TO   READ-FLG
               GO  TO  PL-UP-999
           END-IF.
           ADD   BU-TOUKI     TO   PL-TOUKI.
      *           REWRITE  PL-REC    INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            GEN_PNAME1 GEN_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               MOVE "ｿﾝｴｷ ﾌｧｲﾙ REWRIT ﾌﾉｳ" TO  ERR-MSG
               MOVE  PL-KEY   TO   ERR-CD
               PERFORM ERR-RTN
               MOVE  2        TO   WRITE-FLG
           END-IF.
       PL-UP-999.
           EXIT.
       BNM-READ-RTN                SECTION.
         BNM-READ-000.
           MOVE     0         TO   READ-FLG.
      *           READ     BNM       INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1        TO   READ-FLG
           END-IF.
         BNM-READ-999.
           EXIT.
       BPRN-UP-RTN                 SECTION.
         BPRN-UP-000.
           IF  BNM-GNPG (IX1)  NOT =  ZERO
               PERFORM BPRN-RREAD-RTN
               PERFORM BPRN-EDIT-RTN
               PERFORM BPRN-OUT-RTN
           END-IF.
         BPRN-UP-999.
           EXIT.
       BPRN-RREAD-RTN              SECTION.
         BPRN-RREAD-000.
           MOVE     0         TO   READ-FLG.
           MOVE     BNM-GNPG (IX1) TO  BPLPRN-PG.
           MOVE     BU-LINNO       TO  BPLPRN-LNO.
      *           READ     BGNPRN    INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BGNPRN_PNAME1 BY REFERENCE BPLPRN-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE    1   TO  READ-FLG
           END-IF.
         BPRN-RREAD-999.
           EXIT.
       BPRN-EDIT-RTN               SECTION.
         BPRN-EDIT-000.
           IF  READ-FLG    =   1
               INITIALIZE  BPLPRN-REC
               MOVE    BNM-GNPG (IX1)  TO  BPLPRN-PG
               MOVE    BU-LINNO        TO  BPLPRN-LNO
               MOVE    BU-KAIP         TO  BPLPRN-GYO
               MOVE    BU-GOKBN        TO  BPLPRN-GKB
               MOVE    BU-KMKNM        TO  BPLPRN-NM
               MOVE    BU-URKBN        TO  BPLPRN-UKB
               MOVE    BU-PRKBN        TO  BPLPRN-IKB
           END-IF.
           MOVE    BNM-GNLN (IX1) TO  IX2.
           ADD     BU-TOUKI    TO  BPLPRN-AM (IX2).
         BPRN-EDIT-999.
           EXIT.
       BPRN-OUT-RTN                SECTION.
         BPRN-OUT-000.
           IF  READ-FLG    =   1
               PERFORM BPRN-WR-RTN
           ELSE
               PERFORM BPRN-RWR-RTN
           END-IF.
         BPRN-OUT-999.
           EXIT.
       BPRN-WR-RTN                SECTION.
         BPRN-WR-000.
      *           WRITE    BPLPRN-REC    INVALID KEY
      *///////////////
           CALL "DB_Insert" USING 
            BGNPRN_PNAME1 BGNPRN_LNAME BPLPRN-REC RETURNING RET.
           IF  RET = 1
               MOVE "BPLPRN WRITE ﾌﾉｳ" TO ERR-MSG
               MOVE BPLPRN-KEY TO ERR-CD
               PERFORM ERR-RTN
               MOVE 1    TO   WRITE-FLG
           END-IF.
        BPRN-WR-999.
           EXIT.
       BPRN-RWR-RTN               SECTION.
         BPRN-RWR-000.
      *           REWRITE   BPLPRN-REC    INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BGNPRN_PNAME1 BGNPRN_LNAME BPLPRN-REC RETURNING RET.
           IF  RET = 1
               MOVE "BPLPRN RWRITE ﾌﾉｳ" TO ERR-MSG
               MOVE BPLPRN-KEY TO ERR-CD
               PERFORM ERR-RTN
               MOVE 1    TO   WRITE-FLG
           END-IF.
        BPRN-WR-999.
           EXIT.
       LIST-RTN                   SECTION.
         LIST-000.
           IF  INIT-FLG   =   1
               MOVE    W-NEWKEY   TO  W-OLDKEY
               MOVE    0          TO  INIT-FLG
           END-IF.
           IF  W-NEWKEY   NOT =   W-OLDKEY
               MOVE    W-NEWKEY   TO  W-OLDKEY
               MOVE    60         TO  L-CNT
           END-IF.
           PERFORM HEAD-RTN.
           PERFORM MEISAI-RTN.
         LIST-999.
           EXIT.
       HEAD-RTN                   SECTION.
         HEAD-000.
           IF  L-CNT   >   49
               PERFORM HEAD-EDIT-RTN
               PERFORM HEAD-PR-RTN
           END-IF.
        HEAD-999.
           EXIT.
       HEAD-EDIT-RTN              SECTION.
         HEAD-EDIT-000.
           MOVE    W-MM    TO     W-EDIT-MM.
           MOVE    W-DD    TO     W-EDIT-DD.
           MOVE    W-YY       TO  H1-YY.
           MOVE    W-EDIT-MM  TO  H1-MM.
           MOVE    W-EDIT-DD  TO  H1-DD.
           MOVE   Z-TOUTMM(I) TO  W-EDIT-MM.
           MOVE   Z-TOUTYY2(I) TO  H2-YY.
           MOVE    W-EDIT-MM  TO  H2-MM.
           ADD     1          TO  W-PAGE.
           MOVE    W-PAGE     TO  W-EDIT-PAGE.
           MOVE    W-EDIT-PAGE TO H1-PAGE.
           PERFORM BNM-EDIT-RTN   VARYING  IX1 FROM 1 BY 1
                   UNTIL  IX1  >  5.
         HEAD-EDIT-999.
           EXIT.
       BNM-EDIT-RTN               SECTION.
         BNM-EDIT-000.
           MOVE    0       TO  W-PRSIN (IX1).
           MOVE    BPLPRN-PG   TO BGNHAI-PG.
           MOVE    IX1     TO  BGNHAI-LN.
      *           READ    BGNHAI  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BGNHAI_PNAME1 BY REFERENCE BGNHAI-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE    SPACE   TO  H4-BNM-A (IX1)
               MOVE    1       TO  W-PRSIN  (IX1)
               GO  TO  BNM-EDIT-999
           END-IF.
           MOVE    BGNHAI-BUCD TO  BNM-KEY.
      *           READ    BNM     INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE    SPACE   TO  H4-BNM-A (IX1)
               MOVE    1       TO  W-PRSIN  (IX1)
               GO  TO  BNM-EDIT-999
           END-IF.
           MOVE    BNMNM       TO  H4-BNM-A (IX1).
         BNM-EDIT-999.
           EXIT.
       HEAD-PR-RTN                SECTION.
         HEAD-PR-000.
           IF  L-CNT   NOT =  99
               MOVE H7 TO PR-REC
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PR-REC RETURNING RESP
               MOVE    SPACE   TO     PR-REC
               CALL "PR_Write" USING PR-REC RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           MOVE H1 TO PR-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           MOVE H2 TO PR-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           MOVE H3 TO PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           MOVE H4 TO PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           MOVE H5 TO PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           MOVE H6 TO PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           MOVE    0       TO      L-CNT.
         HEAD-PR-999.
           EXIT.
       MEISAI-RTN                 SECTION.
         MEISAI-000.
           PERFORM ME-EDIT-RTN.
           PERFORM ME-PR-RTN.
         MEISAI-999.
           EXIT.
       ME-EDIT-RTN                SECTION.
         ME-EDIT-000.
           MOVE    H6      TO  ME1.
           IF  BPLPRN-GKB  =  1
               MOVE    BPLPRN-NM   TO  ME1-NM1
           END-IF.
           IF  BPLPRN-GKB  =  2
               MOVE    BPLPRN-NM   TO  ME1-NM2
           END-IF.
           IF  BPLPRN-GKB  =  3
               MOVE    BPLPRN-NM   TO  ME1-NM3
           END-IF.
           IF  BPLPRN-GKB  =  4
               MOVE    BPLPRN-NM   TO  ME1-NM4
           END-IF.
           IF  BPLPRN-GKB  =  5
               MOVE    BPLPRN-NM   TO  ME1-NM5
           END-IF.
           IF  BPLPRN-IKB  NOT =   1
               PERFORM AMT-EDIT-RTN    VARYING  IX1 FROM 1 BY 1
                       UNTIL   IX1 >   5
           END-IF.
         ME-EDIT-999.
           EXIT.
       AMT-EDIT-RTN            SECTION.
         AMT-EDIT-000.
           IF  W-PRSIN (IX1) =   0
               MOVE    BPLPRN-AM (IX1) TO  ME1-AMT (IX1)
               PERFORM RITU-RTN
           END-IF.
         AMT-EDIT-999.
           EXIT.
       RITU-RTN                SECTION.
         RITU-000.
           MOVE    BPLPRN-LNO  TO  PL-KEY.
      *           READ    GEN         INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" GEN_PNAME1 BY REFERENCE PL-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  RITU-999
           END-IF.
           IF  PL-TOUKI    NOT =   0
             COMPUTE ME1-AMTRT (IX1) ROUNDED =   BPLPRN-AM (IX1) * 100
                                                          /   PL-TOUKI
           ELSE
               MOVE    0        TO  ME1-AMTRT (IX1)
           END-IF.
         RITU-999.
           EXIT.
       ME-PR-RTN               SECTION.
         ME-PR-000.
           IF  BPLPRN-GYO  >   1
               MOVE H6 TO PR-REC
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PR-REC RETURNING RESP
               ADD     1      TO    L-CNT
               SUBTRACT 1     FROM  BPLPRN-GYO
               PERFORM HEAD-RTN
               GO  TO  ME-PR-000
           END-IF.
           MOVE ME1 TO PR-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PR-REC RETURNING RESP.
           ADD     1       TO    L-CNT.
         ME-PR-999.
           EXIT.
       ERR-RTN                 SECTION.
         ERR-000.
           CALL "SD_Output" USING
           "DSP-010" DSP-010 "p" RETURNING RESU.
           CALL "SD_Output" USING 
           "DISP-BUZ-J-05" DISP-BUZ-J-05 "p" RETURNING RESU.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-010 "ACP-010" " " "1"
               BY REFERENCE ESTAT RETURNING RESU.
         ERR-999.
           EXIT.
       CLSE-ENT.
           CALL "DB_F_Close" USING
            BY REFERENCE BUGEN-F_IDLST BUGEN-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BGNHAI_IDLST BGNHAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BGNPRN_IDLST BGNPRN_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-EXT.
           EXIT.
       COPY  LPMSG_PR.
