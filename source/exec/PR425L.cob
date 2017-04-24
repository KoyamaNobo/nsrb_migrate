       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR425L.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  H1.
           02  P-K             PIC X(05)   VALUE   X"1A24212474".
           02  FILLER          PIC X(2)    VALUE   SPACE.
           02  H1-YY           PIC N(2).
           02  FILLER          PIC N(1)    VALUE   "年".
           02  H1-MM           PIC N(2).
           02  FILLER          PIC N(1)    VALUE   "月".
           02  H1-DD           PIC N(2).
           02  FILLER          PIC N(3)    VALUE   "日作成".
           02  FILLER          PIC X(25)   VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC0".
           02  FILLER          PIC N(10)   VALUE
                            "損　益　計　算　書　".
           02  FILLER          PIC N(05)   VALUE   "（月次）".
           02  FILLER          PIC X(24)   VALUE   X"1AC1".
           02  H1-PAGE         PIC N(4).
           02  FILLER          PIC N(1)    VALUE   "頁".
       01  H2.
           02  FILLER          PIC X(53)   VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "（".
           02  H2-YY           PIC N(2).
           02  FILLER          PIC N(1)    VALUE   "年".
           02  H2-MM           PIC N(2).
           02  FILLER          PIC N(3)    VALUE   "月分）".
       01  H3.
           02  FILLER          PIC X(62)   VALUE   X"1AC0".
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
       01  H4.
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(8)    VALUE   X"1AC01AC2".
           02  FILLER          PIC N(1)    VALUE   "当".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "月".
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
           02  FILLER          PIC X(4)    VALUE   X"1AC21AC1".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H7.
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(19)   VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H8.
           02  FILLER          PIC X(38)   VALUE   X"1AC01AC2".
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
       01  HYMDIN.
           02  HYYIN           PIC 9(2).
           02  HMMIN           PIC 9(2).
           02  HDDIN           PIC 9(2).
       01  HYMDIN-W.
           02  HMMIN-W         PIC Z(2).
           02  HDDIN-W         PIC Z(2).
       01  HYMDX.
           02  HYYX            PIC X(2).
           02  HMMX            PIC X(2).
           02  HDDX            PIC X(2).
        01  HYMDOUT.
           02  HYYOUT          PIC N(2).
           02  HMMOUT          PIC N(2).
           02  HDDOUT          PIC N(2).
       01  HPAGE               PIC 9(4)    VALUE 0.
       01  HPAGER              PIC ZZZ9.
       01  HPAGEX              PIC X(4).
       01  HPAGEOUT            PIC N(4).
       01  W1.
           02  LINCNT          PIC 9(2)    VALUE 99.
           02  W1-KIN.
             03  W1-KINTO      PIC S9(11).
           02  W1-RIT          PIC S9(4)V9(4).
           02  W1-YMIN.
             03  W1-YYIN       PIC 9(2).
             03  W1-MMIN       PIC 9(2).
           02  W1-IN           PIC X(1).
       01  ERR-STAT            PIC X(2).
      *
       COPY    PL-LIB.
      *       FD  F5  LABEL   RECORD  OMITTED
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
           02  DISP-BUZ-J          PIC X(05) VALUE X"1B4A09".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-011         PIC  X(06) VALUE  "ﾂｷ ｴﾗｰ".
       01  ACP-AREA.
           03  ACP-010.
               05  ACP-011         PIC  9(02).
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "F5-999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-BUZZER
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "22" "0" "6" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "X" "22" "10" "6" " " "DSP-010" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" " " "22" "0" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-011" "9" "2" "50" "2" " " "ACP-010" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-011" BY REFERENCE LINCNT "2" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           MOVE       SPACE        TO    F5-REC.
           ACCEPT     HYMDIN       FROM  DATE.
           MOVE       HMMIN        TO    HMMIN-W.
           MOVE       HDDIN        TO    HDDIN-W.
           MOVE       HYYIN        TO    HYYX.
           MOVE       HMMIN-W      TO    HMMX.
           MOVE       HDDIN-W      TO    HDDX.
           MOVE       HYYX         TO    HYYOUT.
           MOVE       HMMX         TO    HMMOUT.
           MOVE       HDDX         TO    HDDOUT.
           MOVE       HYYOUT       TO    H1-YY.
           MOVE       HMMOUT       TO    H1-MM.
           MOVE       HDDOUT       TO    H1-DD.
           CALL "DB_F_Open" USING
            "INPUT" PL_PNAME1 "SHARED" BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
           CALL "PR_Open" RETURNING RESP.
           MOVE       ZERO         TO     W1-KIN.
       ST-20.
      *           READ       PL           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" PL_PNAME1 BY REFERENCE PL-REC " " RETURNING RET.
           IF  RET = 1
               GO  TO  ST-30
           END-IF.
           IF  PL-URIKB     NOT = "U"
               GO  TO  ST-20
           END-IF.
           ADD        PL-TOUMM     TO     W1-KINTO.
           GO TO      ST-20.
       ST-30.
           CALL "DB_F_Close" USING BY REFERENCE PL_IDLST PL_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" PL_PNAME1 "SHARED" BY REFERENCE PL_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
       ST-40.
      *           READ       PL           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" PL_PNAME1 BY REFERENCE PL-REC " " RETURNING RET.
           IF  RET = 1
               GO TO ST-END
           END-IF.
           IF  PL-MMWK      =     0
               CALL "SD_Output" USING "DSP-011" DSP-011 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                             RETURNING RESU
               CALL "SD_Accept" USING
                     BY REFERENCE ACP-011 "ACP-011" "9" "2"
                     BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF.
           MOVE       PL-YYWK      TO     HYYIN.
           MOVE       PL-MMWK      TO     HMMIN.
           MOVE       HMMIN        TO     HMMIN-W.
           MOVE       HYYIN        TO     HYYX.
           MOVE       HMMIN-W      TO     HMMX.
           MOVE       HYYX         TO     HYYOUT.
           MOVE       HMMX         TO     HMMOUT.
           MOVE       HYYOUT       TO     H2-YY.
           MOVE       HMMOUT       TO     H2-MM.
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
           MOVE       PL-TOUMM     TO     M1-TOKIN.
           IF  W1-KINTO     NOT =  0
               DIVIDE   W1-KINTO     INTO  PL-TOUMM
                                     GIVING W1-RIT   ROUNDED
               MULTIPLY  100         BY     W1-RIT  GIVING M1-TORIT
           END-IF.
           IF  PL-TOUMM     NOT >  0
               GO  TO  ST-60
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
           CALL "PR_Close" RETURNING RESP.
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
           MOVE       HPAGER       TO     HPAGEX.
           MOVE       HPAGEX       TO     HPAGEOUT.
           MOVE       HPAGEOUT     TO     H1-PAGE.
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
