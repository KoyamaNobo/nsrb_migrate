       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR415L.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  H1.
           02  P-K             PIC X(05)  VALUE    X"1A24212474".
           02  FILLER          PIC X(02)   VALUE   SPACE.
           02  H1-YY           PIC N(02).
           02  FILLER          PIC N(01)   VALUE   "年".
           02  H1-MM           PIC N(02).
           02  FILLER          PIC N(01)   VALUE   "月".
           02  H1-DD           PIC N(02).
           02  FILLER          PIC N(03)   VALUE   "日作成".
           02  FILLER          PIC X(26)   VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC0".
           02  FILLER          PIC N(11)   VALUE
                               "　貸　借　対　照　表　".
           02  FILLER          PIC X(42)   VALUE   X"1AC1".
           02  H1-PAGE         PIC N(04).
           02  FILLER          PIC N(01)   VALUE   "頁".
       01  H2.
           02  FILLER          PIC X(48)   VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "（".
           02  H2-YY           PIC N(2).
           02  FILLER          PIC N(1)    VALUE   "年".
           02  H2-MM           PIC N(2).
           02  FILLER          PIC N(1)    VALUE   "月".
           02  H2-DD           PIC N(2).
           02  FILLER          PIC N(4)    VALUE   "日現在）".
       01  H3.
           02  FILLER          PIC X(124)  VALUE   X"1AC0".
           02  FILLER          PIC X(2)    VALUE   X"1AC1".
       01  H4.
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(20)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(20)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H5.
           02  FILLER          PIC X(10)   VALUE  X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "借".
           02  FILLER          PIC X(14)   VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "方".
           02  FILLER          PIC X(8)    VALUE   SPACE.
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "金".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "額".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC2".
           02  FILLER          PIC N(3)    VALUE   "構成比".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(10)   VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "貸".
           02  FILLER          PIC X(14)   VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "方".
           02  FILLER          PIC X(8)    VALUE   SPACE.
           02  FILLER          PIC X(6)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "金".
           02  FILLER          PIC X(6)    VALUE   SPACE.
           02  FILLER          PIC N(1)    VALUE   "額".
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  FILLER          PIC X(4)    VALUE   X"1AC2".
           02  FILLER          PIC N(3)    VALUE   "構成比".
           02  FILLER          PIC X(1)    VALUE   SPACE.
           02  FILLER          PIC X(2)    VALUE   X"1AC2".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  H6.
           02  FILLER          PIC X(38)   VALUE   X"1AC01AC2".
           02  FILLER          PIC X(20)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(36)   VALUE   X"1AC2".
           02  FILLER          PIC X(20)   VALUE   X"1AC2".
           02  FILLER          PIC X(11)   VALUE   X"1AC2".
           02  FILLER          PIC X(4)    VALUE   X"1AC21AC1".
           02  FILLER          PIC N(1)    VALUE   "　".
       01  M1.
           02  FILLER          PIC X(3)    VALUE   SPACE.
           02  M1-NMDR1.
               03  FILLER      PIC X(2).
               03  M1-NMDR2    PIC X(31).
           02  M1-ITEM1        REDEFINES   M1-NMDR1.
               03  FILLER      PIC X(6).
               03  M1-NMDR3    PIC X(27).
           02  M1-ITEM2        REDEFINES   M1-NMDR1.
               03  FILLER      PIC X(10).
               03  M1-NMDR4    PIC X(23).
           02  M1-ITEM3        REDEFINES   M1-NMDR1.
               03  FILLER      PIC X(12).
               03  M1-NMDR5    PIC X(21).
           02  FILLER          PIC X(3)    VALUE   SPACE.
           02  M1-KINDR        PIC ----,---,---,--9.
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  M1-HIDR         PIC ----.99.
           02  FILLER          PIC X(4)    VALUE   SPACE.
           02  M1-NMCR1.
               03  FILLER      PIC X(2).
               03  M1-NMCR2    PIC X(31).
           02  M1-ITEM4        REDEFINES   M1-NMCR1.
               03  FILLER      PIC X(6).
               03  M1-NMCR3    PIC X(27).
           02  M1-ITEM5        REDEFINES   M1-NMCR1.
               03  FILLER      PIC X(10).
               03  M1-NMCR4    PIC X(23).
           02  M1-ITEM6        REDEFINES   M1-NMCR1.
               03  FILLER      PIC X(12).
               03  M1-NMCR5    PIC X(21).
           02  FILLER          PIC X(3).
           02  M1-KINCR        PIC ----,---,---,--9.
           02  FILLER          PIC X(4).
           02  M1-HICR         PIC ----.99.
           02  FILLER          PIC X(5).
       01  W1.
           02  LINCNT          PIC 99      VALUE 70.
           02  W1-IN           PIC X.
           02  INIT-SW         PIC X(3)    VALUE "OFF".
           02  W1-KEI.
             03  W1-KEIDR      PIC S9(12).
             03  W1-KEICR      PIC S9(12).
           02  W1-RITU         PIC S999V9999.
       01  SYMD.
           02  SYY             PIC 9(02).
           02  SMM             PIC 9(02).
           02  SDD             PIC 9(02).
       01  HYMDIN.
           02  HYYIN           PIC 99.
           02  HMMIN           PIC 99.
           02  HDDIN           PIC 99.
       01  HYMDIN-W.
           02  HMMIN-W         PIC ZZ.
           02  HDDIN-W         PIC ZZ.
       01  HPAGE               PIC 9(4)    VALUE 0.
       01  HPAGER              PIC ZZZ9.
       01  ERR-STAT            PIC X(02).
      *
       COPY    LWMSG_PR.
       COPY    BS-LIB.
       COPY    FCTL.
      *
      *       FD  F5  LABEL   RECORD  OMITTED
       77  F5-REC                  PIC X(200).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-012       PIC  X(14) VALUE  "000 ｺｰﾄﾞ ﾐﾄｳﾛｸ".
       01  ACP-AREA.
           03  ACP-010.
               05  ACP-011       PIC  X(01).
       COPY  LSMSG_PR.
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "F5-999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "1" "0" "14" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-012" "X" "1" "5" "14" " " "DSP-010" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" " " "1" "0" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-011" "X" "1" "30" "1" " " "ACP-010" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-011" BY REFERENCE W1-IN "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
      *=======< ｺﾝﾄﾛｰﾙ ﾌｧｲﾙ  READ >=======*
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE   "DATE  "   TO   FCTL-KEY1.
      *           READ   FCTL-F    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1
               CALL "PR_Close" RETURNING RESP
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE    FCTL-KONYY2   TO   HYYIN.
           MOVE    FCTL-KONMM    TO   HMMIN.
           MOVE    FCTL-KONDD    TO   HDDIN.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           MOVE    HMMIN         TO   HMMIN-W.
           MOVE    HDDIN         TO   HDDIN-W.
           MOVE        HYYIN       TO      H2-YY.
           MOVE        HMMIN-W     TO      H2-MM.
           MOVE        HDDIN-W     TO      H2-DD.
           CALL "DB_F_Open" USING
            "INPUT" BS_PNAME1 "SHARED" BY REFERENCE BS_IDLST "1"
            "BS-KEY" BY REFERENCE BS-KEY.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT     SYMD         FROM    DATE.
           MOVE        SYMD        TO      HYMDIN.
           MOVE        HMMIN       TO      HMMIN-W.
           MOVE        HDDIN       TO      HDDIN-W.
           MOVE        HYYIN       TO      H1-YY.
           MOVE        HMMIN-W     TO      H1-MM.
           MOVE        HDDIN-W     TO      H1-DD.
       ST-20.
      *           READ       BS           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" BS_PNAME1 BY REFERENCE BS-REC " " RETURNING RET.
           IF  RET = 1
               GO TO ST-END
           END-IF.
           IF  INIT-SW      =      "ON "
               GO TO ST-30
           END-IF.
           IF  BS-KEY       NOT =  "000"
               CALL "SD_Output" USING "DSP-012" DSP-012 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                             RETURNING RESU
               CALL "SD_Accept" USING
                     BY REFERENCE ACP-011 "ACP-011" "X" "1"
                     BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF.
           MOVE       BS-KINDR     TO     W1-KEIDR.
           MOVE       BS-KINCR     TO     W1-KEICR.
           MOVE       "ON "        TO     INIT-SW.
           GO TO      ST-20.
       ST-30.
           PERFORM  HEAD-RTN     THRU   HEAD-EXT.
           MOVE       H4           TO     M1.
           IF  BS-LIN       >      1
               MOVE     H4           TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               ADD      1            TO     LINCNT
               PERFORM  HEAD-RTN     THRU   HEAD-EXT
               SUBTRACT 1            FROM   BS-LIN
               GO TO    ST-30
           END-IF.
           IF  BS-GKBDR     =      1
               MOVE     BS-NAMDR     TO     M1-NMDR1
           END-IF.
           IF  BS-GKBDR     =      2
               MOVE     BS-NAMDR     TO     M1-NMDR2
           END-IF.
           IF  BS-GKBDR     =      3
               MOVE     BS-NAMDR     TO     M1-NMDR3
           END-IF.
           IF  BS-GKBDR     =      4
               MOVE     BS-NAMDR     TO     M1-NMDR4
           END-IF.
           IF  BS-GKBDR     =      5
               MOVE     BS-NAMDR     TO     M1-NMDR5
           END-IF.
           IF  BS-PKBDR     =      1
               GO TO ST-40
           END-IF.
           MOVE       BS-KINDR     TO     M1-KINDR.
           IF  W1-KEIDR     NOT =  0
               DIVIDE   W1-KEIDR     INTO   BS-KINDR
                                     GIVING W1-RITU  ROUNDED
               MULTIPLY 100          BY     W1-RITU  GIVING  M1-HIDR
           END-IF.
       ST-40.
           IF  BS-GKBCR     =      1
               MOVE     BS-NAMCR     TO     M1-NMCR1
           END-IF.
           IF  BS-GKBCR     =      2
               MOVE     BS-NAMCR     TO     M1-NMCR2
           END-IF.
           IF  BS-GKBCR     =      3
               MOVE     BS-NAMCR     TO     M1-NMCR3
           END-IF.
           IF  BS-GKBCR     =      4
               MOVE     BS-NAMCR     TO     M1-NMCR4
           END-IF.
           IF  BS-GKBCR     =      5
               MOVE     BS-NAMCR     TO     M1-NMCR5
           END-IF.
           IF  BS-PKBCR     =      1
               GO TO ST-50
           END-IF.
           MOVE       BS-KINCR     TO     M1-KINCR.
           IF  W1-KEICR     NOT =  0
               DIVIDE   W1-KEICR     INTO   BS-KINCR
                                     GIVING W1-RITU  ROUNDED
               MULTIPLY 100          BY     W1-RITU  GIVING  M1-HICR
           END-IF.
       ST-50.
           MOVE       M1           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           ADD        1            TO     LINCNT.
           GO TO      ST-20.
       ST-END.
           MOVE        H6          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_Close".
           STOP       RUN.
       HEAD-RTN.
           IF  LINCNT       <      50
               GO  TO  HEAD-EXT
           END-IF.
           ADD        1            TO     HPAGE.
           MOVE       HPAGE        TO     HPAGER.
           MOVE       HPAGER       TO     H1-PAGE.
           IF  LINCNT NOT   =      70
               MOVE     H6           TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               MOVE     SPACE        TO     F5-REC
               CALL "PR_Write" USING F5-REC RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF.
           MOVE       H1           TO     F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H2           TO     F5-REC.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       H3           TO     F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H4          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H5          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE        H6          TO      F5-REC.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
           MOVE       0            TO     LINCNT.
           MOVE        H4          TO      F5-REC.
           CALL "PR_Write" USING F5-REC RETURNING RESP.
       HEAD-EXT.
           EXIT.
