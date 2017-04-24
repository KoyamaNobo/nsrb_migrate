       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PR510U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM100.
       OBJECT-COMPUTER.    NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       77  OKC                 PIC X(01).
       01  W1.
           02  I               PIC 9(2).
           02  W1-WKKIN        PIC S9(11).
           02  W2-WKKIN        PIC S9(11).
           02  IN-YM.
             03  IN-YY         PIC 9(2).
             03  IN-MM         PIC 9(2).
           02  OK-IN           PIC X(1).
       01  WORK-AREA.
           02  MDR             PIC S9(11).
           02  MCR             PIC S9(11).
           02  ZMDR            PIC S9(11).
           02  ZMCR            PIC S9(11).
       01  SOEJI.
           02  FI              PIC 9(02).
           02  TI              PIC 9(02).
           02  TY-SW           PIC 9(01).
      *****
       COPY    LWMSG_PR.
       COPY    ACCUNT.
       COPY    LGENKF.
       COPY    FCTL.
       COPY    LKAZAN.
      *****
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER     PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DISP-INIT.
           02  FILLER.
               03  FILLER  PIC  N(01)  VALUE  "年".
               03  FILLER  PIC  N(02)  VALUE  "月度".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  X(24)  VALUE
               " 製造原価報告書（月次） ".
           02  FILLER.
               03  FILLER               PIC N(02)  VALUE
                   "確認".
               03  FILLER               PIC X(13)  VALUE
                   "OK=1,NO=9 ( )".
       01  DISP-BUZZER.
           02  DISP-BUZ-J-02      PIC X(05) VALUE X"1B4A02".
           02  DISP-BUZ-J-05      PIC X(05) VALUE X"1B4A05".
           02  DISP-BUZ-J-09      PIC X(05) VALUE X"1B4A09".
       01  DSP-AREA.
           03  DSP-010.
               05  DSP-011      PIC  X(23)
                              VALUE  "ｺﾝﾄﾛｰﾙ  ﾌｧｲﾙ  ｶﾞｲﾄｳ ﾅｼ!".
               05  DSP-012      PIC  X(22)
                              VALUE  "P/L ﾌｧｲﾙ ﾘﾗｲﾄ ﾌﾉｳ STOP".
               05  DSP-013      PIC  X(21)
                              VALUE  "P/L ﾌｧｲﾙ ﾆ ﾐﾄｳﾛｸ STOP".
               05  DSP-014      PIC  9(4).
               05  DSP-015      PIC  X(3).
       01  ACP-AREA.
           03  ACP-010.
               05  ACP-011     PIC  X(01).
               05  ACP-012     PIC  X(01).
           03  ACP-020.
               05  ACP-021     PIC  X(01).
      *
       COPY  LSMSG_PR.
      *
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DISP-INIT
       CALL "SD_Init" USING
            "DISP-INIT" " " "0" "0" "51" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DISP-INIT" " " "1" "0" "34" " " "DISP-INIT"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0101DISP-INIT" "N" "1" "4" "2" " "
            "01DISP-INIT" RETURNING RESU.
       CALL "SD_Init" USING
            "0201DISP-INIT" "N" "1" "8" "4" "0101DISP-INIT"
            " " RETURNING RESU.
       CALL "SD_Init" USING
            "0301DISP-INIT" "9" "1" "2" "2" "0201DISP-INIT"
            " " RETURNING RESU.
       CALL "SD_From" USING
           "0301DISP-INIT" BY REFERENCE Z-GEMYY2 "2" "0"  RETURNING RESU.
       CALL "SD_Init" USING
            "0401DISP-INIT" "9" "1" "6" "2" "0301DISP-INIT"
            " " RETURNING RESU.
       CALL "SD_From" USING
            "0401DISP-INIT" BY REFERENCE Z-GEMMM "2" "0"  RETURNING RESU.
       CALL "SD_Init" USING
            "0501DISP-INIT" "RX" "1" "31" "24"
            "0401DISP-INIT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "02DISP-INIT" " " "24" "0" "17" "01DISP-INIT"
            " " RETURNING RESU.
       CALL "SD_Init" USING
            "0102DISP-INIT" "N" "24" "61" "4" " "
            "02DISP-INIT" RETURNING RESU.
       CALL "SD_Init" USING
            "0202DISP-INIT" "X" "24" "66" "13"
            "0102DISP-INIT" " " RETURNING RESU.
      *DISP-BUZZER
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "15" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J-02" "X" "24" "80" "5" " "
            "DISP-BUZZER" RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J-05" "X" "24" "80" "5"
            "DISP-BUZ-J-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J-09" "X" "24" "80" "5"
            "DISP-BUZ-J-05" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "73" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" " " "24" "0" "73" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-011" "X" "24" "5" "23" " " "DSP-010" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-012" "X" "24" "5" "22" "DSP-011" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-013" "X" "24" "5" "21" "DSP-012" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-014" "9" "24" "30" "4" "DSP-013" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-014" BY REFERENCE AM-KEY "4" "0"  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-015" "X" "24" "40" "3" "DSP-014" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-015" BY REFERENCE GNKEY(1) "3" "0"
            BY REFERENCE I 4 RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "3" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" " " "24" "0" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-011" "X" "24" "30" "1" " " "ACP-010" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-011" BY REFERENCE I "2" "0" RETURNING RESU
       CALL "SD_Init" USING
            "ACP-012" "X" "24" "50" "1" "ACP-011" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-012" BY REFERENCE I "2" "0" RETURNING RESU
       CALL "SD_Init" USING
            "ACP-020" " " "24" "0" "1" "ACP-010" " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-021" "X" "24" "77" "1" " " "ACP-020" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-021" BY REFERENCE OKC "1" "0" RETURNING RESU
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
      *
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE       "DATE  "     TO      FCTL-KEY1.
      *           READ    FCTL-F    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING "INV-CON" INV-CON "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_Close"
               STOP      RUN
           END-IF.
           MOVE     FCTL-REC1     TO    Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-INIT" DISP-INIT "p"
                                         RETURNING RESU.
       ST-01.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-020 "ACP-020" " " "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ST-01
           END-IF.
           IF  OKC  NOT =  "1" AND "9"
               GO  TO  ST-01
           END-IF.
           IF  OKC  =  "9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP   RUN
           END-IF.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                           RETURNING RESU.
           CALL "SD_Output" USING "DISP-INIT" DISP-INIT "p"
                                           RETURNING RESU.
           CALL "SD_Output" USING "ACP-020" ACP-020 "p"
                                           RETURNING RESU.
           MOVE     Z-GEMYY2   TO    IN-YY.
           MOVE     Z-GEMMM    TO    IN-MM.
      *
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING
            "I-O" GEN_PNAME1 "SHARED" BY REFERENCE GEN_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
      *
           MOVE  Z-KONYMD     TO  ZYMD.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO ST-END
           END-IF.
           MOVE 1     TO TY-SW.
           MOVE ZI     TO TI.
           IF  TI > 12
               MOVE 0      TO TY-SW
               COMPUTE TI = Z-KSMM + ( TI - 12 )
               IF  TI > 12
                   COMPUTE TI = TI - 12
               END-IF
           END-IF.
           IF  Z-KSMM = 12
               MOVE 1     TO FI
           ELSE
               COMPUTE FI = Z-KSMM + 1
           END-IF.
      *
           PERFORM    ZERO-RTN     THRU   ZERO-EX.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" GEN_PNAME1 "SHARED" BY REFERENCE GEN_IDLST "1"
            "PL-KEY" BY REFERENCE PL-KEY.
       ST-30.
      *           READ       AM           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" AM_PNAME1 BY REFERENCE AM-REC " " RETURNING RET.
           IF  RET = 1
               GO TO ST-END
           END-IF.
           IF  BS-PL        =      0
               GO TO ST-30
           END-IF.
      *
           MOVE AM-KEY     TO KZM-KEY.
      *           READ KZM-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KZM-F_PNAME1 BY REFERENCE KZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE KZM-R
           END-IF.
           MOVE KZM-TJKR(ZI)     TO MDR.
           MOVE KZM-TJKS(ZI)     TO MCR.
           IF  TY-SW = 0
               MOVE KZM-TJKR(TI)     TO ZMDR
               MOVE KZM-TJKS(TI)     TO ZMCR
           ELSE
               MOVE KZM-ZJKR(TI)     TO ZMDR
               MOVE KZM-ZJKS(TI)     TO ZMCR
           END-IF.
           MOVE 1     TO I.
       ST-40.
           IF         GNKEY   (I) =      0
               GO TO ST-80
           END-IF.
           MOVE       GNKEY   (I)  TO     PL-KEY.
      *           READ       GEN          INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" GEN_PNAME1 BY REFERENCE PL-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "DSP-013" DSP-013 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DSP-014" DSP-014 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DSP-015" DSP-015 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J-02" DISP-BUZ-J-02 "p"
                                               RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-012 "ACP-012" "X" "1"
                   BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF.
           IF  DR-CR        =      1
               COMPUTE  W1-WKKIN     =      MDR  -  MCR
               COMPUTE  W2-WKKIN     =      ZMDR -  ZMCR
           ELSE
               COMPUTE  W1-WKKIN     =      MCR  -  MDR
               COMPUTE  W2-WKKIN     =      ZMCR -  ZMDR
           END-IF.
      *
           IF  GNCOM (I)    =      1
               COMPUTE   PL-TOUMM    =       PL-TOUMM + W1-WKKIN
               COMPUTE   PL-ZENMM    =       PL-ZENMM + W2-WKKIN
           ELSE
               COMPUTE   PL-TOUMM    =       PL-TOUMM - W1-WKKIN
               COMPUTE   PL-ZENMM    =       PL-ZENMM - W2-WKKIN
           END-IF.
      *           REWRITE     PL-REC      INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            GEN_PNAME1 GEN_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "DSP-012" DSP-012 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J-09" DISP-BUZ-J-09 "p"
                                             RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-011 "ACP-011" "X" "1"
                   BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF.
       ST-80.
           IF  I            <      12
               ADD      1            TO     I
               GO TO    ST-40
           END-IF.
           GO TO  ST-30.
       ST-END.
           PERFORM    CLSE-ENT     THRU   CLSE-EXT.
           CALL "DB_Close".
           STOP       RUN.
      *****
       ZERO-RTN.
      *           READ   GEN              NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" GEN_PNAME1 BY REFERENCE PL-REC " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  ZERO-EX
           END-IF.
           INITIALIZE    PL-MM.
           MOVE IN-YY     TO PL-YYWK.
           MOVE IN-MM     TO PL-MMWK.
      *           REWRITE     PL-REC       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            GEN_PNAME1 GEN_LNAME PL-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "DSP-013" DSP-013 "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J-09" DISP-BUZ-J-09 "p"
                                             RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-011 "ACP-011" "X" "1"
                   BY REFERENCE ESTAT RETURNING RESU
               PERFORM  CLSE-ENT  THRU  CLSE-EXT
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           GO TO ZERO-RTN.
       ZERO-EX.
           EXIT.
      **
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE GEN_IDLST GEN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
       CLSE-EXT.
           EXIT.
      **
       COPY  LPMSG_PR.
      **
