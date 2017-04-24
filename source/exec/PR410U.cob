       IDENTIFICATION  DIVISION.
       PROGRAM-ID.    PR410U.
       ENVIRONMENT     DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT            PIC X(02).
       77  W-OKC               PIC X(01).
       01  W1.
           02  I               PIC 99.
           02  W1-KEI.
             03  W1-DR         PIC S9(11).
             03  W1-CR         PIC S9(11).
           02  W1-WK1          PIC S9(11).
       01  MDRCR.
           02  BFMZN           PIC S9(11).
           02  MDR             PIC S9(11).
           02  MCR             PIC S9(11).
       01  WORK-AREA.
           02  SET-WORK.
             03  W-ZAN         PIC S9(11).
             03  W-KARI        PIC S9(11).
             03  W-KASI        PIC S9(11).
           02  SOE             PIC  9(02).
           02  FI              PIC  9(02).
           02  TI              PIC  9(02).
      ***
       COPY    LWMSG_PR.
      *
       COPY    ACCUNT.
       COPY    BS-LIB.
       COPY    LKAZAN.
       COPY    FCTL.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER  PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DSP-INIT.
           02  FILLER.
               03  FILLER  PIC  N(01)  VALUE  "年".
               03  FILLER  PIC  N(02)  VALUE  "月度".
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  9(02).
               03  FILLER  PIC  X(12)  VALUE
                       " 貸借対照表 ".
           02  FILLER.
               03  FILLER  PIC  N(02)  VALUE  "確認".
               03  FILLER  PIC  X(13)  VALUE
                       "OK=1,NO=9 ( )".
       01  DSP-AREA.
           02  RW-ERR  PIC  X(13)  VALUE
                       "ﾘﾗｲﾄ ﾌﾉｳ STOP".
           02  BS-RD-ERR.
               03  FILLER    PIC  X(21)  VALUE
                       "B/S ﾌｧｲﾙ ﾆ ﾐﾄｳﾛｸ STOP".
               03  D-AM-KEY  PIC  X(04).
               03  D-BSKEY   PIC  9(03).
           02  BS-RW-ERR  PIC  X(17)  VALUE
                       "B/S ﾌｧｲﾙ ﾘﾗｲﾄ ﾌﾉｳ".
           02  CODE-ERR  PIC  X(22)  VALUE
                       "B/S ﾌｧｲﾙ ﾆ 000 ｺｰﾄﾞ ﾅｼ".
       01  DSP-BUZ.
           02  BUZ-09  PIC  X(05)  VALUE  X"1B4A09".
           02  BUZ-02  PIC  X(05)  VALUE  X"1B4A02".
       01  ACP-AREA.
           02  ACP-CHK1  PIC  9(02).
           02  ACP-CHK2  PIC  9(02).
           02  ACP-CHK3  PIC  9(02).
           02  ACP-OKC   PIC  X(01).
       COPY    LSMSG_PR.
      ******************************************************************
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
      *DSP-INIT
       CALL "SD_Init" USING 
            "DSP-INIT" " " "0" "0" "39" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-INIT" " " "1" "0" "22" " " "DSP-INIT" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-INIT" "N" "1" "4" "2" " " "01DSP-INIT"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-INIT" "N" "1" "8" "4" "0101DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-INIT" "9" "1" "2" "2" "0201DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0301DSP-INIT" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0401DSP-INIT" "9" "1" "6" "2" "0301DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0401DSP-INIT" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0501DSP-INIT" "RX" "1" "35" "12" "0401DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-INIT" " " "24" "0" "17" "01DSP-INIT" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-INIT" "N" "24" "61" "4" " " "02DSP-INIT"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-INIT" "X" "24" "66" "13" "0102DSP-INIT" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "1" "0" "80" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "RW-ERR" "X" "1" "5" "13" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "BS-RD-ERR" " " "1" "0" "28" "RW-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01BS-RD-ERR" "X" "1" "5" "21" " " "BS-RD-ERR"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-AM-KEY" "X" "1" "30" "4" "01BS-RD-ERR" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-AM-KEY" BY REFERENCE AM-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BSKEY" "9" "1" "40" "3" "D-AM-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BSKEY" BY REFERENCE BSKEY(1) "3" "1" BY REFERENCE I 5
            RETURNING RESU.
       CALL "SD_Init" USING 
            "BS-RW-ERR" "X" "1" "5" "17" "BS-RD-ERR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CODE-ERR" "X" "1" "5" "22" "BS-RW-ERR" " " RETURNING RESU.
      *DSP-BUZ
       CALL "SD_Init" USING 
            "DSP-BUZ" " " "1" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "BUZ-09" "X" "1" "5" "5" " " "DSP-BUZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "BUZ-02" "X" "1" "5" "5" "BUZ-09" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "7" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CHK1" "9" "1" "20" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-CHK1" BY REFERENCE I "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CHK2" "9" "1" "50" "2" "ACP-CHK1" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-CHK2" BY REFERENCE I "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CHK3" "9" "1" "30" "2" "ACP-CHK2" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-CHK3" BY REFERENCE I "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "X" "24" "77" "1" "ACP-CHK3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE W-OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       ST.
      *
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY1" BY REFERENCE FCTL-KEY1.
           MOVE       "DATE  "     TO     FCTL-KEY1.
      *           READ  FCTL-F           UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "INV-CON" INV-CON "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               GO TO ST-999
           END-IF
           MOVE       FCTL-REC1    TO     Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-INIT" DSP-INIT "p" RETURNING RESU.
       ST-05.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  ST-999
           END-IF
           IF  ESTAT  NOT =  "01" AND "06"
               GO  TO  ST-05
           END-IF
           IF  W-OKC  NOT =  "1" AND "9"
               GO  TO  ST-05
           END-IF
           IF  W-OKC  =  "9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  ST-999
           END-IF
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-INIT" DSP-INIT "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KZM-F_PNAME1 "SHARED" BY REFERENCE KZM-F_IDLST "1"
            "KZM-KEY" BY REFERENCE KZM-KEY.
           CALL "DB_F_Open" USING
            "I-O" BS_PNAME1 "SHARED" BY REFERENCE BS_IDLST "1"
            "BS-KEY" BY REFERENCE BS-KEY.
           MOVE  Z-KONYMD     TO  ZYMD.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               GO TO ST-999
           END-IF
           MOVE ZI     TO TI.
           IF  TI > 12
               MOVE 13     TO FI
           ELSE
               IF  Z-KSMM = 12
                   MOVE 1     TO FI
               ELSE
                   COMPUTE FI = Z-KSMM + 1
               END-IF
           END-IF
      *****
           MOVE ZERO     TO W1-KEI.
       ST-10.
      *           READ       BS          NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BS_PNAME1 BY REFERENCE BS-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO ST-20
           END-IF
           MOVE       0            TO     BS-KINDR.
           MOVE       0            TO     BS-KINCR.
      *           REWRITE    BS-REC       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BS_PNAME1 BS_LNAME BS-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "RW-ERR" RW-ERR "p" RETURNING RESU
               CALL "SD_Output" USING "BUZ-09" BUZ-09 "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE ACP-CHK1 "ACP-CHK1"
                "9" "2" BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF
           GO TO      ST-10.
       ST-20.
      *           READ       AM          NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO ST-80
           END-IF
           IF  BS-PL        NOT =  0
               GO TO ST-20
           END-IF
      *
           MOVE AM-KEY     TO KZM-KEY.
      *           READ KZM-F UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" KZM-F_PNAME1 BY REFERENCE KZM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE KZM-R
           END-IF
      *
           MOVE ZERO     TO SET-WORK.
           PERFORM ZAN-SET-RTN THRU ZAN-SET-EX.
           PERFORM SET-RTN THRU SET-EX.
           MOVE       1            TO     I.
       ST-30.
           IF  BSKEY (I)    =      0
               GO TO ST-70
           END-IF
           MOVE       BSKEY (I)    TO     BS-KEY.
      *           READ       BS           INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BS_PNAME1 BY REFERENCE BS-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "BS-RD-ERR" BS-RD-ERR "p" RETURNING RESU
               CALL "SD_Output" USING "BUZ-09" BUZ-09 "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE ACP-CHK2 "ACP-CHK2"
                "9" "2" BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF
           IF  DR-CR        =      1
               COMPUTE  W1-WK1      =      BFMZN + MDR - MCR
           ELSE
               COMPUTE  W1-WK1       =      BFMZN - MDR + MCR
           END-IF
           IF  BSDR-CR (I)  =      2
               GO TO ST-40
           END-IF
           IF  BSCOM   (I)  =      1
               COMPUTE  BS-KINDR     =      BS-KINDR + W1-WK1
           ELSE
               COMPUTE  BS-KINDR     =      BS-KINDR - W1-WK1
           END-IF
           GO TO      ST-60.
       ST-40.
           IF  BSCOM   (I)  =      1
               COMPUTE  BS-KINCR     =      BS-KINCR + W1-WK1
           ELSE
               COMPUTE  BS-KINCR     =      BS-KINCR - W1-WK1
           END-IF.
       ST-60.
      *           REWRITE    BS-REC       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BS_PNAME1 BS_LNAME BS-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "BS-RW-ERR" BS-RW-ERR "p" RETURNING RESU
               CALL "SD_Output" USING "BUZ-09" BUZ-09 "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE ACP-CHK3 "ACP-CHK3"
                "9" "2" BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF.
       ST-70.
           IF  I            <      6
               ADD      1            TO     I
               GO TO    ST-30
           END-IF
           IF  DR-CR        =      1
               COMPUTE  W1-DR        =      W1-DR + BFMZN + MDR - MCR
           ELSE
               COMPUTE  W1-CR        =      W1-CR + BFMZN - MDR + MCR
           END-IF
           GO TO      ST-20.
       ST-80.
           MOVE       "000"        TO    BS-KEY.
      *           READ       BS           INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BS_PNAME1 BY REFERENCE BS-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "CODE-ERR" CODE-ERR "p" RETURNING RESU
               CALL "SD_Output" USING "BUZ-02" BUZ-02 "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE ACP-CHK3 "ACP-CHK3"
                "9" "2" BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF
           MOVE       W1-DR        TO     BS-KINDR.
           MOVE       W1-DR        TO     BS-KINCR.
      *           REWRITE    BS-REC       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BS_PNAME1 BS_LNAME BS-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "BS-RW-ERR" BS-RW-ERR "p" RETURNING RESU
               CALL "SD_Output" USING "BUZ-09" BUZ-09 "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE ACP-CHK3 "ACP-CHK3"
                "9" "2" BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1.
           CALL "DB_F_Open" USING
            "I-O " BS_PNAME1 "SHARED" BY REFERENCE BS_IDLST "1"
            "BS-KEY" BY REFERENCE BS-KEY.
       ST-90.
      *           READ       BS           NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" BS_PNAME1 BY REFERENCE BS-REC " "
            RETURNING RET.
           IF  RET = 1
               GO TO ST-END
           END-IF
           IF  BS-RKB       =      2
               GO TO ST-95
           END-IF
           IF  BS-RKB       NOT =  1
               GO TO ST-90
           END-IF.
       ST-95.
           COMPUTE    BS-KINCR     =      BS-KINCR + W1-DR  -  W1-CR.
      *           REWRITE     BS-REC       INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BS_PNAME1 BS_LNAME BS-REC RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "BS-RW-ERR" BS-RW-ERR "p" RETURNING RESU
               CALL "SD_Output" USING "BUZ-09" BUZ-09 "p" RETURNING RESU
               CALL "SD_Accept" USING BY REFERENCE ACP-CHK3 "ACP-CHK3"
                "9" "2" BY REFERENCE ESTAT RETURNING RESU
               GO TO    ST-END
           END-IF
           GO TO      ST-90.
       ST-END.
           PERFORM    CLSE-ENT     THRU   CLSE-EXT.
       ST-999.
           CALL "DB_Close".
           STOP       RUN.
      **********
       ZAN-SET-RTN.
           IF  TI > 12
               GO TO ZAN-SET-500
           END-IF
           MOVE FI          TO SOE.
       ZAN-SET-000.
           ADD KZM-TJKR(SOE)     TO W-KARI.
           ADD KZM-TJKS(SOE)     TO W-KASI.
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF
           IF  SOE = 12
               MOVE 1     TO SOE
               GO TO ZAN-SET-000
           END-IF
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
           IF  SOE = TI
               GO TO ZAN-SET-900
           END-IF
           IF  SOE = 15
               GO TO ZAN-SET-900
           END-IF
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
      *****
       SET-RTN.
           MOVE W-ZAN            TO BFMZN.
           MOVE KZM-TJKR(TI)     TO MDR.
           MOVE KZM-TJKS(TI)     TO MCR.
       SET-EX.
           EXIT.
      *****
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BS_IDLST BS_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KZM-F_IDLST KZM-F_PNAME1.
       CLSE-EXT.
           EXIT.
      *****
       COPY  LPMSG_PR.
      *****
