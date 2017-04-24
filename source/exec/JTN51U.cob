       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JT370U.
      **************************************************************************
      *    PROGRAM  :  ‘q•ÊÝŒÉƒ}ƒXƒ^[@o‰×Žw}”@XV@iŒŽŽŸŒJj          *
      *    COMPILE  :  CBL85(74MODE)                                           *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  WRI-SW                  PIC  9(01).
       77  ZERO-SW                 PIC  9(01).
       01  ERR-STAT                PIC  X(02).
       01  WORK-AREA.
           02  OKC                 PIC  9(01).
           02  CNT                 PIC  9(02).
           02  W-DZC               PIC  9(01).
           COPY    LWMSG.
      *
           COPY    L-JSTR.
           COPY    L-TDIF.
           COPY    LNJZAI.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           03  FILLER.
               05  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
               05  FILLER  PIC X(40) VALUE
                    "                                        ".
               05  FILLER  PIC N(19) VALUE
                    "‘q•ÊÝŒÉƒ}ƒXƒ^[@o‰×Žw}”@‹­§C³".
           03  FILLER.
               05  FILLER  PIC X(26) VALUE
                                 "Šm”F(OK=1,NO=9)-->    ØÀ°Ý".
       01  ACP-AREA.
           02  ACP-OKC  PIC  9(01).
       COPY    LSMSG.
      ***************************************
       PROCEDURE                   DIVISION.
      ***************************************
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "116" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "90" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201DSP-AREA" "RX" "1" "19" "40" "CLEAR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "N" "1" "20" "38" "0201DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" " " "24" "0" "26" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-AREA" "X" "24" "41" "26" " " "02DSP-AREA"
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "61" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM   INI-RTN   THRU   INI-EX.
           IF  COMPLETION_CODE  =  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           PERFORM   UP1-RTN   THRU   UP1-EX.
           PERFORM   UP2-RTN   THRU   UP2-EX.
           PERFORM   UP3-RTN   THRU   UP3-EX.
       OWARI.
           PERFORM   END-RTN   THRU   END-EX.
           CALL "DB_Close".
           STOP      RUN.
      ***************************************
      *    ‰Šúˆ—                         *
      ***************************************
       INI-RTN.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      NOT  =  "01"  AND  "06"
               GO  TO  INI-030
           END-IF
           IF  OKC             =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  OKC        NOT  =  1
               GO  TO  INI-030
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 " " BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TDIF_PNAME1 " " BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       INI-EX.
           EXIT.
      ***************************************
      *    I—¹ˆ—                         *
      ***************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
       END-EX.
           EXIT.
      ***************************************
      *    ‡Œv‹æ•ª@ƒNƒŠƒA                 *
      ***************************************
       UP1-RTN.
      *           READ      NJZAI  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-EX
           END-IF
           INITIALIZE       NJZAI-09.
           PERFORM   REW-RTN   THRU   REW-EX.
           GO   TO   UP1-RTN.
       UP1-EX.
           EXIT.
      ***************************************
      *    ”—ÊWŒvEƒ`ƒFƒbƒNi‚i‚r‚s‚qj   *
      ***************************************
       UP2-RTN.
      *           READ      JSTR        NEXT   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-EX
           END-IF
           IF  JSTR-17            =   1
               GO  TO  UP2-RTN
           END-IF
      *
           MOVE      JSTR-07            TO  NJZAI-01.
           MOVE      JSTR-09            TO  NJZAI-02.
           MOVE      JSTR-10            TO  NJZAI-03.
      *           READ      NJZAI       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-010
           END-IF
      *
           PERFORM   DT1-RTN   THRU   DT1-EX.
           PERFORM   REW-RTN   THRU   REW-EX.
           GO  TO  UP2-020.
       UP2-010.
           INITIALIZE                   NJZAI-R.
           MOVE      JSTR-07            TO  NJZAI-01.
           MOVE      JSTR-09            TO  NJZAI-02.
           MOVE      JSTR-10            TO  NJZAI-03.
           PERFORM   DT1-RTN   THRU   DT1-EX.
           PERFORM   WRI-RTN   THRU   WRI-EX.
           IF  WRI-SW          =    1
               GO  TO  UP2-010
           END-IF.
       UP2-020.
           MOVE      9                  TO  NJZAI-01.
           MOVE      JSTR-09            TO  NJZAI-02.
           MOVE      JSTR-10            TO  NJZAI-03.
      *           READ      NJZAI       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-030
           END-IF
      *
           PERFORM   DT1-RTN   THRU   DT1-EX.
           PERFORM   REW-RTN   THRU   REW-EX.
           GO  TO  UP2-RTN.
       UP2-030.
           INITIALIZE                   NJZAI-R.
           MOVE      9                  TO  NJZAI-01.
           MOVE      JSTR-09            TO  NJZAI-02.
           MOVE      JSTR-10            TO  NJZAI-03.
           PERFORM   DT1-RTN   THRU   DT1-EX.
           PERFORM   WRI-RTN   THRU   WRI-EX.
           IF  WRI-SW          =    1
               GO  TO  UP2-030
           END-IF
           GO  TO  UP2-RTN.
       UP2-EX.
           EXIT.
      ***************************************
      *    ”—ÊWŒvEƒ`ƒFƒbƒNi‚s‚c‚h‚ej   *
      ***************************************
       UP3-RTN.
      *           READ      TDIF        NEXT   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP3-EX
           END-IF
           IF  TDI-UPC       NOT  =   0
               GO  TO  UP3-RTN
           END-IF
           IF  TDI-HCD            >   999899
               GO  TO  UP3-RTN
           END-IF
      *
           MOVE      TDI-SOK            TO  NJZAI-01.
           MOVE      TDI-HCD            TO  NJZAI-02.
           MOVE      TDI-SKB            TO  NJZAI-03.
      *           READ      NJZAI       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP3-010
           END-IF
      *
           ADD   TDI-SU                 TO  NJZAI-0911(TDI-SNO)
           PERFORM   REW-RTN   THRU   REW-EX.
           GO  TO  UP3-020.
       UP3-010.
           INITIALIZE                   NJZAI-R.
           MOVE      TDI-SOK            TO  NJZAI-01.
           MOVE      TDI-HCD            TO  NJZAI-02.
           MOVE      TDI-SKB            TO  NJZAI-03.
           ADD   TDI-SU                 TO  NJZAI-0911(TDI-SNO)
           PERFORM   WRI-RTN   THRU   WRI-EX.
           IF  WRI-SW          =    1
               GO  TO  UP3-010
           END-IF.
       UP3-020.
           MOVE      9                  TO  NJZAI-01.
           MOVE      TDI-HCD            TO  NJZAI-02.
           MOVE      TDI-SKB            TO  NJZAI-03.
      *           READ      NJZAI       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP3-030
           END-IF
      *
           ADD   TDI-SU                 TO  NJZAI-0911(TDI-SNO)
           PERFORM   REW-RTN   THRU   REW-EX.
           GO  TO  UP3-RTN.
       UP3-030.
           INITIALIZE                   NJZAI-R.
           MOVE      9                  TO  NJZAI-01.
           MOVE      TDI-HCD            TO  NJZAI-02.
           MOVE      TDI-SKB            TO  NJZAI-03.
           ADD   TDI-SU                 TO  NJZAI-0911(TDI-SNO)
           PERFORM   WRI-RTN   THRU   WRI-EX.
           IF  WRI-SW          =    1
               GO  TO  UP3-030
           END-IF
           GO  TO  UP3-RTN.
       UP3-EX.
           EXIT.
      ***************************************
      *    ‘q•ÊÝŒÉƒ}ƒXƒ^@‚v‚q‚h‚s‚d       *
      ***************************************
       WRI-RTN.
           MOVE    0           TO  WRI-SW.
      *           WRITE     NJZAI-R INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  WRI-010
           END-IF
           GO  TO  WRI-EX.
       WRI-010.
           IF  ERR-STAT         =  "24"
               GO  TO  WRI-020
           END-IF
           IF  ERR-STAT    NOT  =  "00"
               MOVE    "NJZAI"     TO  ERR-F
               MOVE    "W"         TO  ERR-M
               MOVE     NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           MOVE    2           TO  WRI-SW.
           GO  TO  WRI-EX.
       WRI-020.
           MOVE    1           TO  WRI-SW.
           MOVE    "NJZAI"     TO  ERR-F.
           MOVE    "W"         TO  ERR-M.
           MOVE     NJZAI-KEY  TO  ERR-K.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING
            " " "´Ø± ¶¸Á®³ºÞ,»²¶²·° ¦ µ½!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       WRI-EX.
           EXIT.
      ***************************************
      *    ‘q•ÊÝŒÉƒ}ƒXƒ^@‚q‚d‚v‚q‚h‚s‚d   *
      ***************************************
       REW-RTN.
      *           REWRITE   NJZAI-R INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE    "NJZAI"     TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       REW-EX.
           EXIT.
      ***************************************
      *    ‚c‚`‚s‚`@ƒZƒbƒgi‚i‚r‚s‚qj     *
      ***************************************
       DT1-RTN.
           MOVE      ZERO               TO  CNT.
       DT1-010.
           ADD       1                  TO  CNT.
           IF  CNT                   >  10
               GO  TO  DT1-EX
           END-IF
      *
           IF  JSTR-03          NOT  =  5  AND  6
               IF  JSTR-14               =  9
                   ADD   JSTR-1111(CNT)         TO  NJZAI-0911(CNT)
               ELSE
                   IF  JSTR-17               =  0
                       ADD   JSTR-1111(CNT)         TO  NJZAI-0911(CNT)
                   ELSE
                       ADD   JSTR-1211(CNT)         TO  NJZAI-0911(CNT)
                   END-IF
               END-IF
           END-IF
           IF  JSTR-03               =  5
               IF  JSTR-14               =  9
                   SUBTRACT  JSTR-1111(CNT)   FROM  NJZAI-0911(CNT)
               ELSE
                   IF  JSTR-17               =  0
                       SUBTRACT  JSTR-1111(CNT)   FROM  NJZAI-0911(CNT)
                   ELSE
                       SUBTRACT  JSTR-1211(CNT)   FROM  NJZAI-0911(CNT)
                   END-IF
               END-IF
           END-IF
           GO  TO  DT1-010.
       DT1-EX.
           EXIT.
      *****
           COPY    LPMSG.
      *******************    E N D    O F    P R O G R A M    **********
