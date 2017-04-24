       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      JT600U.
      **************************************************
      **      　出荷確定データ更新                    **
      **        USER  NAME : 日進ゴム.                **
      **        DATE       : 1991･10･09               **
      **        TYPE       : COBOL                    **
      **        PROGRAM-ID : JT600U                   **
      **        SCREEN-ID  : ------.                  **
      **        AUTHOR     : SAKIKO.D                 **
      **************************************************
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       SOURCE-COMPUTER.                 SYSTEM150.
       OBJECT-COMPUTER.                 SYSTEM150.
      ******************************************************************
      *                 DATA              DIVISION                     *
      ******************************************************************
       DATA                             DIVISION.
      ******************************************************************
      *            WORKING     STORAGE     SECTION                     *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       77  ERR-STAT               PIC  X(02).
       77  W-EC                   PIC  9.
       01  W-JS                   PIC  9.
       01  W-16                   PIC  9.
       01  I                      PIC  9(02).
       01  INV-SW                 PIC  9.
       01  WRI-SW                 PIC  9.
       01  CHK-SW                 PIC  9.
       01  HIZUKE                 PIC  9(08).
       01  W-CHK                  PIC  9(01)  VALUE  0.
       01  WORK-AREA.
           03  OKC                PIC  9(01).
           03  W-SDATE            PIC  9(08).
           03  SDATE    REDEFINES  W-SDATE.
               05  SYY            PIC  9(04).
               05  SYYL    REDEFINES SYY.
                 06  SYY1         PIC  9(02).
                 06  SYY2         PIC  9(02).
               05  SMM            PIC  9(02).
               05  SDD            PIC  9(02).
           03  SDATEL   REDEFINES  W-SDATE.
             04  F                PIC  9(02).
             04  SDATES           PIC  9(06).
           03  WK-AREA.
               05  WK-HIN         PIC  9(06).
               05  WK-SIZ         PIC  9(01).
           03  SYS-DATE           PIC  9(08).
      *
           COPY     LWMSG.
      *
           COPY     LIBFDD.
           COPY     L-JSTR.
           COPY     LJMSTD.
           COPY     L-JSJD.
           COPY     L-JNSR.
           COPY     LNJZAI.
           COPY     LIHIM.
      *
      ******************************************************************
      *            SCREEN                  SECTION                     *
      ******************************************************************
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
      *************************
      *    DISPLAY SECTION    *
      *************************
       01  DSP-AREA.
           03  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
           03  CRT-01  PIC X(20)
                   VALUE  " 出荷確定データ更新 ".
           03  CRT-JS.
               04  FILLER  PIC X(21) VALUE  "教　育=0 , 一　般1 , ".
               04  FILLER  PIC X(14) VALUE  "ＡＬＬ=9 ...  ".
           03  CRT-02   PIC X(29)
                   VALUE  "出荷日＝  年  月  日 （西暦）".
           03  CRT-03   PIC X(25)
                   VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  MSG-AREA.
           03  DISP-MSG-SPACE-1    PIC X(40)
                    VALUE  "                                        ".
           03  ERR-HIZ             PIC X(20)
                    VALUE  "先日付分確定　ＯＫ？".
           03  ERR-JSJD            PIC X(28)
                    VALUE  "出荷指図書（実績）未印字あり".
       01  DSP-SDATE.
           03  DSP-SYY   PIC 9(02).
           03  DSP-SMM   PIC Z9 .
           03  DSP-SDD   PIC Z9 .
       01  CLE-AREA.
           03  CLE-01    PIC X(02)  VALUE "  ".
           03  CLE-02    PIC X(02)  VALUE "  ".
           03  CLE-03    PIC X(02)  VALUE "  ".
       01  ACT-AREA.
           03  ACT-JS    PIC 9(01).
           03  ACT-SYY   PIC 9(02).
           03  ACT-SMM   PIC 9(02).
           03  ACT-SDD   PIC 9(02).
           03  ACT-OKC   PIC 9(01).
           COPY     LSMSG.
           COPY     LIBSCR.
      ******************************************************************
      *                 PROCEDURE         DIVISION                     *
      ******************************************************************
       PROCEDURE        DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "121" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-CLR" "X" "1" "0" "12" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-01" "RX" "1" "20" "20" "CRT-CLR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-JS" " " "6" "0" "35" "CRT-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CRT-JS" "X" "6" "16" "21" " " "CRT-JS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CRT-JS" "X" "6" "37" "14" "01CRT-JS" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-02" "X" "9" "21" "29" "CRT-JS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CRT-03" "X" "24" "41" "25" "CRT-02" " " RETURNING RESU.
      *MSG-AREA
       CALL "SD_Init" USING 
            "MSG-AREA" " " "0" "0" "88" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE-1" "X" "24" "1" "40" " " "MSG-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-HIZ" "X" "24" "1" "20" "DISP-MSG-SPACE-1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-JSJD" "X" "24" "1" "28" "ERR-HIZ" " " RETURNING RESU.
      *DSP-SDATE
       CALL "SD_Init" USING 
            "DSP-SDATE" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SYY" "9" "9" "29" "2" " " "DSP-SDATE" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SYY" BY REFERENCE SYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SMM" "Z9" "9" "33" "2" "DSP-SYY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SMM" BY REFERENCE SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-SDD" "Z9" "9" "37" "2" "DSP-SMM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-SDD" BY REFERENCE SDD "2" "0" RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "6" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-01" "X" "9" "29" "2" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" "X" "9" "33" "2" "CLE-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-03" "X" "9" "37" "2" "CLE-02" " " RETURNING RESU.
      *ACT-AREA
       CALL "SD_Init" USING 
            "ACT-AREA" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JS" "9" "6" "50" "1" " " "ACT-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SYY" "9" "9" "29" "2" "ACT-JS" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-SYY" BY REFERENCE SYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SMM" "9" "9" "33" "2" "ACT-SYY" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-SMM" BY REFERENCE SMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-SDD" "9" "9" "37" "2" "ACT-SMM" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACT-SDD" BY REFERENCE SDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-OKC" "9" "24" "60" "1" "ACT-SDD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       PRO-000.
           PERFORM  INI-RTN    THRU  INI-EX.
           PERFORM  ACT-RTN    THRU  ACT-EX.
           IF  ESTAT  =  "P9"
               GO  TO  PRO-999
           END-IF
           PERFORM  CHK-RTN    THRU  CHK-EX.
           IF  CHK-SW =  9
               GO  TO  PRO-999
           END-IF.
       PRO-010.
      *           READ  JSTR    NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  PRO-999
           END-IF
           IF  W-JS          =  9
               GO  TO  PRO-015
           END-IF
           MOVE  JSTR-16    TO  W-16.
           IF  W-16          =  2
               MOVE  1          TO  W-16
           END-IF
           IF  W-16     NOT  =  W-JS
               GO  TO  PRO-010
           END-IF.
       PRO-015.
           IF  JSTR-05       =  ZERO
               GO  TO  PRO-010
           END-IF
           IF  JSTR-05       >  SDATE
               GO  TO  PRO-010
           END-IF
           IF  JSTR-158      =  0
               GO  TO  PRO-010
           END-IF
      ***** 処理部署＝本社の時
           IF  JSTR-19       =  SPACE
               IF  JSTR-17  NOT  =  8  AND  9
                   GO  TO  PRO-010
               ELSE
                   GO  TO  PRO-020
               END-IF
           END-IF
      ***** 処理部署＝他の時
           IF  JSTR-17  NOT  =  8  AND  9
               GO  TO  PRO-010
           END-IF
           MOVE  1          TO  INV-SW.
           MOVE  JSTR-01    TO  JSJD-03.
           MOVE  0          TO  JSJD-04.
      *           START  JSJD      KEY    NOT <  JSJD-KEY2 INVALID
      *///////////////
           CALL "DB_Start" USING
            JSJD_PNAME1 "JSJD-KEY2" " NOT < " JSJD-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  PRO-010
           END-IF
      *           READ  JSJD       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  PRO-010
           END-IF
           IF  JSJD-03  NOT =  JSTR-01
               GO  TO  PRO-010
           END-IF
           MOVE  0          TO  INV-SW.
           IF  JSJD-158  =  0
               GO  TO  PRO-010
           END-IF.
       PRO-020.
           IF  W-CHK     =  0
               MOVE  1       TO  W-CHK
           END-IF
           PERFORM  JMST-RTN    THRU  JMST-EX.
           PERFORM  JSTR-RTN    THRU  JSTR-EX.
           PERFORM  JSJD-RTN    THRU  JSJD-EX.
           PERFORM  JNSR-RTN    THRU  JNSR-EX.
           IF  W-EC      =  0
               PERFORM  NJZAI-RTN   THRU  NJZAI-EX
           END-IF
           GO  TO   PRO-010.
       PRO-999.
           PERFORM  END-RTN     THRU  END-EX.
           IF  ESTAT  NOT =  "P9"
               IF  CHK-SW NOT =  9
                   IF  W-JS      =  1  OR  9
                       IF  W-CHK     =  1
                           CALL "C3_Set_Jrcode" USING 
                            USER_ID BY REFERENCE COMPLETION_CODE  100
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "DB_Close".
           STOP     RUN.
      ******************************************************************
      *    INI-RTN            初期処理
      ******************************************************************
       INI-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JSTR_PNAME1 " " BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 " " BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "I-O" JSJD_PNAME1 " " BY REFERENCE JSJD_IDLST "2"
            "JSJD-KEY" BY REFERENCE JSJD-KEY "JSJD-KEY2" BY REFERENCE
            JSJD-KEY2.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 " " BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           COPY     LIBCPR.
           MOVE     SPACE  TO    WORK-AREA.
           INITIALIZE            WORK-AREA.
           ACCEPT   SDATES       FROM      DATE.
           IF  SYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO SYY
           END-IF
           IF  SYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO SYY
           END-IF
           MOVE     W-SDATE      TO   SYS-DATE.
       INI-EX.
           EXIT.
      ******************************************************************
      *    END-RTN            終了処理
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSJD_IDLST JSJD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
      *
       END-EX.
           EXIT.
      ******************************************************************
      *    ACT-RTN            画面処理
      ******************************************************************
       ACT-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACT-JS "ACT-JS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  ACT-RTN
           END-IF
           IF  W-JS NOT  =  0  AND  1  AND  9
               GO  TO  ACT-RTN
           END-IF
           CALL "SD_Output" USING "ACT-JS" ACT-JS "p" RETURNING RESU.
       ACT-005.
           CALL "SD_Accept" USING BY REFERENCE ACT-SYY "ACT-SYY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  ACT-EX
           END-IF
           IF  ESTAT  =  "09"
               GO  TO  ACT-RTN
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"  AND  "00"
               GO  TO  ACT-005
           END-IF
           CALL "SD_Output" USING "DSP-SYY" DSP-SYY "p" RETURNING RESU.
       ACT-010.
           CALL "SD_Accept" USING BY REFERENCE ACT-SMM "ACT-SMM" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACT-005
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"  AND  "00"
               GO  TO  ACT-010
           END-IF
           CALL "SD_Output" USING "DSP-SMM" DSP-SMM "p" RETURNING RESU.
           IF  ( SMM  =  ZERO )  AND  ( SYY2  =  ZERO)
               GO  TO  ACT-020
           END-IF
           IF  ( SMM  <  1 )  OR  ( SMM  >  12 )
               GO  TO  ACT-010
           END-IF.
       ACT-020.
           CALL "SD_Accept" USING BY REFERENCE ACT-SDD "ACT-SDD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACT-010
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  ACT-020
           END-IF
           CALL "SD_Output" USING "DSP-SDD" DSP-SDD "p" RETURNING RESU.
           IF  ( SDD  =  ZERO )  AND  ( SMM  =  ZERO)
               MOVE  SYS-DATE     TO  W-SDATE
               CALL "SD_Output" USING
                "DSP-SDATE" DSP-SDATE "p" RETURNING RESU
               GO  TO  ACT-030
           END-IF
           IF  ( SDD  <  1 )  OR  ( SDD  >  31 )
               GO  TO  ACT-020
           END-IF
           MOVE     ZERO         TO   SYY1.
           IF  SYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO SYY
           END-IF
           IF  SYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO SYY
           END-IF
           IF  SYS-DATE       <   SDATE
               CALL "SD_Output" USING
                "ERR-HIZ" ERR-HIZ "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU
           END-IF.
       ACT-030.
           CALL "SD_Accept" USING BY REFERENCE ACT-OKC "ACT-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  ACT-020
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  ACT-030
           END-IF
           IF  OKC  NOT  =  "1"  AND  "9"
               GO  TO  ACT-030
           END-IF
           IF  OKC  =  9
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "CLE-AREA" CLE-AREA "p" RETURNING RESU
               MOVE  0    TO  WORK-AREA
               GO  TO  ACT-RTN
           END-IF.
       ACT-EX.
           EXIT.
      ******************************************************************
      *    受注マスター　　更新                              I.911122
      ******************************************************************
       JMST-RTN.
           MOVE  ZERO     TO  WK-AREA.
           IF  JSTR-081  =  ZERO  OR  999999
               GO  TO  JMST-EX
           END-IF
           MOVE  JSTR-08  TO  JMSTD-KEY1  ERR-K.
      *           READ  JMSTD    INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE      ERR-STAT TO   ERR-FLG
               MOVE      "JMST1"  TO   ERR-F
               MOVE      "G"      TO   ERR-M
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO   TO   JMST-EX
           END-IF
           IF  JMSTD-01  NOT =  0  AND  5 AND 6
               GO  TO  JMST-EX
           END-IF
           MOVE  JMSTD-05     TO  WK-HIN
           MOVE  JMSTD-09     TO  WK-SIZ.
           PERFORM  SET1-RTN  THRU  SET1-EX
                    VARYING   I     FROM  1  BY  1
                      UNTIL   I  >  10.
           MOVE  JMSTD-KEY1     TO  ERR-K.
      *           REWRITE   JMSTD-R        INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE     "JMSTD"     TO  ERR-F
               MOVE     "R"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JMST-EX.
           EXIT.
      *--------------------------------*
      *    ＳＥＴ１－ＲＴＮ            *
      *--------------------------------*
       SET1-RTN.
           SUBTRACT  JSTR-1111(I)  FROM  JMSTD-151(I).
           ADD   JSTR-1211(I)  TO  JMSTD-1211(I).
       SET1-EX.
           EXIT.
      ******************************************************************
      *    出荷指図トラン　更新
      ******************************************************************
       JSTR-RTN.
           MOVE      1                 TO    JSTR-17.
           MOVE      JSTR-KEY          TO    ERR-K.
      *           REWRITE   JSTR-R            INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JSTR_PNAME1 JSTR_LNAME JSTR-R RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO  ERR-F
               MOVE     "R"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  JSTR-EX.
       JSTR-010.
           MOVE      JSTR-KEY          TO  ERR-K.
      *           DELETE    JSTR              INVALID  KEY
      *///////////////
           CALL "DB_Delete" USING JSTR_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE     "JSTR"     TO  ERR-F
               MOVE     "D"        TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JSTR-EX.
           EXIT.
      ******************************************************************
      *    出荷指図ファイル更新       --  ADD 92/03/17  --
      ******************************************************************
       JSJD-RTN.
           MOVE      JSTR-01           TO    JSJD-03.
           MOVE      JSTR-02           TO    JSJD-04.
      *           READ      JSJD     KEY  IS  JSJD-KEY2  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JSJD_PNAME1 BY REFERENCE JSJD-REC " " BY REFERENCE
            JSJD-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  JSJD-EX
           END-IF
           MOVE      JSJD-KEY2         TO    ERR-K.
           MOVE      1                 TO    JSJD-17.
      *           REWRITE   JSJD-REC          INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            JSJD_PNAME1 JSJD_LNAME JSJD-REC RETURNING RET.
           IF  RET = 1
               MOVE     "JSJD"     TO  ERR-F
               MOVE     "R"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       JSJD-EX.
           EXIT.
      ******************************************************************
      *    入出庫累積ファイル作成       --  ADD 97/12/05  --
      ******************************************************************
       JNSR-RTN.
           IF  JSTR-17 NOT = 1
               GO  TO  JNSR-EX
           END-IF
           IF  JSTR-1211(1) = ZERO AND JSTR-1211(2) = ZERO AND
               JSTR-1211(3) = ZERO AND JSTR-1211(4) = ZERO AND
               JSTR-1211(5) = ZERO AND JSTR-1211(6) = ZERO AND
               JSTR-1211(7) = ZERO AND JSTR-1211(8) = ZERO AND
               JSTR-1211(9) = ZERO AND JSTR-1211(10) = ZERO
               GO  TO  JNSR-EX
           END-IF
           IF  JSTR-122    = ZERO
               GO  TO  JNSR-EX
           END-IF
      *
           MOVE    SPACE      TO  JNSR-R.
           INITIALIZE         JNSR-R.
           MOVE    JSTR-09    TO  JNSR-01.
           MOVE    JSTR-05    TO  HIZUKE.
           MOVE    HIZUKE     TO  JNSR-02  JNSR-16  JNSR-20.
           IF  JSTR-03    =   0
               MOVE    22         TO  JNSR-03
           ELSE
               IF  JSTR-03        =  5  OR  6
                   MOVE    23         TO  JNSR-03
               ELSE
                   MOVE    25         TO  JNSR-03
               END-IF
           END-IF
           MOVE    JSTR-01    TO  JNSR-04  JNSR-181 JNSR-221.
           MOVE    JSTR-02    TO  JNSR-05  JNSR-182 JNSR-222.
           MOVE    JSTR-07    TO  JNSR-06.
           MOVE    JSTR-10    TO  JNSR-07.
           MOVE    JSTR-1211 (1)  TO  JNSR-081 (1).
           MOVE    JSTR-1211 (2)  TO  JNSR-081 (2).
           MOVE    JSTR-1211 (3)  TO  JNSR-081 (3).
           MOVE    JSTR-1211 (4)  TO  JNSR-081 (4).
           MOVE    JSTR-1211 (5)  TO  JNSR-081 (5).
           MOVE    JSTR-1211 (6)  TO  JNSR-081 (6).
           MOVE    JSTR-1211 (7)  TO  JNSR-081 (7).
           MOVE    JSTR-1211 (8)  TO  JNSR-081 (8).
           MOVE    JSTR-1211 (9)  TO  JNSR-081 (9).
           MOVE    JSTR-1211 (10) TO  JNSR-081 (10).
           MOVE    0            TO  JNSR-09  JNSR-17  JNSR-21.
           MOVE    JSTR-03      TO  JNSR-10.
           IF  JNSR-10      =  5
               MOVE  1              TO  JNSR-10
           END-IF
           IF  JNSR-10      =  6
               MOVE  2              TO  JNSR-10
           END-IF
           MOVE    JSTR-06      TO  JNSR-11.
           MOVE    JSTR-14B     TO  JNSR-12.
           MOVE    JSTR-13      TO  JNSR-13.
           MOVE    JSTR-14      TO  JNSR-14.
           MOVE    JSTR-081     TO  JNSR-151.
           MOVE    JSTR-082     TO  JNSR-152.
           MOVE    JSTR-061     TO  JNSR-19.
           MOVE    JSTR-14D     TO  JNSR-23.
           MOVE    JSTR-15      TO  JNSR-24.
           MOVE    ZERO         TO  JNSR-90  JNSR-91  JNSR-92.
           MOVE    JSTR-09      TO  HI-KEY.
      *           READ  HI-M   WITH UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO       TO  HI-BCD1
           END-IF
           MOVE    HI-BCD1      TO  JNSR-82.
      *
           MOVE   0             TO  W-EC.
           MOVE   JNSR-KEY1     TO  ERR-K.
      *           WRITE   JNSR-R    INVALID
      *//////////////
           CALL "DB_Insert" USING
            JNSR_PNAME1 JNSR_LNAME JNSR-R RETURNING RET.
           IF  RET = 1
               MOVE   1           TO  W-EC
               MOVE  "JNSR"       TO  ERR-F
               MOVE  "W"          TO  ERR-M
               MOVE    ERR-STAT  TO  ERR-FLG
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
           END-IF.
       JNSR-EX.
           EXIT.
      ******************************************************************
      *    倉別在庫マスタ　更新  (NEW)
      ******************************************************************
       NJZAI-RTN.
           IF  JSTR-09       >  999899
               GO  TO  NJZAI-EX
           END-IF
           IF  JSTR-1211(1)  =  ZERO  AND  JSTR-1211(2)  =  ZERO  AND
               JSTR-1211(3)  =  ZERO  AND  JSTR-1211(4)  =  ZERO  AND
               JSTR-1211(5)  =  ZERO  AND  JSTR-1211(6)  =  ZERO  AND
               JSTR-1211(7)  =  ZERO  AND  JSTR-1211(8)  =  ZERO  AND
               JSTR-1211(9)  =  ZERO  AND  JSTR-1211(10) =  ZERO
               GO  TO  NJZAI-EX
           END-IF.
       NJZAI-010.
           MOVE  0        TO  INV-SW.
           MOVE  JSTR-07  TO  NJZAI-01.
           MOVE  JSTR-09  TO  NJZAI-02.
           MOVE  JSTR-10  TO  NJZAI-03.
      *           READ     NJZAI            INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   SPACE    TO  NJZAI-R
               INITIALIZE          NJZAI-R
               MOVE  JSTR-07   TO  NJZAI-01
               MOVE  JSTR-09   TO  NJZAI-02
               MOVE  JSTR-10   TO  NJZAI-03
               MOVE  1         TO  INV-SW
           END-IF
      *
           MOVE    1       TO    I.
       NJZAI-020.
           IF  JSTR-03     NOT =  5  AND 6
               ADD       JSTR-1211(I)      TO      NJZAI-0811(I)
               SUBTRACT  JSTR-1211(I)      FROM    NJZAI-0911(I)
           ELSE
               IF  JSTR-03         =  5
                   SUBTRACT  JSTR-1211(I)      FROM    NJZAI-0811(I)
                   ADD       JSTR-1211(I)      TO      NJZAI-0911(I)
               END-IF
           END-IF
           ADD    1                 TO      I.
           IF  I   NOT  >   10
               GO  TO  NJZAI-020
           END-IF.
       NJZAI-030.
           PERFORM  NJWR-RTN  THRU  NJWR-EX.
           IF  WRI-SW   =   1
               GO  TO  NJZAI-010
           END-IF.
       NJZAI-040.
           MOVE  0        TO  INV-SW.
           MOVE  9        TO  NJZAI-01.
           MOVE  JSTR-09  TO  NJZAI-02.
           MOVE  JSTR-10  TO  NJZAI-03.
      *           READ     NJZAI            INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   SPACE    TO  NJZAI-R
               INITIALIZE          NJZAI-R
               MOVE  9         TO  NJZAI-01
               MOVE  JSTR-09   TO  NJZAI-02
               MOVE  JSTR-10   TO  NJZAI-03
               MOVE  1         TO  INV-SW
           END-IF
      *
           MOVE    1       TO    I.
       NJZAI-050.
           IF  JSTR-03     NOT =  5  AND 6
               ADD       JSTR-1211(I)      TO      NJZAI-0811(I)
               SUBTRACT  JSTR-1211(I)      FROM    NJZAI-0911(I)
           ELSE
               IF  JSTR-03         =  5
                   SUBTRACT  JSTR-1211(I)      FROM    NJZAI-0811(I)
                   ADD       JSTR-1211(I)      TO      NJZAI-0911(I)
               END-IF
           END-IF
           ADD    1                 TO      I.
           IF  I   NOT  >   10
               GO  TO  NJZAI-050
           END-IF
      *
           PERFORM  NJWR-RTN  THRU  NJWR-EX.
           IF  WRI-SW   =   1
               GO  TO  NJZAI-040
           END-IF.
       NJZAI-EX.
           EXIT.
      *****
       NJWR-RTN.
           MOVE   0            TO  WRI-SW.
           IF  INV-SW   =   1
               GO  TO  NJWR-010
           END-IF
      *
           MOVE   NJZAI-KEY     TO  ERR-K.
      *           REWRITE   NJZAI-R           INVALID  KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE     "NJZAI"     TO  ERR-F
               MOVE     "R"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO   TO   NJWR-EX.
       NJWR-010.
           MOVE   NJZAI-KEY    TO  ERR-K.
      *           WRITE     NJZAI-R           INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  NJWR-020
           END-IF
           GO  TO    NJWR-EX.
       NJWR-020.
           IF  ERR-STAT          =  "24"
               GO  TO  NJWR-030
           END-IF
           IF  ERR-STAT      NOT =  "00"
               MOVE     "NJZAI"     TO  ERR-F
               MOVE     "W"         TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO    NJWR-EX.
       NJWR-030.
           MOVE   1            TO  WRI-SW.
           MOVE   "W"          TO  ERR-M.
           MOVE   "NJZAI"      TO  ERR-F.
           MOVE   NJZAI-KEY    TO  ERR-K.
           MOVE   ERR-STAT     TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING
            " " "ｴﾘｱ ｶｸﾁｮｳｺﾞ,ｻｲｶｲｷｰ ｦ ｵｽ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
                "DISP-MSG-SPACE-1" DISP-MSG-SPACE-1 "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       NJWR-EX.
           EXIT.
       CHK-RTN.
      *           READ  JSJD       NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  1       TO  CHK-SW
               GO  TO  CHK-EX
           END-IF
           IF  JSJD-158     =  1
               GO  TO  CHK-RTN
           END-IF
           IF  JSJD-07      >  SDATE
               GO  TO  CHK-RTN
           END-IF
           IF  W-JS         =  0
               IF  JSJD-16  NOT =  0
                   GO  TO  CHK-RTN
               END-IF
           END-IF
           IF  W-JS         =  1
               IF  JSJD-16      =  0
                   GO  TO  CHK-RTN
               END-IF
           END-IF
           CALL "SD_Output" USING
            "ERR-JSJD" ERR-JSJD "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
       CHK-EX.
           EXIT.
      **
           COPY     LPMSG.
