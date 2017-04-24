       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JT370U.
      **************************************************************************
      *    PROGRAM  :  ëqï ç›å…É}ÉXÉ^Å[Å@çáåvÇbÇgÇdÇbÇjÅ@ÅiåééüåJâzÅj          *
      *    COMPILE  :  CBL85(74MODE)                                           *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       INPUT-OUTPUT                SECTION.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  WRI-SW                  PIC  9(01).
       77  ZERO-SW                 PIC  9(01).
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT                PIC  X(02).
       01  WORK-AREA.
           02  W-DATA.
               03  W-02            PIC  9(06).
               03  W-03            PIC  9(01).
               03  W-041.
                   04  W-0411      PIC S9(06)   OCCURS  10.
               03  W-051.
                   04  W-0511      PIC S9(06)   OCCURS  10.
               03  W-061.
                   04  W-0611      PIC S9(06)   OCCURS  10.
               03  W-071.
                   04  W-0711      PIC S9(06)   OCCURS  10.
               03  W-081.
                   04  W-0811      PIC S9(06)   OCCURS  10.
               03  W-091.
                   04  W-0911      PIC S9(06)   OCCURS  10.
               03  W-111.
                   04  W-1111      PIC S9(06)   OCCURS  10.
           02  OKC                 PIC  9(01).
           02  CNT                 PIC  9(02).
           02  W-DZC               PIC  9(01).
           COPY    LWMSG.
      *
           COPY    LNJZAW.
           COPY    LNJZAI.
           COPY    LTKW02.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           03  FILLER.
               05  CLEAR   PIC  X(12) VALUE "CLEAR SCREEN".
               05  FILLER  PIC X(32) VALUE
                    "                                ".
               05  FILLER  PIC N(15) VALUE
                    "ëqï ç›å…É}ÉXÉ^Å[Å@çXêVÉ`ÉFÉbÉN".
           03  FILLER.
               05  FILLER  PIC X(26) VALUE
                                 "ämîF(OK=1,NO=9)-->    ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-OKC  PIC  9(01).
       01  DSP-ERR.
           02  ERR-10.
               03  FILLER  PIC  N(06)  VALUE  "çáåvÇ»ÇµÅ@Å@".
               03  FILLER  PIC  X(08).
           02  ERR-20.
               03  FILLER  PIC  N(06)  VALUE  "çáåvÉGÉâÅ[Å@".
               03  FILLER  PIC  X(08).
           02  ERR-30.
               03  FILLER  PIC  N(06)  VALUE  "ÇcÇ`ÇsÇ`Ç»Çµ".
               03  FILLER  PIC  X(08).
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
            "DSP-AREA" " " "0" "0" "100" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "74" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-AREA" "RX" "1" "19" "32" "CLEAR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "N" "1" "20" "30" "0201DSP-AREA" " "
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
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-10" " " "24" "0" "20" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-10" "N" "24" "15" "12" " " "ERR-10" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-10" "X" "24" "35" "8" "01ERR-10" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-10" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-20" " " "24" "0" "20" "ERR-10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-20" "N" "24" "15" "12" " " "ERR-20" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-20" "X" "24" "35" "8" "01ERR-20" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-20" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-30" " " "24" "0" "20" "ERR-20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ERR-30" "N" "24" "15" "12" " " "ERR-30" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-30" "X" "24" "35" "8" "01ERR-30" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-30" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
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
           PERFORM   UP0-RTN   THRU   UP0-EX.
           PERFORM   UP1-RTN   THRU   UP1-EX.
           PERFORM   UP2-RTN   THRU   UP2-EX.
           PERFORM   UP3-RTN   THRU   UP3-EX.
       OWARI.
           PERFORM   END-RTN   THRU   END-EX.
           CALL "DB_Close".
           STOP      RUN.
      ***************************************
      *    èâä˙èàóù                         *
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
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0512ID.
           MOVE WK0512ID TO NJZAIW_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-KW02_PNAME1 " " BY REFERENCE JT-KW02_IDLST "1"
            "KW02-KEY" BY REFERENCE KW02-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NJZAIW_PNAME1 " " BY REFERENCE NJZAIW_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       INI-EX.
           EXIT.
      ***************************************
      *    èIóπèàóù                         *
      ***************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAIW_IDLST NJZAIW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-KW02_IDLST JT-KW02_PNAME1.
       END-EX.
           EXIT.
      ***************************************
      *    Ç`ÇkÇkÅ@ÇyÇdÇqÇnÅ@ÉNÉäÉA         *
      ***************************************
       UP0-RTN.
      *           READ      NJZAIW      AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NJZAIW_PNAME1 BY REFERENCE NJZAIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP0-EX
           END-IF.
       UP0-010.
           MOVE      ZERO               TO  W-DATA  ZERO-SW.
           MOVE      NJZAIW-02          TO  W-02.
           MOVE      NJZAIW-03          TO  W-03.
       UP0-020.
           IF  ZERO-SW            =  1
               GO  TO  UP0-030
           END-IF
           PERFORM   DZC-RTN   THRU   DZC-EX.
           IF  W-DZC              =  1
               MOVE      W-DZC              TO  ZERO-SW
           END-IF.
       UP0-030.
      *           READ      NJZAIW      AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NJZAIW_PNAME1 BY REFERENCE NJZAIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP0-090
           END-IF
      *
           IF  (NJZAIW-02     NOT  =   W-02)  OR
               (NJZAIW-03     NOT  =   W-03)
               GO  TO  UP0-050
           END-IF
           GO  TO  UP0-020.
       UP0-050.
           PERFORM   DWD-RTN   THRU   DWD-EX.
           GO  TO  UP0-010.
       UP0-090.
           PERFORM   DWD-RTN   THRU   DWD-EX.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-KW02_IDLST JT-KW02_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAIW_IDLST NJZAIW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-KW02_PNAME1 " " BY REFERENCE JT-KW02_IDLST "1"
            "KW02-KEY" BY REFERENCE KW02-KEY.
           CALL "DB_F_Open" USING
            "INPUT" NJZAIW_PNAME1 " " BY REFERENCE NJZAIW_IDLST "0".
       UP0-EX.
           EXIT.
      ***************************************
      *    çáåvãÊï™Å@ÉNÉäÉA                 *
      ***************************************
       UP1-RTN.
           MOVE      SPACE  TO  NJZAI-KEY.
           MOVE      9      TO  NJZAI-01.
      *           START     NJZAI  KEY NOT < NJZAI-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-EX
           END-IF.
       UP1-010.
      *           READ      NJZAI  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-EX
           END-IF
           MOVE      " "    TO  NJZAI-99.
           PERFORM   REW-RTN   THRU   REW-EX.
           GO   TO   UP1-010.
       UP1-EX.
           EXIT.
      ***************************************
      *    êîó èWåvÅEÉ`ÉFÉbÉNÅ@Å@Å@         *
      ***************************************
       UP2-RTN.
      *           READ      NJZAIW      AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NJZAIW_PNAME1 BY REFERENCE NJZAIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-EX
           END-IF
           IF  NJZAIW-01          =   9
               GO  TO  UP2-RTN
           END-IF
           MOVE      SPACE              TO  KW02-R.
           MOVE      NJZAIW-02          TO  KW02-021.
           MOVE      NJZAIW-03          TO  KW02-022.
      *           READ      JT-KW02     INVALID  KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" JT-KW02_PNAME1 BY REFERENCE KW02-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-010
           END-IF
           GO  TO  UP2-RTN.
       UP2-010.
           MOVE      ZERO               TO  W-DATA.
           MOVE      NJZAIW-02          TO  W-02.
           MOVE      NJZAIW-03          TO  W-03.
       UP2-020.
           MOVE      ZERO               TO  CNT.
       UP2-030.
           ADD       1                  TO  CNT.
           IF  CNT                >   10
               GO  TO  UP2-040
           END-IF
           ADD       NJZAIW-0411(CNT)   TO  W-0411(CNT).
           ADD       NJZAIW-0511(CNT)   TO  W-0511(CNT).
           ADD       NJZAIW-0611(CNT)   TO  W-0611(CNT).
           ADD       NJZAIW-0711(CNT)   TO  W-0711(CNT).
           ADD       NJZAIW-0811(CNT)   TO  W-0811(CNT).
           ADD       NJZAIW-0911(CNT)   TO  W-0911(CNT).
           ADD       NJZAIW-1111(CNT)   TO  W-1111(CNT).
           GO  TO    UP2-030.
       UP2-040.
      *           READ      NJZAIW      AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NJZAIW_PNAME1 BY REFERENCE NJZAIW-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-090
           END-IF
           IF  NJZAIW-01          =   9
               GO  TO  UP2-040
           END-IF
           MOVE      SPACE              TO  KW02-R.
           MOVE      NJZAIW-02          TO  KW02-021.
           MOVE      NJZAIW-03          TO  KW02-022.
      *           READ      JT-KW02     INVALID  KEY
      *//////////////////////     
           CALL "DB_Read" USING
            "INVALID KEY" JT-KW02_PNAME1 BY REFERENCE KW02-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-045
           END-IF
           GO  TO  UP2-040.
      *
       UP2-045.
           IF  NJZAIW-02     NOT  =   W-02
               GO  TO  UP2-050
           END-IF
           IF  NJZAIW-03     NOT  =   W-03
               GO  TO  UP2-050
           END-IF
           GO  TO  UP2-020.
       UP2-050.
           PERFORM   TDC-RTN   THRU   TDC-EX.
           GO  TO  UP2-010.
       UP2-090.
           PERFORM   TDC-RTN   THRU   TDC-EX.
       UP2-EX.
           EXIT.
      ***************************************
      *    çáåvãÊï™Å@çXêVÉ`ÉFÉbÉN           *
      ***************************************
       UP3-RTN.
           MOVE      SPACE  TO  NJZAI-KEY.
           MOVE      9      TO  NJZAI-01.
      *           START     NJZAI  KEY NOT < NJZAI-KEY   INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UP3-EX
           END-IF.
       UP3-010.
      *           READ      NJZAI  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP3-EX
           END-IF
           IF  NJZAI-99       =  SPACE
               CALL "SD_Output" USING
                "ERR-30" ERR-30 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               PERFORM   DEL-RTN   THRU   DEL-EX
           END-IF
           GO   TO   UP3-010.
       UP3-EX.
           EXIT.
      ***************************************
      *    ëqï ç›å…É}ÉXÉ^Å@ÇvÇqÇhÇsÇd       *
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
            " " "¥ÿ± ∂∏¡Æ≥∫ﬁ§ª≤∂≤∑∞ ¶ µΩ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 " " BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
       WRI-EX.
           EXIT.
      ***************************************
      *    ëqï ç›å…É}ÉXÉ^Å@ÇqÇdÇvÇqÇhÇsÇd   *
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
      *    ëqï ç›å…É}ÉXÉ^Å@ÇcÇdÇkÇdÇsÇd     *
      ***************************************
       DEL-RTN.
      *           DELETE    NJZAI   INVALID
      *///////////////
           CALL "DB_Delete" USING NJZAI_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "NJZAI"     TO  ERR-F
               MOVE    "D"         TO  ERR-M
               MOVE     NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       DEL-EX.
           EXIT.
      ***************************************
      *    ÇcÇ`ÇsÇ`Å@ÇyÇdÇqÇnÅ@É`ÉFÉbÉN     *
      ***************************************
       DZC-RTN.
           MOVE  ZERO         TO  W-DZC  CNT.
       DZC-010.
           ADD   1            TO  CNT.
           IF  CNT              >   10
               GO  TO  DZC-EX
           END-IF
           IF  NJZAIW-0411(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           IF  NJZAIW-0511(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           IF  NJZAIW-0611(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           IF  NJZAIW-0711(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           IF  NJZAIW-0811(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           IF  NJZAIW-0911(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           IF  NJZAIW-1111(CNT) NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         DZC-EX
           END-IF
           GO  TO  DZC-010.
       DZC-EX.
           EXIT.
      ***************************************
      *    ÉèÅ[ÉNÅ@ÇyÇdÇqÇnÅ@É`ÉFÉbÉN       *
      ***************************************
       WZC-RTN.
           MOVE  ZERO         TO  W-DZC  CNT.
       WZC-010.
           ADD   1            TO  CNT.
           IF  CNT              >   10
               GO  TO  WZC-EX
           END-IF
           IF  W-0411(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           IF  W-0511(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           IF  W-0611(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           IF  W-0711(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           IF  W-0811(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           IF  W-0911(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           IF  W-1111(CNT)      NOT  =   ZERO
               MOVE  1          TO  W-DZC
               GO    TO         WZC-EX
           END-IF
           GO  TO  WZC-010.
       WZC-EX.
           EXIT.
      ***************************************
      *    ÇcÇ`ÇsÇ`Å@ÉZÉbÉgÅ@Å@             *
      ***************************************
       DAT-RTN.
           MOVE      ZERO               TO  CNT.
       DAT-010.
           ADD       1                  TO  CNT.
           IF  CNT                   >  10
               GO  TO  DAT-EX
           END-IF
      *
           MOVE  W-0411(CNT)            TO  NJZAI-0411(CNT).
           MOVE  W-0511(CNT)            TO  NJZAI-0511(CNT).
           MOVE  W-0611(CNT)            TO  NJZAI-0611(CNT).
           MOVE  W-0711(CNT)            TO  NJZAI-0711(CNT).
           MOVE  W-0811(CNT)            TO  NJZAI-0811(CNT).
           MOVE  W-0911(CNT)            TO  NJZAI-0911(CNT).
           MOVE  W-1111(CNT)            TO  NJZAI-1111(CNT).
           GO  TO  DAT-010.
       DAT-EX.
           EXIT.
      ***************************************
      *    çáåvÇcÇ`ÇsÇ`Å@É`ÉFÉbÉN           *
      ***************************************
       TDC-RTN.
           MOVE      9                  TO  NJZAI-01.
           MOVE      W-02               TO  NJZAI-02.
           MOVE      W-03               TO  NJZAI-03.
      *           READ    NJZAI            INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "ERR-10" ERR-10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  TDC-050
           END-IF
           MOVE      ZERO               TO  CNT.
       TDC-010.
           ADD       1                  TO  CNT.
           IF  CNT                   >  10
               GO  TO  TDC-030
           END-IF
      *
           IF  ( NJZAI-0411(CNT)  NOT  =  W-0411(CNT) )  OR
               ( NJZAI-0511(CNT)  NOT  =  W-0511(CNT) )  OR
               ( NJZAI-0611(CNT)  NOT  =  W-0611(CNT) )  OR
               ( NJZAI-0711(CNT)  NOT  =  W-0711(CNT) )  OR
               ( NJZAI-0811(CNT)  NOT  =  W-0811(CNT) )  OR
               ( NJZAI-0911(CNT)  NOT  =  W-0911(CNT) )  OR
               ( NJZAI-1111(CNT)  NOT  =  W-1111(CNT) )
               CALL "SD_Output" USING
                "ERR-20" ERR-20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               GO  TO  TDC-020
           END-IF
           GO  TO  TDC-010.
       TDC-020.
           PERFORM   DAT-RTN   THRU   DAT-EX.
       TDC-030.
           MOVE  "*"                    TO  NJZAI-99.
           PERFORM   REW-RTN   THRU   REW-EX.
           GO  TO  TDC-EX.
       TDC-050.
           CALL "SD_Output" USING "ERR-10" ERR-30 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       TDC-060.
           INITIALIZE                   NJZAI-R.
           MOVE      9                  TO  NJZAI-01.
           MOVE      W-02               TO  NJZAI-02.
           MOVE      W-03               TO  NJZAI-03.
           PERFORM   DAT-RTN   THRU   DAT-EX.
           MOVE  "*"                    TO  NJZAI-99.
           PERFORM   WRI-RTN   THRU   WRI-EX.
           IF  WRI-SW          =    1
               GO  TO  TDC-060
           END-IF.
       TDC-EX.
           EXIT.
      ***************************************
      *    çÌèúÉfÅ[É^Å@ÉèÅ[ÉNçÏê¨Å@         *
      ***************************************
       DWD-RTN.
           IF  ZERO-SW         =    1
               GO  TO  DWD-EX
           END-IF
           MOVE      SPACE              TO  KW02-R.
           MOVE      W-02               TO  KW02-021.
           MOVE      W-03               TO  KW02-022.
      *           WRITE     KW02-R  INVALID
      *//////////////
           CALL "DB_Insert" USING
            JT-KW02_PNAME1 JT-KW02_LNAME KW02-R RETURNING RET.
           IF  RET = 1
               GO  TO  DWD-010
           END-IF
           GO  TO  DWD-030.
       DWD-010.
           MOVE    "JT-OWI20"  TO  ERR-F.
           MOVE    "W"         TO  ERR-M.
           MOVE     KW02-KEY   TO  ERR-K.
           IF  ERR-STAT         =  "24"
               GO  TO  DWD-020
           END-IF
           IF  ERR-STAT    NOT  =  "00"
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  DWD-RTN.
       DWD-020.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-KW02_IDLST JT-KW02_PNAME1.
           CALL "SD_Output" USING
            " " "¥ÿ± ∂∏¡Æ≥∫ﬁ§ª≤∂≤∑∞ ¶ µΩ!" "STOP" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JT-KW02_PNAME1 " " BY REFERENCE JT-KW02_IDLST "1"
            "KW02-KEY" BY REFERENCE KW02-KEY.
           GO  TO  DWD-RTN.
       DWD-030.
           MOVE      ZERO               TO  CNT.
       DWD-040.
           ADD       1                  TO  CNT.
           IF  CNT                   >  9
               GO  TO  DWD-EX
           END-IF
           MOVE      CNT                TO  NJZAI-01.
           MOVE      W-02               TO  NJZAI-02.
           MOVE      W-03               TO  NJZAI-03.
      *           READ    NJZAI            INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  DWD-040
           END-IF
           PERFORM   DEL-RTN   THRU   DEL-EX.
           GO  TO  DWD-040.
       DWD-EX.
           EXIT.
      *****
           COPY    LPMSG.
      *******************    E N D    O F    P R O G R A M    **********
