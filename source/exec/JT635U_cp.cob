       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT635U.
       AUTHOR.                        --------.
      ***************************************************
      *    PROGRAM        : o‰×E“üoŒÉ—ÝÏƒ[ƒNì¬  * @@@
      *    DATA WRITTEN   : 94/09/06                    *
      *    SCREEN USED    :                             *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL85 (74MODE)            *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM3100.
       OBJECT-COMPUTER.               SYSTEM3100.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  JT-W170K1ID               PIC X(12).
       77  JT-W170K3ID               PIC X(12).
       01  STN-NO.
           02  STN-NO-01             PIC X(03).
           02  STN-NO-02             PIC X(03).
       01  WF1-ID.
           02  WORK1-ID-01           PIC X(09) VALUE "JT-W170K1".
           02  WORK1-ID-02           PIC X(03).
       01  WF2-ID.
           02  WORK2-ID-01           PIC X(09) VALUE "JT-W170K3".
           02  WORK2-ID-02           PIC X(03).
       01  W-AREA.
           02  OKC                   PIC 9(01).
           02  W-CHK                 PIC 9(01).
           02  HIZUKE                PIC 9(08).
      *
       COPY    LWMSG.
      *
           COPY  L-JSTR.
           COPY  LJT170.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLR-1.
           02  FILLER      PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA1.
           02  DSP1-01     PIC  X(16)  VALUE
                           " “¾ˆÓæ•Êo‰× E".
           02  DSP1-02     PIC  X(20)  VALUE
                           " •i–¼•Ê“üoŒÉ–â‡‚¹ ".
           02  DSP1-03     PIC  X(26)  VALUE
                           "ÅV‚Ìƒf[ƒ^‚É‚·‚é    =  1".
           02  DSP1-04     PIC  X(35)  VALUE
                           "              ‚µ‚È‚¢  =  9  .....  ".
           02  DSP1-05     PIC  X(26)  VALUE
                           "Šm”F(OK=1,NO=9)-->    ØÀ°Ý".
       01  ACP-AREA.
           02  ACP-CHK     PIC 9(01).
           02  ACP-OKC     PIC 9(01).
      *
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "CLR-1" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLR-1" "X" "1" "0" "12" " " "CLR-1" RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING
            "DSP-AREA1" " " "0" "0" "123" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP1-01" "RX" "1" "18" "16" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP1-02" "RX" "1" "34" "20" "DSP1-01" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP1-03" "X" "14" "17" "26" "DSP1-02" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP1-04" "X" "16" "17" "35" "DSP1-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP1-05" "X" "24" "41" "26" "DSP1-04" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CHK" "9" "16" "51" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "9" "24" "61" "1" "ACP-CHK" " "  RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       HAJIME.
           PERFORM   INI-RTN    THRU  INI-EX.
           IF (ESTAT   =   "P9")  OR  (W-CHK  =  9)
               CALL "SD_Output" USING "CLR-1" CLR-1 "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  OKC     =   1
               PERFORM   MAIN-RTN   THRU  MAIN-EX
           END-IF
           PERFORM   END-RTN    THRU  END-EX.
           CALL "DB_Close"
           STOP  RUN.
      ************************************
      *    ƒƒCƒ“@ƒ‹[ƒ`ƒ“              *
      ************************************
       MAIN-RTN.
      *           SELECT J-M WHERE JSTR-17 >= 2
      *///////////////add koyama 20161031
           CALL "DB_Select" USING
            JSTR_PNAME1 "WHERE"
            "JSTR-17" ">=" 2 RETURNING RET.
      *           READ   JSTR     NEXT RECORD  WITH UNLOCK  AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSTR_PNAME1 BY REFERENCE JSTR-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-EX
           END-IF
           IF  JSTR-17   <  2
               GO  TO  MAIN-RTN
           END-IF
           IF  ZERO  =  JSTR-121(01) AND JSTR-121(02) AND
                        JSTR-121(03) AND JSTR-121(04) AND
                        JSTR-121(05) AND JSTR-121(06) AND
                        JSTR-121(07) AND JSTR-121(08) AND
                        JSTR-121(09) AND JSTR-121(10)
               GO  TO  MAIN-RTN
           END-IF
           PERFORM  JTW-RTN     THRU  JTW-EX.
      *
           IF  COMPLETION_CODE   =  255
               GO  TO  MAIN-EX
           END-IF
           GO TO     MAIN-RTN.
       MAIN-EX.
           EXIT.
      *
      *********************************************
      *    ‚h‚m‚h|‚q‚s‚m                         *
      *********************************************
       INI-RTN.
           CALL "SD_Output" USING "CLR-1" CLR-1 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA1" DSP-AREA1 "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING
            USER_ID BY REFERENCE COMPLETION_CODE  000.
       INI-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-CHK "ACP-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT          =   "P9"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   INI-EX
           END-IF
           IF  ESTAT    NOT   =   "01"  AND  "06"
               GO  TO   INI-005
           END-IF
           IF  W-CHK    NOT   =    1    AND   9
               GO  TO   INI-005
           END-IF.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT          =   "P9"
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO   INI-EX
           END-IF
           IF  ESTAT          =   "09"
               GO  TO   INI-005
           END-IF
           IF  ESTAT    NOT   =   "01"  AND  "06"
               GO  TO   INI-010
           END-IF
           IF  OKC            =   9
               GO  TO  INI-005
           END-IF
           IF  OKC      NOT   =   1
               GO  TO  INI-010
           END-IF
           IF  W-CHK          =   9
               GO  TO  INI-EX
           END-IF
           CALL  "CBLSTNNO"  USING  STN-NO USER_ID.
           MOVE  STN-NO-02   TO     WORK1-ID-02  WORK2-ID-02.
           MOVE  WF1-ID      TO     JT-W170K1ID.
           MOVE  JT-W170K1ID TO     JT-W170_PNAME1.
           MOVE  WF2-ID      TO     JT-W170K3ID.
           MOVE  JT-W170K3ID TO     JT-W170_PNAME2.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-W170_PNAME1 "SHARED" BY REFERENCE JT-W170_IDLST
            "2" "JTW-KEY1" BY REFERENCE JTW-KEY1 "JTW-KEY3" BY REFERENCE
            JTW-KEY3.
       INI-EX.
           EXIT.
      *********************************************
      *    ‚d‚m‚c|‚q‚s‚m                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-W170_IDLST JT-W170_PNAME1.
           CALL "SD_Output" USING "CLR-1" CLR-1 "p" RETURNING RESU.
       END-EX.
           EXIT.
      *
      *********************************************
      *    “üoŒÉ—ÝÏƒ[ƒN                       *
      *********************************************
       JTW-RTN.
           MOVE    SPACE      TO  JTW-R.
           INITIALIZE         JTW-R.
           MOVE    JSTR-09    TO  JTW-01.
           MOVE    JSTR-05    TO  HIZUKE.
           MOVE    HIZUKE     TO  JTW-02  JTW-16  JTW-20.
           IF  JSTR-03    =   0
               MOVE    22         TO  JTW-03
           ELSE
               IF  JSTR-03    =   5    OR    6
                   MOVE    23         TO  JTW-03
               ELSE
                   MOVE    25         TO  JTW-03
               END-IF
           END-IF
           MOVE    JSTR-01    TO  JTW-04  JTW-181 JTW-221.
           MOVE    JSTR-02    TO  JTW-05  JTW-182 JTW-222.
           MOVE    JSTR-07    TO  JTW-06.
           MOVE    JSTR-10    TO  JTW-07.
           MOVE    JSTR-1211 (1)  TO  JTW-081 (1).
           MOVE    JSTR-1211 (2)  TO  JTW-081 (2).
           MOVE    JSTR-1211 (3)  TO  JTW-081 (3).
           MOVE    JSTR-1211 (4)  TO  JTW-081 (4).
           MOVE    JSTR-1211 (5)  TO  JTW-081 (5).
           MOVE    JSTR-1211 (6)  TO  JTW-081 (6).
           MOVE    JSTR-1211 (7)  TO  JTW-081 (7).
           MOVE    JSTR-1211 (8)  TO  JTW-081 (8).
           MOVE    JSTR-1211 (9)  TO  JTW-081 (9).
           MOVE    JSTR-1211 (10) TO  JTW-081 (10).
           MOVE    0            TO  JTW-09  JTW-17  JTW-21.
           MOVE    JSTR-03      TO  JTW-10.
           MOVE    JSTR-06      TO  JTW-11.
           MOVE    JSTR-14B     TO  JTW-12.
           MOVE    JSTR-13      TO  JTW-13.
           MOVE    JSTR-14      TO  JTW-14.
           MOVE    JSTR-081     TO  JTW-151.
           MOVE    JSTR-082     TO  JTW-152.
           MOVE    JSTR-061     TO  JTW-19.
           MOVE    JSTR-14D     TO  JTW-23.
           MOVE    JSTR-15      TO  JTW-24.
           MOVE    ZERO         TO  JTW-90  JTW-91  JTW-92.
      *
           MOVE    JTW-KEY1     TO  ERR-K.
      *           WRITE   JTW-R     INVALID
      *//////////////
           CALL "DB_Insert" USING
            JT-W170_PNAME1 JT-W170_LNAME JTW-R RETURNING RET.
           IF  RET = 1
               CALL "DB_Rollback"
               MOVE  "JT-W170"    TO  ERR-F
               MOVE  "W"          TO  ERR-M
               PERFORM  ERR-RTN  THRU  ERR-EX
               CALL "C3_Set_Jrcode" USING
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           CALL "DB_Commit".
       JTW-EX.
           EXIT.
      *
       COPY LPMSG.
      *
