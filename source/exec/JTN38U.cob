       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JT370U.
      **************************************************************************
      *    PROGRAM  :  ‘q•ÊÝŒÉEŽó’ƒ}ƒXƒ^[@o‰×Žw}”@‹­§XV            *
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
           COPY    LJMSTD.
           COPY    L-JSTR.
           COPY    L-TDIF.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           02  FILLER.
             03  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
             03  FILLER  PIC X(46) VALUE
                  "                                              ".
             03  FILLER  PIC N(22) VALUE
                  "@@@@@Žó’ƒ}ƒXƒ^[@o‰×Žw}”@‹­§C³".
           02  FILLER.
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
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "102" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-AREA" "RX" "1" "19" "46" "CLEAR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "N" "1" "20" "44" "0201DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" " " "24" "0" "0" "01DSP-AREA" " "
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
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 " " BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TDIF_PNAME1 " " BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 " " BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2" BY
            REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE JMSTD-KEY3.
           PERFORM   UP3-RTN   THRU   UP3-EX.
           PERFORM   UP4-RTN   THRU   UP4-EX.
           PERFORM   UP5-RTN   THRU   UP5-EX.
           PERFORM   END-RTN   THRU   END-EX.
       OWARI.
           CALL "DB_Close".
           STOP      RUN.
      ***************************************
      *    ‰Šúˆ—                         *
      ***************************************
       INI-RTN.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
            RETURNING RESU.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT      NOT  =  "01"  AND  "06"
               GO  TO  INI-030
           END-IF
           IF  OKC             =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  INI-EX
           END-IF
           IF  OKC        NOT  =  1
               GO  TO  INI-030
           END-IF.
      *
       INI-EX.
           EXIT.
      ***************************************
      *    ‚d‚m‚c|‚q‚s‚m                   *
      ***************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
       END-EX.
           EXIT.
      ***************************************
      *    Žw}”@ƒNƒŠƒA@i‚i‚l‚r‚s‚cj   *
      ***************************************
       UP3-RTN.
      *           READ      JMSTD  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP3-EX
           END-IF
           INITIALIZE       JMSTD-15.
           PERFORM   REW2-RTN  THRU   REW2-EX.
           GO   TO   UP3-RTN.
       UP3-EX.
           EXIT.
      ***************************************
      *    Žw}”‚`‚c‚c@i‚i‚r‚s‚qj       *
      ***************************************
       UP4-RTN.
      *           READ      JSTR        NEXT   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP4-EX
           END-IF
           IF  JSTR-17            =   1
               GO  TO  UP4-RTN
           END-IF
           IF  JSTR-081           =   ZERO  OR  999999
               GO  TO  UP4-RTN
           END-IF
      *
           MOVE      JSTR-081           TO  JMSTD-07.
           MOVE      JSTR-082           TO  JMSTD-08.
      *           READ      JMSTD       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP4-RTN
           END-IF
      *
           PERFORM   DAT2-RTN  THRU   DAT2-EX.
           PERFORM   REW2-RTN  THRU   REW2-EX.
           GO  TO  UP4-RTN.
       UP4-EX.
           EXIT.
      ***************************************
      *    Žw}”‚`‚c‚c@i‚s‚c‚h‚ej       *
      ***************************************
       UP5-RTN.
      *           READ      TDIF        NEXT   AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP5-EX
           END-IF
           IF  TDI-UPC       NOT  =   0
               GO  TO  UP5-RTN
           END-IF
           IF  TDI-JNO            =   ZERO
               GO  TO  UP5-RTN
           END-IF
           IF  TDI-HCD            >   999899
               GO  TO  UP5-RTN
           END-IF
      *
           MOVE      TDI-JNO            TO  JMSTD-07.
           MOVE      TDI-JGN            TO  JMSTD-08.
      *           READ      JMSTD       INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP5-RTN
           END-IF
      *
           ADD   TDI-SU                 TO  JMSTD-151(TDI-SNO).
           PERFORM   REW2-RTN  THRU   REW2-EX.
           GO  TO  UP5-RTN.
       UP5-EX.
           EXIT.
      ***************************************
      *    Žó’ƒ}ƒXƒ^@‚q‚d‚v‚q‚h‚s‚d       *
      ***************************************
       REW2-RTN.
      *           REWRITE   JMSTD-R INVALID
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE    "JMSTD"     TO  ERR-F
               MOVE    "R"         TO  ERR-M
               MOVE     JMSTD-KEY1 TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       REW2-EX.
           EXIT.
      *****************************************
      *    Žó’‚l@‚c‚`‚s‚`ƒZƒbƒgi‚i‚r‚s‚qj *
      *****************************************
       DAT2-RTN.
           MOVE      ZERO               TO  CNT.
       DAT2-010.
           ADD       1                  TO  CNT.
           IF  CNT                   >  10
               GO  TO  DAT2-EX
           END-IF
      *
           ADD   JSTR-1111(CNT)         TO  JMSTD-151(CNT)
           GO  TO  DAT2-010.
       DAT2-EX.
           EXIT.
      *****
           COPY    LPMSG.
      *******************    E N D    O F    P R O G R A M    **********
