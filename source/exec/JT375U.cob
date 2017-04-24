       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JT375U.
      **************************************************************************
      *    PROGRAM  :  ì`ï[áÇåüçıÉtÉ@ÉCÉãÅ@çÏê¨                                *
      *    COMPILE  :  CBL85(74MODE)                                           *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       01  ERR-STAT                PIC  X(02).
       01  WORK-AREA.
           03  OKC                 PIC  9(01).
           COPY    LWMSG.
      *
           COPY    LJMST1.
           COPY    LTDNKN.
           COPY    L-JSTR.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           03  FILLER.
               05  CLEAR   PIC X(12) VALUE "CLEAR SCREEN".
               05  FILLER  PIC X(20) VALUE   "                    ".
               05  FILLER  PIC N(09) VALUE "ì`ï[áÇåüçıÇeÅ@çÏê¨".
           03  FILLER.
               05  FILLER  PIC X(26) VALUE
                                 "ämîF(OK=1,NO=9)-->    ÿ¿∞›".
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
            "DSP-AREA" " " "0" "0" "76" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "50" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
           "0201DSP-AREA" "RX" "1" "25" "20" "CLEAR" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "N" "1" "26" "18" "0201DSP-AREA" " "
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
           CALL "DB_F_Open" USING
            "OUTPUT" JT-DNKN_PNAME1 " " BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JT-DNKN_PNAME1 " " BY REFERENCE JT-DNKN_IDLST "1"
            "DNKN-KEY" BY REFERENCE DNKN-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JMST1_PNAME1 " " BY REFERENCE JMST1_IDLST "1"
            "JMST1-KEY1" BY REFERENCE JMST1-KEY1.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 " " BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
       INI-EX.
           EXIT.
      ***************************************
      *    èIóπèàóù                         *
      ***************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-DNKN_IDLST JT-DNKN_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST1_IDLST JMST1_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
       END-EX.
           EXIT.
      ***************************************
      *    ì`ï[áÇåüçıÇeçÏê¨Å@ÅiéÛíçÇlï™Åj   *
      ***************************************
       UP1-RTN.
      *           READ      JMST1  NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST1_PNAME1 BY REFERENCE JMST1-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-EX
           END-IF
           MOVE      1            TO  DNKN-01.
           MOVE      JMST1-03     TO  DNKN-02.
           MOVE      2            TO  DNKN-03.
           MOVE      JMST1-KEY1   TO  DNKN-04.
      *           READ      JT-DNKN     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP1-010
           END-IF
           GO  TO  UP1-RTN.
       UP1-010.
      *           WRITE     DNKN-R      INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JT-DNKN_PNAME1 JT-DNKN_LNAME DNKN-R RETURNING RET.
           IF  RET = 1
               MOVE    "JT-DNKN"   TO  ERR-F
               MOVE    "W"         TO  ERR-M
               MOVE     DNKN-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  UP1-RTN.
       UP1-EX.
           EXIT.
      ***************************************
      *    ì`ï[áÇåüçıÇeçÏê¨Å@Åiéwê}Çeï™Åj   *
      ***************************************
       UP2-RTN.
      *           READ      JSTR   NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-EX
           END-IF
           MOVE      JSTR-07      TO  DNKN-01.
           MOVE      JSTR-09      TO  DNKN-02.
           MOVE      3            TO  DNKN-03.
           MOVE      JSTR-KEY     TO  DNKN-04.
      *           READ      JT-DNKN     INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JT-DNKN_PNAME1 BY REFERENCE DNKN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  UP2-010
           END-IF
           GO  TO  UP2-RTN.
       UP2-010.
      *           WRITE     DNKN-R      INVALID  KEY
      *//////////////
           CALL "DB_Insert" USING
            JT-DNKN_PNAME1 JT-DNKN_LNAME DNKN-R RETURNING RET.
           IF  RET = 1
               MOVE    "JT-DNKN"   TO  ERR-F
               MOVE    "W"         TO  ERR-M
               MOVE     DNKN-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  UP2-RTN.
       UP2-EX.
           EXIT.
           COPY    LPMSG.
      *******************    E N D    O F    P R O G R A M    **********
