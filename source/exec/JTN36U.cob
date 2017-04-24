       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 JTN36U.
      **************************************************************************
      *    PROGRAM  :  åééüåJâzämîF                                            *
      *    COMPILE  :  CBL85(74MODE)                                           *
      **************************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       SOURCE-COMPUTER.            SYSTEM3100.
       OBJECT-COMPUTER.            SYSTEM3100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       01  ERR-STAT                PIC  X(02).
       01  W-FILE                  PIC  X(13).
       01  WORK-AREA.
           03  W-NGD.
             04  W-NEND            PIC  9(04).
             04  W-NENL  REDEFINES W-NEND.
               05  W-NEND1         PIC  9(02).
               05  W-NEND2         PIC  9(02).
             04  W-GETD            PIC  9(02).
           03  OKC                 PIC  9(01).
           03  W-MSG               PIC  N(05).
           COPY    LWMSG.
      *
           COPY    L-JCON.
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
               05  FILLER  PIC X(22) VALUE   "                      ".
               05  FILLER  PIC N(10) VALUE "åéÅ@Å@éüÅ@Å@åJÅ@Å@âz".
           03  FILLER.
               05  FILLER  PIC X(21) VALUE   "Åi  '  îN   åé ï™  Åj".
           03  FILLER.
               05  FILLER  PIC X(26) VALUE
                                 "ämîF(OK=1,NO=9)-->    ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-NEN  PIC  9(02).
           02  ACP-GET  PIC  9(02).
           02  ACP-OKC  PIC  9(01).
       01  DSP-MSG.
           02  FILLER  PIC  N(05).
       01  DSP-ERR.
           02  ERR-MSG1   PIC  N(07) VALUE
               "ÇiÇbÇnÇmÅ@Ç»Çµ".
           COPY LSSEM.
           COPY LSMSG.
      ***************************************
       PROCEDURE                   DIVISION.
      ***************************************
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "1" "0" "54" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLEAR" "X" "1" "0" "12" " " "01DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201DSP-AREA" "RX" "1" "25" "22" "CLEAR" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0301DSP-AREA" "N" "1" "26" "20" "0201DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" " " "12" "0" "21" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-AREA" "X" "12" "29" "21" " " "02DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" " " "24" "0" "26" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DSP-AREA" "X" "24" "41" "26" " " "03DSP-AREA"
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NEN" "9" "12" "34" "2" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-NEN" BY REFERENCE W-NEND2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GET" "9" "12" "39" "2" "ACP-NEN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACP-GET" BY REFERENCE W-GETD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "61" "1" "ACP-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-MSG
       CALL "SD_Init" USING 
            "DSP-MSG" " " "16" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MSG" "N" "16" "32" "10" " " "DSP-MSG" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-MSG" BY REFERENCE W-MSG "10" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "24" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" "N" "24" "1" "14" " " "DSP-ERR" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM   INI-RTN   THRU   INI-EX.
       OWARI.
           CALL "DB_Close".
           STOP      RUN.
      ***************************************
      *    èâä˙èàóù                         *
      ***************************************
       INI-RTN.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE    SPACE          TO  JCON6-KEY.
           MOVE    6              TO  JCON6-01.
      *           READ    JCON       UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                 "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           MOVE    JCON6-03           TO  W-NGD.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "ACP-NEN" ACP-NEN "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-GET" ACP-GET "p" RETURNING RESU.
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
           END-IF.
       INI-EX.
           EXIT.
      *******************    E N D    O F    P R O G R A M    ******************
