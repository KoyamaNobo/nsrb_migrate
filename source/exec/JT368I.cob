       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT368I.
       AUTHOR.                        -----------.
      ***************************************************
      *    PROGRAM        : éÛíçécè¡çûÇ›ÅiéÛíçì˙ï ÅjÅ@Å@*
      *    DATA WRITTEN   : 92/10/30                    *
      *    SCREEN USED    : ------                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : UNUSED                      *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       01  ACT-WORK.
           02  W-OK                  PIC 9(01).
           02  W-NGP.
               03  W-NEN             PIC  9(04).
               03  W-GET             PIC  9(02).
               03  W-PEY             PIC  9(02).
       COPY    LWMSG.
      *
      ***  éÛíçÉ}ÉXÉ^
      *FD  JMSTD
       01  JMSTD_JT368I.
           02  JMSTD_PNAME1          PIC  X(005) VALUE "JMST1".
           02  F                     PIC  X(001).
           02  JMSTD_PNAME2          PIC  X(005) VALUE "JMST2".
           02  F                     PIC  X(001).
           02  JMSTD_PNAME3          PIC  X(005) VALUE "JMST3".
           02  F                     PIC  X(001).
           02  JMSTD_LNAME           PIC  X(012) VALUE "JMSTD_JT368I".
           02  F                     PIC  X(001).
           02  JMSTD_KEY1            PIC  X(100) VALUE SPACE.
           02  JMSTD_SORT            PIC  X(100) VALUE SPACE.
           02  JMSTD_IDLST           PIC  X(100) VALUE SPACE.
           02  JMSTD_RES             USAGE  POINTER.
      *
       01  JMSTD-R.
           02   JMSTD-01                 PIC 9(1).
           02   JMSTD-02.
                03  JMSTD-021            PIC 9(4).
                03  JMSTD-022            PIC 9(2).
                03  JMSTD-023            PIC 9(2).
           02   JMSTD-KEY3.
                03  JMSTD-03             PIC 9(6).
                03  JMSTD-KEY2.
                    04  JMSTD-04         PIC 9(4).
                    04  JMSTD-05         PIC 9(6).
                    04  JMSTD-06.
                        05  JMSTD-061    PIC 9(4).
                        05  JMSTD-062    PIC 9(2).
                        05  JMSTD-063    PIC 9(2).
                    04  JMSTD-KEY1.
                        05  JMSTD-07     PIC 9(6).
                        05  JMSTD-08     PIC 9(1).
           02   JMSTD-09                 PIC 9(1).
           02   JMSTD-10                 PIC 9(3).
           02   JMSTD-11.
                03  JMSTD-111            OCCURS  10.
                    04  JMSTD-1111       PIC S9(6)   COMP-3.
           02   JMSTD-12.
                03  JMSTD-121            OCCURS  10.
                    04  JMSTD-1211       PIC S9(6)   COMP-3.
           02   JMSTD-14.
                03  JMSTD-141  OCCURS 10 PIC S9(06)  COMP-3.
           02   JMSTD-15.
                03  JMSTD-151  OCCURS 10 PIC S9(06)  COMP-3.
           02   JMSTD-16                 PIC S9(03).
           02   F                        PIC X(04).
           02   JMSTD-18                 PIC 9(01).
           02   FILLER                   PIC X(07).
           02   JMSTD-20                 PIC 9(03).
           02   JMSTD-13                 PIC N(32).
           02   JMSTD-21                 PIC 9(01).
           02   FILLER                   PIC X(54).
       77  F                             PIC X(01).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(22) VALUE "éÛÅ@íçÅ@écÅ@è¡Å@çûÅ@Ç›".
           02  DSP-07.
               03  FILLER  PIC  X(06) VALUE "éÛíçì˙".
               03  FILLER  PIC  X(10) VALUE "    /  /  ".
               03  FILLER  PIC  X(10) VALUE "Ç‹Ç≈è¡çûÇ›".
           02  DSP-23.
               03  FILLER  PIC  X(06) VALUE "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE "Åj".
               03  FILLER  PIC  X(04) VALUE "--->".
               03  FILLER  PIC  X(04) VALUE "ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-NEN     PIC 9(04).
           02  ACP-GET     PIC 9(02).
           02  ACP-PEY     PIC 9(02).
           02  ACP-OK      PIC 9(01).
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "73" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "22" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "X" "1" "21" "22" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "7" "0" "26" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "7" "17" "6" " " "DSP-07" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-07" "X" "7" "25" "10" "01DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-07" "X" "7" "37" "10" "02DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-23" " " "23" "0" "25" "DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-23" "X" "23" "41" "6" " " "DSP-23" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-23" "X" "23" "47" "9" "01DSP-23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-23" "X" "23" "56" "2" "02DSP-23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-23" "X" "23" "58" "4" "03DSP-23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-23" "X" "23" "63" "4" "04DSP-23" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "9" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NEN" "9" "7" "25" "4" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-NEN" BY REFERENCE W-NEN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GET" "9" "7" "30" "2" "ACP-NEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-PEY" "9" "7" "33" "2" "ACP-GET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "62" "1" "ACP-PEY" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-NEN "ACP-NEN" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "00"  AND   "01"   AND    "06"
               GO  TO  MR010
           END-IF
           IF  W-NEN   <  1980
               GO  TO  MR010
           END-IF.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACP-GET "ACP-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "00"  AND   "01"   AND    "06"
               GO  TO  MR020
           END-IF
           IF  (W-GET  NOT =  99)  AND  (W-GET  <  1  OR  >  12)
               GO  TO  MR020
           END-IF.
       MR025.
           CALL "SD_Accept" USING BY REFERENCE ACP-PEY "ACP-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR025
           END-IF
           IF  (W-PEY  NOT =  99)  AND  (W-GET  <  1  OR  >  12)
               GO  TO  MR025
           END-IF.
       MR030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR025
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR030
           END-IF
           IF  W-OK    =  9
               GO  TO  MR010
           END-IF
           IF  W-OK   NOT  =  1
               GO  TO  MR030
           END-IF.
      *-----------------------------------------------------------------
       MR040.
      *           READ    JMSTD     NEXT      AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           IF  JMSTD-02  >  W-NGP
               GO  TO  MR040
           END-IF.
       MR050.
           PERFORM     DEL-RTN     THRU      DEL-EX.
           GO  TO  MR040.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           INITIALIZE     ACT-WORK.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "EXCLUSIVE" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY3" BY REFERENCE JMSTD-KEY3 "JMSTD-KEY1"
            BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2" BY REFERENCE
            JMSTD-KEY2.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      *********************************************
      *    ÇcÇdÇkÅ|ÇqÇsÇm                         *
      *********************************************
       DEL-RTN.
      *           DELETE      JMSTD        INVALID
      *///////////////
           CALL "DB_Delete" USING JMSTD_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE    "JMSTD"   TO   ERR-F
               MOVE    "D"       TO   ERR-M
               MOVE   JMSTD-KEY1 TO   ERR-K
               PERFORM   ERR-RTN  THRU   ERR-EX
           END-IF.
       DEL-EX.
           EXIT.
       COPY LPMSG.
