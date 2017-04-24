       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT365I.
       AUTHOR.                        -----------.
      ***************************************************
      *    PROGRAM        : Žó’ŽcÁž‚Ýi•i–¼•Êj@@@*
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
           02  W-KEY                 PIC 9(06).
           02  CHK                   PIC 9(01)   VALUE   0.
           02  CNT                   PIC 9(01)   VALUE   0.
           02  I                     PIC 9(02)   VALUE   0.
           02  W-ZAN                 PIC S9(06)  OCCURS  10.
           02  W-TU                  PIC S9(07).
           02  W-AREA1.
               03  W-FROM1           PIC  9(06).
               03  W-TO1             PIC  9(06).
               03  W-FROM2           PIC  9(06).
               03  W-TO2             PIC  9(06).
               03  W-FROM3           PIC  9(06).
               03  W-TO3             PIC  9(06).
       COPY    LWMSG.
      *
      ***  Žó’ƒ}ƒXƒ^
      *FD  JMSTD
       01  JMSTD_JT365I.
           02  JMSTD_PNAME1          PIC  X(005) VALUE "JMST1".
           02  F                     PIC  X(001).
           02  JMSTD_PNAME2          PIC  X(005) VALUE "JMST2".
           02  F                     PIC  X(001).
           02  JMSTD_PNAME3          PIC  X(005) VALUE "JMST3".
           02  F                     PIC  X(001).
           02  JMSTD_LNAME           PIC  X(012) VALUE "JMSTD_JT365I".
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
       01  CLE-AREA.
           02  CLE-02.
               03  FILLER  PIC  X(06) VALUE   "      ".
               03  FILLER  PIC  X(06) VALUE   "      ".
               03  FILLER  PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(22) VALUE "Žó@’@Žc@Á@ž@‚Ý".
           02  DSP-02.
               03  FILLER  PIC  X(04) VALUE "•i–¼".
               03  FILLER  PIC  X(02) VALUE "`".
           02  DSP-03.
               03  FILLER  PIC  X(02) VALUE "`".
           02  DSP-04.
               03  FILLER  PIC  X(02) VALUE "`".
           02  DSP-12.
               03  FILLER  PIC  X(06) VALUE "Šm”Fi".
               03  FILLER  PIC  X(09) VALUE "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE "j".
               03  FILLER  PIC  X(04) VALUE "--->".
               03  FILLER  PIC  X(04) VALUE "ØÀ°Ý".
       01  ACP-AREA.
           02  ACP-FROM1   PIC 9(06).
           02  ACP-TO1     PIC 9(06).
           02  ACP-FROM2   PIC 9(06).
           02  ACP-TO2     PIC 9(06).
           02  ACP-FROM3   PIC 9(06).
           02  ACP-TO3     PIC 9(06).
           02  ACP-OK      PIC 9(01).
       01  DSP-ERR.
           02  INV-01      PIC  X(20) VALUE
             "Žó’ƒ}ƒXƒ^[@–¢“o˜^".
           02  INV-03      PIC  X(24) VALUE
             "—a‚è‹æ•ªƒGƒ‰[@Áž•s‰Â".
           02  INV-04      PIC  X(20) VALUE
             "o‰×ˆ—’†@Áž•s‰Â".
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
      *CLE-AREA
       CALL "SD_Init" USING 
            "CLE-AREA" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "13" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-02" "X" "5" "26" "6" " " "CLE-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-02" "X" "5" "36" "6" "01CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-02" "X" "23" "62" "1" "02CLE-02" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "57" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "22" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "X" "1" "21" "22" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "5" "0" "6" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "5" "21" "4" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-02" "X" "5" "33" "2" "01DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "7" "0" "2" "DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "7" "33" "2" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "9" "0" "2" "DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "X" "9" "33" "2" " " "DSP-04" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-12" " " "23" "0" "25" "DSP-04" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-12" "X" "23" "41" "6" " " "DSP-12" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-12" "X" "23" "47" "9" "01DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-12" "X" "23" "56" "2" "02DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-12" "X" "23" "58" "4" "03DSP-12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-12" "X" "23" "63" "4" "04DSP-12" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "37" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FROM1" "9" "5" "26" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FROM1" BY REFERENCE W-FROM1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TO1" "9" "5" "36" "6" "ACP-FROM1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TO1" BY REFERENCE W-TO1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FROM2" "9" "7" "26" "6" "ACP-TO1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FROM2" BY REFERENCE W-FROM2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TO2" "9" "7" "36" "6" "ACP-FROM2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TO2" BY REFERENCE W-TO2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FROM3" "9" "9" "26" "6" "ACP-TO2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FROM3" BY REFERENCE W-FROM3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TO3" "9" "9" "36" "6" "ACP-FROM3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TO3" BY REFERENCE W-TO3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "62" "1" "ACP-TO3" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "64" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-01" "X" "24" "1" "20" " " "DSP-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-03" "X" "24" "1" "24" "INV-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "INV-04" "X" "24" "1" "20" "INV-03" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ************************************
      *    ƒƒCƒ“@ƒ‹[ƒ`ƒ“              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM1 "ACP-FROM1"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO1 "ACP-TO1"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR020
           END-IF
           IF  W-FROM1  >  W-TO1
               GO  TO  MR020
           END-IF.
       MR021.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM2 "ACP-FROM2"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR021
           END-IF
           IF  W-FROM2  = ZERO
               MOVE ZERO TO W-TO2 W-FROM3 W-TO3
               CALL "SD_Output" USING
                "ACP-TO2" ACP-TO2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACP-FROM3" ACP-FROM3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "ACP-TO3" ACP-TO3 "p" RETURNING RESU
               GO TO MR030
           END-IF
           IF  W-FROM2  <  W-TO1
               GO TO MR021
           END-IF.
       MR022.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO2 "ACP-TO2"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR021
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR022
           END-IF
           IF  W-FROM2  >  W-TO2
               GO  TO  MR022
           END-IF.
       MR023.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM3 "ACP-FROM3"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR022
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR023
           END-IF
           IF  W-FROM3  = ZERO
               MOVE ZERO TO W-TO3
               CALL "SD_Output" USING
                "ACP-TO3" ACP-TO3 "p" RETURNING RESU
               GO TO MR030
           END-IF
           IF  W-FROM3  <  W-TO2
               GO TO MR021
           END-IF.
       MR024.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO3 "ACP-TO3"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR023
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR024
           END-IF
           IF  W-FROM3  >  W-TO3
               GO  TO  MR024
           END-IF.
       MR030.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  NOT =  "09"
               GO  TO  MR035
           END-IF
           IF  W-TO3  NOT =  ZERO
               GO  TO  MR024
           END-IF
           IF  W-TO2  NOT =  ZERO
               GO  TO  MR022
           END-IF
           GO  TO  MR020.
       MR035.
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR030
           END-IF
           IF  W-OK    =  9
               CALL "SD_Output" USING
                 "CLE-AREA" CLE-AREA "p" RETURNING RESU
               GO  TO  MR010
           END-IF
           IF  W-OK   NOT  =  1
               GO  TO  MR030
           END-IF.
       MR040.
      *           READ    JMSTD     NEXT      AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           IF  JMSTD-01  NOT =  0
               GO  TO  MR040
           END-IF
           IF  JMSTD-03  >= W-FROM1 AND <= W-TO1
               GO  TO  MR050
           END-IF
           IF  JMSTD-03  >= W-FROM2 AND <= W-TO2
               GO  TO  MR050
           END-IF
           IF  JMSTD-03  >= W-FROM3 AND <= W-TO3
               GO  TO  MR050
           END-IF
           GO  TO  MR040.
       MR050.
           PERFORM     DEL-RTN     THRU      DEL-EX.
           GO  TO  MR040.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ‚h‚m‚s|‚q‚s‚m                         *
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
      *    ‚d‚m‚c|‚q‚s‚m                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      *********************************************
      *    ‚c‚d‚k|‚q‚s‚m                         *
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
