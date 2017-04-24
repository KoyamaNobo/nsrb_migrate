       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT293U.
       AUTHOR.                        ___________.
      ***************************************************
      *    PROGRAM        : “¾ˆÓæ•ÊŽó’Žc’Šo@@@    *
      *    DATA WRITTEN   : 01/02/02                    *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : JIPS                        *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC  X(02)   VALUE SPACE.
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  ACT-WORK.
           02  W-PC                  PIC 9(01).
           02  W-CHK                 PIC 9(01).
           02  W-FTCD                PIC 9(04).
           02  W-TTCD                PIC 9(04).
           02  W-FHCD                PIC 9(06).
           02  W-THCD                PIC 9(06).
           02  W-SEN                 PIC 9(01).
           02  W-OK                  PIC 9(01).
           02  W-JMST                PIC X(222).
           02  W-90                  PIC 9(01).
       COPY    LWMSG.
      *
           COPY  LJMST2.
           COPY  LTWK04.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER   PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLE-02.
           02  FILLER   PIC  X(01) VALUE " ".
           02  FILLER   PIC  X(04) VALUE "    ".
           02  FILLER   PIC  X(04) VALUE "    ".
           02  FILLER   PIC  X(01) VALUE " ".
           02  FILLER   PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER   PIC  X(36) VALUE
                     "                                    ".
               03  FILLER   PIC  X(44) VALUE
                    "“¾ˆÓæ•Ê@Žó’E—a‚èEŽæ‚æ‚¯@Žc’ @@@”’Ž†".
           02  DSP-21.
               03  FILLER   PIC  X(06) VALUE  "Žó@’".
               03  FILLER   PIC  X(07) VALUE  " = 0 , ".
               03  FILLER   PIC  X(06) VALUE  "—a@‚è".
               03  FILLER   PIC  X(07) VALUE  " = 5 , ".
               03  FILLER   PIC  X(06) VALUE  "Žæ‚æ‚¯".
               03  FILLER   PIC  X(08) VALUE  " = 6    ".
           02  DSP-22.
               03  FILLER   PIC  X(33) VALUE
                    "Žw}ŠÜ‚Ü‚È‚¢ = 0  ,  ŠÜ‚Þ = 1    ".
           02  DSP-02.
               03  FILLER   PIC  X(06) VALUE  "“¾ˆÓæ".
               03  FILLER   PIC  X(04) VALUE  "•i–¼".
           02  DSP-03.
               03  FILLER   PIC  X(08) VALUE  "‚e‚q‚n‚l".
           02  DSP-04.
               03  FILLER   PIC  X(04) VALUE  "‚s‚n".
           02  DSP-08  PIC  X(09) VALUE  "‚O ‹³@ˆç".
           02  DSP-09  PIC  X(09) VALUE  "‚P ˆê@”Ê".
           02  DSP-10  PIC  X(09) VALUE  "‚X ‘S@Œ".
           02  DSP-11  PIC  X(08) VALUE  "‘I‘ð [ ]".
           02  DSP-07.
               03  FILLER   PIC  X(06) VALUE  "Šm”Fi".
               03  FILLER   PIC  X(09) VALUE  "OK=1,NO=9".
               03  FILLER   PIC  X(02) VALUE  "j".
               03  FILLER   PIC  X(08) VALUE  "--> ØÀ°Ý".
       01  ACP-AREA.
           02  ACP-PC      PIC 9(01).
           02  ACP-CHK     PIC 9(01).
           02  ACP-FTCD    PIC 9(04).
           02  ACP-TTCD    PIC 9(04).
           02  ACP-FHCD    PIC 9(06).
           02  ACP-THCD    PIC 9(06).
           02  ACP-SEN     PIC 9(01).
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
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *CLE-02
       CALL "SD_Init" USING 
            "CLE-02" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLE-02" "X" "5" "60" "1" " " "CLE-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLE-02" "X" "10" "34" "4" "01CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLE-02" "X" "12" "34" "4" "02CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLE-02" "X" "16" "56" "1" "03CLE-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLE-02" "X" "23" "61" "1" "04CLE-02" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "235" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "80" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "23" "36" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "24" "44" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-21" " " "5" "0" "40" "DSP-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-21" "X" "5" "21" "6" " " "DSP-21"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-21" "X" "5" "27" "7" "01DSP-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-21" "X" "5" "34" "6" "02DSP-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-21" "X" "5" "40" "7" "03DSP-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-21" "X" "5" "47" "6" "04DSP-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-21" "X" "5" "53" "8" "05DSP-21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-22" " " "8" "0" "33" "DSP-21" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-22" "X" "8" "25" "33" " " "DSP-22"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "10" "0" "10" "DSP-22" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "10" "33" "6" " " "DSP-02"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-02" "X" "10" "42" "4" "01DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "12" "0" "8" "DSP-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "12" "22" "8" " " "DSP-03"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-04" " " "14" "0" "4" "DSP-03" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-04" "X" "14" "22" "4" " " "DSP-04"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-08" "X" "12" "51" "9" "DSP-04" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" "X" "14" "51" "9" "DSP-08" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" "X" "16" "51" "9" "DSP-09" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "X" "18" "52" "8" "DSP-10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "23" "0" "25" "DSP-11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "23" "41" "6" " " "DSP-07"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-07" "X" "23" "47" "9" "01DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-07" "X" "23" "56" "2" "02DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-07" "X" "23" "58" "8" "03DSP-07" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "24" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-PC" "9" "5" "60" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-PC" BY REFERENCE W-PC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-CHK" "9" "8" "57" "1" "ACP-PC" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FTCD" "9" "12" "34" "4" "ACP-CHK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FTCD" BY REFERENCE W-FTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TTCD" "9" "14" "34" "4" "ACP-FTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TTCD" BY REFERENCE W-TTCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FHCD" "9" "12" "41" "6" "ACP-TTCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FHCD" BY REFERENCE W-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-THCD" "9" "14" "41" "6" "ACP-FHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-THCD" BY REFERENCE W-THCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "18" "58" "1" "ACP-THCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "61" "1" "ACP-SEN" " "  RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ƒƒCƒ“@ƒ‹[ƒ`ƒ“              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR005.
           CALL "SD_Accept" USING BY REFERENCE ACP-PC "ACP-PC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR005
           END-IF
           CALL "SD_Output" USING "ACP-PC" ACP-PC "p" RETURNING RESU.
           IF  W-PC  NOT  =  0  AND  5  AND  6
               GO  TO  MR005
           END-IF.
       MR007.
           CALL "SD_Accept" USING BY REFERENCE ACP-CHK "ACP-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR005
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR007
           END-IF
           CALL "SD_Output" USING "ACP-CHK" ACP-CHK "p" RETURNING RESU.
           IF  W-CHK  >   1
               GO  TO  MR007
           END-IF.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FTCD "ACP-FTCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR007
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR010
           END-IF
           CALL "SD_Output" USING
            "ACP-FTCD" ACP-FTCD "p" RETURNING RESU.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TTCD "ACP-TTCD"
            "9" "4" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR010
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR020
           END-IF
           CALL "SD_Output" USING
            "ACP-TTCD" ACP-TTCD "p" RETURNING RESU.
           IF  W-FTCD   >  W-TTCD
               GO  TO  MR010
           END-IF.
       MR022.
           CALL "SD_Accept" USING BY REFERENCE ACP-FHCD "ACP-FHCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR020
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR022
           END-IF
           CALL "SD_Output" USING
            "ACP-FHCD" ACP-FHCD "p" RETURNING RESU.
       MR024.
           CALL "SD_Accept" USING BY REFERENCE ACP-THCD "ACP-THCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR022
           END-IF
           IF  ESTAT  NOT  =  "01"   AND    "06"
               GO  TO  MR024
           END-IF
           CALL "SD_Output" USING
            "ACP-THCD" ACP-THCD "p" RETURNING RESU.
           IF  W-FHCD   >  W-THCD
               GO  TO  MR022
           END-IF.
       MR035.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR024
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  MR035
           END-IF
           IF  W-SEN   NOT  = 0 AND 1 AND 9
               GO  TO  MR035
           END-IF.
       MR040.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR035
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR040
           END-IF
           IF  W-OK   NOT  =  "1"   AND      "9"
               GO  TO  MR040
           END-IF
           IF  W-OK        =  "9"
               CALL "SD_Output" USING "CLE-02" CLE-02 "p" RETURNING RESU
               INITIALIZE   ACT-WORK
               GO  TO  MR005
           END-IF
      *
           MOVE    W-FTCD      TO     JMST2-04.
           MOVE    W-FHCD      TO     JMST2-05.
           MOVE    0           TO     JMST2-06.
           MOVE    0           TO     JMST2-07.
           MOVE    0           TO     JMST2-08.
           MOVE    0           TO     JMST2-09.
      *           START    JMST2  KEY  NOT  <  JMST2-KEY    INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST2_PNAME1 "JMST2-KEY" " NOT < " JMST2-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF.
       MR050.
      *           READ    JMST2   NEXT   UNLOCK    AT      END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST2_PNAME1 BY REFERENCE JMST2-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           IF  W-PC    NOT  =  JMST2-01
               GO  TO  MR050
           END-IF
           MOVE    JMST2-90    TO     W-90.
           IF  W-90         =  2
               MOVE    1           TO     W-90
           END-IF
           IF  W-SEN   NOT  =  9
               IF  W-SEN     NOT  =  W-90
                   GO  TO  MR050
               END-IF
           END-IF
           IF  W-FTCD   >  JMST2-04
               GO  TO  MR050
           END-IF
           IF  W-TTCD   <  JMST2-04
               GO  TO  MR999
           END-IF
           IF  W-FHCD   >  JMST2-05
               GO  TO  MR050
           END-IF
           IF  W-THCD   <  JMST2-05
               GO  TO  MR050
           END-IF.
       MR060.
           MOVE    SPACE       TO     WK04-R.
           INITIALIZE    WK04-R.
           MOVE    JMST2-R     TO     W-JMST.
           MOVE    W-JMST      TO     WK04-R.
           MOVE    W-CHK       TO     WK04-88.
           MOVE    W-PC        TO     WK04-89.
           MOVE    W-SEN       TO     WK04-90.
           MOVE    JMST2-17    TO     WK04-17.
           MOVE    JMST2-22    TO     WK04-22.
           MOVE    JMST2-91    TO     WK04-91.
      *           WRITE   WK04-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK04_PNAME1 JT-WK04_LNAME WK04-R RETURNING RET.
           GO  TO  MR050.
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
            "INPUT" JMST2_PNAME1 "SHARED" BY REFERENCE JMST2_IDLST "1"
            "JMST2-KEY" BY REFERENCE JMST2-KEY.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
       INT-EX.
           EXIT.
      *********************************************
      *    ‚d‚m‚c|‚q‚s‚m                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST2_IDLST JMST2_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
       COPY    LPMSG.
