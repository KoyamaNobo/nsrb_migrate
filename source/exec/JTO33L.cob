       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       JTO33L.
      ******************************************************************
      *            èoâ◊éwê}èë  Åièoâ◊é¿ê—ÇeÅj                          *
      *    DATE. 90.05.10            BY. IKUMI.N                       *
      ******************************************************************
       ENVIRONMENT       DIVISION.
       CONFIGURATION     SECTION.
       SOURCE-COMPUTER.  SYSTEM3100.
       OBJECT-COMPUTER.  SYSTEM3100.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
       01  W-JS                  PIC 9.
       01  END-SW                PIC 9      VALUE 0.
       01  RED-SW                PIC 9      VALUE 0.
       01  TCM-SW                PIC 9      VALUE 0.
       01  WORK-1.
           02  F                 PIC X(102).
           02  WORK-11           PIC 9(6).
       01  WORK-2.
           02  F                 PIC X(33).
           02  WORK-M1           PIC N(02).
           02  WORK-M2           PIC X(01).
           02  WORK-21.
               03  WORK-211      PIC 9(2).
               03  F             PIC X(4).
               03  WORK-212      PIC ZZ.
               03  F             PIC X(4).
               03  WORK-213      PIC ZZ.
           02  F                 PIC X(09).
           02  WORK-22           PIC N(2).
           02  F                 PIC X(4).
           02  WORK-23           PIC N(06).
       01  WORK-3.
           02  F                 PIC X(60).
           02  WORK-31           PIC 9(6).
           02  F                 PIC X(37).
           02  WORK-33           PIC X(6).
       01  WORK-3A.
           02  F                 PIC X(04).
           02  WORK-34A.
             03  WORK-34F        PIC X(1).
             03  WORK-341        PIC 9(1).
             03  WORK-342        PIC X(1).
             03  WORK-343        PIC 9(1).
             03  WORK-34R        PIC X(1).
           02  F                 PIC X(94).
           02  WORK-32           PIC X(6).
       01  WORK-4.
           02  F                 PIC X(05).
           02  WORK-41           PIC 9(4).
           02  F                 PIC X(4).
           02  ST-CD1            PIC X(5).
           02  WORK-42           PIC N(26).
           02  F                 PIC X(5).
           02  WORK-43           PIC X(3).
           02  F                 PIC X(4).
           02  WORK-44           PIC N(26).
           02  ED-CD1            PIC X(5).
       01  WORK-5.
           02  F                 PIC X(06).
           02  ED-CD2            PIC X(5).
           02  F                 PIC X(18).
           02  WORK-NGP          PIC 99/99/99.
           02  WORK-NGPD  REDEFINES  WORK-NGP.
               03  F             PIC X(08).
           02  F                 PIC X(05).
           02  WORK-51F          PIC X(01).
           02  WORK-51           PIC X(10).
           02  WORK-51R          PIC X(01).
           02  F                 PIC X(02).
           02  WORK-52           OCCURS  10.
               03  WORK-521      PIC ----B.
           02  WORK-53           PIC ----.
           02  WORK-54           PIC X(01).
           02  WORK-55           PIC ZZZ.
       01  WORK-6.
           02  F                 PIC X(06).
           02  WORK-61           PIC 9(6).
           02  F                 PIC X.
           02  ST-CD3            PIC X(5).
           02  WORK-62           PIC N(24).
           02  ED-CD3            PIC X(5).
           02  F                 PIC X.
           02  WORK-63           PIC 9.
           02  F                 PIC X.
           02  WORK-64           OCCURS  10.
               03  WORK-641      PIC -----.
           02  WORK-65           PIC -------.
           02  WORK-65R  REDEFINES  WORK-65.
               03  WORK-651      PIC X(01).
               03  WORK-652      PIC ------.
           02  WORK-66           PIC X(01).
           02  WORK-67           PIC X.
       01  WORK-6A.
           02  F                 PIC X(101).
           02  WORK-61A          PIC ----.
           02  F                 PIC X(01).
           02  WORK-62A          PIC X(03).
       01  WORK-7.
           02  F                 PIC X(22).
           02  WORK-70           PIC ZZ9.
           02  WORK-70R  REDEFINES   WORK-70  PIC X(03).
           02  F                 PIC X(09).
           02  ST-CD4            PIC X(5).
           02  WORK-71.
               03  WORK-711      PIC N(5).
               03  WORK-712      PIC N.
           02  F                 PIC X(4).
           02  WORK-72.
               03  WORK-721      PIC N(23).
               03  WORK-722      PIC N(9).
           02  ED-CD4            PIC X(5).
           02  F                 PIC X(7).
           02  WORK-73           PIC -------.
       01  WORK-8.
           02  WORK-81           OCCURS  10.
               03  WORK-811      PIC S9(2).
           02  WORK-82           PIC S9(6).
       01  WORK-9.
           02  WORK-91           OCCURS  10.
               03  WORK-911      PIC S9(4).
           02  WORK-92           PIC S9(6).
       01  I                     PIC 9.
       01  J                     PIC 9(2).
       01  PAGE-C                PIC 9(3).
       01  GOKEI                 PIC S9(6).
       01  SEN-W                 PIC 9.
       01  TEST-W                PIC 9.
       01  DEN-W                 PIC 9(6).
       01  W-TCD                 PIC 9(4).
       01  W-TAN                 PIC 9(5).
       01  FROM-WK.
           02  KURA-W1           PIC 9(1).
           02  TKU-W1.
               03  TKU-W11       PIC 9(4).
               03  TKU-W12       PIC 9(3).
           02  DEN-W1            PIC 9(6).
       01  TO-WK.
           02  KURA-W2           PIC 9(1).
           02  TKU-W2.
               03  TKU-W21       PIC 9(4).
               03  TKU-W22       PIC 9(3).
           02  DEN-W2            PIC 9(6).
       01  KAKU-W                PIC 9.
       01  ERR-STAT              PIC X(2).
       01  WYMD                  PIC 9(6).
       01  KEY-WK.
           02  KEY-W1            PIC 9(1).
           02  KEY-W2            PIC X(7).
           02  KEY-W3            PIC 9(6).
       01  OLD-OKNO              PIC 9(6)  VALUE 0.
       01  SETSU                 PIC S9(3).
      **********************
           COPY  LWMSG.
      **********************
           COPY  L-JSJD.
           COPY  L-JCON.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LITM.
           COPY  LITHTM.
           COPY  LJMSTD.
      *FD  PRN-F
       77  PRN-R                 PIC X(180).
      *
       77  END-STS               PIC  X(002).
       77  RESU                  PIC  9(001).
       77  RESP                  PIC  9(001).
       77  RET                   PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER         PIC  9(003).
       77  USER_ID               PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE       PIC  X(003) VALUE ZERO.
      *
       01  ACP-NYURYOKU.
           02  ACP-JS.
               03  01ACP-JS    PIC 9 .
           02  ACP-SEN.
               03  01ACP-SEN   PIC 9 .
           02  ACP-TEST.
               03  01ACP-TEST  PIC 9 .
           02  ACP-KURA1.
               03  01ACP-KURA1 PIC 9(1).
           02  ACP-KURA2.
               03  01ACP-KURA2 PIC 9(1).
           02  ACP-TKU1.
               03  ACP-TKU11   PIC 9(4).
               03  ACP-TKU12   PIC 9(3).
           02  ACP-TKU2.
               03  ACP-TKU21   PIC 9(4).
               03  ACP-TKU22   PIC 9(3).
           02  ACP-DEN1.
               03  01ACP-DEN1  PIC 9(6).
           02  ACP-DEN2.
               03  01ACP-DEN2  PIC 9(6).
           02  ACP-KAKU.
               03  01ACP-KAKU  PIC 9 .
       01  DSP-GAMEN.
           02  FILLER  PIC  X(20) VALUE
                 " èoâ◊éwê}èëÅié¿ê—Åj ".
           02  FILLER.
               03  FILLER  PIC  X(25) VALUE "ã≥Å@àÁ=0 , àÍÅ@î =1 ...  ".
           02  FILLER  PIC  X(10) VALUE  "ÇPÅ@î≠Å@çs".
           02  FILLER.
               03  FILLER  PIC  X(18) VALUE "ÇQÅ@çƒî≠çsÅ@Å@ëIë".
               03  FILLER  PIC  X(01) VALUE   "[".
               03  FILLER  PIC  X(01) VALUE   "]".
           02  FILLER  PIC  X(18) VALUE  "ÉeÉXÉgÉvÉäÉìÉgàÛéö".
           02  FILLER.
               03  FILLER  PIC  X(14) VALUE   "(YES=1,NO=2) [".
               03  FILLER  PIC  X(01) VALUE   "]".
           02  FILLER  PIC  X(06) VALUE    "ëq∫∞ƒﬁ".
           02  FILLER  PIC  X(06) VALUE  "íºëóêÊ".
           02  FILLER  PIC  X(06) VALUE  "ì`ï[áÇ".
           02  FILLER  PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  FILLER  PIC  X(01) VALUE    "-".
           02  FILLER  PIC  X(04) VALUE  "ÇsÇn".
           02  FILLER  PIC  X(01) VALUE    "-".
           02  FILLER.
               03  FILLER  PIC  X(06) VALUE  "ämîFÅi".
               03  FILLER  PIC  X(09) VALUE    "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "Åj".
               03  FILLER  PIC  X(10) VALUE    "--->  ÿ¿∞›".
       01  DSP-CLER.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-CLER2.
           02  FILLER  PIC X(23)  VALUE "  ".
           02  FILLER  PIC X(01)  VALUE    "-".
           02  FILLER  PIC X(23)  VALUE "  ".
           02  FILLER  PIC X(01)  VALUE    "-".
      *
           COPY  LSERR.
      *
       PROCEDURE         DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "PRF-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACP-NYURYOKU
       CALL "SD_Init" USING 
            "ACP-NYURYOKU" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JS" " " "4" "0" "1" " " "ACP-NYURYOKU" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-JS" "9" "4" "45" "1" " " "ACP-JS" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" " " "8" "0" "1" "ACP-JS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-SEN" "9" "8" "45" "1" " " "ACP-SEN" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-SEN" BY REFERENCE SEN-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TEST" " " "12" "0" "1" "ACP-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-TEST" "9" "12" "45" "1" " " "ACP-TEST" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-TEST" BY REFERENCE TEST-W "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KURA1" " " "16" "0" "1" "ACP-TEST" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-KURA1" "9" "16" "33" "1" " " "ACP-KURA1"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KURA1" BY REFERENCE KURA-W1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KURA2" " " "17" "0" "1" "ACP-KURA1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-KURA2" "9" "17" "33" "1" " " "ACP-KURA2"
            RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KURA2" BY REFERENCE KURA-W2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU1" " " "16" "0" "7" "ACP-KURA2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU11" "9" "16" "39" "4" " " "ACP-TKU1" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU11" BY REFERENCE TKU-W11 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-TKU12" "9" "16" "44" "3" "ACP-TKU11" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU12" BY REFERENCE TKU-W12 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU2" " " "17" "0" "7" "ACP-TKU1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TKU21" "9" "17" "39" "4" " " "ACP-TKU2" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU21" BY REFERENCE TKU-W21 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-TKU22" "9" "17" "44" "3" "ACP-TKU21" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-TKU22" BY REFERENCE TKU-W22 "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN1" " " "16" "0" "6" "ACP-TKU2" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DEN1" "9" "16" "50" "6" " " "ACP-DEN1" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN1" BY REFERENCE DEN-W1 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DEN2" " " "17" "0" "6" "ACP-DEN1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-DEN2" "9" "17" "50" "6" " " "ACP-DEN2" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-DEN2" BY REFERENCE DEN-W2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-KAKU" " " "24" "0" "1" "ACP-DEN2" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-KAKU" "9" "24" "63" "1" " " "ACP-KAKU" RETURNING RESU.
       CALL "SD_Into" USING 
            "01ACP-KAKU" BY REFERENCE KAKU-W "1" "0" RETURNING RESU.
      *DSP-GAMEN
       CALL "SD_Init" USING 
            "DSP-GAMEN" " " "0" "0" "167" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-GAMEN" "RX" "1" "26" "20" " " "DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-GAMEN" " " "4" "0" "25" "01DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-GAMEN" "X" "4" "21" "25" " " "02DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-GAMEN" "X" "7" "25" "10" "02DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-GAMEN" " " "8" "0" "20" "03DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-GAMEN" "X" "8" "25" "18" " " "04DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-GAMEN" "X" "8" "44" "1" "0104DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304DSP-GAMEN" "X" "8" "46" "1" "0204DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-GAMEN" "X" "11" "21" "18" "04DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-GAMEN" " " "12" "0" "15" "05DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-GAMEN" "X" "12" "31" "14" " " "06DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-GAMEN" "X" "12" "46" "1" "0106DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-GAMEN" "X" "15" "31" "6" "06DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-GAMEN" "X" "15" "39" "6" "07DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-GAMEN" "X" "15" "50" "6" "08DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-GAMEN" "X" "16" "21" "8" "09DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-GAMEN" "X" "16" "43" "1" "10DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12DSP-GAMEN" "X" "17" "21" "4" "11DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "13DSP-GAMEN" "X" "17" "43" "1" "12DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "14DSP-GAMEN" " " "24" "0" "27" "13DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0114DSP-GAMEN" "X" "24" "41" "6" " " "14DSP-GAMEN"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0214DSP-GAMEN" "X" "24" "47" "9" "0114DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0314DSP-GAMEN" "X" "24" "56" "2" "0214DSP-GAMEN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0414DSP-GAMEN" "X" "24" "58" "10" "0314DSP-GAMEN" " "
            RETURNING RESU.
      *DSP-CLER
       CALL "SD_Init" USING
           "DSP-CLER" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01DSP-CLER" "X" "1" "0" "12" " " "DSP-CLER" RETURNING RESU.
      *DSP-CLER2
       CALL "SD_Init" USING 
            "DSP-CLER2" " " "0" "0" "48" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-CLER2" "X" "16" "33" "23" " " "DSP-CLER2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-CLER2" "X" "16" "43" "1" "01DSP-CLER2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-CLER2" "X" "17" "33" "23" "02DSP-CLER2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-CLER2" "X" "17" "43" "1" "03DSP-CLER2" " "
            RETURNING RESU.
      *
           COPY LSERR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MAINLINE-ROUTINE.
           PERFORM  INIT-RTN  THRU  INIT-RTN-EXIT.
           PERFORM  SLCT-RTN  THRU  SLCT-RTN-EXIT.
           IF  END-STS  =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  OPEN-RTN  THRU  OPEN-RTN-EXIT.
           PERFORM  PROC-RTN  THRU  PROC-RTN-EXIT
                    UNTIL  NOT  (END-SW  NOT  =  9).
           IF  PAGE-C  NOT =  1
               MOVE     SPACE     TO    PRN-R
               CALL "PR_Write" USING PRN-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
      *
           PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT.
       MAINLINE-END.
           CALL "DB_Close".
           STOP  RUN.
      ******************************************************************
      *    ÉÅÉCÉìèàóù  ÅiÇoÇqÇnÇbÅ|ÇqÇsÇmÅj                            *
      ******************************************************************
       PROC-RTN.
           PERFORM  READ1-RTN  THRU  READ1-RTN-EXIT.
           PERFORM  GENT-RTN   THRU  GENT-RTN-EXIT
                    UNTIL  NOT  (END-SW  =  0  AND  RED-SW  =  1).
       PROC-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÅiÇfÇdÇmÇsÅ|ÇqÇsÇmÅj                                        *
      ******************************************************************
       GENT-RTN.
           PERFORM  READ2-RTN  THRU  READ2-RTN-EXIT.
           PERFORM  DETL-RTN   THRU  DETL-RTN-EXIT.
           PERFORM  READ1-RTN  THRU  READ1-RTN-EXIT.
           PERFORM  TOTL-RTN   THRU  TOTL-RTN-EXIT.
       GENT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    èâä˙ê›íËèàóù  ÅiÇhÇmÇhÇsÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       INIT-RTN.
           ACCEPT   WYMD    FROM    DATE.
           MOVE  1  TO  PAGE-C.
           MOVE  0  TO  GOKEI.
           MOVE  0  TO  SETSU.
       INIT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ëIëèàóù  ÅiÇrÇkÇbÇsÅ|ÇqÇsÇmÅj                              *
      ******************************************************************
       SLCT-RTN.
       SLCT-010.
           CALL "SD_Output" USING
            "DSP-CLER" DSP-CLER "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-GAMEN" DSP-GAMEN "p" RETURNING RESU.
       SLCT-015.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-JS "01ACP-JS"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-015
           END-IF
           IF  W-JS     NOT  =  0  AND  1
               GO  TO  SLCT-015
           END-IF.
       SLCT-020.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-SEN "01ACP-SEN"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "P9"
               MOVE  9  TO  END-SW
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  END-STS       =  "09"
               GO  TO  SLCT-015
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-020
           END-IF
           IF  SEN-W   =  1  OR  2
               GO  TO  SLCT-030
           ELSE
               GO  TO  SLCT-020
           END-IF.
       SLCT-030.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-TEST "01ACP-TEST"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-020
           END-IF
           IF  END-STS  NOT  =  "01"
               GO  TO  SLCT-030
           END-IF
           IF  TEST-W  =  1
               PERFORM  TEST-RTN  THRU  TEST-RTN-EXIT
               GO  TO  SLCT-030
           END-IF
           IF  TEST-W  =  2
               GO  TO  SLCT-040
           ELSE
               GO  TO  SLCT-030
           END-IF.
       SLCT-040.
           IF  SEN-W    =  1
               GO  TO  SLCT-100
           END-IF
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KURA1 "01ACP-KURA1"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               CALL "SD_Output" USING
                "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
           END-IF
           IF  END-STS       =  "09"
               GO  TO  SLCT-030
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-040
           END-IF.
       SLCT-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU11 "ACP-TKU11"
            "9" "4" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-040
           END-IF.
       SLCT-055.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU12 "ACP-TKU12"
            "9" "3" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-050
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-055
           END-IF.
       SLCT-060.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN1 "01ACP-DEN1"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-050
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-060
           END-IF.
       SLCT-070.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KURA2 "01ACP-KURA2"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-060
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-070
           END-IF.
       SLCT-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU21 "ACP-TKU21"
            "9" "4" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-070
           END-IF.
       SLCT-085.
           CALL "SD_Accept" USING BY REFERENCE ACP-TKU22 "ACP-TKU22"
            "9" "3" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-080
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-085
           END-IF.
       SLCT-090.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-DEN2 "01ACP-DEN2"
            "9" "6" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               GO  TO  SLCT-080
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-090
           END-IF
           IF  FROM-WK >  TO-WK
               GO  TO  SLCT-040
           END-IF.
       SLCT-100.
           CALL "SD_Accept" USING BY REFERENCE 01ACP-KAKU "01ACP-KAKU"
            "9" "1" BY REFERENCE END-STS RETURNING RESU.
           IF  END-STS       =  "09"
               IF  SEN-W  =  1
                   CALL "SD_Output" USING
                    "DSP-CLER2" DSP-CLER2 "p" RETURNING RESU
                   GO  TO  SLCT-030
               ELSE
                   GO  TO  SLCT-090
               END-IF
           END-IF
           IF  END-STS  NOT  =  "01"  AND  "06"
               GO  TO  SLCT-100
           END-IF
           IF  KAKU-W  =  1
               GO  TO  SLCT-RTN-EXIT
           END-IF
           IF  KAKU-W  =  9
               GO  TO  SLCT-010
           ELSE
               GO  TO  SLCT-100
           END-IF.
       SLCT-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇPÅj  ÅiÇqÇdÇ`ÇcÇPÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ1-RTN.
           IF  SEN-W   =  1
               GO  TO  READ1-010
           ELSE
               GO  TO  READ1-030
           END-IF.
       READ1-010.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ ( Ø∫≥))
           IF  RED-SW  =  1
               GO  TO  READ1-020
           END-IF
      *           READ  JSJD  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE   9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JSJD-16 NOT = W-JS
               GO  TO  READ1-010
           END-IF
           IF  JSJD-021    = 5000  OR  9850
               GO  TO  READ1-010
           END-IF
           IF  JSJD-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSJD-1211(1)  =  ZERO  AND  JSJD-1211(2)  =  ZERO  AND
               JSJD-1211(3)  =  ZERO  AND  JSJD-1211(4)  =  ZERO  AND
               JSJD-1211(5)  =  ZERO  AND  JSJD-1211(6)  =  ZERO  AND
               JSJD-1211(7)  =  ZERO  AND  JSJD-1211(8)  =  ZERO  AND
               JSJD-1211(9)  =  ZERO  AND  JSJD-1211(10) =  ZERO
               GO  TO  READ1-010
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-020.
      *           READ  JSJD  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JSJD-16 NOT = W-JS
               GO  TO  READ1-010
           END-IF
           IF  JSJD-021    = 5000  OR  9850
               GO  TO  READ1-010
           END-IF
           IF  JSJD-158  NOT  =  0
               GO  TO  READ1-020
           END-IF
           IF  JSJD-1211(1)  =  ZERO  AND  JSJD-1211(2)  =  ZERO  AND
               JSJD-1211(3)  =  ZERO  AND  JSJD-1211(4)  =  ZERO  AND
               JSJD-1211(5)  =  ZERO  AND  JSJD-1211(6)  =  ZERO  AND
               JSJD-1211(7)  =  ZERO  AND  JSJD-1211(8)  =  ZERO  AND
               JSJD-1211(9)  =  ZERO  AND  JSJD-1211(10) =  ZERO
               GO  TO  READ1-020
           END-IF
           MOVE  1  TO  RED-SW.
           GO  TO  READ1-RTN-EXIT.
       READ1-030.
      *    (º≠Ø∂•ªºΩﬁ•ƒ◊› … ÿ∞ƒﬁ (ª≤ Ø∫≥))
           IF  RED-SW  =  1
               GO  TO  READ1-040
           END-IF
           MOVE  KURA-W1  TO  JSJD-01.
           MOVE  TKU-W1   TO  JSJD-02.
           MOVE  DEN-W1   TO  JSJD-03.
           MOVE  0        TO  JSJD-04.
      *           START  JSJD  KEY  IS  NOT  <  JSJD-KEY  INVALID  KEY
      *///////////////
           CALL "DB_Start" USING
            JSJD_PNAME1 "JSJD-KEY" " NOT < " JSJD-KEY RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF.
       READ1-040.
      *           READ  JSJD  NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSJD_PNAME1 BY REFERENCE JSJD-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           MOVE  JSJD-01  TO  KEY-W1.
           MOVE  JSJD-02  TO  KEY-W2.
           MOVE  JSJD-03  TO  KEY-W3.
           IF  TO-WK    <  KEY-WK
               MOVE  9  TO  END-SW
               GO  TO  READ1-RTN-EXIT
           END-IF
           IF  JSJD-16 NOT = W-JS
               GO  TO  READ1-040
           END-IF
           IF  JSJD-021    = 5000  OR  9850
               GO  TO  READ1-040
           END-IF
           IF  JSJD-158  NOT  =  1
               GO  TO  READ1-040
           END-IF
           IF  JSJD-1211(1)  =  ZERO  AND  JSJD-1211(2)  =  ZERO  AND
               JSJD-1211(3)  =  ZERO  AND  JSJD-1211(4)  =  ZERO  AND
               JSJD-1211(5)  =  ZERO  AND  JSJD-1211(6)  =  ZERO  AND
               JSJD-1211(7)  =  ZERO  AND  JSJD-1211(8)  =  ZERO  AND
               JSJD-1211(9)  =  ZERO  AND  JSJD-1211(10) =  ZERO
               GO  TO  READ1-040
           END-IF
           MOVE  1  TO  RED-SW.
       READ1-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇqÇdÇ`ÇcèàóùÅiÇQÅj  ÅiÇqÇdÇ`ÇcÇQÅ|ÇqÇsÇmÅj                  *
      ******************************************************************
       READ2-RTN.
           MOVE  0         TO  TCM-SW.
       READ2-010.
           MOVE  JSJD-021  TO  T-KEY.
      *           READ  T-M   UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  ZERO      TO  T-TNC1  T-DCC
           END-IF
      *    (¡Æ∏ø≥ª∑•œΩ¿ … ÿ∞ƒﬁ)
           MOVE  JSJD-021  TO  TC-TCD.
           MOVE  1         TO  TC-CCD.
      *           READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  JSJD-021  TO  WORK-41
               MOVE  SPACE     TO  WORK-42
               GO  TO  READ2-020
           END-IF
           MOVE  JSJD-021  TO  WORK-41.
           MOVE  TC-NAME   TO  WORK-42.
       READ2-020.
           IF  JSJD-022  =  1
               MOVE  SPACE  TO  WORK-43
               MOVE  SPACE  TO  WORK-44
               GO  TO  READ2-030
           ELSE
               MOVE  JSJD-021  TO  TC-TCD
               MOVE  JSJD-022  TO  TC-CCD
      *               READ  TC-M  UNLOCK  INVALID  KEY
      *///////////////
               CALL "DB_Read" USING
                "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
                RETURNING RET
               IF  RET = 1
                   MOVE  JSJD-022  TO  WORK-43
                   MOVE  SPACE     TO  WORK-44
                   GO  TO  READ2-030
               END-IF
           END-IF
           MOVE  JSJD-022  TO  WORK-43.
           MOVE  TC-NAME   TO  WORK-44.
       READ2-030.
      *    (À›“≤•œΩ¿ … ÿ∞ƒﬁ)
           MOVE  SPACE    TO  WORK-67.
           MOVE  JSJD-09  TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WORK-62
               GO  TO  READ2-040
           END-IF
           MOVE  HI-NAME TO  WORK-62.
       READ2-040.
           IF  JSJD-09     <  999900
               PERFORM  TAN-RTN  THRU  TAN-EX
           END-IF
      *    (∫›ƒ€∞Ÿ•Ãß≤Ÿ … ÿ∞ƒﬁ)
           MOVE  2        TO  JCON2-01.
           MOVE  JSJD-14  TO  JCON2-02.
      *           READ  JCON  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  WORK-711  WORK-712
               GO  TO  READ2-RTN-EXIT
           END-IF
           MOVE  JCON2-03  TO  WORK-711.
           MOVE  SPACE     TO  WORK-712.
       READ2-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ñæç◊èoóÕèàóù  ÅiÇcÇdÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       DETL-RTN.
           IF  TCM-SW  =  1
               GO  TO  DETL-010
           END-IF
           IF  DEN-W  NOT  =  JSJD-03
               PERFORM  HEAD-RTN  THRU  HEAD-RTN-EXIT
           END-IF
           PERFORM  WRITE-RTN  THRU  WRITE-RTN-EXIT.
       DETL-010.
           IF  SEN-W  =  1
               MOVE  1  TO  JSJD-158
      *               REWRITE  JSJD-REC  INVALID  KEY
      *///////////////
               CALL "DB_Update" USING
                JSJD_PNAME1 JSJD_LNAME JSJD-REC RETURNING RET
               IF  RET = 1
                   MOVE  JSJD-KEY  TO  ERR-K
                   MOVE  "JSJD"    TO  ERR-F
                   MOVE  "R"       TO  ERR-M
                   MOVE   0        TO  ERR-LIN
                   CALL "SD_Arg_Match_Line" USING
                    "ERR-LIN" "2" ERR-LIN RETURNING RESU
                   PERFORM  ERR-RTN   THRU  ERR-EX
                   PERFORM  CLSE-RTN  THRU  CLSE-RTN-EXIT
                   CALL "DB_Close"
                   STOP  RUN
               END-IF
           END-IF
           MOVE  JSJD-03           TO  DEN-W.
           IF  JSJD-09  <  999900
               ADD   WORK-92           TO  GOKEI
           END-IF
           ADD  WORK-82           TO  SETSU.
       DETL-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    çáåvèoóÕèàóù  ÅiÇsÇnÇsÇkÅ|ÇqÇsÇmÅj                          *
      ******************************************************************
       TOTL-RTN.
           IF  TCM-SW  NOT  =  0
               MOVE  0  TO  GOKEI
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF  END-SW  =  9
               COMPUTE  J  =  16  -  (I  *  2)
               MOVE   GOKEI          TO  WORK-73
               MOVE   X"1A24212078"  TO  ST-CD4
               MOVE   X"1A24212474"  TO  ED-CD4
               MOVE   WORK-7         TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               GO  TO  TOTL-RTN-EXIT
           END-IF
           IF  JSJD-03  NOT  =  DEN-W
               COMPUTE  J  =  16  -  (I  *  2)
               MOVE   GOKEI          TO  WORK-73
               MOVE   X"1A24212078"  TO  ST-CD4
               MOVE   X"1A24212474"  TO  ED-CD4
               MOVE   WORK-7         TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE  0  TO  GOKEI
           END-IF.
       TOTL-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÉZÉbÉgêîèoóÕèàóù  ÅiÇsÇnÇsÇkÇPÅ|ÇqÇsÇmÅj                    *
      ******************************************************************
       TOTL1-RTN.
           COMPUTE  J  =  16  -  (I  *  2)  -  1.
           IF  SETSU  NOT  =  0
               MOVE  SETSU    TO  WORK-61A
               MOVE  "æØƒ"    TO  WORK-62A
               MOVE   WORK-6A TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE  SPACE    TO  PRN-R
               CALL "PR_LineFeed" USING J RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF
           MOVE  0    TO  SETSU.
       TOTL1-EX.
      ******************************************************************
      *    å©èoÇµèoóÕèàóù  ÅiÇgÇdÇ`ÇcÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       HEAD-RTN.
           IF  PAGE-C  NOT  =  1
               MOVE  SPACE  TO  PRN-R
               CALL "PR_Write" USING PRN-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           MOVE  X"1A24212078"  TO  ST-CD1.
           MOVE  X"1A24212474"  TO  ED-CD1.
           MOVE  JSJD-03         TO  WORK-11.
           MOVE    "'"           TO  WORK-M2.
           MOVE  JSJD-0712       TO  WORK-211.
           MOVE  JSJD-072        TO  WORK-212.
           MOVE  JSJD-073        TO  WORK-213.
           IF  JSJD-05  =  0
               MOVE  "èoâ◊"  TO  WORK-22
           END-IF
           IF  JSJD-05  =  3
               MOVE  "í˘ê≥"  TO  WORK-22
           END-IF
           IF  JSJD-05  =  7
               MOVE  "ÉTèo"  TO  WORK-22
           END-IF
           MOVE  3         TO  JCON3-01.
           MOVE  JSJD-01   TO  JCON3-02.
      *           READ  JCON      UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "Å@"  TO  JCON3-03
           END-IF
           MOVE  JCON3-03  TO  WORK-23.
           MOVE  JSJD-14B  TO  WORK-31.
           MOVE  SPACE     TO  WORK-34A.
           MOVE  "("       TO  WORK-34F.
           MOVE  T-TNC1    TO  WORK-341.
           MOVE  "-"       TO  WORK-342.
           MOVE  T-DCC     TO  WORK-343.
           MOVE  ")"       TO  WORK-34R.
           MOVE  WYMD      TO  WORK-32.
           MOVE  JSJD-06S  TO  WORK-33.
           MOVE  1         TO  I.
           ADD   1         TO  PAGE-C.
           MOVE   WORK-1 TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-2 TO  PRN-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-3 TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-3A TO  PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-4 TO  PRN-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           IF  JSJD-14B  =  OLD-OKNO
               MOVE  SPACE     TO  WORK-70R
           ELSE
               MOVE  JSJD-15A  TO  WORK-70
           END-IF
           MOVE  JSJD-15   TO  WORK-721.
           MOVE  JSJD-14D  TO  WORK-722.
           MOVE  JSJD-14B  TO  OLD-OKNO.
       HEAD-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇvÇqÇhÇsÇdèàóù  ÅiÇvÇqÇhÇsÇdÅ|ÇqÇsÇmÅj                      *
      ******************************************************************
       WRITE-RTN.
           INITIALIZE  WORK-5  WORK-8  WORK-9.
           MOVE  X"1A24212078"  TO          ST-CD3.
           MOVE  X"1A24212474"  TO  ED-CD2  ED-CD3.
           MOVE  SPACE       TO  WORK-NGPD.
           IF (JSJD-14A  NOT  >  1)  AND  (JSJD-20    =  SPACE)
                                     AND  (JSJD-08    =  ZERO)
               IF  I  =  1
                   MOVE   WORK-5 TO  PRN-R
                   CALL "PR_LineFeed" USING "5" RETURNING RESP
                   CALL "PR_Write" USING PRN-R RETURNING RESP
                   GO  TO  WRITE-010
               ELSE
                   MOVE   WORK-5 TO  PRN-R
                   CALL "PR_LineFeed" USING "1" RETURNING RESP
                   CALL "PR_Write" USING PRN-R RETURNING RESP
                   GO  TO  WRITE-010
               END-IF
           END-IF
           IF  JSJD-14A       >  1
               COMPUTE WORK-811(1)  = JSJD-1211(1)  / JSJD-14A
               COMPUTE WORK-811(2)  = JSJD-1211(2)  / JSJD-14A
               COMPUTE WORK-811(3)  = JSJD-1211(3)  / JSJD-14A
               COMPUTE WORK-811(4)  = JSJD-1211(4)  / JSJD-14A
               COMPUTE WORK-811(5)  = JSJD-1211(5)  / JSJD-14A
               COMPUTE WORK-811(6)  = JSJD-1211(6)  / JSJD-14A
               COMPUTE WORK-811(7)  = JSJD-1211(7)  / JSJD-14A
               COMPUTE WORK-811(8)  = JSJD-1211(8)  / JSJD-14A
               COMPUTE WORK-811(9)  = JSJD-1211(9)  / JSJD-14A
               COMPUTE WORK-811(10) = JSJD-1211(10) / JSJD-14A
               COMPUTE WORK-82      = JSJD-1211(1)  + JSJD-1211(2)
                                    + JSJD-1211(3)  + JSJD-1211(4)
                                    + JSJD-1211(5)  + JSJD-1211(6)
                                    + JSJD-1211(7)  + JSJD-1211(8)
                                    + JSJD-1211(9)  + JSJD-1211(10)
               COMPUTE WORK-82      = WORK-82       / JSJD-14A
               MOVE  WORK-811(1)   TO  WORK-521(1)
               MOVE  WORK-811(2)   TO  WORK-521(2)
               MOVE  WORK-811(3)   TO  WORK-521(3)
               MOVE  WORK-811(4)   TO  WORK-521(4)
               MOVE  WORK-811(5)   TO  WORK-521(5)
               MOVE  WORK-811(6)   TO  WORK-521(6)
               MOVE  WORK-811(7)   TO  WORK-521(7)
               MOVE  WORK-811(8)   TO  WORK-521(8)
               MOVE  WORK-811(9)   TO  WORK-521(9)
               MOVE  WORK-811(10)  TO  WORK-521(10)
               MOVE  WORK-82       TO  WORK-53
               MOVE  "X"           TO  WORK-54
               MOVE  JSJD-14A      TO  WORK-55
           END-IF
           IF  JSJD-20    NOT =  SPACE
               MOVE  "["           TO  WORK-51F
               MOVE  JSJD-20       TO  WORK-51
               MOVE  "]"           TO  WORK-51R
           END-IF
           IF (JSJD-081    =  ZERO)   AND  (JSJD-082    =  ZERO)
               MOVE  SPACE        TO  WORK-NGPD
               GO  TO  WRITE-005
           END-IF
           MOVE  JSJD-08       TO  JMSTD-KEY1.
      *           READ  JMSTD  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  999999        TO  WORK-NGP
               GO  TO  WRITE-005
           END-IF
           MOVE  JMSTD-02S     TO  WORK-NGP.
       WRITE-005.
           IF  I  =  1
               MOVE   WORK-5 TO  PRN-R
               CALL "PR_LineFeed" USING "5" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE   WORK-5 TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF.
       WRITE-010.
           MOVE  JSJD-09       TO  WORK-61.
           MOVE  JSJD-10       TO  WORK-63.
           MOVE  JSJD-1211(1)   TO  WORK-641(1).
           MOVE  JSJD-1211(2)   TO  WORK-641(2).
           MOVE  JSJD-1211(3)   TO  WORK-641(3).
           MOVE  JSJD-1211(4)   TO  WORK-641(4).
           MOVE  JSJD-1211(5)   TO  WORK-641(5).
           MOVE  JSJD-1211(6)   TO  WORK-641(6).
           MOVE  JSJD-1211(7)   TO  WORK-641(7).
           MOVE  JSJD-1211(8)   TO  WORK-641(8).
           MOVE  JSJD-1211(9)   TO  WORK-641(9).
           MOVE  JSJD-1211(10)  TO  WORK-641(10).
           COMPUTE  WORK-92  =  JSJD-1211(1)  +  JSJD-1211(2)
                             +  JSJD-1211(3)  +  JSJD-1211(4)
                             +  JSJD-1211(5)  +  JSJD-1211(6)
                             +  JSJD-1211(7)  +  JSJD-1211(8)
                             +  JSJD-1211(9)  +  JSJD-1211(10).
       WRITE-020.
           IF  JSJD-13  =  5 OR 4
               MOVE  "("       TO  WORK-651
               MOVE  WORK-92   TO  WORK-652
               MOVE  ")"       TO  WORK-66
           ELSE
               MOVE  WORK-92   TO  WORK-65
               MOVE  SPACE     TO  WORK-66
           END-IF
           MOVE   WORK-6 TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           ADD  1  TO  I.
       WRITE-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    íPâøåüèoèàóù  ÅiÇsÇ`ÇmÅ|ÇqÇsÇmÅj                            *
      ******************************************************************
       TAN-RTN.
           MOVE JSJD-021 TO W-TCD.
           IF  W-TCD = 0460
               MOVE 0458 TO W-TCD
           END-IF
           MOVE 0 TO W-TAN.
           MOVE W-TCD TO THT-TCD.
           MOVE JSJD-09 TO THT-HCD.
           MOVE JSJD-10 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO W-TAN.
           IF  W-TAN NOT = ZERO
               GO TO TAN-EX
           END-IF
           MOVE W-TCD TO THT-TCD.
           MOVE JSJD-09 TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO W-TAN.
           IF  W-TAN = ZERO
               MOVE  "•"      TO  WORK-67
           END-IF.
       TAN-EX.
           EXIT.
      ******************************************************************
      *    ÉeÉXÉgàÛéöèàóù  ÅiÇsÇdÇrÇsÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       TEST-RTN.
           CALL "PR_Open" RETURNING RESP.
           MOVE X"1A24212078" TO  ST-CD1.
           MOVE X"1A24212474" TO  ED-CD1.
           MOVE  ALL    "9"    TO  WORK-11.
           MOVE  "'"           TO  WORK-M2.
           MOVE  ALL    "9"    TO  WORK-211  WORK-212  WORK-213.
           MOVE  ALL  "Çm"   TO  WORK-23.
           MOVE  ALL    "9"    TO  WORK-31  WORK-32  WORK-33.
           MOVE  ALL    "9"    TO  WORK-41  WORK-43.
           MOVE  ALL  "ÇX"   TO  WORK-42  WORK-44.
           MOVE   WORK-1 TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-2 TO  PRN-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-3 TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-3A TO  PRN-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE   WORK-4 TO  PRN-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  1  TO  I.
       TEST-010.
           MOVE X"1A24212078" TO          ST-CD3.
           MOVE X"1A24212474" TO  ED-CD2  ED-CD3.
           MOVE         99     TO  WORK-521(1)  WORK-521(2)
                                   WORK-521(3)  WORK-521(4)
                                   WORK-521(5)  WORK-521(6)
                                   WORK-521(7)  WORK-521(8)
                                   WORK-521(9)  WORK-521(10).
           MOVE         999    TO  WORK-53.
           MOVE         "X"    TO  WORK-54.
           MOVE         999    TO  WORK-55.
           MOVE  ALL    "9"    TO  WORK-61.
           MOVE  ALL  "ÇX"   TO  WORK-62.
           MOVE  ALL    "9"    TO  WORK-63.
           MOVE  ALL    "9"    TO  WORK-641(1)  WORK-641(2)
                                   WORK-641(3)  WORK-641(4)
                                   WORK-641(5)  WORK-641(6)
                                   WORK-641(7)  WORK-641(8)
                                   WORK-641(9)  WORK-641(10).
           MOVE  ALL    "9"    TO  WORK-65.
           IF  I  =  1
               MOVE   WORK-5 TO  PRN-R
               CALL "PR_LineFeed" USING "5" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-6 TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           ELSE
               MOVE   WORK-5 TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
               MOVE   WORK-6 TO  PRN-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING PRN-R RETURNING RESP
           END-IF
           IF  I  =  6
               GO  TO  TEST-020
           ELSE
               ADD  1  TO  I
               GO  TO  TEST-010
           END-IF.
       TEST-020.
           MOVE X"1A24212078" TO  ST-CD4.
           MOVE X"1A24212474" TO  ED-CD4.
           MOVE   WORK-6A TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           MOVE  ALL  "ÇX"   TO  WORK-711  WORK-712
                                   WORK-721  WORK-722.
           MOVE  ALL    "9"    TO  WORK-73  WORK-70.
           MOVE   WORK-7 TO  PRN-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING PRN-R RETURNING RESP.
           CALL "PR_Close" RETURNING RESP.
       TEST-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇnÇoÇdÇmèàóù  ÅiÇnÇoÇdÇmÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "I-O" JSJD_PNAME1 "SHARED" BY REFERENCE JSJD_IDLST "2"
            "JSJD-KEY" BY REFERENCE JSJD-KEY "JSJD-KEY2" BY REFERENCE
            JSJD-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "PR_Open" RETURNING RESP.
       OPEN-RTN-EXIT.
           EXIT.
      ******************************************************************
      *    ÇbÇkÇnÇrÇdèàóù  ÅiÇbÇkÇrÇdÅ|ÇqÇsÇmÅj                        *
      ******************************************************************
       CLSE-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSJD_IDLST JSJD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       CLSE-RTN-EXIT.
           EXIT.
      *****
           COPY  LPERR.
           COPY  LPACPT.
