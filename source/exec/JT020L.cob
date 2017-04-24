       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT020L.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  éÛíçÅióaÇ©ÇËëºÅjì˙ïÒ            *
      *    PRINTER TYPE    :  JIPS                            *
      *    BASE PROGRAM    :  JT530L                          *
      *    DATA WRITTN     :  91/09/20                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT               PIC X(2).
       77  20K                    PIC X(05)  VALUE  X"1A24212474".
       77  15K                    PIC X(05)  VALUE  X"1A24212078".
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  W-JS                   PIC 9(01).
       01  W-JS-MEI               PIC N(03).
      *
       01  PRN-AREA.
           02  LCNT               PIC  9(02) VALUE  90.
           02  PCNT               PIC  9(03) VALUE  ZERO.
           02  I                  PIC  9(02).
           02  SKEI               PIC S9(05).
           02  KEI                PIC S9(05).
       01  OLD-AREA.
           02  OLD-KEY.
               03  OLD-KEY1       PIC  9(06).
               03  OLD-KEY2       PIC  9(03).
           02  NEW-KEY.
               03  NEW-KEY1       PIC  9(06).
               03  NEW-KEY2       PIC  9(03).
           02  OLD-03             PIC  9(01).
           02  OLD-11.
               03  OLD-TEKIYO1    PIC  N(09).
               03  OLD-TEKIYO2    PIC  N(23).
       01  WORK-AREA.
           02  HIZUKE.
               03  HI-YY          PIC 9(02).
               03  HI-MM          PIC 9(02).
               03  HI-DD          PIC 9(02).
      **
       01  MID1.
           02  F                  PIC X(05) VALUE X"1A24212474".
           02  MID-01             PIC N(05).
           02  F                  PIC X(7) VALUE SPACE.
           02  F                  PIC X(04) VALUE "Åi '".
           02  MID-NEN            PIC 9(02).
           02  F                  PIC N(01) VALUE "îN".
           02  MID-GET            PIC Z9.
           02  F                  PIC N(01) VALUE "åé".
           02  MID-PEY            PIC Z9.
           02  F                  PIC X(07) VALUE "ì˙ï™ Åj".
           02  F                  PIC X(07) VALUE SPACE.
           02  F                  PIC N(05) VALUE "ÅñÅñÅñÅ@Å@".
           02  MID-02             PIC N(04).
           02  F                  PIC N(11) VALUE
               "Å@Å@ì˙Å@Å@ïÒÅ@Å@ÅñÅñÅñ".
           02  F                  PIC X(22) VALUE SPACE.
           02  F                  PIC X(5) VALUE "DATE.".
           02  M-YY               PIC Z9.
           02  F                  PIC X    VALUE "/".
           02  M-MM               PIC Z9.
           02  F                  PIC X    VALUE "/".
           02  M-DD               PIC Z9.
           02  F                  PIC X(7) VALUE SPACE.
           02  F                  PIC X(2) VALUE "P.".
           02  WPCNT              PIC ZZ9.
       01  MID2.
           02  F                  PIC X(05) VALUE X"1A24212078".
           02  F                  PIC N(02) VALUE "èàóù".
           02  F                  PIC X(05) VALUE X"1A24212474".
           02  F                  PIC X(01) VALUE SPACE.
           02  MID-03             PIC N(03).
           02  F                  PIC X(03) VALUE SPACE.
           02  MID-04             PIC N(03).
           02  F                  PIC X(02) VALUE SPACE.
           02  F                  PIC X(05) VALUE "æØƒêî".
           02  F                  PIC X(02) VALUE SPACE.
           02  F                  PIC X(08) VALUE "íºëóêÊCD".
           02  F                  PIC X(01) VALUE SPACE.
           02  F                  PIC N(04) VALUE "ìæà”êÊñº".
           02  F                  PIC X(36) VALUE SPACE.
           02  F                  PIC N(06) VALUE "íºëóêÊÅEìXñº".
       01  MID3.
           02  F                  PIC X(05) VALUE X"1A24212474".
           02  F                  PIC X(03) VALUE  SPACE.
           02  F                  PIC X(01) VALUE  "-".
           02  F                  PIC N(01) VALUE  "çs".
           02  F                  PIC X(03) VALUE  SPACE.
           02  F                  PIC X(11) VALUE  "ïiñºCD ïiñº".
           02  F                  PIC X(33) VALUE  SPACE.
           02  F                  PIC X(27) VALUE
                              "1 ÇRçÜ ÇQçÜ ÇPçÜ ÇOçÜ  íÜ  ".
           02  F                  PIC X(24) VALUE
                              " ëÂ  ì¡ëÂ 28.0 29.0 30.0".
       01  MID4.
           02  F                  PIC X(53) VALUE  SPACE.
           02  F                  PIC X(27) VALUE
                              "2 12.5 13.0 13.5 14.0 15.0 ".
           02  F                  PIC X(24) VALUE
                              "16.0 17.0 18.0 19.0 20.0".
           02  F                  PIC X(06) VALUE  SPACE.
           02  F                  PIC N(01) VALUE  "åv".
           02  F                  PIC X(02) VALUE  SPACE.
           02  F                  PIC N(02) VALUE  "íPâø".
           02  F                  PIC X(01) VALUE  SPACE.
           02  F                  PIC N(03) VALUE  "î[Å@ä˙".
           02  F                  PIC X(01) VALUE  SPACE.
           02  F                  PIC N(03) VALUE  "îıÅ@çl".
           02  F                  PIC X(04) VALUE  SPACE.
       01  MID5.
           02  F                  PIC X(53) VALUE  SPACE.
           02  F                  PIC X(27) VALUE
                              "3 21.0 21.5 22.0 22.5 23.0 ".
           02  F                  PIC X(19) VALUE
                              "23.5 24.0 24.5 25.0".
           02  F                  PIC X(28) VALUE  SPACE.
           02  F                  PIC X(05) VALUE X"1A24212078".
           02  MID-05             PIC N(06).
           02  F                  PIC X(05) VALUE X"1A24212474".
       01  MID6.
           02  F                  PIC X(53) VALUE  SPACE.
           02  F                  PIC X(27) VALUE
                              "4 24.0 24.5 25.0 25.5 26.0 ".
           02  F                  PIC X(20) VALUE
                              "26.5 27.0 27.5      ".
      ***
       COPY  LWMSG.
      ***
           COPY   LTWK02.
           COPY   LITCM.
           COPY   LIHIM2.
           COPY   LWTNAF.
      *FD  P-F
       01  P-R.
           02  P-R1.
               03  P1-15B             PIC X(05).
               03  P1-01              PIC N(02).
               03  FILLER             PIC X(01).
               03  P1-03              PIC 9(06).
               03  FILLER             PIC X(02).
               03  P1-041             PIC 99.
               03  P1-A               PIC X(01).
               03  P1-042             PIC Z9.
               03  P1-B               PIC X(01).
               03  P1-043             PIC Z9.
               03  FILLER             PIC X(02).
               03  P1-05              PIC ----.
               03  FILLER             PIC X(02).
               03  P1-061             PIC 9(04).
               03  P1-C               PIC X(01).
               03  P1-062             PIC 9(03).
               03  FILLER             PIC X(01).
               03  P1-07              PIC N(26).
               03  P1-08A             PIC 9(04).
               03  FILLER             PIC X(01).
               03  P1-08              PIC N(26).
               03  P1-2B              PIC X(05).
               03  F                  PIC X(93).
           02  P-R2 REDEFINES P-R1.
               03  P2-15B             PIC X(05).
               03  FILLER             PIC X(03).
               03  P2-A               PIC X(01).
               03  P2-01              PIC 9(01).
               03  FILLER             PIC X(04).
               03  P2-03              PIC 9(06).
               03  FILLER             PIC X(01).
               03  P2-04              PIC N(24).
               03  FILLER             PIC X(01).
               03  P2-05              PIC 9(01).
               03  P2-06     OCCURS 10.
                   04  P2-061         PIC -----.
               03  P2-07              PIC ----,--9.
               03  FILLER             PIC X(01).
               03  P2-08              PIC ZZZZZ.
               03  FILLER             PIC X(01).
               03  P2-10              PIC 9(06).
               03  FILLER             PIC X(01).
               03  P2-11              PIC X(10).
               03  P2-2B              PIC X(05).
               03  F                  PIC X(92).
           02  P-R4 REDEFINES P-R1.
               03  P4-15B             PIC X(05).
               03  FILLER             PIC X(22).
               03  P4-01              PIC N(02).
               03  P4-A               PIC X(01).
               03  P4-02.
                   04  P4-021         PIC N(09).
                   04  P4-022         PIC N(01).
               03  FILLER             PIC X(01).
               03  P4-03              PIC N(02).
               03  P4-B               PIC X(01).
               03  P4-04.
                   04  P4-041         PIC N(23).
                   04  P4-042         PIC N(01).
               03  FILLER             PIC X(18).
               03  P4-05              PIC N(02).
               03  P4-C               PIC X(01).
               03  P4-06              PIC ----,--9.
               03  FILLER             PIC X(06).
               03  P4-07              PIC ---,---,---.
               03  P4-2B              PIC X(05).
               03  F                  PIC X(91).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
      ***
       01  DSP-AREA.
           02  FILLER  PIC  X(26) VALUE
               " éÛíçÅEóaÇËÅEéÊÇËÇÊÇØì˙ïÒ ".
           02  FILLER  PIC  X(02) VALUE  "Åk".
           02  FILLER  PIC  X(02) VALUE  "Ål".
           02  FILLER  PIC  N(03).
       01  DSP-ERR.
           02  ERR-1   PIC  X(22) VALUE
                                             "ÇiÇrÅ|ÇrÇhÇfÇmÅ@ÉGÉâÅ[".
       01  DISP-MSG-SPACE1.
           02  FILLER  PIC X(40)     VALUE " ".
      ***
       COPY  LSMSG.
      ***
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "36" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "26" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-AREA" "X" "1" "1" "2" "01DSP-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "03DSP-AREA" "X" "1" "9" "2" "02DSP-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "04DSP-AREA" "N" "1" "3" "6" "03DSP-AREA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04DSP-AREA" BY REFERENCE W-JS-MEI "6" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "22" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-1" "X" "24" "1" "22" " " "DSP-ERR"  RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "24" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE1" "X" "24" "1" "40" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  “ ≤ ›  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           IF  COMPLETION_CODE  =  255
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  PRN-RTN     THRU   PRN-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *
      *********************************
      ***   ≤∆º¨Ÿ   R T N           ***
      *********************************
      **
       INI-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK02_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK02_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK02_IDLST "0".
      *           READ  JT-WK02  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK02_PNAME1 BY REFERENCE W02-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JT-WK02_IDLST JT-WK02_PNAME1
               GO  TO  INI-EX
           END-IF
           MOVE W02-JS TO W-JS.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK02_IDLST JT-WK02_PNAME1.
      *
           IF  W-JS  NOT =  0  AND  1  AND  2
               CALL "SD_Output" USING
                 "ERR-1" ERR-1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO  INI-EX
           END-IF
           IF  W-JS  =  ZERO
               MOVE  "ã≥Å@àÁ"     TO  W-JS-MEI
           END-IF
           IF  W-JS  =  1
               MOVE  "ÉèÅ[ÉN"     TO  W-JS-MEI
           END-IF
           IF  W-JS  =  2
               MOVE  "ÉJÉWÉÖ"     TO  W-JS-MEI
           END-IF
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "INPUT" JT-WK02_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK02_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
      *
           ACCEPT  HIZUKE  FROM  DATE.
           MOVE  HI-YY     TO  M-YY.
           MOVE  HI-MM     TO  M-MM.
           MOVE  HI-DD     TO  M-DD.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
       INI-EX.
            EXIT.
      *
      ******************************
      ***   ÿ Ω ƒ   R T N        ***
      ******************************
      **
       PRN-RTN.
      ***  ÉèÅ[ÉNÉtÉ@ÉCÉãÅ@ÇqÇdÇ`Çc
      *           READ  JT-WK02  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK02_PNAME1 BY REFERENCE W02-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               PERFORM  KEI-RTN     THRU KEI-EX
               GO  TO  PRN-EX
           END-IF
           MOVE  W02-01      TO  NEW-KEY1.
           MOVE  W02-12      TO  NEW-KEY2.
           IF  ( ( OLD-03  =  ZERO ) AND ( OLD-03  NOT =  W02-03 ) )
               OR  ( NEW-KEY  NOT =  OLD-KEY )
               PERFORM  KEI-RTN     THRU  KEI-EX
           END-IF
           IF  ( ( OLD-03  =  ZERO ) AND ( OLD-03  NOT =  W02-03 ) )
               OR ( LCNT  NOT <  62 )
               PERFORM  MID-RTN        THRU  MID-EX
           END-IF
           IF  ( NEW-KEY  NOT =  OLD-KEY ) OR ( LCNT  =  8 )
               PERFORM  HEAD-RTN        THRU  HEAD-EX
           END-IF
           PERFORM  MEI-RTN     THRU  MEI-EX.
           PERFORM  OLD-MOVE-RTN       THRU  OLD-MOVE-EX.
           GO  TO  PRN-RTN.
       PRN-EX.
           EXIT.
      *
      *****************************
      ***  H E A D -  R T N     ***
      *****************************
       HEAD-RTN.
           IF  LCNT  NOT <  61
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF
           MOVE  15K          TO  P1-15B.
           MOVE  20K          TO  P1-2B.
           MOVE  "/"          TO  P1-A  P1-B.
           MOVE  "-"          TO  P1-C.
           MOVE  SPACE        TO  P1-01.
           IF  W02-10  =  1
               MOVE  "í«â¡"     TO  P1-01
           END-IF
           IF  W02-10  =  2
               MOVE  "ïœçX"     TO  P1-01
           END-IF
           IF  W02-10  =  3
               MOVE  "çÌèú"     TO  P1-01
           END-IF
           MOVE  W02-01       TO  P1-03.
           MOVE  W02-0412     TO  P1-041.
           MOVE  W02-042      TO  P1-042.
           MOVE  W02-043      TO  P1-043.
           MOVE  W02-16       TO  P1-05.
           MOVE  W02-051      TO  P1-061.
           MOVE  W02-052      TO  P1-062.
      *
           MOVE  W02-051      TO  TC-TCD.
           MOVE  "001"        TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           MOVE  TC-NAME      TO  P1-07.
           IF  W02-051          =  9850
               IF  W02-23       NOT =  ZERO
                   GO  TO  HEAD-10
               END-IF
           END-IF
      *
           MOVE  W02-05       TO  TC-KEY.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           MOVE  TC-NAME      TO  P1-08.
           GO  TO  HEAD-20.
      *
       HEAD-10.
           MOVE  W02-23       TO  WTNA-KEY.
           PERFORM  WTN-READ-RTN     THRU  WTN-READ-EX.
           MOVE  W02-23       TO  P1-08A.
           MOVE  WTNA-NAME    TO  P1-08.
       HEAD-20.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE     TO  P-R.
           ADD  1      TO  LCNT.
       HEAD-EX.
           EXIT.
      *
      *****************************
      ***    M E I   R T N      ***
      *****************************
       MEI-RTN.
           MOVE  15K          TO  P2-15B.
           MOVE  20K          TO  P2-2B.
           MOVE  "-"          TO  P2-A.
           MOVE  W02-02       TO  P2-01.
           MOVE  W02-06       TO  P2-03.
           MOVE  W02-06       TO  HI-MHCD HI-HCD.
           PERFORM HIM-READ-RTN     THRU  HIM-READ-EX.
           MOVE  HI-NAME      TO  P2-04.
           MOVE  W02-06A      TO  P2-05.
           MOVE  1            TO  I.
           MOVE  ZERO         TO  SKEI.
       MEI-010.
           IF  I  >  10
               GO  TO  MEI-020
           END-IF
           MOVE  W02-0711(I)  TO  P2-061(I).
           ADD   W02-0711(I)  TO  SKEI.
           ADD   1            TO  I.
           GO  TO  MEI-010.
       MEI-020.
           MOVE  SKEI         TO  P2-07.
           MOVE  W02-15       TO  P2-08.
           IF  W02-08      NOT  =  ZERO
               MOVE  W02-08S      TO  P2-10
           END-IF
           MOVE  W02-17       TO  P2-11.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           ADD  1             TO  LCNT.
      *
           ADD  SKEI          TO  KEI.
       MEI-EX.
           EXIT.
      *
      ********************************
      ***      K E I   R T N       ***
      ********************************
       KEI-RTN.
           IF  LCNT  =  90
               GO  TO  KEI-EX
           END-IF
           MOVE  15K          TO  P4-15B.
           MOVE  20K          TO  P4-2B.
           MOVE  ":"          TO  P4-A  P4-B  P4-C.
           MOVE  "îzíB"     TO  P4-01.
           MOVE  "ìEóv"     TO  P4-03.
           MOVE  "çáåv"     TO  P4-05.
           MOVE  OLD-TEKIYO1  TO  P4-021.
           MOVE  OLD-TEKIYO2  TO  P4-041.
           MOVE  SPACE        TO  P4-022  P4-042.
           MOVE  KEI          TO  P4-06.
           MOVE  ZERO         TO  KEI.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           ADD  1             TO  LCNT.
       KEI-EX.
           EXIT.
      *
      *************************************
      ***    O L D   M O V E   R T N    ***
      *************************************
       OLD-MOVE-RTN.
           MOVE  W02-01       TO  OLD-KEY1.
           MOVE  W02-12       TO  OLD-KEY2.
           MOVE  W02-03       TO  OLD-03.
           MOVE  W02-11       TO  OLD-11.
       OLD-MOVE-EX.
           EXIT.
      *
      *****************************
      ***   R E A D   T C M     ***
      *****************************
       TCM-READ-RTN.
      ***  íºëóêÊÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  TC-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF.
       TCM-READ-EX.
           EXIT.
      *
       WTN-READ-RTN.
      *           READ  WTNAF UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  WTNA-NAME
           END-IF.
       WTN-READ-EX.
           EXIT.
      *****************************
      ***   R E A D   S H M     ***
      *****************************
       HIM-READ-RTN.
      *           READ  HI2-M   UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  HI-NAME
           END-IF.
       HIM-READ-EX.
           EXIT.
      *
      *****************************
      ***    – ¿ﬁ º  R T N      ***
      *****************************
       MID-RTN.
           IF  LCNT   <  90
               MOVE   SPACE   TO   P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
      *
           ADD   1   TO    PCNT.
           MOVE  PCNT  TO  WPCNT.
           MOVE  W02-8911  TO  MID-NEN.
           MOVE  W02-8912  TO  MID-GET.
           MOVE  W02-8913  TO  MID-PEY.
           IF  W-JS  =  ZERO
               MOVE  "Åyã≥Å@àÁÅz"     TO  MID-01
           ELSE
               IF  W-JS  =  1
                   MOVE  "ÅyÉèÅ[ÉNÅz"     TO  MID-01
               ELSE
                   MOVE  "ÅyÉJÉWÉÖÅz"     TO  MID-01
               END-IF
           END-IF
           IF  W02-03  =  ZERO
               MOVE  "éÛÅ@Å@íç"     TO  MID-02
               MOVE  "éÛíçáÇ"       TO  MID-03
               MOVE  "éÛíçì˙"       TO  MID-04
           END-IF
           IF  W02-03  =  5
               MOVE  "óaÅ@Å@ÇË"     TO  MID-02
               MOVE  "óaÇËáÇ"       TO  MID-03
               MOVE  "óaÇËì˙"       TO  MID-04
           END-IF
           IF  W02-03  =  6
               MOVE  "éÊÇËÇÊÇØ"     TO  MID-02
               MOVE  "éÊÇËáÇ"       TO  MID-03
               MOVE  "éÊÇËì˙"       TO  MID-04
           END-IF
           MOVE  SPACE          TO  MID-05.
           
      *
           MOVE   MID1    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID2    TO    P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO     P-R.
           MOVE   MID3    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID4    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID5    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID6    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
      *
           MOVE  8     TO  LCNT.
       MID-EX.
           EXIT.
      *
      **************************
      ***  Ã ß ≤ Ÿ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK02_IDLST JT-WK02_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
