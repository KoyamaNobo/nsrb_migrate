       IDENTIFICATION         DIVISION.
       PROGRAM-ID.            HN290U.
      ***********************************************************
      *    PROGRAM         :  “üoŒÉ—ÝÏƒtƒ@ƒCƒ‹ì¬            *
      *    PRINTER TYPE    :  ****                              *
      *    SCREEN          :  ****                              *
      *    DATE      @@  :  03/11/11                          *
      *    COMPILE TYPE    :  COBOL                             *
      *    JS-SIGN         :  ”„ã=2 , ŽdãŽó“ü=3 , ‘qŠÔˆÚ“®=4  *
      ***********************************************************
       ENVIRONMENT            DIVISION.
       CONFIGURATION          SECTION.
       SOURCE-COMPUTER.       NEAC-SYSTEM100.
       OBJECT-COMPUTER.       NEAC-SYSTEM100.
       DATA                   DIVISION.
       WORKING-STORAGE        SECTION.
       77  ERR-STAT  PIC  X(02).
       77  JS-SIGN   PIC  X(01).
       77  WRI-SW    PIC  9(01).
       77  END-SW    PIC  9(01)  VALUE 0.
       01  W-KEY.
           02  W-K1       PIC  9(06).
           02  W-K2       PIC  9(08).
           02  W-K3       PIC  9(02).
           02  W-K4       PIC  9(06).
           02  W-K5       PIC  9(01).
       01  W-AREA.
           02  W-DEN      PIC  9(01).
           02  P          PIC  9(02).
           02  W-WRC      PIC  X(01).
       01  W-NG.
           02  W-NEN      PIC  9(04).
           02  W-NENL  REDEFINES W-NEN.
             03  W-NEN1   PIC  9(02).
             03  W-NEN2   PIC  9(02).
           02  W-GET      PIC  9(02).
       01  W-NGPD.
           02  W-NGD      PIC  9(06).
           02  W-PEYD     PIC  9(02).
       01  W-DATA.
           02  W-DNO      PIC  9(06).
           02  W-DATE     PIC  9(08).
           02  W-TCD      PIC  9(04).
           02  W-CCD      PIC  9(03).
           02  W-SOC      PIC  9(01).
           02  W-RECD.
             03  W-REC   OCCURS  6.
               04  W-HCD  PIC  9(06).
               04  W-SIZ  PIC  9(01).
               04  W-SUD   OCCURS 10.
                 05  W-SU PIC S9(04).
               04  W-DC   PIC  9(01).
               04  W-GNO  PIC  9(01).
               04  W-BC1D PIC  9(03).
           02  W-BI       PIC  N(24).
           02  CNT        PIC  9(01).
      **
           COPY   LWMSG.
      **
           COPY   LIBFDD.
           COPY   LNSTRN.
           COPY   LUTRAN.
           COPY   L-JNSR.
           COPY   LNJZAI.
           COPY   L-JCON.
           COPY   LJNYZ.
           COPY   LIHIM.
      *FD  HSKIF
       01  HSKIF_HN290U.
           02  HSKIF_PNAME1   PIC  X(005) VALUE "HSKIF".
           02  F              PIC  X(001).
           02  HSKIF_LNAME    PIC  X(012) VALUE "HSKIF_HN290U".
           02  F              PIC  X(001).
           02  HSKIF_KEY1     PIC  X(100) VALUE SPACE.
           02  HSKIF_SORT     PIC  X(100) VALUE SPACE.
           02  HSKIF_IDLST    PIC  X(100) VALUE SPACE.
           02  HSKIF_RES      USAGE  POINTER.
       01  HSKI-R.
           02  HSKI-NO        PIC  9(007).
           02  HSKI-NOD   REDEFINES HSKI-NO.
             03  HSKI-UNO     PIC  9(006).
             03  HSKI-GYO     PIC  9(001).
           02  HSKI-DATE      PIC  9(008).
           02  HSKI-NGPD  REDEFINES HSKI-DATE.
             03  HSKI-NG      PIC  9(006).
             03  F            PIC  9(002).
           02  HSKI-NGP   REDEFINES HSKI-DATE.
             03  F            PIC  9(002).
             03  HSKI-NGPS    PIC  9(006).
           02  HSKI-HCD       PIC  9(006).
           02  HSKI-SIZ       PIC  9(001).
           02  HSKI-SUD.
             03  HSKI-SU      PIC S9(005)  OCCURS  10.
           02  HSKI-SKC       PIC  9(001).
           02  HSKI-IDC       PIC  9(001).
           02  F              PIC  X(009).
           02  HSKI-PRN       PIC  9(001).
           02  HSKI-UPD       PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
           02  FILLER  PIC  X(24) VALUE
               " “üoŒÉ—ÝÏƒtƒ@ƒCƒ‹XV ".
      ****
       01  EMSG-AREA.
           02  EMSG-CON.
               03  FILLER  PIC N(18) VALUE
                   "––@ƒRƒ“ƒgƒ[ƒ‹î•ñ@ƒGƒ‰[@––".
           02  EMSG-SP.
               03  FILLER  PIC X(70) VALUE " ".
           02  ERR-MSG1.
               03  FILLER  PIC N(16) VALUE
                   "––@ƒRƒ“ƒgƒ[ƒ‹‚e@‚È‚µ@––".
           02  ERR-MSG2.
               03  FILLER  PIC N(14) VALUE
                   "––@‚c‚`‚s‚`@ƒGƒ‰[@––".
               03  02ERR-MSG2  PIC  9(06).
           COPY   LSMSG.
           COPY   LIBSCR.
      *****
       PROCEDURE              DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "X" "1" "0" "12" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "RX" "1" "20" "24" "01DSP-AREA" " "
             RETURNING RESU.
      *EMSG-AREA
       CALL "SD_Init" USING 
            "EMSG-AREA" " " "24" "0" "172" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-CON" " " "24" "0" "36" " " "EMSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
           "01EMSG-CON" "N" "24" "1" "36" " " "EMSG-CON" RETURNING RESU.
       CALL "SD_Init" USING 
            "EMSG-SP" " " "24" "0" "70" "EMSG-CON" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01EMSG-SP" "X" "24" "1" "70" " " "EMSG-SP" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG1" " " "24" "0" "32" "EMSG-SP" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ERR-MSG1" "N" "24" "1" "32" " " "ERR-MSG1" RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-MSG2" " " "24" "0" "34" "ERR-MSG1" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ERR-MSG2" "N" "24" "1" "28" " " "ERR-MSG2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ERR-MSG2" "9" "24" "33" "6" "01ERR-MSG2" " "
             RETURNING RESU.
       CALL "SD_From" USING 
            "02ERR-MSG2" BY REFERENCE STRN-20 "6" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *************************************************
      *    ‚l‚`‚h‚m|‚q‚s‚m@ iŽåˆ—j              *
      *************************************************
       MAIN-RTN.
           PERFORM  OPEN-RTN  THRU  OPEN-EX.
           PERFORM  UPD-RTN   THRU  UPD-EX.
           PERFORM  END-RTN   THRU  END-EX.
           CALL "DB_Close".
           STOP     RUN.
      **
           COPY   LPMSG.
      *************************************************
      *    ‰Šú@ˆ—        ‚n‚o‚d‚m|‚q‚s‚m         *
      *************************************************
       OPEN-RTN.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           IF  JS-SIGN NOT = "2" AND "3" AND "4"
               CALL "SD_Output" USING
                "EMSG-CON" EMSG-CON "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "DB_Close"
               STOP    RUN
           END-IF
           COPY   LIBCPR.
      **
           IF  JS-SIGN   =  "2"
               CALL "DB_F_Open" USING
                "INPUT" HN-STRN_PNAME1 " " BY REFERENCE
                HN-STRN_IDLST "0"
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
                "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2"
                BY REFERENCE JNSR-KEY2 "JNSR-KEY3" BY REFERENCE
                JNSR-KEY3
               CALL "DB_F_Open" USING
                "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
                "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               PERFORM   JCON-NG-RTN  THRU  JCON-NG-RTN
           END-IF
           IF  JS-SIGN   =  "3"
               CALL "DB_F_Open" USING
                "INPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0"
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
                "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2"
                BY REFERENCE JNSR-KEY2 "JNSR-KEY3" BY REFERENCE
                JNSR-KEY3
               CALL "DB_F_Open" USING
                "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
                "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               CALL "DB_F_Open" USING
                "I-O" JNYZ_PNAME1 "SHARED" BY REFERENCE JNYZ_IDLST "1"
                "JNYZ-KEY" BY REFERENCE JNYZ-KEY
               PERFORM   JCON-NG-RTN  THRU  JCON-NG-RTN
           END-IF
           IF  JS-SIGN   =  "4"
               CALL "DB_F_Open" USING
                "I-O" HSKIF_PNAME1 " " BY REFERENCE HSKIF_IDLST "0"
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
                "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2"
                BY REFERENCE JNSR-KEY2 "JNSR-KEY3" BY REFERENCE
                JNSR-KEY3
               CALL "DB_F_Open" USING
                "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
                "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               PERFORM   JCON-NG-RTN  THRU  JCON-NG-RTN
           END-IF.
       OPEN-EX.
           EXIT.
      *************************************************
      *    I—¹@ˆ—@@@@‚d‚m‚c|‚q‚s‚m           *
      *************************************************
       END-RTN.
           IF  JS-SIGN   =  "2"
               CALL "DB_F_Close" USING
                BY REFERENCE HN-STRN_IDLST HN-STRN_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNSR_IDLST JNSR_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
           END-IF
           IF  JS-SIGN   =  "3"
               CALL "DB_F_Close" USING
                BY REFERENCE UTRAN_IDLST UTRAN_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNSR_IDLST JNSR_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNYZ_IDLST JNYZ_PNAME1
           END-IF
           IF  JS-SIGN   =  "4"
               CALL "DB_F_Close" USING
                BY REFERENCE HSKIF_IDLST HSKIF_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JNSR_IDLST JNSR_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
           END-IF.
       END-EX.
           EXIT.
      *************************************************
      *    XV@ˆ—@@@@‚t‚o‚c|‚q‚s‚m           *
      *************************************************
       UPD-RTN.
           IF  JS-SIGN  =  "2"
               PERFORM  UP2-RTN  THRU  UP2-EX
           END-IF
           IF  JS-SIGN  =  "3"
               PERFORM  UP3-RTN  THRU  UP3-EX
           END-IF
           IF  JS-SIGN  =  "4"
               PERFORM  UP4-RTN  THRU  UP4-EX
           END-IF.
       UPD-EX.
           EXIT.
      **************************************************
      *    ƒRƒ“ƒgƒ[ƒ‹î•ñh‚Qh@‚t‚o‚Q|‚q‚s‚m    *
      **************************************************
       UP2-RTN.
      *           READ     HN-STRN   NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HN-STRN_PNAME1 BY REFERENCE STRN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO    UP2-EX
           END-IF
           IF  STRN-13       =   9
               GO  TO  UP2-RTN
           END-IF
           IF  STRN-91       = 1
               GO  TO  UP2-RTN
           END-IF
           IF  STRN-10   NOT =
                         0  AND  1  AND  2  AND  4  AND  5  AND  7
               GO  TO  UP2-RTN
           END-IF
           IF  STRN-23      =  0
               GO  TO  UP2-RTN
           END-IF.
       UP2-001.
           MOVE ZERO TO W-DATA.
           MOVE SPACE TO W-BI.
           IF  STRN-13 = 9
               GO TO UP2-003
           END-IF
           IF  STRN-13 NOT = 1
               CALL "SD_Output" USING
                "ERR-MSG2" ERR-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
           END-IF
           MOVE STRN-20 TO W-DNO.
           MOVE STRN-01 TO W-DATE.
           MOVE STRN-02 TO W-TCD.
           MOVE STRN-14A TO W-CCD.
           MOVE STRN-16 TO W-SOC.
       UP2-002.
           IF  STRN-13 = 9
               MOVE STRN1-51 TO W-BI
               GO TO UP2-003
           END-IF
           ADD 1 TO CNT.
           IF  CNT > 6
               CALL "SD_Output" USING
                "ERR-MSG2" ERR-MSG2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO TO UP2-003
           END-IF
           MOVE STRN-13 TO W-GNO(CNT).
           MOVE STRN-03 TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE STRN-03 TO HI-MHCD
           END-IF
           MOVE HI-MHCD TO W-HCD(CNT).
           MOVE STRN-04 TO W-SIZ(CNT).
           MOVE STRN-10 TO W-DC(CNT).
           MOVE STRN-151D TO W-BC1D(CNT).
           IF  STRN-05 = ZERO
               GO TO UP2-003
           END-IF
           IF  STRN-03    < 999900
               MOVE STRN-051(01) TO W-SU(CNT,01)
               MOVE STRN-051(02) TO W-SU(CNT,02)
               MOVE STRN-051(03) TO W-SU(CNT,03)
               MOVE STRN-051(04) TO W-SU(CNT,04)
               MOVE STRN-051(05) TO W-SU(CNT,05)
               MOVE STRN-051(06) TO W-SU(CNT,06)
               MOVE STRN-051(07) TO W-SU(CNT,07)
               MOVE STRN-051(08) TO W-SU(CNT,08)
               MOVE STRN-051(09) TO W-SU(CNT,09)
               MOVE STRN-051(10) TO W-SU(CNT,10)
           END-IF.
       UP2-003.
      *           READ   HN-STRN   NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HN-STRN_PNAME1 BY REFERENCE STRN-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE  1         TO  END-SW
               GO  TO    UP2-005
           END-IF
           IF  STRN-91       =  1
               GO  TO  UP2-003
           END-IF
           IF  STRN-13   NOT =   9
               IF  STRN-10   NOT =
                      0  AND  1  AND  2  AND  4  AND  5  AND  7
                    GO  TO  UP2-003
               END-IF
           END-IF
           IF  STRN-13   NOT =   9
               IF  STRN-23      =  0
                   GO  TO  UP2-003
               END-IF
           END-IF
           IF  STRN-20       =  W-DNO
               GO  TO  UP2-002
           END-IF.
       UP2-005.
           MOVE ZERO TO CNT.
       UP2-006.
           ADD 1 TO CNT.
           IF  CNT > 6
               IF  END-SW = 0
                   GO TO UP2-001
               ELSE
                   GO TO UP2-EX
               END-IF
           END-IF
           IF  W-HCD(CNT) = ZERO
               IF  END-SW = 0
                   GO TO UP2-001
               ELSE
                   GO TO UP2-EX
               END-IF
           END-IF
      *
           MOVE     1   TO    P.
       UP2-010.
           IF  W-SU(CNT,P)  NOT = 0
               GO  TO  UP2-020
           END-IF
           ADD      1   TO    P.
           IF  P NOT >   10
               GO  TO  UP2-010
           END-IF
           GO  TO   UP2-006.
       UP2-020.
           MOVE     W-DATE     TO  W-NGPD.
           PERFORM  UPD-JNSR-RTN    THRU    UPD-JNSR-EX.
           IF  W-HCD(CNT) NOT < 999900
               GO  TO  UP2-006
           END-IF
           IF  W-DC(CNT)      =   2
               GO  TO  UP2-006
           END-IF
           PERFORM  UPD-NJZAI-RTN   THRU    UPD-NJZAI-EX.
           GO  TO  UP2-006.
       UP2-EX.
           EXIT.
      **************************************************
      *    ƒRƒ“ƒgƒ[ƒ‹î•ñh‚Rh@‚t‚o‚R|‚q‚s‚m    *
      **************************************************
       UP3-RTN.
      *           READ     UTRAN     NEXT  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO    UP3-EX
           END-IF
           IF  UTRAN-NRC NOT = 1 AND 3 AND 5
               GO  TO    UP3-RTN
           END-IF
           IF  UTRAN-HPC        = 0
               GO  TO  UP3-RTN
           END-IF
           MOVE     1   TO    P.
       UP3-010.
           IF  UTRAN-SU(P)  NOT = 0
               GO  TO  UP3-020
           END-IF
           ADD      1   TO    P.
           IF  P NOT >   10
               GO  TO  UP3-010
           END-IF
           GO  TO   UP3-RTN.
       UP3-020.
           MOVE     UTRAN-HCD  TO  HI-KEY.
      *           READ     HI-M       WITH  UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     UTRAN-HCD  TO  HI-MHCD
           END-IF
           MOVE     UTRAN-DATE TO  W-NGPD.
           PERFORM  UPD-JNSR-RTN    THRU    UPD-JNSR-EX.
           PERFORM  UPD-NJZAI-RTN   THRU    UPD-NJZAI-EX.
           IF  W-NG             >  W-NGD
               IF  UTRAN-NRC NOT =  5
                   PERFORM  JNYZ-RTN       THRU  JNYZ-EX
               END-IF
           END-IF
           GO  TO   UP3-RTN.
       UP3-EX.
           EXIT.
      **************************************************
      *    ƒRƒ“ƒgƒ[ƒ‹î•ñh‚Sh@‚t‚o‚S|‚q‚s‚m    *
      **************************************************
       UP4-RTN.
      *           READ     HSKIF           AT  END
      *///////////////
           CALL "DB_Read" USING
            "AT END" HSKIF_PNAME1 BY REFERENCE HSKI-R " " RETURNING RET.
           IF  RET = 1
               GO  TO    UP4-EX
           END-IF
           IF  HSKI-UPD         NOT = 0
               GO  TO  UP4-RTN
           END-IF
           MOVE     1   TO    P.
       UP4-010.
           IF  HSKI-SU(P)  NOT = 0
               GO  TO  UP4-020
           END-IF
           ADD      1   TO    P.
           IF  P NOT >   10
               GO  TO  UP4-010
           END-IF
           GO  TO   UP4-RTN.
       UP4-020.
           MOVE     HSKI-HCD   TO  HI-KEY.
      *           READ     HI-M       WITH  UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     HSKI-HCD   TO  HI-MHCD
           END-IF
           MOVE     HSKI-DATE  TO  W-NGPD.
           PERFORM  UPD-JNSR-RTN    THRU    UPD-JNSR-EX.
           PERFORM  UPD-NJZAI-RTN   THRU    UPD-NJZAI-EX.
           MOVE     1          TO  HSKI-UPD.
      *           REWRITE  HSKI-R.
      *///////////////
           CALL "DB_Update" USING
            HSKIF_PNAME1 HSKIF_LNAME HSKI-R RETURNING RET.
           GO  TO   UP4-RTN.
       UP4-EX.
           EXIT.
      ****************************************************
      *    “üoŒÉ—ÝÏ‚eXV@‚t‚o‚c|‚i‚m‚r‚q|‚q‚s‚m    *
      ****************************************************
       UPD-JNSR-RTN.
           IF  JS-SIGN  =  "2"
               PERFORM  SET10-RTN THRU  SET10-EX
           END-IF
           IF  JS-SIGN  =  "3"
               PERFORM  SET11-RTN THRU  SET11-EX
           END-IF
           IF  JS-SIGN  =  "4"
               PERFORM  SET12-RTN THRU  SET12-EX
           END-IF
           MOVE     "9"            TO    JNSR-90.
           PERFORM  WRI-JNSR-RTN   THRU  WRI-JNSR-EX.
           IF  WRI-SW   = 1
               GO  TO  UPD-JNSR-RTN
           END-IF.
       UPD-JNSR-EX.
           EXIT.
      ********************************************************
      *    ‘q•ÊÝŒÉƒ}ƒXƒ^XV@‚t‚o‚c|‚m‚i‚y‚`‚h|‚q‚s‚m    *
      ********************************************************
       UPD-NJZAI-RTN.
           IF  JS-SIGN  =  "2"
               PERFORM  SET6-RTN  THRU  SET6-EX
           END-IF
           IF  JS-SIGN  =  "3"
               PERFORM  SET7-RTN  THRU  SET7-EX
           END-IF
           IF  JS-SIGN  =  "4"
               PERFORM  SET13-RTN THRU  SET13-EX
           END-IF
      ***
           PERFORM  WRI-NJZAI-RTN  THRU  WRI-NJZAI-EX.
           IF  WRI-SW   = 1
               GO  TO  UPD-NJZAI-RTN
           END-IF.
       UPD-NJZAI-010.
           IF  JS-SIGN  =  "2"
               PERFORM  SET8-RTN  THRU  SET8-EX
           END-IF
           IF  JS-SIGN  =  "3"
               PERFORM  SET9-RTN  THRU  SET9-EX
           END-IF
           IF  JS-SIGN  =  "4"
               PERFORM  SET14-RTN THRU  SET14-EX
           END-IF
      ***
           PERFORM  WRI-NJZAI-RTN  THRU  WRI-NJZAI-EX.
           IF  WRI-SW   = 1
               GO  TO  UPD-NJZAI-010
           END-IF.
       UPD-NJZAI-EX.
           EXIT.
       JNYZ-RTN.
           MOVE     HI-MHCD    TO  JNYZ-01.
           MOVE     UTRAN-SIZ  TO  JNYZ-02.
      *           READ     JNYZ         INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JNYZ_PNAME1 BY REFERENCE JNYZ-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  JNYZ-EX
           END-IF
           COMPUTE  JNYZ-0311(01)   =  JNYZ-0311(01)  -  UTRAN-SU(01).
           COMPUTE  JNYZ-0311(02)   =  JNYZ-0311(02)  -  UTRAN-SU(02).
           COMPUTE  JNYZ-0311(03)   =  JNYZ-0311(03)  -  UTRAN-SU(03).
           COMPUTE  JNYZ-0311(04)   =  JNYZ-0311(04)  -  UTRAN-SU(04).
           COMPUTE  JNYZ-0311(05)   =  JNYZ-0311(05)  -  UTRAN-SU(05).
           COMPUTE  JNYZ-0311(06)   =  JNYZ-0311(06)  -  UTRAN-SU(06).
           COMPUTE  JNYZ-0311(07)   =  JNYZ-0311(07)  -  UTRAN-SU(07).
           COMPUTE  JNYZ-0311(08)   =  JNYZ-0311(08)  -  UTRAN-SU(08).
           COMPUTE  JNYZ-0311(09)   =  JNYZ-0311(09)  -  UTRAN-SU(09).
           COMPUTE  JNYZ-0311(10)   =  JNYZ-0311(10)  -  UTRAN-SU(10).
           IF  ZERO          =  JNYZ-0311(01)  AND  JNYZ-0311(02)  AND
                                JNYZ-0311(03)  AND  JNYZ-0311(04)  AND
                                JNYZ-0311(05)  AND  JNYZ-0311(06)  AND
                                JNYZ-0311(07)  AND  JNYZ-0311(08)  AND
                                JNYZ-0311(09)  AND  JNYZ-0311(10)
               PERFORM  DEL-JNYZ-RTN  THRU  DEL-JNYZ-EX
           ELSE
               PERFORM  REW-JNYZ-RTN  THRU  REW-JNYZ-EX
           END-IF.
       JNYZ-EX.
           EXIT.
      **********************************************
      *    “üoŒÉ€–ÚƒZƒbƒg@  ‚r‚d‚s‚PC‚QC‚R    *
      **********************************************
       SET6-RTN.
           MOVE     "R"        TO  W-WRC.
           MOVE     W-SOC      TO  NJZAI-01.
           MOVE     W-HCD(CNT) TO  NJZAI-02.
           MOVE     W-SIZ(CNT) TO  NJZAI-03.
      *           READ     NJZAI        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   "W"     TO   W-WRC
           END-IF
           IF  W-WRC         =  "W"
               INITIALIZE              NJZAI-R
               MOVE     W-SOC      TO  NJZAI-01
               MOVE     W-HCD(CNT) TO  NJZAI-02
               MOVE     W-SIZ(CNT) TO  NJZAI-03
           END-IF
           MOVE     1     TO   P.
       SET6-010.
           IF  W-DC(CNT)  =   1  OR  5
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0511(P) =  NJZAI-0511(P)  -  W-SU(CNT,P)
               ELSE
                 COMPUTE  NJZAI-0811(P) =  NJZAI-0811(P)  -  W-SU(CNT,P)
               END-IF
           END-IF
           IF  W-DC(CNT)  =   0  OR  4  OR 7
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0511(P) =  NJZAI-0511(P)  +  W-SU(CNT,P)
               ELSE
                 COMPUTE  NJZAI-0811(P) =  NJZAI-0811(P)  +  W-SU(CNT,P)
               END-IF
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET6-010
           END-IF.
       SET6-EX.
           EXIT.
      *
       SET7-RTN.
           MOVE     "R"        TO  W-WRC.
           MOVE     UTRAN-SKC  TO  NJZAI-01.
           MOVE     HI-MHCD    TO  NJZAI-02.
           MOVE     UTRAN-SIZ  TO  NJZAI-03.
      *           READ     NJZAI        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   "W"     TO   W-WRC
           END-IF
           IF  W-WRC         =  "W"
               INITIALIZE              NJZAI-R
               MOVE     UTRAN-SKC  TO  NJZAI-01
               MOVE     HI-MHCD    TO  NJZAI-02
               MOVE     UTRAN-SIZ  TO  NJZAI-03
           END-IF
           MOVE     1     TO   P.
       SET7-010.
           IF  UTRAN-NRC NOT =  5
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0411(P) =  NJZAI-0411(P)  +  UTRAN-SU(P)
               ELSE
                 COMPUTE  NJZAI-0711(P) =  NJZAI-0711(P)  +  UTRAN-SU(P)
               END-IF
           END-IF
           IF  UTRAN-NRC     =  5
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0411(P) =  NJZAI-0411(P)  -  UTRAN-SU(P)
               ELSE
                 COMPUTE  NJZAI-0711(P) =  NJZAI-0711(P)  -  UTRAN-SU(P)
               END-IF
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET7-010
           END-IF.
       SET7-EX.
           EXIT.
      *
       SET8-RTN.
           MOVE     "R"        TO  W-WRC.
           MOVE     9          TO  NJZAI-01.
           MOVE     W-HCD(CNT) TO  NJZAI-02.
           MOVE     W-SIZ(CNT) TO  NJZAI-03.
      *           READ     NJZAI        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   "W"     TO   W-WRC
           END-IF
           IF  W-WRC         =  "W"
               INITIALIZE              NJZAI-R
               MOVE     9          TO  NJZAI-01
               MOVE     W-HCD(CNT) TO  NJZAI-02
               MOVE     W-SIZ(CNT) TO  NJZAI-03
           END-IF
           MOVE     1     TO   P.
       SET8-010.
           IF  W-DC(CNT)  =   1  OR  5
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0511(P) =  NJZAI-0511(P)  -  W-SU(CNT,P)
               ELSE
                 COMPUTE  NJZAI-0811(P) =  NJZAI-0811(P)  -  W-SU(CNT,P)
               END-IF
           END-IF
           IF  W-DC(CNT)  =   0  OR  4  OR  7
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0511(P) =  NJZAI-0511(P)  +  W-SU(CNT,P)
               ELSE
                 COMPUTE  NJZAI-0811(P) =  NJZAI-0811(P)  +  W-SU(CNT,P)
               END-IF
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET8-010
           END-IF.
       SET8-EX.
           EXIT.
      *
       SET9-RTN.
           MOVE     "R"        TO  W-WRC.
           MOVE     9          TO  NJZAI-01.
           MOVE     HI-MHCD    TO  NJZAI-02.
           MOVE     UTRAN-SIZ  TO  NJZAI-03.
      *           READ     NJZAI        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   "W"     TO   W-WRC
           END-IF
           IF  W-WRC         =  "W"
               INITIALIZE              NJZAI-R
               MOVE     9          TO  NJZAI-01
               MOVE     HI-MHCD    TO  NJZAI-02
               MOVE     UTRAN-SIZ  TO  NJZAI-03
           END-IF
           MOVE     1     TO   P.
       SET9-010.
           IF  UTRAN-NRC NOT =  5
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0411(P) =  NJZAI-0411(P)  +  UTRAN-SU(P)
               ELSE
                 COMPUTE  NJZAI-0711(P) =  NJZAI-0711(P)  +  UTRAN-SU(P)
               END-IF
           END-IF
           IF  UTRAN-NRC     =  5
               IF  W-NG       >    W-NGD
                 COMPUTE  NJZAI-0411(P) =  NJZAI-0411(P)  -  UTRAN-SU(P)
               ELSE
                 COMPUTE  NJZAI-0711(P) =  NJZAI-0711(P)  -  UTRAN-SU(P)
               END-IF
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET9-010
           END-IF.
       SET9-EX.
           EXIT.
      *
       SET10-RTN.
       SET10-005.
           MOVE     SPACE      TO  JNSR-R.
           INITIALIZE              JNSR-R.
           MOVE     W-HCD(CNT) TO  JNSR-01.
           MOVE     W-DATE     TO  JNSR-02  JNSR-16  JNSR-20.
           IF  W-DC(CNT)  =   5  OR  0  OR  4  OR  7
               MOVE     25         TO  JNSR-03
           ELSE
               MOVE     23         TO  JNSR-03
           END-IF
           MOVE     W-DNO      TO  JNSR-04  JNSR-181 JNSR-221.
           MOVE     W-GNO(CNT) TO  JNSR-05  JNSR-182 JNSR-222.
           MOVE     W-SOC      TO  JNSR-06.
           MOVE     W-SIZ(CNT) TO  JNSR-07.
           MOVE     W-DC(CNT)  TO  JNSR-10.
           MOVE     W-BC1D(CNT) TO  JNSR-82.
           MOVE     W-TCD      TO  JNSR-111 JNSR-19.
           MOVE     W-CCD      TO  JNSR-112.
           MOVE     1          TO  JNSR-17  JNSR-21.
           IF  JNSR-112   =   ZERO
               MOVE  001  TO  JNSR-112
           END-IF
           MOVE       W-BI           TO  JNSR-24.
           IF  W-NG       >   W-NGD
               MOVE       1              TO  JNSR-91
           ELSE
               MOVE       0              TO  JNSR-91
           END-IF
           MOVE     1     TO   P.
       SET10-010.
           IF  W-DC(CNT)  =   0  OR  1 OR 4  OR  7
               MOVE       W-SU(CNT,P)    TO JNSR-081(P)
           ELSE
               COMPUTE    JNSR-081(P)    =  W-SU(CNT,P)   *  -1
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET10-010
           END-IF.
       SET10-EX.
           EXIT.
      *****
       SET11-RTN.
           MOVE     SPACE      TO  JNSR-R.
           INITIALIZE              JNSR-R.
           MOVE     HI-MHCD    TO  JNSR-01.
           MOVE     UTRAN-DATE TO  JNSR-02  JNSR-16  JNSR-20.
           IF  UTRAN-NRC =     1  OR  3
               MOVE 10   TO   JNSR-03
           END-IF
           IF  UTRAN-NRC =     5
               MOVE 12   TO   JNSR-03
           END-IF
           MOVE     UTRAN-UNO  TO  JNSR-04  JNSR-181 JNSR-221.
           MOVE     UTRAN-GYO  TO  JNSR-05  JNSR-182 JNSR-222.
           MOVE     UTRAN-SKC  TO  JNSR-06.
           MOVE     UTRAN-SIZ  TO  JNSR-07.
           MOVE     UTRAN-SSC  TO  JNSR-09.
           MOVE     UTRAN-BC1D TO  JNSR-82.
           MOVE     5          TO  JNSR-17  JNSR-21.
           IF  W-NG       >   W-NGD
               MOVE       1              TO JNSR-91
           ELSE
               MOVE       0              TO JNSR-91
           END-IF
           MOVE     1     TO   P.
       SET11-010.
           MOVE     UTRAN-SU(P) TO JNSR-081(P).
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET11-010
           END-IF.
       SET11-EX.
           EXIT.
       SET12-RTN.
           MOVE     SPACE      TO  JNSR-R.
           INITIALIZE              JNSR-R.
           MOVE     HI-MHCD    TO  JNSR-01.
           MOVE     HSKI-DATE  TO  JNSR-02  JNSR-16  JNSR-20.
           MOVE     21         TO  JNSR-03.
           MOVE     HSKI-UNO   TO  JNSR-04  JNSR-181 JNSR-221.
           MOVE     HSKI-GYO   TO  JNSR-05  JNSR-182 JNSR-222.
           MOVE     HSKI-SKC   TO  JNSR-06.
           MOVE     HSKI-SIZ   TO  JNSR-07.
           MOVE     0          TO  JNSR-09.
           MOVE     HI-BCD1    TO  JNSR-82.
           MOVE     5          TO  JNSR-17  JNSR-21.
           IF  W-NG       >   W-NGD
               MOVE       1              TO JNSR-91
           ELSE
               MOVE       0              TO JNSR-91
           END-IF
           MOVE     1     TO   P.
       SET12-010.
           MOVE     HSKI-SU(P) TO JNSR-081(P).
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET12-010
           END-IF.
       SET12-EX.
           EXIT.
       SET13-RTN.
           MOVE     "R"        TO  W-WRC.
           MOVE     HSKI-SKC   TO  NJZAI-01.
           MOVE     HI-MHCD    TO  NJZAI-02.
           MOVE     HSKI-SIZ   TO  NJZAI-03.
      *           READ     NJZAI        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   "W"     TO   W-WRC
           END-IF
           IF  W-WRC         =  "W"
               INITIALIZE              NJZAI-R
               MOVE     HSKI-SKC   TO  NJZAI-01
               MOVE     HI-MHCD    TO  NJZAI-02
               MOVE     HSKI-SIZ   TO  NJZAI-03
           END-IF
           MOVE     1     TO   P.
       SET13-010.
           IF  W-NG       >    W-NGD
               COMPUTE  NJZAI-0411(P) =  NJZAI-0411(P)  +  HSKI-SU(P)
           ELSE
               COMPUTE  NJZAI-0711(P) =  NJZAI-0711(P)  +  HSKI-SU(P)
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET13-010
           END-IF.
       SET13-EX.
           EXIT.
       SET14-RTN.
           MOVE     "R"        TO  W-WRC.
           MOVE     9          TO  NJZAI-01.
           MOVE     HI-MHCD    TO  NJZAI-02.
           MOVE     HSKI-SIZ   TO  NJZAI-03.
      *           READ     NJZAI        INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE   "W"     TO   W-WRC
           END-IF
           IF  W-WRC         =  "W"
               INITIALIZE              NJZAI-R
               MOVE     9          TO  NJZAI-01
               MOVE     HI-MHCD    TO  NJZAI-02
               MOVE     HSKI-SIZ   TO  NJZAI-03
           END-IF
           MOVE     1     TO   P.
       SET14-010.
           IF  W-NG       >    W-NGD
               COMPUTE  NJZAI-0411(P) =  NJZAI-0411(P)  +  HSKI-SU(P)
           ELSE
               COMPUTE  NJZAI-0711(P) =  NJZAI-0711(P)  +  HSKI-SU(P)
           END-IF
           ADD      1     TO   P.
           IF  P NOT >    10
               GO  TO  SET14-010
           END-IF.
       SET14-EX.
           EXIT.
      ***************************
      *    “üoŒÉ—ÝÏ‚e@XV   *
      ***************************
       WRI-JNSR-RTN.
           MOVE     0   TO   WRI-SW.
           IF JNSR-06  = 9 OR 0
              GO TO WRI-JNSR-EX
           END-IF
      *           WRITE    JNSR-R      INVALID KEY   GO  TO  WRI-JNSR-010.
      *//////////////
           CALL "DB_Insert" USING
            JNSR_PNAME1 JNSR_LNAME JNSR-R RETURNING RET.
           IF  RET = 1
               GO  TO  WRI-JNSR-010
           END-IF.
       WRI-JNSR-005.
           GO  TO   WRI-JNSR-EX.
       WRI-JNSR-010.
           MOVE     JNSR-01   TO  W-K1.
           MOVE     JNSR-02   TO  W-K2.
           MOVE     JNSR-03   TO  W-K3.
           MOVE     JNSR-04   TO  W-K4.
           MOVE     JNSR-05   TO  W-K5.
           IF  ERR-STAT =  "24"
               GO  TO  WRI-JNSR-999
           END-IF
           IF  ERR-STAT NOT = "00"
               MOVE  "W"         TO  ERR-M
               MOVE  "JNSR"      TO  ERR-F
               MOVE  W-KEY       TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF
           GO  TO   WRI-JNSR-005.
       WRI-JNSR-999.
           MOVE     1          TO  WRI-SW.
           MOVE    "W"         TO  ERR-M.
           MOVE    "JNSR"      TO  ERR-F.
           MOVE     W-KEY      TO  ERR-K.
           MOVE     ERR-STAT   TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE JNSR_IDLST JNSR_PNAME1.
           STOP    "´Ø± ¶¸Á®³ºÞ¤»²¶²·° ¦ µ½!".
           CALL "SD_Output" USING "EMSG-SP" EMSG-SP "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" JNSR_PNAME1 "SHARED" BY REFERENCE JNSR_IDLST "3"
            "JNSR-KEY1" BY REFERENCE JNSR-KEY1 "JNSR-KEY2" BY REFERENCE
            JNSR-KEY2 "JNSR-KEY3" BY REFERENCE JNSR-KEY3.
       WRI-JNSR-EX.
           EXIT.
      **********************************
      *    ‘q•ÊÝŒÉƒ}ƒXƒ^@XV  (NEW) *
      **********************************
       WRI-NJZAI-RTN.
           IF  W-WRC         =  "R"
               GO  TO  WRI-NJZAI-050
           END-IF
           MOVE     0   TO   WRI-SW.
      *           WRITE    NJZAI-R     INVALID KEY   GO  TO  WRI-NJZAI-010.
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  WRI-NJZAI-010
           END-IF.
       WRI-NJZAI-005.
           GO  TO   WRI-NJZAI-EX.
       WRI-NJZAI-010.
           IF  ERR-STAT =  "24"
               GO  TO  WRI-NJZAI-020
           END-IF
           IF  ERR-STAT NOT = "00"
               MOVE  "W"         TO  ERR-M
               MOVE  "NJZAI"     TO  ERR-F
               MOVE  NJZAI-KEY   TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF
           GO  TO   WRI-NJZAI-005.
       WRI-NJZAI-020.
           MOVE     1          TO  WRI-SW.
           MOVE    "W"         TO  ERR-M.
           MOVE    "NJZAI"     TO  ERR-F.
           MOVE     NJZAI-KEY  TO  ERR-K.
           MOVE     ERR-STAT   TO  ERR-FLG.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           STOP    "´Ø± ¶¸Á®³ºÞ¤»²¶²·° ¦ µ½!".
           CALL "SD_Output" USING "EMSG-SP" EMSG-SP "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           GO  TO   WRI-NJZAI-EX.
       WRI-NJZAI-050.
      *           REWRITE  NJZAI-R               INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"         TO  ERR-M
               MOVE  "NJZAI"      TO  ERR-F
               MOVE  NJZAI-KEY   TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       WRI-NJZAI-EX.
           EXIT.
      *****
       REW-JNYZ-RTN.
      *           REWRITE  JNYZ-R                INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JNYZ_PNAME1 JNYZ_LNAME JNYZ-R RETURNING RET.
           IF  RET = 1
               MOVE  "R"         TO  ERR-M
               MOVE  "JNYZ"      TO  ERR-F
               MOVE  JNYZ-KEY    TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       REW-JNYZ-EX.
           EXIT.
      *****
       DEL-JNYZ-RTN.
      *           DELETE   JNYZ                  INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JNYZ_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE  "D"         TO  ERR-M
               MOVE  "JNYZ"      TO  ERR-F
               MOVE  JNYZ-KEY    TO  ERR-K
               PERFORM     ERR-RTN   THRU      ERR-EX
           END-IF.
       DEL-JNYZ-EX.
           EXIT.
      ***********************************
      *    ƒRƒ“ƒgƒ[ƒ‹‚e@”NŒŽƒZƒbƒg   *
      ***********************************
       JCON-NG-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE  SPACE        TO  JCON6-KEY.
           MOVE  6            TO  JCON6-01.
      *           READ  JCON       UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "SD_Output" USING
                "ERR-MSG1" ERR-MSG1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO   JCON-NG-RTN
           END-IF
           MOVE  JCON6-0312   TO  W-NEN2.
           MOVE  JCON6-032    TO  W-GET.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           MOVE  ZERO         TO  W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       JCON-NG-EX.
           EXIT.
