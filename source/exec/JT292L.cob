       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT292L.
       AUTHOR.                        E-SHIGIHARA.
      ***************************************************
      *    PROGRAM        : ìæà”êÊï éÛíçécí†Å@Å@Å@Å@    *
      *    DATA WRITTEN   : 87/08/28                    *
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
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       01  R2.
           02  R2-R        OCCURS  4.
               03  R2-K2             PIC X(05).
               03  FILLER            PIC X(04).
               03  R2-00             PIC 9(03).
               03  F                 PIC X(02).
               03  R2-021            PIC 9(02).
               03  R2-022            PIC X(01).
               03  R2-02             PIC 9(02).
               03  R2-03             PIC X(01).
               03  R2-M.
                   04  R2-04         PIC 9(02).
                   04  F             PIC X(01).
                   04  R2-06         PIC 9(02).
                   04  R2-B          PIC X(01).
                   04  R2-07         PIC 9(02).
                   04  F             PIC X(01).
               03  R2-MR  REDEFINES  R2-M.
                   04  FILLER        PIC X(01).
                   04  R2-KEI        PIC N(04).
               03  R2-08             PIC 9(06).
               03  R2-E              PIC X(01).
               03  R2-09             PIC 9(01).
               03  FILLER            PIC X(01).
               03  R2-16             PIC 9(01).
               03  R2-17        OCCURS   10.
                   04  R2-171        PIC ---,---.
               03  R2-18             PIC ----,--9.
               03  FILLER            PIC X(01).
               03  F                 PIC X(13).
               03  R2-22             PIC X(10).
       01  HEAD1.
           02  K-2                   PIC X(05)  VALUE  X"1A24212474".
           02  FILLER                PIC X(32)  VALUE   SPACE.
           02  FILLER                PIC N(05)  VALUE
                 "ìæà”êÊï Å@".
           02  H-PM                  PIC N(03).
           02  FILLER                PIC N(03)  VALUE
                 "Å@écí†".
           02  FILLER                PIC X(02)  VALUE  SPACE.
           02  H-CHK                 PIC N(05)  VALUE  SPACE.
           02  FILLER                PIC X(06)  VALUE  SPACE.
           02  H-SEN                 PIC N(05).
           02  FILLER                PIC X(32)  VALUE  SPACE.
           02  H-NEN                 PIC 9(02).
           02  FILLER                PIC N(01)  VALUE  "îN".
           02  H-GET                 PIC Z9.
           02  FILLER                PIC N(01)  VALUE  "åé".
           02  H-PEY                 PIC Z9.
           02  FILLER                PIC N(01)  VALUE  "ì˙".
           02  FILLER                PIC X(07)  VALUE  "     P.".
           02  H-PAGE                PIC ZZ9.
       01  HEAD2.
           02  K-1                   PIC X(05)  VALUE  X"1A24212078".
           02  FILLER                PIC X(05)  VALUE  "∫∞ƒﬁ ".
           02  FILLER                PIC N(10)  VALUE
                 "ìæÅ@Å@à”Å@Å@êÊÅ@Å@ñº".
           02  FILLER                PIC X(115)  VALUE  SPACE.
       01  HEAD3.
           02  FILLER                PIC X(09)  VALUE  "   ∫∞ƒﬁ  ".
           02  FILLER                PIC N(08)  VALUE
                 "ïiÅ@Å@Å@Å@Å@ñºÅ@".
           02  FILLER                PIC X(114)  VALUE  SPACE.
       01  HEAD4.
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "íºëó".
           02  FILLER                PIC X(02)  VALUE  SPACE.
           02  FILLER                PIC X(02)  VALUE  SPACE.
           02  FILLER                PIC N(04)  VALUE  "éÛíçì˙Å@".
           02  FILLER                PIC X(01)  VALUE  SPACE.
           02  FILLER                PIC N(04)  VALUE  "ó\íËì˙Å@".
           02  FILLER                PIC N(06)  VALUE  "éÛíçáÇÅ|çsÅ@".
           02  FILLER                PIC X(01)  VALUE  "1".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇRçÜ".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇQçÜ".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇPçÜ".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "ÇOçÜ".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "Å@íÜ".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "Å@ëÂ".
           02  FILLER                PIC X(04)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "ì¡ëÂ".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "28.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "29.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "30.0".
           02  FILLER                PIC X(09)  VALUE  SPACE.
           02  FILLER                PIC X(13)  VALUE  SPACE.
           02  FILLER                PIC N(04)  VALUE  "îıÅ@Å@çl".
           02  FILLER                PIC X(04)  VALUE  SPACE.
       01  HEAD5.
           02  FILLER                PIC X(33)  VALUE  SPACE.
           02  FILLER                PIC X(01)  VALUE  "2".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "12.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "13.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "13.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "14.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "15.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "16.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "17.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "18.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "19.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "20.0".
           02  FILLER                PIC X(32)  VALUE  SPACE.
       01  HEAD6.
           02  FILLER                PIC X(33)  VALUE  SPACE.
           02  FILLER                PIC X(01)  VALUE  "3".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "21.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "21.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "22.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "22.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "23.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "23.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "24.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "24.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "25.0".
           02  FILLER                PIC X(39)  VALUE  SPACE.
       01  HEAD7.
           02  FILLER                PIC X(33)  VALUE  SPACE.
           02  FILLER                PIC X(01)  VALUE  "4".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "24.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "24.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "25.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "25.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "26.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "26.5".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "27.0".
           02  FILLER                PIC X(03)  VALUE  SPACE.
           02  FILLER                PIC X(04)  VALUE  "27.5".
           02  FILLER                PIC X(19)  VALUE  SPACE.
           02  FILLER                PIC N(02)  VALUE  "çáåv".
           02  FILLER                PIC X(24)  VALUE  SPACE.
       01  ACT-WORK.
           02  W-PC                  PIC 9(01).
           02  I                     PIC 9(02).
           02  TOT                   PIC S9(07).
           02  OLD-1                 PIC 9(04).
           02  OLD-2                 PIC 9(06).
           02  OLD-3                 PIC X(07).
           02  NEW-1                 PIC 9(04).
           02  NEW-2                 PIC 9(06).
           02  CNT                   PIC 9(01).
           02  SW1                   PIC 9(01).
           02  SW2                   PIC 9(01).
           02  W-ZAN                 PIC S9(06).
           02  0-CNT     OCCURS  4   PIC  9(02).
           02  W-CHK                 PIC  9(01).
           02  W-SEN                 PIC  9(01).
           02  W-JYU.
               03  W-J1              PIC  9(01).
               03  W-J2              PIC  9(05).
           02  N                     PIC  9(01).
           02  CNT-M                 PIC  9(01).
           02  CNT-M1                PIC  9(01).
           02  W-NC                  PIC  9(01).
           02  O-SW1                 PIC  9(01).
           02  O-SW2                 PIC  9(01).
           02  ASW                   PIC  9(01).
           02  J                     PIC  9(01).
           02  W-GC                  PIC  9(01).
           02  W-DCC                 PIC  9(01).
       01  W-A.
           02  W-AA        OCCURS         4.
               03  W-AAA             PIC  S9(06)  OCCURS 10.
       01  W-B.
           02  W-BB        OCCURS         4.
               03  W-BBB             PIC  S9(06)  OCCURS 10.
       01  W-AREA.
           02  PCNT                  PIC 9(03)   VALUE   0.
           02  LCNT                  PIC 9(02)   VALUE  90.
           02  W-KEI                 PIC N(04).
           02  W-KEY.
               03  W-JNO             PIC 9(06).
               03  W-GYO             PIC 9(01).
           02  G-SW                  PIC 9(01).
       01  WYMD.
           02  WYY                   PIC 9(02).
           02  WMM                   PIC 9(02).
           02  WDD                   PIC 9(02).
       COPY    LWMSG.
      *
           COPY  LTWK04.
           COPY  LITCM.
           COPY  LIHIM2.
      *FD  P-F
       01  P-R.
           02  R1-R.
               03  R1-K1                 PIC X(05).
               03  R1-00                 PIC 9(04).
               03  F                     PIC X(01).
               03  R1-01                 PIC N(26).
               03  R1-K2                 PIC X(05).
               03  F                     PIC X(189).
           02  R3-R   REDEFINES R1-R.
               03  R3-K1                 PIC X(05).
               03  F                     PIC X(02).
               03  R3-00                 PIC 9(06).
               03  F                     PIC X(01).
               03  R3-01                 PIC N(24).
               03  R3-K2                 PIC X(05).
               03  F                     PIC X(189).
       77  F                             PIC X(01).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(36) VALUE
                     "                                    ".
               03  FILLER  PIC  X(48) VALUE
                    "ìæà”êÊï Å@éÛíçÅEóaÇËÅEéÊÇÊÇØÅ@écí†Å@Å@ÅiîíéÜÅj".
       COPY    LSMSG.
      *
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "82" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-01" " " "1" "0" "82" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-01" "RX" "1" "23" "36" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-01" "X" "1" "24" "46" "01DSP-01" " " RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ************************************
      *    ÉÅÉCÉìÅ@ÉãÅ[É`Éì              *
      ************************************
       HAJIME.
           PERFORM   INT-RTN   THRU  INT-EX.
       MR005.
      *           READ      JT-WK04           AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "ERR-02" ERR-02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               GO  TO  MR999
           END-IF
           MOVE  WK04-88    TO  W-CHK.
           MOVE  WK04-89    TO  W-PC.
           MOVE  WK04-90    TO  W-SEN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
           IF  W-PC     =  0
               MOVE  "éÛíçÅ@"    TO  H-PM
           END-IF
           IF  W-PC     =  5
               MOVE  "óaÇËÅ@"    TO  H-PM
           END-IF
           IF  W-PC     =  6
               MOVE  "éÊÇÊÇØ"    TO  H-PM
           END-IF
           IF  W-CHK    =   1
               MOVE  "Åiéwê}ä‹Åj"   TO  H-CHK
           ELSE
               MOVE  ALL  "Å@"    TO  H-CHK
           END-IF.
       MR050.
      *           READ    JT-WK04                  AT      END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               PERFORM     TOT-RTN     THRU     TOT-EX
               PERFORM     GP1-RTN     THRU     GP1-EX
               PERFORM     GP2-RTN     THRU     GP2-EX
               GO  TO  MR999
           END-IF
           MOVE  WK04-07    TO  W-JNO.
           MOVE  WK04-08    TO  W-GYO.
           IF (WK04-04  NOT   =   OLD-1)  OR  (CNT  =  0)
               PERFORM  TOT-RTN  THRU   TOT-EX
               PERFORM     GP1-RTN     THRU     GP1-EX
               PERFORM     GP2-RTN     THRU     GP2-EX
               MOVE     1   TO   CNT-M
               GO  TO  MR060
           END-IF
           IF  WK04-05  NOT   =   OLD-2
               PERFORM  TOT-RTN  THRU   TOT-EX
               PERFORM     GP1-RTN     THRU     GP1-EX
               MOVE  1  TO  CNT-M1
               GO  TO  MR060
           END-IF
           IF  W-KEY     NOT  =   OLD-3
               PERFORM  TOT-RTN  THRU   TOT-EX
               GO  TO  MR060
           END-IF
           IF  N  >    4
               PERFORM  TOT-RTN  THRU  TOT-EX
           END-IF.
       MR060.
           PERFORM     MOV-RTN    THRU   MOV-EX.
           GO  TO  MR050.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           INITIALIZE     ACT-WORK    R2.
           MOVE     ZERO    TO   W-A   W-B  G-SW.
           ACCEPT         WYMD        FROM     DATE.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK04_PNAME1 " " BY REFERENCE JT-WK04_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
       END-EX.
           EXIT.
      **********************************************
      *    ÇgÇdÇcÅ|ÇqÇsÇm                          *
      **********************************************
       HED-010.
           MOVE   SPACE     TO  P-R.
           IF  LCNT      NOT     =    90
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           ADD    1         TO  PCNT.
           IF  W-SEN      =   0
               MOVE  "Åyã≥Å@àÁÅz"   TO  H-SEN
           END-IF
           IF  W-SEN      =   1
               MOVE  "ÅyàÍÅ@î Åz"   TO  H-SEN
           END-IF
           IF  W-SEN      =   9
               MOVE  ALL  "Å@"    TO  H-SEN
           END-IF
           MOVE   WYY       TO  H-NEN.
           MOVE   WMM       TO  H-GET.
           MOVE   WDD       TO  H-PEY.
           MOVE   PCNT      TO  H-PAGE.
           MOVE   HEAD1     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   HEAD2     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   HEAD3     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   HEAD4     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   HEAD5     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   HEAD6     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   HEAD7     TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           MOVE   8        TO  LCNT.
       HED-EX.
           EXIT.
      ***************************************************
      *    ÇsÇbÇlÅ|ÇqÇsÇm                               *
      ***************************************************
       TCM-RTN.
           IF  LCNT    NOT <  60
               PERFORM     HED-010  THRU  HED-EX
           END-IF
           MOVE   SPACE    TO  P-R.
           MOVE   K-1      TO  R1-K1.
           MOVE   K-2      TO  R1-K2.
           MOVE   OLD-1    TO    TC-TCD   R1-00.
           MOVE 001 TO TC-CCD.
      *           READ   TC-M    UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO    TC-NAME
           END-IF
           MOVE   TC-NAME   TO    R1-01.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           ADD    1        TO  LCNT.
       TCM-EX.
           EXIT.
      ***************************************************
      *    ÇgÇhÇlÅ|ÇqÇsÇm                               *
      ***************************************************
       HIM-RTN.
           IF  LCNT    NOT <  61
               PERFORM     HED-010  THRU  HED-EX
               PERFORM     TCM-RTN  THRU  TCM-EX
           END-IF
           MOVE   SPACE    TO  P-R.
           MOVE   K-1      TO  R3-K1.
           MOVE   K-2      TO  R3-K2.
           MOVE   OLD-2    TO    HI-MHCD HI-HCD   R3-00.
      *           READ   HI2-M  UNLOCK   INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   ALL "Å@"  TO    HI-NAME
           END-IF
           MOVE   HI-NAME   TO    R3-01.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO  P-R.
           ADD    1        TO  LCNT.
       HIM-EX.
           EXIT.
      ***************************************************
      *    ÇlÇnÇuÅ|ÇqÇsÇm                               *
      ***************************************************
       MOV-RTN.
           IF  N  =    0
               MOVE    1  TO  N
           END-IF
           MOVE  SPACE     TO  R2-R(N).
           IF  N     =    1
               GO  TO  MOV-020
           END-IF
           GO  TO  MOV-030.
       MOV-020.
           IF  WK04-10  NOT   =  001
               MOVE  WK04-10     TO    R2-00(N)
           END-IF
           MOVE   WK04-021    TO     R2-021(N).
           MOVE   WK04-022    TO     R2-02(N).
           MOVE     "/"       TO     R2-03(N) R2-022(N).
           MOVE   WK04-023    TO     R2-04(N).
           MOVE     "-"       TO     R2-E(N).
           IF  WK04-06       NOT   =  ZERO
               MOVE     "/"       TO     R2-B(N)
               MOVE   WK04-062    TO     R2-06(N)
               MOVE   WK04-063    TO     R2-07(N)
           END-IF
           MOVE   WK04-07     TO     R2-08(N).
           MOVE   WK04-08     TO     R2-09(N).
           MOVE   WK04-22     TO     R2-22(N).
       MOV-030.
           MOVE   WK04-09   TO  R2-16(N).
           MOVE   0       TO  I.
       MOV-040.
           ADD    1       TO  I.
           IF  I       >   10
               GO  TO  MOV-050
           END-IF
           IF  W-CHK   =   0
               COMPUTE  W-ZAN  =  WK04-1111(I)  -  WK04-141(I)
                                                -  WK04-1211(I)
           ELSE
               COMPUTE  W-ZAN  =  WK04-1111(I)  -  WK04-141(I)
                               -  WK04-1211(I)  -  WK04-151(I)
           END-IF
           MOVE    W-ZAN       TO   R2-171(N , I).
           ADD     W-ZAN       TO         TOT.
           IF  WK04-09      =        1
               ADD   W-ZAN   TO    W-AAA(1 , I)   W-BBB(1 , I)
               IF  SW1        <        1
                   MOVE       1        TO       SW1
                   IF  SW2        <        1
                       MOVE       1        TO       SW2
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        2
               ADD   W-ZAN   TO    W-AAA(2 , I)   W-BBB(2 , I)
               IF  SW1        <        2
                   MOVE       2        TO       SW1
                   IF  SW2        <        2
                       MOVE       2        TO       SW2
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        3
               ADD   W-ZAN   TO    W-AAA(3 , I)   W-BBB(3 , I)
               IF  SW1        <        3
                   MOVE       3        TO       SW1
                   IF  SW2        <        3
                       MOVE       3        TO       SW2
                   END-IF
               END-IF
           END-IF
           IF  WK04-09      =        4
               ADD   W-ZAN   TO    W-AAA(4 , I)   W-BBB(4 , I)
               MOVE       4        TO       SW1   SW2
           END-IF
           IF  W-ZAN        =        0
               ADD     1     TO    0-CNT(N)
           END-IF
           MOVE    0     TO   W-ZAN.
           GO  TO  MOV-040.
       MOV-050.
           IF  0-CNT(N)   =    10
               MOVE    0  TO   0-CNT(N)
               MOVE   O-SW1   TO   SW1
               MOVE   O-SW2   TO   SW2
               GO  TO  MOV-EX
           END-IF
           MOVE   WK04-04    TO  OLD-1.
           MOVE   WK04-05    TO  OLD-2.
           MOVE   W-KEY      TO  OLD-3
           MOVE     1        TO   CNT.
           ADD      1        TO   N.
           MOVE     SW1   TO   O-SW1.
           MOVE     SW2   TO   O-SW2.
       MOV-EX.
           EXIT.
      ***************************************************
      *    ÇsÇnÇsÅ[ÇqÇsÇm                               *
      ***************************************************
       TOT-RTN.
           IF  CNT      =       0
               GO  TO  TOT-EX
           END-IF
           IF  W-GC     =       1
               IF  W-DCC       NOT  =  9
                   MOVE   SPACE     TO     P-R
                   GO     TO    TOT-030
               END-IF
           END-IF
           IF  N        =       1
               GO  TO  TOT-EX
           END-IF
           COMPUTE   N  =  N   -   1.
           IF  TOT      =       ZERO
               IF  R2-R(N)   =   SPACE
                   MOVE    0    TO  N
                   GO  TO  TOT-EX
               END-IF
           END-IF
           IF  (CNT-M       =  1)  OR  (LCNT  =  90)
               PERFORM  TCM-RTN  THRU  TCM-EX
               MOVE   1        TO  CNT-M1
           END-IF
           IF  (CNT-M1      =  1)  OR  (LCNT  =  8)
               IF  W-GC      =   0
                   PERFORM  HIM-RTN  THRU  HIM-EX
               END-IF
           END-IF
           MOVE   N    TO     W-NC.
           MOVE   0    TO     N.
       TOT-010.
           ADD    1  TO    N.
           IF  N  =  W-NC
               GO  TO  TOT-020
           END-IF
           IF  0-CNT(N)  =   10
               MOVE  0  TO  0-CNT(N)
               GO  TO  TOT-010
           END-IF
           IF  LCNT    NOT <  62
               PERFORM     HED-010  THRU  HED-EX
               PERFORM  TCM-RTN  THRU  TCM-EX
               IF  W-GC      =   0
                   PERFORM  HIM-RTN  THRU  HIM-EX
               END-IF
           END-IF
           MOVE SPACE  TO  P-R.
           MOVE K-2    TO  R2-K2(N).
           MOVE    R2-R(N)  TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE    SPACE TO  P-R.
           ADD     1       TO  LCNT.
           MOVE    0       TO  0-CNT(N).
           IF  W-DCC   =   5
               MOVE    9        TO  W-DCC
           END-IF
           IF  W-DCC   =   0
               MOVE    5        TO  W-DCC
           END-IF
           GO  TO  TOT-010.
       TOT-020.
           IF  LCNT    NOT <  62
               PERFORM     HED-010  THRU  HED-EX
               PERFORM  TCM-RTN  THRU  TCM-EX
               IF  W-GC      =   0
                   PERFORM  HIM-RTN  THRU  HIM-EX
               END-IF
           END-IF
           MOVE       TOT     TO    R2-18(N).
           MOVE     SPACE      TO    P-R.
           MOVE     K-2        TO    R2-K2(N).
           MOVE     R2-R(N)    TO    P-R.
       TOT-030.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE     SPACE      TO    P-R.
           IF  W-DCC   =   5
               MOVE    9        TO  W-DCC
           END-IF
           IF  W-DCC   =   0
               MOVE    5        TO  W-DCC
           END-IF
           ADD      1          TO    LCNT.
           MOVE    0       TO  0-CNT(N).
           MOVE     0          TO     TOT     N   CNT-M  CNT-M1.
           IF  W-GC    =   1
               MOVE    0         TO  W-DCC
           END-IF.
       TOT-EX.
      ***************************************************
      *    ÇfÇoÇPÅ|ÇqÇsÇm                               *
      ***************************************************
       GP1-RTN.
           IF  CNT      =       0
               GO  TO  GP1-120
           END-IF
           MOVE "Å@è¨Å@åv"  TO     W-KEI.
           MOVE  SPACE        TO     R2.
           MOVE     1      TO  J  N.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-030
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   1       TO     R2-16(N).
           MOVE   0       TO     I.
       GP1-010.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-020
           END-IF
           MOVE   W-AAA(1 , I)  TO     R2-171(N , I).
           ADD    W-AAA(1 , I)  TO     TOT.
           GO  TO  GP1-010.
       GP1-020.
           IF  SW1       =       1
               MOVE   1       TO    W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP1-120
           END-IF.
       GP1-030.
           MOVE     2      TO  J.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF       ASW    =   1
                    GO  TO  GP1-060
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   2       TO     R2-16(N).
           MOVE   0       TO     I.
       GP1-040.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-050
           END-IF
           MOVE   W-AAA(2 , I)  TO     R2-171(N , I).
           ADD    W-AAA(2 , I)  TO     TOT.
           GO  TO  GP1-040.
       GP1-050.
           IF  SW1       =       2
               MOVE   1       TO    W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP1-120
           END-IF.
       GP1-060.
           MOVE     3      TO  J.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-090
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   3       TO     R2-16(N).
           MOVE   0       TO     I.
       GP1-070.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-080
           END-IF
           MOVE   W-AAA(3 , I)  TO     R2-171(N , I).
           ADD    W-AAA(3 , I)  TO     TOT.
           GO  TO  GP1-070.
       GP1-080.
           IF  SW1       =       3
               MOVE   1       TO    W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP1-120
           END-IF.
       GP1-090.
           MOVE     4      TO  J.
           PERFORM  MZA-RTN  THRU  MZA-EX.
           IF  ASW    =   1
               GO  TO  GP1-120
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   4       TO     R2-16(N).
           MOVE   0       TO     I.
       GP1-100.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP1-110
           END-IF
           MOVE   W-AAA(4 , I)  TO     R2-171(N , I).
           ADD    W-AAA(4 , I)  TO     TOT.
           GO  TO  GP1-100.
       GP1-110.
           IF  SW1       =       4
               MOVE   1       TO    W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
           END-IF.
       GP1-120.
           MOVE   ZERO      TO   W-GC.
           MOVE   ZERO      TO   W-A.
           MOVE   ZERO      TO   SW1    N    O-SW1.
       GP1-EX.
           EXIT.
      ***************************************************
      *    ÇfÇoÇQÅ|ÇqÇsÇm                               *
      ***************************************************
       GP2-RTN.
           IF  CNT      =       0
               GO  TO  GP2-120
           END-IF
           MOVE     1      TO  G-SW.
           MOVE "çáÅ@Å@åv"  TO   W-KEI.
           MOVE  SPACE        TO     R2.
           MOVE     1      TO  J  N.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-030
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   1       TO     R2-16(N).
           MOVE   0       TO     I.
       GP2-010.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-020
           END-IF
           MOVE   W-BBB(1 , I)  TO     R2-171(N , I).
           ADD    W-BBB(1 , I)  TO     TOT.
           GO  TO  GP2-010.
       GP2-020.
           IF  SW2       =       1
               MOVE   2        TO     W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP2-120
           END-IF.
       GP2-030.
           MOVE     2      TO  J.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-060
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   2       TO     R2-16(N).
           MOVE   0       TO     I.
       GP2-040.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-050
           END-IF
           MOVE   W-BBB(2 , I)  TO     R2-171(N , I).
           ADD    W-BBB(2 , I)  TO     TOT.
           GO  TO  GP2-040.
       GP2-050.
           IF  SW2       =       2
               MOVE   2        TO     W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP2-120
           END-IF.
       GP2-060.
           MOVE     3      TO  J.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-090
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   3       TO     R2-16(N).
           MOVE   0       TO     I.
       GP2-070.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-080
           END-IF
           MOVE   W-BBB(3 , I)  TO     R2-171(N , I).
           ADD    W-BBB(3 , I)  TO     TOT.
           GO  TO  GP2-070.
       GP2-080.
           IF  SW2       =       3
               MOVE   2        TO     W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
               GO  TO    GP2-120
           END-IF.
       GP2-090.
           MOVE     4      TO  J.
           PERFORM  MZB-RTN  THRU  MZB-EX.
           IF  ASW    =   1
               GO  TO  GP2-120
           END-IF
           MOVE   W-KEI   TO     R2-KEI(N).
           MOVE   SPACE   TO     W-KEI.
           MOVE   4       TO     R2-16(N).
           MOVE   0       TO     I.
       GP2-100.
           ADD    1       TO     I.
           IF  I       >      10
               ADD  1  TO    N
               GO  TO  GP2-110
           END-IF
           MOVE   W-BBB(4 , I)  TO     R2-171(N , I).
           ADD    W-BBB(4 , I)  TO     TOT.
           GO  TO  GP2-100.
       GP2-110.
           IF  SW2       =       4
               MOVE   2        TO     W-GC
               PERFORM   TOT-RTN   THRU     TOT-EX
           END-IF.
       GP2-120.
           MOVE   ZERO      TO   W-B.
           MOVE   ZERO      TO   SW2    N    O-SW2.
           MOVE     0      TO  G-SW.
           MOVE   ZERO      TO   W-GC   W-DCC.
       GP2-EX.
           EXIT.
      *****************************
      *      ÇlÇyÇ`Å@Å|ÇqÇsÇm     Åñ
      *****************************
       MZA-RTN.
           MOVE    0    TO    I    ASW.
       MZA-001.
           ADD     1    TO    I.
           IF  I    >    10
               MOVE    1   TO   ASW
               GO  TO  MZA-EX
           END-IF
           IF  W-AAA(J , I)  NOT   =    0
               GO  TO  MZA-EX
           END-IF
           GO  TO  MZA-001.
       MZA-EX.
           EXIT.
      *****************************
      *      ÇlÇyÇaÅ@Å|ÇqÇsÇm     Åñ
      *****************************
       MZB-RTN.
           MOVE    0    TO    I    ASW.
       MZB-001.
           ADD     1    TO    I.
           IF  I    >    10
               MOVE    1   TO   ASW
               GO  TO  MZB-EX
           END-IF
           IF  W-BBB(J , I)  NOT   =    0
               GO  TO  MZB-EX
           END-IF
           GO  TO  MZB-001.
       MZB-EX.
           EXIT.
       COPY    LPMSG.
