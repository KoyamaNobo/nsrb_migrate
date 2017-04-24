       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         JT240R.
      ******************************************************************
      *    <<REMARKS>>                                                 *
      *    FUNCTION.......  ìæà”êÊï éÛíçécñ‚çáÇπ                       *
      *    AUTHOR.........  Y.KATOH                                    *
      *    COMPILE MODE...  NORMAL                                     *
      *    SCREEN.........  SJ240R                                     *
      *    RELEASE DATE...  62/08/31         (REV.001)                 *
      ******************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        NEAC-SYSTEM100.
       OBJECT-COMPUTER.        NEAC-SYSTEM100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT             PIC  X(02).
       01  GMN-KAK              PIC  9(01).
       01  WK-TCD               PIC  9(04).
       01  NEXT-AREA.
           02  NEXT-TCD         PIC  9(04).
           02  NEXT-TNAM        PIC  N(24).
           02  NEXT-JUNO        PIC  9(06).
       01  WK-AREA.
           02  GMN-AREA.
               03  W-AZKBN      PIC  9(01)   OCCURS 13.
               03  GMN-TCD      PIC  9(04).
               03  GMN-TNAM     PIC  N(24).
               03  GMN-HNAM     PIC  N(24)   OCCURS 13.
               03  GMN-CCD      PIC  9(04)   OCCURS 13.
               03  GMN-ZAN      PIC S9(06).
               03  GMN-DATE.
                 04  GMN-YY     PIC  9(02).
                 04  GMN-MM     PIC  9(02).
                 04  GMN-DD     PIC  9(02).
               03  GMN-JUNO     PIC  9(06).
               03  GMN-GYNO     PIC  9(01).
               03  GMN-KBN      PIC  N(01).
               03  GMN-LN       PIC  9(02).
               03  GMN-DATA     PIC S9(04).
           02  DSP-AREA1.
               03  WK-DATA      OCCURS 13.
                 04  WK-DATA-R  OCCURS  4.
                   05  WK-DT-A  PIC S9(06)   OCCURS 10.
                   05  WK-DT-B  PIC S9(06)   OCCURS 10.
                   05  WK-DT-C  PIC S9(06)   OCCURS 10.
                   05  WK-DT-D  PIC S9(06)   OCCURS 10.
               03  WG-DT-A      PIC S9(06).
               03  WG-DT-B      PIC S9(06).
               03  WG-DT-C      PIC S9(06).
               03  WG-DT-D      PIC S9(06).
           02  DSP-AREA2.
               03  WK-NAM       PIC  N(24).
               03  WK-NAM-R     REDEFINES    WK-NAM.
                 04  WK-NAM1    PIC  N(12).
                 04  WK-NAM2    PIC  N(12).
               03  WK-CNAM      PIC  N(24).
               03  WK-CNAM-R    REDEFINES    WK-CNAM.
                 04  WK-CNAM1   PIC  N(12).
                 04  WK-CNAM2   PIC  N(12).
               03  SW-END       PIC  X(02).
               03  NEW-KEY.
                 04  N-HNO      PIC  9(06).
                 04  N-JUNO     PIC  9(06).
                 04  N-GYNO     PIC  9(01).
                 04  N-AZKBN    PIC  9(01).
               03  OLD-KEY.
                 04  O-HNO      PIC  9(06).
                 04  O-JUNO     PIC  9(06).
                 04  O-GYNO     PIC  9(01).
                 04  O-AZKBN    PIC  9(01).
               03  WK-CODE      PIC  9(07).
               03  WK-CODE-R    REDEFINES      WK-CODE.
                 04  WK-CD1     PIC  9(04).
                 04  WK-CD2     PIC  9(03).
               03  AA           PIC  9(02).
               03  BB           PIC  9(02).
               03  ZZ           PIC  9(02).
               03  II           PIC  9(02).
               03  WW           PIC  9(02).
      ***************************************
      *    ¥◊∞ DISPLAY (‹∞∏)                *
      ***************************************
       01  DISP-ERR-WORK.
           02  DISP-MSG.
               03  ERR-MSGX.
                   04  ERR-MSGN     PIC N(20).
               03  ERR-SPACE        PIC X(40).
               03  ERR-SPACES       PIC X(40).
               03  ERR-F            PIC X(12).
               03  ERR-M            PIC X(01).
               03  ERR-K            PIC X(30).
               03  ERR-FLG          PIC X(02).
      *
           COPY  LJMST2.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LWTNAF.
      *
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  ACT-CLEAR.
           02  CLR-GMN  PIC  X(12) VALUE  "CLEAR SCREEN".
       01  MST-ERR.
           02  FILLER   PIC  X(20) VALUE  "éÛíçÉ}ÉXÉ^Å[Å@ñ¢ìoò^".
       01  MST-ERR1.
           02  FILLER   PIC  X(18) VALUE  "ÇvÇsÇmÇ`ÇeÅ@ñ¢ìoò^".
       01  ACT-ACTION.
           02  ACT-TCD     PIC  9(04).
           02  ACT-LINE    PIC  9(02).
           02  ACT-KAK     PIC  9(01).
       01  ACT-HEAD.
           02  FILLER   PIC  9(04).
           02  FILLER   PIC  N(24).
       01  ACT-DATA1.
           02  ACT-HNAM    PIC  N(23).
           02  ACT-ZAN     PIC  ----,--9 .
           02  ACT-DATE.
               03  FILLER   PIC  9(02).
               03  FILLER   PIC  X(01)  VALUE "/".
               03  FILLER   PIC  ZZ .
               03  FILLER   PIC  X(01)  VALUE "/".
               03  FILLER   PIC  ZZ .
           02  ACT-JUNO.
               03  FILLER   PIC  9(06).
               03  FILLER   PIC  X(01)             VALUE "-".
               03  FILLER   PIC  9(01).
           02  ACT-KBN     PIC  N(01).
       01  ACT-DATA2.
           02  ACT-NAM.
               03  FILLER   PIC  N(12).
               03  FILLER   PIC  N(12).
           02  ACT-CNAM.
               03  FILLER   PIC  N(12).
           02  ACT-MS1.
               03  FILLER   PIC  ----- .
           02  ACT-MS2.
               03  FILLER   PIC  ------- .
       01  ACT-SPACE.
           02  CLR-HEAD.
               03  FILLER   PIC  X(53)  VALUE  " ".
           02  CLR-DATA.
               03  FILLER   PIC  X(52)  VALUE  " ".
               03  FILLER   PIC  X(52)  VALUE  " ".
               03  FILLER   PIC  X(52)  VALUE  " ".
               03  FILLER   PIC  X(52)  VALUE  " ".
           02  CLR-KAK   PIC  X(01)  VALUE  " ".
       01  DISP-MSG-SPACE1.
           03  FILLER   PIC X(40) VALUE " ".
       COPY  LSMSG.
       PROCEDURE           DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *ACT-CLEAR
       CALL "SD_Init" USING
           "ACT-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "CLR-GMN" "X" "1" "0" "12" " " "ACT-CLEAR" RETURNING RESU.
      *MST-ERR
       CALL "SD_Init" USING 
            "MST-ERR" " " "0" "0" "20" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01MST-ERR" "X" "24" "1" "20" " " "MST-ERR" RETURNING RESU.
      *MST-ERR1
       CALL "SD_Init" USING 
            "MST-ERR1" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01MST-ERR1" "X" "24" "1" "18" " " "MST-ERR1" RETURNING RESU.
      *ACT-ACTION
       CALL "SD_Init" USING 
            "ACT-ACTION" " " "0" "0" "7" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-TCD" "9" "1" "28" "4" " " "ACT-ACTION" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-TCD" BY REFERENCE GMN-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-LINE" "9" "20" "1" "2" "ACT-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-LINE" BY REFERENCE GMN-LN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-KAK" "9" "24" "71" "1" "ACT-LINE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-KAK" BY REFERENCE GMN-KAK "1" "0" RETURNING RESU.
      *ACT-HEAD
       CALL "SD_Init" USING 
            "ACT-HEAD" " " "0" "0" "52" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACT-HEAD" "9" "1" "28" "4" " " "ACT-HEAD" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-HEAD" BY REFERENCE GMN-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACT-HEAD" "N" "1" "33" "48" "01ACT-HEAD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02ACT-HEAD" BY REFERENCE GMN-TNAM "48" "0" RETURNING RESU.
      *ACT-DATA1
       CALL "SD_Init" USING 
            "ACT-DATA1" " " "0" "0" "72" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-HNAM" "N" "AA" "1" "46" " " "ACT-DATA1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "ACT-HNAM" BY REFERENCE GMN-HNAM(1) "48" "1" BY REFERENCE 
            II 48 RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-ZAN" "----,--9" "AA" "49" "8" "ACT-HNAM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "ACT-ZAN" BY REFERENCE GMN-ZAN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-DATE" " " "0" "0" "8" "ACT-ZAN" " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACT-DATE" "9" "AA" "58" "2" " " "ACT-DATE" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-DATE" BY REFERENCE GMN-YY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACT-DATE" "X" "AA" "60" "1" "01ACT-DATE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACT-DATE" "ZZ" "AA" "61" "2" "02ACT-DATE" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ACT-DATE" BY REFERENCE GMN-MM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ACT-DATE" "X" "AA" "63" "1" "03ACT-DATE" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05ACT-DATE" "ZZ" "AA" "64" "2" "04ACT-DATE" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "05ACT-DATE" BY REFERENCE GMN-DD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-JUNO" " " "AA" "0" "8" "ACT-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACT-JUNO" "9" "AA" "67" "6" " " "ACT-JUNO" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-JUNO" BY REFERENCE GMN-JUNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACT-JUNO" "X" "AA" "73" "1" "01ACT-JUNO" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACT-JUNO" "9" "AA" "74" "1" "02ACT-JUNO" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03ACT-JUNO" BY REFERENCE GMN-GYNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-KBN" "N" "AA" "76" "2" "ACT-JUNO" " " RETURNING RESU.
       CALL "SD_From" USING 
            "ACT-KBN" BY REFERENCE GMN-KBN "2" "0" RETURNING RESU.
      *ACT-DATA2
       CALL "SD_Init" USING 
            "ACT-DATA2" " " "0" "0" "84" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-NAM" " " "0" "0" "48" " " "ACT-DATA2" RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACT-NAM" "N" "21" "3" "24" " " "ACT-NAM" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-NAM" BY REFERENCE WK-NAM1 "24" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "02ACT-NAM" "N" "22" "3" "24" "01ACT-NAM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02ACT-NAM" BY REFERENCE WK-NAM2 "24" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-CNAM" " " "0" "0" "24" "ACT-NAM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACT-CNAM" "N" "23" "3" "24" " " "ACT-CNAM" RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-CNAM" BY REFERENCE WK-CNAM1 "24" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-MS1" " " "0" "0" "5" "ACT-CNAM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACT-MS1" "-----" "AA" "BB" "5" " " "ACT-MS1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-MS1" BY REFERENCE GMN-DATA "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-MS2" " " "0" "0" "7" "ACT-MS1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACT-MS2" "-------" "AA" "74" "7" " " "ACT-MS2"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01ACT-MS2" BY REFERENCE GMN-DATA "4" "0" RETURNING RESU.
      *ACT-SPACE
       CALL "SD_Init" USING 
            "ACT-SPACE" " " "0" "0" "262" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-HEAD" " " "0" "0" "53" " " "ACT-SPACE" RETURNING RESU.
       CALL "SD_Init" USING 
           "01CLR-HEAD" "X" "1" "28" "53" " " "CLR-HEAD" RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-DATA" " " "0" "0" "208" "CLR-HEAD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-DATA" "X" "20" "29" "52" " " "CLR-DATA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-DATA" "X" "21" "29" "52" "01CLR-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-DATA" "X" "22" "29" "52" "02CLR-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-DATA" "X" "23" "29" "52" "03CLR-DATA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLR-KAK" "X" "24" "71" "1" "CLR-DATA" " " RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE1" "X" "24" "1" "40" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ******************************************************************
      *                    ÇlÇ`ÇhÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       MAIN-RTN.
           PERFORM   OPEN-RTN     THRU         OPEN-EX.
           PERFORM   DISP-RTN     THRU         DISP-EX.
           PERFORM   END-RTN      THRU         END-EX.
           CALL "DB_Close".
           STOP      RUN.
      ******************************************************************
      *                    ÇnÇoÇdÇmÅ|ÇqÇsÇm                            *
      ******************************************************************
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" JMST2_PNAME1 "SHARED" BY REFERENCE JMST2_IDLST "1"
            "JMST2-KEY" BY REFERENCE JMST2-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ240R" RETURNING RESU.
           INITIALIZE      WK-AREA  GMN-KAK  NEXT-AREA.
       OPEN-EX.
           EXIT.
      ******************************************************************
      *                    ÇcÇhÇrÇoÅ|ÇqÇsÇm                            *
      ******************************************************************
       DISP-RTN.
           MOVE      GMN-TCD    TO    WK-TCD.
       DISP-TCD.
           CALL "SD_Accept" USING BY REFERENCE ACT-TCD "ACT-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           MOVE      GMN-TCD   TO   TC-TCD.
           IF  ESTAT  =   "P9"
               GO  TO  DISP-EX
           END-IF
           IF  ESTAT  NOT =   "01"  AND  "06"
               GO  TO  DISP-TCD
           END-IF.
       DISP-TNAM.
           CALL "SD_Output" USING
            "CLR-HEAD" CLR-HEAD "p" RETURNING RESU.
           MOVE      001        TO    TC-CCD.
      *           READ      TC-M             UNLOCK       INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE   TO   TC-NAME
           END-IF
           MOVE      TC-NAME    TO    GMN-TNAM.
           MOVE      TC-TCD     TO    WK-TCD.
           CALL "SD_Output" USING
            "ACT-HEAD" ACT-HEAD "p" RETURNING RESU.
           MOVE      ZERO       TO    JMST2-KEY.
           MOVE      GMN-TCD    TO    JMST2-04.
      *           START     JMST2      KEY   NOT <    JMST2-KEY INVALID    KEY
      *///////////////
           CALL "DB_Start" USING
            JMST2_PNAME1 "JMST2-KEY" " NOT < " JMST2-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "MST-ERR" MST-ERR "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
               GO  TO  DISP-RTN
           END-IF.
           MOVE      3          TO    AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE      1          TO    II.
           MOVE    SPACE        TO    SW-END.
       DISP-READ.
      *           READ      JMST2    NEXT    UNLOCK       AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST2_PNAME1 BY REFERENCE JMST2-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     "01"    TO   SW-END
           END-IF
           IF  GMN-TCD    NOT =     JMST2-04
               MOVE     "01"    TO   SW-END
           END-IF
           IF  SW-END     =      "01"
               IF  II  =  1
                   IF  O-JUNO  = ZERO
                       CALL "SD_Output" USING
                        "MST-ERR" MST-ERR "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "DISP-BUZ-J" DISP-BUZ-J "p" RETURNING RESU
                       GO  TO  DISP-RTN
                   ELSE
                       MOVE    1    TO   WW
                       GO  TO  DISP-END
                   END-IF
               ELSE
                   MOVE     II      TO   WW
                   GO  TO  DISP-END
               END-IF
           END-IF
           MOVE      JMST2-05   TO    N-HNO.
           MOVE      JMST2-07   TO    N-JUNO.
           MOVE      JMST2-08   TO    N-GYNO.
           MOVE      JMST2-01   TO    N-AZKBN.
           IF  II  =    1   AND     O-JUNO    =  ZERO
               GO  TO  DISP-SYOKI
           END-IF
           IF  NEW-KEY  =   OLD-KEY AND  SW-END  =   SPACE
               MOVE     1   TO  ZZ
               GO  TO  DISP-ADD
           END-IF.
       DISP-END.
           IF  O-AZKBN           =     2  OR  ZERO  OR  5  OR  6
               COMPUTE   GMN-ZAN    =     WG-DT-A
                                    -     WG-DT-C
                                    -     WG-DT-B
           END-IF
           IF  O-AZKBN           =     4
               COMPUTE   GMN-ZAN    =     WG-DT-A
                                    -     WG-DT-D
           END-IF
           IF  GMN-ZAN  NOT =   ZERO
               CALL "SD_Output" USING
                "ACT-DATA1" ACT-DATA1 "p" RETURNING RESU
           END-IF
           IF  SW-END  =  "01"
               GO  TO  DISP-KAK
           END-IF
           INITIALIZE           WG-DT-A  WG-DT-B  WG-DT-C  WG-DT-D.
           IF  GMN-ZAN  =   ZERO
               GO  TO  DISP-SYOKI
           END-IF
           IF  II  =   13
               MOVE    13     TO    WW
               GO  TO  DISP-KAK
           END-IF
           ADD       1          TO    AA  II.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
       DISP-SYOKI.
           MOVE      JMST2-06S  TO    GMN-DATE.
           MOVE      JMST2-07   TO    GMN-JUNO.
           MOVE      JMST2-08   TO    GMN-GYNO.
           IF  JMST2-01   =   ZERO
               MOVE       SPACE     TO   GMN-KBN
           END-IF
           IF  JMST2-01   =   4
               MOVE       "óa"    TO   GMN-KBN
           END-IF
           IF  JMST2-01   =   5
               MOVE       "äm"    TO   GMN-KBN
           END-IF
           IF  JMST2-01   =   6
               MOVE       "éÊ"    TO   GMN-KBN
           END-IF
           MOVE      JMST2-05   TO    HI-MHCD HI-HCD.
      *           READ      HI2-M            UNLOCK       INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE     SPACE   TO   HI-NAME
           END-IF
           MOVE      HI-NAME    TO    GMN-HNAM(II).
           MOVE      JMST2-01   TO    W-AZKBN(II).
           MOVE      JMST2-10   TO    GMN-CCD(II).
           IF  JMST2-04      =   9850
               IF  JMST2-23  NOT =   ZERO
                   MOVE      JMST2-23   TO    GMN-CCD(II)
               END-IF
           END-IF
           MOVE      1          TO    ZZ.
       DISP-ADD.
           ADD       JMST2-1111(ZZ)   TO   WK-DT-A(II JMST2-09 ZZ)
                                           WG-DT-A.
           ADD       JMST2-1211(ZZ)   TO   WK-DT-B(II JMST2-09 ZZ)
                                           WG-DT-B.
           ADD       JMST2-141(ZZ)    TO   WK-DT-C(II JMST2-09 ZZ)
                                           WG-DT-C.
           ADD       JMST2-151(ZZ)    TO   WK-DT-D(II JMST2-09 ZZ)
                                           WG-DT-D.
           IF  ZZ  =   10
               GO  TO  DISP-SET
           END-IF
           ADD       1          TO    ZZ.
           GO   TO   DISP-ADD.
       DISP-SET.
           MOVE      NEW-KEY    TO    OLD-KEY.
           MOVE      N-AZKBN    TO    O-AZKBN.
           GO   TO   DISP-READ.
       DISP-KAK.
           CALL "SD_Accept" USING BY REFERENCE ACT-KAK "ACT-KAK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               MOVE   GMN-TCD    TO  NEXT-TCD
               MOVE   GMN-TNAM   TO  NEXT-TNAM
               INITIALIZE        WK-AREA
               CALL "SD_Screen_Output" USING "SJ240R" RETURNING RESU
               MOVE   NEXT-TCD   TO  GMN-TCD
               MOVE   NEXT-TNAM  TO  GMN-TNAM
               CALL "SD_Output" USING
                "ACT-HEAD" ACT-HEAD "p" RETURNING RESU
               GO  TO  DISP-RTN
           END-IF
           IF  ESTAT  NOT =   "01"   AND   "06"
               GO  TO  DISP-KAK
           END-IF
           IF  GMN-KAK NOT =  1  AND  2  AND  9
               GO  TO  DISP-KAK
           END-IF
           IF  GMN-KAK    =   9
               CALL "SD_Output" USING
                "CAN-01" CAN-01 "p" RETURNING RESU
           END-IF
           IF  GMN-KAK    =   1      AND   SW-END    =   "01"
               GO  TO  DISP-KAK
           END-IF
           IF  GMN-KAK    =   2
               IF  II  =  1   AND    SW-END   =  "01"
                     AND    O-JUNO  =  ZERO
                   GO  TO  DISP-KAK
               ELSE
                   GO  TO  DISP-MEISAI
               END-IF
           END-IF
           MOVE      GMN-TCD    TO    NEXT-TCD.
           MOVE      GMN-TNAM   TO    NEXT-TNAM.
           MOVE      N-JUNO     TO    NEXT-JUNO.
           INITIALIZE             WK-AREA.
           CALL "SD_Output" USING "CLR-GMN" CLR-GMN "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJ240R" RETURNING RESU.
           MOVE      3          TO    AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE      1          TO    II.
           IF  GMN-KAK    =   1
               MOVE   NEXT-TCD  TO  GMN-TCD
               MOVE   NEXT-TNAM TO  GMN-TNAM
               MOVE   NEXT-JUNO TO  N-JUNO
               CALL "SD_Output" USING
                "ACT-HEAD" ACT-HEAD "p" RETURNING RESU
               GO  TO  DISP-SYOKI
           END-IF
           GO    TO     DISP-RTN.
       DISP-MEISAI.
           CALL "SD_Accept" USING BY REFERENCE ACT-LINE "ACT-LINE"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  NOT =   "01"   AND   "06"
               GO  TO  DISP-MEISAI
           END-IF
           IF  GMN-LN     =   ZERO
               GO  TO  DISP-KAK
           END-IF
           IF  GMN-LN     >   13     OR    GMN-LN    >   WW
               GO  TO  DISP-MEISAI
           END-IF
           MOVE      SPACE      TO    WK-NAM.
           MOVE      SPACE      TO    WK-CNAM.
           CALL "SD_Output" USING "ACT-NAM" ACT-NAM "p" RETURNING RESU.
           CALL "SD_Output" USING
            "CLR-DATA" CLR-DATA "p" RETURNING RESU.
           CALL "SD_Output" USING
            "ACT-CNAM" ACT-CNAM "p" RETURNING RESU.
           MOVE      GMN-HNAM(GMN-LN) TO     WK-NAM.
           CALL "SD_Output" USING "ACT-NAM" ACT-NAM "p" RETURNING RESU.
      *
           MOVE      GMN-TCD          TO     TC-TCD.
           MOVE      GMN-CCD(GMN-LN)  TO     TC-CCD.
      *           READ      TC-M                    UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      SPACE         TO  TC-NAME
           END-IF
           MOVE      TC-NAME          TO     WK-CNAM.
           IF  GMN-TCD   NOT =   9850
               GO  TO  DISP-MEISAI1
           END-IF
           IF  GMN-CCD(GMN-LN) = ZERO
               GO  TO  DISP-MEISAI1
           END-IF
           MOVE      GMN-CCD(GMN-LN)  TO     WTNA-KEY.
      *           READ      WTNAF                   UNLOCK   INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE      SPACE         TO  WTNA-NAME
           END-IF
           MOVE      WTNA-NAME        TO     WK-CNAM.
       DISP-MEISAI1.
           CALL "SD_Output" USING
            "ACT-CNAM" ACT-CNAM "p" RETURNING RESU.
           MOVE      20         TO    AA.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE      29         TO    BB.
           CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU.
           MOVE      1          TO    II  ZZ.
       DISP-LOOP.
           IF  W-AZKBN(GMN-LN)      =     2  OR  ZERO  OR 5  OR  6
               COMPUTE   GMN-DATA   =     WK-DT-A(GMN-LN ZZ  II)
                                    -     WK-DT-C(GMN-LN ZZ  II)
                                    -     WK-DT-B(GMN-LN ZZ  II)
           END-IF
           IF  W-AZKBN(GMN-LN)      =     4
               COMPUTE   GMN-DATA   =     WK-DT-A(GMN-LN ZZ  II)
                                    -     WK-DT-D(GMN-LN ZZ  II)
           END-IF
           CALL "SD_Output" USING "ACT-MS1" ACT-MS1 "p" RETURNING RESU.
           ADD       1          TO    II.
           ADD       5          TO    BB.
           CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU.
           IF  II  =  10
               GO  TO  DISP-LOOP-E
           END-IF
           GO   TO   DISP-LOOP.
       DISP-LOOP-E.
           IF  W-AZKBN(GMN-LN)      =     2  OR  ZERO  OR  5  OR  6
               COMPUTE   GMN-DATA   =     WK-DT-A(GMN-LN ZZ  II)
                                    -     WK-DT-C(GMN-LN ZZ  II)
                                    -     WK-DT-B(GMN-LN ZZ  II)
           END-IF
           IF  W-AZKBN(GMN-LN)      =     4
               COMPUTE   GMN-DATA   =     WK-DT-A(GMN-LN ZZ  II)
                                    -     WK-DT-D(GMN-LN ZZ  II)
           END-IF
           CALL "SD_Output" USING "ACT-MS2" ACT-MS2 "p" RETURNING RESU.
           IF  ZZ  =   4
               GO  TO  DISP-MEISAI
           END-IF
           ADD       1          TO    AA  ZZ.
           CALL "SD_Arg_Match_Line" USING "AA" "2" AA RETURNING RESU.
           MOVE      29         TO    BB.
           CALL "SD_Arg_Match_Col" USING "BB" "2" BB RETURNING RESU.
           MOVE      1          TO    II.
           GO   TO   DISP-LOOP.
       DISP-EX.
           EXIT.
      ******************************************************************
      *                    ÇdÇmÇcÅ|ÇqÇsÇm                              *
      ******************************************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST2_IDLST JMST2_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
       END-EX.
           EXIT.
       COPY  LPMSG.
