       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             JT323L.
      *********************************************
      *    PROGRAM NAME  :  品名倉別受払明細表　  *
      *    DATA WRITTEN  :  00/11/21              *
      *    SCREEN  USED  :  ***                   *
      *    COMPILE TYPE  :  COBOL                 *
      *********************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SOURCE-COMPUTER.        SYSTEM100.
       OBJECT-COMPUTER.        SYSTEM100.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
       77  ERR-STAT            PIC  X(02)   VALUE  SPACE.
       77  PCNT                PIC  9(03)   VALUE   0.
       77  W-OK                PIC  9.
       77  CHK                 PIC  9.
       77  WK0128ID            PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1         PIC  X(003).
           02  STN-NO2         PIC  X(003).
       01  W-FID.
           02  W-FID1          PIC  X(006) VALUE "WK0128".
           02  W-FID2          PIC  X(003).
       01  W-DATA.
           02  W-HCD           PIC  9(06).
           02  W-KURA          PIC  9(01).
           02  W-KBN           PIC  9(01).
           02  W-C1            PIC  9(01).
           02  W-C2            PIC  9(01).
           02  W-C3            PIC  9(01).
           02  W-PC            PIC  9(01).
           02  CNT2            PIC  9(02).
           02  W-SUT           PIC S9(07).
           02  W-KURAN         PIC  N(06).
           02  W-KBNN          PIC  N(06).
       01  W-P1.
           02  P-HCD           PIC  9(06).
           02  F               PIC  X(01).
           02  P-NAME          PIC  N(24).
           02  F               PIC  X(93).
       01  W-P2.
           02  F               PIC  X(26).
           02  P-KURAN         PIC  N(06).
           02  F               PIC  X(02).
           02  P-KBNN          PIC  N(06).
           02  P-SIZ           PIC  9.
           02  P-ASUD.
             03  P-SUD   OCCURS  10.
               04  P-SU        PIC  ----,---.
           02  P-SUT           PIC  -----,--9.
       01  HEAD1.
           02  W-20K           PIC  X(05)   VALUE  X"1A24212474".
           02  F               PIC  X(44)   VALUE  SPACE.
           02  H-MID           PIC  N(18).
           02  F               PIC  X(33)   VALUE  SPACE.
           02  F               PIC  X(05)   VALUE  "DATE ".
           02  H-DATE          PIC  99/99/99.
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  X(02)   VALUE  "P.".
           02  H-PAGE          PIC ZZ9.
       01  HEAD2.
           02  W-15K           PIC  X(05)   VALUE  X"1A24212078".
           02  F               PIC  X(07)   VALUE  " ｺｰﾄﾞ  ".
           02  F               PIC  N(08)   VALUE  "品　　　　　名　".
           02  F               PIC  X(27)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "1".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "３号".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "２号".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "１号".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "０号".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "　中".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "　大".
           02  F               PIC  X(05)   VALUE  SPACE.
           02  F               PIC  N(02)   VALUE  "特大".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "28.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "29.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "30.0".
           02  F               PIC  X(09)   VALUE  SPACE.
       01  HEAD3.
           02  F               PIC  X(46)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "2".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "12.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "13.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "13.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "14.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "15.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "16.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "17.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "18.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "19.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "20.0".
           02  F               PIC  X(09)   VALUE  SPACE.
       01  HEAD4.
           02  F               PIC  X(46)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "3".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "21.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "21.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "22.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "22.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "23.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "23.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "25.0".
           02  F               PIC  X(17)   VALUE  SPACE.
       01  HEAD5.
           02  F               PIC  X(46)   VALUE  SPACE.
           02  F               PIC  X(01)   VALUE  "4".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "24.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "25.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "25.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "26.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "26.5".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "27.0".
           02  F               PIC  X(04)   VALUE  SPACE.
           02  F               PIC  X(04)   VALUE  "27.5".
           02  F               PIC  X(19)   VALUE  SPACE.
           02  F               PIC  N(04)   VALUE  "　合　計".
      **
           COPY       L-JCON.
           COPY       LIHIM2.
      *FD  JT-YZAI
       01  JT-YZAI_JT323L.
           02  JT-YZAI_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  JT-YZAI_LNAME        PIC  X(014) VALUE "JT-YZAI_JT323L".
           02  F                    PIC  X(001).
           02  JT-YZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_SORT         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_RES          USAGE  POINTER.
       01  YZAI-R.
           02   YZAI-01             PIC 9(1).
           02   YZAI-02             PIC 9(6).
           02   YZAI-03             PIC 9(1).
           02   YZAI-04.
                03  YZAI-041     OCCURS  10.
                    04  YZAI-0411   PIC S9(6).
           02   FILLER              PIC X(11).
           02   YZAI-88             PIC 9(01).
           02   YZAI-89             PIC 9(01).
           02   YZAI-90             PIC 9(01).
           02   YZAI-91             PIC 9(02).
           02   YZAI-92             PIC 9(01).
           02   YZAI-93             PIC 9(01).
           02   FILLER              PIC X(42).
       77  F                        PIC X(01).
      *FD  P-F
       77  P-R                      PIC X(170).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  DSP-AREA.
           02  DSP-CLR   PIC  X(12) VALUE "CLEAR SCREEN".
           02  DSP-01    PIC  X(22)
               VALUE   "                      ".
           02  DSP-02    PIC  N(10)
                       VALUE   "品名倉別　在庫受払表".
           02  DSP-09    PIC  N(03)
                       VALUE   "確認（".
           02  DSP-10    PIC  X(11)
                       VALUE   "OK=1,NO=9）".
           02  DSP-11    PIC  X(08)
                       VALUE   "--> ﾘﾀｰﾝ".
       01  ACT-AREA.
           02  ACT-OK    PIC  9 .
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "79" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-CLR" "X" "1" "0" "12" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" "RX" "1" "23" "22" "DSP-CLR" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" "N" "1" "24" "20" "DSP-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-09" "N" "23" "41" "6" "DSP-02" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-10" "X" "23" "47" "11" "DSP-09" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-11" "X" "23" "58" "8" "DSP-10" " "  RETURNING RESU.
      *ACT-AREA
       CALL "SD_Init" USING 
            "ACT-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACT-OK" "9" "23" "61" "1" " " "ACT-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACT-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       MR010.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       MR020.
           CALL "SD_Accept" USING BY REFERENCE ACT-OK "ACT-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT    NOT = "01" AND "06"
               GO TO    MR020
           END-IF
           IF  W-OK     =  9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  MR990
           END-IF
           IF  W-OK  NOT  =  1
               GO  TO   MR020
           END-IF
      *
           CALL    "CBLSTNNO"   USING STN-NO USER_ID.
           MOVE     STN-NO2     TO    W-FID2.
           MOVE     W-FID       TO    WK0128ID.
           MOVE WK0128ID TO JT-YZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-YZAI_PNAME1 " " BY REFERENCE JT-YZAI_IDLST "0".
       MR025.
      *           READ  JT-YZAI        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-YZAI_PNAME1 BY REFERENCE YZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1
               GO  TO  MR990
           END-IF
           IF  ZERO        =  YZAI-0411(01)  AND  YZAI-0411(02)  AND
                              YZAI-0411(03)  AND  YZAI-0411(04)  AND
                              YZAI-0411(05)  AND  YZAI-0411(06)  AND
                              YZAI-0411(07)  AND  YZAI-0411(08)  AND
                              YZAI-0411(09)  AND  YZAI-0411(10)
               GO  TO  MR025
           END-IF
           MOVE  YZAI-88   TO  CHK.
           IF  YZAI-88     =  0
               MOVE "＊＊＊　　実在庫　受払表　　＊＊＊　" TO H-MID
           ELSE
               MOVE "＊＊＊　　有効在庫　受払表　　＊＊＊" TO H-MID
           END-IF
      *
           ACCEPT      H-DATE   FROM  DATE.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  CHK        =  0
               CALL "DB_F_Open" USING
                "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
                "JCON3-KEY" BY REFERENCE JCON3-KEY
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM  HED-010     THRU  HED-EX.
       MR030.
           MOVE     YZAI-02     TO    W-HCD.
           MOVE     ZERO        TO    W-C1.
           MOVE     W-HCD       TO    HI-MHCD HI-HCD.
      *           READ     HI2-M       UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  HI-NAME
           END-IF
           MOVE     SPACE       TO    W-P1.
           MOVE     W-HCD       TO    P-HCD.
           MOVE     HI-NAME     TO    P-NAME.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER   >  59
               PERFORM  HED-RTN    THRU  HED-EX
           END-IF
           MOVE     SPACE       TO    P-R.
           MOVE     W-P1        TO    P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE     SPACE       TO    P-R.
           IF  CHK        =  1
               GO  TO  MR050
           END-IF.
       MR040.
           MOVE     YZAI-01     TO    W-KURA.
           MOVE     ZERO        TO    W-C2.
           MOVE     3           TO    JCON3-01.
           MOVE     W-KURA      TO    JCON3-02.
      *           READ        JCON     UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE  TO  JCON3-03
           END-IF
           MOVE     JCON3-03    TO    W-KURAN.
           IF  W-KURA     =  9
               MOVE  "全　　社　　"    TO  W-KURAN
           END-IF.
       MR050.
           MOVE     YZAI-90     TO    W-KBN.
           MOVE     ZERO        TO    W-SUT   W-C3.
           MOVE     SPACE       TO    W-KBNN.
           IF  W-KBN      =  0
               MOVE  "（前月残）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  1
               MOVE  "（入庫数）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  2
               MOVE  "（出庫数）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  3
               MOVE  "（指図数）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  4
               MOVE  "（実在庫）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  5
               MOVE  "（入予定）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  6
               MOVE  "（預り数）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  7
               MOVE  "（取よけ）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  8
               MOVE  "（有効数）　"    TO  W-KBNN
           END-IF
           IF  W-KBN      =  9
               MOVE  "（受注数）　"    TO  W-KBNN
           END-IF.
       MR060.
           MOVE    SPACE     TO   W-P2.
           MOVE    SPACE     TO   P-KURAN P-KBNN.
           IF  W-C2       =  0
               IF  CHK        =  0
                   MOVE    1         TO   W-C2
                   MOVE    W-KURAN   TO   P-KURAN
               END-IF
           END-IF
           IF  W-C3       =  0
               MOVE    1         TO   W-C3
               MOVE    W-KBNN    TO   P-KBNN
           END-IF
           MOVE  YZAI-03        TO  P-SIZ.
           MOVE  ZERO           TO  CNT2.
       MR070.
           ADD   1              TO  CNT2.
           IF  CNT2       <  11
               MOVE  YZAI-0411(CNT2)  TO  P-SU(CNT2)
               ADD   YZAI-0411(CNT2)  TO  W-SUT
               GO  TO  MR070
           END-IF.
       MR080.
      *           READ  JT-YZAI        AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-YZAI_PNAME1 BY REFERENCE YZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR090
           END-IF
           IF  ZERO        =  YZAI-0411(01)  AND  YZAI-0411(02)  AND
                              YZAI-0411(03)  AND  YZAI-0411(04)  AND
                              YZAI-0411(05)  AND  YZAI-0411(06)  AND
                              YZAI-0411(07)  AND  YZAI-0411(08)  AND
                              YZAI-0411(09)  AND  YZAI-0411(10)
               GO  TO  MR080
           END-IF
           IF (YZAI-02   NOT =  W-HCD)  OR  (YZAI-90   NOT  =  W-KBN)
                   OR  ((CHK   =  0)  AND  (YZAI-01   NOT  =  W-KURA))
               MOVE    W-SUT     TO   P-SUT
           END-IF
           PERFORM  PRN-RTN    THRU  PRN-EX.
           IF  YZAI-02     NOT =  W-HCD
               GO  TO  MR030
           END-IF
           IF  CHK             =  0
               IF  YZAI-01     NOT =  W-KURA
                   GO  TO  MR040
               END-IF
           END-IF
           IF  YZAI-90     NOT =  W-KBN
               GO  TO  MR050
           END-IF
           GO  TO  MR060.
       MR090.
           MOVE    W-SUT     TO   P-SUT.
           PERFORM  PRN-RTN    THRU  PRN-EX.
       MR900.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  CHK        =  0
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
           END-IF.
       MR990.
           CALL "DB_Close".
           STOP       RUN.
      **
      ***********************************
      *     ＳＵＢ　　ＲＯＵＴＩＮＥ    *
      ***********************************
      **
      *------------------*
      *    印字　処理    *
      *------------------*
       PRN-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER   >  60
               PERFORM  HED-RTN    THRU  HED-EX
               MOVE     SPACE       TO    P-R
               MOVE     W-P1        TO    P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               MOVE     SPACE       TO    P-R
           END-IF
           MOVE     SPACE       TO    P-R.
           MOVE     W-P2        TO    P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE     SPACE       TO    P-R.
       PRN-EX.
           EXIT.
      **
       HED-RTN.
           MOVE   SPACE  TO  P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       HED-010.
           ADD   1    TO    PCNT.
           MOVE  PCNT           TO   H-PAGE.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD1          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD2          TO   P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD3          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD4          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
           MOVE  HEAD5          TO   P-R.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE          TO   P-R.
       HED-EX.
           EXIT.
