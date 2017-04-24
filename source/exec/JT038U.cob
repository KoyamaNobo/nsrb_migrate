       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT038U.
       AUTHOR.                        IKUMI.N.
      ***************************************************
      *    PROGRAM        : èoâ◊éwê}ÉäÉXÉgê∂ê¨Å@Å@      *
      *    DATA WRITTEN   : 90/04/18                    *
      *    SCREEN USED    : UNUSED                      *
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
       77  ERR-SW                    PIC 9(01)    VALUE 0.
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  WK-SYMD.
           02  WK-SY                 PIC 9(02).
           02  WK-SM                 PIC 9(02).
           02  WK-SD                 PIC 9(02).
       01  KBN                       PIC N(03).
       COPY    LWMSG.
      *
           COPY  L-JSTR.
           COPY  LJWTOK.
           COPY  L-JCON.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC X(20)
                   VALUE  " èoâ◊éwê}ÉäÉXÉgê∂ê¨ ".
               03  FILLER  PIC X(02)  VALUE "Åi".
               03  DSP-011 PIC N(03).
               03  FILLER  PIC X(02)  VALUE "Åj".
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
            "DSP-AREA" " " "0" "0" "30" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "30" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "20" "20" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "41" "2" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-011" "N" "1" "43" "6" "02DSP-01" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-011" BY REFERENCE KBN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "X" "1" "49" "2" "DSP-011" " " RETURNING RESU.
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
       MR010.
           MOVE    SPACE  TO  JCON7-KEY.
           MOVE    7      TO  JCON7-01.
      *           READ    JCON   UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "JCON"       TO  ERR-F
               MOVE   "A"          TO  ERR-M
               MOVE    JCON1-KEY   TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
               GO  TO  MR999
           END-IF
           IF  IPN-KYO-KBN  =  0
               MOVE    JCON7-05 TO  WK-SYMD
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE    JCON7-07 TO  WK-SYMD
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
       MR020.
      *           READ    JSTR     NEXT       AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           IF  JSTR-03  NOT   =   0 AND 7
               GO  TO  MR020
           END-IF
           IF  JSTR-14        =   9
               GO  TO  MR020
           END-IF
           IF  JSTR-04S NOT   =   WK-SYMD
               GO  TO  MR020
           END-IF
           IF  JSTR-05  NOT   =   ZERO
               GO  TO  MR020
           END-IF
           IF  JSTR-17  NOT   =   9
               GO  TO  MR020
           END-IF
           IF  JSTR-16  NOT   =   IPN-KYO-KBN
               GO  TO  MR020
           END-IF
           IF  JSTR-4012       =   0
               GO  TO  MR020
           END-IF
           PERFORM     WRT-RTN    THRU   WRT-EX.
           IF  ERR-SW         =   1
               GO  TO  MR999
           END-IF
           GO  TO  MR020.
       MR999.
           PERFORM     END-RTN    THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************************
      *    ÇhÇmÇsÅ|ÇqÇsÇm                         *
      *********************************************
       INT-RTN.
           PERFORM  JS-ACP-RTN     THRU  JS-ACP-EX.
           IF  IPN-KYO-KBN  =  0
               MOVE  "ã≥Å@àÁ"    TO  KBN
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE  "ÉèÅ[ÉN"    TO  KBN
           END-IF
      *           DISPLAY        CLE-01      DSP-AREA   WITH   BYPASS   MODE.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JWTOK_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JWTOK_PNAME1 "EXCLUSIVE" BY REFERENCE
            JWTOK_IDLST "0".
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JWTOK_IDLST JWTOK_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       END-EX.
           EXIT.
      **********************************************
      *    ÇvÇqÇsÅ|ÇqÇsÇm                          *
      **********************************************
       WRT-RTN.
           MOVE SPACE  TO  JWTOK-R.
           INITIALIZE      JWTOK-R.
           MOVE   JSTR-06   TO  JWTOK-01.
           MOVE   JSTR-03   TO  JWTOK-02.
           MOVE   JSTR-KEY  TO  JWTOK-03.
           MOVE   JSTR-13   TO  JWTOK-04.
           MOVE   JSTR-08   TO  JWTOK-05.
           MOVE   JSTR-09   TO  JWTOK-06.
           MOVE   JSTR-10   TO  JWTOK-07.
           MOVE   JSTR-1111(01) TO  JWTOK-0811(01).
           MOVE   JSTR-1111(02) TO  JWTOK-0811(02).
           MOVE   JSTR-1111(03) TO  JWTOK-0811(03).
           MOVE   JSTR-1111(04) TO  JWTOK-0811(04).
           MOVE   JSTR-1111(05) TO  JWTOK-0811(05).
           MOVE   JSTR-1111(06) TO  JWTOK-0811(06).
           MOVE   JSTR-1111(07) TO  JWTOK-0811(07).
           MOVE   JSTR-1111(08) TO  JWTOK-0811(08).
           MOVE   JSTR-1111(09) TO  JWTOK-0811(09).
           MOVE   JSTR-1111(10) TO  JWTOK-0811(10).
           MOVE   JSTR-112  TO  JWTOK-082.
           MOVE   JSTR-04S  TO  JWTOK-09.
           MOVE   JSTR-14D  TO  JWTOK-10.
           MOVE   JSTR-15   TO  JWTOK-11.
           MOVE   JSTR-07   TO  JWTOK-12.
           MOVE   JSTR-14   TO  JWTOK-13.
           MOVE   JSTR-16   TO  JWTOK-14.
           MOVE   JSTR-14A  TO  JWTOK-15.
           MOVE   JSTR-15A  TO  JWTOK-16.
           MOVE   JSTR-14B  TO  JWTOK-17.
           MOVE   JSTR-20   TO  JWTOK-20.
           MOVE IPN-KYO-KBN TO  JWTOK-JS.
      *           WRITE  JWTOK-R.
      *//////////////
           CALL "DB_Insert" USING
            JWTOK_PNAME1 JWTOK_LNAME JWTOK-R RETURNING RET.
           IF  ERR-STAT   NOT   =   "00"
               MOVE    "JWTOK"    TO  ERR-F
               MOVE    "W"        TO  ERR-M
               PERFORM    ERR-RTN    THRU    ERR-EX
               MOVE    1          TO  ERR-SW
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  WRT-EX
           END-IF.
       WRT-EX.
      *****
      *****************************
      *    ¥◊∞ DISPLAY (“≤›)      *
      *****************************
       ERR-RTN.
           MOVE    ERR-STAT  TO  ERR-FLG.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-010.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-EX.
           EXIT.
       COPY  LPACPT.
