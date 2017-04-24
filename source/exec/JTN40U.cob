       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT100U.
       AUTHOR.                        E-SHIGIHARA.
      ***************************************************
      *    PROGRAM        : ìæà”êÊï èoâ◊ÉèÅ[ÉNçÏê¨      *
      *    DATA WRITTEN   : 87/08/04                    *
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
       01  W-DATE                    PIC 9(08).
       01  W-MID                     PIC N(05).
       77  W-16                      PIC 9(01)    VALUE 0.
       COPY    LWMSG.
      *
           COPY  LJSTRR.
           COPY  L-JCON.
           COPY  LJWTOK.
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
               03  FILLER  PIC  X(24) VALUE  "                        ".
               03  FILLER  PIC  X(02) VALUE  "èo".
               03  FILLER  PIC  X(02) VALUE  "â◊".
               03  FILLER  PIC  X(02) VALUE  "ì˙".
               03  FILLER  PIC  X(02) VALUE  "ïÒ".
               03  FILLER  PIC  X(01) VALUE  "(".
               03  FILLER  PIC  X(08) VALUE  "ìæà”êÊï ".
               03  FILLER  PIC  X(01) VALUE  ")".
           02  DSP-03.
               03  FILLER  PIC N(05).
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
            "DSP-AREA" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "42" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "20" "24" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "21" "2" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-01" "X" "1" "24" "2" "02DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-01" "X" "1" "27" "2" "03DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-01" "X" "1" "30" "2" "04DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-01" "X" "1" "33" "1" "05DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-01" "X" "1" "34" "8" "06DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-01" "X" "1" "42" "1" "07DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "1" "0" "10" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "N" "1" "1" "10" " " "DSP-03" RETURNING RESU.
       CALL "SD_From" USING 
            "01DSP-03" BY REFERENCE W-MID "10" "0" RETURNING RESU.
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
      *
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON6-KEY" BY REFERENCE JCON6-KEY.
           MOVE   SPACE   TO    JCON6-KEY.
           MOVE   6       TO    JCON6-01.
      *           READ   JCON    UNLOCK  INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE    "JCON"     TO  ERR-F
               MOVE    "I"        TO  ERR-M
               PERFORM    ERR-RTN    THRU    ERR-EX
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU
               CALL "DB_Close"
               STOP    RUN
           END-IF
           MOVE   JCON6-08      TO  IPN-KYO-KBN.
           MOVE   JCON6-09      TO  W-DATE.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
      *
           IF  IPN-KYO-KBN  =  0
               MOVE  "Åkã≥Å@àÁÅl"   TO  W-MID
           END-IF
           IF  IPN-KYO-KBN  =  1
               MOVE  "ÅkàÍÅ@î Ål"   TO  W-MID
           END-IF
           CALL "SD_Output" USING "DSP-03" DSP-03 "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JWTOK_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTRRF_PNAME1 "SHARED" BY REFERENCE
            JSTRRF_IDLST "0".
           CALL "DB_F_Open" USING
            "OUTPUT" JWTOK_PNAME1 "EXCLUSIVE" BY REFERENCE
            JWTOK_IDLST "0".
       MR020.
      *           READ    JSTRRF              AT   END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JSTRRF_PNAME1 BY REFERENCE JSTRR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           IF   (JSTRR-1211(1)   =   0) AND (JSTRR-1211(2)  =  0) AND
                (JSTRR-1211(3)   =   0) AND (JSTRR-1211(4)  =  0) AND
                (JSTRR-1211(5)   =   0) AND (JSTRR-1211(6)  =  0) AND
                (JSTRR-1211(7)   =   0) AND (JSTRR-1211(8)  =  0) AND
                (JSTRR-1211(9)   =   0) AND (JSTRR-1211(10) =  0)
               GO  TO  MR020
           END-IF
           MOVE  JSTRR-16         TO  W-16.
           IF  W-16               =   2
               MOVE  1      TO  W-16
           END-IF
           IF  IPN-KYO-KBN   NOT  =   W-16
               GO  TO  MR020
           END-IF
           IF  JSTRR-90           <  W-DATE
               GO  TO  MR020
           END-IF
           IF  JSTRR-90           >  W-DATE
               GO  TO  MR999
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
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING "DSP-01" DSP-01 "p" RETURNING RESU.
       INT-EX.
           EXIT.
      *********************************************
      *    ÇdÇmÇcÅ|ÇqÇsÇm                         *
      *********************************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JSTRRF_IDLST JSTRRF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JWTOK_IDLST JWTOK_PNAME1.
       END-EX.
           EXIT.
      **********************************************
      *    ÇvÇqÇsÅ|ÇqÇsÇm                          *
      **********************************************
       WRT-RTN.
           MOVE SPACE  TO  JWTOK-R.
           INITIALIZE      JWTOK-R.
           MOVE   JSTRR-06  TO  JWTOK-01.
           MOVE   JSTRR-03  TO  JWTOK-02.
           MOVE   JSTRR-KEY TO  JWTOK-03.
           MOVE   JSTRR-13  TO  JWTOK-04.
           MOVE   JSTRR-08  TO  JWTOK-05.
           MOVE   JSTRR-09  TO  JWTOK-06.
           MOVE   JSTRR-10  TO  JWTOK-07.
           MOVE   JSTRR-1211(01) TO  JWTOK-0811(01).
           MOVE   JSTRR-1211(02) TO  JWTOK-0811(02).
           MOVE   JSTRR-1211(03) TO  JWTOK-0811(03).
           MOVE   JSTRR-1211(04) TO  JWTOK-0811(04).
           MOVE   JSTRR-1211(05) TO  JWTOK-0811(05).
           MOVE   JSTRR-1211(06) TO  JWTOK-0811(06).
           MOVE   JSTRR-1211(07) TO  JWTOK-0811(07).
           MOVE   JSTRR-1211(08) TO  JWTOK-0811(08).
           MOVE   JSTRR-1211(09) TO  JWTOK-0811(09).
           MOVE   JSTRR-1211(10) TO  JWTOK-0811(10).
           MOVE   JSTRR-122 TO  JWTOK-082.
           MOVE   JSTRR-05S TO  JWTOK-09.
           MOVE   JSTRR-14D TO  JWTOK-10.
           MOVE   JSTRR-15  TO  JWTOK-11.
           MOVE   ZERO      TO  JWTOK-12.
           MOVE   JSTRR-14  TO  JWTOK-13.
           MOVE   W-16      TO  JWTOK-14.
           MOVE   JSTRR-14A TO  JWTOK-15.
           MOVE   JSTRR-15A TO  JWTOK-16.
           MOVE   JSTRR-14B TO  JWTOK-17.
           MOVE   JSTRR-20  TO  JWTOK-20.
           MOVE   IPN-KYO-KBN TO JWTOK-JS.
      *           WRITE  JWTOK-R.
      *//////////////
           CALL "DB_Insert" USING
            JWTOK_PNAME1 JWTOK_LNAME JWTOK-R RETURNING RET.
           IF  ERR-STAT   NOT   =   "00"
               MOVE    "WK0256"   TO  ERR-F
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
           CALL "SD_Output" USING
            "ERR-DIS" ERR-DIS "p" RETURNING RESU
           CALL "SD_Output" USING
            "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
       ERR-EX.
           EXIT.
      *
