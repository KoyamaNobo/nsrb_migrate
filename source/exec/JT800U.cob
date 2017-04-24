       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT800U.
       AUTHOR.                        I.N.
      ***************************************************
      *    PROGRAM        : èoâ◊éwê}écÉäÉXÉgÅ@Å@Å@      *
      *    DATA WRITTEN   : 92/05/11                    *
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
       01  I                         PIC 9(02).
       01  W-OK                      PIC X(01).
       01  W-JS                      PIC 9(01).
       01  W-JSD                     PIC 9(01).
       COPY    LWMSG.
      *
           COPY  L-JSTR.
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
               03  FILLER  PIC  X(18) VALUE " èoâ◊éwê}écÉäÉXÉg ".
           02  DSP-JS.
               03  FILLER  PIC  X(25) VALUE "ã≥Å@àÁ=0 , àÍÅ@î =1 ...  ".
           02  DSP-02.
               03  FILLER  PIC  X(06) VALUE "ämîFÅi".
               03  FILLER  PIC  X(05) VALUE "OK=1,".
               03  FILLER  PIC  X(04) VALUE "èIóπ".
               03  FILLER  PIC  X(04) VALUE "=PF9".
               03  FILLER  PIC  X(02) VALUE "Åj".
               03  FILLER  PIC  X(08) VALUE "--> ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-JS      PIC 9(01).
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
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "18" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "20" "18" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-JS" " " "7" "0" "25" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-JS" "X" "7" "14" "25" " " "DSP-JS" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-02" " " "23" "0" "29" "DSP-JS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-02" "X" "23" "41" "6" " " "DSP-02" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-02" "X" "23" "47" "5" "01DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-02" "X" "23" "52" "4" "02DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-02" "X" "23" "56" "4" "03DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-02" "X" "23" "60" "2" "04DSP-02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-02" "X" "23" "62" "8" "05DSP-02" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JS" "9" "7" "38" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OK" "9" "23" "65" "1" "ACP-JS" " " RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OK" BY REFERENCE W-OK "1" "0" RETURNING RESU.
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
           CALL "SD_Accept" USING BY REFERENCE ACP-JS "ACP-JS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  MR999
           END-IF
           IF  ESTAT  NOT  =  "01"  AND  "06"
               GO  TO  MR005
           END-IF
           IF  W-JS   NOT  =  0  AND  1
               GO  TO  MR005
           END-IF.
       MR010.
           CALL "SD_Accept" USING BY REFERENCE ACP-OK "ACP-OK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  MR005
           END-IF
           IF  ESTAT  NOT  =  "01"
               GO  TO  MR010
           END-IF
           IF  W-OK        =  "9"
               GO  TO  MR005
           END-IF
           IF  W-OK   NOT  =  "1"
               GO  TO  MR010
           END-IF.
       MR020.
      *           READ    JSTR     NEXT       AT   END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR999
           END-IF
           IF  JSTR-17  NOT   =   1
               GO  TO  MR020
           END-IF
           IF  JSTR-03  NOT   =   0
               GO  TO  MR020
           END-IF
           IF  JSTR-112 NOT   >   JSTR-122
               GO  TO  MR020
           END-IF
           MOVE  JSTR-16    TO  W-JSD.
           IF  W-JSD              =   2
               MOVE    1        TO  W-JSD
           END-IF
           IF  W-JS          NOT  =   W-JSD
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
           INITIALIZE     W-OK.
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
           PERFORM  SET-RTN  THRU  SET-EX
                  VARYING   I   FROM  1 BY  1
                  UNTIL     I   >  10.
           MOVE   JSTR-05S  TO  JWTOK-09.
           MOVE   JSTR-14D  TO  JWTOK-10.
           MOVE   JSTR-15   TO  JWTOK-11.
           MOVE   ZERO      TO  JWTOK-12.
           MOVE   JSTR-14   TO  JWTOK-13.
           MOVE   JSTR-16   TO  JWTOK-14.
           MOVE   JSTR-14A  TO  JWTOK-15.
           MOVE   ZERO      TO  JWTOK-16.
           MOVE   JSTR-14B  TO  JWTOK-17.
           MOVE   JSTR-20   TO  JWTOK-20.
           MOVE   W-JS      TO  JWTOK-JS.
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
           END-IF
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
       WRT-EX.
      *****
      **************************************************
      *    èoâ◊êîÉZÉbÉgÅ@èàóù                          *
      **************************************************
       SET-RTN.
           COMPUTE  JWTOK-0811(I) =  JSTR-1111(I)  -  JSTR-1211(I).
           ADD   JWTOK-0811(I)   TO  JWTOK-082.
       SET-EX.
           EXIT.
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
      *
