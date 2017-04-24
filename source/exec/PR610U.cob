      *************************************************
      *    PROGRAM Å@Å@Å@ :Å@è¡îÔê≈êUë÷ñæç◊èëê∂ê¨     *
      *    AUTHOR         :  MAYUMI.I                 *
      *    DATE           :  90/12/26                 *
      *    COMPILE  TYPE  :  COBOL                    *
      *    PRINTER  TYPE  :  JIPS                     *
      *************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     PR610U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA           DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN             PIC  X(01).
       77  ERR-STAT            PIC  X(02).
       77  WK0256ID            PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1         PIC  X(003).
           02  STN-NO2         PIC  X(003).
       01  W-FID.
           02  W-FID1          PIC  X(006) VALUE "WK0256".
           02  W-FID2          PIC  X(003).
       01  W-KAKU              PIC  X(01).
      *
           COPY  LWMSG_PR.
       01  SDH_PR610U.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F            PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY  SIWAKH.
       77  F                   PIC  X(001).
      ***
       COPY  FCTL.
       COPY  SIWAKW.
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER     PIC  X(12)  VALUE  "CLEAR SCREEN".
       01  DSP-AREA.
           02  FILLER.
               03  FILLER     PIC X(22) VALUE
                        " è¡îÔê≈êUë÷ñæç◊èëê∂ê¨ ".
           02  FILLER.
               03  FILLER     PIC  N(01) VALUE "îN".
               03  FILLER     PIC  N(02) VALUE "åéìx".
           02  FILLER  PIC X(18) VALUE  "ämîF OK=1,NO=9 ( )".
      *
       01  DSP-SIGN1.
           02  FILLER.
               03  FILLER  PIC X(34) VALUE
                        "Ç±ÇÃèàóùÇ…ÇÊÇËÅAè¡îÔê≈êUë÷ñæç◊èëÇÃ".
               03  FILLER  PIC X(14) VALUE
                        "è¡îÔê≈äzÇäÓÇ…".
           02  FILLER.
               03  FILLER  PIC X(32) VALUE
                        "è¡îÔê≈é©ìÆêUë÷ì`ï[ÇçÏê¨ÇµÇ‹Ç∑ÅB".
           02  FILLER.
               03  FILLER  PIC X(38) VALUE
                        "èàóùèIóπå„ÅAì˙éüèàóùÇé¿çsÇµÇƒâ∫Ç≥Ç¢ÅB".
       01  DSP-YM.
           02  FILLER.
               03  FILLER     PIC  N(02).
               03  FILLER     PIC  N(02).
       01  ACP-AREA.
           02  ACP-KAKU       PIC X(01).
      *
           COPY  LSMSG_PR.
      *
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "CLEAR" "X" "1" "0" "12" " " "DSP-CLR"  RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "46" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA" " " "1" "0" "22" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0101DSP-AREA" "RX" "1" "30" "22" " " "01DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA" " " "2" "0" "6" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0102DSP-AREA" "N" "2" "6" "2" " " "02DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0202DSP-AREA" "N" "2" "12" "4" "0102DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-AREA" "X" "24" "61" "18" "02DSP-AREA" " "
            RETURNING RESU.
      *DSP-SIGN1
       CALL "SD_Init" USING
            "DSP-SIGN1" " " "0" "0" "118" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-SIGN1" " " "5" "0" "48" " " "DSP-SIGN1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0101DSP-SIGN1" "X" "5" "11" "34" " " "01DSP-SIGN1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "0201DSP-SIGN1" "X" "5" "45" "14" "0101DSP-SIGN1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-SIGN1" " " "7" "0" "32" "01DSP-SIGN1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0102DSP-SIGN1" "X" "7" "9" "32" " " "02DSP-SIGN1"
            RETURNING RESU.
       CALL "SD_Init" USING
            "03DSP-SIGN1" " " "9" "0" "38" "02DSP-SIGN1" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0103DSP-SIGN1" "X" "9" "9" "38" " " "03DSP-SIGN1"
            RETURNING RESU.
      *DSP-YM
       CALL "SD_Init" USING
            "DSP-YM" " " "0" "0" "8" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-YM" " " "2" "0" "8" " " "DSP-YM"  RETURNING RESU.
       CALL "SD_Init" USING
            "0101DSP-YM" "N" "2" "2" "4" " " "01DSP-YM"
            RETURNING RESU.
       CALL "SD_From" USING
            "0101DSP-YM" BY REFERENCE Z-GEMYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0201DSP-YM" "N" "2" "8" "4" "0101DSP-YM" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0201DSP-YM" BY REFERENCE Z-GEMMM "2" "0" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KAKU" "X" "24" "77" "1" " " "ACP-AREA"
            RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-KAKU" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       HAJIME.
           PERFORM  INI-RTN     THRU  INI-EX.
           PERFORM  MAIN-RTN    THRU  MAIN-EX.
           PERFORM  CLSE-ENT    THRU  CLSE-EXT.
           CALL "DB_Close".
           STOP  RUN.
      *************************
      *    ÇhÇmÇhÅ|ÇqÇsÇm     *
      *************************
       INI-RTN.
           ACCEPT   JS-SIGN FROM ARGUMENT-VALUE.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 255.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                           RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA" DSP-AREA "p"
                                           RETURNING RESU.
           IF  JS-SIGN  =  1
               CALL "SD_Output" USING "DSP-SIGN1" DSP-SIGN1 "p"
                                             RETURNING RESU
           END-IF.
      *
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE  "DATE  "     TO  FCTL-KEY.
      *           READ  FCTL-F  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
	           CALL "SD_Output" USING "INV-MCT" INV-MCT "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           MOVE  FCTL-REC     TO  Z-R.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           CALL "SD_Output" USING "DSP-YM" DSP-YM "p"
                                           RETURNING RESU.
           MOVE  Z-KONYMD     TO  ZYMD.
           PERFORM  Z-RTN     THRU  Z-EXT.
           IF  ZI > 15
               CALL "DB_Close"
               STOP  RUN
           END-IF.
       INI-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KAKU "ACP-KAKU" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           IF  W-KAKU = "9"
               CALL "DB_Close"
               STOP  RUN
           END-IF.
           IF  W-KAKU NOT = "1"
               GO  TO  INI-010
           END-IF.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO SDW_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           CALL "DB_F_Open" USING
            "OUTPUT" SDW_PNAME1 "EXCLUSIVE" BY REFERENCE SDW_IDLST "0".
      *
       INI-EX.
           EXIT.
      *****************************
      *    ÇlÇ`ÇhÇmÅ|ÇqÇsÇm       *
      *****************************
       MAIN-RTN.
           MOVE ZERO         TO SH-KEY1.
           MOVE Z-GESYMD     TO HTRDATE.
      *           START SDH KEY NOT LESS SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT LESS " SH-KEY1 RETURNING RET.
           IF  RET = 1
               GO TO MAIN-EX
           END-IF.
       MAIN-000.
      *           READ  SDH  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MAIN-EX
           END-IF.
           IF  HTRDATE < Z-GESYMD
               GO  TO  MAIN-EX
           END-IF.
           IF  HTRDATE > Z-GEMYMD
               GO  TO  MAIN-EX
           END-IF.
           IF  (HTAXKB NOT = SPACE) AND (HCOM = 0)
               CONTINUE
           ELSE
               GO  TO  MAIN-000
           END-IF.
           PERFORM  WRITE-RTN     THRU  WRITE-EX.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 000.
           GO  TO  MAIN-000.
       MAIN-EX.
           EXIT.
      ******************************
      *    ÇbÇkÇrÇdÅ|ÇdÇmÇs  Å@Å@  *
      ******************************
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
       CLSE-EXT.
           EXIT.
      ******************************
      *    ÇvÇqÇhÇsÇdÅ|ÇqÇsÇm      *
      ******************************
       WRITE-RTN.
           MOVE  SH-REC     TO  SW-REC.
           MOVE  SH-REC      TO  ERR-K.
      *           WRITE  SW-REC.
      *///////////////
           CALL "DB_Insert" USING
            SDW_PNAME1 SDW_LNAME SW-REC RETURNING RET.
           IF  ERR-STAT NOT = "00"
               MOVE  "SDW"     TO  ERR-F
               MOVE  "W"       TO  ERR-M
               PERFORM  ERR-ENT     THRU  ERR-EXT
           END-IF.
       WRITE-EX.
           EXIT.
      *
           COPY  LPMSG_PR.
      *
