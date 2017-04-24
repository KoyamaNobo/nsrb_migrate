       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT025U.
      *********************************************************
      *    PROGRAM         :  éÛíçÅEóaÇ©ÇËì˙ïÒíäèo            *
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  98/11/26                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT               PIC X(2).
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  W-JS                   PIC 9(01).
       01  W-JS-MEI               PIC N(03).
       01  W-AREA.
           02  ACT                PIC  9(01).
           02  W-NGP.
               03  W-NEN          PIC  9(04).
               03  W-NENL   REDEFINES  W-NEN.
                   04  W-NEN1     PIC  9(02).
                   04  W-NEN2     PIC  9(02).
               03  W-GET          PIC  9(02).
               03  W-PEY          PIC  9(02).
           02  W-NGPL   REDEFINES  W-NGP.
               03  F              PIC  9(02).
               03  W-NGPS         PIC  9(06).
           02  OKC                PIC  9(01).
      ***
       COPY  LWMSG.
      ***
           COPY   LIBFDD.
           COPY   LTWK02.
           COPY   LJMST1.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLR-01.
           02  FILLER  PIC  X(01) VALUE " ".
           02  FILLER  PIC  X(01) VALUE " ".
      ***
       01  DSP-AREA.
           02  FILLER  PIC  X(30) VALUE
               " éÛíçÅEóaÇËÅEéÊÇËÇÊÇØì˙ïÒíäèo ".
           02  FILLER.
             03  FILLER  PIC  X(06) VALUE  "ã≥Å@àÁ".
             03  FILLER  PIC  X(05) VALUE  "=0 , ".
             03  FILLER  PIC  X(06) VALUE  "àÍÅ@î ".
             03  FILLER  PIC  X(09) VALUE  "=1  ...  ".
           02  FILLER  PIC  X(14) VALUE  "ÇO éÛ íç ì˙ ïÒ".
           02  FILLER  PIC  X(14) VALUE  "ÇT óa ÇË ì˙ ïÒ".
           02  FILLER  PIC  X(15) VALUE  "ÇU éÊÇËÇÊÇØì˙ïÒ".
           02  FILLER  PIC  X(14) VALUE  "ÇX ëS       åè".
           02  FILLER  PIC  X(08) VALUE  "ëIë [ ]".
           02  FILLER  PIC  X(28) VALUE
               "Åi '  îN   åé   ì˙ ì¸óÕï™ Åj".
           02  FILLER  PIC  X(25) VALUE  "ämîF(OK=1,NO=9)-->   ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-JS      PIC 9(01).
           02  ACP-ACT     PIC 9(01).
           02  ACP-NGP.
               03  ACP-NEN2  PIC  9(02).
               03  ACP-GET   PIC  9(02).
               03  ACP-PEY   PIC  9(02).
           02  ACP-OKC     PIC 9(01).
      *
       01  DISP-MSG-SPACE1.
           02  FILLER  PIC X(40)     VALUE " ".
      ***
           COPY  LSMSG.
           COPY  LIBSCR.
      ***
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *CLR-01
       CALL "SD_Init" USING 
            "CLR-01" " " "0" "0" "2" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "10" "40" "1" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-01" "X" "24" "60" "1" "01CLR-01" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "174" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "30" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" " " "4" "0" "26" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-AREA" "X" "4" "20" "6" " " "02DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-AREA" "X" "4" "26" "5" "0102DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-AREA" "X" "4" "31" "6" "0202DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-AREA" "X" "4" "37" "9" "0302DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "7" "21" "14" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "9" "21" "14" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "11" "21" "15" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "13" "21" "14" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "15" "37" "8" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "19" "22" "28" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "24" "51" "25" "08DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "9" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JS" "9" "4" "45" "1" " " "ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-ACT" "9" "15" "43" "1" "ACP-JS" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-ACT" BY REFERENCE ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NGP" " " "19" "0" "6" "ACP-ACT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-NEN2" "9" "19" "26" "2" " " "ACP-NGP" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-NEN2" BY REFERENCE W-NEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-GET" "9" "19" "31" "2" "ACP-NEN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-PEY" "9" "19" "36" "2" "ACP-GET" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "70" "1" "ACP-NGP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DISP-MSG-SPACE1
       CALL "SD_Init" USING 
            "DISP-MSG-SPACE1" " " "24" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DISP-MSG-SPACE1" "X" "24" "1" "40" " " "DISP-MSG-SPACE1"
            RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  “ ≤ ›  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           PERFORM  UPD-RTN     THRU   UPD-EX.
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
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           ACCEPT  W-NGPS    FROM  DATE.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           CALL "SD_Output" USING "ACP-NGP" ACP-NGP "p" RETURNING RESU.
      *
           COPY  LIBCPR.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK02_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JMST1_PNAME1 "SHARED" BY REFERENCE JMST1_IDLST "1"
            "JMST1-KEY1" BY REFERENCE JMST1-KEY1.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK02_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK02_IDLST "0".
       INI-EX.
            EXIT.
      *
      ******************************
      ***   U P D   R T N        ***
      ******************************
      **
       UPD-RTN.
           PERFORM  GAMEN-RTN     THRU  GAMEN-EX.
           IF  ESTAT  =  "P9"
               GO  TO  UPD-EX
           END-IF
      *
           MOVE  SPACE          TO  JMST1-KEY1.
           IF  W-JS            =   0
               MOVE  100000         TO  JMST1-07
           END-IF
           IF  W-JS            =   1
               MOVE  300000         TO  JMST1-07
           END-IF.
      *           START JMST1 KEY NOT < JMST1-KEY1  INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMST1_PNAME1 "JMST1-KEY1" " NOT < " JMST1-KEY1
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF.
       UPD-010.
      ***  éÛíçÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  JMST1 NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST1_PNAME1 BY REFERENCE JMST1-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF
           IF  JMST1-90        =   2
               MOVE  1              TO  JMST1-90
           END-IF
           IF  W-JS       NOT  =   JMST1-90
               GO  TO  UPD-EX
           END-IF
           IF  W-NGP      NOT  =   JMST1-891
               GO  TO  UPD-010
           END-IF
           IF  ACT  =  9
               GO  TO  UPD-020
           END-IF
           IF  ACT  NOT  =  JMST1-01
               GO  TO  UPD-010
           END-IF.
       UPD-020.
           PERFORM  WRI-RTN     THRU  WRI-EX.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO  TO  UPD-010.
       UPD-EX.
           EXIT.
      *
      *******************************
      ***   G A M E N   R T N     ***
      *******************************
      **
       GAMEN-RTN.
           CALL "SD_Accept" USING BY REFERENCE ACP-JS "ACP-JS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  GAMEN-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-RTN
           END-IF
           IF  W-JS NOT =  0  AND  1
               GO  TO  GAMEN-RTN
           END-IF
           CALL "SD_Output" USING "ACP-JS" ACP-JS "p" RETURNING RESU.
       GAMEN-ACT.
           CALL "SD_Accept" USING BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  GAMEN-EX
           END-IF
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-RTN
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-ACT
           END-IF
           IF  ACT  NOT =  0  AND  5  AND  6  AND  9
               GO  TO  GAMEN-ACT
           END-IF
           CALL "SD_Output" USING "ACP-ACT" ACP-ACT "p" RETURNING RESU.
       GAMEN-NEN.
           CALL "SD_Accept" USING BY REFERENCE ACP-NEN2 "ACP-NEN2"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-ACT
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-NEN
           END-IF
           MOVE  ZERO           TO  W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       GAMEN-GET.
           CALL "SD_Accept" USING BY REFERENCE ACP-GET "ACP-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-NEN
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-GET
           END-IF
           IF  W-GET      <  0   OR   >  12
               GO  TO  GAMEN-GET
           END-IF.
       GAMEN-PEY.
           CALL "SD_Accept" USING BY REFERENCE ACP-PEY "ACP-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-GET
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-PEY
           END-IF
           IF  W-PEY      <  0   OR   >  31
               GO  TO  GAMEN-PEY
           END-IF.
       GAMEN-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-PEY
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-OKC
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT =  "1"  AND  "9"
               GO  TO  GAMEN-OKC
           END-IF
           IF  OKC  =  "9"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU
               GO  TO  GAMEN-RTN
           END-IF.
       GAMEN-EX.
           EXIT.
      *
      ***************************
      ***   W R I   R T N     ***
      ***************************
      **
       WRI-RTN.
           MOVE  SPACE          TO  W02-R.
           INITIALIZE               W02-R.
           MOVE  JMST1-07       TO  W02-01.
           MOVE  JMST1-08       TO  W02-02.
           MOVE  JMST1-01       TO  W02-03.
           MOVE  JMST1-02       TO  W02-04.
           MOVE  JMST1-04       TO  W02-051.
           MOVE  JMST1-10       TO  W02-052.
           MOVE  JMST1-03       TO  W02-06.
           MOVE  JMST1-09       TO  W02-06A.
           COMPUTE W02-0711(01) = JMST1-1111(01) - JMST1-151(01)
                                - JMST1-1211(01) - JMST1-141(01).
           COMPUTE W02-0711(02) = JMST1-1111(02) - JMST1-151(02)
                                - JMST1-1211(02) - JMST1-141(02).
           COMPUTE W02-0711(03) = JMST1-1111(03) - JMST1-151(03)
                                - JMST1-1211(03) - JMST1-141(03).
           COMPUTE W02-0711(04) = JMST1-1111(04) - JMST1-151(04)
                                - JMST1-1211(04) - JMST1-141(04).
           COMPUTE W02-0711(05) = JMST1-1111(05) - JMST1-151(05)
                                - JMST1-1211(05) - JMST1-141(05).
           COMPUTE W02-0711(06) = JMST1-1111(06) - JMST1-151(06)
                                - JMST1-1211(06) - JMST1-141(06).
           COMPUTE W02-0711(07) = JMST1-1111(07) - JMST1-151(07)
                                - JMST1-1211(07) - JMST1-141(07).
           COMPUTE W02-0711(08) = JMST1-1111(08) - JMST1-151(08)
                                - JMST1-1211(08) - JMST1-141(08).
           COMPUTE W02-0711(09) = JMST1-1111(09) - JMST1-151(09)
                                - JMST1-1211(09) - JMST1-141(09).
           COMPUTE W02-0711(10) = JMST1-1111(10) - JMST1-151(10)
                                - JMST1-1211(10) - JMST1-141(10).
           MOVE  JMST1-06       TO  W02-08.
           MOVE  JMST1-892      TO  W02-10.
           MOVE  JMST1-13       TO  W02-11.
           MOVE  JMST1-20       TO  W02-12.
           MOVE  JMST1-16       TO  W02-16.
           MOVE  JMST1-90       TO  W02-90.
           MOVE  JMST1-17       TO  W02-15.
           MOVE  JMST1-22       TO  W02-17.
           MOVE  JMST1-23       TO  W02-23.
           MOVE  W-JS           TO  W02-JS.
           MOVE  W-NGPS         TO  W02-891S.
      *           WRITE    W02-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK02_PNAME1 JT-WK02_LNAME W02-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  "W"          TO  ERR-M
               MOVE  "JT-WK02"    TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *
      **************************
      ***  Ã ß ≤ Ÿ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK02_IDLST JT-WK02_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST1_IDLST JMST1_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
