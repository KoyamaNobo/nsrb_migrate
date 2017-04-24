       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT910U.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  éÛíçécí†íäèoÅiéÛíçì˙ìæà”êÊïiñºï *
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  91/10/08                        *
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
       01  W-JMSTD-R              PIC X(222).
       01  W-COM                  PIC S9(06).
       01  ZERO-SW                PIC X(04)     VALUE  SPACE.
       01  W-AREA.
           02  I                  PIC  9(02).
           02  W-SEN              PIC  9(01).
           02  W-FNGP             PIC  9(08).
           02  W-FNGPD  REDEFINES  W-FNGP.
             03  W-FNEN           PIC  9(04).
             03  W-FNENL  REDEFINES W-FNEN.
               04  W-FNEN1        PIC  9(02).
               04  W-FNEN2        PIC  9(02).
             03  W-FGET           PIC  9(02).
             03  W-FPEY           PIC  9(02).
           02  W-TNGP             PIC  9(08).
           02  W-TNGPD  REDEFINES  W-TNGP.
             03  W-TNEN           PIC  9(04).
             03  W-TNENL  REDEFINES W-TNEN.
               04  W-TNEN1        PIC  9(02).
               04  W-TNEN2        PIC  9(02).
             03  W-TGET           PIC  9(02).
             03  W-TPEY           PIC  9(02).
           02  W-FHCD             PIC  9(06).
           02  W-THCD             PIC  9(06).
           02  OKC                PIC  9(01).
           02  W-TEKI             PIC N(32).
           02  W-TEK   REDEFINES  W-TEKI.
             03  F                PIC  N(09).
             03  W-TEK1           PIC  N(06).
             03  W-TEK2           PIC  N(02).
             03  F                PIC  N(15).
       COPY  LWMSG.
      *
           COPY   LIBFDD.
           COPY   LTWK04.
           COPY   LJMSTD.
           COPY   LIHIM2.
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL     PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLR-01.
           02  FILLER   PIC  X(04) VALUE "    ".
           02  FILLER   PIC  X(04) VALUE "    ".
           02  FILLER   PIC  X(01) VALUE " ".
           02  FILLER   PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  FILLER   PIC  X(38) VALUE
               "                                      ".
           02  FILLER   PIC  X(34) VALUE
               "éÛíçécí†ÅiéÛíçì˙ìæà”êÊïiñºï Åjíäèo".
           02  FILLER   PIC  X(06) VALUE  "éÛíçì˙".
           02  FILLER   PIC  X(04) VALUE  "ïiñº".
           02  FILLER   PIC  X(18) VALUE  "ÇeÇqÇnÇl    /  /  ".
           02  FILLER   PIC  X(18) VALUE  "ÇsÇn        /  /  ".
           02  FILLER   PIC  X(08) VALUE  "0 ã≥Å@àÁ".
           02  FILLER   PIC  X(08) VALUE  "1 ÉèÅ[ÉN".
           02  FILLER   PIC  X(08) VALUE  "2 àÍÅ@î ".
           02  FILLER   PIC  X(08) VALUE  "9 ëSÅ@åè".
           02  FILLER   PIC  X(08) VALUE  "ëIë [ ]".
           02  FILLER   PIC  X(25) VALUE  "ämîF(OK=1,NO=9)-->   ÿ¿∞›".
       01  ACP-AREA.
           02  FILLER.
             03  ACP-FNEN   PIC 9(02).
             03  ACP-FGET   PIC 9(02).
             03  ACP-FPEY   PIC 9(02).
           02  FILLER.
             03  ACP-TNEN   PIC 9(02).
             03  ACP-TGET   PIC 9(02).
             03  ACP-TPEY   PIC 9(02).
           02  ACP-FHCD PIC 9(06).
           02  ACP-THCD PIC 9(06).
           02  ACP-SEN  PIC 9(01).
           02  ACP-OKC  PIC 9(01).
       01  DISP-MSG-SPACE1.
           02  FILLER   PIC X(40)     VALUE " ".
           COPY  LSMSG.
           COPY  LIBSCR.
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
            "CLR-01" " " "0" "0" "10" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "8" "24" "4" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-01" "X" "10" "24" "4" "01CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-01" "X" "10" "59" "1" "02CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-01" "X" "23" "61" "1" "03CLR-01" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "183" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "38" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "1" "21" "34" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "6" "26" "6" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "6" "37" "4" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "8" "15" "18" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "10" "15" "18" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "6" "47" "8" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "8" "47" "8" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "10" "47" "8" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-AREA" "X" "12" "47" "8" "09DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-AREA" "X" "12" "61" "8" "10DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12DSP-AREA" "X" "23" "41" "25" "11DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "26" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "8" "0" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FNEN" "9" "8" "25" "2" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FNEN" BY REFERENCE W-FNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FGET" "9" "8" "28" "2" "ACP-FNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FPEY" "9" "8" "31" "2" "ACP-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
          "02ACP-AREA" " " "10" "0" "6" "01ACP-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-TNEN" "9" "10" "25" "2" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TNEN" BY REFERENCE W-TNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TGET" "9" "10" "28" "2" "ACP-TNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TPEY" "9" "10" "31" "2" "ACP-TGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FHCD" "9" "8" "36" "6" "02ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FHCD" BY REFERENCE W-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-THCD" "9" "10" "36" "6" "ACP-FHCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-THCD" BY REFERENCE W-THCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "12" "67" "1" "ACP-THCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" "ACP-SEN" " "  RETURNING RESU.
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
      *
      ****************************
      ***  “ ≤ ›  R T N        ***
      ****************************
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           PERFORM  UPD-RTN     THRU   UPD-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************
      ***   ≤∆º¨Ÿ   R T N           ***
      *********************************
       INI-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK04_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK04_IDLST "0".
       INI-EX.
            EXIT.
      ******************************
      ***   U P D   R T N        ***
      ******************************
       UPD-RTN.
           COPY LIBCPR.
           PERFORM  GAMEN-RTN     THRU  GAMEN-EX.
           IF  ESTAT  =  "P9"
               GO  TO  UPD-EX
           END-IF
      *
           INITIALIZE                 JMSTD-KEY3.
           MOVE  W-FHCD           TO  JMSTD-03.
      *
      *           START  JMSTD  KEY  NOT <  JMSTD-KEY3  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY3" " NOT < " JMSTD-KEY3
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF.
       UPD-010.
      ***  éÛíçÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  JMSTD  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF
      *
           IF  JMSTD-02  <  W-FNGP   OR  >  W-TNGP
               GO  TO  UPD-010
           END-IF
      *
           IF  JMSTD-03  <  W-FHCD
               GO  TO  UPD-010
           END-IF
           IF  JMSTD-03  >  W-THCD
               GO  TO  UPD-EX
           END-IF
      *
           PERFORM  ZERO-RTN     THRU  ZERO-EX.
           IF  ZERO-SW  =  "ZERO"
               MOVE  SPACE     TO  ZERO-SW
               GO  TO  UPD-010
           END-IF
      *
           IF  W-SEN  =  9
               GO  TO  UPD-020
           END-IF
           MOVE  JMSTD-05     TO  HI-MHCD HI-HCD.
      ***  ïiñºÉ}ÉXÉ^Å@ÇqÇdÇ`Çc
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-010
           END-IF
           IF  W-SEN  =  ZERO
               IF  HI-BC3         <  30  OR  >  39
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3         <  20  OR  >  29
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3         >  19
                   GO  TO  UPD-010
               END-IF
           END-IF.
      *
       UPD-020.
           MOVE  SPACE     TO  WK04-R.
           INITIALIZE  WK04-R.
           MOVE  JMSTD-R   TO  W-JMSTD-R.
           MOVE  W-JMSTD-R TO  WK04-R.
           MOVE  JMSTD-17  TO  WK04-17.
           MOVE  JMSTD-91  TO  WK04-91.
           MOVE  ZERO      TO  WK04-991.
           MOVE  SPACE     TO  W-TEKI.
           MOVE  JMSTD-13  TO  W-TEKI.
           MOVE  W-TEK1    TO  WK04-801.
           MOVE  W-TEK2    TO  WK04-803.
           MOVE  JMSTD-23  TO  WK04-23.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           CALL "SD_Output" USING "OK-01" OK-01 "p" RETURNING RESU.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO  TO  UPD-010.
       UPD-EX.
           EXIT.
      *******************************
      ***   G A M E N   R T N     ***
      *******************************
       GAMEN-RTN.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       GAMEN-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FNEN "ACP-FNEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  GAMEN-EX
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-010
           END-IF
           CALL "SD_Output" USING
            "ACP-FNEN" ACP-FNEN "p" RETURNING RESU.
       GAMEN-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-FGET "ACP-FGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-010
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-020
           END-IF
           CALL "SD_Output" USING
            "ACP-FGET" ACP-FGET "p" RETURNING RESU.
           IF  W-FGET      =  ZERO
               IF  W-FNEN2      =  ZERO
                   GO  TO  GAMEN-030
               END-IF
           END-IF
           IF  W-FGET      <  1  OR  >  12
               GO  TO  GAMEN-020
           END-IF.
       GAMEN-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-FPEY "ACP-FPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-020
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-030
           END-IF
           CALL "SD_Output" USING
            "ACP-FPEY" ACP-FPEY "p" RETURNING RESU.
           IF  W-FPEY   =   ZERO
               IF  W-FGET   =  ZERO
                   MOVE  ZERO      TO  W-FNGP
                   GO  TO  GAMEN-040
               END-IF
           END-IF
           IF  W-FPEY   <   1   OR   >  31
               GO  TO  GAMEN-030
           END-IF
           MOVE  ZERO       TO  W-FNEN1.
           IF  W-FNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-FNEN
           END-IF
           IF  W-FNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-FNEN
           END-IF.
       GAMEN-040.
           CALL "SD_Accept" USING BY REFERENCE ACP-TNEN "ACP-TNEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-030
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-040
           END-IF
           CALL "SD_Output" USING
            "ACP-TNEN" ACP-TNEN "p" RETURNING RESU.
       GAMEN-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-TGET "ACP-TGET"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-040
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-050
           END-IF
           CALL "SD_Output" USING
            "ACP-TGET" ACP-TGET "p" RETURNING RESU.
           IF  W-TGET      =  99
               IF  W-TNEN2      =  99
                   GO  TO  GAMEN-060
               END-IF
           END-IF
           IF  W-TGET      <  1  OR  >  12
               GO  TO  GAMEN-050
           END-IF.
       GAMEN-060.
           CALL "SD_Accept" USING BY REFERENCE ACP-TPEY "ACP-TPEY"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-050
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-060
           END-IF
           CALL "SD_Output" USING
            "ACP-TPEY" ACP-TPEY "p" RETURNING RESU.
           IF  W-TPEY   =    99
               IF  W-TGET   =    99
                   MOVE  99999999    TO  W-TNGP
                   GO  TO  GAMEN-070
               END-IF
           END-IF
           IF  W-TPEY   <   1   OR   >  31
               GO  TO  GAMEN-060
           END-IF
           MOVE  ZERO       TO  W-TNEN1.
           IF  W-TNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-TNEN
           END-IF
           IF  W-TNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-TNEN
           END-IF
           IF  W-FNGP  >  W-TNGP
               GO  TO  GAMEN-040
           END-IF.
       GAMEN-070.
           CALL "SD_Accept" USING BY REFERENCE ACP-FHCD "ACP-FHCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-060
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-070
           END-IF
           CALL "SD_Output" USING
            "ACP-FHCD" ACP-FHCD "p" RETURNING RESU.
       GAMEN-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-THCD "ACP-THCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-070
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-080
           END-IF
           CALL "SD_Output" USING
            "ACP-THCD" ACP-THCD "p" RETURNING RESU.
           IF  W-FHCD  >  W-THCD
               GO  TO  GAMEN-070
           END-IF.
       GAMEN-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-080
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-090
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT =  0 AND 1 AND 2 AND 9
               GO  TO  GAMEN-090
           END-IF.
       GAMEN-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-090
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
               INITIALIZE  W-AREA
               GO  TO  GAMEN-RTN
           END-IF.
       GAMEN-EX.
           EXIT.
      *
      **************************
      ***  Ã ß ≤ Ÿ  CLOSE    ***
      **************************
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***************************
      ***   W R I   R T N     ***
      ***************************
       WRI-RTN.
      *           WRITE    WK04-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK04_PNAME1 JT-WK04_LNAME WK04-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  "W"          TO  ERR-M
               MOVE  "JT-WK04"    TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *****************************
      *    ÇyÇdÇqÇnÅ|ÇqÇsÇm       *
      *****************************
       ZERO-RTN.
           MOVE  1     TO  I.
       ZERO-010.
           IF  I  >  10
               MOVE  "ZERO"     TO  ZERO-SW
               GO  TO  ZERO-EX
           END-IF
           COMPUTE W-COM = JMSTD-1111(I) - JMSTD-141(I) - JMSTD-1211(I)
                                                        - JMSTD-151(I).
           IF  W-COM  NOT =  ZERO
               GO  TO  ZERO-EX
           END-IF
           ADD  1     TO  I.
           GO  TO  ZERO-010.
       ZERO-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
