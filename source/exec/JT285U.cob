       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT285U.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  éÛíçécí†íäèoÅiïiñºÅEíSìñÅjÅ@    *
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
       01  W-JMSTD-R              PIC X(218).
       01  W-COM                  PIC S9(06).
       01  ZERO-SW                PIC X(04)     VALUE  SPACE.
      *    ZERO-SW  =  "ZERO"  åvéZéÆÇ™ëSïîÇyÇdÇqÇnÇ»ÇÃÇ≈ÇÊÇ›Ç∆ÇŒÇµÅB
      *    ÇªÇÍà»äOÇÕÅCà»â∫ÇÃèàóùÅB
       01  W-AREA.
           02  I                  PIC  9(02).
           02  W-SEN              PIC  9(01).
           02  W-FROM             PIC  9(06).
           02  W-TO               PIC  9(06).
           02  W-FTNC             PIC  9(02).
           02  W-TTNC             PIC  9(02).
           02  OKC                PIC  9(01).
      ***
       COPY  LWMSG.
      ***
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  CLR-01.
           02  FILLER  PIC  X(06) VALUE "      ".
           02  FILLER  PIC  X(06) VALUE "      ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(01) VALUE " ".
           02  FILLER  PIC  X(01) VALUE " ".
      ***
       01  DSP-AREA.
           02  FILLER  PIC  X(28) VALUE
               "                            ".
           02  FILLER  PIC  X(26) VALUE
               "éÛíçécí†ÅiïiñºÅEíSìñÅjíäèo".
           02  FILLER  PIC  X(08) VALUE  "ïiñº∫∞ƒﬁ".
           02  FILLER  PIC  X(08) VALUE  "ÇeÇqÇnÇl".
           02  FILLER  PIC  X(04) VALUE  "ÇsÇn".
           02  FILLER  PIC  X(08) VALUE  "íSìñ∫∞ƒﬁ".
           02  FILLER  PIC  X(09) VALUE  "0Å@ã≥Å@àÁ".
           02  FILLER  PIC  X(09) VALUE  "1Å@ÉèÅ[ÉN".
           02  FILLER  PIC  X(09) VALUE  "2Å@àÍÅ@î ".
           02  FILLER  PIC  X(09) VALUE  "9Å@ëSÅ@åè".
           02  FILLER  PIC  X(08) VALUE  "ëIë [ ]".
           02  FILLER  PIC  X(25) VALUE  "ämîF(OK=1,NO=9)-->   ÿ¿∞›".
       01  ACP-AREA.
           02  ACP-FROM       PIC 9(06).
           02  ACP-TO         PIC 9(06).
           02  ACP-FTNC       PIC 9(02).
           02  ACP-TTNC       PIC 9(02).
           02  ACP-SEN        PIC 9(01).
           02  ACP-OKC        PIC 9(01).
      *
       01  DISP-MSG-SPACE1.
           02  FILLER  PIC X(40)     VALUE " ".
      ***
       COPY  LSMSG.
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
            "CLR-01" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "8" "24" "6" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-01" "X" "10" "24" "6" "01CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-01" "X" "8" "39" "2" "02CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-01" "X" "10" "39" "2" "03CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-01" "X" "10" "69" "1" "04CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLR-01" "X" "23" "61" "1" "05CLR-01" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "151" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "28" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "1" "21" "26" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "6" "22" "8" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "8" "11" "8" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "10" "11" "4" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "6" "35" "8" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "6" "51" "9" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "8" "51" "9" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "10" "51" "9" "08DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10DSP-AREA" "X" "12" "51" "9" "09DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "11DSP-AREA" "X" "12" "63" "8" "10DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12DSP-AREA" "X" "23" "41" "25" "11DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "18" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FROM" "9" "8" "24" "6" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FROM" BY REFERENCE W-FROM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TO" "9" "10" "24" "6" "ACP-FROM" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TO" BY REFERENCE W-TO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FTNC" "9" "8" "39" "2" "ACP-TO" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FTNC" BY REFERENCE W-FTNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TTNC" "9" "10" "39" "2" "ACP-FTNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TTNC" BY REFERENCE W-TTNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "12" "69" "1" "ACP-TTNC" " " RETURNING RESU.
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
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  “ ≤ ›  R T N        ***
      ****************************
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           IF  ESTAT     =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           PERFORM  UPD-RTN     THRU   UPD-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************
      ***   ≤∆º¨Ÿ   R T N           ***
      *********************************
       INI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FROM "ACP-FROM"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-010
           END-IF
           CALL "SD_Output" USING
            "ACP-FROM" ACP-FROM "p" RETURNING RESU.
       INI-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-TO "ACP-TO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-020
           END-IF
           CALL "SD_Output" USING "ACP-TO" ACP-TO "p" RETURNING RESU.
           IF  W-FROM  >  W-TO
               GO  TO  INI-020
           END-IF.
       INI-022.
           CALL "SD_Accept" USING BY REFERENCE ACP-FTNC "ACP-FTNC"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-020
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-022
           END-IF
           CALL "SD_Output" USING
            "ACP-FTNC" ACP-FTNC "p" RETURNING RESU.
       INI-024.
           CALL "SD_Accept" USING BY REFERENCE ACP-TTNC "ACP-TTNC"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-022
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-024
           END-IF
           CALL "SD_Output" USING
            "ACP-TTNC" ACP-TTNC "p" RETURNING RESU.
           IF  W-FTNC  >  W-TTNC
               GO  TO  INI-024
           END-IF.
       INI-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-024
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-030
           END-IF
           CALL "SD_Output" USING "ACP-SEN" ACP-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT =  0 AND 1 AND 2 AND 9
               GO  TO  INI-030
           END-IF.
       INI-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  INI-030
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-050
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT =  "1"  AND  "9"
               GO  TO  INI-050
           END-IF
           IF  OKC  =  "9"
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU
               INITIALIZE  W-AREA
               GO  TO  INI-RTN
           END-IF
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
      *
           INITIALIZE                 JMSTD-KEY3.
           MOVE  W-FROM           TO  JMSTD-03.
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
           IF  JMSTD-03  <  W-FROM
               GO  TO  UPD-010
           END-IF
           IF  JMSTD-03  >  W-TO
               GO  TO  UPD-EX
           END-IF
           IF  JMSTD-91  <  W-FTNC  OR  >  W-TTNC
               GO  TO  UPD-010
           END-IF
      *
           IF  JMSTD-01  NOT =  ZERO  AND  2
               GO  TO  UPD-010
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
           MOVE  JMSTD-03     TO  HI-MHCD HI-HCD.
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
               IF  HI-BC3         <  20  OR  > 29
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3         > 19
                   GO  TO  UPD-010
               END-IF
           END-IF.
      *
       UPD-020.
           MOVE  SPACE     TO  WK04-R.
           INITIALIZE  WK04-R.
           MOVE  JMSTD-R   TO  W-JMSTD-R.
           MOVE  W-JMSTD-R TO  WK04-R.
           MOVE  JMSTD-22  TO  WK04-22.
           MOVE  JMSTD-23  TO  WK04-23.
           MOVE  ZERO      TO  WK04-991.
           MOVE  JMSTD-91  TO  WK04-91.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD-010.
       UPD-EX.
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
           COMPUTE W-COM = JMSTD-1111(I) - JMSTD-141(I) - JMSTD-1211(I).
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
