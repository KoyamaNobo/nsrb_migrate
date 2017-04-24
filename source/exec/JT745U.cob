       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT750U.
      *********************************************************
      *    PROGRAM         :  受注合計表抽出（月得意先品名別）*
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  98/02/12                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT             PIC X(2).
       77  WK0128ID             PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1          PIC  X(003).
           02  STN-NO2          PIC  X(003).
       01  W-FID.
           02  W-FID1           PIC  X(006) VALUE "WK0128".
           02  W-FID2           PIC  X(003).
       01  W-DATA.
           02  W-NG.
             03  W-NEN          PIC  9(02).
             03  W-GET          PIC  9(02).
           02  W-TCD.
             03  W-STCD         PIC  9(04).
             03  W-ETCD         PIC  9(04).
           02  W-HCD.
             03  W-SHCD         PIC  9(06).
             03  W-EHCD         PIC  9(06).
           02  W-SEN            PIC  9(01).
           02  W-DMM            PIC  9(01).
       COPY  LWMSG.
      *
           COPY  LTWK07.
           COPY  LJMSTD.
           COPY  LIHIM2.
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
           02  FILLER.
             03  FILLER  PIC  X(02) VALUE "  ".
             03  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER.
             03  FILLER  PIC  X(04) VALUE "    ".
             03  FILLER  PIC  X(06) VALUE "      ".
           02  FILLER.
             03  FILLER  PIC  X(04) VALUE "    ".
             03  FILLER  PIC  X(06) VALUE "      ".
           02  FILLER  PIC  X(01) VALUE " ".
           02  FILLER  PIC  X(01) VALUE " ".
       01  DSP-AREA.
           02  FILLER  PIC  X(34) VALUE
                "                                  ".
           02  FILLER  PIC  X(32) VALUE
               "受注数合計表（得意先品名別）抽出".
           02  FILLER  PIC  X(12) VALUE  "'  年   月分".
           02  FILLER.
             03  FILLER  PIC  X(10) VALUE  "得意先ｺｰﾄﾞ".
             03  FILLER  PIC  X(08) VALUE  "品名ｺｰﾄﾞ".
             03  FILLER  PIC  X(09) VALUE  "0　教　育".
           02  FILLER.
             03  FILLER  PIC  X(08) VALUE  "ＦＲＯＭ".
             03  FILLER  PIC  X(09) VALUE  "1　ワーク".
           02  FILLER.
             03  FILLER  PIC  X(04) VALUE  "ＴＯ".
             03  FILLER  PIC  X(09) VALUE  "2　一　般".
           02  FILLER.
             03  FILLER  PIC  X(09) VALUE  "9　全　件".
             03  FILLER  PIC  X(08) VALUE  "選択 [ ]".
           02  FILLER  PIC  X(25) VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  FILLER.
             03  A-NEN     PIC 9(02).
             03  A-GET     PIC 9(02).
           02  FILLER.
             03  A-STCD    PIC 9(04).
             03  A-SHCD    PIC 9(06).
           02  FILLER.
             03  A-ETCD    PIC 9(04).
             03  A-EHCD    PIC 9(06).
           02  FILLER.
             03  A-SEN     PIC 9(01).
           02  A-DMM       PIC 9(01).
       01  DISP-MSG-SPACE1.
           02  FILLER  PIC X(40)     VALUE " ".
       COPY  LSMSG.
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
            "CLR-01" " " "0" "0" "26" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" " " "6" "0" "4" " " "CLR-01"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101CLR-01" "X" "6" "12" "2" " " "01CLR-01" RETURNING RESU.
       CALL "SD_Init" USING 
          "0201CLR-01" "X" "6" "16" "2" "0101CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-01" " " "8" "0" "10" "01CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102CLR-01" "X" "8" "21" "4" " " "02CLR-01" RETURNING RESU.
       CALL "SD_Init" USING 
          "0202CLR-01" "X" "8" "30" "6" "0102CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-01" " " "10" "0" "10" "02CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "0103CLR-01" "X" "10" "21" "4" " " "03CLR-01" RETURNING RESU.
       CALL "SD_Init" USING 
         "0203CLR-01" "X" "10" "30" "6" "0103CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-01" "X" "14" "61" "1" "03CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-01" "X" "23" "61" "1" "04CLR-01" " " RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "177" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "20" "34" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "1" "21" "32" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "6" "11" "12" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" " " "8" "0" "27" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-AREA" "X" "8" "18" "10" " " "04DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-AREA" "X" "8" "29" "8" "0104DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304DSP-AREA" "X" "8" "43" "9" "0204DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" " " "10" "0" "17" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0105DSP-AREA" "X" "10" "11" "8" " " "05DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0205DSP-AREA" "X" "10" "43" "9" "0105DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" " " "12" "0" "13" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0106DSP-AREA" "X" "12" "11" "4" " " "06DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0206DSP-AREA" "X" "12" "43" "9" "0106DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" " " "14" "0" "17" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0107DSP-AREA" "X" "14" "43" "9" " " "07DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0207DSP-AREA" "X" "14" "55" "8" "0107DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "23" "41" "25" "07DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "26" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01ACP-AREA" " " "6" "0" "4" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NEN" "9" "6" "12" "2" " " "01ACP-AREA"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NEN" BY REFERENCE W-NEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GET" "9" "6" "17" "2" "A-NEN" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GET" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "10" "0" "10" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "10" "21" "4" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "9" "10" "30" "6" "A-STCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03ACP-AREA" " " "12" "0" "10" "02ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "12" "21" "4" " " "03ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "9" "12" "30" "6" "A-ETCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04ACP-AREA" " " "14" "0" "1" "03ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "14" "61" "1" " " "04ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "61" "1" "04ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
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
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           PERFORM  UPD-RTN     THRU   UPD-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
       INI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-WK07_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK07_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK07_IDLST "0".
       INI-EX.
            EXIT.
       UPD-RTN.
           PERFORM  GAMEN-RTN     THRU  GAMEN-EX.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  UPD-EX
           END-IF
           INITIALIZE                 JMSTD-KEY2.
           MOVE  W-STCD           TO  JMSTD-04.
           MOVE  W-SHCD           TO  JMSTD-05.
      *           START  JMSTD  KEY  NOT <  JMSTD-KEY2  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY2" " NOT < " JMSTD-KEY2 RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF.
       UPD-010.
      *           READ  JMSTD  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF
           IF  JMSTD-04  >  W-ETCD
               GO  TO  UPD-EX
           END-IF
           IF  JMSTD-05  <  W-SHCD  OR  >  W-EHCD
               GO  TO  UPD-010
           END-IF
           IF  (JMSTD-0212 NOT = W-NEN)  OR  (JMSTD-022 NOT = W-GET)
               GO  TO  UPD-010
           END-IF
           IF  W-SEN  =  9
               GO  TO  UPD-020
           END-IF
           MOVE  JMSTD-05     TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-010
           END-IF
           IF  W-SEN  =  0
               IF  HI-BC3  <  30  OR  >  39
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-SEN  =  1
               IF  HI-BC3  <  20  OR   > 29
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-SEN  =  2
               IF  HI-BC3  > 19
                   GO  TO  UPD-010
               END-IF
           END-IF.
       UPD-020.
           MOVE  SPACE     TO  WK07-R.
           INITIALIZE  WK07-R.
           MOVE  W-NG           TO  WK07-01.
           MOVE  JMSTD-04       TO  WK07-02.
           MOVE  JMSTD-05       TO  WK07-03.
           MOVE  JMSTD-09       TO  WK07-04.
           MOVE  JMSTD-1111(01) TO  WK07-0511(01).
           MOVE  JMSTD-1111(02) TO  WK07-0511(02).
           MOVE  JMSTD-1111(03) TO  WK07-0511(03).
           MOVE  JMSTD-1111(04) TO  WK07-0511(04).
           MOVE  JMSTD-1111(05) TO  WK07-0511(05).
           MOVE  JMSTD-1111(06) TO  WK07-0511(06).
           MOVE  JMSTD-1111(07) TO  WK07-0511(07).
           MOVE  JMSTD-1111(08) TO  WK07-0511(08).
           MOVE  JMSTD-1111(09) TO  WK07-0511(09).
           MOVE  JMSTD-1111(10) TO  WK07-0511(10).
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD-010.
       UPD-EX.
           EXIT.
       GAMEN-RTN.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       GAMEN-010.
           CALL "SD_Accept" USING BY REFERENCE A-NEN "A-NEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               GO  TO  GAMEN-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-010
           END-IF
           CALL "SD_Output" USING "A-NEN" A-NEN "p" RETURNING RESU.
       GAMEN-020.
           CALL "SD_Accept" USING BY REFERENCE A-GET "A-GET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-020
           END-IF
           CALL "SD_Output" USING "A-GET" A-GET "p" RETURNING RESU.
           IF  W-GET  <  1  OR  > 12
               GO  TO  GAMEN-020
           END-IF.
      *
       GAMEN-030.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-020
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-030
           END-IF
           CALL "SD_Output" USING "A-STCD" A-STCD "p" RETURNING RESU.
       GAMEN-040.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-030
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-040
           END-IF
           CALL "SD_Output" USING "A-ETCD" A-ETCD "p" RETURNING RESU.
           IF  W-STCD  >  W-ETCD
               GO  TO  GAMEN-040
           END-IF.
      *
       GAMEN-050.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-040
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-050
           END-IF
           CALL "SD_Output" USING "A-SHCD" A-SHCD "p" RETURNING RESU.
       GAMEN-060.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-050
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-060
           END-IF
           CALL "SD_Output" USING "A-EHCD" A-EHCD "p" RETURNING RESU.
           IF  W-SHCD  >  W-EHCD
               GO  TO  GAMEN-060
           END-IF.
       GAMEN-070.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-060
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-070
           END-IF
           CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU.
           IF  W-SEN  NOT =  0 AND 1 AND 2 AND 9
               GO  TO  GAMEN-070
           END-IF.
       GAMEN-900.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-070
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-900
           END-IF
           CALL "SD_Output" USING "A-DMM" A-DMM "p" RETURNING RESU.
           IF  W-DMM  =  9
               CALL "SD_Output" USING "CAN-01" CAN-01 "p" RETURNING RESU
               CALL "SD_Output" USING "CLR-01" CLR-01 "p" RETURNING RESU
               INITIALIZE  W-DATA
               GO  TO  GAMEN-RTN
           END-IF
           IF  W-DMM  NOT =  1
               GO  TO  GAMEN-900
           END-IF.
       GAMEN-EX.
           EXIT.
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK07_IDLST JT-WK07_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
       WRI-RTN.
      *           WRITE    WK07-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK07_PNAME1 JT-WK07_LNAME WK07-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE  "W"          TO  ERR-M
               MOVE  "JT-WK07"    TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
       COPY  LPMSG.
