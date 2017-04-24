       IDENTIFICATION  DIVISION.
      ***  仕訳データ生成******************************
      *    BASE   :  ZA0104
      *    AUTHOR :  MAYUMI.I.
       PROGRAM-ID.     PR215U.
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    NEAC-SYSTEM3100.
       OBJECT-COMPUTER.    NEAC-SYSTEM3100.
       INPUT-OUTPUT        SECTION.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       77  IN-WK               PIC X(01).
       77  IN-SW               PIC X(3)    VALUE "OFF".
       77  SD-STATUS           PIC X(2).
       01  W-KAKU              PIC X(01).
       01  W1.
           02  I               PIC 9(2).
           02  J               PIC 9(2).
       01  W2.
           02  W2-ITEM         OCCURS  20.
             03  W2-KEY.
               04  W2-KEY1.
                 05  W2-YMD    PIC 9(8).
                 05  W2-DEN    PIC 9(6).
               04  W2-LIN      PIC 9(2).
             03  W2-KRCD.
               04  W2-KRCDM    PIC 9(4).
               04  W2-KRCDS    PIC 9(4).
             03  W2-KRSEC      PIC 9(4).
             03  W2-KRSKN      PIC 9(3).
             03  W2-KRTAX      PIC X(1).
             03  W2-KRKIN      PIC S9(10).
             03  W2-KRTB       PIC 9(2).
             03  W2-KSCD.
               04  W2-KSCDM    PIC 9(4).
               04  W2-KSCDS    PIC 9(4).
             03  W2-KSSEC      PIC 9(4).
             03  W2-KSSKN      PIC 9(3).
             03  W2-KSTAX      PIC X(1).
             03  W2-KSKIN      PIC S9(10).
             03  W2-KSTB       PIC 9(2).
             03  W2-CUST       PIC 9(5).
             03  W2-TEKICD     PIC 9(3).
             03  W2-TEKI       PIC N(20).
             03  W2-SIN        PIC X(1).
             03  W2-NAMEN      PIC N(10).
             03  W2-FILLER1    PIC X(10).
             03  W2-ETAX       PIC X(1).
             03  W2-FILLER2    PIC X(17).
             03  W2-DEL        PIC X(1).
       01  W3.
           02  W3-NKEY.
             03  W3-NKEY1.
               04  W3-YMD      PIC 9(8).
               04  W3-DEN      PIC 9(6).
             03  W3-LIN        PIC 9(2).
           02  W3-KAMOKU.
             03  W3-DRCNT      PIC 9(2).
             03  W3-CRCNT      PIC 9(2).
             03  W3-DRCD       PIC X(4).
             03  W3-CRCD       PIC X(4).
             03  W3-DRKIN      PIC S9(10).
             03  W3-CRKIN      PIC S9(10).
       01  ERR-STAT            PIC X(02).
      ***  科目マスタ          (256/1)
       COPY    ACCUNT.
      ***  仕訳インプット      (170/3)
       COPY    SIWAID.
      ***  仕訳データ          (128/2)
       COPY    SIWAKE.
       77  USER_ID             PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE     PIC  X(003) VALUE ZERO.
       77  ESTAT               PIC  X(002).
       77  RESU                PIC  9(001).
       77  RET                 PIC  9(001) VALUE ZERO.
      *
       01  DSP-CLR.
           02  FILLER          PIC  X(12)  VALUE "CLEAR SCREEN".
       01  DISP-BUZZER.
           02  DISP-BUZ-J      PIC  X(05) VALUE X"1B4A02".
           02  DISP-BUZ-B      PIC  X(05) VALUE X"1B4202".
       01  DSP-AREA.
           02  DSP-010         PIC  X(13) VALUE  "ｶﾓｸ ｺｰﾄﾞ ﾆ ﾅｼ".
           02  DSP-020         PIC  X(16) VALUE  "ｼﾜｹﾃﾞｰﾀ ｵｰﾊﾞｰﾌﾛｰ".
           02  DSP-030         PIC  9(04).
           02  DSP-040         PIC  9(04).
           02  DSP-100         PIC  N(05) VALUE  "未作表有り".
       01  DSP-AREA2.
           02  FILLER          PIC N(007) VALUE  "マスタ更新　①".
           02  FILLER          PIC X(018) VALUE  "確認 OK=1,NO=9 ( )".
       01  ACP-AREA.
           02  ACP-010         PIC  X(03).
           02  ACP-020         PIC  X(01).
       01  ACP-AREA1.
           02  ACP-01          PIC  X(01).
      *
       PROCEDURE       DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "SD_Initialize" RETURNING RESU.
      *DSP-CLR
       CALL "SD_Init" USING
            "DSP-CLR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-CLR" "X" "1" "0" "12" " " "DSP-CLR" RETURNING RESU.
      *DISP-BUZZER
       CALL "SD_Init" USING
            "DISP-BUZZER" " " "24" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-J" "X" "24" "80" "5" " " "DISP-BUZZER"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-BUZ-B" "X" "24" "80" "5" "DISP-BUZ-J" " "
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING
            "DSP-AREA" " " "12" "0" "47" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-010" "X" "12" "5" "13" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-020" "X" "12" "5" "16" "DSP-010" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-030" "9" "12" "30" "4" "DSP-020" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-030" BY REFERENCE W2-KRCDM(1) "4" "1"
            BY REFERENCE I 170 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-040" "9" "12" "30" "4" "DSP-030" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-040" BY REFERENCE W2-KSCDM(1) "4" "1"
            BY REFERENCE I 170 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-100" "N" "12" "5" "10" "DSP-040" " " RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING
            "DSP-AREA2" " " "0" "0" "32" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01DSP-AREA2" "RN" "1" "35" "14" " " "DSP-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING
            "02DSP-AREA2" "N" "24" "61" "18" "01DSP-AREA2" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "12" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-010" "X" "12" "50" "3" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-010" BY REFERENCE IN-SW "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-020" "X" "12" "30" "1" "ACP-010" " " RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-020" BY REFERENCE IN-WK "1" "0" RETURNING RESU.
      *ACP-AREA1
       CALL "SD_Init" USING
            "ACP-AREA1" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-01" "X" "24" "77" "1" " " "ACP-AREA1" RETURNING RESU.
       CALL "SD_Into" USING
            "ACP-01" BY REFERENCE W-KAKU "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       ST.
           CALL "SD_Output" USING "DSP-CLR" DSP-CLR "p"
                                  RETURNING RESU.
           CALL "SD_Output" USING "DSP-AREA2" DSP-AREA2 "p"
                                  RETURNING RESU.
       ST-00.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-01 "ACP-01" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  ST-999
           END-IF.
           IF  ESTAT NOT = "01"
               GO  TO  ST-00
           END-IF.
           IF  W-KAKU NOT = "1" AND "9"
               GO  TO  ST-00
           END-IF.
           IF  W-KAKU = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO  TO  ST-999
           END-IF.
           PERFORM  CHK-RTN      THRU   CHK-EX.
           IF  COMPLETION_CODE  =  255
               GO  TO  ST-999
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 " " BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDI_PNAME1 " " BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" SSD_PNAME1 " " BY REFERENCE SSD_IDLST "0".
       ST-10.
           MOVE       ZERO         TO     W2   W3.
           MOVE       0            TO     I.
       ST-20.
      *           READ       SDI          AT     END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SDI_PNAME1 BY REFERENCE SDI-REC " " RETURNING RET.
           IF  RET = 1
               PERFORM  WRITE-RTN    THRU   WRITE-EXT
               GO TO    ST-END
           END-IF.
           MOVE       SDI-KEY      TO     W3-NKEY.
           IF  I            =      0
               GO TO ST-30
           END-IF.
           IF  W3-NKEY1     =      W2-KEY1 (I)
               GO TO ST-30
           END-IF.
           PERFORM    WRITE-RTN    THRU   WRITE-EXT.
       ST-30.
           ADD        1            TO     I.
           MOVE       SDI-REC      TO     W2-ITEM (I).
           IF  KRCDM        =      ZERO
               GO TO ST-50
           END-IF.
           IF  W3-DRCD      =      ZERO
               GO TO ST-40
           END-IF.
           IF  KRCDM        =      W3-DRCD
               GO TO ST-50
           END-IF.
       ST-40.
           MOVE       KRCDM        TO     W3-DRCD.
           ADD        1            TO     W3-DRCNT.
       ST-50.
           IF  KSCDM        =      ZERO
               GO TO ST-70
           END-IF.
           IF  W3-CRCD      =      ZERO
               GO TO ST-60
           END-IF.
           IF  KSCDM        =      W3-CRCD
               GO TO ST-70
           END-IF.
       ST-60.
           MOVE       KSCDM        TO     W3-CRCD.
           ADD        1            TO     W3-CRCNT.
       ST-70.
           IF  I            =      20
               PERFORM  WRITE-RTN    THRU   WRITE-EXT
               GO TO    ST-20
           END-IF.
           ADD        W2-KRKIN (I) TO     W3-DRKIN.
           ADD        W2-KSKIN (I) TO     W3-CRKIN.
           IF  W3-DRKIN     =      W3-CRKIN
               PERFORM  WRITE-RTN    THRU   WRITE-EXT
           END-IF.
           GO TO      ST-20.
       ST-END.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
       ST-999.
           CALL "DB_Close".
           STOP       RUN.
      *
       CHK-RTN.
           CALL "DB_F_Open" USING
            "INPUT" SDI_PNAME1 " " BY REFERENCE SDI_IDLST "1"
            "SDI-KEY" BY REFERENCE SDI-KEY.
       CHK-10.
      *           READ       SDI          AT     END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SDI_PNAME1 BY REFERENCE SDI-REC " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-20
           END-IF.
           IF  SDISIN  NOT  =      SPACE
               GO TO CHK-10
           END-IF.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 255.
           CALL "SD_Output" USING "DSP-100" DSP-100 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                         RETURNING RESU.
       CHK-20.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
       CHK-EX.
           EXIT.
      *
       WRITE-RTN.
           MOVE       1            TO     I.
       WR-05.
           IF  W2-KRCD  (I) =      ZERO
               GO TO WR-10
           END-IF.
           MOVE       SPACE        TO     SD-REC.
           MOVE       W2-YMD   (I) TO     TRDATE.
           MOVE       W2-DEN   (I) TO     JUNLNO.
           MOVE       W2-LIN   (I) TO     LINENO.
           MOVE       W2-KRCDM (I) TO     ACCNTCD.
           MOVE       W2-KRCDS (I) TO     HOACCNT.
           MOVE       W2-KRSEC (I) TO     SECTCD.
           MOVE       W2-KRSKN (I) TO     SKINCD.
           MOVE       W2-KRTAX (I) TO     TAXKB.
           MOVE       W2-KRKIN (I) TO     AMOUNT.
           MOVE       W2-KRTB  (I) TO     TEG-BAN  OF  SD-REC.
           IF  W2-CUST(I)   >   09999   AND  <   30000
               MOVE       99999        TO     CUSTCD
           ELSE
               MOVE       W2-CUST  (I) TO     CUSTCD
           END-IF.
           MOVE       W2-TEKICD(I) TO     TEKICD.
           MOVE       W2-TEKI  (I) TO     TEKIYO.
           MOVE       W2-NAMEN (I) TO     NAMEN.
           MOVE       W2-ETAX  (I) TO     ETAX.
           MOVE       W2-DEL   (I) TO     DELKB.
           MOVE       ZERO         TO     KACD2.
           IF  W3-CRCNT     =      1
               MOVE     W3-CRCD      TO     KACD2
               MOVE     ZERO         TO     HOOPPCD
           END-IF.
           MOVE       W2-KRCDM (I) TO     AM-KEY.
      *           READ       AM           INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "DSP-010" DSP-010 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DSP-030" DSP-030 "p"
                                               RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-010 "ACP-010" "X" "3"
                   BY REFERENCE ESTAT RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE       1                   TO  DR-CR  OF  SD-REC.
           MOVE       KEIHI        TO     KEIHIKB.
      *           WRITE      SD-REC.
      *///////////////
           CALL "DB_Insert" USING
            SSD_PNAME1 SSD_LNAME SD-REC RETURNING RET.
           IF  SD-STATUS    =      34
               GO TO WR-90
           END-IF.
       WR-10.
           IF  W2-KSCD  (I) =      ZERO
               GO TO WR-20
           END-IF.
           MOVE       SPACE        TO     SD-REC.
           MOVE       W2-YMD   (I) TO     TRDATE.
           MOVE       W2-DEN   (I) TO     JUNLNO.
           MOVE       W2-LIN   (I) TO     LINENO.
           MOVE       W2-KSCDM (I) TO     ACCNTCD.
           MOVE       W2-KSCDS (I) TO     HOACCNT.
           MOVE       W2-KSSEC (I) TO     SECTCD.
           MOVE       W2-KSSKN (I) TO     SKINCD.
           MOVE       W2-KSTAX (I) TO     TAXKB.
           MOVE       W2-KSKIN (I) TO     AMOUNT.
           MOVE       W2-KSTB  (I) TO     TEG-BAN  OF  SD-REC.
           IF  W2-CUST(I)   >   09999   AND  <   30000
               MOVE       99999        TO     CUSTCD
           ELSE
               MOVE       W2-CUST  (I) TO     CUSTCD
           END-IF.
           MOVE       W2-TEKICD(I) TO     TEKICD.
           MOVE       W2-TEKI  (I) TO     TEKIYO.
           MOVE       W2-NAMEN (I) TO     NAMEN.
           MOVE       W2-ETAX  (I) TO     ETAX.
           MOVE       W2-DEL   (I) TO     DELKB.
           MOVE       ZERO         TO     KACD2.
           IF  W3-DRCNT     =      1
               MOVE     W3-DRCD      TO     KACD2
               MOVE     ZERO         TO     HOOPPCD
           END-IF.
           MOVE       W2-KSCDM (I) TO     AM-KEY.
      *           READ   AM           INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "DSP-010" DSP-010 "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                               RETURNING RESU
               CALL "SD_Output" USING "DSP-040" DSP-040 "p"
                                               RETURNING RESU
               CALL "SD_Accept" USING
                   BY REFERENCE ACP-010 "ACP-010" "X" "3"
                   BY REFERENCE ESTAT RETURNING RESU
               CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP     RUN
           END-IF.
           MOVE       2                   TO  DR-CR  OF  SD-REC.
           MOVE       KEIHI        TO     KEIHIKB.
      *           WRITE      SD-REC.
      *///////////////
           CALL "DB_Insert" USING
            SSD_PNAME1 SSD_LNAME SD-REC RETURNING RET.
           IF  SD-STATUS    =      34
               GO TO WR-90
           END-IF.
       WR-20.
           IF  I            <      20
               ADD      1            TO     I
               GO TO    WR-05
           END-IF.
           MOVE       ZERO         TO     W2  W3.
           MOVE       0            TO     I.
           GO TO      WRITE-EXT.
       WR-90.
           CALL "SD_Output" USING "DSP-020" DSP-020 "p"
                                           RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-J" DISP-BUZ-J "p"
                                             RETURNING RESU.
           CALL "SD_Accept" USING
               BY REFERENCE ACP-020 "ACP-020" "X" "1"
               BY REFERENCE ESTAT RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SSD_IDLST SSD_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDI_IDLST SDI_PNAME1.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE 255.
           CALL "DB_Close".
           STOP       RUN.
       WRITE-EXT.     EXIT.
