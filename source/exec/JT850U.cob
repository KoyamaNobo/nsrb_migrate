       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT410U.
      *********************************************************
      *    PROGRAM         :  出荷確定未処理集計表（品名別）　*
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  91/09/13                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       INPUT-OUTPUT     SECTION.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT               PIC X(2).
       77  ERR-SW                 PIC 9(1)  VALUE 0.
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  W-AREA.
           02  I                  PIC  9(02).
           02  OKC                PIC  9(01).
       01  W-FT.
           02  W-FNGP.
             03  W-FNEN              PIC 9(04).
             03  W-FNENL  REDEFINES  W-FNEN.
               04  W-FNEN1           PIC 9(02).
               04  W-FNEN2           PIC 9(02).
             03  W-FGET              PIC 9(02).
             03  W-FPEY              PIC 9(02).
           02  W-FNGPL  REDEFINES  W-FNGP.
             03  F                   PIC 9(02).
             03  W-FNGPS             PIC 9(06).
           02  W-TNGP.
             03  W-TNEN              PIC 9(04).
             03  W-TNENL  REDEFINES  W-TNEN.
               04  W-TNEN1           PIC 9(02).
               04  W-TNEN2           PIC 9(02).
             03  W-TGET              PIC 9(02).
             03  W-TPEY              PIC 9(02).
           02  W-TNGPL  REDEFINES  W-TNGP.
             03  F                   PIC 9(02).
             03  W-TNGPS             PIC 9(06).
           02  W-FHCD                PIC 9(06).
           02  W-THCD                PIC 9(06).
           02  W-SEN                 PIC 9(01).
           02  W-DC                  PIC 9(01).
           02  W-16                  PIC 9(01).
      ***
       COPY  LWMSG.
      ***
           COPY   LIBFDD.
           COPY   LTWK03.
           COPY   L-JSTR.
      ***
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
      ***
       01  DSP-AREA.
           02  FILLER  PIC  X(30) VALUE
               " 出荷確定未処理集計表(品名別) ".
           02  FILLER.
               03  FILLER  PIC  X(04) VALUE  "日付".
               03  FILLER  PIC  X(04) VALUE  "品名".
               03  FILLER  PIC  X(01) VALUE    "0".
               03  FILLER  PIC  X(06) VALUE  "教　育".
           02  FILLER.
               03  FILLER  PIC  X(08) VALUE  "ＦＲＯＭ".
               03  FILLER  PIC  X(08) VALUE  "  /  /  ".
               03  FILLER  PIC  X(01) VALUE    "1".
               03  FILLER  PIC  X(06) VALUE  "一　般".
           02  FILLER.
               03  FILLER  PIC  X(04) VALUE  "ＴＯ".
               03  FILLER  PIC  X(08) VALUE  "  /  /  ".
               03  FILLER  PIC  X(01) VALUE    "9".
               03  FILLER  PIC  X(06) VALUE  "全　件".
               03  FILLER  PIC  X(04) VALUE  "選択".
               03  FILLER  PIC  X(03) VALUE    "[ ]".
           02  FILLER  PIC  X(43) VALUE
                "得意先別 = 1  ,  得意先･直送先別 = 2  ...  ".
           02  FILLER  PIC  X(25) VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  DSP-AREA1.
           02  FILLER.
             03  DSP-FNEN   PIC 9(2).
             03  DSP-FGET   PIC Z9 .
             03  DSP-FPEY   PIC Z9 .
             03  DSP-FHCD   PIC 9(06).
           02  FILLER.
             03  DSP-TNEN   PIC 9(2).
             03  DSP-TGET   PIC Z9 .
             03  DSP-TPEY   PIC Z9 .
             03  DSP-THCD   PIC 9(06).
       01  ACP-AREA.
           02  FILLER.
             03  ACP-FNEN   PIC 9(02).
             03  ACP-FGET   PIC 9(02).
             03  ACP-FPEY   PIC 9(02).
             03  ACP-FHCD   PIC 9(06).
           02  FILLER.
             03  ACP-TNEN   PIC 9(02).
             03  ACP-TGET   PIC 9(02).
             03  ACP-TPEY   PIC 9(02).
             03  ACP-THCD   PIC 9(06).
             03  ACP-SEN    PIC 9(01).
           02  ACP-DC      PIC 9(01).
           02  ACP-OKC     PIC 9(01).
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
            "CLR-01" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "24" "60" "1" " " "CLR-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "162" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "RX" "1" "17" "30" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" " " "8" "0" "15" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102DSP-AREA" "X" "8" "27" "4" " " "02DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202DSP-AREA" "X" "8" "37" "4" "0102DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302DSP-AREA" "X" "8" "47" "1" "0202DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402DSP-AREA" "X" "8" "49" "6" "0302DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" " " "10" "0" "23" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103DSP-AREA" "X" "10" "15" "8" " " "03DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203DSP-AREA" "X" "10" "25" "8" "0103DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0303DSP-AREA" "X" "10" "47" "1" "0203DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0403DSP-AREA" "X" "10" "49" "6" "0303DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" " " "12" "0" "26" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104DSP-AREA" "X" "12" "15" "4" " " "04DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204DSP-AREA" "X" "12" "25" "8" "0104DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0304DSP-AREA" "X" "12" "47" "1" "0204DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0404DSP-AREA" "X" "12" "49" "6" "0304DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0504DSP-AREA" "X" "12" "58" "4" "0404DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0604DSP-AREA" "X" "12" "62" "3" "0504DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "16" "15" "43" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "24" "41" "25" "05DSP-AREA" " "
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA1" " " "10" "0" "12" " " "DSP-AREA1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FNEN" "9" "10" "25" "2" " " "01DSP-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FNEN" BY REFERENCE W-FNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FGET" "Z9" "10" "28" "2" "DSP-FNEN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FPEY" "Z9" "10" "31" "2" "DSP-FGET" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FHCD" "9" "10" "36" "6" "DSP-FPEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FHCD" BY REFERENCE W-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA1" " " "12" "0" "12" "01DSP-AREA1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TNEN" "9" "12" "25" "2" " " "02DSP-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TNEN" BY REFERENCE W-TNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TGET" "Z9" "12" "28" "2" "DSP-TNEN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TPEY" "Z9" "12" "31" "2" "DSP-TGET" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-THCD" "9" "12" "36" "6" "DSP-TPEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-THCD" BY REFERENCE W-THCD "6" "0" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "27" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
           "01ACP-AREA" " " "10" "0" "12" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-FNEN" "9" "10" "25" "2" " " "01ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FNEN" BY REFERENCE W-FNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FGET" "9" "10" "28" "2" "ACP-FNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FPEY" "9" "10" "31" "2" "ACP-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FHCD" "9" "10" "36" "6" "ACP-FPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FHCD" BY REFERENCE W-FHCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02ACP-AREA" " " "12" "0" "13" "01ACP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
           "ACP-TNEN" "9" "12" "25" "2" " " "02ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TNEN" BY REFERENCE W-TNEN2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TGET" "9" "12" "28" "2" "ACP-TNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TPEY" "9" "12" "31" "2" "ACP-TGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-THCD" "9" "12" "36" "6" "ACP-TPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-THCD" BY REFERENCE W-THCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-SEN" "9" "12" "63" "1" "ACP-THCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DC" "9" "16" "57" "1" "02ACP-AREA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-DC" BY REFERENCE W-DC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "60" "1" "ACP-DC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
      ****************************
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           PERFORM  GAMEN-RTN   THRU   GAMEN-EX.
           PERFORM  RED-RTN     THRU   RED-EX.
           PERFORM  END-RTN     THRU   END-EX.
           IF  COMPLETION_CODE  =  000
               IF  W-DC             =  1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  001
               ELSE
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  002
               END-IF
           END-IF
           CALL "DB_Close".
           STOP  RUN.
      *
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INI-RTN.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           COPY  LIBCPR.
       INI-EX.
            EXIT.
      *
      ******************************
      ***   R E D   R T N        ***
      ******************************
      **
       RED-RTN.
       RED-010.
      ***  出荷指図ファイル　ＲＥＡＤ
           IF  ERR-SW         =   1
               GO  TO  RED-EX
           END-IF
      *           READ  JSTR NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  RED-EX
           END-IF
           IF  JSTR-03   NOT   =   0  AND  3  AND  7
               GO  TO  RED-010
           END-IF
           IF  JSTR-17         =   1
               GO  TO  RED-010
           END-IF
           IF  JSTR-04    <   W-FNGP  OR  >   W-TNGP
               GO  TO  RED-010
           END-IF
           IF  JSTR-09    <   W-FHCD  OR  >   W-THCD
               GO  TO  RED-010
           END-IF
           IF  (JSTR-1111(1)  =  0)  AND  (JSTR-1111(2)  =  0)  AND
               (JSTR-1111(3)  =  0)  AND  (JSTR-1111(4)  =  0)  AND
               (JSTR-1111(5)  =  0)  AND  (JSTR-1111(6)  =  0)  AND
               (JSTR-1111(7)  =  0)  AND  (JSTR-1111(8)  =  0)  AND
               (JSTR-1111(9)  =  0)  AND  (JSTR-1111(10)  =  0)
               GO  TO  RED-010
           END-IF
           MOVE  JSTR-16        TO  W-16.
           IF  W-16            =  2
               MOVE  1              TO  W-16
           END-IF
           IF  W-SEN      NOT  =  9
               IF  W-16       NOT  =  W-SEN
                   GO  TO  RED-010
               END-IF
           END-IF
           PERFORM  WRI-RTN     THRU  WRI-EX.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  000.
           GO  TO  RED-010.
       RED-EX.
           EXIT.
      *
      *******************************
      ***   G A M E N   R T N     ***
      *******************************
      **
       GAMEN-RTN.
       GAMEN-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FNEN "ACP-FNEN"
            "9" "2" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP    RUN
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-010
           END-IF
           CALL "SD_Output" USING
            "DSP-FNEN" DSP-FNEN "p" RETURNING RESU.
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
            "DSP-FGET" DSP-FGET "p" RETURNING RESU.
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
            "DSP-FPEY" DSP-FPEY "p" RETURNING RESU.
           IF  W-FNGPS  =   ZERO
               MOVE  ZERO      TO  W-FNGP
               GO  TO  GAMEN-040
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
            "DSP-TNEN" DSP-TNEN "p" RETURNING RESU.
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
            "DSP-TGET" DSP-TGET "p" RETURNING RESU.
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
            "DSP-TPEY" DSP-TPEY "p" RETURNING RESU.
           IF  W-TNGPS  =    999999
               MOVE  99999999  TO  W-TNGP
               GO  TO  GAMEN-070
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
           IF  W-FNGP   >  W-TNGP
               GO  TO  GAMEN-040
           END-IF.
       GAMEN-070.
           CALL "SD_Accept" USING BY REFERENCE ACP-FHCD "ACP-FHCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-060
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-070
           END-IF
           CALL "SD_Output" USING
            "DSP-FHCD" DSP-FHCD "p" RETURNING RESU.
       GAMEN-080.
           CALL "SD_Accept" USING BY REFERENCE ACP-THCD "ACP-THCD"
            "9" "6" BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-070
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-080
           END-IF
           CALL "SD_Output" USING
            "DSP-THCD" DSP-THCD "p" RETURNING RESU.
       GAMEN-090.
           CALL "SD_Accept" USING BY REFERENCE ACP-SEN "ACP-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-080
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-090
           END-IF
           IF  W-SEN  NOT  =  0 AND 1 AND 9
               GO  TO  GAMEN-090
           END-IF.
       GAMEN-100.
           CALL "SD_Accept" USING BY REFERENCE ACP-DC "ACP-DC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-090
           END-IF
           IF  ESTAT  NOT  =  "01" AND "06" AND "00"
               GO  TO  GAMEN-100
           END-IF
           IF  W-DC   NOT  =  1 AND 2
               GO  TO  GAMEN-100
           END-IF.
       GAMEN-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-100
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
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK03_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JSTR_PNAME1 "SHARED" BY REFERENCE JSTR_IDLST "1"
            "JSTR-KEY" BY REFERENCE JSTR-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JT-WK03_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-WK03_IDLST "0".
       GAMEN-EX.
           EXIT.
      *
      ***************************
      ***   W R I   R T N     ***
      ***************************
      **
       WRI-RTN.
           MOVE  SPACE     TO  W03-R.
           INITIALIZE          W03-R.
           MOVE  JSTR-R    TO  W03-R.
      *           WRITE    W03-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-WK03_PNAME1 JT-WK03_LNAME W03-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  1            TO  ERR-SW
               MOVE  "W"          TO  ERR-M
               MOVE  "JT-WK03"    TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
      *
      **************************
      ***  ﾌ ｧ ｲ ﾙ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JSTR_IDLST JSTR_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
