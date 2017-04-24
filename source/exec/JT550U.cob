       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT550U.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  出荷確認未処理リスト抽出　　　　*
      *                    : (送り状未更新リスト抽出) 　　　　*
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  91/09/13                        *
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
       01  W-16                   PIC 9(01).
       01  W-JSP                  PIC 9(01).
       01  W-JS-MEI               PIC N(03).
       01  W-AREA.
           02  I                  PIC  9(02).
           02  W-FD.
               03  W-FD1          PIC  9(04).
               03  W-FD1L  REDEFINES  W-FD1.
                   04  W-FD11     PIC  9(02).
                   04  W-FD12     PIC  9(02).
               03  W-FD2          PIC  9(02).
               03  W-FD3          PIC  9(02).
           02  W-FDL   REDEFINES  W-FD.
               03  F              PIC  9(02).
               03  W-FDS          PIC  9(06).
           02  W-TD.
               03  W-TD1          PIC  9(04).
               03  W-TD1L  REDEFINES  W-TD1.
                   04  W-TD11     PIC  9(02).
                   04  W-TD12     PIC  9(02).
               03  W-TD2          PIC  9(02).
               03  W-TD3          PIC  9(02).
           02  W-TDL   REDEFINES  W-TD.
               03  F              PIC  9(02).
               03  W-TDS          PIC  9(06).
           02  OKC                PIC  9(01).
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
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(02) VALUE "  ".
           02  FILLER  PIC  X(01) VALUE " ".
      ***
       01  DSP-AREAA.
           02  FILLER  PIC  X(26) VALUE
               " 出荷確定未処理リスト抽出 ".
       01  DSP-AREAB.
           02  FILLER  PIC  X(24) VALUE
               " 送り状未更新リスト抽出 ".
       01  DSP-AREA.
           02  FILLER.
               03  FILLER  PIC  X(25) VALUE "教　育=0 , 一　般=1 ...  ".
           02  FILLER  PIC  X(08) VALUE  "出 荷 日".
           02  FILLER  PIC  X(08) VALUE  "ＦＲＯＭ".
           02  FILLER  PIC  X(04) VALUE  "ＴＯ".
           02  FILLER  PIC  X(01) VALUE  "/".
           02  FILLER  PIC  X(01) VALUE  "/".
           02  FILLER  PIC  X(01) VALUE  "/".
           02  FILLER  PIC  X(01) VALUE  "/".
           02  FILLER  PIC  X(25) VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-JS      PIC 9(01).
           02  ACP-FD1     PIC 9(02).
           02  ACP-FD2     PIC 9(02).
           02  ACP-FD3     PIC 9(02).
           02  ACP-TD1     PIC 9(02).
           02  ACP-TD2     PIC 9(02).
           02  ACP-TD3     PIC 9(02).
           02  ACP-OKC     PIC 9(01).
       01  DSP-AREA1.
           02  DSP-FD1     PIC 9(2).
           02  DSP-FD2     PIC Z9  .
           02  DSP-FD3     PIC Z9  .
           02  DSP-TD1     PIC 9(2).
           02  DSP-TD2     PIC Z9  .
           02  DSP-TD3     PIC Z9  .
      *
       01  DSP-ERR.
           02  ERR-1       PIC  X(22) VALUE
                           "ＪＳ－ＳＩＧＮ　エラー".
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
            "CLR-01" " " "0" "0" "13" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01CLR-01" "X" "6" "27" "2" " " "CLR-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02CLR-01" "X" "6" "30" "2" "01CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03CLR-01" "X" "6" "33" "2" "02CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04CLR-01" "X" "8" "27" "2" "03CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05CLR-01" "X" "8" "30" "2" "04CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06CLR-01" "X" "8" "33" "2" "05CLR-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07CLR-01" "X" "24" "60" "1" "06CLR-01" " " RETURNING RESU.
      *DSP-AREAA
       CALL "SD_Init" USING 
            "DSP-AREAA" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREAA" "RX" "1" "20" "26" " " "DSP-AREAA"
            RETURNING RESU.
      *DSP-AREAB
       CALL "SD_Init" USING 
            "DSP-AREAB" " " "0" "0" "24" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREAB" "RX" "1" "20" "24" " " "DSP-AREAB"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "74" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" " " "5" "0" "25" " " "DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101DSP-AREA" "X" "5" "14" "25" " " "01DSP-AREA"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-AREA" "X" "8" "27" "8" "01DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-AREA" "X" "10" "15" "8" "02DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-AREA" "X" "12" "15" "4" "03DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05DSP-AREA" "X" "10" "29" "1" "04DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06DSP-AREA" "X" "10" "32" "1" "05DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07DSP-AREA" "X" "12" "29" "1" "06DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08DSP-AREA" "X" "12" "32" "1" "07DSP-AREA" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09DSP-AREA" "X" "24" "41" "25" "08DSP-AREA" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "14" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-JS" "9" "5" "38" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-JS" BY REFERENCE W-JS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FD1" "9" "10" "27" "2" "ACP-JS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FD1" BY REFERENCE W-FD12 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FD2" "9" "10" "30" "2" "ACP-FD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FD2" BY REFERENCE W-FD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-FD3" "9" "10" "33" "2" "ACP-FD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-FD3" BY REFERENCE W-FD3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TD1" "9" "12" "27" "2" "ACP-FD3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TD1" BY REFERENCE W-TD12 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TD2" "9" "12" "30" "2" "ACP-TD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TD2" BY REFERENCE W-TD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-TD3" "9" "12" "33" "2" "ACP-TD2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-TD3" BY REFERENCE W-TD3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "60" "1" "ACP-TD3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FD1" "9" "10" "27" "2" " " "DSP-AREA1" RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FD1" BY REFERENCE W-FD12 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FD2" "Z9" "10" "30" "2" "DSP-FD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FD2" BY REFERENCE W-FD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-FD3" "Z9" "10" "33" "2" "DSP-FD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-FD3" BY REFERENCE W-FD3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TD1" "9" "12" "27" "2" "DSP-FD3" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TD1" BY REFERENCE W-TD12 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TD2" "Z9" "12" "30" "2" "DSP-TD1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TD2" BY REFERENCE W-TD2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-TD3" "Z9" "12" "33" "2" "DSP-TD2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DSP-TD3" BY REFERENCE W-TD3 "2" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-1" "X" "24" "1" "22" " " "DSP-ERR" RETURNING RESU.
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
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           PERFORM  GAMEN-RTN   THRU   GAMEN-EX.
           PERFORM  UPD-RTN     THRU   UPD-EX.
           PERFORM  END-RTN     THRU   END-EX.
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
           ACCEPT  W-JSP FROM ARGUMENT-VALUE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  W-JSP  NOT =  0  AND  1
               CALL "SD_Output" USING
                "ERR-1" ERR-1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  W-JSP =  ZERO
               CALL "SD_Output" USING
                "DSP-AREAA" DSP-AREAA "p" RETURNING RESU
           END-IF
           IF  W-JSP =  1
               CALL "SD_Output" USING
                "DSP-AREAB" DSP-AREAB "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
           COPY  LIBCPR.
       INI-EX.
            EXIT.
      *
      ******************************
      ***   U P D   R T N        ***
      ******************************
      **
       UPD-RTN.
           MOVE  ZERO  TO  JSTR-KEY.
           IF  W-JS   =  0
               MOVE  "000000"  TO  JSTR-01
           ELSE
               MOVE  "100000"  TO  JSTR-01
           END-IF.
      *           START JSTR KEY NOT < JSTR-KEY INVALID
      *///////////////
           CALL "DB_Start" USING
            JSTR_PNAME1 "JSTR-KEY" " NOT < " JSTR-KEY RETURNING RET.
           IF  RET = 1
               GO TO UPD-EX
           END-IF.
       UPD-010.
      ***  出荷指図ファイル　ＲＥＡＤ
      *           READ  JSTR NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JSTR_PNAME1 BY REFERENCE JSTR-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD-EX
           END-IF
           IF  W-JS        =  0
               IF  JSTR-01 >  099999
                   GO  TO  UPD-EX
               END-IF
           END-IF
           IF  W-JSP       =  ZERO
               IF  JSTR-05     <  W-FD
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-JSP       =  ZERO
               IF  JSTR-05     >  W-TD
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-JSP       =  1
               IF  JSTR-04     <  W-FD
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-JSP       =  1
               IF  JSTR-04     >  W-TD
                   GO  TO  UPD-010
               END-IF
           END-IF
           MOVE  JSTR-16        TO  W-16.
           IF  W-16        =  2
               MOVE  1              TO  W-16
           END-IF
           IF  W-16    NOT =  W-JS
               GO  TO  UPD-010
           END-IF
           IF  W-JSP       =  ZERO
               IF  JSTR-17 NOT =  9 AND 8
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-JSP       =  ZERO
               IF  JSTR-05     =  ZERO
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-JSP       =  1
               IF  JSTR-17     =  1 OR 8
                   GO  TO  UPD-010
               END-IF
           END-IF
           IF  W-JSP       =  1
               IF  JSTR-17     =  9
                   IF  JSTR-05 NOT =  ZERO
                       GO  TO  UPD-010
                   END-IF
               END-IF
           END-IF
      *****
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
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-RTN
           END-IF
           IF  W-JS   NOT =  0  AND  1
               GO  TO  GAMEN-RTN
           END-IF
           CALL "SD_Output" USING "ACP-JS" ACP-JS "p" RETURNING RESU.
       GAMEN-005.
           CALL "SD_Accept" USING BY REFERENCE ACP-FD1 "ACP-FD1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE1" DISP-MSG-SPACE1 "p" RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-RTN
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-005
           END-IF
           CALL "SD_Output" USING "DSP-FD1" DSP-FD1 "p" RETURNING RESU.
       GAMEN-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-FD2 "ACP-FD2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-005
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-010
           END-IF
           CALL "SD_Output" USING "DSP-FD2" DSP-FD2 "p" RETURNING RESU.
           IF  W-FD2      =  ZERO
               IF  W-FD12      =  ZERO
                   GO  TO  GAMEN-020
               END-IF
           END-IF
           IF  ( W-FD2  <  1 )  OR  ( W-FD2  >  12 )
               GO  TO  GAMEN-010
           END-IF.
       GAMEN-020.
           CALL "SD_Accept" USING BY REFERENCE ACP-FD3 "ACP-FD3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-010
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-020
           END-IF
           CALL "SD_Output" USING "DSP-FD3" DSP-FD3 "p" RETURNING RESU.
           IF  W-FD3      =  ZERO
               IF  W-FD2       =  ZERO
                   GO  TO  GAMEN-030
               END-IF
           END-IF
           IF  ( W-FD3  <  1 )  OR  ( W-FD3  >  31 )
               GO  TO  GAMEN-020
           END-IF.
       GAMEN-030.
           CALL "SD_Accept" USING BY REFERENCE ACP-TD1 "ACP-TD1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-005
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-030
           END-IF
           CALL "SD_Output" USING "DSP-TD1" DSP-TD1 "p" RETURNING RESU.
       GAMEN-040.
           CALL "SD_Accept" USING BY REFERENCE ACP-TD2 "ACP-TD2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-030
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"  AND  "00"
               GO  TO  GAMEN-040
           END-IF
           CALL "SD_Output" USING "DSP-TD2" DSP-TD2 "p" RETURNING RESU.
           IF  W-TD2      =  99
               IF  W-TD12      =  99
                   GO  TO  GAMEN-050
               END-IF
           END-IF
           IF  ( W-TD2  <  1 )  OR  ( W-TD2  >  12 )
               GO  TO  GAMEN-040
           END-IF.
       GAMEN-050.
           CALL "SD_Accept" USING BY REFERENCE ACP-TD3 "ACP-TD3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-040
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  GAMEN-050
           END-IF
           CALL "SD_Output" USING "DSP-TD3" DSP-TD3 "p" RETURNING RESU.
           IF  W-TD3      =  99
               IF  W-TD2       =  99
                   GO  TO  GAMEN-060
               END-IF
           END-IF
           IF  ( W-TD3  <  1 )  OR  ( W-TD3  >  31 )
               GO  TO  GAMEN-050
           END-IF.
       GAMEN-060.
           MOVE  ZERO      TO  W-FD11.
           IF  W-FD       =  ZERO
               GO  TO  GAMEN-070
           END-IF
           IF  W-FD12 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-FD1
           END-IF
           IF  W-FD12 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-FD1
           END-IF.
       GAMEN-070.
           IF  99         =  W-TD12  AND  W-TD2  AND  W-TD3
               MOVE  99       TO  W-TD11
               GO  TO  GAMEN-080
           END-IF
           MOVE  ZERO      TO  W-TD11.
           IF  W-TD12 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-TD1
           END-IF
           IF  W-TD12 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-TD1
           END-IF.
       GAMEN-080.
           IF  W-FD    >  W-TD
               GO  TO  GAMEN-005
           END-IF.
       GAMEN-OKC.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "09"
               GO  TO  GAMEN-050
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
