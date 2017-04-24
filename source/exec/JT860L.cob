       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT420L.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  出荷確定未処理集計表（品名別）　*
      *    PRINTER TYPE    :  JIPS                            *
      *    BASE PROGRAM    :  JT530L                          *
      *    DATA WRITTN     :  91/09/17                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT               PIC X(2).
       77  20K                    PIC X(05)  VALUE  X"1A24212474".
       77  15K                    PIC X(05)  VALUE  X"1A24212078".
       77  W-PC1                  PIC 9(01).
       77  W-PC2                  PIC 9(01).
       77  W-DC                   PIC 9(01).
       77  OKC                    PIC 9(01).
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
      *
       01  PRN-AREA.
           02  LCNT               PIC  9(02) VALUE  90.
           02  PCNT               PIC  9(03) VALUE  ZERO.
           02  OLD-KEY1           PIC  9(06).
           02  OLD-KEY2           PIC  9(04).
           02  OLD-KEY3           PIC  9(03).
           02  W-TNA              PIC  N(26).
           02  N                  PIC  9(01).
           02  I                  PIC  9(02).
           02  GOUKEI             PIC S9(06).
           02  W-ZCD.
               03  W-ZC           PIC  9(01)  OCCURS  4.
       01  WORK-AREA.
           02  HIZUKE.
               03  HI-YY          PIC 9(02).
               03  HI-MM          PIC 9(02).
               03  HI-DD          PIC 9(02).
      *****   明細　取り込み　ワーク  ********************
       01  W-ASU.
           02  W-SUD    OCCURS   4.
               03  W-SU           PIC S9(05)  OCCURS  10.
      *****   合計　取り込み　ワーク  ********************
       01  W-AKEI.
           02  W-KEID   OCCURS   4.
               03  W-KEI          PIC S9(05)  OCCURS  10.
      **
       01  MID1.
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC X(39) VALUE SPACE.
           02  F              PIC N(25) VALUE
               "＊＊＊　　出荷確定未処理集計表（品名別）　　＊＊＊".
           02  F              PIC X(21) VALUE SPACE.
           02  F              PIC X(5) VALUE "DATE.".
           02  M-YY           PIC 9(2).
           02  F              PIC X    VALUE "/".
           02  M-MM           PIC Z9.
           02  F              PIC X    VALUE "/".
           02  M-DD           PIC Z9.
           02  F              PIC X(7) VALUE SPACE.
           02  F              PIC X(2) VALUE "P.".
           02  WPCNT          PIC ZZ9.
       01  MID2.
           02  F              PIC X(05) VALUE X"1A24212078".
           02  F              PIC X(07) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC N(08) VALUE "品　　　　　名　".
           02  F              PIC X(116) VALUE SPACE.
       01  MID3.
           02  F              PIC X(07) VALUE  SPACE.
           02  F              PIC X(05) VALUE  "ｺｰﾄﾞ ".
           02  F              PIC N(08) VALUE  "得　意　先　名　".
           02  F              PIC X(111) VALUE  SPACE.
       01  MID4.
           02  F              PIC X(11) VALUE  SPACE.
           02  M-M1           PIC X(05) VALUE  SPACE.
           02  M-M2           PIC N(08) VALUE  SPACE.
           02  F              PIC X(28) VALUE  SPACE.
           02  F              PIC X(01) VALUE  "1".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "３号".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "２号".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "１号".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "０号".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "　中".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "　大".
           02  F              PIC X(04) VALUE  SPACE.
           02  F              PIC N(02) VALUE  "特大".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "28.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "29.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "30.0".
           02  F              PIC X(08) VALUE  SPACE.
       01  MID5.
           02  F              PIC X(53) VALUE  SPACE.
           02  F              PIC X(01) VALUE  "2".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "12.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "13.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "13.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "14.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "15.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "16.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "17.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "18.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "19.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "20.0".
           02  F              PIC X(08) VALUE  SPACE.
       01  MID6.
           02  F              PIC X(53) VALUE  SPACE.
           02  F              PIC X(01) VALUE  "3".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "21.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "21.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "22.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "22.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "23.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "23.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "24.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "24.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "25.0".
           02  F              PIC X(15) VALUE  SPACE.
       01  MID7.
           02  F              PIC X(53) VALUE  SPACE.
           02  F              PIC X(01) VALUE  "4".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "24.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "24.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "25.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "25.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "26.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "26.5".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "27.0".
           02  F              PIC X(03) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "27.5".
           02  F              PIC X(16) VALUE  SPACE.
           02  F              PIC N(04) VALUE  "　合　計".
      ***
       01  P-R1.
           02  P1-01              PIC 9(06).
           02  FILLER             PIC X(01).
           02  P1-02              PIC N(24).
           02  FILLER             PIC X(92).
       01  P-R2.
           02  F                  PIC X(07).
           02  P2-01              PIC 9(04).
           02  F                  PIC X(01).
           02  P2-02              PIC N(26).
           02  F                  PIC X(84).
       01  P-R3.
           02  F                  PIC X(11).
           02  P3-01              PIC 9(04).
           02  P3-01D    REDEFINES  P3-01.
               03  F              PIC X(01).
               03  P3-011         PIC 9(03).
           02  F                  PIC X(01).
           02  P3-02              PIC N(26).
           02  F                  PIC X(01).
           02  P3-03              PIC 9(01).
           02  P3-04     OCCURS 10.
               03  P3-041         PIC ---,---.
           02  P3-05              PIC ----,--9.
       01  P-R4.
           02  F                  PIC X(09).
           02  P4-02              PIC N(24).
           02  F                  PIC X(11).
           02  P4-03              PIC 9(01).
           02  P4-04     OCCURS 10.
               03  P4-041         PIC ---,---.
           02  P4-05              PIC ----,--9.
      ***
       COPY  LWMSG.
      ***
           COPY   LTWK03.
           COPY   LITCM.
           COPY   LIHIM2.
      *FD  P-F
       77  P-R                    PIC X(250).
      ***
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
      ***
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(16) VALUE  " 品名別出荷日報 ".
           02  DSP-03.
               03  FILLER  PIC  X(06) VALUE  "確認（".
               03  FILLER  PIC  X(09) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "）".
               03  FILLER  PIC  X(04) VALUE  "ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-OKC  PIC 9(01).
      ***
       COPY  LSMSG.
      ***
       PROCEDURE   DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "37" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "16" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "20" "16" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-03" " " "24" "0" "21" "DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-03" "X" "24" "41" "6" " " "DSP-03" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-03" "X" "24" "47" "9" "01DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-03" "X" "24" "56" "2" "02DSP-03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-03" "X" "24" "64" "4" "03DSP-03" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "24" "62" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *
           COPY LSMSG_P.
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
      **
       MR-010.
      *           READ  JT-WK03  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK03_PNAME1 BY REFERENCE W03-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  MR-900
           END-IF.
       MR-020.
           MOVE  W03-09      TO  OLD-KEY1.
           MOVE  OLD-KEY1     TO  HI-MHCD HI-HCD.
      *           READ  HI2-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  HI-NAME
           END-IF
           MOVE  ZERO        TO  W-AKEI  W-PC1 W-DC.
       MR-030.
           MOVE  W03-061     TO  OLD-KEY2.
           MOVE  OLD-KEY2      TO  TC-TCD.
           MOVE  "001"         TO  TC-CCD.
      *           READ  TC-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF
           MOVE  ZERO        TO  W-PC2.
           IF  COMPLETION_CODE   =  001
               GO  TO  MR-034
           END-IF
           MOVE  TC-NAME       TO  W-TNA.
       MR-032.
           MOVE  W03-062     TO  OLD-KEY3.
           MOVE  OLD-KEY2      TO  TC-TCD.
           MOVE  OLD-KEY3      TO  TC-CCD.
      *           READ  TC-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF.
       MR-034.
           MOVE  ZERO        TO  W-ASU.
       MR-040.
           PERFORM  MEI-RTN     THRU  MEI-EX.
      *           READ  JT-WK03  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK03_PNAME1 BY REFERENCE W03-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               PERFORM  PRI-RTN        THRU  PRI-EX
               PERFORM  SHOKEI-RTN     THRU SHOKEI-EX
               GO  TO  MR-900
           END-IF
           IF  W03-09  NOT =  OLD-KEY1
               PERFORM  PRI-RTN        THRU  PRI-EX
               PERFORM  SHOKEI-RTN     THRU  SHOKEI-EX
               GO  TO  MR-020
           END-IF
           IF  W03-061 NOT =  OLD-KEY2
               PERFORM  PRI-RTN        THRU  PRI-EX
               GO  TO  MR-030
           END-IF
           IF  COMPLETION_CODE  =  002
               IF  W03-062 NOT =  OLD-KEY3
                   PERFORM  PRI-RTN        THRU  PRI-EX
                   GO  TO  MR-032
               END-IF
           END-IF
           GO  TO  MR-040.
       MR-900.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INI-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       INI-010.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT        =  "P9"
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT   NOT  =  "01"
               GO   TO   INI-010
           END-IF
           IF  OKC          =  9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  OKC     NOT  =  1
               GO   TO   INI-010
           END-IF
      *
           CALL  "CBLSTNNO"  USING  STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK03_PNAME1.
      *
           CALL "DB_F_Open" USING
            "INPUT" JT-WK03_PNAME1 "SHARED" BY REFERENCE
            JT-WK03_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT   HIZUKE  FROM  DATE.
           MOVE  HI-YY     TO  M-YY.
           MOVE  HI-MM     TO  M-MM.
           MOVE  HI-DD     TO  M-DD.
           IF  COMPLETION_CODE    =  001
               MOVE  "ｺｰﾄﾞ "                TO  M-M1
               MOVE  "得　意　先　名　"   TO  M-M2
           ELSE
               MOVE  " ｺｰﾄﾞ"                TO  M-M1
               MOVE  "直　送　先　名　"   TO  M-M2
           END-IF.
       INI-EX.
            EXIT.
      *
      ***************************
      ***  H I M -  R T N     ***
      ***************************
       HIM-RTN.
           IF  LCNT  NOT <  59
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF
           MOVE  SPACE        TO  P-R1.
           MOVE  OLD-KEY1     TO  P1-01.
           MOVE  HI-NAME      TO  P1-02.
      *
           MOVE  SPACE     TO  P-R.
           MOVE  P-R1      TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE     TO  P-R.
           ADD  1      TO  LCNT.
      *SHM-EX.
       HIM-EX.
           EXIT.
      ***************************
      ***  T C M -  R T N     ***
      ***************************
       TCM-RTN.
           IF  LCNT  NOT <  60
               PERFORM  MID-RTN     THRU  MID-EX
               PERFORM  HIM-RTN     THRU  HIM-EX
           END-IF
           MOVE  SPACE        TO  P-R2.
           MOVE  OLD-KEY2     TO  P2-01.
           MOVE  W-TNA        TO  P2-02.
      *
           MOVE  SPACE     TO  P-R.
           MOVE  P-R2      TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE     TO  P-R.
           ADD  1      TO  LCNT.
       TCM-EX.
           EXIT.
      *
      *****************************
      ***    M E I   R T N      ***
      *****************************
       MEI-RTN.
           MOVE  1            TO  I.
       MEI-010.
           IF  I  >  10
               GO  TO  MEI-EX
           END-IF
           MOVE  W03-1111(I)  TO  W-SU(W03-10,I).
           ADD   1            TO  I.
           GO  TO  MEI-010.
       MEI-EX.
           EXIT.
      *
      ********************************
      ***  P R I N T     R T N     ***
      ********************************
       PRI-RTN.
           IF  W-ASU     =  ZERO
               GO  TO  PRI-EX
           END-IF
           IF  W-PC1     =  ZERO
               MOVE  5            TO  W-PC1
               PERFORM   HIM-RTN    THRU  HIM-EX
           END-IF
           MOVE  ZERO         TO  GOUKEI.
           MOVE  1111         TO  W-ZCD.
           MOVE  0            TO  N.
       PRI-010.
           IF  W-ZCD     =  ZERO
               GO  TO  PRI-EX
           END-IF
           ADD  1             TO  N.
           IF  N     =  5
               GO  TO  PRI-015
           END-IF
           IF  ZERO = W-SU(N,1) AND W-SU(N,2) AND W-SU(N,3) AND
                     W-SU(N,4) AND W-SU(N,5) AND W-SU(N,6) AND
               W-SU(N,7) AND W-SU(N,8) AND W-SU(N,9) AND W-SU(N,10)
               MOVE  0            TO  W-ZC(N)
           END-IF
           IF  W-ZC(N)  =  1
               COMPUTE GOUKEI = W-SU(N,1) + W-SU(N,2) + W-SU(N,3) +
                    W-SU(N,4) + W-SU(N,5) + W-SU(N,6) + W-SU(N,7) +
                    W-SU(N,8) + W-SU(N,9) + W-SU(N,10) + GOUKEI
           END-IF
           GO  TO  PRI-010.
       PRI-015.
           IF  COMPLETION_CODE    =  002
               IF  W-PC2     =  ZERO
                   MOVE  5            TO  W-PC2
                   PERFORM   TCM-RTN    THRU  TCM-EX
               END-IF
           END-IF
           MOVE  0            TO  N.
       PRI-020.
           ADD  1             TO  N.
           IF  N     =  5
               GO  TO  PRI-090
           END-IF
           IF  W-ZC(N) = 0
               GO  TO  PRI-020
           END-IF
           MOVE  SPACE        TO  P-R3.
           MOVE  SPACE        TO  P3-02.
           IF  N     =  1
               MOVE  TC-NAME      TO  P3-02
               IF  COMPLETION_CODE    =  001
                   MOVE  OLD-KEY2     TO  P3-01
               ELSE
                   MOVE  OLD-KEY3     TO  P3-011
               END-IF
           END-IF
           IF  N     =  2
               IF  0      =  W-ZC(1)
                   MOVE  TC-NAME      TO  P3-02
                   IF  COMPLETION_CODE    =  001
                       MOVE  OLD-KEY2     TO  P3-01
                   ELSE
                       MOVE  OLD-KEY3     TO  P3-011
                   END-IF
               END-IF
           END-IF
           IF  N     =  3
               IF  0      =  W-ZC(1)  AND  W-ZC(2)
                   MOVE  TC-NAME      TO  P3-02
                   IF  COMPLETION_CODE    =  001
                       MOVE  OLD-KEY2     TO  P3-01
                   ELSE
                       MOVE  OLD-KEY3     TO  P3-011
                   END-IF
               END-IF
           END-IF
           IF  N     =  4
               IF  0      =  W-ZC(1)  AND  W-ZC(2)  AND  W-ZC(3)
                   MOVE  TC-NAME      TO  P3-02
                   IF  COMPLETION_CODE    =  001
                       MOVE  OLD-KEY2     TO  P3-01
                   ELSE
                       MOVE  OLD-KEY3     TO  P3-011
                   END-IF
               END-IF
           END-IF
           MOVE  N            TO  P3-03.
           MOVE  ZERO         TO  I.
       PRI-030.
           ADD  1             TO  I.
           IF  I     NOT  =  11
               MOVE  W-SU(N,I)   TO  P3-041(I)
               ADD   W-SU(N,I)   TO  W-KEI(N,I)
               GO  TO  PRI-030
           END-IF
           IF  N     =  4
               MOVE  GOUKEI  TO  P3-05
           END-IF
           IF  N     =  3
               IF  0      =  W-ZC(4)
                   MOVE  GOUKEI  TO  P3-05
               END-IF
           END-IF
           IF  N     =  2
               IF  0      =  W-ZC(4)  AND  W-ZC(3)
                   MOVE  GOUKEI  TO  P3-05
               END-IF
           END-IF
           IF  N     =  1
               IF  0      =  W-ZC(4)  AND  W-ZC(3)  AND  W-ZC(2)
                   MOVE  GOUKEI  TO  P3-05
               END-IF
           END-IF
           IF  LCNT  NOT <  61
               PERFORM  MID-RTN     THRU  MID-EX
               PERFORM  HIM-RTN      THRU  HIM-EX
               MOVE  TC-NAME      TO  P3-02
               IF  COMPLETION_CODE    =  001
                   MOVE  OLD-KEY2     TO  P3-01
               ELSE
                   MOVE  OLD-KEY3     TO  P3-011
                   PERFORM  TCM-RTN      THRU  TCM-EX
               END-IF
           END-IF
           MOVE  SPACE        TO  P-R.
           MOVE  P-R3         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           ADD  1             TO  LCNT.
           GO  TO  PRI-020.
       PRI-090.
           IF  W-DC  =  5
               MOVE  9   TO  W-DC
           END-IF
           IF  W-DC  =  0
               MOVE  5   TO  W-DC
           END-IF.
       PRI-EX.
           EXIT.
      *
      ********************************
      ***  S H O K E I   R T N     ***
      ********************************
       SHOKEI-RTN.
           IF  W-DC   NOT =  9
               GO  TO  SHOKEI-EX
           END-IF
           IF  W-AKEI    =  ZERO
               GO  TO  SHOKEI-EX
           END-IF
           MOVE  ZERO         TO  GOUKEI.
           MOVE  1111         TO  W-ZCD.
           MOVE  0            TO  N.
       SHOKEI-010.
           ADD  1             TO  N.
           IF  N     =  5
               MOVE  0            TO  N
               GO  TO  SHOKEI-020
           END-IF
           IF  ZERO = W-KEI(N,1) AND W-KEI(N,2) AND W-KEI(N,3) AND
                     W-KEI(N,4) AND W-KEI(N,5) AND W-KEI(N,6) AND
               W-KEI(N,7) AND W-KEI(N,8) AND W-KEI(N,9) AND W-KEI(N,10)
               MOVE  0            TO  W-ZC(N)
           END-IF
           IF  W-ZC(N)  =  1
               COMPUTE GOUKEI = W-KEI(N,1) + W-KEI(N,2) + W-KEI(N,3) +
                   W-KEI(N,4) + W-KEI(N,5) + W-KEI(N,6) + W-KEI(N,7) +
                   W-KEI(N,8) + W-KEI(N,9) + W-KEI(N,10) + GOUKEI
           END-IF
           GO  TO  SHOKEI-010.
       SHOKEI-020.
           ADD  1             TO  N.
           IF  N     =  5
               GO  TO  SHOKEI-EX
           END-IF
           IF  W-ZC(N) = 0
               GO  TO  SHOKEI-020
           END-IF
           MOVE  SPACE        TO  P-R4.
           MOVE  SPACE        TO  P4-02.
           IF  N     =  1
               MOVE  "　［　　合　計　　］　　　　　"  TO  P4-02
           END-IF
           IF  N     =  2
               IF  0      =  W-ZC(1)
                   MOVE  "　［　　合　計　　］　　　　　"  TO  P4-02
               END-IF
           END-IF
           IF  N     =  3
               IF  0      =  W-ZC(1)  AND  W-ZC(2)
                   MOVE  "　［　　合　計　　］　　　　　"  TO  P4-02
               END-IF
           END-IF
           IF  N     =  4
               IF  0      =  W-ZC(1)  AND  W-ZC(2)  AND  W-ZC(3)
                   MOVE  "　［　　合　計　　］　　　　　"  TO  P4-02
               END-IF
           END-IF
           MOVE  N            TO  P4-03.
           MOVE  ZERO         TO  I.
       SHOKEI-030.
           ADD  1             TO  I.
           IF  I     NOT  =  11
               MOVE  W-KEI(N,I)   TO  P4-041(I)
               GO  TO  SHOKEI-030
           END-IF
           IF  N     =  4
               MOVE  GOUKEI  TO  P4-05
           END-IF
           IF  N     =  3
               IF  0      =  W-ZC(4)
                   MOVE  GOUKEI  TO  P4-05
               END-IF
           END-IF
           IF  N     =  2
               IF  0      =  W-ZC(4)  AND  W-ZC(3)
                   MOVE  GOUKEI  TO  P4-05
               END-IF
           END-IF
           IF  N     =  1
               IF  0      =  W-ZC(4)  AND  W-ZC(3)  AND  W-ZC(2)
                   MOVE  GOUKEI  TO  P4-05
               END-IF
           END-IF
           IF  LCNT  NOT <  61
               PERFORM  MID-RTN      THRU  MID-EX
               PERFORM  HIM-RTN      THRU  HIM-EX
               MOVE  "　［　　合　計　　］　　　　　"  TO  P4-02
           END-IF
           MOVE  SPACE        TO  P-R.
           MOVE  P-R4         TO  P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           ADD  1             TO  LCNT.
           GO  TO  SHOKEI-020.
       SHOKEI-EX.
           EXIT.
      *
      *****************************
      ***    ﾐ ﾀﾞ ｼ  R T N      ***
      *****************************
      **
       MID-RTN.
           IF  LCNT   <  90
               MOVE   SPACE   TO   P-R
               CALL "PR_Write" USING P-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
      *
           ADD   1   TO    PCNT.
           MOVE  PCNT  TO  WPCNT.
      *
           MOVE   MID1    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID2    TO    P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO     P-R.
           IF  COMPLETION_CODE    =  002
               MOVE   MID3    TO    P-R
               CALL "PR_LineFeed" USING "1" RETURNING RESP
               CALL "PR_Write" USING P-R RETURNING RESP
               MOVE   SPACE   TO     P-R
           END-IF
           MOVE   MID4    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID5    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID6    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID7    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
      *
           IF  COMPLETION_CODE    =  001
               MOVE  8     TO  LCNT
           ELSE
               MOVE  9     TO  LCNT
           END-IF.
       MID-EX.
           EXIT.
      *
      **************************
      ***  ﾌ ｧ ｲ ﾙ  CLOSE    ***
      **************************
      **
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
