       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT090U.
       AUTHOR.          I.NAKANISHI.
      *********************************************************
      *    PROGRAM         :  棚卸更新                        *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ***                             *
      *    DATA WRITTN     :  62/08/21                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM150.
       OBJECT-COMPUTER. NEAC-SYSTEM150.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  N                  PIC  9(01)   VALUE  ZERO.
       77  I                  PIC  9(02)   VALUE  ZERO.
       77  OKC                PIC  9(01).
       77  WRI-SW             PIC  9(01).
       77  CHK-SW             PIC  9(02).
       77  ERR-STAT           PIC  X(02).
       77  WK0256ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0256".
           02  W-FID2         PIC  X(003).
       COPY  LWMSG.
      *
           COPY   LIHIM.
           COPY   LNJZAI.
      *FD  HTIF
       01  HTIF_JT090U.
           02  HTIF_PNAME1    PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  HTIF_LNAME     PIC  X(011) VALUE "HTIF_JT090U".
           02  F              PIC  X(001).
           02  HTIF_KEY1      PIC  X(100) VALUE SPACE.
           02  HTIF_SORT      PIC  X(100) VALUE SPACE.
           02  HTIF_IDLST     PIC  X(100) VALUE SPACE.
           02  HTIF_RES       USAGE  POINTER.
       01  HTI-R.
           02  F              PIC  X(006).
           02  HTI-GNO        PIC  9(001).
           02  HTI-SNO        PIC  9(001).
           02  HTI-HCD        PIC  9(006).
           02  HTI-SIZ        PIC  9(001).
           02  HTI-SUD.
             03  HTI-SU       PIC S9(006)  OCCURS  10.
           02  HTI-BC.
             03  HTI-BC1      PIC  9(002).
             03  HTI-BC2      PIC  9(002).
             03  HTI-BC3      PIC  9(002).
           02  HTI-ISU        PIC  9(003).
           02  F              PIC  X(172).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-OKC   PIC 9 .
       01  C-PM.
           02  FILLER  PIC X(13) VALUE
                "             " .
           02  FILLER  PIC X(11) VALUE
                "棚 卸 更 新".
           02  FILLER  PIC X(29) VALUE
                "確認（OK=1,終了=PF9）--> ﾘﾀｰﾝ".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-OKC" "9" "24" "65" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-OKC" BY REFERENCE OKC "1" "0" RETURNING RESU.
      *C-PM
       CALL "SD_Init" USING 
            "C-PM" " " "0" "0" "53" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PM" "RX" "1" "20" "13" " " "C-PM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-PM" "X" "1" "21" "11" "01C-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-PM" "X" "24" "41" "29" "02C-PM" " " RETURNING RESU.
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
           PERFORM  INIT-RTN  THRU   INIT-EX.
       MR-10.
           CALL "SD_Accept" USING BY REFERENCE A-OKC "A-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT  NOT =  "01"
               GO  TO  MR-10
           END-IF
           IF  OKC    NOT  =  1
               GO  TO  MR-10
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO HTIF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" HTIF_PNAME1 " " BY REFERENCE HTIF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "EXCLUSIVE" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           PERFORM     NJZS-RTN   THRU  NJZS-EX.
           PERFORM     ENT-RTN    THRU  ENT-EX.
       MR-99.
           PERFORM  END-RTN  THRU  END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INIT-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-PM" C-PM "p" RETURNING RESU.
       INIT-EX.
            EXIT.
      ******************************
      ***   ENT     R T N        ***
      ******************************
      **
       ENT-RTN.
       ENT-01.
      *           READ   HTIF          AT  END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" HTIF_PNAME1 BY REFERENCE HTI-R " " RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-EX
           END-IF
           IF  ZERO    =  HTI-SU(01) AND HTI-SU(02) AND HTI-SU(03)
                        AND HTI-SU(04) AND HTI-SU(05) AND HTI-SU(06)
                        AND HTI-SU(07) AND HTI-SU(08) AND HTI-SU(09)
                        AND HTI-SU(10)
               GO  TO  ENT-01
           END-IF
           MOVE HTI-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE HTI-HCD TO HI-MHCD
           END-IF.
       ENT-11.
           MOVE  HTI-SNO   TO  NJZAI-01.
           MOVE  HI-MHCD   TO  NJZAI-02.
           MOVE  HTI-SIZ   TO  NJZAI-03.
       ENT-13.
      *           READ  NJZAI INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-15
           END-IF
           PERFORM  NJS-RTN  THRU  NJS-EX.
      *
           PERFORM  NJR-RTN  THRU  NJR-EX.
           GO  TO  ENT-21.
       ENT-15.
           INITIALIZE    NJZAI-R.
           MOVE  HTI-SNO   TO  NJZAI-01.
           MOVE  HI-MHCD   TO  NJZAI-02.
           MOVE  HTI-SIZ   TO  NJZAI-03.
           PERFORM  NJS-RTN  THRU  NJS-EX.
      *
           PERFORM  NJW-RTN  THRU  NJW-EX.
           IF  WRI-SW       =  1
               GO  TO  ENT-15
           END-IF.
       ENT-21.
           MOVE  9         TO  NJZAI-01.
           MOVE  HI-MHCD   TO  NJZAI-02.
           MOVE  HTI-SIZ   TO  NJZAI-03.
       ENT-23.
      *           READ  NJZAI INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  ENT-25
           END-IF
           PERFORM  NJS-RTN  THRU  NJS-EX.
      *
           PERFORM  NJR-RTN  THRU  NJR-EX.
           GO  TO  ENT-01.
       ENT-25.
           INITIALIZE    NJZAI-R.
           MOVE  9         TO  NJZAI-01.
           MOVE  HI-MHCD   TO  NJZAI-02.
           MOVE  HTI-SIZ   TO  NJZAI-03.
           PERFORM  NJS-RTN  THRU  NJS-EX.
      *
           PERFORM  NJW-RTN  THRU  NJW-EX.
           IF  WRI-SW       =  1
               GO  TO  ENT-25
           END-IF
           GO  TO  ENT-01.
       ENT-EX.
           EXIT.
      *****
      *********************************
      *    NJZAI ZERO SET RTN         *
      *********************************
       NJZS-RTN.
      *           READ  NJZAI     NEXT   AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               GO  TO  NJZS-EX
           END-IF
      *
           INITIALIZE          NJZAI-04  NJZAI-05  NJZAI-06  NJZAI-11.
      *
      *           REWRITE  NJZAI-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE  "NJZAI"  TO  ERR-F
               MOVE  "R"  TO  ERR-M
               MOVE  NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           GO  TO  NJZS-RTN.
       NJZS-EX.
           EXIT.
      ****************************
      *    NJZAI SET RTN         *
      ****************************
       NJS-RTN.
           MOVE  0  TO  I.
       NJS-02.
           ADD  1  TO  I.
           IF  I  >  10
               GO  TO  NJS-EX
           END-IF
           IF  HTI-GNO < 5
               COMPUTE NJZAI-0611(I) = NJZAI-0611(I) +
                                               (HTI-SU(I) * HTI-ISU)
           ELSE
               ADD    HTI-SU(I)   TO   NJZAI-0611(I)
           END-IF
           GO  TO  NJS-02.
       NJS-EX.
           EXIT.
      ****************************
      *    NJZAI REWRITE RTN     *
      ****************************
       NJR-RTN.
      *           REWRITE  NJZAI-R   INVALID
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE  "NJZAI"  TO  ERR-F
               MOVE  "R"  TO  ERR-M
               MOVE  NJZAI-KEY  TO  ERR-K
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF.
       NJR-EX.
           EXIT.
      ****************************
      *    NJZAI WRITE RTN       *
      ****************************
       NJW-RTN.
           MOVE   0       TO  WRI-SW.
      *           WRITE    NJZAI-R   INVALID
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO  TO  NJW-01
           END-IF
           GO  TO  NJW-EX.
       NJW-01.
           MOVE  "NJZAI"  TO  ERR-F.
           MOVE  "W"  TO  ERR-M.
           MOVE  NJZAI-KEY  TO  ERR-K.
           IF  ERR-STAT     =  "24"
               MOVE  ERR-STAT     TO  ERR-FLG
               CALL "SD_Output" USING
                "ERR-DIS" ERR-DIS "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE NJZAI_IDLST NJZAI_PNAME1
               CALL "SD_Output" USING
                " " "ｴﾘｱ ｶｸﾁｮｳｺﾞ,ｻｲｶｲｷｰ ｦ ｵｽ!" "STOP" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU
               CALL "DB_F_Open" USING
                "I-O" NJZAI_PNAME1 "EXCLUSIVE" BY REFERENCE NJZAI_IDLST
                "1" "NJZAI-KEY" BY REFERENCE NJZAI-KEY
               MOVE  1            TO  WRI-SW
               GO  TO  NJW-EX
           END-IF
           IF  ERR-STAT NOT =  "00"
               PERFORM  ERR-RTN  THRU  ERR-EX
           END-IF
           MOVE  2            TO  WRI-SW.
       NJW-EX.
           EXIT.
      *****
       END-RTN.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HTIF_IDLST HTIF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
       COPY  LPMSG.
