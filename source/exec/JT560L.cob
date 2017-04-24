       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JT560L.
       AUTHOR.          MAYUMI.I.
      *********************************************************
      *    PROGRAM         :  出荷確定未処理リスト　　　　　　*
      *                    : (送り状未生成リスト) 　　　　　　*
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
       77  WK0256ID               PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1            PIC  X(003).
           02  STN-NO2            PIC  X(003).
       01  W-FID.
           02  W-FID1             PIC  X(006) VALUE "WK0256".
           02  W-FID2             PIC  X(003).
       01  W-JS                   PIC 9(01).
       01  W-JSP                  PIC 9(01).
       01  W-JS-MEI               PIC N(03).
      *
       01  PRN-AREA.
           02  LCNT               PIC  9(02) VALUE  90.
           02  PCNT               PIC  9(03) VALUE  ZERO.
           02  OLD-KEY            PIC  9(06).
           02  I                  PIC  9(02).
           02  SHOKEI             PIC S9(06).
           02  W-HCD.
               03  W-HCD1         PIC  9(04).
               03  W-HCD2         PIC  9(02).
       01  OLD-AREA.
           02  OLD-14D            PIC  N(09).
           02  OLD-15             PIC  N(23).
           02  OLD-15A            PIC  S9(03).
       01  WORK-AREA.
           02  HIZUKE.
               03  HI-YY          PIC 9(02).
               03  HI-MM          PIC 9(02).
               03  HI-DD          PIC 9(02).
      **
       01  W-MID0.
           02  F              PIC X(48) VALUE
               "＊＊＊　　出　荷　確　定　未　処　理　リ　ス　ト".
           02  F              PIC X(10)  VALUE
               "　　＊＊＊".
       01  W-MID1.
           02  F              PIC X(48) VALUE
               "    ＊＊＊　　送　り　状　未　生　成　リ　ス　ト".
           02  F              PIC X(10)  VALUE
               "　　＊＊＊".
       01  MID1.
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC X(04) VALUE SPACE.
           02  M-01           PIC N(05).
           02  F              PIC X(15) VALUE SPACE.
           02  M-02           PIC X(58).
           02  F              PIC X(18) VALUE SPACE.
           02  F              PIC X(5) VALUE "DATE.".
           02  M-YY           PIC Z9.
           02  F              PIC X    VALUE "/".
           02  M-MM           PIC Z9.
           02  F              PIC X    VALUE "/".
           02  M-DD           PIC Z9.
           02  F              PIC X(7) VALUE SPACE.
           02  F              PIC X(2) VALUE "P.".
           02  WPCNT          PIC ZZ9.
       01  MID2.
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC N(03) VALUE "指図№".
           02  F              PIC X(01) VALUE SPACE.
           02  F              PIC X(05) VALUE X"1A24212078".
           02  F              PIC N(02) VALUE "伝区".
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC X(02) VALUE SPACE.
           02  F              PIC N(03) VALUE "出荷日".
           02  F              PIC X(02) VALUE SPACE.
           02  F              PIC X(05) VALUE "ｾｯﾄ数".
           02  F              PIC X(02) VALUE SPACE.
           02  F              PIC X(08) VALUE "直送先CD".
           02  F              PIC X(01) VALUE SPACE.
           02  F              PIC N(04) VALUE "得意先名".
           02  F              PIC X(32) VALUE SPACE.
           02  F              PIC N(04) VALUE "直送先名".
           02  F              PIC X(32) VALUE SPACE.
           02  F              PIC N(03) VALUE "倉　庫".
       01  MID3.
           02  F              PIC X(05) VALUE X"1A24212474".
           02  F              PIC X(01) VALUE  SPACE.
           02  F              PIC X(04) VALUE  "-行 ".
           02  F              PIC X(13) VALUE  "品名CD 品　名".
           02  F              PIC X(31) VALUE  SPACE.
           02  F              PIC X(27) VALUE
                              "1 ３号 ２号 １号 ０号  中  ".
           02  F              PIC X(24) VALUE
                              " 大  特大 28.0 29.0 30.0".
       01  MID4.
           02  F              PIC X(49) VALUE  SPACE.
           02  F              PIC X(27) VALUE
                              "2 12.5 13.0 13.5 14.0 15.0 ".
           02  F              PIC X(24) VALUE
                              "16.0 17.0 18.0 19.0 20.0".
       01  MID5.
           02  F              PIC X(49) VALUE  SPACE.
           02  F              PIC X(27) VALUE
                              "3 21.0 21.5 22.0 22.5 23.0 ".
           02  F              PIC X(24) VALUE
                              "23.5 24.0 24.5 25.0     ".
       01  MID6.
           02  F              PIC X(49) VALUE  SPACE.
           02  F              PIC X(27) VALUE
                              "4 24.0 24.5 25.0 25.5 26.0 ".
           02  F              PIC X(24) VALUE
                              "26.5 27.0 27.5          ".
           02  F              PIC X(06) VALUE SPACE.
           02  F              PIC N(01) VALUE "計".
           02  F              PIC X(02) VALUE  SPACE.
           02  F              PIC X(09) VALUE  "受注№-行".
           02  F              PIC X(01) VALUE  SPACE.
           02  F              PIC N(03) VALUE  "備　考".
      ***
       COPY  LWMSG.
      ***
           COPY   LTWK03.
           COPY   LITCM.
           COPY   LIHIM2.
           COPY   L-JCON.
      *FD  P-F
       01  P-R.
           02  P-R1.
               03  P1-15B             PIC X(05).
               03  P1-01              PIC 9(06).
               03  FILLER             PIC X(01).
               03  P1-02              PIC N(02).
               03  FILLER             PIC X(01).
               03  P1-031             PIC Z9.
               03  P1-A               PIC X(01).
               03  P1-032             PIC Z9.
               03  P1-B               PIC X(01).
               03  P1-033             PIC Z9.
               03  FILLER             PIC X(02).
               03  P1-04              PIC ----.
               03  FILLER             PIC X(02).
               03  P1-051             PIC 9(04).
               03  P1-C               PIC X(01).
               03  P1-052             PIC 9(03).
               03  FILLER             PIC X(01).
               03  P1-06              PIC N(26).
               03  FILLER             PIC X(01).
               03  P1-07              PIC N(26).
               03  FILLER             PIC X(01).
               03  P1-081             PIC 9(01).
               03  FILLER             PIC X(01).
               03  P1-082             PIC N(06).
               03  P1-2B              PIC X(05).
               03  F                  PIC X(83).
           02  P-R2 REDEFINES P-R1.
               03  P2-15B             PIC X(05).
               03  FILLER             PIC X(02).
               03  P2-D               PIC X(01).
               03  P2-01              PIC 9(01).
               03  FILLER             PIC X(01).
               03  P2-02              PIC 9(06).
               03  FILLER             PIC X(01).
               03  P2-03              PIC N(24).
               03  FILLER             PIC X(01).
               03  P2-04              PIC 9(01).
               03  P2-05     OCCURS 10.
                   04  P2-051         PIC -----.
               03  P2-06              PIC ----,---.
               03  FILLER             PIC X(02).
               03  P2-071             PIC 9(06).
               03  P2-E               PIC X(01).
               03  P2-072             PIC 9(01).
               03  FILLER             PIC X(02).
               03  P2-08              PIC X(10).
               03  P2-2B              PIC X(05).
               03  F                  PIC X(98).
           02  P-R3 REDEFINES P-R1.
               03  P3-15B             PIC X(05).
               03  FILLER             PIC X(18).
               03  P3-01              PIC N(02).
               03  P3-F               PIC X(01).
               03  P3-02.
                   04  P3-021         PIC N(09).
                   04  P3-022         PIC N(01).
               03  FILLER             PIC X(01).
               03  P3-03              PIC N(02).
               03  P3-G               PIC X(01).
               03  P3-04.
                   04  P3-041         PIC N(23).
                   04  P3-042         PIC N(01).
               03  P3-05              PIC N(02).
               03  P3-H               PIC X(01).
               03  P3-06              PIC ----.
               03  FILLER             PIC X(10).
               03  P3-07              PIC N(02).
               03  P3-K               PIC X(01).
               03  P3-08              PIC ----,---.
               03  P3-2B              PIC X(05).
               03  F                  PIC X(111).
      *
       77  ESTAT                      PIC  X(002).
       77  RESU                       PIC  9(001).
       77  RESP                       PIC  9(001).
       77  RET                        PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER              PIC  9(003).
       77  USER_ID                    PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE            PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
      ***
       01  DSP-AREA0.
           02  FILLER  PIC  X(22) VALUE
               " 出荷確定未処理リスト ".
       01  DSP-AREA1.
           02  FILLER  PIC  X(20) VALUE
               " 送り状未生成リスト ".
       01  DSP-AREA.
           02  FILLER  PIC  X(02) VALUE  "〔".
           02  FILLER  PIC  X(02) VALUE  "〕".
           02  FILLER  PIC  N(03).
       01  DSP-ERR.
           02  ERR-1   PIC  X(22) VALUE
                                 "ＪＳ－ＳＩＧＮ　エラー".
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
      *DSP-AREA0
       CALL "SD_Init" USING 
            "DSP-AREA0" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA0" "RX" "1" "20" "22" " " "DSP-AREA0"
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING 
            "DSP-AREA1" " " "0" "0" "20" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA1" "RX" "1" "20" "20" " " "DSP-AREA1"
            RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "10" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-AREA" "X" "1" "1" "2" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
           "02DSP-AREA" "X" "1" "9" "2" "01DSP-AREA" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "03DSP-AREA" "N" "1" "3" "6" "02DSP-AREA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03DSP-AREA" BY REFERENCE W-JS-MEI "6" "0" RETURNING RESU.
      *DSP-ERR
       CALL "SD_Init" USING 
            "DSP-ERR" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ERR-1" "X" "24" "1" "22" " " "DSP-ERR" RETURNING RESU.
      *
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      ****************************
      ***  ﾒ ｲ ﾝ  R T N        ***
      ****************************
      **
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           PERFORM  PRN-RTN     THRU   PRN-EX.
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
      *
      *********************************
      ***   ｲﾆｼｬﾙ   R T N           ***
      *********************************
      **
       INI-RTN.
           ACCEPT  W-JSP FROM ARGUMENT-VALUE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  W-JSP NOT =  ZERO  AND  1
               CALL "SD_Output" USING
                "ERR-1" ERR-1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  W-JSP =  ZERO
               CALL "SD_Output" USING
                "DSP-AREA0" DSP-AREA0 "p" RETURNING RESU
               MOVE  W-MID0   TO  M-02
           END-IF
           IF  W-JSP =  1
               CALL "SD_Output" USING
                "DSP-AREA1" DSP-AREA1 "p" RETURNING RESU
               MOVE  W-MID1   TO  M-02
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK03_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK03_PNAME1 "SHARED" BY REFERENCE
            JT-WK03_IDLST "0".
      *           READ  JT-WK03  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK03_PNAME1 BY REFERENCE W03-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_Close"
               STOP  RUN
           END-IF
           MOVE  W03-16   TO  W-JS.
           IF  W-JS  =  2
               MOVE  1        TO  W-JS
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK03_IDLST JT-WK03_PNAME1.
           IF  W-JS  NOT =  ZERO  AND  1
               CALL "SD_Output" USING
                "ERR-1" ERR-1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
      *
           IF  W-JS  =  ZERO
               MOVE  "教　育"     TO  W-JS-MEI
           END-IF
           IF  W-JS  =  1
               MOVE  "一　般"     TO  W-JS-MEI
           END-IF
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
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
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON2-KEY" BY REFERENCE JCON2-KEY.
           CALL "PR_Open" RETURNING RESP.
           ACCEPT   HIZUKE  FROM  DATE.
           MOVE  HI-YY     TO  M-YY.
           MOVE  HI-MM     TO  M-MM.
           MOVE  HI-DD     TO  M-DD.
       INI-EX.
           EXIT.
      *
      ******************************
      ***   ﾘ ｽ ﾄ   R T N        ***
      ******************************
      **
       PRN-RTN.
      ***  ワークファイル　ＲＥＡＤ
      *           READ  JT-WK03  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JT-WK03_PNAME1 BY REFERENCE W03-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               PERFORM  SHOKEI-RTN     THRU SHOKEI-EX
               GO  TO  PRN-EX
           END-IF
           IF  LCNT  =  90
               MOVE  W03-01      TO  OLD-KEY
           END-IF
           IF  W03-01  NOT =  OLD-KEY
               PERFORM  SHOKEI-RTN     THRU  SHOKEI-EX
           END-IF
           IF  LCNT  NOT <  62
               PERFORM  MID-RTN        THRU  MID-EX
           END-IF
           PERFORM  MEI-RTN     THRU  MEI-EX.
           PERFORM  OLD-MOVE-RTN       THRU  OLD-MOVE-EX.
           GO  TO  PRN-RTN.
       PRN-EX.
           EXIT.
      *
      *****************************
      ***  H E A D -  R T N     ***
      *****************************
       HEAD-RTN.
           IF  LCNT  NOT <  61
               PERFORM  MID-RTN     THRU  MID-EX
           END-IF
           MOVE  15K          TO  P1-15B.
           MOVE  20K          TO  P1-2B.
           MOVE  "/"          TO  P1-A  P1-B.
           MOVE  "-"          TO  P1-C.
           MOVE  W03-01       TO  P1-01.
           MOVE  SPACE        TO  P1-02.
           IF  W03-03  =  ZERO
               MOVE  "出荷"     TO  P1-02
           END-IF
           IF  W03-03  =  3
               MOVE  "訂正"     TO  P1-02
           END-IF
           IF  W03-03  =  7
               MOVE  "サ出"     TO  P1-02
           END-IF
           IF  W-JSP        =   ZERO
               MOVE  W03-0512     TO  P1-031
               MOVE  W03-052      TO  P1-032
               MOVE  W03-053      TO  P1-033
           END-IF
           IF  W-JSP        =   1
               MOVE  W03-0412     TO  P1-031
               MOVE  W03-042      TO  P1-032
               MOVE  W03-043      TO  P1-033
           END-IF
           MOVE  W03-14A      TO  P1-04.
           MOVE  W03-061      TO  P1-051.
           MOVE  W03-062      TO  P1-052.
      *
           MOVE  W03-061      TO  TC-TCD.
           MOVE  "001"        TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           MOVE  TC-NAME      TO  P1-06.
      *
           IF  W03-062         =  001
               MOVE  SPACE        TO  P1-07
               GO  TO  HEAD-010
           END-IF
           MOVE  W03-061      TO  TC-TCD.
           MOVE  W03-062      TO  TC-CCD.
           PERFORM  TCM-READ-RTN     THRU  TCM-READ-EX.
           MOVE  TC-NAME      TO  P1-07.
       HEAD-010.
           MOVE  W03-07       TO  P1-081.
      *
           MOVE  3            TO  JCON3-01.
           MOVE  W03-07       TO  JCON3-02.
           PERFORM  JCON3-READ-RTN     THRU  JCON3-READ-EX.
           MOVE  JCON3-03     TO  P1-082.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE     TO  P-R.
           ADD  1      TO  LCNT.
       HEAD-EX.
           EXIT.
      *
      *****************************
      ***    M E I   R T N      ***
      *****************************
       MEI-RTN.
           IF  ( W03-01  NOT =  OLD-KEY )  OR  ( LCNT  =  8 )
               PERFORM  HEAD-RTN     THRU  HEAD-EX
           END-IF
           MOVE  15K          TO  P2-15B.
           MOVE  20K          TO  P2-2B.
           MOVE  "-"          TO  P2-D.
           MOVE  W03-02       TO  P2-01.
           MOVE  W03-09       TO  P2-02.
           MOVE  W03-09       TO  HI-MHCD HI-HCD.
           PERFORM HIM-READ-RTN     THRU  HIM-READ-EX.
           MOVE  HI-NAME      TO  P2-03.
           MOVE  W03-10       TO  P2-04.
           MOVE  1            TO  I.
       MEI-010.
           IF  I  >  10
               GO  TO  MEI-020
           END-IF
           IF  W-JSP            =  ZERO
               MOVE  W03-1211(I)  TO  P2-051(I)
           END-IF
           IF  W-JSP            =  1
               MOVE  W03-1111(I)  TO  P2-051(I)
           END-IF
           ADD   1            TO  I.
           GO  TO  MEI-010.
       MEI-020.
           IF  W-JSP            =  ZERO
               MOVE  W03-122      TO  P2-06
           END-IF
           IF  W-JSP            =  1
               MOVE  W03-112      TO  P2-06
           END-IF
           IF  W03-081      NOT =  ZERO
               MOVE  W03-081      TO  P2-071
               MOVE  "-"          TO  P2-E
               MOVE  W03-082      TO  P2-072
           END-IF
           MOVE  W03-20       TO  P2-08.
           MOVE  W03-09       TO  W-HCD.
           IF  W-HCD1       NOT =  9999
               IF  W-JSP        =   ZERO
                   ADD   W03-122      TO  SHOKEI
               END-IF
           END-IF
           IF  W-HCD1       NOT =  9999
               IF  W-JSP        =   1
                   ADD   W03-112      TO  SHOKEI
               END-IF
           END-IF
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           ADD  1             TO  LCNT.
       MEI-EX.
           EXIT.
      *
      ********************************
      ***  S H O K E I   R T N     ***
      ********************************
       SHOKEI-RTN.
           IF  LCNT  =  90
               GO  TO  SHOKEI-EX
           END-IF
           MOVE  15K          TO  P3-15B.
           MOVE  20K          TO  P3-2B.
           MOVE  ":"          TO  P3-F  P3-G  P3-H  P3-K.
           MOVE  "配達"     TO  P3-01.
           MOVE  "摘要"     TO  P3-03.
           MOVE  "個数"     TO  P3-05.
           MOVE  "小計"     TO  P3-07.
           MOVE  OLD-14D      TO  P3-021.
           MOVE  "　"       TO  P3-022.
           MOVE  OLD-15       TO  P3-041.
           MOVE  "　"       TO  P3-042.
           MOVE  OLD-15A      TO  P3-06.
      *
           MOVE  SHOKEI       TO  P3-08.
           MOVE  ZERO         TO  SHOKEI.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE  SPACE        TO  P-R.
           ADD  1             TO  LCNT.
       SHOKEI-EX.
           EXIT.
      *
      *************************************
      ***    O L D   M O V E   R T N    ***
      *************************************
       OLD-MOVE-RTN.
           MOVE  W03-01       TO  OLD-KEY.
           MOVE  W03-14D      TO  OLD-14D.
           MOVE  W03-15       TO  OLD-15.
           MOVE  W03-15A      TO  OLD-15A.
       OLD-MOVE-EX.
           EXIT.
      *
      *******************************
      ***   R E A D   J C O N 2   ***
      *******************************
       JCON2-READ-RTN.
      ***  コントロールＦ　ＲＥＡＤ
      *           READ  JCON  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  JCON2-03
           END-IF.
       JCON2-READ-EX.
           EXIT.
      *
      *******************************
      ***   R E A D   J C O N 3   ***
      *******************************
       JCON3-READ-RTN.
      ***  コントロールＦ　ＲＥＡＤ
      *           READ  JCON  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  JCON3-03
           END-IF.
       JCON3-READ-EX.
           EXIT.
      *
      *****************************
      ***   R E A D   T C M     ***
      *****************************
       TCM-READ-RTN.
      ***  直送先マスタ　ＲＥＡＤ
      *           READ  TC-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  TC-NAME
           END-IF.
       TCM-READ-EX.
           EXIT.
      *
      *****************************
      ***   R E A D   S H M     ***
      *****************************
       HIM-READ-RTN.
      *           READ HI2-M UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE     TO  HI-NAME
           END-IF.
       HIM-READ-EX.
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
           IF  W-JS  =  ZERO
               MOVE  "【教　育】"     TO  M-01
           END-IF
           IF  W-JS  =  1
               MOVE  "【一　般】"     TO  M-01
           END-IF
      *
           MOVE   MID1    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
           MOVE   MID2    TO    P-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE   TO     P-R.
           MOVE   MID3    TO    P-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING P-R RETURNING RESP.
           MOVE   SPACE    TO     P-R.
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
      *
           MOVE  8     TO  LCNT.
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
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
      ***
       COPY  LPMSG.
      ***
