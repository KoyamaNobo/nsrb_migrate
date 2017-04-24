       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      JTN27U.
       AUTHOR.          ________.
      *********************************************************
      *    PROGRAM         :  有効在庫ワーク　作成 (生協)     *
      *    PRINTER TYPE    :  JIPS                            *
      *    DATA WRITTN     :  03/02/21                        *
      *    COMPILE TYPE    :  CBL85 (74MODE)                  *
      *    JS-SIGN         :  0=生協 , 1=ＶＩＶ , 9=全件      *
      *********************************************************
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SOURCE-COMPUTER. NEAC-SYSTEM3100.
       OBJECT-COMPUTER. NEAC-SYSTEM3100.
       DATA    DIVISION.
       WORKING-STORAGE  SECTION.
       77  JS-SIGN                  PIC  9(1).
       77  ERR-STAT                 PIC  X(2).
       77  WK0128ID                 PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1              PIC  X(003).
           02  STN-NO2              PIC  X(003).
       01  W-FID.
           02  W-FID1               PIC  X(006) VALUE "WK0128".
           02  W-FID2               PIC  X(003).
       01  W-SUD.
           02  W-SU                 PIC S9(06)  OCCURS  10.
       01  W-SUT                    PIC  S9(06).
       01  ZERO-SW                  PIC  9(01).
       01  W-AREA.
           02  W-KEY.
               03  W-MHCD           PIC  9(06).
               03  W-HCD            PIC  9(06).
           02  I                    PIC  9(02).
           02  W-SIZ                PIC  9(01).
           02  OKC                  PIC  9(01).
       COPY  LWMSG.
      *
           COPY   LNJZAI.
           COPY   LJMST3.
           COPY   LIHIM.
           COPY   LIHSHF.
      *FD  JT-YZAI
       01  JT-YZAI_JTN27U.
           02  JT-YZAI_PNAME1       PIC  X(009) VALUE SPACE.
           02  F                    PIC  X(001).
           02  JT-YZAI_LNAME        PIC  X(014) VALUE "JT-YZAI_JTN27U".
           02  F                    PIC  X(001).
           02  JT-YZAI_KEY1         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_SORT         PIC  X(100) VALUE SPACE.
           02  JT-YZAI_IDLST        PIC  X(100) VALUE SPACE.
           02  JT-YZAI_RES          USAGE  POINTER.
       01  YZAI-R.
           02   YZAI-HCD            PIC 9(6).
           02   YZAI-HNA            PIC N(24).
           02   YZAI-KBN            PIC N(03).
           02   YZAI-SIZ            PIC 9(1).
           02   YZAI-ASU.
                03   YZAI-SUD       OCCURS  10.
                    04   YZAI-SU    PIC S9(6).
           02   YZAI-TSU            PIC S9(06).
           02   YZAI-KC             PIC 9(1).
       77  F                        PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-MID.
             03  FILLER  PIC  X(26) VALUE
                  "                          ".
             03  FILLER  PIC  X(24) VALUE
                 "生協有効在庫ワーク　抽出".
             03  FILLER  PIC  X(25) VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
           02  DSP-MID1.
             03  FILLER  PIC  X(36) VALUE
                  "                                    ".
             03  FILLER  PIC  X(34) VALUE
                 "ヴィヴェンディ有効在庫ワーク　抽出".
             03  FILLER  PIC  X(25) VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
           02  DSP-MID9.
             03  FILLER  PIC  X(32) VALUE
                  "                                ".
             03  FILLER  PIC  X(30) VALUE
                 "有効在庫ワーク　抽出　（全件）".
             03  FILLER  PIC  X(25) VALUE  "確認(OK=1,NO=9)-->   ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-OKC        PIC 9(01).
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
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "257" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID" " " "0" "0" "75" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID" "RX" "1" "27" "26" " " "DSP-MID"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MID" "X" "1" "28" "24" "01DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MID" "X" "23" "41" "25" "02DSP-MID" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID1" " " "0" "0" "95" "DSP-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID1" "RX" "1" "27" "36" " " "DSP-MID1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MID1" "X" "1" "28" "34" "01DSP-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MID1" "X" "23" "41" "25" "02DSP-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-MID9" " " "0" "0" "87" "DSP-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-MID9" "RX" "1" "27" "32" " " "DSP-MID9"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-MID9" "X" "1" "28" "30" "01DSP-MID9" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-MID9" "X" "23" "41" "25" "02DSP-MID9" " "
            RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-OKC" "9" "23" "61" "1" " " "ACP-AREA" RETURNING RESU.
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
       MR-RTN.
           PERFORM  INI-RTN     THRU   INI-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_Close"
               STOP RUN
           END-IF
           PERFORM  UPD1-RTN    THRU   UPD1-EX.
           PERFORM  UPD2-RTN    THRU   UPD2-EX.
           IF  JS-SIGN  =  0
               PERFORM  UPD3-RTN    THRU   UPD3-EX
           END-IF
           PERFORM  END-RTN     THRU   END-EX.
           CALL "DB_Close".
           STOP  RUN.
       INI-RTN.
           ACCEPT  JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN  NOT  =  0  AND  1  AND  9
               CALL "C3_Set_Jrcode" USING 
               USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 9
               CALL "SD_Output" USING
                "DSP-MID9" DSP-MID9 "p" RETURNING RESU
           ELSE
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "DSP-MID1" DSP-MID1 "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "DSP-MID" DSP-MID "p" RETURNING RESU
               END-IF
           END-IF 
           INITIALIZE  W-AREA.
       INI-510.
           CALL "SD_Accept" USING BY REFERENCE ACP-OKC "ACP-OKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT  =  "P9"
               CALL "C3_Set_Jrcode" USING 
               USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
           IF  ESTAT  NOT =  "01"  AND  "06"
               GO  TO  INI-510
           END-IF
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p" RETURNING RESU.
           IF  OKC  NOT =  "1"  AND  "9"
               GO  TO  INI-510
           END-IF
           IF  OKC  =  "9"
               CALL "C3_Set_Jrcode" USING 
               USER_ID BY REFERENCE COMPLETION_CODE  255
               GO  TO  INI-EX
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO JT-YZAI_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JMST3_PNAME1 "SHARED" BY REFERENCE JMST3_IDLST "1"
            "JMST3-KEY" BY REFERENCE JMST3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           IF  JS-SIGN  = 0
               CALL "DB_F_Open" USING
                "INPUT" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
                "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
                HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3
           END-IF
           CALL "DB_F_Open" USING
            "OUTPUT" JT-YZAI_PNAME1 "EXCLUSIVE" BY REFERENCE
            JT-YZAI_IDLST "0".
       INI-EX.
            EXIT.
       UPD1-RTN.
           INITIALIZE                 NJZAI-KEY.
           MOVE  9                TO  NJZAI-01.
      *           START  NJZAI  KEY  NOT <  NJZAI-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            NJZAI_PNAME1 "NJZAI-KEY" " NOT < " NJZAI-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF.
       UPD1-010.
      *           READ  NJZAI  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD1-EX
           END-IF
           IF  NJZAI-02 < 100000
               GO TO UPD1-010
           END-IF
           MOVE  NJZAI-02     TO  HI-MHCD HI-HCD.
      *           READ  HI-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO UPD1-010
           END-IF
           IF  JS-SIGN = 0
               IF  HI-BC1 NOT = 26
                   GO TO UPD1-010
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HI-BC1 NOT = 33
                   GO TO UPD1-010
               END-IF
           END-IF.
       UPD1-020.
           PERFORM  ZC1-RTN      THRU  ZC1-EX.
           IF  ZERO-SW  =  0
               GO  TO  UPD1-010
           END-IF
           MOVE  SPACE     TO  YZAI-R.
           INITIALIZE  YZAI-R.
           MOVE  NJZAI-02  TO  YZAI-HCD.
           MOVE  HI-NAME   TO  YZAI-HNA.
           MOVE "在　庫" TO  YZAI-KBN.
           MOVE  NJZAI-03  TO  YZAI-SIZ.
           MOVE  1         TO  YZAI-KC.
           PERFORM  MOV-RTN       THRU  MOV-EX.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD1-010.
       UPD1-EX.
           EXIT.
       UPD2-RTN.
           INITIALIZE                 JMST3-KEY.
      *           START  JMST3  KEY  NOT <  JMST3-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            JMST3_PNAME1 "JMST3-KEY" " NOT < " JMST3-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-EX
           END-IF.
       UPD2-010.
      *           READ  JMST3  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" JMST3_PNAME1 BY REFERENCE JMST3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD2-EX
           END-IF
           IF  JMST3-01  NOT =  5  AND  6
               GO  TO  UPD2-010
           END-IF
           MOVE  JMST3-03     TO  HI-MHCD HI-HCD.
      *           READ  HI-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO UPD2-010
           END-IF
           IF  JS-SIGN = 0
               IF  HI-BC1 NOT = 26
                   GO TO UPD2-010
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  HI-BC1 NOT = 33
                   GO TO UPD2-010
               END-IF
           END-IF.
       UPD2-020.
           PERFORM  ZC2-RTN      THRU  ZC2-EX.
           IF  ZERO-SW  =  0
               GO  TO  UPD2-010
           END-IF
           MOVE  SPACE     TO  YZAI-R.
           INITIALIZE  YZAI-R.
           MOVE  JMST3-03  TO  YZAI-HCD.
           MOVE  HI-NAME   TO  YZAI-HNA.
           MOVE "受注残" TO  YZAI-KBN.
           MOVE  JMST3-09  TO  YZAI-SIZ.
           MOVE  3         TO  YZAI-KC.
           PERFORM  MOV-RTN       THRU  MOV-EX.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD2-010.
       UPD2-EX.
           EXIT.
       UPD3-RTN.
           INITIALIZE                 HSH-KEY.
      *           START  HSHF  KEY  NOT <  HSH-KEY  INVALID
      *///////////////
           CALL "DB_Start" USING
            HSHF_PNAME1 "HSH-KEY" " NOT < " HSH-KEY RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-EX
           END-IF.
       UPD3-010.
      *           READ  HSHF  NEXT  UNLOCK  AT  END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  UPD3-EX
           END-IF
           IF  HSH-ENGP NOT =  ZERO
               GO  TO  UPD3-010
           END-IF
           MOVE 0 TO W-SIZ.
       UPD3-020.
           ADD 1 TO W-SIZ.
           IF  W-SIZ = 5
               GO  TO  UPD3-010
           END-IF.
       UPD3-030.
           PERFORM  ZC3-RTN      THRU  ZC3-EX.
           IF  ZERO-SW  =  0
               GO  TO  UPD3-020
           END-IF
           MOVE  HSH-HCD    TO  HI-HCD.
      *           READ  HI-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO UPD3-010
           END-IF
           MOVE  HI-MHCD    TO  W-MHCD  W-HCD.
           MOVE  W-KEY      TO  HI-KEY2.
      *           READ  HI-M  UNLOCK  INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO UPD3-010
           END-IF
           MOVE  SPACE     TO  YZAI-R.
           INITIALIZE  YZAI-R.
           MOVE  HI-MHCD TO  YZAI-HCD.
           MOVE  HI-NAME   TO  YZAI-HNA.
           MOVE "入庫残" TO  YZAI-KBN.
           MOVE  W-SIZ  TO  YZAI-SIZ.
           MOVE  2         TO  YZAI-KC.
           PERFORM  MOV-RTN       THRU  MOV-EX.
           PERFORM  WRI-RTN       THRU  WRI-EX.
           GO  TO  UPD3-020.
       UPD3-EX.
           EXIT.
       END-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-YZAI_IDLST JT-YZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMST3_IDLST JMST3_PNAME1.
           IF  JS-SIGN  =  0
               CALL "DB_F_Close" USING
                BY REFERENCE HSHF_IDLST HSHF_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
       END-EX.
           EXIT.
       WRI-RTN.
      *           WRITE    YZAI-R.
      *//////////////
           CALL "DB_Insert" USING
            JT-YZAI_PNAME1 JT-YZAI_LNAME YZAI-R RETURNING RET.
           IF  ERR-STAT  NOT = "00"
               MOVE  "W"          TO  ERR-M
               MOVE  "WK0128"     TO  ERR-F
               PERFORM  ERR-RTN   THRU  ERR-EX
           END-IF.
       WRI-EX.
           EXIT.
       ZC1-RTN.
           MOVE  ZERO  TO  W-SUD.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC1-010.
           IF  I  >  10
               GO  TO  ZC1-EX
           END-IF
           COMPUTE  W-SU(I)  =  NJZAI-0411(I)  -  NJZAI-0511(I)
                             +  NJZAI-0611(I)  +  NJZAI-0711(I)
                             -  NJZAI-0811(I)  +  NJZAI-1111(I)
                                               -  NJZAI-0911(I).
           IF  (ZERO-SW   = 0)  AND  (W-SU(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC1-010.
       ZC1-EX.
           EXIT.
       ZC2-RTN.
           MOVE  ZERO  TO  W-SUD.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC2-010.
           IF  I  >  10
               GO  TO  ZC2-EX
           END-IF
           COMPUTE  W-SU(I)  =  JMST3-1111(I)  -  JMST3-1211(I)
                             -  JMST3-141 (I)  -  JMST3-151 (I).
           IF  (ZERO-SW   = 0)  AND  (W-SU(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC2-010.
       ZC2-EX.
           EXIT.
       ZC3-RTN.
           MOVE  ZERO  TO  W-SUD.
           MOVE  0     TO  ZERO-SW.
           MOVE  1     TO  I.
       ZC3-010.
           IF  I  >  10
               GO  TO  ZC3-EX
           END-IF
           COMPUTE  W-SU(I)  =  HSH-HSU(W-SIZ,I)  -  HSH-NSU(W-SIZ,I)
                             -  HSH-ISU(W-SIZ,I).
           IF  (ZERO-SW   = 0)  AND  (W-SU(I)  NOT =  ZERO)
               MOVE   1     TO  ZERO-SW
           END-IF
           ADD  1     TO  I.
           GO  TO  ZC3-010.
       ZC3-EX.
           EXIT.
       MOV-RTN.
           MOVE  ZERO  TO  W-SUT.
           MOVE  1     TO  I.
       MOV-010.
           IF  I  >  10
               GO  TO  MOV-020
           END-IF
           MOVE   W-SU(I)     TO  YZAI-SU(I).
           ADD    W-SU(I)     TO  W-SUT.
           ADD  1     TO  I.
           GO  TO  MOV-010.
       MOV-020.
           MOVE   W-SUT       TO  YZAI-TSU.
       MOV-EX.
           EXIT.
       COPY  LPMSG.
