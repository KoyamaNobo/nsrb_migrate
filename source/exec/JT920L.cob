       IDENTIFICATION                 DIVISION.
       PROGRAM-ID.                    JT220L.
       AUTHOR.                        E-SHIGIHARA.
      ***************************************************
      *    PROGRAM        : 受注日得意先別　受注残リスト*
      *    DATA WRITTEN   :                             *
      *    SCREEN USED    : UNUSED                      *
      *    FORM   USED    : UNUSED                      *
      *    PRINTER TYPE   : JIPS                        *
      *    COMPILE TYPE   : COBOL                       *
      ***************************************************
       ENVIRONMENT                    DIVISION.
       CONFIGURATION                  SECTION.
       SOURCE-COMPUTER.               SYSTEM150.
       OBJECT-COMPUTER.               SYSTEM150.
       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77  ERR-STAT                  PIC X(02)    VALUE SPACE.
       77  WK0256ID                  PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1               PIC  X(003).
           02  STN-NO2               PIC  X(003).
       01  W-FID.
           02  W-FID1                PIC  X(006) VALUE "WK0256".
           02  W-FID2                PIC  X(003).
       01  W-P1.
           02  P-15K1                PIC X(05).
           02  P-DATE                PIC 99/99/99.
           02  F                     PIC X(01).
           02  P-KBN                 PIC N(02).
           02  P-TCD                 PIC 9(04).
           02  F                     PIC X(01).
           02  P-TNA                 PIC N(26).
           02  P-TEN                 PIC 9(04).
           02  P-TEND  REDEFINES P-TEN.
             03  F                   PIC X(01).
             03  P-CCD               PIC 9(03).
           02  F                     PIC X(01).
           02  P-CNA                 PIC N(26).
           02  F                     PIC X(01).
           02  P-NKD                 PIC 99/99/99.
           02  F                     PIC X(12).
           02  P-TEK1                PIC N(06).
           02  P-TEK2                PIC N(02).
           02  P-20K1                PIC X(05).
       01  W-P2.
           02  P-15K2                PIC X(05).
           02  F                     PIC X(04).
           02  P-JNO                 PIC 9(06).
           02  P-V                   PIC X(01).
           02  P-GNO                 PIC 9(01).
           02  F                     PIC X(01).
           02  P-HCD                 PIC 9(06).
           02  F                     PIC X(01).
           02  P-HNA                 PIC N(24).
           02  F                     PIC X(01).
           02  P-SIZ                 PIC 9(01).
           02  P-SUD       OCCURS   10.
             03  P-SU                PIC ---,---.
           02  P-SUT                 PIC ----,--9.
           02  P-20K2                PIC X(05).
       01  HEAD1.
           02  W-20K                 PIC X(05)  VALUE  X"1A24212474".
           02  F                     PIC X(40)  VALUE   SPACE.
           02  F                     PIC N(24)  VALUE
                "＊＊＊　　受注日得意先別　受注残リスト　　＊＊＊".
           02  F                     PIC X(26)  VALUE  SPACE.
           02  F                     PIC X(05)  VALUE  "DATE ".
           02  H-DATE                PIC 99B99B99.
           02  F                     PIC X(07)  VALUE  "     P.".
           02  H-PAGE                PIC Z9.
       01  HEAD2.
           02  W-15K                 PIC X(05)  VALUE  X"1A24212078".
           02  F                     PIC X(01)  VALUE  SPACE.
           02  F                     PIC N(04)  VALUE  "　受注日".
           02  F                     PIC X(05)  VALUE  SPACE.
           02  F                     PIC X(05)  VALUE  "ｺｰﾄﾞ ".
           02  F                     PIC N(08)  VALUE
                 "得　意　先　名　".
           02  F                     PIC X(28)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "ｺｰﾄﾞ".
           02  F                     PIC N(08)  VALUE
                 "直送先・店名　　".
           02  F                     PIC X(29)  VALUE  SPACE.
           02  F                     PIC N(04)  VALUE  "　納　期".
           02  F                     PIC X(13)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "摘要".
           02  F                     PIC X(12)  VALUE  SPACE.
       01  HEAD3.
           02  F                     PIC X(05)  VALUE  SPACE.
           02  F                     PIC N(04)  VALUE  "　受注№".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(06)  VALUE  "ｺｰﾄﾞ  ".
           02  F                     PIC N(06)  VALUE
                 "品　　　名　".
           02  F                     PIC X(28)  VALUE  SPACE.
           02  F                     PIC X(01)  VALUE  "1".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "３号".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "２号".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "１号".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "０号".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "　中".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "　大".
           02  F                     PIC X(04)  VALUE  SPACE.
           02  F                     PIC N(02)  VALUE  "特大".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "28.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "29.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "30.0".
           02  F                     PIC X(08)  VALUE  SPACE.
       01  HEAD4.
           02  F                     PIC X(57)  VALUE  SPACE.
           02  F                     PIC X(01)  VALUE  "2".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "12.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "13.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "13.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "14.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "15.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "16.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "17.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "18.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "19.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "20.0".
           02  F                     PIC X(08)  VALUE  SPACE.
       01  HEAD5.
           02  F                     PIC X(57)  VALUE  SPACE.
           02  F                     PIC X(01)  VALUE  "3".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "21.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "21.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "22.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "22.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "23.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "23.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "24.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "24.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "25.0".
           02  F                     PIC X(15)  VALUE  SPACE.
       01  HEAD6.
           02  F                     PIC X(57)  VALUE  SPACE.
           02  F                     PIC X(01)  VALUE  "4".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "24.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "24.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "25.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "25.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "26.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "26.5".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "27.0".
           02  F                     PIC X(03)  VALUE  SPACE.
           02  F                     PIC X(04)  VALUE  "27.5".
           02  F                     PIC X(16)  VALUE  SPACE.
           02  F                     PIC N(04)  VALUE  "　合　計".
       01  W-DATA.
           02  W-DMM                 PIC 9(01).
           02  W-DATE                PIC 9(08).
           02  W-DATEL  REDEFINES  W-DATE.
             03  F                   PIC 9(02).
             03  W-DATES             PIC 9(06).
           02  W-MD.
             03  W-KBN               PIC 9(01).
             03  W-TCD               PIC 9(04).
             03  W-TNA               PIC N(26).
             03  W-CCD               PIC 9(03).
             03  W-TEN               PIC 9(04).
             03  W-CNA               PIC N(26).
             03  W-NKD               PIC 9(08).
             03  W-NKDL  REDEFINES  W-NKD.
               04  F                 PIC 9(02).
               04  W-NKDS            PIC 9(06).
             03  W-TEK1              PIC N(06).
             03  W-TEK2              PIC N(02).
             03  W-JNO               PIC 9(06).
             03  W-D  OCCURS  6.
               04  W-GNO             PIC 9(01).
               04  W-HCD             PIC 9(06).
               04  W-HNA             PIC N(24).
               04  W-SIZ             PIC 9(01).
               04  W-SUD.
                 05  W-SU            PIC S9(06)  OCCURS  10.
             03  W-SUT               PIC S9(06).
           02  W-ZSU                 PIC S9(06).
           02  CNT.
             03  CNT1                PIC 9(02).
             03  CNT2                PIC 9(02).
             03  CNT3                PIC 9(02).
           02  CHK                   PIC 9(01).
           02  W-TC                  PIC 9(01).
           02  W-HC                  PIC 9(01).
           02  W-PAGE                PIC 9(02).
      *
           COPY  LTWK04.
           COPY  LITCM.
           COPY  LIHIM2.
           COPY  LWTNAF.
      *FD  SP-F
       77  SP-R                      PIC X(256).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  LINAGECOUNTER      PIC  9(003).
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  CLE-01.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  DSP-AREA.
           02  DSP-01.
               03  FILLER  PIC  X(30) 
                       VALUE  "                              ".
               03  FILLER  PIC  X(28) 
                       VALUE  "受注日得意先別　受注残リスト".
           02  DSP-07.
               03  FILLER  PIC  X(06) VALUE  "確認（".
               03  FILLER  PIC  X(09) VALUE  "OK=1,NO=9".
               03  FILLER  PIC  X(02) VALUE  "）".
               03  FILLER  PIC  X(08) VALUE  "--> ﾘﾀｰﾝ".
       01  ACP-AREA.
           02  ACP-DMM     PIC 9(01).
       PROCEDURE               DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "P1-999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *CLE-01
       CALL "SD_Init" USING
           "CLE-01" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01CLE-01" "X" "1" "0" "12" " " "CLE-01" RETURNING RESU.
      *DSP-AREA
       CALL "SD_Init" USING 
            "DSP-AREA" " " "0" "0" "83" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-01" " " "1" "0" "58" " " "DSP-AREA"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-01" "RX" "1" "23" "30" " " "DSP-01" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-01" "X" "1" "24" "28" "01DSP-01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DSP-07" " " "23" "0" "25" "DSP-01" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01DSP-07" "X" "23" "41" "6" " " "DSP-07"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02DSP-07" "X" "23" "47" "9" "01DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DSP-07" "X" "23" "56" "2" "02DSP-07" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DSP-07" "X" "23" "58" "8" "03DSP-07" " " RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING 
            "ACP-AREA" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "ACP-DMM" "9" "23" "61" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Into" USING 
            "ACP-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU.
           CALL "SD_Output" USING
            "DSP-AREA" DSP-AREA "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE ACP-DMM "ACP-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO M-95
           END-IF
           IF  ESTAT NOT = "01" AND "06"
               GO TO M-10
           END-IF
           IF  W-DMM =  9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0256ID.
           MOVE WK0256ID TO JT-WK04_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" JT-WK04_PNAME1 "SHARED" BY REFERENCE
            JT-WK04_IDLST "0".
      *
      *           READ JT-WK04 AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1
               CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-PAGE.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-10 THRU S-15.
       M-15.
           MOVE WK04-02 TO W-DATE.
           MOVE ZERO TO CHK.
       M-20.
           MOVE ZERO TO W-MD.
           MOVE SPACE TO W-TNA W-CNA
                         W-HNA(01) W-HNA(02) W-HNA(03) W-HNA(04)
                         W-HNA(05) W-HNA(06).
           MOVE WK04-01 TO W-KBN.
           MOVE WK04-06 TO W-NKD.
           MOVE WK04-07 TO W-JNO.
           MOVE WK04-04 TO W-TCD.
           MOVE WK04-10 TO W-CCD.
           MOVE WK04-23 TO W-TEN.
           MOVE WK04-801 TO W-TEK1.
           MOVE WK04-802 TO W-TEK2.
      *
           MOVE W-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "　" TO TC-NAME
           END-IF
           MOVE TC-NAME TO W-TNA.
           MOVE W-TCD TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "　" TO TC-NAME
           END-IF
           MOVE TC-NAME TO W-CNA.
           IF  W-CCD = 001
               MOVE SPACE TO W-CNA
           END-IF
           IF  WK04-04   NOT =  9850
               GO  TO  M-24
           END-IF
           IF  WK04-23       =  ZERO
               GO  TO  M-24
           END-IF
           MOVE W-TEN TO WTNA-KEY.
      *           READ WTNAF UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "　" TO WTNA-NAME
           END-IF
           MOVE WTNA-NAME TO W-CNA.
       M-24.
           MOVE ZERO TO CNT1 W-TC.
       M-25.
           ADD 1 TO CNT1.
           IF  CNT1 > 6
               MOVE 6 TO W-TC
               GO TO M-40
           END-IF
           MOVE WK04-08 TO W-GNO(CNT1).
           MOVE WK04-05 TO W-HCD(CNT1).
           MOVE WK04-05 TO HI-MHCD HI-HCD.
      *           READ HI2-M UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ALL "　" TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-HNA(CNT1).
           MOVE WK04-09 TO W-SIZ(CNT1).
           MOVE ZERO TO CNT2.
       M-30.
           ADD 1 TO CNT2.
           IF  CNT2 NOT > 10
               COMPUTE W-ZSU = WK04-1111(CNT2) - WK04-1211(CNT2) -
                               WK04-141(CNT2)  - WK04-151(CNT2)
               MOVE W-ZSU TO W-SU(CNT1,CNT2)
               ADD W-ZSU TO W-SUT
               GO TO M-30
           END-IF.
       M-35.
      *           READ JT-WK04 AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" JT-WK04_PNAME1 BY REFERENCE WK04-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE CNT1 TO W-TC
               GO TO M-80
           END-IF
           IF  W-JNO = WK04-07
               GO TO M-25
           END-IF
           MOVE CNT1 TO W-TC.
       M-40.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-50.
           IF  W-DATE = WK04-02
               GO TO M-20
           END-IF
           GO TO M-15.
       M-80.
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-50.
       M-90.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-WK04_IDLST JT-WK04_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "CLE-01" CLE-01 "p" RETURNING RESU
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
      *
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P-15K1.
           MOVE W-20K TO P-20K1.
           MOVE ALL "　" TO P-KBN P-TNA P-CNA.
           IF  CHK = 0
               MOVE 1 TO CHK
               MOVE W-DATES TO P-DATE
           END-IF
           IF  W-KBN = 0
               MOVE "受　" TO P-KBN
           END-IF
           IF  W-KBN = 5
               MOVE "預　" TO P-KBN
           END-IF
           IF  W-KBN = 6
               MOVE "取　" TO P-KBN
           END-IF
           MOVE W-TCD TO P-TCD.
           MOVE W-TNA TO P-TNA.
           IF (W-TCD = 9850) AND (W-TEN NOT = ZERO)
               MOVE W-TEN TO P-TEN
               MOVE W-CNA TO P-CNA
           ELSE
               IF  W-CCD NOT = 001
                   MOVE W-CCD TO P-CCD
                   MOVE W-CNA TO P-CNA
               END-IF
           END-IF
           IF  W-NKD NOT = ZERO
               MOVE W-NKDS TO P-NKD
           END-IF
           MOVE W-TEK1 TO P-TEK1.
           MOVE W-TEK2 TO P-TEK2.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           MOVE ZERO TO CNT1 W-HC.
       S-35.
           ADD 1 TO CNT1.
           IF  CNT1 > 6
               GO TO S-50
           END-IF
           IF  W-GNO(CNT1) = ZERO
               GO TO S-50
           END-IF
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P-15K2.
           MOVE W-20K TO P-20K2.
           MOVE SPACE TO P-HNA.
           MOVE W-JNO TO P-JNO.
           MOVE "-" TO P-V.
           MOVE W-GNO(CNT1) TO P-GNO.
           MOVE W-SIZ(CNT1) TO P-SIZ.
           MOVE ZERO TO CNT2.
       S-40.
           ADD 1 TO CNT2.
           IF  CNT2 NOT > 10
               MOVE W-SU(CNT1,CNT2) TO P-SU(CNT2)
               GO TO S-40
           END-IF
           IF  CNT1 = W-TC
               MOVE W-SUT TO P-SUT
           END-IF
           IF  CNT1 NOT = 1
               COMPUTE CNT3 = CNT1 - 1
               IF  W-HCD(CNT1) = W-HCD(CNT3)
                   IF  W-HC = 0
                       GO TO S-45
                   END-IF
               END-IF
           END-IF
           MOVE W-HCD(CNT1) TO P-HCD.
           MOVE W-HNA(CNT1) TO P-HNA.
       S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               MOVE 0 TO CHK
               MOVE 1 TO W-HC
               PERFORM S-05 THRU S-15
               PERFORM S-20 THRU S-25
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO S-35.
       S-50.
           EXIT.
