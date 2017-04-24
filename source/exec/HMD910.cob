       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD910.
       AUTHOR. S-NAKAO.
       DATE-WRITTEN. 1977-04-07.
      ***************************************************************
      *    PROGRAM         :  売上・消費税計算　チェックリスト　　  *
      *    PRINTER TYPE    :  JIPS                                  *
      *    SCREEN          :  ******                                *
      *        変更　　　  :  62/04/15                              *
      *    COMPILE TYPE    :  COBOL                                 *
      ***************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(022) VALUE
                "＊＊＊　　売上・消費税計算　リスト　　＊＊＊".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(005) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "日　付".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(003) VALUE "伝票№".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(007) VALUE "得　意　先　名".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  N(003) VALUE "消費税".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売上金額".
           02  F              PIC  X(010) VALUE SPACE.
       01  W-P.
           02  P-15K          PIC  X(005).
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-DNO          PIC  9(006).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  P-SHZ          PIC --,---,--9.
           02  P-KIN          PIC ----,---,--9.
           02  F              PIC  X(002).
           02  P-UNM          PIC  N(002).
           02  P-F            PIC  X(001).
           02  P-CSM          PIC  N(002).
           02  P-R            PIC  X(001).
       01  W-D.
           02  W-DNO          PIC  9(006).
           02  W-DC           PIC  9(001).
           02  W-DATE         PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-DATED        PIC  9(006).
           02  W-TCDD         PIC  9(004).
           02  W-CC           PIC  9(001).
           02  W-SNC          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-SHZ          PIC S9(006).
           02  W-KIN          PIC S9(008).
           02  W-SEPEY.
             03  W-SPEY       PIC  9(002).
             03  W-EPEY       PIC  9(002).
           02  W-SETCD.
             03  W-STCD       PIC  9(004).
             03  W-ETCD       PIC  9(004).
           02  W-DMM          PIC  9(001).
       01  WT-D.
           02  WT-SHZ         PIC S9(007).
           02  WT-KIN         PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LSPF.
      *FD  SNTRF
       01  SNTRF_HMD910.
           02  SNTRF_PNAME1   PIC  X(005) VALUE "SNTRF".
           02  F              PIC  X(001).
           02  SNTRF_LNAME    PIC  X(012) VALUE "SNTRF_HMD910".
           02  F              PIC  X(001).
           02  SNTRF_KEY1     PIC  X(100) VALUE SPACE.
           02  SNTRF_SORT     PIC  X(100) VALUE SPACE.
           02  SNTRF_IDLST    PIC  X(100) VALUE SPACE.
           02  SNTRF_RES      USAGE  POINTER.
       01  SNTR-R.
           02  SNTR-DNO       PIC  9(006).
           02  SNTR-GNO       PIC  9(001).
           02  F              PIC  9(002).
           02  SNTR-DATE      PIC  9(006).
           02  SNTR-NGP   REDEFINES SNTR-DATE.
             03  SNTR-NEN     PIC  9(002).
             03  SNTR-GET     PIC  9(002).
             03  SNTR-PEY     PIC  9(002).
           02  SNTR-TCD       PIC  9(004).
           02  SNTR-D1.
             03  F            PIC  X(047).
             03  SNTR-KIN     PIC S9(008).
             03  SNTR-CC      PIC  9(001).
             03  SNTR-DC      PIC  9(001).
             03  F            PIC  X(051).
           02  SNTR-D2   REDEFINES SNTR-D1.
             03  F            PIC  X(084).
             03  SNTR-SHZ     PIC S9(007).
             03  F            PIC  9(017).
           02  SNTR-SNC       PIC  9(001).
       77  F                  PIC  X(001).
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　消費税計算　リスト　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(019) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(032) VALUE
                "ＰＲＩＮＴ  範囲      日 ～   日".
           02  FILLER  PIC  X(032) VALUE
                "        得意先ｺｰﾄﾞ       ～     ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-SPEY  PIC  9(002).
             03  A-EPEY  PIC  9(002).
           02  FILLER.
             03  A-STCD  PIC  9(004).
             03  A-ETCD  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "352" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "38" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "38" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "38" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "38" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "38" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "38" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "38" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "13" "32" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "13" "13" "32" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "18" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "13" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "12" "0" "4" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SPEY" "9" "12" "33" "2" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SPEY" BY REFERENCE W-SPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EPEY" "9" "12" "41" "2" "A-SPEY" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EPEY" BY REFERENCE W-EPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "13" "0" "8" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "13" "33" "4" " " "02C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "13" "41" "4" "A-STCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "35" "1" "02C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "79" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "79" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SPEY "A-SPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-SPEY > 31
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EPEY "A-EPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SPEY > W-EPEY
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-STCD > W-ETCD
               GO TO M-25
           END-IF.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-25
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-30
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" SNTRF_PNAME1 " " BY REFERENCE SNTRF_IDLST "0".
       M-35.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE SNTRF_IDLST SNTRF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  SNTR-PEY < W-SPEY OR > W-EPEY
               GO TO M-35
           END-IF
           IF  SNTR-TCD < W-STCD OR > W-ETCD
               GO TO M-35
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO WT-D W-PAGE W-DATE W-TCD.
           PERFORM S-10 THRU S-15.
       M-40.
           MOVE SNTR-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　マスター　なし　＊＊　" TO T-NAME
           END-IF
           MOVE SNTR-DNO TO W-DNO.
           MOVE ZERO TO W-KIN W-SHZ W-DC.
           IF  SNTR-GNO NOT = 9
               MOVE SNTR-DC TO W-DC
               MOVE SNTR-CC TO W-CC
           END-IF
           MOVE SNTR-SNC TO W-SNC.
           MOVE SNTR-DATE TO W-DATED.
           MOVE SNTR-TCD TO W-TCDD.
       M-45.
           IF  SNTR-GNO = 9
               ADD SNTR-SHZ TO W-SHZ
           ELSE
               IF  SNTR-DC NOT = 4 AND 8 AND 9
                   ADD SNTR-KIN TO W-KIN
               END-IF
           END-IF.
       M-50.
      *           READ SNTRF AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" SNTRF_PNAME1 BY REFERENCE SNTR-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  SNTR-PEY < W-SPEY OR > W-EPEY
               GO TO M-50
           END-IF
           IF  SNTR-TCD < W-STCD OR > W-ETCD
               GO TO M-50
           END-IF
           IF  SNTR-DNO = W-DNO
               GO TO M-45
           END-IF
           PERFORM S-20 THRU S-25.
           GO TO M-40.
       M-90.
           PERFORM S-20 THRU S-25.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-UNM.
           MOVE W-15K TO P-15K.
           MOVE "　［　　ＡＬＬ　ＴＯＴＡＬ　　］　" TO P-NAME.
           MOVE WT-SHZ TO P-SHZ.
           MOVE WT-KIN TO P-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING
            BY REFERENCE SNTRF_IDLST SNTRF_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
       S-20.
           IF  W-SNC = 1
               COMPUTE W-SHZ = -1 * W-SHZ
               COMPUTE W-KIN = -1 * W-KIN
           END-IF
           IF  W-DC = 1 OR 2 OR 5
               COMPUTE W-KIN = -1 * W-KIN
           END-IF
      *
           MOVE SPACE TO W-P.
           MOVE W-15K TO P-15K.
           MOVE SPACE TO P-NAME P-UNM P-CSM.
           IF  W-DATED NOT = W-DATE
               MOVE W-DATED TO W-DATE P-DATE
           END-IF
           MOVE W-DNO TO P-DNO.
           IF  W-TCDD NOT = W-TCD
               MOVE W-TCDD TO W-TCD P-TCD
               MOVE T-NAME TO P-NAME
           END-IF
           IF  W-SNC = 0
               MOVE "売上" TO P-UNM
           ELSE
               MOVE "値引" TO P-UNM
           END-IF
           MOVE W-SHZ TO P-SHZ.
           MOVE W-KIN TO P-KIN.
           IF  W-CC = 9
               MOVE "(" TO P-F
               MOVE "調整" TO P-CSM
               MOVE ")" TO P-R
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE W-DATED TO W-DATE P-DATE
               MOVE W-TCDD TO W-TCD P-TCD
               MOVE T-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD W-SHZ TO WT-SHZ.
           ADD W-KIN TO WT-KIN.
       S-25.
           EXIT.
