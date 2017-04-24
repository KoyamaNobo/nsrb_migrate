       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KHG350.
      *********************************************************
      *    PROGRAM         :  防振ゴム　不良統計表　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *        変更　　　  :  62/04/07                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0064ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0064".
           02  W-FID2         PIC  X(003).
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(018) VALUE
                "＊＊＊　　工品　不良統計表　　＊＊＊".
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99/99/99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(003) VALUE "品　名".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "不良名".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
           02  F              PIC  X(013) VALUE "   :   ｺｰﾄﾞ  ".
           02  F              PIC  N(003) VALUE "品　名".
           02  F              PIC  X(015) VALUE SPACE.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(003) VALUE "不良名".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(003) VALUE "数　量".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(003) VALUE "金　額".
       01  W-P.
           02  W-P1    OCCURS  58.
             03  P-HCD1       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME1      PIC  X(020).
             03  F            PIC  X(001).
             03  P-NHC1       PIC  9(002).
             03  F            PIC  X(001).
             03  P-15K1       PIC  X(005).
             03  P-FRN1       PIC  N(006).
             03  P-20K1       PIC  X(005).
             03  P-SU1        PIC --,---,--9.
             03  P-KIN1       PIC ----,---,--9.
             03  F            PIC  X(003).
             03  P-X          PIC  X(001).
             03  F            PIC  X(003).
             03  P-HCD2       PIC  X(005).
             03  F            PIC  X(001).
             03  P-NAME2      PIC  X(020).
             03  F            PIC  X(001).
             03  P-NHC2       PIC  9(002).
             03  F            PIC  X(001).
             03  P-15K2       PIC  X(005).
             03  P-FRN2       PIC  N(006).
             03  P-20K2       PIC  X(005).
             03  P-SU2        PIC --,---,--9.
             03  P-KIN2       PIC ----,---,--9.
       01  W-DATA.
           02  W-SEHCD.
             03  W-SHCD       PIC  X(005).
             03  W-EHCD       PIC  X(005) VALUE "99999".
           02  W-DMM          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-LD           PIC  9(002).
           02  W-CD           PIC  9(001).
           02  W-PC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-YC           PIC  9(002).
           02  W-HCD          PIC  X(005).
           02  W-NHC          PIC  9(002).
           02  W-NAME         PIC  X(020).
       01  W-D.
           02  W-SU           PIC S9(007).
           02  W-KIN          PIC S9(009).
       01  WT-D.
           02  WT-SU          PIC S9(007).
           02  WT-KIN         PIC S9(009).
       01  WS-D.
           02  WS-DD   OCCURS  30.
             03  WS-SU        PIC S9(007).
             03  WS-KIN       PIC S9(009).
       01  WA-D.
           02  WA-DD   OCCURS  30.
             03  WA-SU        PIC S9(007).
             03  WA-KIN       PIC S9(009).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKKBM.
           COPY LSPF.
      *FD  KNHR-F
       01  KNHR-F_KHG350.
           02  KNHR-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  KNHR-F_LNAME   PIC  X(013) VALUE "KNHR-F_KHG350".
           02  F              PIC  X(001).
           02  KNHR-F_KEY1    PIC  X(100) VALUE SPACE.
           02  KNHR-F_SORT    PIC  X(100) VALUE SPACE.
           02  KNHR-F_IDLST   PIC  X(100) VALUE SPACE.
           02  KNHR-F_RES     USAGE  POINTER.
       01  K-R.
           02  K-NHC          PIC  9(002).
           02  F              PIC  X(008).
           02  K-HCD          PIC  X(005).
           02  F              PIC  X(007).
           02  K-SU           PIC S9(006)V9(02).
           02  F              PIC  X(008).
           02  K-KIN          PIC S9(008).
           02  K-YC           PIC  9(002).
           02  F              PIC  X(016).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　工品　不良　統計表　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(035) VALUE
                "(  ｺｰﾄﾞ       ﾖﾘ 99999 ﾏﾃﾞ PRINT  )".
           02  FILLER  PIC  X(028) VALUE
                "<  確認  OK=1 NO=9   ﾘﾀｰﾝ  >".
       01  C-ACP.
           02  FILLER.
             03  A-SHCD  PIC  X(005).
             03  A-EHCD  PIC  X(005).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "357" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "42" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "42" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "42" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "42" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "42" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "42" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "42" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "17" "14" "35" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "22" "17" "28" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "11" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "17" "0" "10" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHCD" "X" "17" "22" "5" " " "01C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHCD" BY REFERENCE W-SHCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EHCD" "X" "17" "31" "5" "A-SHCD" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EHCD" BY REFERENCE W-EHCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "37" "1" "01C-ACP" " "  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "28" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "28" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-SHCD "A-SHCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-EHCD "A-EHCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-SHCD > W-EHCD
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-20
           END-IF
      *
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0064ID.
           MOVE WK0064ID TO KNHR-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KNHR-F_PNAME1 " " BY REFERENCE KNHR-F_IDLST "0".
       M-25.
      *           READ KNHR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNHR-F_PNAME1 BY REFERENCE K-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  K-NHC = ZERO
               GO TO M-25
           END-IF
           IF  K-HCD < W-SHCD OR > W-EHCD
               GO TO M-25
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           PERFORM SPC-RTN THRU SPC-EX.
           MOVE ZERO TO WA-D W-LD W-CD W-PC W-PAGE.
       M-30.
           MOVE K-YC TO W-YC.
           MOVE ZERO TO WS-D.
       M-35.
           MOVE K-HCD TO W-HCD.
           MOVE W-HCD TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "    **  KHM ﾅｼ  **  " TO KH-NAME
           END-IF
           MOVE ZERO TO WT-D CHK CNT.
       M-40.
           MOVE K-NHC TO W-NHC.
           PERFORM KKB-RTN THRU KKB-EX.
           MOVE ZERO TO W-D.
       M-45.
           ADD K-SU TO W-SU.
           ADD K-KIN TO W-KIN.
       M-50.
      *           READ KNHR-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNHR-F_PNAME1 BY REFERENCE K-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-70
           END-IF
           IF  K-NHC = ZERO
               GO TO M-50
           END-IF
           IF  K-HCD < W-SHCD OR > W-EHCD
               GO TO M-50
           END-IF
           IF  K-YC NOT = W-YC
               GO TO M-60
           END-IF
           IF  K-HCD NOT = W-HCD
               GO TO M-55
           END-IF
           IF  K-NHC = W-NHC
               GO TO M-45
           END-IF
           IF (W-SU NOT = ZERO) OR (W-KIN NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           GO TO M-40.
       M-55.
           IF (W-SU NOT = ZERO) OR (W-KIN NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-35.
       M-60.
           PERFORM TOT-RTN THRU TOT-EX.
           GO TO M-30.
       M-70.
           PERFORM TOT-RTN THRU TOT-EX.
           MOVE "【　総　合　計　】  " TO W-NAME.
           MOVE ZERO TO W-NHC WT-D CHK CNT.
       M-75.
           ADD 1 TO W-NHC.
           IF  W-NHC = 31
               GO TO M-80
           END-IF
           IF  ZERO = WA-SU(W-NHC) AND WA-KIN(W-NHC)
               GO TO M-75
           END-IF
           PERFORM KKB-RTN THRU KKB-EX.
           PERFORM LIN-RTN THRU LIN-EX.
           ADD WA-SU(W-NHC) TO WT-SU.
           ADD WA-KIN(W-NHC) TO WT-KIN.
           ADD 1 TO CNT.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
               MOVE W-NHC TO P-NHC1(W-LD)
               MOVE KKB-FRN TO P-FRN1(W-LD)
               MOVE WA-SU(W-NHC) TO P-SU1(W-LD)
               MOVE WA-KIN(W-NHC) TO P-KIN1(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE W-NHC TO P-NHC2(W-LD)
               MOVE KKB-FRN TO P-FRN2(W-LD)
               MOVE WA-SU(W-NHC) TO P-SU2(W-LD)
               MOVE WA-KIN(W-NHC) TO P-KIN2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF
           GO TO M-75.
       M-80.
           IF  CNT < 2
               GO TO M-95
           END-IF
           PERFORM LIN-RTN THRU LIN-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
               MOVE "［　合計　］" TO P-FRN1(W-LD)
               MOVE WT-SU TO P-SU1(W-LD)
               MOVE WT-KIN TO P-KIN1(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE "［　合計　］" TO P-FRN2(W-LD)
               MOVE WT-SU TO P-SU2(W-LD)
               MOVE WT-KIN TO P-KIN2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KNHR-F_IDLST KNHR-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           
           IF  W-PC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       MID-EX.
           EXIT.
       LIN-RTN.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               GO TO LIN-EX
           END-IF
           ADD 1 TO W-CD.
           IF  W-CD = 1
               MOVE ZERO TO W-LD CHK
               GO TO LIN-RTN
           END-IF
           PERFORM PRI-RTN THRU PRI-EX.
           PERFORM SPC-RTN THRU SPC-EX.
           MOVE ZERO TO W-LD W-CD CHK.
           GO TO LIN-RTN.
       LIN-EX.
           EXIT.
       MEI-RTN.
           PERFORM LIN-RTN THRU LIN-EX.
           ADD W-SU TO WT-SU WS-SU(W-NHC) WA-SU(W-NHC).
           ADD W-KIN TO WT-KIN WS-KIN(W-NHC) WA-KIN(W-NHC).
           ADD 1 TO CNT.
           IF  W-CD = 1
               GO TO MEI-010
           END-IF
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-HCD TO P-HCD1(W-LD)
               MOVE KH-NAME TO P-NAME1(W-LD)
           END-IF
           MOVE W-NHC TO P-NHC1(W-LD).
           MOVE KKB-FRN TO P-FRN1(W-LD).
           MOVE W-SU TO P-SU1(W-LD).
           MOVE W-KIN TO P-KIN1(W-LD).
           MOVE ":" TO P-X(W-LD).
           GO TO MEI-EX.
       MEI-010.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-HCD TO P-HCD2(W-LD)
               MOVE KH-NAME TO P-NAME2(W-LD)
           END-IF
           MOVE W-NHC TO P-NHC2(W-LD).
           MOVE KKB-FRN TO P-FRN2(W-LD).
           MOVE W-SU TO P-SU2(W-LD).
           MOVE W-KIN TO P-KIN2(W-LD).
       MEI-EX.
           EXIT.
       PRI-RTN.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-010 THRU MID-EX
               ELSE PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE ZERO TO W-LD.
       PRI-010.
           ADD 1 TO W-LD.
           IF  W-LD = 59
               GO TO PRI-EX
           END-IF
           IF  P-X(W-LD) = SPACE
               GO TO PRI-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1(W-LD) TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PRI-010.
       PRI-EX.
           EXIT.
       KEI-RTN.
           PERFORM LIN-RTN THRU LIN-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF
           IF  CNT < 2
               GO TO KEI-EX
           END-IF
           IF  W-CD = 0
               MOVE "［　合計　］" TO P-FRN1(W-LD)
               MOVE WT-SU TO P-SU1(W-LD)
               MOVE WT-KIN TO P-KIN1(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-HCD TO P-HCD1(W-LD)
                   MOVE KH-NAME TO P-NAME1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE "［　合計　］" TO P-FRN2(W-LD)
               MOVE WT-SU TO P-SU2(W-LD)
               MOVE WT-KIN TO P-KIN2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-HCD TO P-HCD2(W-LD)
                   MOVE KH-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF
           PERFORM LIN-RTN THRU LIN-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       KEI-EX.
           EXIT.
       TOT-RTN.
           IF (W-SU NOT = ZERO) OR (W-KIN NOT = ZERO)
               PERFORM MEI-RTN THRU MEI-EX
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           MOVE "　　（　小　計　）  " TO W-NAME.
           MOVE ZERO TO W-NHC WT-D CHK CNT.
       TOT-010.
           ADD 1 TO W-NHC.
           IF  W-NHC = 31
               GO TO TOT-020
           END-IF
           IF  ZERO = WS-SU(W-NHC) AND WS-KIN(W-NHC)
               GO TO TOT-010
           END-IF
           PERFORM KKB-RTN THRU KKB-EX.
           PERFORM LIN-RTN THRU LIN-EX.
           ADD WS-SU(W-NHC) TO WT-SU.
           ADD WS-KIN(W-NHC) TO WT-KIN.
           ADD 1 TO CNT.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
               MOVE W-NHC TO P-NHC1(W-LD)
               MOVE KKB-FRN TO P-FRN1(W-LD)
               MOVE WS-SU(W-NHC) TO P-SU1(W-LD)
               MOVE WS-KIN(W-NHC) TO P-KIN1(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE W-NHC TO P-NHC2(W-LD)
               MOVE KKB-FRN TO P-FRN2(W-LD)
               MOVE WS-SU(W-NHC) TO P-SU2(W-LD)
               MOVE WS-KIN(W-NHC) TO P-KIN2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF
           GO TO TOT-010.
       TOT-020.
           IF  CNT < 2
               GO TO TOT-EX
           END-IF
           PERFORM LIN-RTN THRU LIN-EX.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
               MOVE "［　合計　］" TO P-FRN1(W-LD)
               MOVE WT-SU TO P-SU1(W-LD)
               MOVE WT-KIN TO P-KIN1(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME1(W-LD)
               END-IF
           END-IF
           IF  W-CD = 1
               MOVE "［　合計　］" TO P-FRN2(W-LD)
               MOVE WT-SU TO P-SU2(W-LD)
               MOVE WT-KIN TO P-KIN2(W-LD)
               IF  CHK = 0
                   MOVE 5 TO CHK
                   MOVE W-NAME TO P-NAME2(W-LD)
               END-IF
           END-IF.
       TOT-EX.
           EXIT.
       SPC-RTN.
           MOVE SPACE TO W-P.
           MOVE ZERO TO W-LD.
       SPC-010.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 59
               MOVE W-15K TO P-15K1(W-LD) P-15K2(W-LD)
               MOVE W-20K TO P-20K1(W-LD) P-20K2(W-LD)
               MOVE SPACE TO P-FRN1(W-LD) P-FRN2(W-LD)
               GO TO SPC-010
           END-IF.
       SPC-EX.
           EXIT.
       KKB-RTN.
           MOVE SPACE TO KKB-KEY.
           MOVE 05 TO KKB-NO.
           MOVE W-NHC TO KKB-FRC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KKB-FRN
           END-IF.
       KKB-EX.
           EXIT.
