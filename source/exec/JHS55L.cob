       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JHS55L.
      *********************************************************
      *    PROGRAM         :  赤ちゃん本舗納品明細表          *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ******                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0:明細含む , 1=見出しのみ       *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  JS-SIGN            PIC  9(001).
       77  W-END              PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001).
       77  W-20K              PIC  X(005) VALUE X"1A24212474".
       01  W-P00.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(079) VALUE SPACE.
           02  P-NHSN         PIC  N(016) VALUE SPACE.
       01  W-P01.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  9(006) VALUE 915190.
           02  F              PIC  X(064) VALUE SPACE.
       01  W-P02.
           02  F              PIC  X(063) VALUE SPACE.
           02  P-M1           PIC  N(002) VALUE "○　".
           02  F              PIC  X(017) VALUE SPACE.
           02  P-PD           PIC  Z(002).
           02  F              PIC  X(008) VALUE SPACE.
           02  P-HP           PIC  X(001) VALUE "X".
       01  W-P03.
           02  F              PIC  X(027) VALUE SPACE.
           02  P-NRN          PIC  N(008) VALUE "日進ゴム株式会社".
           02  F              PIC  X(006) VALUE SPACE.
           02  P-SKN          PIC  N(002) VALUE "小野".
           02  F              PIC  X(049) VALUE SPACE.
       01  W-P04.
           02  F              PIC  X(004) VALUE SPACE.
           02  P-NEN          PIC  9(002) VALUE 99.
           02  P-NEND  REDEFINES P-NEN  PIC  X(002).
           02  F              PIC  X(002) VALUE SPACE.
           02  P-GET          PIC  Z(002).
           02  F              PIC  X(002) VALUE SPACE.
           02  P-PEY          PIC  Z(002).
           02  F              PIC  X(043) VALUE SPACE.
           02  P-M2           PIC  N(002) VALUE "○　".
           02  F              PIC  X(037) VALUE SPACE.
       01  W-P05.
           02  F              PIC  X(003).
           02  P-STC1         PIC  9(002).
           02  P-V            PIC  X(001).
           02  P-STC2         PIC  9(003).
           02  F              PIC  X(022).
           02  P-ADNO.
             03  P-DNOD  OCCURS   7.
               04  F          PIC  X(003).
               04  P-DNO      PIC  9(006).
           02  F              PIC  X(003).
       01  W-P06.
           02  F              PIC  X(088).
           02  P-MSU          PIC  Z(003).
           02  P-MM           PIC  N(002).
           02  F              PIC  X(003).
       01  W-DATA.
           02  W-CCD          PIC  9(003).
           02  W-STC12        PIC  9(005).
           02  CNT            PIC  9(001).
           02  W-TPC          PIC  9(001).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN1       PIC  9(002).
             03  W-NGPS.
               04  W-NEN2     PIC  9(002).
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-MSU          PIC  9(003).
           02  W-MSUC         PIC  9(003).
           02  W-DATE         PIC  9(006).
           02  W-POC          PIC  9(001).
           COPY LSTAT.
      *
           COPY LITDNA.
           COPY LIAHNH.
      *FD  SP-F
       77  SP-R               PIC  X(170).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(012) VALUE
                "赤ちゃん本舗　納品明細表".
           02  FILLER  PIC  X(035) VALUE
                "テストプリント印字 (YES=1,NO=2) [ ]".
           02  FILLER  PIC  X(028) VALUE
                "確認 (OK=1,NO=9) --->   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  A-MSU   PIC  9(003).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-JS    PIC  X(019) VALUE
                "見出しのみ       枚".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME5   PIC  X(019) VALUE
                  "***  ﾉｳﾋﾝｻｷ ﾅｼ  ***".
             03  E-STC   PIC  X(007).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "87" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "20" "24" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "X" "7" "22" "35" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "X" "23" "40" "28" "02C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "5" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "7" "55" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MSU" "9" "12" "33" "3" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MSU" BY REFERENCE W-MSU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "62" "1" "A-MSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "19" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JS" "X" "12" "20" "19" " " "C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "43" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "43" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "19" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STC" "X" "24" "45" "7" "E-ME5" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STC" BY REFERENCE AHNH-KEY "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "D-JS" D-JS "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-DATA.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-END = 1
               GO TO M-95
           END-IF
           IF  JS-SIGN = 1
               GO TO M-70
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TDNAF_PNAME1 "SHARED" BY REFERENCE TDNAF_IDLST "1"
            "TDNA-KEY" BY REFERENCE TDNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AHNHF_PNAME1 "SHARED" BY REFERENCE AHNHF_IDLST "1"
            "AHNH-KEY" BY REFERENCE AHNH-KEY.
       M-10.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-60
           END-IF
           IF  TDNA-DGN NOT = 1
               GO TO M-10
           END-IF
           ACCEPT W-DATE FROM DATE.
           MOVE W-DATE TO W-NGPS.
       M-15.
           MOVE TDNA-CCD TO W-CCD.
           MOVE ZERO TO W-MSU.
           MOVE TDNA-STC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO AHNH-NHSN AHNH-HP
               GO TO M-20
           END-IF
           IF  AHNH-CTC = ZERO
               GO TO M-20
           END-IF
           MOVE AHNH-CTC TO AHNH-KEY.
      *           READ AHNHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AHNHF_PNAME1 BY REFERENCE AHNH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO AHNH-NHSN AHNH-HP
           END-IF.
       M-20.
           PERFORM HEAD-RTN THRU HEAD-EX.
       M-25.
           MOVE TDNA-STC12 TO W-STC12.
           MOVE SPACE TO W-P05.
           MOVE TDNA-STC1 TO P-STC1.
           MOVE "-" TO P-V.
           MOVE TDNA-STC2 TO P-STC2.
           MOVE 0 TO CNT.
       M-30.
           ADD 1 TO CNT.
           IF  CNT = 8
               GO TO M-40
           END-IF
           MOVE TDNA-DNO TO P-DNO(CNT).
           ADD 1 TO W-MSU.
       M-35.
      *           READ TDNAF NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDNAF_PNAME1 BY REFERENCE TDNA-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           IF  TDNA-DGN NOT = 1
               GO TO M-35
           END-IF
           IF  TDNA-CCD NOT = W-CCD
               GO TO M-45
           END-IF
           IF  TDNA-STC12 = W-STC12
               GO TO M-30
           END-IF.
       M-40.
           PERFORM MEIS-RTN THRU MEIS-EX.
           GO TO M-25.
       M-45.
           PERFORM MEIS-RTN THRU MEIS-EX.
           PERFORM KEI-RTN THRU KEI-EX.
           GO TO M-15.
       M-55.
           PERFORM MEIS-RTN THRU MEIS-EX.
           PERFORM KEI-RTN THRU KEI-EX.
       M-60.
           CALL "DB_F_Close" USING
            BY REFERENCE AHNHF_IDLST AHNHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDNAF_IDLST TDNAF_PNAME1.
           GO TO M-95.
       M-70.
           MOVE SPACE TO P-NHSN P-HP P-NEND.
           MOVE 0 TO P-PD P-GET P-PEY.
       M-75.
           PERFORM HEAD-RTN THRU HEAD-EX.
           ADD 1 TO W-MSUC.
           IF  W-MSU NOT = W-MSUC
               GO TO M-75
           END-IF.
       M-95.
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 1 TO W-END
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-TPC = 2
               IF  JS-SIGN = 1
                   GO TO ACP-20
               ELSE
                   GO TO ACP-50
               END-IF
           END-IF
           IF  W-TPC NOT = 1
               GO TO ACP-RTN
           END-IF
           PERFORM TST-RTN THRU TST-EX.
           GO TO ACP-RTN.
       ACP-20.
           CALL "SD_Accept" USING BY REFERENCE A-MSU "A-MSU" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-20
           END-IF
           IF  W-MSU = 0
               GO TO ACP-20
           END-IF.
       ACP-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 1
                   GO TO ACP-20
               ELSE
                   GO TO ACP-RTN
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-50
           END-IF
           IF  W-DMM = 9
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-50
           END-IF.
       ACP-EX.
           EXIT.
       HEAD-RTN.
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
           ELSE
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
           END-IF
           IF  JS-SIGN = 0
               MOVE AHNH-NHSN TO P-NHSN
               MOVE "○　" TO P-M1 P-M2
               MOVE 0 TO P-PD
               MOVE AHNH-HP TO P-HP
               MOVE W-NEN2 TO P-NEN
               MOVE W-GET TO P-GET
               MOVE W-PEY TO P-PEY
           END-IF
      *
           MOVE SPACE TO SP-R.
           MOVE W-P00 TO SP-R.
           CALL "PR_LineFeed" USING "1" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P01 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       HEAD-EX.
           EXIT.
       MEIS-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 80
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               PERFORM HEAD-RTN THRU HEAD-EX
           END-IF
           MOVE W-P05 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEIS-EX.
           EXIT.
       KEI-RTN.
           MOVE SPACE TO W-P06.
           MOVE W-MSU TO P-MSU.
           MOVE "枚　" TO P-MM.
           MOVE SPACE TO SP-R.
           MOVE W-P06 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-EX.
           EXIT.
       TST-RTN.
           IF  W-POC = 0
               CALL "PR_Open" RETURNING RESP
               MOVE 1 TO W-POC
               MOVE SPACE TO W-P05
               MOVE 99 TO P-STC1 P-PD P-GET P-PEY
               MOVE "-" TO P-V
               MOVE 999 TO P-STC2
               MOVE 999999 TO P-DNO(1) P-DNO(2) P-DNO(3) P-DNO(4)
                              P-DNO(5) P-DNO(6) P-DNO(7)
               GO TO TST-05
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 5 AND < 79
               GO TO TST-10
           END-IF
           MOVE SPACE TO SP-R
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       TST-05.
           MOVE SPACE TO SP-R.
           MOVE W-P01 TO SP-R.
           CALL "PR_LineFeed" USING "5" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P02 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P03 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-P04 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       TST-10.
           MOVE SPACE TO SP-R.
           MOVE W-P05 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TST-EX.
           EXIT.
