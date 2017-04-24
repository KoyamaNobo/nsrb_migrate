       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG720.
      *******************************************
      *****    振替伝票（支払手形決済）     *****
      *****        ( FDL = FTG710 )         *****
      *******************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(008) VALUE  "　振　替　伝　票".
           02  F              PIC  X(033) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
       01  HEAD2.
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(002) VALUE  "平成".
           02  F              PIC  X(002) VALUE SPACE.
           02  H-NEN          PIC  N(002).
           02  F              PIC  N(001) VALUE  "年".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-GET          PIC  N(002).
           02  F              PIC  N(001) VALUE  "月".
           02  F              PIC  X(001) VALUE SPACE.
           02  H-PEY          PIC  N(002).
           02  F              PIC  N(001) VALUE  "日".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "№".
           02  F              PIC  X(007) VALUE SPACE.
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(005) VALUE  "科　目　名".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "金".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "額".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "摘".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "要".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(005) VALUE  "科　目　名".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "金".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(001) VALUE  "額".
           02  F              PIC  X(007) VALUE SPACE.
       01  HEAD9.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(043) VALUE SPACE.
           02  F              PIC  N(016) VALUE
                 "日　進　ゴ　ム　株　式　会　社　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-PD.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(092) VALUE SPACE.
           02  P-IM           PIC  N(004).
           02  F              PIC  X(005) VALUE X"1A24212474".
       01  W-P1.
           02  P1-15K         PIC  X(005).
           02  F              PIC  X(037).
           02  P-NA1          PIC  N(020).
           02  F              PIC  X(002).
           02  P-NA2          PIC  N(008).
           02  F              PIC  X(017).
           02  P1-20K         PIC  X(005).
       01  W-P2.
           02  P2-15K         PIC  X(005).
           02  F              PIC  X(013).
           02  P-KMK1         PIC  N(008).
           02  F              PIC  X(001).
           02  P-KIN1         PIC ZZZZZZZZZ9.
           02  F              PIC  X(001).
           02  P-GET          PIC Z9.
           02  P-VER          PIC  X(001).
           02  P-PEY          PIC Z9.
           02  F              PIC  X(001).
           02  P-D1           PIC  N(004).
           02  F              PIC  X(005).
           02  P-D2           PIC  N(008).
           02  F              PIC  X(003).
           02  P-KMK2         PIC  N(008).
           02  F              PIC  X(001).
           02  P-KIN2         PIC ZZZZZZZZZ9.
           02  F              PIC  X(006).
           02  P2-20K         PIC  X(005).
       01  W-DATA.
           02  W-SNG          PIC  9(006).
           02  W-SNGD  REDEFINES W-SNG.
             03  W-SNEN       PIC  9(004).
             03  W-SNENL REDEFINES W-SNEN.
               04  W-SNEN1    PIC  9(002).
               04  W-SNEN2    PIC  9(002).
             03  W-SGET       PIC  9(002).
           02  W-SNGL  REDEFINES W-SNG.
             03  F            PIC  9(002).
             03  W-SNGS       PIC  9(004).
           02  W-WNG          PIC  9(004).
           02  W-WNGD  REDEFINES W-WNG.
             03  W-WNEN       PIC  9(002).
             03  W-WGET       PIC  9(002).
           02  W-PEY          PIC  9(002).
           02  W-HNGP.
             03  W-HNEN       PIC Z9.
             03  W-HGET       PIC Z9.
             03  W-HPEY       PIC Z9.
           02  W-BKC          PIC  9(004).
           02  W-KIN          PIC  9(010).
           02  W-TCD          PIC  9(004).
           02  W-PAGE         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-TC           PIC  9(001).
           02  CHK            PIC  9(001).
           02  CNT            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIBANK.
           COPY LSSHIT.
           COPY LISM.
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　振替伝票　（支手決済）　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "【  H.  年   月 分  】".
           02  FILLER  PIC  X(036) VALUE
                "[   ﾃｽﾄ ﾌﾟﾘﾝﾄ  ｽﾙ=9 ｼﾅｲ=1   ﾘﾀｰﾝ   ]".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TC    PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  FILLER  PIC Z9 .
             03  FILLER  PIC Z9 .
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  BANKM ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-BKC   PIC  9(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FTG710" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "402" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "46" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "46" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "46" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "46" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "46" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "46" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "46" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "12" "22" "22" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "14" "15" "36" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "20" "22" "22" "09C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TC" "9" "14" "42" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TC" BY REFERENCE W-TC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "39" "1" "A-TC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "4" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "12" "0" "4" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "Z9" "12" "28" "2" " " "D-NG" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-WNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "12" "33" "2" "01D-NG" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-WGET "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "47" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "47" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-BKC" "9" "24" "40" "4" "E-ME2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-BKC" BY REFERENCE ST-BCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-BKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO TO W-SNG W-WNG.
           MOVE D-NTNG TO W-SNGS.
           IF  W-SNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-SNEN
           END-IF
           IF  W-SNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-SNEN
           END-IF
           COMPUTE W-WNEN = W-SNEN - DATE-YC1.
           MOVE W-SGET TO W-WGET.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P2-15K.
           MOVE W-20K TO P2-20K.
           MOVE ALL  "Ｎ" TO P-KMK1 P-KMK2 P-D1 P-D2.
           MOVE 999999999 TO P-KIN1 P-KIN2.
           MOVE 99 TO H-NEN H-GET H-PEY H-PAGE P-GET P-PEY.
           MOVE "/" TO P-VER.
           MOVE ZERO TO CNT W-PAGE.
           CALL "PR_Open" RETURNING RESP.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TC "A-TC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TC = 1
               GO TO M-15
           END-IF
           IF  W-TC NOT = 9
               GO TO M-10
           END-IF
           IF  CNT = ZERO
               PERFORM S-10 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO CNT.
           IF  CNT = 6
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE ZERO TO CNT
           END-IF
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-DMM = 9
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
      *
           IF  CNT NOT = ZERO
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE ZERO TO CNT
           END-IF
           MOVE ZERO TO CHK.
           CALL "CBLSTNNO" USING STN-NO USER_ID.
           MOVE STN-NO2 TO W-FID2.
           MOVE W-FID TO WK0128ID.
           MOVE WK0128ID TO SHIT-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SHIT-F_PNAME1 " " BY REFERENCE SHIT-F_IDLST "0".
       M-20.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF (ST-SNM NOT = W-SNEN) OR (ST-MKG NOT = W-SGET)
               GO TO M-20
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-20
           END-IF.
       M-25.
           MOVE ZERO TO W-PAGE.
           MOVE ST-MKP TO W-PEY.
           MOVE W-WNEN TO W-HNEN.
           MOVE W-WGET TO W-HGET.
           MOVE W-PEY TO W-HPEY.
           MOVE ST-BCD TO W-BKC.
           MOVE W-BKC TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-BKC" E-BKC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF  CHK = ZERO
               MOVE 5 TO CHK
               PERFORM S-10 THRU S-15
           ELSE
               PERFORM S-05 THRU S-15
           END-IF
           MOVE ZERO TO W-KIN.
       M-30.
           MOVE ST-TCD TO W-TCD.
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE W-TCD TO S-NAME
           END-IF
           PERFORM S-20 THRU S-30.
       M-45.
           MOVE SPACE TO W-P2.
           MOVE W-15K TO P2-15K.
           MOVE W-20K TO P2-20K.
           MOVE SPACE TO P-KMK1 P-KMK2 P-D1 P-D2.
           MOVE  "支　払　手　形　" TO P-KMK1.
           MOVE  "当　座　預　金　" TO P-KMK2.
           MOVE ST-KIN TO P-KIN1 P-KIN2.
           MOVE W-WGET TO P-GET.
           MOVE "/" TO P-VER.
           MOVE W-PEY TO P-PEY.
           MOVE  "為替手形" TO P-D1.
           MOVE  "支　手　決　済　" TO P-D2.
           IF  CNT NOT = 6
               GO TO M-50
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_LineFeed" USING "4" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           PERFORM S-05 THRU S-15.
           PERFORM S-20 THRU S-30.
       M-50.
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD ST-KIN TO W-KIN.
           ADD 1 TO CNT.
       M-55.
      *           READ SHIT-F AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" SHIT-F_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF (ST-SNM NOT = W-SNEN) OR (ST-MKG NOT = W-SGET)
               GO TO M-55
           END-IF
           IF  ST-SKC NOT = ZERO
               GO TO M-55
           END-IF
           IF  ST-MKP NOT = W-PEY
               GO TO M-60
           END-IF
           IF  ST-BCD NOT = W-BKC
               GO TO M-60
           END-IF
           IF  ST-TCD = W-TCD
               GO TO M-45
           END-IF
           GO TO M-30.
       M-60.
           PERFORM S-35 THRU S-40.
           GO TO M-25.
       M-90.
           PERFORM S-35 THRU S-40.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-F_IDLST SHIT-F_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           ADD 1 TO W-PAGE.
           MOVE W-HNEN TO H-NEN.
           MOVE W-HGET TO H-GET.
           MOVE W-HPEY TO H-PEY.
           MOVE W-PAGE TO H-PAGE.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE  "承認印　" TO P-IM.
           MOVE W-PD TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE ZERO TO CNT.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO W-P1.
           MOVE W-15K TO P1-15K.
           MOVE W-20K TO P1-20K.
           MOVE S-NAME TO P-NA1.
           MOVE B-BNA TO P-NA2.
           IF  CNT < 5
               GO TO S-25
           END-IF
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           IF  CNT = 6
               CALL "PR_LineFeed" USING "3" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           ELSE
               CALL "PR_LineFeed" USING "5" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           PERFORM S-05 THRU S-15.
       S-25.
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO CNT.
       S-30.
           EXIT.
       S-35.
           IF  CNT = 6
               MOVE SPACE TO W-P2
               MOVE W-15K TO P2-15K
               MOVE W-20K TO P2-20K
               MOVE SPACE TO P-KMK1 P-KMK2 P-D1 P-D2
               MOVE W-KIN TO P-KIN1 P-KIN2
               MOVE SPACE TO SP-R
               MOVE W-P2 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD9 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               GO TO S-40
           END-IF
           PERFORM S-45 THRU S-50.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 1 TO CNT.
           GO TO S-35.
       S-40.
           EXIT.
       S-45.
           MOVE SPACE TO P-IM.
           IF  CNT = 1
               MOVE  "検　印　" TO P-IM
           END-IF
           IF  CNT = 3
               MOVE  "記帳印　" TO P-IM
           END-IF
           IF  CNT = 5
               MOVE  "入力印　" TO P-IM
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PD TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-50.
           EXIT.
