       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBP050.
      ********************************************
      *****    中銀・商中　総合振込　明細表  *****
      ********************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(002).
       01  HEAD1.
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(029) VALUE SPACE.
           02  F            PIC  N(005) VALUE   "＊＊＊　　".
           02  H-GET        PIC  Z(002).
           02  F            PIC  N(001) VALUE   "月".
           02  H-PEY        PIC  Z(002).
           02  F            PIC  N(002) VALUE   "日　".
           02  H-BKN        PIC  N(004).
           02  F            PIC  N(014) VALUE
                  "　総合振込　明細表　　＊＊＊".
           02  F            PIC  X(021) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(010) VALUE   "振　　込　　先　　名".
           02  F            PIC  X(027) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "振込金額".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "　手数料".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "支払金額".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "銀行".
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(006) VALUE   "銀　行　名　".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(008) VALUE   "本　支　店　名　".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "種別".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "口座番号".
           02  F            PIC  X(001) VALUE SPACE.
       01  W-P.
           02  P-KEY        PIC  9(004).
           02  F            PIC  X(001).
           02  P-NAME       PIC  N(024).
           02  P-FKIN       PIC ----,---,--9.
           02  F            PIC  X(001).
           02  P-X1         PIC  X(001).
           02  P-TKIN       PIC ZZZ,ZZ9.
           02  P-X2         PIC  X(001).
           02  P-SKIN       PIC ----,---,--9.
           02  F            PIC  X(003).
           02  P-BKC        PIC  9(007).
           02  F            PIC  X(001).
           02  P-BKN        PIC  X(015).
           02  F            PIC  X(001).
           02  P-HSN        PIC  X(015).
           02  F            PIC  X(001).
           02  P-YKS        PIC  9(001).
           02  F            PIC  X(002).
           02  P-KNO        PIC  9(007).
       01  W-TP.
           02  F            PIC  X(005).
           02  P-TM         PIC  N(018).
           02  P-KSU        PIC  Z(005).
           02  P-KM         PIC  N(002).
           02  F            PIC  X(001).
           02  P-TFKIN      PIC ----,---,--9.
           02  F            PIC  X(001).
           02  P-TX1        PIC  X(001).
           02  P-TTKIN      PIC ZZZ,ZZ9.
           02  P-TX2        PIC  X(001).
           02  P-TSKIN      PIC ----,---,--9.
           02  F            PIC  X(053).
       01  W-DATA.
           02  W-GP.
             03  W-GET      PIC  9(002).
             03  W-PEY      PIC  9(002).
           02  W-DMM        PIC  9(001).
           02  W-PAGE       PIC  9(002).
           02  W-D.
             03  W-FKN      PIC  X(030).
             03  W-BKC.
               04  W-BK     PIC  9(004).
               04  W-HS     PIC  9(003).
             03  W-YKS      PIC  9(001).
             03  W-KNO      PIC  9(007).
             03  W-TRC      PIC  9(001).
             03  W-KIN      PIC  9(009).
           02  WN-D.
             03  W-FKIN     PIC  9(009).
             03  W-TKIN     PIC  9(006).
             03  W-SKIN     PIC  9(009).
           02  WS-D.
             03  WS-KSU     PIC  9(004).
             03  WS-FKIN    PIC  9(009).
             03  WS-TKIN1   PIC  9(006).
             03  WS-TKIN2   PIC  9(006).
             03  WS-SKIN    PIC  9(009).
           02  WA-D.
             03  WA-KSU     PIC  9(004).
             03  WA-FKIN    PIC  9(009).
             03  WA-TKIN1   PIC  9(006).
             03  WA-TKIN2   PIC  9(006).
             03  WA-SKIN    PIC  9(009).
           02  W-C          PIC  9(001).
           02  CHK          PIC  9(001).
           02  CNT          PIC  9(001).
           02  W-DC         PIC  9(001).
           02  W-BKCD       PIC  9(001).
           02  W-BKN        PIC  N(004).
           COPY LSTAT.
      *
           COPY LISM.
           COPY LIFBKM.
           COPY LSPF.
      *FD  FKSM
       01  FKSM_FBP050.
           02  FKSM_PNAME1    PIC  X(004) VALUE "FKSM".
           02  F              PIC  X(001).
           02  FKSM_LNAME     PIC  X(011) VALUE "FKSM_FBP050".
           02  F              PIC  X(001).
           02  FKSM_KEY1      PIC  X(100) VALUE SPACE.
           02  FKSM_SORT      PIC  X(100) VALUE SPACE.
           02  FKSM_IDLST     PIC  X(100) VALUE SPACE.
           02  FKSM_RES       USAGE  POINTER.
       01  FKS-R.
           02  FS-KEY       PIC  X(004).
           02  FS-FKC       PIC  9(001).
           02  FS-D1.
             03  FS-FKN1    PIC  X(030).
             03  FS-BKC1.
               04  FS-BK1   PIC  9(004).
               04  FS-HS1   PIC  9(003).
             03  FS-YKS1    PIC  9(001).
             03  FS-KNO1    PIC  9(007).
             03  FS-TRC1    PIC  9(001).
             03  FS-KIN1    PIC  9(009).
           02  FS-D2.
             03  FS-FKN2    PIC  X(030).
             03  FS-BKC2.
               04  FS-BK2   PIC  9(004).
               04  FS-HS2   PIC  9(003).
             03  FS-YKS2    PIC  9(001).
             03  FS-KNO2    PIC  9(007).
             03  FS-TRC2    PIC  9(001).
             03  FS-KIN2    PIC  9(009).
           02  FS-BKC       PIC  9(001).
           02  FS-FGP       PIC  9(004).
           02  F            PIC  X(008).
       77  F                PIC  X(001).
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
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊　　　総合振込　明細表　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "【            】       振込日    月   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-DT.
             03  FILLER  PIC  N(004).
             03  FILLER  PIC  Z(002).
             03  FILLER  PIC  Z(002).
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗ-  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "342" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "10" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "19" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "36" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DT" " " "15" "0" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DT" "N" "15" "14" "8" " " "D-DT" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DT" BY REFERENCE W-BKN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DT" "Z" "15" "41" "2" "01D-DT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DT" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DT" "Z" "15" "46" "2" "02D-DT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DT" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "97" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "97" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "INPUT" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
       M-10.
      *           READ FKSM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  FS-BKC = 0
               GO TO M-10
           END-IF
           MOVE FS-BKC TO W-BKCD.
           MOVE FS-FGP TO W-GP.
           MOVE SPACE TO W-BKN.
           IF  W-BKCD = 1
               MOVE   "中国銀行" TO W-BKN
           END-IF
           IF  W-BKCD = 2
               MOVE   "商工中金" TO W-BKN
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "SD_Output" USING "D-DT" D-DT "p" RETURNING RESU.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
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
           MOVE ZERO TO W-PAGE WA-D CNT W-DC.
       M-22.
           ADD 1 TO CNT.
           IF  CNT = 4
               GO TO M-90
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
       M-25.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               GO TO M-22
           END-IF
           IF  FS-FKC NOT = CNT
               GO TO M-25
           END-IF
           IF  ZERO = FS-KIN1 AND FS-KIN2
               GO TO M-25
           END-IF
           IF (FS-BKC NOT = W-BKCD) OR (FS-FGP NOT = W-GP)
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               GO TO M-95
           END-IF
           IF  W-DC NOT = ZERO
               GO TO M-30
           END-IF
           MOVE 5 TO W-DC.
           ACCEPT H-DATE FROM DATE.
           MOVE W-BKN TO H-BKN.
           MOVE W-GET TO H-GET.
           MOVE W-PEY TO H-PEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
       M-30.
           MOVE FS-KEY TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE   "　　＊＊　マスター　なし　＊＊" TO S-NAME
           END-IF
           MOVE ZERO TO CHK.
           MOVE ZERO TO W-D.
           MOVE SPACE TO W-FKN.
           IF  FS-KIN1 NOT = ZERO
               MOVE FS-D1 TO W-D
               PERFORM S-20 THRU S-35
           END-IF
           IF  FS-KIN2 NOT = ZERO
               MOVE FS-D2 TO W-D
               PERFORM S-20 THRU S-35
           END-IF.
       M-35.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-40
           END-IF
           IF  FS-FKC NOT = CNT
               GO TO M-35
           END-IF
           IF  ZERO = FS-KIN1 AND FS-KIN2
               GO TO M-35
           END-IF
           IF (FS-BKC NOT = W-BKCD) OR (FS-FGP NOT = W-GP)
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE FBKM_IDLST FBKM_PNAME1
               CALL "PR_Close" RETURNING RESP
               GO TO M-95
           END-IF
           GO TO M-30.
       M-40.
           MOVE SPACE TO W-TP.
           MOVE   "　　　［　　ＳＵＢ　ＴＯＴＡＬ　　］" TO P-TM.
           MOVE WS-KSU TO P-KSU.
           MOVE   "件　" TO P-KM.
           MOVE WS-FKIN TO P-TFKIN.
           MOVE WS-TKIN1 TO P-TTKIN.
           MOVE WS-SKIN TO P-TSKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-TP TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-TP.
           MOVE SPACE TO P-TM P-KM.
           MOVE "(" TO P-TX1.
           MOVE ")" TO P-TX2.
           MOVE WS-TKIN2 TO P-TTKIN.
           MOVE SPACE TO SP-R.
           MOVE W-TP TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD WS-KSU TO WA-KSU.
           ADD WS-FKIN TO WA-FKIN.
           ADD WS-TKIN1 TO WA-TKIN1.
           ADD WS-TKIN2 TO WA-TKIN2.
           ADD WS-SKIN TO WA-SKIN.
           MOVE ZERO TO WS-D.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           GO TO M-22.
       M-90.
           MOVE SPACE TO W-TP.
           MOVE   "【　　ＡＬＬ　ＴＯＴＡＬ　　】　　　" TO P-TM.
           MOVE WA-KSU TO P-KSU.
           MOVE   "件　" TO P-KM.
           MOVE WA-FKIN TO P-TFKIN.
           MOVE WA-TKIN1 TO P-TTKIN.
           MOVE WA-SKIN TO P-TSKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-TP TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-TP.
           MOVE SPACE TO P-TM P-KM.
           MOVE "(" TO P-TX1.
           MOVE ")" TO P-TX2.
           MOVE WA-TKIN2 TO P-TTKIN.
           MOVE SPACE TO SP-R.
           MOVE W-TP TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-10.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
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
           MOVE ZERO TO WN-D W-C.
           IF  W-BKCD = 1
               IF  W-BKC = "0168101"
                   MOVE W-KIN TO W-FKIN W-SKIN
                   GO TO S-30
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BKC = "2004331"
                   MOVE W-KIN TO W-FKIN W-SKIN
                   GO TO S-30
               END-IF
           END-IF
           IF  W-TRC = 0
               GO TO S-25
           END-IF
           IF  W-BKCD = 1
               IF  W-BK = 0168
                   IF  W-KIN < 30108
                       SUBTRACT 108 FROM W-TKIN
                   ELSE
                       SUBTRACT 324 FROM W-TKIN
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 1
               IF  W-BK NOT = 0168
                   IF  W-KIN < 30432
                       SUBTRACT 432 FROM W-TKIN
                   ELSE
                       SUBTRACT 648 FROM W-TKIN
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BK = 2004
                   IF  W-KIN < 30108
                       SUBTRACT 108 FROM W-TKIN
                   ELSE
                       SUBTRACT 324 FROM W-TKIN
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BK NOT = 2004
                   IF  W-KIN < 30432
                       SUBTRACT 432 FROM W-TKIN
                   ELSE
                       SUBTRACT 648 FROM W-TKIN
                   END-IF
               END-IF
           END-IF
           MOVE W-KIN TO W-SKIN.
           COMPUTE W-FKIN = W-SKIN - W-TKIN.
           GO TO S-30.
       S-25.
           IF  W-BKCD = 1
               IF  W-BK = 0168
                   IF  W-KIN < 30000
                       MOVE 108 TO W-TKIN
                   ELSE
                       MOVE 324 TO W-TKIN
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 1
               IF  W-BK NOT = 0168
                   IF  W-KIN < 30000
                       MOVE 432 TO W-TKIN
                   ELSE
                       MOVE 648 TO W-TKIN
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BK = 2004
                   IF  W-KIN < 30000
                       MOVE 108 TO W-TKIN
                   ELSE
                       MOVE 324 TO W-TKIN
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BK NOT = 2004
                   IF  W-KIN < 30000
                       MOVE 432 TO W-TKIN
                   ELSE
                       MOVE 648 TO W-TKIN
                   END-IF
               END-IF
           END-IF
           MOVE W-KIN TO W-FKIN W-SKIN.
           MOVE 5 TO W-C.
       S-30.
           MOVE W-BKC TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " ** ﾏｽﾀｰ ﾅｼ  **" TO FBK-BKN
               MOVE SPACE TO FBK-HSN
           END-IF
           MOVE SPACE TO W-P.
           IF  CHK = ZERO
               MOVE FS-KEY TO P-KEY
               MOVE S-NAME TO P-NAME
               MOVE 9 TO CHK
           ELSE
               MOVE SPACE TO P-NAME
           END-IF
           IF  W-C NOT = ZERO
               MOVE "(" TO P-X1
               MOVE ")" TO P-X2
           END-IF
           MOVE W-FKIN TO P-FKIN.
           MOVE W-TKIN TO P-TKIN.
           MOVE W-SKIN TO P-SKIN.
           MOVE W-BKC TO P-BKC.
           MOVE FBK-BKN TO P-BKN.
           MOVE FBK-HSN TO P-HSN.
           MOVE W-YKS TO P-YKS.
           MOVE W-KNO TO P-KNO.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE FS-KEY TO P-KEY
               MOVE S-NAME TO P-NAME
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 1 TO WS-KSU.
           ADD W-FKIN TO WS-FKIN.
           IF  W-C = ZERO
               ADD W-TKIN TO WS-TKIN1
           ELSE
               ADD W-TKIN TO WS-TKIN2
           END-IF
           ADD W-SKIN TO WS-SKIN.
       S-35.
           EXIT.
