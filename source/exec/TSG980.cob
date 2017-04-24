       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSG980.
      ********************************************************
      *****     手形・領収書・買掛支払　未変換リスト     *****
      ********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　手形・領収書・買掛支払　未変換リスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "種類".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形他№".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(018) VALUE
                "取　引　先　名　（　振　出　人　）　".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　満期日".
           02  F              PIC  X(008) VALUE "   ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "銀　行　名　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(008) VALUE "本　支　店　名　".
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  N(004) VALUE "　領収書".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "領収".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "手形".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "購買".
       01  HEAD3.
           02  F              PIC  X(041) VALUE SPACE.
           02  F              PIC  N(006) VALUE "相手科目１２".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１４".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "４６".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２０".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２２".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３４".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "５２".
       01  W-P1.
           02  P-DAT          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-KBN          PIC  9(002).
           02  F              PIC  X(003).
           02  P-NO           PIC  9(004).
           02  F              PIC  X(002).
           02  P-TCD          PIC  9(004).
           02  P-K1           PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  P-K2           PIC  X(001).
           02  P-MAN          PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-BK           PIC  9(004).
           02  F              PIC  X(001).
           02  P-BNA          PIC  N(008).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(008).
           02  P-KIN          PIC ----,---,--9.
           02  F              PIC  X(005).
           02  P-RSC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCR          PIC  N(002).
           02  F              PIC  X(001).
           02  P-HCT          PIC  N(002).
           02  F              PIC  X(001).
           02  P-HCK          PIC  N(002).
       01  W-P2.
           02  F              PIC  X(039).
           02  P-ZR           PIC ---,---,--9.
           02  P-ZSHZ         PIC --,---,--9.
           02  P-SS           PIC ---,---,--9.
           02  P-SSHZ         PIC --,---,--9.
           02  P-SB           PIC ---,---,--9.
           02  P-GC           PIC ---,---,--9.
           02  P-SZ           PIC ---,---,--9.
           02  P-EG           PIC ---,---,--9.
           02  P-ST           PIC ---,---,--9.
       01  W-DATA.
           02  W-NG.
             03  W-NEN        PIC  9(004).
             03  W-GET        PIC  9(002).
           02  W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENL  REDEFINES W-NEND.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGL   REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-DAT          PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-TNA          PIC  N(026).
           02  W-BNA          PIC  N(008).
           02  W-SNA          PIC  N(008).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LITM.
           COPY LIBANK.
           COPY LSPF.
      *FD  TDT-M
       01  TDT-M_TSG980.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_TSG980".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-KBN       PIC  X(002).
             03  TD-NO        PIC  X(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DAT         PIC  9(006).
           02  TD-NGP   REDEFINES TD-DAT.
             03  TD-NEN       PIC  9(002).
             03  TD-GET       PIC  9(002).
             03  TD-PEY       PIC  9(002).
           02  TD-MAN         PIC  9(006).
           02  TD-KIN         PIC S9(010).
           02  TD-BK          PIC  9(004).
           02  TD-HAC         PIC  N(024).
           02  TD-ZR          PIC S9(008).
           02  TD-SS          PIC S9(008).
           02  TD-SB          PIC  9(008).
           02  TD-GC          PIC  9(008).
           02  TD-SZ          PIC  9(008).
           02  TD-EG          PIC  9(008).
           02  TD-ST          PIC  9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN        PIC  9(004).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PCHK        PIC  9(001).
           02  TD-RSC         PIC  9(001).
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
           02  FILLER  PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　手形・領収書・買掛支払　未変換リスト　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊　　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(026) VALUE
              "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(027) VALUE
                "全件作表=1  未変換分=5 ... ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "413" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "52" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "52" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "52" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "52" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "52" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "52" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "52" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "13" "22" "27" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "30" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "2" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "13" "48" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "47" "1" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "79" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "79" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           ACCEPT H-DATE FROM DATE.
           MOVE ZERO TO W-NGD.
           COPY LIBCPR.
           MOVE D-NTNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT NOT = 1 AND 5
               GO TO M-10
           END-IF.
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
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-15
           END-IF
           IF  W-ACT = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
       M-20.
      *           READ TDT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE TDT-M_IDLST TDT-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  W-ACT = 5
               IF (TD-HCR NOT = 0) AND (TD-HCT NOT = 0)
                                   AND (TD-HCK NOT = 0)
                   GO TO M-20
               END-IF
           END-IF
           MOVE TD-SNEN TO W-NEN.
           MOVE TD-GET TO W-GET.
           IF  W-NG > W-NGD
               GO TO M-20
           END-IF
      *
           CALL "PR_Open" RETURNING RESP.
           PERFORM S-10 THRU S-15.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
       M-25.
           MOVE TD-DAT TO W-DAT.
           MOVE ZERO TO CHK.
       M-30.
           MOVE SPACE TO W-TNA W-BNA W-SNA.
           IF  TD-KBN > 19
               GO TO M-35
           END-IF
           MOVE TD-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　得意先　なし　　＊＊" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
           GO TO M-40.
       M-35.
           MOVE TD-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　＊＊　　仕入先　なし　　＊＊" TO S-NAME
           END-IF
           MOVE S-NAME TO W-TNA.
       M-40.
           IF  TD-BK = ZERO
               GO TO M-45
           END-IF
           MOVE TD-BK TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "ＢＡＮＫＭ　無し" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
       M-45.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNA P-BNA P-SNA P-HCR P-HCT P-HCK.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE TD-DAT TO P-DAT
           END-IF
           MOVE TD-KBN TO P-KBN.
           MOVE TD-NO TO P-NO.
           MOVE TD-TCD TO P-TCD.
           MOVE W-TNA TO P-TNA.
           IF  TD-MAN NOT = ZERO
               MOVE TD-MAN TO P-MAN
           END-IF
           MOVE TD-KIN TO P-KIN.
           IF  TD-BK NOT = ZERO
               MOVE TD-BK TO P-BK
               MOVE W-BNA TO P-BNA
               MOVE W-SNA TO P-SNA
           END-IF
           IF  TD-KBN = 20 OR 21 OR 22
               MOVE SPACE TO P-SNA
           END-IF
           IF  TD-KBN < 20
               MOVE TD-RSC TO P-RSC
           END-IF
           IF  TD-HCR = 0
               MOVE "　未" TO P-HCR
           END-IF
           IF  TD-HCT = 0
               MOVE "　未" TO P-HCT
           END-IF
           IF  TD-HCK = 0
               MOVE "　未" TO P-HCK
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               MOVE TD-DAT TO P-DAT
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  TD-KBN > 19
               GO TO M-50
           END-IF
           IF  TD-HAC NOT = SPACE
               MOVE SPACE TO W-P1
               MOVE SPACE TO P-TNA P-BNA P-SNA
               MOVE TD-HAC TO P-TNA
               MOVE "(" TO P-K1
               MOVE ")" TO P-K2
               MOVE W-P1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           GO TO M-55.
       M-50.
           MOVE SPACE TO W-P2.
           MOVE TD-ZR TO P-ZR.
           MOVE TD-ZSHZ TO P-ZSHZ.
           MOVE TD-SS TO P-SS.
           MOVE TD-SSHZ TO P-SSHZ.
           MOVE TD-SB TO P-SB.
           MOVE TD-GC TO P-GC.
           MOVE TD-SZ TO P-SZ.
           MOVE TD-EG TO P-EG.
           MOVE TD-ST TO P-ST.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       M-55.
      *           READ TDT-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-95
           END-IF
           IF  W-ACT = 5
               IF (TD-HCR NOT = 0) AND (TD-HCT NOT = 0)
                                   AND (TD-HCK NOT = 0)
                   GO TO M-55
               END-IF
           END-IF
           MOVE TD-SNEN TO W-NEN.
           MOVE TD-GET TO W-GET.
           IF  W-NG > W-NGD
               GO TO M-55
           END-IF
           IF  TD-DAT = W-DAT
               GO TO M-30
           END-IF
           GO TO M-25.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-15.
           EXIT.
