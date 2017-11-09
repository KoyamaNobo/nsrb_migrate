       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSD150.
      ****************************************
      *****     手形・購買支払　変換     *****
      ****************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-DC               PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-D.
             03  W-UDN        PIC  9(004).
             03  W-SDN        PIC  9(004).
           02  W-FKC          PIC  X(002).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-TNGP         PIC  9(008).
           02  W-TNGPD REDEFINES W-TNGP.
             03  W-TNEN       PIC  9(004).
             03  W-TGP        PIC  9(004).
           02  W-TNGPL REDEFINES W-TNGP.
             03  F            PIC  9(002).
             03  W-TNGPS      PIC  9(006).
           02  W-DNGP         PIC  9(008).
           02  W-DNGPD REDEFINES W-DNGP.
             03  W-DNEN       PIC  9(004).
             03  W-DGP        PIC  9(004).
           02  W-DNGPL REDEFINES W-DNGP.
             03  F            PIC  9(002).
             03  W-DNGPS      PIC  9(006).
           02  W-KIN          PIC S9(009).
           02  W-DMM          PIC  9(001).
           02  W-CC           PIC  9(001) VALUE 0.
           02  W-EC           PIC  9(001) VALUE 0.
           02  W-SRC          PIC  9(001).
           02  W-FILE         PIC  X(013).
           02  W-BNG.
             03  W-BNEN       PIC  9(004).
             03  W-BNENL REDEFINES W-BNEN.
               04  W-BNEN1    PIC  9(002).
               04  W-BNEN2    PIC  9(002).
             03  W-BGET       PIC  9(002).
           02  W-BNGL  REDEFINES W-BNG.
             03  F            PIC  9(002).
             03  W-BNGS       PIC  9(004).
           02  W-ENG.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  W-EGET       PIC  9(002).
           02  W-ENGL  REDEFINES W-ENG.
             03  F            PIC  9(002).
             03  W-ENGS       PIC  9(004).
           02  W-NTNG.
             03  W-NTN        PIC  9(002).
             03  W-NTG        PIC  9(002).
           02  W-NBNG.
             03  W-NBN        PIC  9(002).
             03  W-NBG        PIC  9(002).
           02  W-OKD          PIC  9(006).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LISM.
           COPY LITM.
           COPY LIUKET.
           COPY LISHIT.
           COPY LIBANK.
      *FD  TDT-M
       01  TDT-M_TSD150.
           02  TDT-M_PNAME1   PIC  X(004) VALUE "TDTM".
           02  F              PIC  X(001).
           02  TDT-M_LNAME    PIC  X(012) VALUE "TDT-M_TSD150".
           02  F              PIC  X(001).
           02  TDT-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TDT-M_SORT     PIC  X(100) VALUE SPACE.
           02  TDT-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TDT-M_RES      USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-TKC.
               04  TD-C1      PIC  9(001).
               04  TD-C2      PIC  9(001).
             03  TD-TNO       PIC  9(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DATE        PIC  9(006).
           02  TD-NGP   REDEFINES TD-DATE.
             03  TD-NG.
               04  TD-NEN     PIC  9(002).
               04  TD-GET     PIC  9(002).
             03  TD-PEY       PIC  9(002).
           02  TD-MND         PIC  9(006).
           02  TD-MNGP  REDEFINES TD-MND.
             03  TD-MN        PIC  9(002).
             03  TD-MG        PIC  9(002).
             03  TD-MP        PIC  9(002).
           02  TD-KIN         PIC S9(010).
           02  TD-BKC         PIC  9(004).
           02  TD-FRN         PIC  N(024).
           02  TD-SAD.
             03  TD-S     OCCURS   7  PIC S9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN.
             03  TD-SNEN1     PIC  9(002).
             03  TD-SNEN2     PIC  9(002).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PC          PIC  9(001).
           02  TD-RSC         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  TNO-M
       01  TNO-M_TSD150.
           02  TNO-M_PNAME1   PIC  X(004) VALUE "TNOM".
           02  F              PIC  X(001).
           02  TNO-M_LNAME    PIC  X(012) VALUE "TNO-M_TSD150".
           02  F              PIC  X(001).
           02  TNO-M_KEY1     PIC  X(100) VALUE SPACE.
           02  TNO-M_SORT     PIC  X(100) VALUE SPACE.
           02  TNO-M_IDLST    PIC  X(100) VALUE SPACE.
           02  TNO-M_RES      USAGE  POINTER.
       01  TNO-R.
           02  NO-KEY         PIC  X(002).
           02  NO-UMN         PIC  9(004).
           02  NO-SMN         PIC  9(004).
           02  NO-D.
             03  NO-UDN       PIC  9(004).
             03  NO-SDN       PIC  9(004).
           02  NO-HDN         PIC  9(004).
           02  NO-NDN         PIC  9(004).
           02  F              PIC  X(230).
       77  F                  PIC  X(001).
      *FD  JSS-F
       01  JSS-F_TSD150.
           02  JSS-F_PNAME1   PIC  X(005) VALUE "TJSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_TSD150".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC          PIC  9(002).
           02  JS-DATE        PIC  9(008).
           02  JS-NGP   REDEFINES JS-DATE.
             03  JS-NEN       PIC  9(004).
             03  F            PIC  X(004).
           02  JS-SCD.
             03  JS-SCD1      PIC  9(001).
             03  JS-SCD2      PIC  9(003).
           02  JS-JCD         PIC  9(006).
           02  F              PIC  X(017).
           02  JS-KIN         PIC S9(008).
           02  JS-SHZ         PIC S9(007).
           02  F              PIC  X(019).
           02  JS-YC          PIC  9(001).
           02  F              PIC  X(002).
           02  JS-SC          PIC  9(001).
           02  F              PIC  X(027).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
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
                "＊＊＊　　手形・購買支払　変換　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(026) VALUE
                  "***  UKETM ﾆｼﾞｭｳ ﾄｳﾛｸ  ***".
             03  E-ME2   PIC  X(026) VALUE
                  "***  SHITM ﾆｼﾞｭｳ ﾄｳﾛｸ  ***".
             03  E-ME3   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME4   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-ME5   PIC  X(023) VALUE
                  "***  ｼﾃ ｱｲﾃｶﾓｸ ｴﾗｰ  ***".
             03  E-ME6   PIC  X(025) VALUE
                  "***  UKETM WRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(025) VALUE
                  "***  SHITM WRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-ME9   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10  PIC  X(017) VALUE
                  "***  TNOM ﾅｼ  ***".
             03  E-ME11  PIC  X(026) VALUE
                  "***  TNOM REWRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(018) VALUE
                  "***  DATEM ﾅｼ  ***".
             03  E-ME13  PIC  X(027) VALUE
                  "***  DATEM REWRITE ｴﾗｰ  ***".
             03  E-ME15  PIC  X(025) VALUE
                  "***  JSSRF WRITE ｴﾗｰ  ***".
             03  E-ME16  PIC  X(027) VALUE
                  "***  BANKM REWRITE ｴﾗｰ  ***".
             03  E-ME17  PIC  X(018) VALUE
                  "***  BANKM ﾅｼ  ***".
             03  E-ME18  PIC  X(024) VALUE
                  "***  SM REWRITE ｴﾗｰ  ***".
             03  E-ME21  PIC  X(026) VALUE
                  "***  TDTM REWRITE ｴﾗｰ  ***".
             03  E-ME25  PIC  X(025) VALUE
                  "***  UKETM ｵﾁｺﾐﾋﾞｴﾗｰ  ***".
             03  E-KEY   PIC  X(006).
             03  E-TCD   PIC  X(004).
             03  E-NGP   PIC  9(008).
             03  E-BKC   PIC  9(004).
             03  E-ME71.
               04  FILLER  PIC  X(013).
               04  FILLER  PIC  N(021) VALUE
                    "オーバーフロー、領域を拡張し、ＦＮＣ＋再開".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "302" " " " " RETURNING RESU.
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
            "08C-MID" "X" "20" "23" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "597" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "597" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "26" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "26" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "15" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "15" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "23" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "25" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "18" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "18" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "17" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "26" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "18" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "27" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "25" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "27" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "18" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" "X" "24" "15" "24" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "26" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" "X" "24" "15" "25" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "50" "6" "E-ME25" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TD-KEY "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "X" "24" "50" "4" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE TD-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NGP" "9" "24" "60" "8" "E-TCD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NGP" BY REFERENCE CL-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-BKC" "9" "24" "50" "4" "E-NGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-BKC" BY REFERENCE TD-BKC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME71" " " "24" "0" "55" "E-BKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME71" "X" "24" "1" "13" " " "E-ME71" RETURNING RESU.
       CALL "SD_From" USING 
            "01E-ME71" BY REFERENCE W-FILE "13" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME71" "N" "24" "15" "42" "01E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME71" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDT-M_PNAME1 " " BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
           CALL "DB_F_Open" USING
            "I-O" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           CALL "DB_F_Open" USING
            "I-O" SHIT-M_PNAME1 " " BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           COPY LIBCPR.
           MOVE ZERO TO W-D W-NGP.
           MOVE D-NTNG TO W-NTNG.
           MOVE D-NBNG TO W-NBNG.
       M-15.
      *           READ TDT-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  TD-PC NOT = 5
               GO TO M-15
           END-IF
           IF (TD-HCT = 1 OR 9) AND (TD-HCK = 1 OR 9)
               GO TO M-15
           END-IF
           IF  TD-C1 = 0
               GO TO M-15
           END-IF
      *
           IF  TD-C1 NOT = 1
               GO TO M-25
           END-IF
           IF  TD-HCT = 1 OR 9
               GO TO M-15
           END-IF
           IF (TD-SNEN2 NOT = W-NTN) OR (TD-GET NOT = W-NTG)
               GO TO M-15
           END-IF
           PERFORM S-160 THRU S-280.
           GO TO M-30.
       M-25.
           IF  TD-C1 NOT = 2 AND 3
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           IF (TD-HCT = 1 OR 9) AND (TD-HCK = 1 OR 9)
               GO TO M-15
           END-IF
           IF (TD-HCT = 0) AND
              ((TD-SNEN2 NOT = W-NTN) OR (TD-GET NOT = W-NTG))
               GO TO M-15
           END-IF
           IF (TD-HCK = 0) AND
              ((TD-SNEN2 NOT = W-NBN) OR (TD-GET NOT = W-NBG))
               GO TO M-15
           END-IF
           PERFORM S-300 THRU S-500.
       M-30.
           IF  W-EC = 9
               GO TO M-85
           END-IF
           IF  TD-BKC = ZERO
               GO TO M-15
           END-IF
           MOVE TD-BKC TO B-KEY.
      *           READ BANK-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-BKC" E-BKC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-15
           END-IF
           MOVE ZERO TO W-ENG.
           MOVE B-NG TO W-ENGS.
           IF  B-NG NOT = ZERO
               IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
                   ADD DATE-NC1 TO W-ENEN
               END-IF
           END-IF
           IF  B-NG NOT = ZERO
               IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
                   ADD DATE-NC2 TO W-ENEN
               END-IF
           END-IF
           MOVE ZERO TO W-BNG.
           MOVE TD-NG TO W-BNGS.
           IF  W-BNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-BNEN
           ELSE
               IF  W-BNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-BNEN
               END-IF
           END-IF
           IF  W-BNG NOT > W-ENG
               GO TO M-15
           END-IF
           MOVE W-BNGS TO B-NG.
      *           REWRITE BANK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            BANK-M_PNAME1 BANK-M_LNAME BANK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-BKC" E-BKC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO M-15.
       M-85.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
      *
           CALL "DB_F_Open" USING
            "I-O" TNO-M_PNAME1 " " BY REFERENCE TNO-M_IDLST "1"
            "NO-KEY" BY REFERENCE NO-KEY.
           MOVE "01" TO NO-KEY.
      *           READ TNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TNO-M_PNAME1 BY REFERENCE TNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           IF ((NO-UMN < W-UDN) OR (NO-UMN = 9999)) OR
              ((NO-UMN > 9900) AND (W-UDN < 0100))
               MOVE W-UDN TO NO-UMN
           END-IF
           IF ((NO-SMN < W-SDN) OR (NO-SMN = 9999)) OR
              ((NO-SMN > 9900) AND (W-SDN < 0100))
               MOVE W-SDN TO NO-SMN
           END-IF
           MOVE ZERO TO NO-D.
      *           REWRITE TNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TNO-M_PNAME1 TNO-M_LNAME TNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE TNO-M_IDLST TNO-M_PNAME1.
       M-90.
           CALL "DB_F_Open" USING
            "I-O" M-DATE_PNAME1 "SHARED" BY REFERENCE M-DATE_IDLST "1"
            "DATE-KEY" BY REFERENCE DATE-KEY.
           MOVE "01" TO DATE-KEY.
      *           READ M-DATE INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" M-DATE_PNAME1 BY REFERENCE DATE-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE M-DATE_IDLST M-DATE_PNAME1
               GO TO M-95
           END-IF
           MOVE ZERO TO W-DNGP.
           MOVE D-TGD TO W-DNGPS.
           IF  W-DNEN >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-DNEN
           END-IF
           IF  W-DNEN >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-DNEN
           END-IF
           IF  W-NGP > W-DNGP
               MOVE W-NGPS TO D-TGD
           END-IF
      *           REWRITE DATE-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            M-DATE_PNAME1 M-DATE_LNAME DATE-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE M-DATE_IDLST M-DATE_PNAME1.
       M-95.
           IF  W-DC = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-020.
           MOVE ZERO TO JSS-R.
           IF  TD-C1 = 2
               MOVE 4 TO JS-SC
           END-IF
           IF  TD-TKC = 30
               MOVE 1 TO JS-SC
           END-IF
           IF  TD-TKC = 31
               MOVE 2 TO JS-SC
           END-IF
           IF  TD-TKC = 32
               MOVE 3 TO JS-SC
           END-IF
           IF  TD-TKC = 33
               MOVE 5 TO JS-SC
           END-IF
           IF  TD-TKC = 34
               MOVE 6 TO JS-SC
           END-IF
           MOVE 30 TO JS-DC.
           MOVE TD-DATE TO JS-DATE.
           MOVE TD-SNEN TO JS-NEN.
           MOVE TD-TCD TO JS-SCD.
       S-040.
           EXIT.
       S-060.
           MOVE ZERO TO JSS-R.
           MOVE 10 TO JS-DC.
           MOVE TD-DATE TO JS-DATE.
           MOVE TD-SNEN TO JS-NEN.
           MOVE TD-TCD TO JS-SCD.
           MOVE 999000 TO JS-JCD.
           MOVE 5 TO JS-YC.
       S-080.
           EXIT.
       S-100.
           MOVE UT-SNM TO CL-NEN.
           MOVE UT-MKG TO CL-GET.
           MOVE UT-MKP TO CL-PEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NGP" E-NGP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-140
           END-IF
           IF  CL-SJ = 0
               MOVE CL-NGPS TO W-OKD
               GO TO S-140
           END-IF.
       S-120.
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NGP" E-NGP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE UT-SNM TO CL-NEN
               MOVE UT-MKG TO CL-GET
               MOVE UT-MKP TO CL-PEY
               MOVE CL-NGPS TO W-OKD
               GO TO S-140
           END-IF
           IF  CL-SJ = 1
               GO TO S-120
           END-IF
           IF  UT-MKG = CL-GET
               MOVE CL-NGPS TO W-OKD
               GO TO S-140
           END-IF
           MOVE ZERO TO CL-KEY.
           MOVE UT-SNM TO CL-NEN.
           MOVE UT-MKG TO CL-GET.
           MOVE UT-MKP TO CL-PEY.
      *           START CALNM KEY NOT < CL-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            CALNM_PNAME1 "CL-KEY" " NOT < " CL-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE CL-NGPS TO W-OKD
               GO TO S-140
           END-IF.
       S-130.
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NGP" E-NGP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE UT-SNM TO CL-NEN
               MOVE UT-MKG TO CL-GET
               MOVE UT-MKP TO CL-PEY
               MOVE CL-NGPS TO W-OKD
               GO TO S-140
           END-IF
           IF  UT-MKG = CL-GET
               MOVE CL-NGPS TO W-OKD
               GO TO S-130
           END-IF.
       S-140.
           EXIT.
      *****-------------------------------------------------------------
       S-160.
           MOVE TD-TNO TO UT-KEY.
      *           READ UKET-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-180
           END-IF
           CALL "SD_Output" USING "E-ME1" E-ME1 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO S-280.
       S-180.
           MOVE TD-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO T-FKC
           END-IF.
       S-200.
           INITIALIZE UKET-R.
           MOVE TD-TNO TO UT-KEY.
           MOVE TD-TKC TO UT-TSC.
           MOVE TD-BKC TO UT-BCD.
           MOVE T-FKC TO UT-FKC.
           MOVE TD-TCD TO UT-TCD.
           MOVE TD-FRN TO UT-FDM.
           MOVE TD-KIN TO UT-KIN.
           MOVE TD-DATE TO UT-UTD.
           MOVE TD-MND TO UT-MKD.
           MOVE TD-SNEN TO UT-SNU.
           IF  TD-MN >= DATE-YF1 AND <= DATE-YT1
               COMPUTE UT-SNM = TD-MN + DATE-YC1
           ELSE
               IF  TD-MN >= DATE-YF2 AND <= DATE-YT2
                   COMPUTE UT-SNM = TD-MN + DATE-YC2
               END-IF
           END-IF
           PERFORM S-100 THRU S-140.
           IF  W-OKD = ZERO
               CALL "SD_Output" USING
                "E-ME25" E-ME25 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           MOVE W-OKD TO UT-OKD.
      *           WRITE UKET-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            UKET-M_PNAME1 UKET-M_LNAME UKET-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-220
           END-IF
           IF  TD-TNO > W-UDN
               MOVE TD-TNO TO W-UDN
           END-IF
           MOVE ZERO TO W-TNGP.
           MOVE TD-DATE TO W-TNGPS.
           IF  W-TNEN >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-TNEN
           ELSE
               IF  W-TNEN >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-TNEN
               END-IF
           END-IF
           IF  W-TNGP > W-NGP
               MOVE W-TNGP TO W-NGP
           END-IF
           MOVE 1 TO TD-HCT.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-EC
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-280.
       S-220.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO S-280
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
           MOVE "UKETM        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" UKET-M_PNAME1 " " BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
           GO TO S-200.
       S-280.
           EXIT.
      *****-------------------------------------------------------------
       S-300.
           MOVE 0 TO W-SRC.
           MOVE TD-TCD TO S-KEY.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE ZERO TO S-FKC
               GO TO S-310
           END-IF
           MOVE ZERO TO W-ENG.
           MOVE S-ENG TO W-ENGS.
           IF  S-ENG NOT = ZERO
               IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
                   ADD DATE-NC1 TO W-ENEN
               END-IF
           END-IF
           IF  S-ENG NOT = ZERO
               IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
                   ADD DATE-NC2 TO W-ENEN
               END-IF
           END-IF
           MOVE ZERO TO W-BNG.
           MOVE TD-NG TO W-BNGS.
           IF  W-BNEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-BNEN
           ELSE
               IF  W-BNEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-BNEN
               END-IF
           END-IF
           IF  W-BNG > W-ENG
               MOVE 1 TO W-SRC
               MOVE W-BNGS TO S-ENG
           END-IF
           IF  TD-C1 = 2
               IF  S-TGC = 0
                   MOVE 1 TO W-SRC
                   MOVE 1 TO S-TGC
               END-IF
           END-IF
           IF  W-SRC = 0
               GO TO S-310
           END-IF
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       S-310.
           IF  TD-C1 = 3
               GO TO S-360
           END-IF
           IF (TD-HCT = 1 OR 9) OR
              ((TD-SNEN2 NOT = W-NTN) OR (TD-GET NOT = W-NTG))
               GO TO S-360
           END-IF
           MOVE TD-TNO TO ST-KEY.
      *           READ SHIT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SHIT-M_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-320
           END-IF
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO S-500.
       S-320.
           INITIALIZE SHIT-R.
           MOVE TD-TNO TO ST-KEY.
           MOVE TD-TKC TO ST-TSC.
           MOVE TD-BKC TO ST-BCD.
           MOVE S-FKC TO ST-FKC.
           MOVE TD-TCD TO ST-TCD.
           MOVE TD-KIN TO ST-KIN.
           MOVE TD-DATE TO ST-FDD.
           MOVE TD-MND TO ST-MKD.
           MOVE TD-SAD TO ST-UKD.
           ADD TD-ZSHZ TO ST-UK(1).
           ADD TD-SSHZ TO ST-UK(2).
           MOVE TD-SNEN TO ST-SNF.
           IF  TD-MN >= DATE-YF1 AND <= DATE-YT1
               COMPUTE ST-SNM = TD-MN + DATE-YC1
           ELSE
               IF  TD-MN >= DATE-YF2 AND <= DATE-YT2
                   COMPUTE ST-SNM = TD-MN + DATE-YC2
               END-IF
           END-IF
      *           WRITE SHIT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SHIT-M_PNAME1 SHIT-M_LNAME SHIT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-340
           END-IF
           IF  TD-TNO > W-SDN
               MOVE TD-TNO TO W-SDN
           END-IF
           MOVE ZERO TO W-TNGP.
           MOVE TD-DATE TO W-TNGPS.
           IF  W-TNEN >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-TNEN
           ELSE
               IF  W-TNEN >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-TNEN
               END-IF
           END-IF
           IF  W-TNGP > W-NGP
               MOVE W-TNGP TO W-NGP
           END-IF
           MOVE 1 TO TD-HCT.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-EC
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-360.
       S-340.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO S-500
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1.
           MOVE "SHITM        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" SHIT-M_PNAME1 " " BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           GO TO S-320.
       S-360.
           IF  TD-TCD = 1905
               GO TO S-500
           END-IF
           IF (TD-HCK = 1 OR 9) OR
              ((TD-SNEN2 NOT = W-NBN) OR (TD-GET NOT = W-NBG))
               GO TO S-500
           END-IF
           IF  ZERO NOT = TD-S(1) OR TD-ZSHZ
               GO TO S-380
           END-IF
           IF  ZERO NOT = TD-S(2) OR TD-SSHZ
               GO TO S-440
           END-IF
           IF  TD-C1 = 3
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO S-500.
       S-380.
           PERFORM S-020 THRU S-040.
           MOVE TD-S(1) TO JS-KIN.
           MOVE TD-ZSHZ TO JS-SHZ.
      *           WRITE JSS-R.
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 1 TO W-CC
               GO TO S-480
           END-IF
           IF  W-DC = 0
               MOVE 9 TO W-DC
           END-IF
           IF  TD-ZSHZ = ZERO
               GO TO S-420
           END-IF.
       S-400.
           PERFORM S-060 THRU S-080.
           MOVE TD-ZSHZ TO JS-SHZ.
      *           WRITE JSS-R.
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 2 TO W-CC
               GO TO S-480
           END-IF.
       S-420.
           IF  ZERO = TD-S(2) AND TD-SSHZ
               GO TO S-490
           END-IF.
       S-440.
           PERFORM S-020 THRU S-040.
           ADD 5 TO JS-SCD1.
           MOVE TD-S(2) TO JS-KIN.
           MOVE TD-SSHZ TO JS-SHZ.
      *           WRITE JSS-R.
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 3 TO W-CC
               GO TO S-480
           END-IF
           IF  W-DC = 0
               MOVE 9 TO W-DC
           END-IF
           IF  TD-SSHZ = ZERO
               GO TO S-490
           END-IF.
       S-460.
           PERFORM S-060 THRU S-080.
           ADD 5 TO JS-SCD1.
           MOVE TD-SSHZ TO JS-SHZ.
      *           WRITE JSS-R.
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 4 TO W-CC
               GO TO S-480
           END-IF
           GO TO S-490.
       S-480.
           IF  ERR-STAT NOT = "34"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO S-500
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           MOVE "TJSSF        " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" JSS-F_PNAME1 " " BY REFERENCE JSS-F_IDLST "0".
           IF  W-CC = 1
               GO TO S-380
           END-IF
           IF  W-CC = 2
               GO TO S-400
           END-IF
           IF  W-CC = 3
               GO TO S-440
           END-IF
           IF  W-CC = 4
               GO TO S-460
           END-IF
           GO TO S-500.
       S-490.
           MOVE 1 TO TD-HCK.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-EC
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       S-500.
           EXIT.
