       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBP910.
      ****************************************************
      *****    銀行　入出金取引伝送データ　明細表    *****
      *****        (  中国銀行  JS-SIGN=0  )         *****
      *****        (  商工中金  JS-SIGN=1  )         *****
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN          PIC  9(001).
       77  ERR-STAT         PIC  X(002).
       77  W-EC             PIC  9(001) VALUE 0.
       01  W-R1.
           02  W1-DC        PIC  9(001).
           02  F            PIC  X(003).
           02  W1-NGP       PIC  9(006).
           02  W1-SNGP      PIC  9(006).
           02  W1-ENGP      PIC  9(006).
           02  W1-BKC       PIC  9(004).
           02  W1-BKN       PIC  X(015).
           02  W1-HSC       PIC  9(003).
           02  W1-HSN       PIC  X(015).
           02  F            PIC  X(003).
           02  W1-YKS       PIC  9(001).
           02  W1-KNO       PIC  9(010).
           02  W1-KNA       PIC  X(040).
           02  W1-PMC       PIC  9(001).
           02  F            PIC  X(001).
           02  W1-ZKIN      PIC  9(014).
           02  F            PIC  X(071).
       01  W-R2.
           02  W2-DC        PIC  9(001).
           02  F            PIC  X(014).
           02  W2-DATE      PIC  9(006).
           02  W2-NSC       PIC  9(001).
           02  W2-TRC       PIC  9(002).
           02  W2-KIN       PIC  9(012).
           02  F            PIC  X(024).
           02  W2-TKC       PIC  9(001).
           02  W2-TKN       PIC  9(007).
           02  F            PIC  X(013).
           02  W2-NAME.
             03  W2-NAME1   PIC  X(027).
             03  W2-NAME2   PIC  X(021).
           02  W2-BKN       PIC  X(015).
           02  W2-HSN       PIC  X(015).
           02  W2-TEK       PIC  X(020).
           02  F            PIC  X(021).
       01  W-R3.
           02  W3-DC        PIC  9(001).
           02  W3-ASU       PIC  9(006).
           02  W3-AKIN      PIC  9(013).
           02  W3-HSU       PIC  9(006).
           02  W3-HKIN      PIC  9(013).
           02  W3-PMC       PIC  9(001).
           02  W3-ZKIN      PIC  9(014).
           02  W3-DSU       PIC  9(007).
           02  F            PIC  X(139).
       01  W-R4.
           02  W4-DC        PIC  9(001).
           02  W4-RSU       PIC  9(010).
           02  W4-KSU       PIC  9(005).
           02  F            PIC  X(184).
       01  HEAD1.
           02  W-20K        PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(033) VALUE SPACE.
           02  F            PIC  N(005) VALUE   "＊＊＊　　".
           02  H-BANK       PIC  N(004).
           02  F            PIC  N(020) VALUE
                  "　入出金取引伝送データ　明細表　　＊＊＊".
           02  F            PIC  X(023) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  N(006) VALUE   "データ作成日".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(006) VALUE   "勘定日（自）".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(006) VALUE   "勘定日（至）".
           02  F            PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F            PIC  N(006) VALUE   "銀　行　名　".
           02  F            PIC  X(011) VALUE "       ｺｰﾄﾞ".
           02  F            PIC  N(008) VALUE   "本　支　店　名　".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "預金種目".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "口座番号".
           02  F            PIC  X(002) VALUE SPACE.
           02  F            PIC  N(006) VALUE   "口　座　名　".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "預入金額".
           02  F            PIC  X(008) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "払出金額".
           02  F            PIC  X(008) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "預金残高".
       01  HEAD3.
           02  F            PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  N(006) VALUE   "預入払出日　".
           02  F            PIC  X(004) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "取引".
           02  F            PIC  X(006) VALUE SPACE.
           02  F            PIC  N(008) VALUE   "手形・小切手　№".
           02  F            PIC  X(003) VALUE SPACE.
           02  F            PIC  N(010) VALUE   "振込依頼人名（摘要）".
           02  F            PIC  X(011) VALUE SPACE.
           02  F            PIC  N(006) VALUE   "仕向銀行名　".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "仕向店名".
           02  F            PIC  X(051) VALUE SPACE.
       01  W-P1.
           02  P1-NGP       PIC 99/99/99.
           02  F            PIC  X(002).
           02  P1-SNGP      PIC 99/99/99.
           02  F            PIC  X(002).
           02  P1-ENGP      PIC 99/99/99.
           02  F            PIC  X(002).
           02  P1-BKC       PIC  9(004).
           02  F            PIC  X(001).
           02  P1-BKN       PIC  X(015).
           02  F            PIC  X(001).
           02  P1-HSC       PIC  9(003).
           02  F            PIC  X(001).
           02  P1-HSN       PIC  X(015).
           02  F            PIC  X(001).
           02  P1-YKSN      PIC  N(004).
           02  F            PIC  X(001).
           02  P1-KNO       PIC  9(007).
           02  F            PIC  X(001).
           02  P1-KNA       PIC  X(012).
           02  F            PIC  X(024).
           02  P1-ZKIN      PIC --,---,---,--9.
       01  W-P2.
           02  P2-DATE      PIC 99/99/99.
           02  F            PIC  X(001).
           02  P2-NSCN      PIC  N(002).
           02  F            PIC  X(001).
           02  P2-TRCN      PIC  N(006).
           02  P2-TKCN      PIC  N(004).
           02  F            PIC  X(001).
           02  P2-TKN       PIC  Z(007).
           02  F            PIC  X(001).
           02  P2-D1.
             03  P2-NAME    PIC  X(048).
             03  F          PIC  X(009).
           02  P2-D2  REDEFINES P2-D1.
             03  F          PIC  X(026).
             03  P2-BKN     PIC  X(015).
             03  F          PIC  X(001).
             03  P2-HSN     PIC  X(015).
           02  P2-AKIN      PIC --,---,---,---.
           02  P2-AKIND  REDEFINES P2-AKIN.
             03  F          PIC  X(008).
             03  P2-F1      PIC  X(001).
             03  P2-ASU     PIC  -(003).
             03  P2-R1      PIC  X(001).
             03  F          PIC  X(001).
           02  P2-HKIN      PIC --,---,---,---.
           02  P2-HKIND  REDEFINES P2-HKIN.
             03  F          PIC  X(008).
             03  P2-F2      PIC  X(001).
             03  P2-HSU     PIC  -(003).
             03  P2-R2      PIC  X(001).
             03  F          PIC  X(001).
           02  P2-ZKIN      PIC --,---,---,---.
       01  W-P3.
           02  P3-X1        PIC  X(098).
           02  P3-X2        PIC  X(038).
       01  W-DATA.
           02  W-BANK       PIC  N(004).
           02  W-DMM        PIC  9(001).
           02  W-PC         PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-PAGE       PIC  9(002).
           02  W-ZKIN       PIC S9(010).
           02  WT-D.
             03  WT-DSU     PIC  9(004).
             03  WT-ASU     PIC  9(004).
             03  WT-HSU     PIC  9(004).
             03  WT-AKIN    PIC S9(010).
             03  WT-HKIN    PIC S9(010).
             03  WT-ZKIN    PIC S9(010).
           02  W-RSU        PIC  9(004).
           02  W-KSU        PIC  9(002).
           COPY LSTAT.
      *
           COPY LSPF.
      *FD  RNOUSTF
       01  RNOUSTF_FBP910.
           02  RNOUSTF_PNAME1 PIC  X(007) VALUE "RNOUSTF".
           02  F              PIC  X(001).
           02  RNOUSTF_LNAME  PIC  X(014) VALUE "RNOUSTF_FBP910".
           02  F              PIC  X(001).
           02  RNOUSTF_KEY1   PIC  X(100) VALUE SPACE.
           02  RNOUSTF_SORT   PIC  X(100) VALUE SPACE.
           02  RNOUSTF_IDLST  PIC  X(100) VALUE SPACE.
           02  RNOUSTF_RES    USAGE  POINTER.
       01  RNOUST-R.
           02  NS-DC          PIC  9(001).
           02  F              PIC  X(199).
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
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　入出金取引　明細表　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-BANK  PIC  N(004).
           02  D-CLM   PIC  N(016) VALUE
                  "【　　ＤＡＴＡ　ＣＬＥＡＲ　　】".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME3   PIC  X(024) VALUE
                  "***  ﾆｭｳｷﾝ ｹﾝｽｳ ｴﾗｰ  ***".
             03  E-ME4   PIC  X(025) VALUE
                  "***  ﾆｭｳｷﾝ ｷﾝｶﾞｸ ｴﾗｰ  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  ｼｭｯｷﾝ ｹﾝｽｳ ｴﾗｰ  ***".
             03  E-ME6   PIC  X(025) VALUE
                  "***  ｼｭｯｷﾝ ｷﾝｶﾞｸ ｴﾗｰ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  ｻﾞﾝﾀﾞｶ ｷﾝｶﾞｸ ｴﾗｰ  ***".
             03  E-ME8   PIC  X(023) VALUE
                  "***  DATA ｹﾝｽｳ ｴﾗｰ  ***".
             03  E-ME9   PIC  X(025) VALUE
                  "***  RECORD ｹﾝｽｳ ｴﾗｰ  ***".
             03  E-ME10  PIC  X(023) VALUE
                  "***  ｺｳｻﾞ ｹﾝｽｳ ｴﾗｰ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  ｷﾞﾝｺｳ ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
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
            "C-MID" " " "0" "0" "358" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "48" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "48" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "48" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "48" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "48" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "48" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "48" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "23" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "40" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "40" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BANK" "N" "6" "20" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BANK" BY REFERENCE W-BANK "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CLM" "RN" "15" "16" "32" "D-BANK" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "311" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "311" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "24" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "23" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "25" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "23" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
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
           MOVE SPACE TO W-BANK.
           IF  JS-SIGN = 0
               MOVE   "中国銀行" TO W-BANK
           END-IF
           IF  JS-SIGN = 1
               MOVE   "商工中金" TO W-BANK
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BANK" D-BANK "p" RETURNING RESU.
           MOVE W-BANK TO H-BANK.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-10
           END-IF
           MOVE ZERO TO W-PC W-PAGE W-RSU W-KSU.
           ACCEPT H-DATE FROM DATE.
           CALL "DB_F_Open" USING
            "INPUT" RNOUSTF_PNAME1 " " BY REFERENCE RNOUSTF_IDLST "0".
       M-15.
      *           READ RNOUSTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTF_PNAME1 BY REFERENCE RNOUST-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1
               GO TO M-95
           END-IF
           IF  NS-DC NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1
               GO TO M-95
           END-IF
           INITIALIZE W-R1.
           MOVE RNOUST-R TO W-R1.
           IF  JS-SIGN = 0
               IF  W1-BKC NOT = 0168
                   CALL "SD_Output" USING
                    "E-ME11" E-ME11 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "DB_F_Close" USING
                    BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1
                   GO TO M-95
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  W1-BKC NOT = 2004
                   CALL "SD_Output" USING
                    "E-ME11" E-ME11 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "DB_F_Close" USING
                    BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1
                   GO TO M-95
               END-IF
           END-IF.
       M-20.
           ADD 1 TO W-RSU W-KSU.
           IF  W-PC = ZERO
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-10 THRU S-15
           END-IF
           PERFORM S-20 THRU S-25.
           PERFORM S-30 THRU S-35.
           MOVE ZERO TO WT-D.
       M-25.
      *           READ RNOUSTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTF_PNAME1 BY REFERENCE RNOUST-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO M-55
           END-IF
           IF  NS-DC = 8
               GO TO M-30
           END-IF
           IF  NS-DC NOT = 2
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO M-55
           END-IF
           ADD 1 TO W-RSU WT-DSU.
           PERFORM S-40 THRU S-55.
           GO TO M-25.
       M-30.
           ADD 1 TO W-RSU.
           PERFORM S-60 THRU S-75.
      *           READ RNOUSTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTF_PNAME1 BY REFERENCE RNOUST-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO M-55
           END-IF
           IF  NS-DC = 9
               GO TO M-35
           END-IF
           IF  NS-DC NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO M-55
           END-IF
           GO TO M-20.
       M-35.
           MOVE RNOUST-R TO W-R4.
           ADD 1 TO W-RSU.
           IF  W-RSU NOT = W4-RSU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
           END-IF
           IF  W-KSU NOT = W4-KSU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
           END-IF
      *           READ RNOUSTF AT END
      *//////////////////////
           CALL "DB_Read" USING
            "AT END" RNOUSTF_PNAME1 BY REFERENCE RNOUST-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           CALL "SD_Output" USING "E-ME2" E-ME2 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           MOVE 9 TO W-EC.
       M-55.
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           IF  W-EC = 9
               GO TO M-95
           END-IF
           CALL "SD_Output" USING "D-CLM" D-CLM "p" RETURNING RESU.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-DMM = 9
               GO TO M-95
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-60
           END-IF
      *
           CALL "DB_F_Open" USING
            "OUTPUT" RNOUSTF_PNAME1 " " BY REFERENCE RNOUSTF_IDLST "0".
           CALL "DB_F_Close" USING
            BY REFERENCE RNOUSTF_IDLST RNOUSTF_PNAME1.
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
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P3.
           MOVE ALL "-" TO W-P3.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-15.
           EXIT.
       S-20.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 54
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE RNOUST-R TO W-R1.
           MOVE SPACE TO W-P1.
           MOVE W1-NGP TO P1-NGP.
           MOVE W1-SNGP TO P1-SNGP.
           MOVE W1-ENGP TO P1-ENGP.
           MOVE W1-BKC TO P1-BKC.
           MOVE W1-BKN TO P1-BKN.
           MOVE W1-HSC TO P1-HSC.
           MOVE W1-HSN TO P1-HSN.
           MOVE   "　　　　" TO P1-YKSN.
           IF  W1-YKS = 1
               MOVE   "普通預金" TO P1-YKSN
           END-IF
           IF  W1-YKS = 2
               MOVE   "当座預金" TO P1-YKSN
           END-IF
           MOVE W1-KNO TO P1-KNO.
           MOVE W1-KNA TO P1-KNA.
           IF  W1-PMC = 1
               COMPUTE W-ZKIN = 1 * W1-ZKIN
           ELSE
               COMPUTE W-ZKIN = -1 * W1-ZKIN
           END-IF
           MOVE W-ZKIN TO P1-ZKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P3.
           MOVE ALL "･" TO P3-X1.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-25.
           EXIT.
       S-30.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-35.
           EXIT.
       S-40.
           MOVE RNOUST-R TO W-R2.
           MOVE SPACE TO W-P2.
           MOVE W2-DATE TO P2-DATE.
           IF  W2-NSC = 1
               MOVE   "入金" TO P2-NSCN
               MOVE W2-KIN TO P2-AKIN
               ADD 1 TO WT-ASU
               ADD W2-KIN TO W-ZKIN WT-AKIN
           ELSE
               MOVE   "出金" TO P2-NSCN
               MOVE W2-KIN TO P2-HKIN
               ADD 1 TO WT-HSU
               ADD W2-KIN TO WT-HKIN
               SUBTRACT W2-KIN FROM W-ZKIN
           END-IF
           MOVE W-ZKIN TO P2-ZKIN.
           MOVE   "　　　　　　" TO P2-TRCN.
           IF  W2-TRC = 10
               MOVE   "現　金　　　" TO P2-TRCN
           END-IF
           IF  W2-TRC = 11
               MOVE   "振　込　　　" TO P2-TRCN
           END-IF
           IF  W2-TRC = 12
               MOVE   "他店券入金　" TO P2-TRCN
           END-IF
           IF  W2-TRC = 13
               MOVE   "交　換　　　" TO P2-TRCN
           END-IF
           IF  W2-TRC = 14
               MOVE   "振　替　　　" TO P2-TRCN
           END-IF
           IF  W2-TRC = 18
               MOVE   "その他　　　" TO P2-TRCN
           END-IF
           IF  W2-TRC = 19
               MOVE   "訂　正　　　" TO P2-TRCN
           END-IF
           MOVE   "　　　　" TO P2-TKCN.
           IF  W2-TKC = 1
               MOVE   "小切手　" TO P2-TKCN
           END-IF
           IF  W2-TKC = 2
               MOVE   "約束手形" TO P2-TKCN
           END-IF
           IF  W2-TKC = 3
               MOVE   "為替手形" TO P2-TKCN
           END-IF
           MOVE W2-TKN TO P2-TKN.
           MOVE ZERO TO CHK.
           MOVE W2-NAME TO P2-NAME.
           IF  W2-NAME2 NOT = SPACE
               MOVE 5 TO CHK
               GO TO S-45
           END-IF
           IF  W2-NAME1 = SPACE
               MOVE W2-TEK TO P2-NAME
           END-IF
           MOVE W2-BKN TO P2-BKN.
           MOVE W2-HSN TO P2-HSN.
       S-45.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 62
               GO TO S-50
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-30 THRU S-35.
       S-50.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  CHK = ZERO
               GO TO S-55
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P2-NSCN P2-TRCN P2-TKCN.
           MOVE W2-BKN TO P2-BKN.
           MOVE W2-HSN TO P2-HSN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-55.
           EXIT.
       S-60.
           MOVE RNOUST-R TO W-R3.
           IF  ZERO = WT-DSU AND WT-ASU AND WT-HSU AND
                                WT-AKIN AND WT-HKIN
               GO TO S-70
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 60
               GO TO S-65
           END-IF
           PERFORM S-05 THRU S-15.
           PERFORM S-30 THRU S-35.
       S-65.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P2-NSCN P2-TRCN P2-TKCN.
           MOVE "                         ***  TOTAL  ***" TO P2-NAME.
           MOVE WT-AKIN TO P2-AKIN.
           MOVE WT-HKIN TO P2-HKIN.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P2-NSCN P2-TRCN P2-TKCN.
           MOVE "                             ( ｹﾝｽｳ )   " TO P2-NAME.
           MOVE "(" TO P2-F1 P2-F2.
           MOVE ")" TO P2-R1 P2-R2.
           MOVE WT-ASU TO P2-ASU.
           MOVE WT-HSU TO P2-HSU.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  W3-PMC = 1
               COMPUTE WT-ZKIN = 1 * W3-ZKIN
           ELSE
               COMPUTE WT-ZKIN = -1 * W3-ZKIN
           END-IF
           IF  W-ZKIN NOT = WT-ZKIN
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       S-70.
           IF  WT-ASU NOT = W3-ASU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  WT-AKIN NOT = W3-AKIN
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  WT-HSU NOT = W3-HSU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  WT-HKIN NOT = W3-HKIN
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           IF  WT-DSU NOT = W3-DSU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE SPACE TO W-P3.
           MOVE ALL "-" TO W-P3.
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-75.
           EXIT.
