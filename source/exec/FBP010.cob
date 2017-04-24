       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBP010.
      *******************************************************
      *****    中銀・商中　総合振込伝送データ　リスト   *****
      *******************************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT         PIC  X(002).
       01  HEAD1.
           02  F            PIC  X(005) VALUE X"1A24212474".
           02  F            PIC  X(031) VALUE SPACE.
           02  F            PIC  N(005) VALUE   "＊＊＊　　".
           02  H-BKN        PIC  N(004).
           02  F            PIC  N(019) VALUE
                  "　総合振込伝送データ　リスト　　＊＊＊".
           02  F            PIC  X(024) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC Z9.
       01  HEAD2.
           02  F            PIC  N(002) VALUE   "種別".
           02  F            PIC  X(009) VALUE "ｺｰﾄﾞ ｺｰﾄﾞ".
           02  F            PIC  N(002) VALUE   "区分".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "依頼人".
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(007) VALUE   "依　頼　人　名".
           02  F            PIC  X(026) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "振込月日".
           02  F            PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F            PIC  N(005) VALUE   "仕向銀行名".
           02  F            PIC  X(010) VALUE "     ｺｰﾄﾞ ".
           02  F            PIC  N(006) VALUE   "仕向本支店名".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "預金種目".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "口座番号".
       01  HEAD3.
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(003) VALUE   "銀行名".
           02  F            PIC  X(014) VALUE "         ｺｰﾄﾞ ".
           02  F            PIC  N(010) VALUE   "本支店名　　　交換所".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "預金種目".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "口座番号".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(005) VALUE   "受取人氏名".
           02  F            PIC  X(025) VALUE SPACE.
           02  F            PIC  N(003) VALUE   "金　額".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "新規".
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(002) VALUE   "社内".
           02  F            PIC  X(014) VALUE "ｺｰﾄﾞ          ".
       01  W-P1.
           02  F            PIC  X(003).
           02  P1-FRC       PIC  9(002).
           02  F            PIC  X(008).
           02  P1-CDC       PIC  9(001).
           02  F            PIC  X(004).
           02  P1-NRC       PIC  X(010).
           02  F            PIC  X(001).
           02  P1-NRN       PIC  X(040).
           02  F            PIC  X(002).
           02  P1-FGP       PIC 99/99.
           02  F            PIC  X(002).
           02  P1-CBC       PIC  9(004).
           02  F            PIC  X(001).
           02  P1-CBN       PIC  X(015).
           02  F            PIC  X(001).
           02  P1-CSC       PIC  9(003).
           02  F            PIC  X(001).
           02  P1-CSN       PIC  X(015).
           02  F            PIC  X(002).
           02  P1-YCD       PIC  X(001).
           02  F            PIC  X(004).
           02  P1-CCN       PIC  9(007).
           02  F            PIC  X(001).
       01  W-P2.
           02  P2-BKC       PIC  9(004).
           02  F            PIC  X(001).
           02  P2-BKN       PIC  X(015).
           02  F            PIC  X(001).
           02  P2-HSC       PIC  9(003).
           02  F            PIC  X(001).
           02  P2-HSN       PIC  X(015).
           02  F            PIC  X(001).
           02  P2-KBG       PIC  X(004).
           02  F            PIC  X(005).
           02  P2-YKC       PIC  9(001).
           02  F            PIC  X(004).
           02  P2-KNO       PIC  9(007).
           02  F            PIC  X(002).
           02  P2-UNA       PIC  X(030).
           02  P2-TP    REDEFINES P2-UNA.
             03  F          PIC  X(004).
             03  P2-TM      PIC  X(016).
             03  P2-KSU     PIC ZZZZZ9.
             03  P2-KM      PIC  N(001).
             03  F          PIC  X(002).
           02  P2-KIN       PIC ZZZ,ZZZ,ZZ9.
           02  F            PIC  X(005).
           02  P2-SCD       PIC  9(001).
           02  F            PIC  X(007).
           02  P2-KEY       PIC  9(004).
           02  F            PIC  X(011).
       01  W-R              PIC  X(120).
       01  W-R1.
           02  W1-KBN       PIC  9(001) VALUE 1.
           02  W1-FRC       PIC  9(002) VALUE 21.
           02  W1-CDC       PIC  9(001) VALUE 1.
           02  W1-NRC       PIC  X(010).
           02  W1-NRN       PIC  X(040) VALUE
                "ﾆﾂｼﾝｺﾞﾑ(ｶ                               ".
           02  W1-FGP       PIC  9(004).
           02  W1-FGPD REDEFINES W1-FGP.
             03  W1-GET     PIC  9(002).
             03  W1-PEY     PIC  9(002).
           02  W1-CBC       PIC  9(004).
           02  W1-CBN       PIC  X(015).
           02  W1-CSC       PIC  9(003).
           02  W1-CSN       PIC  X(015).
           02  W1-YCD       PIC  9(001) VALUE 2.
           02  W1-CCN       PIC  9(007).
           02  F            PIC  X(017).
       01  W-R2.
           02  W2-KBN       PIC  9(001).
           02  W2-BKC       PIC  9(004).
           02  W2-BKN       PIC  X(015).
           02  W2-HSC       PIC  9(003).
           02  W2-HSN       PIC  X(015).
           02  W2-KBG       PIC  X(004).
           02  W2-YKC       PIC  9(001).
           02  W2-KNO       PIC  9(007).
           02  W2-UNA       PIC  X(030).
           02  W2-KIN       PIC  9(010).
           02  W2-SCD       PIC  9(001).
           02  W2-KEY       PIC  9(004).
           02  F            PIC  X(025).
       01  W-R3.
           02  W3-KBN       PIC  9(001) VALUE 8.
           02  W3-KSU       PIC  9(006).
           02  W3-TKIN      PIC  9(012).
           02  F            PIC  X(101).
       01  W-R4.
           02  W4-KBN       PIC  9(001) VALUE 9.
           02  F            PIC  X(119).
       01  W-DATA.
           02  W-BKCD       PIC  9(001).
           02  W-BKN        PIC  N(004).
           02  W-GP.
             03  W-GET      PIC  9(002).
             03  W-PEY      PIC  9(002).
           02  W-DMM        PIC  9(001).
           02  W-PAGE       PIC  9(002).
           02  W-KIND       PIC S9(009).
           02  W-KSU        PIC  9(004).
           02  W-TKIN       PIC S9(009).
           02  CHK          PIC  9(001).
           02  W-EC         PIC  9(001).
           02  W-ENGD.
             03  W-ENEN     PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1  PIC  9(002).
               04  W-ENEN2  PIC  9(002).
             03  F          PIC  9(002).
           02  W-ENGL  REDEFINES W-ENGD.
             03  F          PIC  9(002).
             03  W-ENGS     PIC  9(004).
           02  W-NGPD.
             03  W-NEN      PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1   PIC  9(002).
               04  W-NEN2   PIC  9(002).
             03  W-GPD      PIC  9(004).
           02  W-NGPL  REDEFINES W-NGPD.
             03  F          PIC  9(002).
             03  W-DATE     PIC  9(006).
             03  W-NGPS  REDEFINES W-DATE.
               04  W-NG     PIC  9(004).
               04  F        PIC  9(002).
           02  W-NGP   REDEFINES W-NGPD.
             03  W-NGD      PIC  9(006).
             03  F          PIC  9(002).
           02  W-D.
             03  W-FKN      PIC  X(030).
             03  W-BKC.
               04  W-BK     PIC  9(004).
               04  W-HS     PIC  9(003).
             03  W-YKS      PIC  9(001).
             03  W-KNO      PIC  9(007).
             03  W-TRC      PIC  9(001).
             03  W-KIN      PIC  9(009).
           02  W-FILE       PIC  X(013).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIFBKM.
           COPY LSPF.
      *FD  FKSM
       01  FKSM_FBP010.
           02  FKSM_PNAME1  PIC  X(004) VALUE "FKSM".
           02  F            PIC  X(001).
           02  FKSM_LNAME   PIC  X(011) VALUE "FKSM_FBP010".
           02  F            PIC  X(001).
           02  FKSM_KEY1    PIC  X(100) VALUE SPACE.
           02  FKSM_SORT    PIC  X(100) VALUE SPACE.
           02  FKSM_IDLST   PIC  X(100) VALUE SPACE.
           02  FKSM_RES     USAGE  POINTER.
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
      *FD  SSOUGOF
       01  SSOUGOF_FBP010.
           02  SSOUGOF_PNAME1 PIC  X(007) VALUE "SSOUGOF".
           02  F              PIC  X(001).
           02  SSOUGOF_LNAME  PIC  X(014) VALUE "SSOUGOF_FBP010".
           02  F              PIC  X(001).
           02  SSOUGOF_KEY1   PIC  X(100) VALUE SPACE.
           02  SSOUGOF_SORT   PIC  X(100) VALUE SPACE.
           02  SSOUGOF_IDLST  PIC  X(100) VALUE SPACE.
           02  SSOUGOF_RES    USAGE  POINTER.
       01  SSOUGO-R           PIC  X(120).
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
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊　　Ｆ・Ｂ　総合振込データ　リスト　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊　　　　　（送信データ作成）　　　　　＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(025) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(042) VALUE
                "【            】         振込日    月   日".
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
             03  E-ME1   PIC  X(021) VALUE
                  "***  ﾌﾘｺﾐﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME2   PIC  X(022) VALUE
                  "***  ｷﾞﾝｺｳﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME3   PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  FBKM REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(024) VALUE
                  "***  SM REWRITE ｴﾗｰ  ***".
             03  E-KEY   PIC  9(004).
             03  E-BKC   PIC  9(007).
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "414" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "50" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "50" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "50" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "50" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "50" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "50" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "50" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "14" "14" "42" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "20" "24" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "41" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DT" " " "14" "0" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-DT" "N" "14" "18" "8" " " "D-DT" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-DT" BY REFERENCE W-BKN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-DT" "Z" "14" "47" "2" "01D-DT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-DT" BY REFERENCE W-GET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-DT" "Z" "14" "52" "2" "02D-DT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-DT" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "158" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "158" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "21" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "22" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "19" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "24" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "9" "24" "50" "4" "E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE FS-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-BKC" "9" "24" "55" "7" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-BKC" BY REFERENCE W-BKC "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
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
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
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
      *
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
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ZERO = FS-KIN1 AND FS-KIN2
               GO TO M-25
           END-IF
           IF (W-BKCD NOT = FS-BKC) OR (W-GP NOT = FS-FGP)
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE FKSM_IDLST FKSM_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE W-GP TO W1-FGP.
           IF  W-BKCD = 1
               MOVE "0168101146" TO W1-NRC
               MOVE 0168 TO W1-CBC
               MOVE "ﾁﾕｳｺﾞｸ         " TO W1-CBN
               MOVE 101 TO W1-CSC
               MOVE "ﾎﾝﾃﾝ           " TO W1-CSN
               MOVE 0006265 TO W1-CCN
           END-IF
           IF  W-BKCD = 2
               MOVE "0000067420" TO W1-NRC
               MOVE 2004 TO W1-CBC
               MOVE "ｼﾖｳﾁﾕｳ         " TO W1-CBN
               MOVE 331 TO W1-CSC
               MOVE "ｵｶﾔﾏ           " TO W1-CSN
               MOVE 2002701 TO W1-CCN
           END-IF
      *
           COPY LIBCPR.
           MOVE ZERO TO W-NGPD.
           ACCEPT W-DATE FROM DATE.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           CALL "DB_F_Open" USING
            "I-O" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" SSOUGOF_PNAME1 " " BY REFERENCE SSOUGOF_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           MOVE SPACE TO SP-R.
           MOVE 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO W-P1.
           MOVE W1-FRC TO P1-FRC.
           MOVE W1-CDC TO P1-CDC.
           MOVE W1-NRC TO P1-NRC.
           MOVE W1-NRN TO P1-NRN.
           MOVE W1-FGP TO P1-FGP.
           MOVE W1-CBC TO P1-CBC.
           MOVE W1-CBN TO P1-CBN.
           MOVE W1-CSC TO P1-CSC.
           MOVE W1-CSN TO P1-CSN.
           MOVE W1-YCD TO P1-YCD.
           MOVE W1-CCN TO P1-CCN.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
      *
           MOVE SPACE TO W-R.
           MOVE W-R1 TO W-R.
           MOVE 0 TO W-EC.
           PERFORM S-20 THRU S-30.
           IF  W-EC NOT = 0
               GO TO M-95
           END-IF
           MOVE SPACE TO W-P1.
           MOVE ALL "-" TO W-P1.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD3 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE ZERO TO W-KSU W-TKIN.
       M-30.
           MOVE ZERO TO W-EC.
           IF  FS-KIN1 = ZERO
               GO TO M-35
           END-IF
           MOVE FS-D1 TO W-D.
           PERFORM S-05 THRU S-15.
           IF  W-EC NOT = 0
               GO TO M-95
           END-IF.
       M-35.
           IF  FS-KIN2 = ZERO
               GO TO M-40
           END-IF
           MOVE FS-D2 TO W-D.
           PERFORM S-05 THRU S-15.
           IF  W-EC NOT = 0
               GO TO M-95
           END-IF.
       M-40.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-45
           END-IF
           IF  ZERO = FS-KIN1 AND FS-KIN2
               GO TO M-40
           END-IF
           IF (W-BKCD NOT = FS-BKC) OR (W-GP NOT = FS-FGP)
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-30.
       M-45.
           MOVE SPACE TO W-P2.
           MOVE "***  TOTAL  *** " TO P2-TM.
           MOVE W-KSU TO P2-KSU.
           MOVE   "件" TO P2-KM.
           MOVE W-TKIN TO P2-KIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               ADD 1 TO W-PAGE
               MOVE W-PAGE TO H-PAGE
               MOVE SPACE TO SP-R
               MOVE HEAD1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD3 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE W-KSU TO W3-KSU.
           MOVE W-TKIN TO W3-TKIN.
      *
           MOVE 0 TO W-EC.
           MOVE SPACE TO W-R.
           MOVE W-R3 TO W-R.
           PERFORM S-20 THRU S-30.
           IF  W-EC NOT = 0
               GO TO M-95
           END-IF
           MOVE 0 TO W-EC.
           MOVE SPACE TO W-R.
           MOVE W-R4 TO W-R.
           PERFORM S-20 THRU S-30.
           IF  W-EC NOT = 0
               GO TO M-95
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGOF_IDLST SSOUGOF_PNAME1.
           CALL "PR_Close" RETURNING RESP.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE W-KIN TO W-KIND.
           IF  W-TRC = 0
               GO TO S-10
           END-IF
           IF  W-BKCD = 1
               IF  W-BKC = "0168101"
                   GO TO S-10
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BKC = "2004331"
                   GO TO S-10
               END-IF
           END-IF
           IF  W-BKCD = 1
               IF  W-BK = 0168
                   IF  W-KIN < 30108
                       SUBTRACT 108 FROM W-KIND
                   ELSE
                       SUBTRACT 324 FROM W-KIND
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 1
               IF  W-BK NOT = 0168
                   IF  W-KIN < 30432
                       SUBTRACT 432 FROM W-KIND
                   ELSE
                       SUBTRACT 648 FROM W-KIND
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BK = 2004
                   IF  W-KIN < 30108
                       SUBTRACT 108 FROM W-KIND
                   ELSE
                       SUBTRACT 324 FROM W-KIND
                   END-IF
               END-IF
           END-IF
           IF  W-BKCD = 2
               IF  W-BK NOT = 2004
                   IF  W-KIN < 30432
                       SUBTRACT 432 FROM W-KIND
                   ELSE
                       SUBTRACT 648 FROM W-KIND
                   END-IF
               END-IF
           END-IF.
       S-10.
           MOVE W-BKC TO FBK-KEY.
      *           READ FBKM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-BKC" E-BKC "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO S-15
           END-IF
           INITIALIZE W-R2.
           MOVE 2 TO W2-KBN.
           MOVE FBK-BKC TO W2-BKC.
           MOVE FBK-BKN TO W2-BKN.
           MOVE FBK-HSC TO W2-HSC.
           MOVE FBK-HSN TO W2-HSN.
           MOVE W-YKS TO W2-YKC.
           MOVE W-KNO TO W2-KNO.
           MOVE W-FKN TO W2-UNA.
           MOVE W-KIND TO W2-KIN.
           MOVE FS-KEY TO W2-KEY.
      *
           MOVE SPACE TO W-P2.
           MOVE W2-BKC TO P2-BKC.
           MOVE W2-BKN TO P2-BKN.
           MOVE W2-HSC TO P2-HSC.
           MOVE W2-HSN TO P2-HSN.
           MOVE W2-KBG TO P2-KBG.
           MOVE W2-YKC TO P2-YKC.
           MOVE W2-KNO TO P2-KNO.
           MOVE W2-UNA TO P2-UNA.
           MOVE W2-KIN TO P2-KIN.
           MOVE W2-SCD TO P2-SCD.
           MOVE W2-KEY TO P2-KEY.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               ADD 1 TO W-PAGE
               MOVE W-PAGE TO H-PAGE
               MOVE SPACE TO SP-R
               MOVE HEAD1 TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               CALL "PR_NewPage" RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD3 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           ADD 1 TO W-KSU.
           ADD W2-KIN TO W-TKIN.
           MOVE SPACE TO W-R.
           MOVE W-R2 TO W-R.
           PERFORM S-20 THRU S-30.
           IF  W-EC = 0
               PERFORM S-35 THRU S-55
           END-IF.
       S-15.
           EXIT.
       S-20.
           MOVE SPACE TO SSOUGO-R.
           MOVE W-R TO SSOUGO-R.
      *           WRITE SSOUGO-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            SSOUGOF_PNAME1 SSOUGOF_LNAME SSOUGO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-25
           END-IF
           MOVE 0 TO W-EC.
           GO TO S-30.
       S-25.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-EC
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO S-30
           END-IF
           MOVE 1 TO W-EC.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE SSOUGOF_IDLST SSOUGOF_PNAME1.
           MOVE "SSOUGOF      " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "EXTEND" SSOUGOF_PNAME1 " " BY REFERENCE SSOUGOF_IDLST "0".
           GO TO S-20.
       S-30.
           EXIT.
       S-35.
           IF  FBK-ENG = ZERO
               GO TO S-40
           END-IF
           MOVE ZERO TO W-ENGD.
           MOVE FBK-ENG TO W-ENGS.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  W-ENGD >= W-NGD
               GO TO S-45
           END-IF.
       S-40.
           MOVE W-NG TO FBK-ENG.
      *           REWRITE FBK-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            FBKM_PNAME1 FBKM_LNAME FBK-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       S-45.
           MOVE FS-KEY TO S-KEY.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO S-55
           END-IF
           IF  S-ENG = ZERO
               GO TO S-50
           END-IF
           MOVE ZERO TO W-ENGD.
           MOVE S-ENG TO W-ENGS.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           IF  W-ENGD >= W-NGD
               GO TO S-55
           END-IF.
       S-50.
           MOVE W-NG TO S-ENG.
           MOVE ZERO TO S-TNG.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7  "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       S-55.
           EXIT.
