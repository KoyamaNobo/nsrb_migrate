       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBM010.
      ************************************************
      *****     振込先マスター　メンテナンス     *****
      *****         ( SCREEN : SCFB01 )          *****
      ************************************************
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
           02  F            PIC  X(034) VALUE SPACE.
           02  F            PIC  N(024) VALUE
                  "＊＊＊　　Ｆ・Ｂ　振込先マスター　プルーフリスト".
           02  F            PIC  N(005) VALUE   "　　＊＊＊".
           02  F            PIC  X(022) VALUE SPACE.
           02  F            PIC  X(005) VALUE "DATE ".
           02  H-DATE       PIC 99B99B99.
           02  F            PIC  X(007) VALUE "     P.".
           02  H-PAGE       PIC ZZ.
       01  HEAD2.
           02  W-15K        PIC  X(005) VALUE X"1A24212078".
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(010) VALUE   "振　　込　　先　　名".
           02  F            PIC  X(020) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "区分".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(012) VALUE
                  "口　座　振　込　先　名　".
           02  F            PIC  X(013) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "銀行".
           02  F            PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F            PIC  N(006) VALUE   "銀　行　名　".
           02  F            PIC  X(007) VALUE SPACE.
           02  F            PIC  N(008) VALUE   "本　支　店　名　".
           02  F            PIC  X(002) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "種別".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "口座番号".
           02  F            PIC  X(001) VALUE SPACE.
           02  F            PIC  N(002) VALUE   "料金".
           02  F            PIC  X(002) VALUE SPACE.
           02  F            PIC  N(004) VALUE   "最終日付".
           02  F            PIC  X(001) VALUE SPACE.
       01  W-P.
           02  P-KEY        PIC  9(004).
           02  F            PIC  X(001).
           02  P-NAME       PIC  N(024).
           02  F            PIC  X(001).
           02  P-FKC        PIC  9(001).
           02  F            PIC  X(001).
           02  P-FKN        PIC  X(030).
           02  F            PIC  X(001).
           02  P-BKC        PIC  9(007).
           02  F            PIC  X(001).
           02  P-BKN        PIC  X(015).
           02  F            PIC  X(001).
           02  P-HSN        PIC  X(015).
           02  F            PIC  X(001).
           02  P-YKS        PIC  9(001).
           02  F            PIC  X(001).
           02  P-KNO        PIC  9(007).
           02  F            PIC  X(002).
           02  P-TRC        PIC  9(001).
           02  F            PIC  X(001).
           02  P-ENGP       PIC 99/99/99.
       01  W-R.
           02  W-KEY        PIC  9(004).
           02  W-FKC        PIC  9(001).
           02  W-RD1.
             03  W-FKN1     PIC  X(030).
             03  W-BKC1     PIC  9(007).
             03  W-YKS1     PIC  9(001).
             03  W-KNO1     PIC  9(007).
             03  W-TRC1     PIC  9(001).
             03  W-KIN1     PIC  9(009).
           02  W-RD2.
             03  W-FKN2     PIC  X(030).
             03  W-BKC2     PIC  9(007).
             03  W-YKS2     PIC  9(001).
             03  W-KNO2     PIC  9(007).
             03  W-TRC2     PIC  9(001).
             03  W-KIN2     PIC  9(009).
           02  F            PIC  X(013).
       01  W-D.
           02  W-FKM        PIC  N(003).
           02  W-PAGE       PIC  9(002).
           02  W-KEYD       PIC  9(004).
           02  W-ACT        PIC  9(001).
           02  W-DMM        PIC  9(001).
           02  CHK          PIC  9(001).
           02  W-C          PIC  9(002).
           02  W-SEKEY.
             03  W-SKEY     PIC  9(004).
             03  W-EKEY     PIC  9(004) VALUE 9999.
           COPY LSTAT.
      *
           COPY LISM.
           COPY LIFBKM.
           COPY LSPF.
      *FD  FKSM
       01  FKSM_FBM010.
           02  FKSM_PNAME1    PIC  X(004) VALUE "FKSM".
           02  F              PIC  X(001).
           02  FKSM_LNAME     PIC  X(011) VALUE "FKSM_FBM010".
           02  F              PIC  X(001).
           02  FKSM_KEY1      PIC  X(100) VALUE SPACE.
           02  FKSM_SORT      PIC  X(100) VALUE SPACE.
           02  FKSM_IDLST     PIC  X(100) VALUE SPACE.
           02  FKSM_RES       USAGE  POINTER.
       01  FKS-R.
           02  FS-KEY       PIC  X(004).
           02  FS-FKC       PIC  9(001).
           02  FS-FKN1      PIC  X(030).
           02  FS-BKC1      PIC  9(007).
           02  FS-YKS1      PIC  9(001).
           02  FS-KNO1      PIC  9(007).
           02  FS-TRC1      PIC  9(001).
           02  FS-KIN1      PIC  9(009).
           02  FS-FKN2      PIC  X(030).
           02  FS-BKC2      PIC  9(007).
           02  FS-YKS2      PIC  9(001).
           02  FS-KNO2      PIC  9(007).
           02  FS-TRC2      PIC  9(001).
           02  FS-KIN2      PIC  9(009).
           02  F            PIC  X(007).
           02  FS-ENGP      PIC  9(006).
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-KEY   PIC  9(004).
             03  A-FKC   PIC  9(001).
           02  A-FKN1  PIC  X(030).
           02  A-BKC1  PIC  9(007).
           02  A-YKS1  PIC  9(001).
           02  A-KNO1  PIC  9(007).
           02  A-TRC1  PIC  9(001).
           02  A-FKN2  PIC  X(030).
           02  A-BKC2  PIC  9(007).
           02  A-YKS2  PIC  9(001).
           02  A-KNO2  PIC  9(007).
           02  A-TRC2  PIC  9(001).
           02  FILLER.
             03  A-SKEY  PIC  9(004).
             03  A-EKEY  PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-NAME  PIC  N(024).
             03  D-FKM   PIC  N(003).
           02  D-BK1.
             03  FILLER  PIC  X(015).
             03  FILLER  PIC  X(015).
           02  D-BK2.
             03  FILLER  PIC  X(015).
             03  FILLER  PIC  X(015).
           02  D-PM    PIC  X(045) VALUE
                "＜  コード 0000 より 9999 まで ＰＲＩＮＴ  ＞".
       01  C-SPC.
           02  S-D1.
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(007) VALUE "       ".
             03  FILLER  PIC  X(001) VALUE " ".
           02  S-D2.
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(007) VALUE "       ".
             03  FILLER  PIC  X(001) VALUE " ".
           02  S-PM    PIC  X(045) VALUE
                "                                             ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(022) VALUE
                  "***  ｼｲﾚｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME2   PIC  X(023) VALUE
                  "***  ﾌﾘｺﾐｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ﾄｳﾛｸｽﾞﾐ  ***".
             03  E-ME4   PIC  X(022) VALUE
                  "***  ｷﾞﾝｺｳﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  FKSM WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  FKSM REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(025) VALUE
                  "***  FKSM DELETE ｴﾗｰ  ***".
             03  E-ME8   PIC  X(017) VALUE
                  "***  ｺﾓｼﾞ ｱﾘ  ***".
             03  E-ME9   PIC  X(024) VALUE
                  "***  SM REWRITE ｴﾗｰ  ***".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "107" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "54" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "6" "0" "5" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "6" "8" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FKC" "9" "6" "63" "1" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FKC" BY REFERENCE W-FKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FKN1" "X" "8" "24" "30" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FKN1" BY REFERENCE W-FKN1 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BKC1" "9" "9" "24" "7" "A-FKN1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BKC1" BY REFERENCE W-BKC1 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YKS1" "9" "10" "24" "1" "A-BKC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YKS1" BY REFERENCE W-YKS1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KNO1" "9" "11" "24" "7" "A-YKS1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KNO1" BY REFERENCE W-KNO1 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TRC1" "9" "12" "24" "1" "A-KNO1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TRC1" BY REFERENCE W-TRC1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FKN2" "X" "14" "24" "30" "A-TRC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FKN2" BY REFERENCE W-FKN2 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BKC2" "9" "15" "24" "7" "A-FKN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BKC2" BY REFERENCE W-BKC2 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YKS2" "9" "16" "24" "1" "A-BKC2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YKS2" BY REFERENCE W-YKS2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KNO2" "9" "17" "24" "7" "A-YKS2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KNO2" BY REFERENCE W-KNO2 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TRC2" "9" "18" "24" "1" "A-KNO2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TRC2" BY REFERENCE W-TRC2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-ACP" " " "20" "0" "8" "A-TRC2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "20" "27" "4" " " "13C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "20" "37" "4" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "47" "1" "13C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "6" "0" "54" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "6" "13" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FKM" "N" "6" "65" "6" "D-NAME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-FKM" BY REFERENCE W-FKM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BK1" " " "9" "0" "30" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BK1" "X" "9" "33" "15" " " "D-BK1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BK1" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-BK1" "X" "9" "49" "15" "01D-BK1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-BK1" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BK2" " " "15" "0" "30" "D-BK1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BK2" "X" "15" "33" "15" " " "D-BK2" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BK2" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-BK2" "X" "15" "49" "15" "01D-BK2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-BK2" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "20" "16" "45" "D-BK2" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "143" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-D1" " " "0" "0" "49" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-D1" "X" "9" "24" "40" " " "S-D1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-D1" "X" "10" "24" "1" "01S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03S-D1" "X" "11" "24" "7" "02S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04S-D1" "X" "12" "24" "1" "03S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-D2" " " "0" "0" "49" "S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-D2" "X" "15" "24" "40" " " "S-D2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-D2" "X" "16" "24" "1" "01S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03S-D2" "X" "17" "24" "7" "02S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04S-D2" "X" "18" "24" "1" "03S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-PM" "X" "20" "16" "45" "S-D2" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "262" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "262" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "22" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "25" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "17" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "24" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME9" " " RETURNING RESU.
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
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
           CALL "SD_Screen_Output" USING "SCFB01" RETURNING RESU.
           MOVE ZERO TO W-PAGE.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU
               GO TO M-700
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Screen_Output" USING "SCFB01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE SPACE TO W-FKN1 W-FKN2.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           MOVE W-KEY TO S-KEY.
           IF  W-ACT NOT = 1
               GO TO M-080
           END-IF
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           GO TO M-100.
       M-080.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE   "　　＜　マスター　なし　＞" TO S-NAME
           END-IF.
       M-100.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE W-KEY TO FS-KEY.
      *           READ FKSM INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-120
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           PERFORM S-30 THRU S-40.
           IF  W-ACT = 3
               GO TO M-380
           END-IF
           GO TO M-140.
       M-120.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-FKC "A-FKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF
           IF  W-FKC NOT = 1 AND 2 AND 3
               GO TO M-140
           END-IF
           PERFORM S-20 THRU S-25.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-FKN1 "A-FKN1" "X" "30"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           IF  W-FKN1 = SPACE
               INITIALIZE W-RD1
               CALL "SD_Output" USING "S-D1" S-D1 "p" RETURNING RESU
               GO TO M-260
           END-IF
           MOVE ZERO TO W-C.
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｧ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｩ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｪ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｫ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｬ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｭ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｮ".
           INSPECT W-FKN1 TALLYING W-C FOR ALL "ｯ".
           IF  W-C NOT = ZERO
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-160
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-BKC1 "A-BKC1" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           MOVE W-BKC1 TO FBK-KEY.
      *           READ FBKM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-180
           END-IF
           CALL "SD_Output" USING "D-BK1" D-BK1 "p" RETURNING RESU.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-YKS1 "A-YKS1" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           IF  W-YKS1 NOT = 1 AND 2
               GO TO M-200
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-KNO1 "A-KNO1" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-KNO1 = ZERO
               GO TO M-220
           END-IF.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-TRC1 "A-TRC1" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           IF  W-TRC1 > 1
               GO TO M-240
           END-IF.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-FKN2 "A-FKN2" "X" "30"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-280
           END-IF
           IF  W-FKN1 = SPACE
               GO TO M-160
           END-IF
           GO TO M-240.
       M-280.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           IF  W-FKN2 = SPACE
               INITIALIZE W-RD2
               CALL "SD_Output" USING "S-D2" S-D2 "p" RETURNING RESU
               GO TO M-380
           END-IF
           MOVE ZERO TO W-C.
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｧ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｩ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｪ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｫ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｬ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｭ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｮ".
           INSPECT W-FKN2 TALLYING W-C FOR ALL "ｯ".
           IF  W-C NOT = ZERO
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-260
           END-IF.
       M-300.
           CALL "SD_Accept" USING BY REFERENCE A-BKC2 "A-BKC2" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-300
           END-IF
           MOVE W-BKC2 TO FBK-KEY.
      *           READ FBKM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "D-BK2" D-BK2 "p" RETURNING RESU.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-YKS2 "A-YKS2" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-YKS2 NOT = 1 AND 2
               GO TO M-320
           END-IF.
       M-340.
           CALL "SD_Accept" USING BY REFERENCE A-KNO2 "A-KNO2" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-340
           END-IF
           IF  W-KNO2 = ZERO
               GO TO M-340
           END-IF.
       M-360.
           CALL "SD_Accept" USING BY REFERENCE A-TRC2 "A-TRC2" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-340
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-360
           END-IF
           IF  W-TRC2 > 1
               GO TO M-360
           END-IF.
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-400
           END-IF
           IF  W-ACT = 3
               GO TO M-060
           END-IF
           IF  W-FKN2 = SPACE
               GO TO M-260
           END-IF
           GO TO M-360.
       M-400.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-380
           END-IF
           IF  W-DMM = 9
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-380
           END-IF
           IF  W-ACT = 2
               GO TO M-420
           END-IF
           IF  W-ACT = 3
               GO TO M-440
           END-IF
           MOVE W-R TO FKS-R.
      *           WRITE FKS-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            FKSM_PNAME1 FKSM_LNAME FKS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE 1 TO S-SFC.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-060.
       M-420.
           MOVE ZERO TO FKS-R.
           MOVE W-R TO FKS-R.
      *           REWRITE FKS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            FKSM_PNAME1 FKSM_LNAME FKS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE 1 TO S-SFC.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-060.
       M-440.
      *           DELETE FKSM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING FKSM_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE 0 TO S-SFC.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO M-060.
       M-700.
           PERFORM S-45 THRU S-80.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
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
           IF  W-FKC = 1
               MOVE   "外　注" TO W-FKM
           END-IF
           IF  W-FKC = 2
               MOVE   "買　掛" TO W-FKM
           END-IF
           IF  W-FKC = 3
               MOVE   "その他" TO W-FKM
           END-IF
           CALL "SD_Output" USING "D-FKM" D-FKM "p" RETURNING RESU.
       S-25.
           EXIT.
       S-30.
           MOVE FKS-R TO W-R.
           CALL "SD_Output" USING "A-FKC" A-FKC "p" RETURNING RESU.
           PERFORM S-20 THRU S-25.
           IF  W-FKN1 = SPACE
               CALL "SD_Output" USING "S-D1" S-D1 "p" RETURNING RESU
               GO TO S-35
           END-IF
           CALL "SD_Output" USING "A-FKN1" A-FKN1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BKC1" A-BKC1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YKS1" A-YKS1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KNO1" A-KNO1 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TRC1" A-TRC1 "p" RETURNING RESU.
           MOVE W-BKC1 TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " ** ﾏｽﾀｰ ﾅｼ ** " TO FBK-BKN
               MOVE SPACE TO FBK-HSN
           END-IF
           CALL "SD_Output" USING "D-BK1" D-BK1 "p" RETURNING RESU.
       S-35.
           IF  W-FKN2 = SPACE
               CALL "SD_Output" USING "S-D2" S-D2 "p" RETURNING RESU
               GO TO S-40
           END-IF
           CALL "SD_Output" USING "A-FKN2" A-FKN2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BKC2" A-BKC2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-YKS2" A-YKS2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KNO2" A-KNO2 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TRC2" A-TRC2 "p" RETURNING RESU.
           MOVE W-BKC2 TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " ** ﾏｽﾀｰ ﾅｼ ** " TO FBK-BKN
               MOVE SPACE TO FBK-HSN
           END-IF
           CALL "SD_Output" USING "D-BK2" D-BK2 "p" RETURNING RESU.
       S-40.
           EXIT.
       S-45.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               CALL "SD_Output" USING "S-PM" S-PM "p" RETURNING RESU
               GO TO S-80
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-45
           END-IF.
       S-50.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-45
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-50
           END-IF
           IF  W-EKEY < W-SKEY
               GO TO S-50
           END-IF.
       S-55.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO S-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO S-55
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING "S-PM" S-PM "p" RETURNING RESU
               GO TO S-80
           END-IF
           IF  W-DMM NOT = 1
               GO TO S-55
           END-IF
           MOVE W-SKEY TO FS-KEY.
      *           START FKSM KEY NOT < FS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            FKSM_PNAME1 "FS-KEY" " NOT < " FS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO S-45
           END-IF
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO S-45
           END-IF
           IF  FS-KEY > W-EKEY
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO S-45
           END-IF
           CALL "PR_Open" RETURNING RESP.
           ACCEPT H-DATE FROM DATE.
           PERFORM S-10 THRU S-15.
       S-60.
           MOVE FS-KEY TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE   "　　＜　マスター　なし　＞" TO S-NAME
           END-IF
           MOVE SPACE TO W-P.
           MOVE FS-KEY TO P-KEY.
           MOVE S-NAME TO P-NAME.
           MOVE FS-FKC TO P-FKC.
           IF  FS-FKN1 = SPACE
               GO TO S-65
           END-IF
           MOVE FS-BKC1 TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " ** ﾏｽﾀｰ ﾅｼ ** " TO FBK-BKN
               MOVE SPACE TO FBK-HSN
           END-IF
           MOVE FS-FKN1 TO P-FKN.
           MOVE FS-BKC1 TO P-BKC.
           MOVE FBK-BKN TO P-BKN.
           MOVE FBK-HSN TO P-HSN.
           MOVE FS-YKS1 TO P-YKS.
           MOVE FS-KNO1 TO P-KNO.
           MOVE FS-TRC1 TO P-TRC.
           IF  FS-FKN2 = SPACE
               IF  FS-ENGP NOT = ZERO
                   MOVE FS-ENGP TO P-ENGP
               END-IF
           END-IF.
       S-65.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 62
               PERFORM S-05 THRU S-15
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-70.
           IF  FS-FKN2 = SPACE
               GO TO S-75
           END-IF
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME.
           MOVE FS-BKC2 TO FBK-KEY.
      *           READ FBKM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FBKM_PNAME1 BY REFERENCE FBK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " ** ﾏｽﾀｰ ﾅｼ ** " TO FBK-BKN
               MOVE SPACE TO FBK-HSN
           END-IF
           MOVE FS-FKN2 TO P-FKN.
           MOVE FS-BKC2 TO P-BKC.
           MOVE FBK-BKN TO P-BKN.
           MOVE FBK-HSN TO P-HSN.
           MOVE FS-YKS2 TO P-YKS.
           MOVE FS-KNO2 TO P-KNO.
           MOVE FS-TRC2 TO P-TRC.
           IF  FS-ENGP NOT = ZERO
               MOVE FS-ENGP TO P-ENGP
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       S-75.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "PR_Close" RETURNING RESP
               GO TO S-80
           END-IF
           IF  FS-KEY > W-EKEY
               CALL "PR_Close" RETURNING RESP
               GO TO S-80
           END-IF
           GO TO S-60.
       S-80.
           EXIT.
