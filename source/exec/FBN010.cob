       IDENTIFICATION DIVISION.
       PROGRAM-ID. FBN010.
      *********************************************
      *****    Ｆ・Ｂ　総合振込金額　入力     *****
      *********************************************
       AUTHOR. S-NAKAO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-CC           PIC  9(001).
           02  W-FKM          PIC  N(003).
           02  W-KEY          PIC  9(004).
           02  W-KEYD         PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-BKC          PIC  9(001).
           02  W-FGP          PIC  9(004).
           02  W-FGPD  REDEFINES W-FGP.
             03  W-FGET       PIC  9(002).
             03  W-FPEY       PIC  9(002).
           02  W-ENGP.
             03  W-ENEN       PIC  9(004).
             03  W-ENENL REDEFINES W-ENEN.
               04  W-ENEN1    PIC  9(002).
               04  W-ENEN2    PIC  9(002).
             03  F            PIC  9(004).
           02  W-ENGPL REDEFINES W-ENGP.
             03  F            PIC  9(002).
             03  W-ENGPS      PIC  9(006).
           02  W-BKN.
             03  F            PIC  N(001) VALUE   "【".
             03  W-BKND       PIC  N(004).
             03  F            PIC  N(001) VALUE   "】".
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GP         PIC  9(004).
           02  W-GNGP         PIC  9(008).
           02  W-GNGPD REDEFINES W-GNGP.
             03  W-GNEN       PIC  9(004).
             03  W-GGET       PIC  9(002).
             03  W-GPEY       PIC  9(002).
           02  W-DC           PIC  9(001).
           02  W-EC           PIC  9(001) VALUE 0.
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LIFBKM.
           COPY LICAL.
      *FD  FKSM
       01  FKSM_FBN010.
           02  FKSM_PNAME1    PIC  X(004) VALUE "FKSM".
           02  F              PIC  X(001).
           02  FKSM_LNAME     PIC  X(011) VALUE "FKSM_FBN010".
           02  F              PIC  X(001).
           02  FKSM_KEY1      PIC  X(100) VALUE SPACE.
           02  FKSM_SORT      PIC  X(100) VALUE SPACE.
           02  FKSM_IDLST     PIC  X(100) VALUE SPACE.
           02  FKSM_RES       USAGE  POINTER.
       01  FKS-R.
           02  FS-KEY         PIC  X(004).
           02  FS-FKC         PIC  9(001).
           02  FS-FKN1        PIC  X(030).
           02  FS-BKC1        PIC  9(007).
           02  FS-YKS1        PIC  9(001).
           02  FS-KNO1        PIC  9(007).
           02  FS-TRC1        PIC  9(001).
           02  FS-KIN1        PIC  9(009).
           02  FS-FKN2        PIC  X(030).
           02  FS-BKC2        PIC  9(007).
           02  FS-YKS2        PIC  9(001).
           02  FS-KNO2        PIC  9(007).
           02  FS-TRC2        PIC  9(001).
           02  FS-KIN2        PIC  9(009).
           02  FS-BKC         PIC  9(001).
           02  FS-FGP         PIC  9(004).
           02  F              PIC  X(002).
           02  FS-ENGP        PIC  9(006).
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
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　Ｆ・Ｂ　総合振込金額　入力　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(042) VALUE
                "前回のデータを消す   OK = 1  NO = 5   ﾘﾀｰﾝ".
           02  FILLER  PIC  X(038) VALUE
                "銀行区分       (1=中国銀行,2=商工中金)".
           02  FILLER  PIC  X(019) VALUE
                "振込日      月   日".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-CC    PIC  9(001).
           02  A-BKC   PIC  9(001).
           02  FILLER.
             03  A-FGET  PIC  9(002).
             03  A-FPEY  PIC  9(002).
           02  A-KEY   PIC  9(004).
           02  A-KIN1  PIC  9(009).
           02  A-KIN2  PIC  9(009).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-BKN   PIC  N(006).
             03  D-FGP.
               04  FILLER  PIC  N(003) VALUE   "振込日".
               04  FILLER  PIC  Z(002).
               04  FILLER  PIC  N(001) VALUE   "月".
               04  FILLER  PIC  Z(002).
               04  FILLER  PIC  N(001) VALUE   "日".
           02  D-MD.
             03  FILLER  PIC  N(024).
             03  FILLER  PIC  9(001).
             03  FILLER  PIC  N(003).
           02  D-D1.
             03  FILLER  PIC  X(030).
             03  FILLER.
               04  FILLER  PIC  9(007).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  X(015).
             03  FILLER  PIC  9(001).
             03  FILLER  PIC  9(007).
             03  FILLER  PIC  9(001).
             03  D-KIN1  PIC ZZZZZZZZ9 .
           02  D-D2.
             03  FILLER  PIC  X(030).
             03  FILLER.
               04  FILLER  PIC  9(007).
               04  FILLER  PIC  X(015).
               04  FILLER  PIC  X(015).
             03  FILLER  PIC  9(001).
             03  FILLER  PIC  9(007).
             03  FILLER  PIC  9(001).
             03  D-KIN2  PIC ZZZZZZZZ9 .
       01  C-SPC.
           02  S-D1.
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(007) VALUE "       ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(009) VALUE "         ".
           02  S-D2.
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(040) VALUE
                  "                                        ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(007) VALUE "       ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(009) VALUE "         ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(022) VALUE
                  "***  ｼｲﾚｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME2   PIC  X(023) VALUE
                  "***  ﾌﾘｺﾐｻｷﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾋﾂﾞｹ ｴﾗｰ  ***".
             03  E-ME4   PIC  X(019) VALUE
                  "***  ｶﾚﾝﾀﾞｰ ﾅｼ  ***".
             03  E-ME5   PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-STAT  PIC  X(002).
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
            "C-MID" " " "0" "0" "443" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "12" "42" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "17" "12" "38" "08C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-MID" "X" "19" "12" "19" "09C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-MID" "X" "23" "28" "22" "10C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "29" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CC" "9" "14" "49" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CC" BY REFERENCE W-CC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BKC" "9" "17" "23" "1" "A-CC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BKC" BY REFERENCE W-BKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "19" "0" "4" "A-BKC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FGET" "9" "19" "22" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FPEY" "9" "19" "27" "2" "A-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "6" "8" "4" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN1" "9" "13" "24" "9" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN1" BY REFERENCE FS-KIN1 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN2" "9" "20" "24" "9" "A-KIN1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN2" BY REFERENCE FS-KIN2 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "45" "1" "A-KIN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "251" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "3" "0" "26" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BKN" "N" "3" "7" "12" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-BKN" BY REFERENCE W-BKN "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FGP" " " "3" "0" "14" "D-BKN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-FGP" "N" "3" "55" "6" " " "D-FGP" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-FGP" "Z" "3" "63" "2" "01D-FGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-FGP" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-FGP" "N" "3" "65" "2" "02D-FGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-FGP" "Z" "3" "68" "2" "03D-FGP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-FGP" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-FGP" "N" "3" "70" "2" "04D-FGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD" " " "6" "0" "55" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MD" "N" "6" "13" "48" " " "D-MD" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-MD" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MD" "9" "6" "63" "1" "01D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-MD" BY REFERENCE FS-FKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MD" "N" "6" "65" "6" "02D-MD" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-MD" BY REFERENCE W-FKM "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D1" " " "0" "0" "85" "D-MD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-D1" "X" "8" "24" "30" " " "D-D1" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-D1" BY REFERENCE FS-FKN1 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-D1" " " "9" "0" "37" "01D-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-D1" "9" "9" "24" "7" " " "02D-D1" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-D1" BY REFERENCE FS-BKC1 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-D1" "X" "9" "33" "15" "0102D-D1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-D1" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-D1" "X" "9" "49" "15" "0202D-D1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-D1" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-D1" "9" "10" "24" "1" "02D-D1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-D1" BY REFERENCE FS-YKS1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-D1" "9" "11" "24" "7" "03D-D1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-D1" BY REFERENCE FS-KNO1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-D1" "9" "12" "24" "1" "04D-D1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-D1" BY REFERENCE FS-TRC1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN1" "ZZZZZZZZ9" "13" "24" "9" "05D-D1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN1" BY REFERENCE FS-KIN1 "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-D2" " " "0" "0" "85" "D-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-D2" "X" "15" "24" "30" " " "D-D2" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-D2" BY REFERENCE FS-FKN2 "30" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-D2" " " "16" "0" "37" "01D-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-D2" "9" "16" "24" "7" " " "02D-D2" RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-D2" BY REFERENCE FS-BKC2 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-D2" "X" "16" "33" "15" "0102D-D2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0202D-D2" BY REFERENCE FBK-BKN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-D2" "X" "16" "49" "15" "0202D-D2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0302D-D2" BY REFERENCE FBK-HSN "15" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-D2" "9" "17" "24" "1" "02D-D2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "03D-D2" BY REFERENCE FS-YKS2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-D2" "9" "18" "24" "7" "03D-D2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-D2" BY REFERENCE FS-KNO2 "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-D2" "9" "19" "24" "1" "04D-D2" " " RETURNING RESU.
       CALL "SD_From" USING 
            "05D-D2" BY REFERENCE FS-TRC2 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN2" "ZZZZZZZZ9" "20" "24" "9" "05D-D2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN2" BY REFERENCE FS-KIN2 "9" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "196" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-D1" " " "0" "0" "98" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-D1" "X" "8" "24" "40" " " "S-D1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-D1" "X" "9" "24" "40" "01S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03S-D1" "X" "10" "24" "1" "02S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04S-D1" "X" "11" "24" "7" "03S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05S-D1" "X" "12" "24" "1" "04S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06S-D1" "X" "13" "24" "9" "05S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-D2" " " "0" "0" "98" "S-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-D2" "X" "15" "24" "40" " " "S-D2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-D2" "X" "16" "24" "40" "01S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03S-D2" "X" "17" "24" "1" "02S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04S-D2" "X" "18" "24" "7" "03S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05S-D2" "X" "19" "24" "1" "04S-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06S-D2" "X" "20" "24" "9" "05S-D2" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "165" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "165" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "23" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "19" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "21" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-STAT" " " RETURNING RESU.
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
           MOVE ZERO TO W-ENGP.
           ACCEPT W-ENGPS FROM DATE.
           IF  W-ENEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ENEN
           END-IF
           IF  W-ENEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ENEN
           END-IF
           MOVE W-ENGP TO W-GNGP.
           ADD 1 TO W-GGET.
           IF  W-GGET = 13
               ADD 1 TO W-GNEN
               MOVE 1 TO W-GGET
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-CC "A-CC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-CC NOT = 1 AND 5
               GO TO M-10
           END-IF
           MOVE 0 TO W-DC.
           IF  W-CC = 1
               GO TO M-15
           END-IF
           PERFORM KEN-RTN THRU KEN-EX.
           IF  W-BKC NOT = 0
               MOVE 9 TO W-DC
               GO TO M-30
           END-IF.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-BKC "A-BKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  W-BKC NOT = 1 AND 2
               GO TO M-15
           END-IF.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-FGET "A-FGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-15
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF
           IF  W-FGET < 1 OR > 12
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-FPEY "A-FPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-FGET < 1 OR > 31
               GO TO M-25
           END-IF.
       M-30.
           MOVE W-ENGP TO W-NGP.
           MOVE W-FGP TO W-GP.
           IF  W-NGP < W-ENGP
               ADD 1 TO W-NEN
           END-IF
           IF  W-GNGP < W-NGP
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               IF  W-DC = 0
                   GO TO M-20
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           MOVE W-NGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-10
           END-IF
           IF  CL-SJ = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               IF  W-DC = 0
                   GO TO M-20
               ELSE
                   GO TO M-10
               END-IF
           END-IF.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-DC = 0
                   GO TO M-25
               ELSE
                   GO TO M-10
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM = 9
               GO TO M-10
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF
      *
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           IF  W-BKC = 1
               MOVE   "中国銀行" TO W-BKND
           END-IF
           IF  W-BKC = 2
               MOVE   "商工中金" TO W-BKND
           END-IF
           IF  W-CC = 5
               GO TO M-40
           END-IF
           PERFORM CLR-RTN THRU CLR-EX.
           IF  W-EC = 9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-40.
           CALL "DB_F_Open" USING
            "I-O" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" FBKM_PNAME1 "SHARED" BY REFERENCE FBKM_IDLST "1"
            "FBK-KEY" BY REFERENCE FBK-KEY.
           MOVE ZERO TO W-KEYD CHK.
       M-45.
           CALL "SD_Screen_Output" USING "SCFB10" RETURNING RESU.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-FGP" D-FGP "p" RETURNING RESU.
       M-50.
           MOVE W-KEYD TO W-KEY.
           CALL "SD_Output" USING "A-KEY" A-KEY "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = ADV
               GO TO M-65
           END-IF.
       M-55.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           MOVE FS-KEY TO W-KEY W-KEYD.
           PERFORM DSP-RTN THRU DSP-EX.
           MOVE 5 TO CHK.
           GO TO M-50.
       M-60.
           IF  CHK = ZERO
               GO TO M-50
           END-IF
           MOVE ZERO TO CHK.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
           GO TO M-55.
       M-65.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-50
           END-IF
           MOVE W-KEY TO W-KEYD FS-KEY.
      *           READ FKSM INVALID KEY
      *//////////////////////
           CALL "DB_Read" USING
            "INVALID KEY" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-50
           END-IF
           PERFORM DSP-RTN THRU DSP-EX.
           IF  FS-FKN1 = SPACE
               GO TO M-75
           END-IF.
       M-70.
           CALL "SD_Accept" USING BY REFERENCE A-KIN1 "A-KIN1" "9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-50
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-70
           END-IF
           CALL "SD_Output" USING "D-KIN1" D-KIN1 "p" RETURNING RESU.
           IF  FS-FKN2 = SPACE
               GO TO M-80
           END-IF.
       M-75.
           CALL "SD_Accept" USING BY REFERENCE A-KIN2 "A-KIN2" "9" "9"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  FS-FKN1 = SPACE
                   GO TO M-50
               ELSE
                   GO TO M-70
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-75
           END-IF
           CALL "SD_Output" USING "D-KIN2" D-KIN2 "p" RETURNING RESU.
       M-80.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  SPACE = FS-FKN1 AND FS-FKN2
                   GO TO M-50
               ELSE
                   IF  FS-FKN2 = SPACE
                       GO TO M-70
                   ELSE
                       GO TO M-75
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-80
           END-IF
           IF  W-DMM = 9
               GO TO M-50
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-80
           END-IF
      *
           MOVE ZERO TO FS-BKC FS-FGP.
           IF (FS-KIN1 NOT = ZERO) OR (FS-KIN2 NOT = ZERO)
               MOVE W-BKC TO FS-BKC
               MOVE W-FGP TO FS-FGP
           END-IF
           MOVE W-ENGPS TO FS-ENGP.
      *           REWRITE FKS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            FKSM_PNAME1 FKSM_LNAME FKS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           GO TO M-45.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE FBKM_IDLST FBKM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       KEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
           MOVE ZERO TO W-BKC W-FGP.
       KEN-020.
      *           READ FKSM NEXT RECORD WITH UNLOCK AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KEN-040
           END-IF
           IF  FS-BKC = 0
               GO TO KEN-020
           END-IF
           MOVE FS-BKC TO W-BKC.
           MOVE FS-FGP TO W-FGP.
           CALL "SD_Output" USING "A-BKC" A-BKC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-FGET" A-FGET "p" RETURNING RESU.
           CALL "SD_Output" USING "A-FPEY" A-FPEY "p" RETURNING RESU.
       KEN-040.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
       KEN-EX.
           EXIT.
      *
       CLR-RTN.
           CALL "DB_F_Open" USING
            "I-O" FKSM_PNAME1 "SHARED" BY REFERENCE FKSM_IDLST "1"
            "FS-KEY" BY REFERENCE FS-KEY.
       CLR-020.
      *           READ FKSM NEXT RECORD AT END
      *//////////////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" FKSM_PNAME1 BY REFERENCE FKS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CLR-040
           END-IF
           MOVE ZERO TO FS-KIN1 FS-KIN2 FS-BKC FS-FGP.
      *           REWRITE FKS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            FKSM_PNAME1 FKSM_LNAME FKS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO CLR-040
           END-IF
           GO TO CLR-020.
       CLR-040.
           CALL "DB_F_Close" USING BY REFERENCE FKSM_IDLST FKSM_PNAME1.
       CLR-EX.
           EXIT.
      *
       DSP-RTN.
           MOVE W-KEY TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE   "　　＜　マスター　なし　＞" TO S-NAME
           END-IF
           IF  FS-FKC = 1
               MOVE   "外　注" TO W-FKM
           END-IF
           IF  FS-FKC = 2
               MOVE   "買　掛" TO W-FKM
           END-IF
           IF  FS-FKC = 3
               MOVE   "その他" TO W-FKM
           END-IF
           CALL "SD_Output" USING "D-MD" D-MD "p" RETURNING RESU.
           IF  FS-FKN1 = SPACE
               CALL "SD_Output" USING "S-D1" S-D1 "p" RETURNING RESU
               GO TO DSP-020
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
           CALL "SD_Output" USING "D-D1" D-D1 "p" RETURNING RESU.
       DSP-020.
           IF  FS-FKN2 = SPACE
               CALL "SD_Output" USING "S-D2" S-D2 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
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
           CALL "SD_Output" USING "D-D2" D-D2 "p" RETURNING RESU.
       DSP-EX.
           EXIT.
