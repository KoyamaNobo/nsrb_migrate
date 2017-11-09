       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD510.
      *********************************************************
      *    PROGRAM         :  工品他　日次　マスター更新　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  NO                              *
      *    DATA WRITTN     :  49/04/25                        *
      *        変更　　　  :  62/04/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ERR-STAT           PIC  X(002).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-KEY          PIC  X(005).
           02  W-DATE         PIC  9(006).
           02  W-NG           PIC  9(006).
           02  W-NGD   REDEFINES W-NG.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
           02  W-NGL   REDEFINES W-NG.
             03  F            PIC  9(002).
             03  W-NGS        PIC  9(004).
           02  W-PC           PIC  9(001) VALUE 0.
           02  W-ERR          PIC  9(001) VALUE 0.
           02  W-SU           PIC S9(007)V9(02).
           02  W-D.
             03  W-TCD        PIC  9(004).
             03  W-DNO        PIC  9(006).
             03  W-KIN        PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  WT-D.
             03  WT-TZZ       PIC S9(009).
             03  WT-TUA       PIC S9(009).
             03  WT-TNB       PIC S9(009).
             03  WT-TNK       PIC S9(009).
             03  WT-TUZ       PIC S9(009).
             03  WT-TUZW      PIC S9(009).
           02  W-FILE         PIC  X(004).
           02  W-ME           PIC  X(040).
           02  W-EME.
             03  W-ME1        PIC  X(016) VALUE
                  "***  TTM ﾅｼ  ***".
             03  W-ME2        PIC  X(025) VALUE
                  "***  TTM REWRITE ｴﾗｰ  ***".
             03  W-ME3        PIC  X(017) VALUE
                  "***  KHTM ﾅｼ  ***".
             03  W-ME4        PIC  X(026) VALUE
                  "***  KHTM REWRITE ｴﾗｰ  ***".
             03  W-ME7        PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  W-ME8        PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  W-ME9        PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  W-ME10       PIC  X(025) VALUE
                  "***  KHM REWRITE ｴﾗｰ  ***".
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKKBM.
           COPY LITTM.
           COPY LIKHTM.
      *FD  URI-F
       01  URI-F_KHD510.
           02  URI-F_PNAME1   PIC  X(004) VALUE "URIF".
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_KHD510".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R.
           02  U-DC           PIC  9(001).
           02  U-DATE         PIC  9(008).
           02  U-TCD          PIC  9(004).
           02  U-HCD          PIC  X(005).
           02  U-SU           PIC S9(006)V9(02).
           02  U-T            PIC S9(006)V9(02).
           02  U-KI           PIC S9(008).
           02  U-YC           PIC  9(002).
           02  F              PIC  X(011).
           02  U-CSC          PIC  9(001).
           02  F              PIC  X(023).
           02  U-GT           PIC  9(006)V9(02).
           02  F              PIC  X(040).
           02  U-PRC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  KNH-F
       01  KNH-F_KHD510.
           02  KNH-F_PNAME1   PIC  X(004) VALUE "KNHF".
           02  F              PIC  X(001).
           02  KNH-F_LNAME    PIC  X(012) VALUE "KNH-F_KHD510".
           02  F              PIC  X(001).
           02  KNH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KNH-F_RES      USAGE  POINTER.
       01  KNH-R.
           02  NH-NHC         PIC  9(002).
           02  NH-DATE        PIC  9(008).
           02  NH-HCD         PIC  X(005).
           02  NH-TS          PIC  9(002).
           02  NH-KS          PIC S9(005).
           02  NH-SU          PIC S9(006)V9(02).
           02  F              PIC  9(008).
           02  NH-KIN         PIC S9(008).
           02  NH-YC          PIC  9(002).
           02  F              PIC  X(005).
           02  NH-NC          PIC  9(001).
           02  F              PIC  X(009).
           02  NH-PRC         PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　工品他　日次更新　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(018) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MIDD.
           02  FILLER.
             03  C-MID1  PIC  N(018) VALUE
                  "＊＊＊　　工品他　売上更新　　＊＊＊".
             03  C-MID4  PIC  N(018) VALUE
                  "＊＊＊　　加硫・廃却　更新　　＊＊＊".
       01  C-ACP.
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-SME01 PIC  N(017) VALUE
                  "＊＊＊　　他で売上入力中　　＊＊＊".
             03  D-SME02 PIC  N(017) VALUE
                  "＊＊＊　　他で売上変換中　　＊＊＊".
             03  D-SME03 PIC  N(017) VALUE
                  "＊＊＊　　他で値引入力中　　＊＊＊".
             03  D-SME05 PIC  N(017) VALUE
                  "＊＊＊　　他で加硫入力中　　＊＊＊".
             03  D-SME06 PIC  N(017) VALUE
                  "＊＊＊　　他で廃却入力中　　＊＊＊".
             03  D-SME13 PIC  N(017) VALUE
                  "＊＊＊　　他で日次更新中　　＊＊＊".
             03  D-SME14 PIC  N(017) VALUE
                  "＊＊＊　　他で手配変換中　　＊＊＊".
             03  D-SME15 PIC  N(017) VALUE
                  "＊＊＊　　他で月次更新中　　＊＊＊".
             03  D-SME99 PIC  N(015) VALUE
                  "＊＊＊　　他で使用中　　＊＊＊".
           02  D-ZM.
             03  FILLER.
               04  FILLER   PIC  X(004) VALUE "ｺｰﾄﾞ".
               04  0201D-ZM PIC  9(004).
             03  FILLER.
               04  FILLER   PIC  N(006) VALUE "　　　前月残".
               04  FILLER   PIC  N(006) VALUE "　　　売　上".
               04  FILLER   PIC  N(006) VALUE "　　　値　引".
               04  FILLER   PIC  N(006) VALUE "　　　入　金".
               04  FILLER   PIC  N(006) VALUE "　　　売掛残".
             03  FILLER.
               04  FILLER   PIC  N(003) VALUE "本　体".
               04  0203D-ZM PIC ----,---,--9 .
               04  0303D-ZM PIC ----,---,--9 .
               04  0403D-ZM PIC ----,---,--9 .
               04  0503D-ZM PIC ----,---,--9 .
               04  0603D-ZM PIC ----,---,--9 .
             03  FILLER.
               04  FILLER   PIC  N(003) VALUE "消費税".
               04  0204D-ZM PIC ----,---,--9 .
               04  0304D-ZM PIC ----,---,--9 .
               04  0404D-ZM PIC ----,---,--9 .
               04  0504D-ZM PIC ----,---,--9 .
               04  0604D-ZM PIC ----,---,--9 .
             03  FILLER.
               04  FILLER   PIC  N(003) VALUE "合　計".
               04  0205D-ZM PIC ----,---,--9 .
               04  0305D-ZM PIC ----,---,--9 .
               04  0405D-ZM PIC ----,---,--9 .
               04  0505D-ZM PIC ----,---,--9 .
               04  0605D-ZM PIC ----,---,--9 .
           02  D-ZMC.
             03  FILLER.
               04  FILLER   PIC  X(004) VALUE "    ".
               04  FILLER   PIC  X(004) VALUE "    ".
             03  FILLER.
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
             03  FILLER.
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
               04  FILLER   PIC  X(012) VALUE "            ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME    PIC  X(040).
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
             03  E-TCD   PIC  X(004).
             03  E-KEY   PIC  X(005).
             03  E-HCD   PIC  X(005).
             03  E-ME61.
               04  FILLER    PIC  X(034) VALUE
                    "***  入力リスト未印字  (    )  ***".
               04  02E-ME61  PIC  X(004).
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL.
               04  FILLER    PIC  X(040) VALUE
                    "                                        ".
               04  FILLER    PIC  X(040) VALUE
                    "                                        ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "274" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "36" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "36" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "36" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "36" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "36" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "36" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "36" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "20" "17" "22" "07C-MID" " " RETURNING RESU.
      *C-MIDD
       CALL "SD_Init" USING 
            "C-MIDD" " " "0" "0" "72" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MIDD" " " "6" "0" "72" " " "C-MIDD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MID1" "N" "6" "10" "36" " " "01C-MIDD"  RETURNING RESU.
       CALL "SD_Init" USING 
            "C-MID4" "N" "6" "10" "36" "C-MID1" " "  RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "1" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "34" "1" " " "C-ACP"  RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "892" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "15" "0" "302" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME01" "bN" "15" "18" "34" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME02" "bN" "15" "18" "34" "D-SME01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME03" "bN" "15" "18" "34" "D-SME02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME05" "bN" "15" "18" "34" "D-SME03" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME06" "bN" "15" "18" "34" "D-SME05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME13" "bN" "15" "18" "34" "D-SME06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME14" "bN" "15" "18" "34" "D-SME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME15" "bN" "15" "18" "34" "D-SME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME99" "bN" "15" "18" "30" "D-SME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZM" " " "0" "0" "266" "01C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZM" " " "14" "0" "8" " " "D-ZM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZM" "X" "14" "8" "4" " " "01D-ZM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-ZM" "9" "14" "13" "4" "0101D-ZM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "0201D-ZM" BY REFERENCE TT-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZM" " " "15" "0" "60" "01D-ZM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ZM" "N" "15" "14" "12" " " "02D-ZM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-ZM" "N" "15" "26" "12" "0102D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-ZM" "N" "15" "38" "12" "0202D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-ZM" "N" "15" "50" "12" "0302D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502D-ZM" "N" "15" "62" "12" "0402D-ZM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ZM" " " "16" "0" "66" "02D-ZM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-ZM" "N" "16" "8" "6" " " "03D-ZM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-ZM" "----,---,--9" "16" "14" "12" "0103D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0203D-ZM" BY REFERENCE TT-TZZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0303D-ZM" "----,---,--9" "16" "26" "12" "0203D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0303D-ZM" BY REFERENCE TT-TUA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0403D-ZM" "----,---,--9" "16" "38" "12" "0303D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0403D-ZM" BY REFERENCE TT-TNB "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0503D-ZM" "----,---,--9" "16" "50" "12" "0403D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0503D-ZM" BY REFERENCE TT-TNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0603D-ZM" "----,---,--9" "16" "62" "12" "0503D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0603D-ZM" BY REFERENCE TT-TUZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ZM" " " "17" "0" "66" "03D-ZM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-ZM" "N" "17" "8" "6" " " "04D-ZM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-ZM" "----,---,--9" "17" "14" "12" "0104D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0204D-ZM" BY REFERENCE TT-TZZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0304D-ZM" "----,---,--9" "17" "26" "12" "0204D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0304D-ZM" BY REFERENCE TT-TUAZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0404D-ZM" "----,---,--9" "17" "38" "12" "0304D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0404D-ZM" BY REFERENCE TT-TNBZ "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0504D-ZM" "----,---,--9" "17" "50" "12" "0404D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0504D-ZM" BY REFERENCE TT-TNKZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0604D-ZM" "----,---,--9" "17" "62" "12" "0504D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0604D-ZM" BY REFERENCE TT-TUZZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-ZM" " " "18" "0" "66" "04D-ZM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-ZM" "N" "18" "8" "6" " " "05D-ZM"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-ZM" "----,---,--9" "18" "14" "12" "0105D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0205D-ZM" BY REFERENCE WT-TZZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0305D-ZM" "----,---,--9" "18" "26" "12" "0205D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0305D-ZM" BY REFERENCE WT-TUA "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0405D-ZM" "----,---,--9" "18" "38" "12" "0305D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0405D-ZM" BY REFERENCE WT-TNB "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0505D-ZM" "----,---,--9" "18" "50" "12" "0405D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0505D-ZM" BY REFERENCE WT-TNK "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0605D-ZM" "----,---,--9" "18" "62" "12" "0505D-ZM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "0605D-ZM" BY REFERENCE WT-TUZ "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ZMC" " " "0" "0" "248" "D-ZM" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-ZMC" " " "14" "0" "8" " " "D-ZMC"  RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-ZMC" "X" "14" "8" "4" " " "01D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-ZMC" "X" "14" "13" "4" "0101D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-ZMC" " " "15" "0" "60" "01D-ZMC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-ZMC" "X" "15" "14" "12" " " "02D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
          "0202D-ZMC" "X" "15" "26" "12" "0102D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0302D-ZMC" "X" "15" "38" "12" "0202D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0402D-ZMC" "X" "15" "50" "12" "0302D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0502D-ZMC" "X" "15" "62" "12" "0402D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-ZMC" " " "16" "0" "60" "02D-ZMC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-ZMC" "X" "16" "14" "12" " " "03D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
          "0203D-ZMC" "X" "16" "26" "12" "0103D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0303D-ZMC" "X" "16" "38" "12" "0203D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0403D-ZMC" "X" "16" "50" "12" "0303D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0503D-ZMC" "X" "16" "62" "12" "0403D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-ZMC" " " "17" "0" "60" "03D-ZMC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-ZMC" "X" "17" "14" "12" " " "04D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
          "0204D-ZMC" "X" "17" "26" "12" "0104D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0304D-ZMC" "X" "17" "38" "12" "0204D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0404D-ZMC" "X" "17" "50" "12" "0304D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0504D-ZMC" "X" "17" "62" "12" "0404D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-ZMC" " " "18" "0" "60" "04D-ZMC" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-ZMC" "X" "18" "14" "12" " " "05D-ZMC" RETURNING RESU.
       CALL "SD_Init" USING 
          "0205D-ZMC" "X" "18" "26" "12" "0105D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0305D-ZMC" "X" "18" "38" "12" "0205D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0405D-ZMC" "X" "18" "50" "12" "0305D-ZMC" " " RETURNING RESU.
       CALL "SD_Init" USING 
          "0505D-ZMC" "X" "18" "62" "12" "0405D-ZMC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "232" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "239" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME" "X" "24" "15" "40" "E-STAT" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-ME" BY REFERENCE W-ME "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "N" "24" "15" "44" "E-ME78" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-TCD" "X" "24" "60" "4" "E-ME90" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-TCD" BY REFERENCE TT-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "60" "5" "E-TCD" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KH-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-HCD" "X" "24" "60" "5" "E-KEY" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-HCD" BY REFERENCE KHT-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME61" " " "24" "0" "38" "E-HCD" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME61" "X" "24" "15" "34" " " "E-ME61"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME61" "X" "24" "39" "4" "01E-ME61" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME61" BY REFERENCE W-FILE "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME61" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-ME99" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-CL" "X" "24" "1" "40" " " "E-CL"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-CL" "X" "24" "41" "40" "01E-CL" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
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
            "I-O" KKB-M_PNAME1 " " BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               MOVE SPACE TO W-ME
               MOVE W-ME7 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC15 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME15" D-SME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC14 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME14" D-SME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  1 = KKB-SC13 OR KKB-SC12
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME13" D-SME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  1 = KKB-SC11 OR KKB-SC10 OR KKB-SC09
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME99" D-SME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC06 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME06" D-SME06 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC05 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME05" D-SME05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC03 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME03" D-SME03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC02 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME02" D-SME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           IF  KKB-SC01 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME01" D-SME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           PERFORM CHK-RTN THRU CHK-EX.
           IF  W-ERR = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "SD_Output" USING
                "E-ME61" E-ME61 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-95
           END-IF
           MOVE 1 TO KKB-SC13.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME8 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-95
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
      *
           COPY LIBCPR.
           MOVE ZERO TO W-NG.
           MOVE D-NKNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-DATE.
      *
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
           CALL "DB_F_Open" USING
            "I-O" TT-M_PNAME1 " " BY REFERENCE TT-M_IDLST "1"
            "TT-KEY" BY REFERENCE TT-KEY.
           CALL "DB_F_Open" USING
            "I-O" KHT-M_PNAME1 " " BY REFERENCE KHT-M_IDLST "2"
            "KHT-KEY" BY REFERENCE KHT-KEY "KHT-KEYD" BY REFERENCE
            KHT-KEYD.
           CALL "DB_F_Open" USING
            "I-O" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
       M-15.
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "C-MID4" C-MID4 "p" RETURNING RESU
               GO TO M-35
           END-IF
           IF  U-HCD NOT = SPACE AND ZERO
               MOVE U-HCD TO W-KEY
               PERFORM KHM-RTN THRU KHM-EX
           END-IF
           IF  U-DC = 4
               GO TO M-25
           END-IF
           MOVE U-TCD TO TT-KEY.
      *           READ TT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TT-M_PNAME1 BY REFERENCE TT-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME1 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-ERR
               GO TO M-90
           END-IF
           IF  U-DC = 8
               COMPUTE TT-TUZ = TT-TUZ - U-KI
               COMPUTE TT-TNB = TT-TNB + U-KI
               GO TO M-20
           END-IF
           IF  U-DC = 9
               COMPUTE TT-TUZZ = TT-TUZZ - U-KI
               COMPUTE TT-TNBZ = TT-TNBZ + U-KI
               GO TO M-20
           END-IF
           IF  U-DC = 5
               COMPUTE TT-TUZZ = TT-TUZZ + U-KI
               COMPUTE TT-TUAZ = TT-TUAZ + U-KI
           ELSE
               COMPUTE TT-TUZ = TT-TUZ + U-KI
               COMPUTE TT-TUA = TT-TUA + U-KI
           END-IF
           IF  U-YC = 00
               GO TO M-20
           END-IF
           IF  U-DC = 5
               GO TO M-20
           END-IF
           COMPUTE TT-TUG ROUNDED = TT-TUG + (U-SU * U-GT).
       M-20.
           PERFORM TTMC-RTN THRU TTMC-EX.
      *           REWRITE TT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TT-M_PNAME1 TT-M_LNAME TT-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME2 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-TCD" E-TCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-ERR
               GO TO M-90
           END-IF
           IF U-DC = 5 OR 9
               GO TO M-15
           END-IF.
       M-25.
           MOVE U-HCD TO KHT-KEY.
      *           READ KHT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME3 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-ERR
               GO TO M-90
           END-IF
           IF  U-DC NOT = 4 AND 8
               COMPUTE KHT-UKIN = U-KI + KHT-UKIN
               COMPUTE KHT-GKIN = (U-SU * U-GT) + KHT-GKIN
           END-IF
           IF  U-DC NOT = 4 AND 8
               IF  U-DC NOT = 2
                   COMPUTE KHT-SSU = U-SU + KHT-SSU
               END-IF
           END-IF
           IF  U-DC = 8
               COMPUTE KHT-NKIN = KHT-NKIN + U-KI
           END-IF
           IF  U-DC = 3
               MOVE 5 TO KHT-AC
               ADD U-SU TO KHT-AUS KHT-AAS
           END-IF
           IF  U-DC = 4
               MOVE 5 TO KHT-AC
               ADD U-SU TO KHT-ASS
               SUBTRACT U-SU FROM KHT-AAS
           END-IF
      *           REWRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME4 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-ERR
               GO TO M-90
           END-IF
           GO TO M-15.
      *
       M-35.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO W-D
               GO TO M-60
           END-IF
           IF  NH-HCD NOT = SPACE AND ZERO
               MOVE NH-HCD TO W-KEY
               PERFORM KHM-RTN THRU KHM-EX
           END-IF
           MOVE NH-HCD TO KHT-KEY.
      *           READ KHT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME3 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-ERR
               GO TO M-90
           END-IF
           IF  NH-NHC NOT = ZERO
               ADD NH-SU TO KHT-HSU
               COMPUTE KHT-KKIN = KHT-KKIN - NH-KIN
           ELSE
               COMPUTE KHT-KKIN = NH-KIN + KHT-KKIN
               IF  NH-NC = 0
                   ADD NH-SU TO KHT-KSU
               ELSE
                  ADD NH-SU TO KHT-ISU
               END-IF
           END-IF
      *           REWRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME4 TO W-ME
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-HCD" E-HCD "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               MOVE 9 TO W-ERR
               GO TO M-90
           END-IF.
      *
       M-60.
       M-90.
           IF  W-ERR = 0
               IF  COMPLETION_CODE = 255
                   GO TO M-95
               END-IF
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TT-M_IDLST TT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       KHM-RTN.
           MOVE W-KEY TO KH-KEY.
      *           READ KH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO KH-T1
               MOVE SPACE TO W-ME
               MOVE W-ME9 TO W-ME
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO KHM-EX
           END-IF
           IF  KH-ENG = ZERO
               GO TO KHM-010
           END-IF
           MOVE ZERO TO W-NG.
           MOVE KH-ENG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NG >= W-DATE
               GO TO KHM-EX
           END-IF.
       KHM-010.
           MOVE W-DATE TO W-NG.
           MOVE W-NGS TO KH-ENG.
      *           REWRITE KH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KH-M_PNAME1 KH-M_LNAME KH-R RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO W-ME
               MOVE W-ME10 TO W-ME
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       KHM-EX.
           EXIT.
       CHK-RTN.
           CALL "DB_F_Open" USING
            "INPUT" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
       CHK-010.
      *           READ URI-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" URI-F_PNAME1 BY REFERENCE URI-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-020
           END-IF
           IF  U-PRC = 9
               GO TO CHK-010
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           MOVE 1 TO W-ERR.
           MOVE "URIF" TO W-FILE.
           GO TO CHK-EX.
       CHK-020.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
       CHK-030.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO CHK-040
           END-IF
           IF  NH-PRC = 9
               GO TO CHK-030
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           MOVE 1 TO W-ERR.
           MOVE "KNHF" TO W-FILE.
           GO TO CHK-EX.
       CHK-040.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
       CHK-EX.
           EXIT.
       TTMC-RTN.
           COMPUTE WT-TZZ = TT-TZZ + TT-TZZZ.
           COMPUTE WT-TUA = TT-TUA + TT-TUAZ.
           COMPUTE WT-TNB = TT-TNB + TT-TNBZ.
           COMPUTE WT-TNK = TT-TNK + TT-TNKZ.
           COMPUTE WT-TUZ = TT-TUZ + TT-TUZZ.
           COMPUTE WT-TUZW = WT-TZZ + WT-TUA - WT-TNB - WT-TNK.
           IF  WT-TUZ NOT = WT-TUZW
               CALL "SD_Output" USING
                "D-ZM" D-ZM "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-ZMC" D-ZMC "p" RETURNING RESU
           END-IF.
       TTMC-EX.
           EXIT.
