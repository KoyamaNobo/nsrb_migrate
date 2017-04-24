       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD010.
      *********************************************************
      *    PROGRAM         :  工品他　売上・値引入力　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKD01                          *
      *        変更　　　  :  62/03/26                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-AR.
           02  W-R     OCCURS   6.
             03  W-DC         PIC  9(001).
             03  W-DATE       PIC  9(008).
             03  W-TCD        PIC  9(004).
             03  W-HCD.
               04  W-HCD1     PIC  X(003).
               04  W-HCD2     PIC  X(002).
             03  W-SU         PIC S9(006)V9(02).
             03  W-T          PIC S9(006)V9(02).
             03  W-KIN        PIC S9(008).
             03  W-YC         PIC  9(002).
             03  F            PIC  X(011).
             03  W-CSC        PIC  9(001).
             03  W-BKC        PIC  9(002).
             03  W-SKD        PIC  9(008).
             03  W-DNO        PIC  9(006).
             03  W-GNO        PIC  9(001).
             03  W-JCD        PIC  9(006).
             03  W-GT         PIC S9(006)V9(02).
             03  W-TEK        PIC  N(018).
             03  W-BMC        PIC  9(001).
             03  F            PIC  9(003).
             03  W-PRC        PIC  9(001).
       01  W-DATA.
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-TCDD         PIC  9(004).
           02  W-CSCD         PIC  9(001).
           02  W-DCD          PIC  9(001).
           02  W-SKDD         PIC  9(008).
           02  W-SNGP  REDEFINES W-SKDD.
             03  W-SNEN       PIC  9(004).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
           02  W-SDATE        PIC  9(008).
           02  CNT            PIC  9(001).
           02  ACNT           PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-GKIN         PIC S9(008).
           02  W-SHZ          PIC S9(007).
           02  W-TKIN         PIC S9(008).
           02  W-ATND.
             03  W-TND   OCCURS   6.
               04  W-TN       PIC S9(006)V9(02).
           02  W-DMM          PIC  9(001).
           02  W-CHK          PIC  9(001).
           02  W-KNG          PIC  9(004).
           02  W-DNOD         PIC  9(006).
           02  W-ZC           PIC  9(001).
           02  W-EC           PIC  9(001) VALUE 0.
           02  W-MC           PIC  9(001).
           02  W-UNC          PIC  9(001).
           02  W-UNCD         PIC  9(001).
           02  W-TEKD         PIC  N(018).
           02  W-TEKDD REDEFINES W-TEKD.
             03  W-TEKX  OCCURS  36.
               04  W-TEKA     PIC  X(001).
           02  W-DTW1         PIC  9(003).
           02  W-DTW2         PIC  9(001).
           02  W-CC           PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKKBM.
           COPY LIKHM.
           COPY LITM.
           COPY LITSKF.
           COPY LIJM.
      *FD  URI-F
       01  URI-F_KHD010.
           02  URI-F_PNAME1   PIC  X(004) VALUE "URIF".
           02  F              PIC  X(001).
           02  URI-F_LNAME    PIC  X(012) VALUE "URI-F_KHD010".
           02  F              PIC  X(001).
           02  URI-F_KEY1     PIC  X(100) VALUE SPACE.
           02  URI-F_SORT     PIC  X(100) VALUE SPACE.
           02  URI-F_IDLST    PIC  X(100) VALUE SPACE.
           02  URI-F_RES      USAGE  POINTER.
       01  URI-R.
           02  U-DC           PIC  9(001).
           02  U-DATE.
             03  F            PIC  9(002).
             03  U-NGPS       PIC  9(006).
           02  U-TCD          PIC  9(004).
           02  U-HCD          PIC  X(005).
           02  U-SU           PIC S9(006)V9(02).
           02  U-T            PIC S9(006)V9(02).
           02  U-KIN          PIC S9(008).
           02  U-YC           PIC  9(002).
           02  U-SD           PIC  9(004).
           02  U-NNO          PIC  X(006).
           02  U-HYC          PIC  9(001).
           02  U-CSC          PIC  9(001).
           02  U-BKC          PIC  9(002).
           02  U-SKD          PIC  9(008).
           02  U-DNO          PIC  9(006).
           02  U-GNO          PIC  9(001).
           02  U-JCD          PIC  9(006).
           02  U-GT           PIC S9(006)V9(02).
           02  U-TEK          PIC  N(018).
           02  U-BMC          PIC  9(001).
           02  F              PIC  X(002).
           02  U-HSC          PIC  9(001).
           02  U-PRC          PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-UNC   PIC  9(001).
           02  A-DATE  PIC  9(006).
           02  A-TCD   PIC  9(004).
           02  A-CSC   PIC  9(001).
           02  A-DC    PIC  9(001).
           02  A-SKD   PIC  9(008).
           02  FILLER.
             03  A-HCD   PIC  X(005).
             03  A-JCD   PIC  9(006).
             03  A-SU    PIC S9(006)V9(02).
             03  A-T     PIC S9(006)V9(02).
             03  A-KIN   PIC S9(008).
           02  FILLER.
             03  A-ZC    PIC  9(001).
             03  A-SHZ   PIC S9(007).
           02  A-TEK   PIC  N(018).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-TNAME PIC  N(026).
           02  FILLER.
             03  D-HNAME PIC  X(020).
             03  D-JNAME PIC  N(013).
             03  D-SU    PIC ZZZZZ9.99-.
             03  D-T     PIC ZZZZZ9.99-.
             03  D-KIN   PIC ZZZZZZZ9-.
           02  D-TD.
             03  D-GKIN.
               04  FILLER  PIC ZZZZZZZ9-.
             03  D-SHZ.
               04  FILLER  PIC ZZZZZZ9-.
             03  D-TKIN.
               04  FILLER  PIC ZZZZZZZ9-.
           02  FILLER.
             03  D-SME01 PIC  N(017) VALUE
                  "＊＊＊　　他で売上入力中　　＊＊＊".
             03  D-SME02 PIC  N(017) VALUE
                  "＊＊＊　　他で売上変換中　　＊＊＊".
             03  D-SME13 PIC  N(017) VALUE
                  "＊＊＊　　他で日次更新中　　＊＊＊".
             03  D-SME15 PIC  N(017) VALUE
                  "＊＊＊　　他で月次更新中　　＊＊＊".
       01  C-SPC.
           02  S-SKD.
             03  FILLER  PIC  X(008) VALUE "        ".
           02  S-D.
             03  S-HCD   PIC  X(005) VALUE "     ".
             03  S-HNAME PIC  X(033) VALUE
                  "                                 ".
             03  S-SU    PIC  X(010) VALUE "          ".
             03  S-T     PIC  X(010) VALUE "          ".
             03  S-KIN   PIC  X(009) VALUE "         ".
           02  S-GKIN.
             03  FILLER  PIC  X(009) VALUE "         ".
           02  S-SHZ.
             03  FILLER  PIC  X(008) VALUE "        ".
           02  S-TKIN.
             03  FILLER  PIC  X(009) VALUE "         ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME3   PIC  X(019) VALUE
                  "***  ｱｽﾞｶﾘ ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ﾋﾂﾞｹ ｴﾗｰ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  ﾌﾞﾓﾝ ｴﾗｰ  ***".
             03  E-ME6   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME7   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME8   PIC  N(022) VALUE
                  "材料使用マスターなし　（チェックして下さい）".
             03  E-ME9   PIC  X(019) VALUE
                  "***  ｻﾞｲﾘｮｳ ﾅｼ  ***".
             03  E-ME10  PIC  X(032) VALUE
                  "***  ﾃｷﾖｳ ﾆ ｺﾓｼﾞ ﾉ ｽﾍﾟｰｽ ｱﾘ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  ｹﾞﾝｶ ZERO  ***".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UNC" "9" "3" "50" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UNC" BY REFERENCE W-UNC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "4" "11" "6" "A-UNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "5" "11" "4" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CSC" "9" "6" "11" "1" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CSC" BY REFERENCE W-CSCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DC" "9" "7" "11" "1" "A-CSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DC" BY REFERENCE W-DCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKD" "9" "8" "11" "8" "A-DC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKD" BY REFERENCE W-SKDD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "W-L" "0" "35" "A-SKD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "X" "W-L" "2" "5" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD(1) "5" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD" "9" "W-L" "8" "6" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD" BY REFERENCE W-JCD(1) "6" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9V9" "W-L" "43" "8" "A-JCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU(1) "8" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T" "S9V9" "W-L" "54" "8" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-T" BY REFERENCE W-T(1) "8" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "S9" "W-L" "65" "8" "A-T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE W-KIN(1) "8" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-ACP" " " "19" "0" "8" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ZC" "9" "19" "51" "1" " " "08C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ZC" BY REFERENCE W-ZC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHZ" "S9" "19" "66" "7" "A-ZC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHZ" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TEK" "N" "21" "2" "36" "08C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TEK" BY REFERENCE W-TEKD "36" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "47" "1" "A-TEK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "289" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNAME" "N" "5" "16" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "75" "D-TNAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNAME" "X" "W-L" "8" "20" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNAME" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JNAME" "N" "W-L" "15" "26" "D-HNAME" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-JNAME" BY REFERENCE J-NAME "26" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZ9.99-" "W-L" "43" "10" "D-JNAME" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU(1) "8" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "ZZZZZ9.99-" "W-L" "54" "10" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE W-T(1) "8" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZ9-" "W-L" "65" "9" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE W-KIN(1) "8" "1" BY REFERENCE CNT 128
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TD" " " "0" "0" "26" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKIN" " " "0" "16" "9" " " "D-TD" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-GKIN" "ZZZZZZZ9-" "18" "65" "9" " " "D-GKIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-GKIN" BY REFERENCE W-GKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHZ" " " "0" "16" "8" "D-GKIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SHZ" "ZZZZZZ9-" "19" "66" "8" " " "D-SHZ"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-SHZ" BY REFERENCE W-SHZ "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TKIN" " " "0" "16" "9" "D-SHZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TKIN" "ZZZZZZZ9-" "20" "65" "9" " " "D-TKIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01D-TKIN" BY REFERENCE W-TKIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "15" "0" "136" "D-TD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME01" "bN" "15" "18" "34" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME02" "bN" "15" "18" "34" "D-SME01" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME13" "bN" "15" "18" "34" "D-SME02" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME15" "bN" "15" "18" "34" "D-SME13" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "101" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SKD" " " "0" "0" "8" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-SKD" "X" "8" "11" "8" " " "S-SKD" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-D" " " "W-L" "0" "67" "S-SKD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCD" "X" "W-L" "2" "5" " " "S-D" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HNAME" "X" "W-L" "8" "33" "S-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SU" "X" "W-L" "43" "10" "S-HNAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-T" "X" "W-L" "54" "10" "S-SU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-KIN" "X" "W-L" "65" "9" "S-T" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-GKIN" " " "0" "0" "9" "S-D" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-GKIN" "X" "18" "65" "9" " " "S-GKIN" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SHZ" " " "0" "0" "8" "S-GKIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-SHZ" "X" "19" "66" "8" " " "S-SHZ" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-TKIN" " " "0" "0" "9" "S-SHZ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-TKIN" "X" "20" "65" "9" " " "S-TKIN" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "353" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "353" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "16" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "19" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "17" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "26" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "N" "24" "15" "44" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "19" "E-ME8" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "32" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME10" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME11" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "N" "24" "15" "44" "E-ME78" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME90" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " "  RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           COPY LIBCPR.
           MOVE D-NKNG TO W-KNG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           PERFORM KKB1-RTN THRU KKB1-EX.
           IF  W-EC NOT = 0
               GO TO M-980
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" URI-F_PNAME1 " " BY REFERENCE URI-F_IDLST "0".
       M-040.
           CALL "SD_Screen_Output" USING "SCKD01" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-UNC "A-UNC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-700
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-UNC NOT = 1 AND 2
               GO TO M-040
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-700
           END-IF
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-03R TO W-NGPS
               CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-060
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-060
           END-IF
           IF  W-NGS NOT = W-KNG
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-060
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       M-080.
           CALL "SD_Screen_Output" USING "SCKD01" RETURNING RESU.
           CALL "SD_Output" USING "A-UNC" A-UNC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
      *
           MOVE W-TCDD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           CALL "SD_Output" USING "D-TNAME" D-TNAME "p" RETURNING RESU.
           IF  T-BC = 0
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           PERFORM SKD-RTN THRU SKD-EX.
           IF  W-UNC = 2
               MOVE 0 TO W-CSCD
               GO TO M-120
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-CSC "A-CSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           IF  W-CSCD NOT = 0 AND 9
               GO TO M-100
           END-IF.
       M-120.
           CALL "SD_Screen_Output" USING "SCKD01" RETURNING RESU.
           CALL "SD_Output" USING "A-UNC" A-UNC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNAME" D-TNAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CSC" A-CSC "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DC "A-DC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-UNC = 2
                   GO TO M-080
               ELSE
                   GO TO M-100
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-UNC = 1
               IF  W-DCD = 8
                   GO TO M-120
               END-IF
           END-IF
           IF  W-UNC = 2
               IF  W-DCD NOT = 8
                   GO TO M-120
               END-IF
           END-IF
           IF  W-CSCD = 9
               IF  W-DCD = 4
                   GO TO M-120
               END-IF
           END-IF
      *
           IF  W-DCD = 5
               MOVE 1 TO CNT
               MOVE 12 TO W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               CALL "SD_Output" USING "A-SKD" A-SKD "p" RETURNING RESU
               GO TO M-420
           END-IF
           IF  W-DCD NOT = 0 AND 1 AND 2 AND 3 AND 4 AND 8
               GO TO M-120
           END-IF
           CALL "SD_Output" USING "A-SKD" A-SKD "p" RETURNING RESU.
           GO TO M-160.
       M-150.
           CALL "SD_Accept" USING BY REFERENCE A-SKD "A-SKD" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-150
           END-IF
           IF  W-SKDD = ZERO
               GO TO M-160
           END-IF
           IF  W-SGET < 1 OR > 12
               GO TO M-150
           END-IF
           IF  W-SPEY < 1 OR > 31
               GO TO M-150
           END-IF
           IF  W-NGP > W-SKDD
               GO TO M-150
           END-IF
           IF  W-DCD = 5
               GO TO M-420
           END-IF.
       M-160.
           MOVE ZERO TO W-AR CNT W-GKIN.
           MOVE SPACE TO W-TEKD W-TEK(1) W-TEK(2) W-TEK(3)
                                W-TEK(4) W-TEK(5) W-TEK(6).
           IF  T-BC = 3
               MOVE ZERO TO W-ATND
           END-IF
           MOVE 11 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-180.
           ADD 1 TO W-L CNT.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = 7
               GO TO M-420
           END-IF
           MOVE ZERO TO W-R(CNT).
           MOVE SPACE TO W-TEK(CNT).
           MOVE W-NGP TO W-DATE(CNT).
           MOVE W-TCDD TO W-TCD(CNT).
           MOVE W-CSCD TO W-CSC(CNT).
           MOVE W-DCD TO W-DC(CNT).
           MOVE W-SKDD TO W-SKD(CNT).
           MOVE T-BC TO W-BMC(CNT).
           IF  T-BC = 3
               MOVE 00800 TO W-HCD(CNT)
               CALL "SD_Output" USING
                "A-HCD" A-HCD "p" RETURNING RESU
               GO TO M-250
           ELSE
               GO TO M-240
           END-IF.
       M-210.
           SUBTRACT 1 FROM CNT W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  CNT = ZERO
               IF  T-SS = 00 OR 99
                   GO TO M-120
               ELSE
                   GO TO M-150
               END-IF
           END-IF
           IF  W-TCD(CNT) = ZERO
               GO TO M-210
           END-IF
           IF  W-UNC = 2
               GO TO M-380
           END-IF
           IF  W-YC(CNT) = 00 OR 99
               MOVE 1 TO W-CHK
               GO TO M-380
           END-IF
           IF  W-DCD = 4
               GO TO M-320
           END-IF
           IF  W-YC(CNT) = 10 OR 11
               GO TO M-320
           END-IF
           GO TO M-340.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF ESTAT = BTB
               GO TO M-210
           END-IF
           IF  ESTAT = C1
               GO TO M-420
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF.
       M-250.
           MOVE W-HCD(CNT) TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-260
           END-IF
           GO TO M-270.
       M-260.
           IF  T-BC = 3
               GO TO M-210
           ELSE
               GO TO M-240
           END-IF.
       M-270.
           IF  W-JCD(CNT) = ZERO
               CALL "SD_Output" USING
                "D-HNAME" D-HNAME "p" RETURNING RESU
           END-IF
           IF  W-UNC = 2
               IF  KH-YC NOT = 00
                   MOVE KH-YC TO W-YC(CNT)
                   MOVE ZERO TO W-SU(CNT) W-T(CNT) W-GT(CNT)
                   CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
                   CALL "SD_Output" USING "S-T" S-T "p" RETURNING RESU
                   GO TO M-380
               END-IF
           END-IF
           IF  KH-YC = 10 OR 11 OR 99
               IF  W-DCD > 2
                   CALL "SD_Output" USING
                    "E-ME3" E-ME3 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-240
               END-IF
           END-IF
           IF  KH-YC = 00
               IF (W-DCD > 2) AND (W-DCD NOT = 8)
                   CALL "SD_Output" USING
                    "E-ME3" E-ME3 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-240
               END-IF
           END-IF.
       M-280.
           MOVE KH-YC TO W-YC(CNT).
           MOVE ZERO TO W-GT(CNT).
           IF  W-DCD < 4
               MOVE KH-GT1 TO W-GT(CNT)
           END-IF
           IF  KH-YC NOT = 00 AND 99
               IF  W-DCD < 4
                   IF  W-GT(CNT) = ZERO
                       CALL "SD_Output" USING
                        "E-ME11" E-ME11 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME99" E-ME99 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-CL" E-CL "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           MOVE ZERO TO W-CHK.
           IF  W-YC(CNT) NOT = 00 AND 99
               MOVE ZERO TO W-JCD(CNT) W-BKC(CNT)
               GO TO M-310
           END-IF
           IF  W-YC(CNT) = 99
               MOVE 1 TO W-CHK
               MOVE ZERO TO W-SU(CNT) W-T(CNT) W-JCD(CNT) W-GT(CNT)
                                                          W-BKC(CNT)
               CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
               CALL "SD_Output" USING "S-T" S-T "p" RETURNING RESU
               GO TO M-360
           END-IF
           IF  W-JCD(CNT) = ZERO
               CALL "SD_Output" USING
                "S-HNAME" S-HNAME "p" RETURNING RESU
           END-IF.
       M-290.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-210
           END-IF
           IF  ESTAT = C1
               GO TO M-420
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO M-290
           END-IF
      *
           MOVE W-JCD(CNT) TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-290
           END-IF
           CALL "SD_Output" USING "D-JNAME" D-JNAME "p" RETURNING RESU.
           MOVE J-BKC TO W-BKC(CNT).
           GO TO M-320.
       M-310.
           IF  W-DCD = 4
               MOVE ZERO TO W-T(CNT)
           ELSE
               MOVE KH-T1 TO W-T(CNT)
           END-IF.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YC(CNT) = 00
                   GO TO M-290
               ELSE
                   GO TO M-240
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-DCD = 1 OR 2
               COMPUTE W-SU(CNT) = W-SU(CNT) * -1
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           IF  W-DCD = 4
               GO TO M-360
           END-IF
           IF  KH-YC = 10 OR 11
               GO TO M-360
           END-IF.
       M-340.
           CALL "SD_Accept" USING BY REFERENCE A-T "A-T" "S9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-340
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
      *
           IF  W-YC(CNT) NOT = ZERO
               GO TO M-360
           END-IF
           IF  CNT > 1
               IF  W-T(CNT) = W-TN(1)
                   GO TO M-340
               END-IF
           END-IF
           IF  CNT > 2
               IF  W-T(CNT) = W-TN(2)
                   GO TO M-340
               END-IF
           END-IF
           IF  CNT > 3
               IF  W-T(CNT) = W-TN(3)
                   GO TO M-340
               END-IF
           END-IF
           IF  CNT > 4
               IF  W-T(CNT) = W-TN(4)
                   GO TO M-340
               END-IF
           END-IF
           IF  CNT > 5
               IF  W-T(CNT) = W-TN(5)
                   GO TO M-340
               END-IF
           END-IF.
       M-360.
           IF  W-CHK = ZERO
               COMPUTE W-KIN(CNT) = W-SU(CNT) * W-T(CNT)
               CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU
               GO TO M-400
           END-IF.
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-380
           END-IF
           IF  W-DC(CNT) = 1 OR 2
               COMPUTE W-KIN(CNT) = W-KIN(CNT) * -1
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       M-400.
           IF  W-YC(CNT) = ZERO
               MOVE W-T(CNT) TO W-TN(CNT) W-GT(CNT)
           END-IF
           GO TO M-180.
       M-420.
           IF  CNT NOT = 7
               MOVE ZERO TO W-R(CNT)
               MOVE SPACE TO W-TEK(CNT)
               CALL "SD_Output" USING "S-D" S-D "p" RETURNING RESU
               ADD 1 TO CNT W-L
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               GO TO M-420
           END-IF
           MOVE ZERO TO CNT W-GKIN W-SHZ W-TKIN.
       M-440.
           ADD 1 TO CNT.
           IF  CNT NOT = 7
               ADD W-KIN(CNT) TO W-GKIN
               GO TO M-440
           END-IF.
      *
       M-480.
           IF  W-DCD = 5
               MOVE 8 TO W-ZC
               CALL "SD_Output" USING "A-ZC" A-ZC "p" RETURNING RESU
               GO TO M-560
           END-IF
           IF  W-DCD = 4
               MOVE 8 TO W-ZC
               CALL "SD_Output" USING "A-ZC" A-ZC "p" RETURNING RESU
               GO TO M-580
           END-IF
           IF (T-SS = 99) OR (T-ZEI = 1)
               MOVE 0 TO W-ZC
           ELSE
               MOVE 8 TO W-ZC
           END-IF
           CALL "SD_Output" USING "A-ZC" A-ZC "p" RETURNING RESU.
           GO TO M-520.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-ZC "A-ZC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-210
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           IF  W-ZC NOT = 0 AND 1 AND 3 AND 5 AND 8
               GO TO M-500
           END-IF.
       M-520.
           IF  W-ZC = 0
               MOVE ZERO TO W-SHZ
           ELSE
               IF  W-ZC = 1
                   COMPUTE W-SHZ ROUNDED = W-GKIN * 0.10
               ELSE
                   IF  W-ZC = 3
                       COMPUTE W-SHZ ROUNDED = W-GKIN * 0.03
                   ELSE
                       IF  W-ZC = 5
                           COMPUTE W-SHZ ROUNDED = W-GKIN * 0.05
                       ELSE
                           IF  W-ZC = 8
                               COMPUTE W-SHZ ROUNDED = W-GKIN * 0.08
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           GO TO M-580.
       M-560.
           CALL "SD_Accept" USING BY REFERENCE A-SHZ "A-SHZ" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-150
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-560
           END-IF
           IF  W-SHZ = ZERO
               GO TO M-560
           END-IF.
       M-580.
           COMPUTE W-TKIN = W-GKIN + W-SHZ.
           CALL "SD_Output" USING "D-TD" D-TD "p" RETURNING RESU.
       M-590.
           CALL "SD_Accept" USING BY REFERENCE A-TEK "A-TEK" "N" "36"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-DCD = 5
                   GO TO M-560
               ELSE
                   IF  W-DCD = 4
                       GO TO M-210
                   ELSE
                       GO TO M-500
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-590
           END-IF
           IF  SPACE = W-TEKA(01) OR W-TEKA(03) OR W-TEKA(05) OR
                      W-TEKA(07) OR W-TEKA(09) OR W-TEKA(11) OR
                      W-TEKA(13) OR W-TEKA(15) OR W-TEKA(17) OR
                      W-TEKA(19) OR W-TEKA(21) OR W-TEKA(23) OR
                      W-TEKA(25) OR W-TEKA(27) OR W-TEKA(29) OR
                      W-TEKA(31) OR W-TEKA(33) OR W-TEKA(35)
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-590
           END-IF.
       M-600.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-590
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-600
           END-IF
           IF  W-DMM = 9
               GO TO M-120
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-600
           END-IF
      *
           MOVE ZERO TO CNT.
           PERFORM KKB2-RTN THRU KKB2-EX.
       M-640.
           ADD 1 TO CNT.
           IF  CNT = 7
               GO TO M-660
           END-IF
           IF  W-TCD(CNT) = ZERO
               GO TO M-660
           END-IF
           MOVE W-DNOD TO W-DNO(CNT).
           MOVE CNT TO W-GNO(CNT).
           MOVE W-TEKD TO W-TEK(CNT).
           PERFORM WRI1-RTN THRU WRI1-EX.
           GO TO M-640.
       M-660.
           IF  W-SHZ NOT = ZERO
               PERFORM WRI2-RTN THRU WRI2-EX
           END-IF
           GO TO M-120.
       M-700.
       M-920.
           IF  W-CC = 0
               PERFORM KKB3-RTN THRU KKB3-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE URI-F_IDLST URI-F_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       KKB1-RTN.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO KKB1-020
           END-IF
           IF  KKB-SC15 = 1
               CALL "SD_Output" USING
                "D-SME15" D-SME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO KKB1-020
           END-IF
           IF  KKB-SC13 = 1
               CALL "SD_Output" USING
                "D-SME13" D-SME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO KKB1-020
           END-IF
           IF  KKB-SC01 = 1
               CALL "SD_Output" USING
                "D-SME01" D-SME01 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO KKB1-020
           END-IF
           IF  KKB-SC02 = 1
               CALL "SD_Output" USING
                "D-SME02" D-SME02 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
               GO TO KKB1-020
           END-IF
           MOVE 1 TO KKB-SC01.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-EC
           END-IF.
       KKB1-020.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB1-EX.
           EXIT.
       KKB2-RTN.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 95 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KKB2-020
           END-IF
           ADD 1 TO KKB-DNO.
           IF  KKB-DNO = ZERO
               ADD 1 TO KKB-DNO
           END-IF
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE KKB-DNO TO W-DNOD.
       KKB2-020.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB2-EX.
           EXIT.
       KKB3-RTN.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE SPACE TO KKB-KEY.
           MOVE 90 TO KKB-NO.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KKB3-020
           END-IF
           IF KKB-SC01 = 0
               GO TO KKB3-020
           END-IF
           MOVE 0 TO KKB-SC01.
      *           REWRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       KKB3-020.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB3-EX.
           EXIT.
       WRI1-RTN.
           MOVE ZERO TO URI-R.
           MOVE SPACE TO U-TEK.
           MOVE W-R(CNT) TO URI-R.
           IF  W-SHZ NOT = ZERO
               MOVE W-ZC TO U-HSC
           END-IF
      *           WRITE URI-R.
      *//////////////
           CALL "DB_Insert" USING
            URI-F_PNAME1 URI-F_LNAME URI-R RETURNING RET.
       WRI1-EX.
           EXIT.
       WRI2-RTN.
           MOVE ZERO TO URI-R.
           MOVE SPACE TO U-TEK.
           IF  W-UNC = 2
               MOVE 9 TO U-DC
           ELSE
               MOVE 5 TO U-DC
           END-IF
           MOVE W-NGP TO U-DATE.
           MOVE W-TCDD TO U-TCD.
           MOVE W-SHZ TO U-KIN.
           MOVE W-CSCD TO U-CSC.
           MOVE W-DNOD TO U-DNO.
           IF  W-DCD = 5
               MOVE 1 TO U-GNO
           ELSE
               MOVE 9 TO U-GNO
           END-IF
           MOVE W-SKDD TO U-SKD.
           MOVE W-TEKD TO U-TEK.
           MOVE T-BC TO U-BMC.
      *           WRITE URI-R.
      *//////////////
           CALL "DB_Insert" USING
            URI-F_PNAME1 URI-F_LNAME URI-R RETURNING RET.
       WRI2-EX.
           EXIT.
      *
       SKD-RTN.
           MOVE W-TCDD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF
           MOVE TSK-ZNGP(4) TO W-SDATE.
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE 99999999 TO W-SDATE
           END-IF
      *
           MOVE ZERO TO W-SNGP.
           IF  W-SDATE = 99999999
               GO TO SKD-EX
           END-IF
           MOVE W-NGP TO W-SNGP.
       SKD-020.
           MOVE T-SS TO W-SPEY.
           IF  W-SPEY = 00 OR 99
               MOVE ZERO TO W-SNGP
               GO TO SKD-EX
           END-IF
           IF  W-SPEY = 30 OR 31
               IF  W-SGET = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   MOVE 31 TO W-SPEY
               ELSE
                   IF  W-SGET = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO W-SPEY
                   ELSE
                       DIVIDE 4 INTO W-SNEN GIVING W-DTW1
                                                 REMAINDER W-DTW2
                       IF  W-DTW2 = 0
                           MOVE 29 TO W-SPEY
                       ELSE
                           MOVE 28 TO W-SPEY
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-NGP > W-SNGP
               IF  W-GET NOT = W-SGET
                   MOVE ZERO TO W-SNGP
                   GO TO SKD-EX
               ELSE
                   ADD 1 TO W-SGET
                   GO TO SKD-040
               END-IF
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SNGP
                   ADD 1 TO W-SGET
                   GO TO SKD-040
               END-IF
           END-IF
           GO TO SKD-EX.
       SKD-040.
           IF  W-SGET = 13
               MOVE 1 TO W-SGET
               ADD 1 TO W-SNEN
           END-IF
           GO TO SKD-020.
       SKD-EX.
           EXIT.
