       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHD150.
      *********************************************************
      *    PROGRAM         :  工品加硫伝票インプット　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKD15                          *
      *        変更　　　  :  62/04/01                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  入力=0 , 変換=1                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(017) VALUE SPACE.
           02  H-MID          PIC  N(022) VALUE
                "＊＊＊　　加硫入力　プルーフリスト　　＊＊＊".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC  Z(002).
       01  HEAD2.
           02  F              PIC  X(049) VALUE
                " 日　付　　ｺ-ﾄﾞ 品　　　　名　　　　取ｺ数　　回数".
           02  F              PIC  X(042) VALUE
                "       数　量　　 単　価　　　 金　額 場所".
       01  W-P.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-KEY          PIC  X(005).
           02  F              PIC  X(001).
           02  P-NA           PIC  X(020).
           02  P-NAR   REDEFINES P-NA.
             03  P-NAN        PIC  N(010).
           02  P-TS           PIC  Z(005).
           02  P-KS           PIC ----,---.
           02  P-SU           PIC --,---,--9.99.
           02  P-TN           PIC ----,--9.99.
           02  P-KN           PIC -----,---,--9.
           02  F              PIC  X(001).
           02  P-BS           PIC ZZZ9.
       01  W-R.
           02  W-NHK          PIC  9(002).
           02  W-DATE         PIC  9(008).
           02  W-NGPD  REDEFINES W-DATE.
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
           02  W-NGPL  REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-KEY          PIC  X(005).
           02  W-TS           PIC  9(002).
           02  W-KS           PIC S9(005).
           02  W-SU           PIC S9(006)V9(02).
           02  W-TN           PIC  9(006)V9(02).
           02  W-KN           PIC S9(008).
           02  W-YC           PIC  9(002).
           02  F              PIC  X(001).
           02  W-BS           PIC  9(002).
           02  F              PIC  X(002).
           02  W-NC           PIC  9(001).
           02  F              PIC  X(009).
           02  W-PRC          PIC  9(001).
       01  WT-D.
           02  WT-KS          PIC S9(006).
           02  WT-SU          PIC S9(007)V9(02).
           02  WT-KN          PIC S9(010).
       01  W-DATA.
           02  W-NGP          PIC  9(008) VALUE ZERO.
           02  W-CO           PIC  X(005).
           02  W-DMM          PIC  9(001).
           02  W-CHK          PIC  9(001).
           02  W-SPACE        PIC  X(078) VALUE SPACE.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-KNG          PIC  9(004).
           02  W-KNGD  REDEFINES W-KNG.
             03  W-KNEN       PIC  9(002).
             03  W-KGET       PIC  9(002).
           02  W-L            PIC  9(002) VALUE ZERO.
           02  W-BSM          PIC  X(005).
           02  CNT            PIC  9(004).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKKBM.
           COPY LIKHM.
           COPY LSPF.
      *FD  KNH-F
       01  KNH-F_KHD150.
           02  KNH-F_PNAME1   PIC  X(004) VALUE "KNHF".
           02  F              PIC  X(001).
           02  KNH-F_LNAME    PIC  X(012) VALUE "KNH-F_KHD150".
           02  F              PIC  X(001).
           02  KNH-F_KEY1     PIC  X(100) VALUE SPACE.
           02  KNH-F_SORT     PIC  X(100) VALUE SPACE.
           02  KNH-F_IDLST    PIC  X(100) VALUE SPACE.
           02  KNH-F_RES      USAGE  POINTER.
       01  KNH-R.
           02  NH-NHK         PIC  9(002).
           02  NH-DATE.
             03  F            PIC  9(002).
             03  NH-NGPS      PIC  9(006).
           02  NH-KEY         PIC  X(005).
           02  NH-TS          PIC  9(002).
           02  NH-KS          PIC S9(005).
           02  NH-SU          PIC S9(006)V9(02).
           02  NH-TN          PIC  9(006)V9(02).
           02  NH-KN          PIC S9(008).
           02  NH-YC          PIC  9(002).
           02  F              PIC  X(001).
           02  NH-BS          PIC  9(002).
           02  F              PIC  X(002).
           02  NH-NC          PIC  9(001).
           02  F              PIC  X(008).
           02  NH-NRC         PIC  9(001).
           02  NH-PRC         PIC  9(001).
       77  F                  PIC  X(001).
      *FD  JSS-F
       01  JSS-F_KHD150.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_KHD150".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC          PIC  9(002).
           02  JS-DATE        PIC  9(008).
           02  JS-NGP   REDEFINES JS-DATE.
             03  F            PIC  9(002).
             03  JS-NGS       PIC  9(004).
             03  F            PIC  9(002).
           02  JS-SCD         PIC  9(004).
           02  JS-JCD         PIC  9(006).
           02  JS-SU          PIC S9(007)V9(02).
           02  JS-T           PIC S9(006)V9(02).
           02  JS-KIN         PIC S9(008).
           02  JS-SHZ         PIC S9(007).
           02  JS-CD          PIC  9(006).
           02  JS-SJCD        PIC  9(006).
           02  JS-NNO         PIC  9(006).
           02  JS-FC          PIC  9(001).
           02  JS-YC          PIC  9(001).
           02  JS-TC          PIC  9(001).
           02  JS-SEC         PIC  9(001).
           02  JS-SC          PIC  9(001).
           02  JS-BSC         PIC  9(001).
           02  JS-BKC         PIC  9(002).
           02  JS-KCO         PIC  X(005).
           02  JS-KHC         PIC  9(001).
           02  F              PIC  X(010).
           02  JS-KEY.
             03  JS-DNO       PIC  X(006).
             03  JS-GNO       PIC  9(001).
           02  JS-PCNT        PIC  9(001).
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　工品　製品仕入　変換　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  FILLER.
             03  A-PEY   PIC  9(002).
             03  A-KEY   PIC  X(005).
             03  A-TS    PIC  9(002).
             03  A-KS    PIC S9(004).
             03  A-SU    PIC S9(006)V9(02).
             03  A-BS    PIC  9(002).
             03  A-DMM   PIC  9(001).
           02  A-DMMC  PIC  9(001).
       01  C-DSP.
           02  D-NG.
             03  01D-NG  PIC Z9 .
             03  02D-NG  PIC Z9 .
           02  FILLER.
             03  D-NA    PIC  X(020).
             03  D-TS    PIC Z9 .
             03  D-KS    PIC ZZZ9- .
             03  D-SU    PIC ZZZZZ9.99- .
             03  D-TN    PIC ZZZZZ9.99 .
             03  D-BSM   PIC  X(005).
           02  D-PRN1  PIC  N(020) VALUE
                "＊＊＊　　加硫伝票　入力リスト　　＊＊＊".
           02  D-PRN2  PIC  N(020) VALUE
                "＊＊＊　　製品仕入　変換リスト　　＊＊＊".
           02  FILLER.
             03  D-SME05 PIC  N(017) VALUE
                  "＊＊＊　　他で加硫入力中　　＊＊＊".
             03  D-SME06 PIC  N(017) VALUE
                  "＊＊＊　　他で廃却入力中　　＊＊＊".
             03  D-SME13 PIC  N(017) VALUE
                  "＊＊＊　　他で日次更新中　　＊＊＊".
             03  D-SME15 PIC  N(017) VALUE
                  "＊＊＊　　他で月次更新中　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME2   PIC  X(014) VALUE
                  "(  LTI ｶｸﾆﾝ  )".
             03  E-ME3   PIC  X(026) VALUE
                  "***  JSSF REWRITE ｴﾗｰ  ***".
             03  E-ME4   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME5   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME6.
               04  FILLER   PIC  X(018) VALUE
                    "***  ﾋﾂﾞｹ ｴﾗｰ  ***".
               04  02E-ME6  PIC 99/99 .
             03  E-ME7   PIC  X(023) VALUE
                  "***  ﾀﾝｶ ｶﾞ ﾊｲｯﾃﾅｲ  ***".
             03  E-ME9   PIC  X(022) VALUE
                  "***  ｾﾝﾋﾝｼｲﾚ ﾁｪｯｸ  ***".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  X(005).
             03  E-CL.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
           COPY LIBSCR.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT  RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "302" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "22" "20" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "25" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "W-L" "0" "24" " " "C-ACP" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PEY" "9" "W-L" "2" "2" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PEY" BY REFERENCE W-PEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "X" "W-L" "5" "5" "A-PEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TS" "9" "W-L" "34" "2" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TS" BY REFERENCE W-TS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KS" "S9" "W-L" "37" "4" "A-TS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KS" BY REFERENCE W-KS "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9V9" "W-L" "43" "8" "A-KS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BS" "9" "W-L" "64" "2" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BS" BY REFERENCE W-BS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-L" "75" "1" "A-BS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMMC" "9" "22" "37" "1" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMMC" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "271" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NG" " " "2" "0" "4" " " "C-DSP"  RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NG" "Z9" "2" "2" "2" " " "D-NG"  RETURNING RESU.
       CALL "SD_From" USING 
            "01D-NG" BY REFERENCE W-KNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NG" "Z9" "2" "6" "2" "01D-NG" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NG" BY REFERENCE W-KGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "51" "D-NG" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NA" "X" "W-L" "11" "20" " " "02C-DSP"  RETURNING RESU.
       CALL "SD_From" USING 
            "D-NA" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TS" "Z9" "W-L" "34" "2" "D-NA" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-TS" BY REFERENCE W-TS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KS" "ZZZ9-" "W-L" "37" "5" "D-TS" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-KS" BY REFERENCE W-KS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZ9.99-" "W-L" "43" "10" "D-KS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TN" "ZZZZZ9.99" "W-L" "54" "9" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-TN" BY REFERENCE W-TN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BSM" "X" "W-L" "67" "5" "D-TN" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "D-BSM" BY REFERENCE W-BSM "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN1" "N" "1" "20" "40" "02C-DSP" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN2" "N" "1" "20" "40" "D-PRN1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "15" "0" "136" "D-PRN2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME05" "bN" "15" "18" "34" " " "05C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME06" "bN" "15" "18" "34" "D-SME05" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME13" "bN" "15" "18" "34" "D-SME06" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SME15" "bN" "15" "18" "34" "D-SME13" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "312" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "356" " " "C-ERR"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR"  RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "16" "E-STAT" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "14" "E-ME1" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "17" "E-ME3" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "26" "E-ME4" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" " " "24" "0" "23" "E-ME5" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME6" "X" "24" "15" "18" " " "E-ME6"  RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME6" "99/99" "24" "35" "5" "01E-ME6" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME6" BY REFERENCE JS-NGS "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "23" "E-ME6" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "22" "E-ME7" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME9" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "N" "24" "15" "44" "E-ME78" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME90" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "40" "5" "E-ME99" " "  RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE KH-KEY "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" " " "24" "0" "80" "E-KEY" " "  RETURNING RESU.
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
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           MOVE D-NKNG TO W-KNG.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
      *
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
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC15 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME15" D-SME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC13 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME13" D-SME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC05 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME05" D-SME05 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  KKB-SC06 = 1
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "D-SME06" D-SME06 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE 1 TO KKB-SC05.
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
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
      *
           IF  JS-SIGN = 0
               GO TO M-15
           END-IF
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-DMMC "A-DMMC" "9" "1"
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
           END-IF.
       M-15.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "EXTEND" KNH-F_PNAME1 " " BY REFERENCE KNH-F_IDLST "0".
           MOVE DATE-03R TO H-DATE.
           MOVE 5 TO W-CHK.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
                "JS-KEY" BY REFERENCE JS-KEY
               GO TO M-40
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
       M-20.
           PERFORM ACP-RTN THRU ACP-EX.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           GO TO M-50.
       M-40.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSS-F_IDLST JSS-F_PNAME1
               GO TO M-50
           END-IF
           IF  JS-KHC NOT = 1
               GO TO M-40
           END-IF
           IF  JS-KCO = ZERO OR SPACE
               GO TO M-40
           END-IF
           IF  JS-JCD < 490000 OR > 498999
               GO TO M-40
           END-IF
           IF  JS-NGS > W-KNG
               GO TO M-40
           END-IF
           IF  JS-NGS < W-KNG
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-40
           END-IF
      *
           MOVE JS-KCO TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-40
           END-IF
           IF  KH-YC NOT = 10 AND 11
               IF  KH-GT1 = ZERO
                   CALL "SD_Output" USING
                    "E-ME7" E-ME7 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           MOVE ZERO TO KNH-R.
           MOVE JS-DATE TO NH-DATE.
           MOVE JS-KCO TO NH-KEY.
           MOVE JS-SU TO NH-SU.
           MOVE KH-GT1 TO NH-TN.
           COMPUTE NH-KN = NH-SU * NH-TN.
           MOVE KH-YC TO NH-YC.
           MOVE KH-NC TO NH-NC.
           MOVE 1 TO NH-NRC.
      *           WRITE KNH-R.
      *//////////////
           CALL "DB_Insert" USING
            KNH-F_PNAME1 KNH-F_LNAME KNH-R RETURNING RET.
      *
           MOVE 2 TO JS-KHC.
      *           REWRITE JSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE JSS-F_IDLST JSS-F_PNAME1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-50
           END-IF
           GO TO M-40.
       M-50.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
           PERFORM LST-RTN THRU LST-EX.
       M-90.
           IF  W-CHK = ZERO
               PERFORM TOT-RTN THRU TOT-EX
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE KNH-F_IDLST KNH-F_PNAME1.
       M-95.
           PERFORM KKB-RTN THRU KKB-EX.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Screen_Output" USING "SCKD15" RETURNING RESU.
           CALL "SD_Output" USING "D-NG" D-NG "p" RETURNING RESU.
           MOVE 4 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE W-KNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       ACP-020.
           IF  W-NGP NOT = ZERO
               MOVE W-NGP TO W-DATE
               MOVE W-CO TO W-KEY
               CALL "SD_Output" USING "A-PEY" A-PEY "p" RETURNING RESU
               CALL "SD_Output" USING "A-KEY" A-KEY "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-PEY "A-PEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO ACP-020
           END-IF
           IF  W-NGP = W-DATE
               GO TO ACP-040
           END-IF
           MOVE W-DATE TO W-NGP.
       ACP-040.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-040
           END-IF
           MOVE W-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-040
           END-IF
           CALL "SD_Output" USING "D-NA" D-NA "p" RETURNING RESU.
           MOVE W-KEY TO W-CO.
           MOVE KH-TRS TO W-TS.
           MOVE KH-GT1 TO W-TN.
           IF  KH-YC NOT = 10 AND 11
               MOVE ZERO TO W-TS W-KS
               CALL "SD_Output" USING "D-TS" D-TS "p" RETURNING RESU
               CALL "SD_Output" USING "D-KS" D-KS "p" RETURNING RESU
           END-IF
           IF  W-TN = ZERO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-040
           END-IF
           IF  KH-NC = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF  W-SU = ZERO
               GO TO ACP-060
           END-IF
           GO TO ACP-140.
       ACP-080.
           CALL "SD_Output" USING "D-TS" D-TS "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-TS "A-TS" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           CALL "SD_Output" USING "D-TS" D-TS "p" RETURNING RESU.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-KS "A-KS" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           CALL "SD_Output" USING "D-KS" D-KS "p" RETURNING RESU.
           COMPUTE W-SU = W-TS * W-KS.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF  W-SU = ZERO
               GO TO ACP-080
           END-IF.
       ACP-140.
           COMPUTE W-KN = W-SU * W-TN.
           CALL "SD_Output" USING "D-TN" D-TN "p" RETURNING RESU.
       ACP-160.
           CALL "SD_Accept" USING BY REFERENCE A-BS "A-BS" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  KH-YC NOT = 10 AND 11
                   GO TO ACP-060
               ELSE
                   GO TO ACP-100
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-160
           END-IF
           MOVE SPACE TO KKB-KEY.
           MOVE "04" TO KKB-NO.
           MOVE W-BS TO KKB-KS2.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-160
           END-IF
           MOVE KKB-KSN2 TO W-BSM.
           CALL "SD_Output" USING "D-BSM" D-BSM "p" RETURNING RESU.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-200
           END-IF
           IF  W-DMM = 9
               GO TO ACP-020
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-200
           END-IF
      *
           MOVE ZERO TO W-NHK.
           MOVE KH-YC TO W-YC.
           MOVE KH-NC TO W-NC.
           MOVE ZERO TO KNH-R.
           MOVE W-R TO KNH-R.
      *           WRITE KNH-R.
      *//////////////
           CALL "DB_Insert" USING
            KNH-F_PNAME1 KNH-F_LNAME KNH-R RETURNING RET.
           ADD 1 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-L > 20
               GO TO ACP-RTN
           ELSE
               GO TO ACP-020
           END-IF.
       ACP-EX.
           EXIT.
       LST-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "D-PRN1" D-PRN1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-PRN2" D-PRN2 "p" RETURNING RESU
               MOVE  "＊＊＊　　工品製品仕入　変換リスト　　＊＊＊"
                                                             TO H-MID
           END-IF
           CALL "DB_F_Open" USING
            "I-O" KNH-F_PNAME1 "SHARED" BY REFERENCE KNH-F_IDLST "0".
       LST-020.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO LST-EX
           END-IF
           IF  NH-NHK NOT = ZERO
               GO TO LST-020
           END-IF
           IF  NH-PRC = 9
               GO TO LST-020
           END-IF.
       LST-040.
           MOVE NH-DATE TO W-DATE.
           MOVE ZERO TO WT-D CNT.
       LST-060.
           IF  W-CHK = 5
               MOVE ZERO TO W-CHK
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           END-IF
      *
           MOVE NH-KEY TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE " **  ﾏｽﾀｰ ﾅｼ  **    " TO KH-NAME
           END-IF
      *
           MOVE SPACE TO SP-R W-P.
           IF  CNT = ZERO
               MOVE NH-NGPS TO P-DATE
           END-IF
           MOVE NH-KEY TO P-KEY.
           MOVE KH-NAME TO P-NA.
           MOVE NH-TS TO P-TS.
           MOVE NH-KS TO P-KS.
           MOVE NH-SU TO P-SU.
           MOVE NH-TN TO P-TN.
           MOVE NH-KN TO P-KN.
           IF  NH-BS NOT = ZERO
               MOVE NH-BS TO P-BS
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               MOVE NH-NGPS TO P-DATE
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE 9 TO NH-PRC.
      *           REWRITE KNH-R.
      *///////////////
           CALL "DB_Update" USING
            KNH-F_PNAME1 KNH-F_LNAME KNH-R RETURNING RET.
      *
           ADD NH-KS TO WT-KS.
           ADD NH-SU TO WT-SU.
           ADD NH-KN TO WT-KN.
       LST-080.
      *           READ KNH-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" KNH-F_PNAME1 BY REFERENCE KNH-R " " RETURNING RET.
           IF  RET = 1
               GO TO LST-EX
           END-IF
           IF  NH-NHK NOT = ZERO
               GO TO LST-080
           END-IF
           IF  NH-PRC = 9
               GO TO LST-080
           END-IF
           IF  NH-DATE = W-DATE
               GO TO LST-060
           END-IF
           PERFORM TOT-RTN THRU TOT-EX
           GO TO LST-040.
       LST-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
       MID-EX.
           EXIT.
       TOT-RTN.
           MOVE SPACE TO SP-R W-P.
           MOVE "　［　ＴＯＴＡＬ　］" TO P-NAN.
           MOVE WT-KS TO P-KS.
           MOVE WT-SU TO P-SU.
           MOVE WT-KN TO P-KN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       TOT-EX.
           EXIT.
       KKB-RTN.
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
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KKB-EX
           END-IF
           MOVE 0 TO KKB-SC05.
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
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB-EX.
           EXIT.
