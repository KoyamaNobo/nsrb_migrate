       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD210.
      *********************************************************
      *    PROGRAM         :  履物売上・値引入力              *
      *    SCREEN          :  SCHD01,SCHD11                   *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  当月=0 , 選択=1                 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  W-FILE             PIC  X(013).
       77  W-SEN              PIC  9(001) VALUE 0.
       77  W-END              PIC  9(001) VALUE 0.
       77  CHK                PIC  9(001) VALUE 0.
       77  W-INV              PIC  9(001) VALUE 0.
       77  W-ADV              PIC  9(001) VALUE 0.
       01  ERR-STAT           PIC  X(002).
       01  W-SR.
           02  W-DNO          PIC  9(006).
           02  W-GNO          PIC  9(001).
           02  W-DATE         PIC  9(008).
           02  W-TCD          PIC  9(004).
           02  W-DT1          PIC  X(107).
           02  W-DT2   REDEFINES W-DT1.
             03  W-ZD1.
               04  W-HCD      PIC  9(006).
               04  W-SIZ      PIC  9(001).
               04  W-SUS.
                 05  W-SUSD  OCCURS  10.
                   06  W-SU   PIC S9(004)  COMP-3.
               04  W-SUT      PIC S9(005).
               04  W-BT       PIC S9(005).
             03  W-KIN        PIC S9(008).
             03  W-CSC        PIC  9(001).
             03  W-DC         PIC  9(001).
             03  W-FT         PIC  9(005).
             03  W-CCD        PIC  9(003).
             03  W-ZD2.
               04  W-BC1      PIC  9(002).
               04  W-BC2.
                 05  W-BC21   PIC  9(001).
                 05  W-BC22   PIC  9(001).
               04  W-BC3      PIC  9(002).
               04  W-SOK      PIC  9(001).
             03  W-TC2        PIC  9(002).
             03  W-FKC        PIC  9(002).
             03  W-HSC        PIC  9(001).
             03  W-KOSU       PIC  9(003).
             03  W-FRC        PIC  9(001).
             03  W-TCD2       PIC  9(004).
             03  W-BIK        PIC  X(010).
             03  W-SDT        PIC  9(008).
             03  W-BMC        PIC  9(002).
             03  W-BMNO       PIC  9(001).
             03  W-USC        PIC  9(001).
           02  W-DHC          PIC  9(001).
           02  W-UNC          PIC  9(001).
       01  W-ASR.
           02  W-AS    OCCURS   6  PIC  X(128).
       01  W-BR.
           02  W-BI           PIC  N(024).
           02  W-HANO         PIC  9(006).
           02  F              PIC  X(030).
           02  W-TAX          PIC S9(007).
           02  W-UZZ          PIC S9(007).
           02  W-UZ           PIC S9(009).
       01  W-ASD.
           02  W-SD    OCCURS  10  PIC  9(001).
       01  W-DATA.
           02  W-L.
             03  W-L1         PIC  9(002).
             03  W-L2         PIC  9(002).
             03  W-L3         PIC  9(002).
           02  W-C            PIC S9(002).
           02  W-GSUT         PIC S9(006).
           02  W-UNCD         PIC  9(001).
           02  W-RC           PIC  9(001).
           02  W-SC           PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-DNOD         PIC  9(006).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NGD.
               04  W-ND       PIC  9(004).
               04  W-NDD   REDEFINES W-ND.
                 05  W-ND1    PIC  9(002).
                 05  W-ND2    PIC  9(002).
               04  W-GD       PIC  9(002).
             03  W-PD         PIC  9(002).
           02  W-NGPSD REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-TCDD         PIC  9(004).
           02  W-TCDN         PIC  9(004).
           02  W-CCDD         PIC  9(003).
           02  W-HCDD         PIC  9(006).
           02  W-HCDO         PIC  9(006).
           02  W-CSCD         PIC  9(001).
           02  W-HNOD         PIC  9(006).
           02  W-DCD          PIC  9(001).
           02  W-DCC          PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-CNG.
             03  W-CN         PIC  9(004).
             03  W-CND   REDEFINES W-CN.
               04  W-CN1      PIC  9(002).
               04  W-CN2      PIC  9(002).
             03  W-CG         PIC  9(002).
           02  W-SNGD  REDEFINES W-CNG.
             03  F            PIC  9(002).
             03  W-CNGS       PIC  9(004).
           02  W-HSCD         PIC  9(001).
           02  W-HNC          PIC  9(001).
           02  W-HNCD         PIC  9(001).
           02  W-SCD          PIC  9(001).
           02  W-KOSUR        PIC  9(003).
           02  WK-TAN         PIC S9(005).
           02  W-FRCD         PIC  9(001).
           02  W-DTW1         PIC  9(003).
           02  W-DTW2         PIC  9(001).
           02  W-SDTD         PIC  9(008).
           02  W-SDTM  REDEFINES W-SDTD.
             03  W-SDTNG.
               04  W-SDTN     PIC  9(004).
               04  W-SDTG     PIC  9(002).
             03  W-SDTP       PIC  9(002).
           02  W-HMN          PIC  9(006).
           02  WK-SCD         PIC  9(001).
           02  WK-DC          PIC  9(001).
           02  WK-BI          PIC  N(024).
           02  W-EC           PIC  9(001).
           02  W-DEL          PIC  9(001).
           02  W-RCD          PIC  9(001).
           02  CNT            PIC  9(001).
           02  W-HC           PIC  9(001).
           02  W-LD           PIC  9(006).
           02  W-RCB          PIC  9(001).
           02  W-LB           PIC  9(006).
           02  W-DSPC         PIC  9(001).
           02  W-DKIN         PIC S9(008).
           02  W-SDATE        PIC  9(008).
           02  W-HIC          PIC  9(001).
           02  W-FTD          PIC  9(005).
           02  W-TCHK         PIC  9(001).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY LIHIM.
           COPY LITCM.
           COPY LIHSMS.
           COPY LIHKBM.
           COPY LITHTM.
           COPY LIOTHT.
           COPY L-JCON.
           COPY LITSKF.
           COPY LIHUHM.
      *FD  S-TRAN
       01  S-TRAN_HMD210.
           02  S-TRAN_PNAME1  PIC  X(005) VALUE "STRAN".
           02  F              PIC  X(001).
           02  S-TRAN_LNAME   PIC  X(013) VALUE "S-TRAN_HMD210".
           02  F              PIC  X(001).
           02  S-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  S-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  S-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  S-TRAN_RES     USAGE  POINTER.
       01  S-R.
           02  S-DNO          PIC  9(006).
           02  S-GNO          PIC  9(001).
           02  S-DATE         PIC  9(008).
           02  S-TCD          PIC  9(004).
           02  F              PIC  X(107).
           02  S-DHC          PIC  9(001).
           02  S-SNC          PIC  9(001).
       77  F                  PIC  X(001).
      *FD  BBS-TRAN
       01  BBS-TRAN_HMD210.
           02  BBS-TRAN_PNAME1  PIC  X(008) VALUE "BB-STRAN".
           02  F                PIC  X(001).
           02  BBS-TRAN_LNAME   PIC  X(015) VALUE "BBS-TRAN_HMD210".
           02  F                PIC  X(001).
           02  BBS-TRAN_KEY1    PIC  X(100) VALUE SPACE.
           02  BBS-TRAN_SORT    PIC  X(100) VALUE SPACE.
           02  BBS-TRAN_IDLST   PIC  X(100) VALUE SPACE.
           02  BBS-TRAN_RES     USAGE  POINTER.
       01  BBS-R.
           02  BBS-DNO        PIC  9(006).
           02  BBS-GNO        PIC  9(001).
           02  BBS-CNG        PIC  9(006).
           02  F              PIC  9(002).
           02  BBS-TCD        PIC  9(004).
           02  F              PIC  X(107).
           02  BBS-DHC        PIC  9(001).
           02  BBS-SNC        PIC  9(001).
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
       01  C-MID.
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　履物売上・値引伝票　入力　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(022) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "売上=0 , 値引=1   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  X(026) VALUE
                "当月分=0 , 翌月分=1   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-SEN   PIC  9(001).
           02  A-UNC   PIC  9(001).
      *
           02  FILLER.
             03  AU-NGP   PIC  9(006).
             03  AU-HMN   PIC  9(006).
             03  AU-SOK   PIC  9(001).
           02  FILLER.
             03  AU-TCD   PIC  9(004).
           02  FILLER.
             03  AU-CCD   PIC  9(003).
             03  AU-CSC   PIC  9(001).
           02  FILLER.
             03  AU-DC    PIC  9(001).
             03  AU-HCD   PIC  9(006).
             03  AU-BIK   PIC  X(010).
           02  FILLER.
             03  AU-SIZ   PIC  9(001).
             03  AU-SU    PIC S9(004).
             03  AU-SU1   PIC S9(003).
             03  AU-SUT   PIC S9(005).
             03  AU-BT    PIC  9(005).
             03  AU-KIN   PIC S9(007).
           02  AU-BI    PIC  N(024).
           02  FILLER.
             03  AU-KOSU  PIC  9(003).
             03  AU-HSC   PIC  9(001).
             03  AU-FRC   PIC  9(001).
             03  AU-SDT   PIC  9(008).
             03  AU-DMM   PIC  9(001).
      *
           02  AN-NGP   PIC  9(006).
           02  AN-TCD   PIC  9(004).
           02  AN-CSC   PIC  9(001).
           02  FILLER.
             03  AN-HCD   PIC  9(006).
             03  AN-SUT   PIC S9(005).
             03  AN-BT    PIC S9(004).
             03  AN-KIN   PIC S9(008).
           02  AN-BI    PIC  N(024).
           02  AN-HSC   PIC  9(001).
           02  AN-SDT   PIC  9(008).
           02  AN-DMM   PIC  9(001).
      *
           02  A-CHK   PIC  9(001).
       01  C-DSP.
           02  DU-SNA   PIC  N(006).
           02  DU-TNA   PIC  N(026).
           02  DU-CNA   PIC  N(026).
           02  FILLER.
             03  DU-HNA   PIC  N(024).
             03  DU-SZM   PIC  N(006)    VALUE
                  "消費税調整分".
           02  FILLER.
             03  DU-S1.
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE " SS  ".
               04  FILLER  PIC  X(005) VALUE "  S  ".
               04  FILLER  PIC  X(005) VALUE "  M  ".
               04  FILLER  PIC  X(005) VALUE "  L  ".
               04  FILLER  PIC  X(005) VALUE " LL  ".
               04  FILLER  PIC  X(004) VALUE "28.0".
               04  FILLER  PIC  X(004) VALUE "29.0".
               04  FILLER  PIC  X(004) VALUE "30.0".
             03  DU-S2.
               04  FILLER  PIC  X(005) VALUE "12.5 ".
               04  FILLER  PIC  X(005) VALUE "13.0 ".
               04  FILLER  PIC  X(005) VALUE "13.5 ".
               04  FILLER  PIC  X(005) VALUE "14.0 ".
               04  FILLER  PIC  X(005) VALUE "15.0 ".
               04  FILLER  PIC  X(005) VALUE "16.0 ".
               04  FILLER  PIC  X(005) VALUE "17.0 ".
               04  FILLER  PIC  X(004) VALUE "18.0".
               04  FILLER  PIC  X(004) VALUE "19.0".
               04  FILLER  PIC  X(004) VALUE "20.0".
             03  DU-S3.
               04  FILLER  PIC  X(005) VALUE "21.0 ".
               04  FILLER  PIC  X(005) VALUE "21.5 ".
               04  FILLER  PIC  X(005) VALUE "22.0 ".
               04  FILLER  PIC  X(005) VALUE "22.5 ".
               04  FILLER  PIC  X(005) VALUE "23.0 ".
               04  FILLER  PIC  X(005) VALUE "23.5 ".
               04  FILLER  PIC  X(005) VALUE "24.0 ".
               04  FILLER  PIC  X(004) VALUE "24.5".
               04  FILLER  PIC  X(004) VALUE "25.0".
               04  FILLER  PIC  X(004) VALUE "    ".
             03  DU-S4.
               04  FILLER  PIC  X(005) VALUE "24.0 ".
               04  FILLER  PIC  X(005) VALUE "24.5 ".
               04  FILLER  PIC  X(005) VALUE "25.0 ".
               04  FILLER  PIC  X(005) VALUE "25.5 ".
               04  FILLER  PIC  X(005) VALUE "26.0 ".
               04  FILLER  PIC  X(005) VALUE "26.5 ".
               04  FILLER  PIC  X(005) VALUE "27.0 ".
               04  FILLER  PIC  X(004) VALUE "27.5".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
           02  FILLER.
             03  DU-SU    PIC ZZZZ- .
             03  DU-SU1   PIC ZZZ- .
             03  DU-SUT   PIC ZZZZZ- .
             03  DU-BT    PIC ZZZZZ .
             03  DU-KIN   PIC ZZZZZZZ- .
           02  FILLER.
             03  DU-GSUT  PIC ZZZ,ZZZ- .
           02  FILLER.
             03  DU-KOSU  PIC ZZ9 .
      *
           02  DN-TNA   PIC  N(026).
           02  FILLER.
             03  DN-HNA   PIC  N(024).
             03  DN-SUT   PIC ZZZZZZ- .
             03  DN-BT    PIC ZZZZ- .
             03  DN-KIN   PIC ZZZZZZZZ- .
           02  DN-DKIN.
             03  FILLER  PIC ZZ,ZZZ,ZZ9- .
       01  C-SPC.
           02  SU-HNO.
             03  FILLER.
               04  SU-DATE  PIC  X(006) VALUE "      ".
               04  SU-HMN   PIC  X(006) VALUE "      ".
               04  SU-SOK   PIC  X(014) VALUE
                    "            ".
               04  SU-HNM   PIC  X(010) VALUE "          ".
           02  SU-CNA.
             03  FILLER  PIC  X(003) VALUE "   ".
             03  FILLER  PIC  X(052) VALUE
                 "                                                    ".
           02  SU-D1.
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(006) VALUE "     ".
             03  FILLER  PIC  X(048) VALUE
                  "                                                ".
             03  SU-BIK   PIC  X(010) VALUE "          ".
           02  SU-D2.
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER  PIC  X(004) VALUE "    ".
           02  SU-D3.
             03  SU-D31.
               04  FILLER  PIC  X(001) VALUE " ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
               04  FILLER  PIC  X(004) VALUE "    ".
             03  FILLER   PIC  X(006) VALUE "      ".
             03  SU-BT    PIC  X(005) VALUE "     ".
             03  SU-KIN   PIC  X(008) VALUE "        ".
           02  FILLER.
             03  SU-KOSU  PIC  X(003) VALUE "   ".
             03  SU-HSC   PIC  X(001) VALUE " ".
             03  SU-FRC   PIC  X(001) VALUE " ".
      *
           02  SN-DS.
             03  FILLER  PIC  X(006) VALUE "      ".
             03  FILLER  PIC  X(048) VALUE
                  "                                                ".
             03  FILLER  PIC  X(007) VALUE "       ".
             03  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER  PIC  X(009) VALUE "         ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME2   PIC  X(016) VALUE
                  "***  TCM ﾅｼ  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME4   PIC  X(020) VALUE
                  "***  区分エラー  ***".
             03  E-ME5   PIC  X(020) VALUE
                  "***  直送エラー ***".
             03  E-ME6.
               04  FILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  FILLER  PIC  9(006).
             03  E-ME7.
               04  FILLER  PIC  X(022) VALUE
                    "***  振替単価無し  ***".
               04  FILLER  PIC  9(006).
             03  E-ME8   PIC  X(024) VALUE
                  "***  預りサイズ無し  ***".
             03  E-ME9   PIC  X(020) VALUE
                  "***  サイズ無し  ***".
             03  E-ME12  PIC  X(020) VALUE
                  "***  日付エラー ***".
             03  E-ME14  PIC  X(025) VALUE
                  "***  発送明細№ CHECK ***".
             03  E-ME15  PIC  X(025) VALUE
                  "***  単価  ZERO CHECK ***".
             03  E-ME16  PIC  X(025) VALUE
                  "***  STRAN WRITE ｴﾗｰ  ***".
             03  E-ME17  PIC  X(027) VALUE
                  "***  STRAN REWRITE ｴﾗｰ  ***".
             03  E-ME19  PIC  X(028) VALUE
                  "***  未更新データ　有り  ***".
             03  E-ME20  PIC  X(041) VALUE
                  "伝票未発行データ有り   消す  OK=1 NO=5   ".
             03  E-ME21  PIC  X(017) VALUE
                  "***  ﾃﾞｰﾀ ﾅｼ  ***".
             03  E-ME22  PIC  X(027) VALUE
                  "***  HSMSF REWRITE ｴﾗｰ  ***".
             03  E-ME23  PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME24  PIC  X(026) VALUE
                  "***  HKBM REWRITE ｴﾗｰ  ***".
             03  E-ME25.
               04  FILLER  PIC  N(008) VALUE
                    "請求書　発行済み".
               04  FILLER  PIC 9999/99/99 .
             03  E-ME26  PIC  X(024) VALUE
                  "***  THTM WRITE ｴﾗｰ  ***".
             03  E-ME27  PIC  X(026) VALUE
                  "***  THTM REWRITE ｴﾗｰ  ***".
             03  E-ME28  PIC  X(017) VALUE
                  "***  HUHM ﾅｼ  ***".
             03  E-ME29  PIC  X(030) VALUE
                  "***  マスター　単価エラー  ***".
             03  E-ME30  PIC  X(030) VALUE
                  "***  データが入っていない  ***".
             03  E-ME31  PIC  X(029) VALUE
                  "***  BB-STRAN 年月エラー  ***".
             03  E-ME90  PIC  X(021) VALUE
                  "---   キャンセル  ---".
             03  MG-03   PIC  X(024) VALUE
                  "***  ｺﾝﾄﾛｰﾙＦ未登録  ***".
             03  MG-05   PIC  X(022) VALUE
                  "***  倉庫省略不可  ***".
           COPY LSSEM.
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "330" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "15" "44" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "15" "44" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "15" "44" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "15" "44" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "15" "44" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "15" "44" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "15" "44" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "15" "26" "22" "07C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "26" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "X" "12" "24" "26" " " "C-MID1" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "220" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "12" "45" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UNC" "9" "15" "43" "1" "A-SEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UNC" BY REFERENCE W-UNCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "1" "0" "13" "A-UNC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-NGP" "9" "1" "16" "6" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-NGP" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-HMN" "9" "1" "30" "6" "AU-NGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-HMN" BY REFERENCE W-HMN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-SOK" "9" "1" "42" "1" "AU-HMN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-SOK" BY REFERENCE W-SCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "2" "0" "4" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-TCD" "9" "2" "6" "4" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-TCD" BY REFERENCE W-TCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "3" "0" "4" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-CCD" "9" "3" "6" "3" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-CCD" BY REFERENCE W-CCDD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-CSC" "9" "3" "65" "1" "AU-CCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-CSC" BY REFERENCE W-CSCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "W-L1" "0" "17" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-DC" "9" "W-L1" "4" "1" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-DC" BY REFERENCE W-DC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-HCD" "9" "W-L1" "11" "6" "AU-DC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-BIK" "X" "W-L1" "71" "10" "AU-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-BIK" BY REFERENCE W-BIK "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "W-L3" "0" "25" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-SIZ" "9" "W-L3" "1" "1" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-SIZ" BY REFERENCE W-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-SU" "S9" "W-L3" "W-C" "4" "AU-SIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-SU" BY REFERENCE W-SU(1) "3" "1" BY REFERENCE W-SC 3
            RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-SU1" "S9" "W-L3" "W-C" "3" "AU-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-SU1" BY REFERENCE W-SU(1) "3" "1" BY REFERENCE W-SC 3
            RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-SUT" "S9" "W-L3" "60" "5" "AU-SU1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-SUT" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-BT" "9" "W-L3" "67" "5" "AU-SUT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-BT" BY REFERENCE W-BT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-KIN" "S9" "W-L3" "73" "7" "AU-BT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-BI" "N" "22" "6" "48" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-BI" BY REFERENCE W-BI "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-ACP" " " "23" "0" "14" "AU-BI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-KOSU" "9" "23" "6" "3" " " "09C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-KOSU" BY REFERENCE W-KOSUR "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-HSC" "9" "23" "17" "1" "AU-KOSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-HSC" BY REFERENCE W-HSCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-FRC" "9" "23" "24" "1" "AU-HSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-FRC" BY REFERENCE W-FRCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-SDT" "9" "23" "33" "8" "AU-FRC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-SDT" BY REFERENCE W-SDTD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AU-DMM" "9" "23" "63" "1" "AU-SDT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AU-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-NGP" "9" "4" "8" "6" "09C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-NGP" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-TCD" "9" "5" "8" "4" "AN-NGP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-TCD" BY REFERENCE W-TCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-CSC" "9" "5" "75" "1" "AN-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-CSC" BY REFERENCE W-CSCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-ACP" " " "W-L1" "0" "23" "AN-CSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-HCD" "9" "W-L1" "1" "6" " " "13C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-SUT" "S9" "W-L1" "58" "5" "AN-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-SUT" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-BT" "S9" "W-L1" "65" "4" "AN-SUT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-BT" BY REFERENCE W-BT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-KIN" "S9" "W-L1" "71" "8" "AN-BT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-BI" "N" "15" "8" "48" "13C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-BI" BY REFERENCE W-BI "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-HSC" "9" "16" "12" "1" "AN-BI" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-HSC" BY REFERENCE W-HSCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-SDT" "9" "17" "8" "8" "AN-HSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-SDT" BY REFERENCE W-SDTD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "AN-DMM" "9" "20" "46" "1" "AN-SDT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "AN-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CHK" "9" "24" "55" "1" "AN-DMM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CHK" BY REFERENCE CHK "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "535" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-SNA" "N" "1" "44" "12" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "DU-SNA" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-TNA" "N" "2" "11" "52" "DU-SNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DU-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-CNA" "N" "3" "11" "52" "DU-TNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DU-CNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-L1" "0" "60" "DU-CNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-HNA" "N" "W-L1" "18" "48" " " "04C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "DU-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-SZM" "N" "W-L1" "18" "12" "DU-HNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "W-L2" "0" "188" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-S1" " " "W-L2" "44" "47" " " "05C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01DU-S1" "X" "W-L2" "3" "5" " " "DU-S1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DU-S1" "X" "W-L2" "9" "5" "01DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DU-S1" "X" "W-L2" "15" "5" "02DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DU-S1" "X" "W-L2" "21" "5" "03DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DU-S1" "X" "W-L2" "27" "5" "04DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DU-S1" "X" "W-L2" "33" "5" "05DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DU-S1" "X" "W-L2" "39" "5" "06DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DU-S1" "X" "W-L2" "45" "4" "07DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DU-S1" "X" "W-L2" "50" "4" "08DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10DU-S1" "X" "W-L2" "55" "4" "09DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-S2" " " "W-L2" "44" "47" "DU-S1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DU-S2" "X" "W-L2" "3" "5" " " "DU-S2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DU-S2" "X" "W-L2" "9" "5" "01DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DU-S2" "X" "W-L2" "15" "5" "02DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DU-S2" "X" "W-L2" "21" "5" "03DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DU-S2" "X" "W-L2" "27" "5" "04DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DU-S2" "X" "W-L2" "33" "5" "05DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DU-S2" "X" "W-L2" "39" "5" "06DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DU-S2" "X" "W-L2" "45" "4" "07DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DU-S2" "X" "W-L2" "50" "4" "08DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10DU-S2" "X" "W-L2" "55" "4" "09DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-S3" " " "W-L2" "44" "47" "DU-S2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DU-S3" "X" "W-L2" "3" "5" " " "DU-S3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DU-S3" "X" "W-L2" "9" "5" "01DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DU-S3" "X" "W-L2" "15" "5" "02DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DU-S3" "X" "W-L2" "21" "5" "03DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DU-S3" "X" "W-L2" "27" "5" "04DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DU-S3" "X" "W-L2" "33" "5" "05DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DU-S3" "X" "W-L2" "39" "5" "06DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DU-S3" "X" "W-L2" "45" "4" "07DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DU-S3" "X" "W-L2" "50" "4" "08DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10DU-S3" "X" "W-L2" "55" "4" "09DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-S4" " " "W-L2" "44" "47" "DU-S3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DU-S4" "X" "W-L2" "3" "5" " " "DU-S4" RETURNING RESU.
       CALL "SD_Init" USING 
            "02DU-S4" "X" "W-L2" "9" "5" "01DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03DU-S4" "X" "W-L2" "15" "5" "02DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04DU-S4" "X" "W-L2" "21" "5" "03DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05DU-S4" "X" "W-L2" "27" "5" "04DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06DU-S4" "X" "W-L2" "33" "5" "05DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07DU-S4" "X" "W-L2" "39" "5" "06DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08DU-S4" "X" "W-L2" "45" "4" "07DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09DU-S4" "X" "W-L2" "50" "4" "08DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10DU-S4" "X" "W-L2" "55" "4" "09DU-S4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "W-L3" "0" "28" "05C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-SU" "ZZZZ-" "W-L3" "W-C" "5" " " "06C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DU-SU" BY REFERENCE W-SU(1) "3" "1" BY REFERENCE W-SC 3
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-SU1" "ZZZ-" "W-L3" "W-C" "4" "DU-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DU-SU1" BY REFERENCE W-SU(1) "3" "1" BY REFERENCE W-SC 3
            RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-SUT" "ZZZZZ-" "W-L3" "60" "6" "DU-SU1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DU-SUT" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-BT" "ZZZZZ" "W-L3" "67" "5" "DU-SUT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DU-BT" BY REFERENCE W-BT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-KIN" "ZZZZZZZ-" "W-L3" "73" "8" "DU-BT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DU-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "22" "0" "8" "06C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-GSUT" "ZZZ,ZZZ-" "22" "58" "8" " " "07C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "DU-GSUT" BY REFERENCE W-GSUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-DSP" " " "23" "0" "3" "07C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DU-KOSU" "ZZ9" "23" "6" "3" " " "08C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "DU-KOSU" BY REFERENCE W-KOSUR "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DN-TNA" "N" "5" "13" "52" "08C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DN-TNA" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-DSP" " " "W-L1" "0" "69" "DN-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "DN-HNA" "N" "W-L1" "8" "48" " " "10C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "DN-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DN-SUT" "ZZZZZZ-" "W-L1" "57" "7" "DN-HNA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DN-SUT" BY REFERENCE W-SUT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DN-BT" "ZZZZ-" "W-L1" "65" "5" "DN-SUT" " " RETURNING RESU.
       CALL "SD_From" USING 
            "DN-BT" BY REFERENCE W-BT "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DN-KIN" "ZZZZZZZZ-" "W-L1" "71" "9" "DN-BT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "DN-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "DN-DKIN" " " "0" "0" "11" "10C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01DN-DKIN" "ZZ,ZZZ,ZZ9-" "14" "69" "11" " " "DN-DKIN"
            RETURNING RESU.
       CALL "SD_From" USING 
            "01DN-DKIN" BY REFERENCE W-DKIN "8" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "350" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-HNO" " " "0" "0" "36" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-HNO" " " "1" "0" "36" " " "SU-HNO" RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-DATE" "X" "1" "16" "6" " " "01SU-HNO" RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-HMN" "X" "1" "30" "6" "SU-DATE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-SOK" "X" "1" "42" "14" "SU-HMN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-HNM" "X" "1" "56" "10" "SU-SOK" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-CNA" " " "3" "0" "55" "SU-HNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-CNA" "X" "3" "6" "3" " " "SU-CNA" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-CNA" "X" "3" "11" "52" "01SU-CNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D1" " " "W-L1" "0" "65" "SU-CNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-D1" "X" "W-L1" "4" "1" " " "SU-D1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D1" "X" "W-L1" "11" "6" "01SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SU-D1" "X" "W-L1" "18" "48" "02SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-BIK" "X" "W-L1" "71" "10" "03SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D2" " " "W-L2" "0" "47" "SU-D1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-D2" "X" "W-L2" "3" "5" " " "SU-D2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D2" "X" "W-L2" "9" "5" "01SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SU-D2" "X" "W-L2" "15" "5" "02SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04SU-D2" "X" "W-L2" "21" "5" "03SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05SU-D2" "X" "W-L2" "27" "5" "04SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06SU-D2" "X" "W-L2" "33" "5" "05SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07SU-D2" "X" "W-L2" "39" "5" "06SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08SU-D2" "X" "W-L2" "45" "4" "07SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09SU-D2" "X" "W-L2" "50" "4" "08SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10SU-D2" "X" "W-L2" "55" "4" "09SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D3" " " "W-L3" "0" "67" "SU-D2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-D31" " " "W-L3" "0" "48" " " "SU-D3" RETURNING RESU.
       CALL "SD_Init" USING 
            "01SU-D31" "X" "W-L3" "1" "1" " " "SU-D31" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D31" "X" "W-L3" "3" "5" "01SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SU-D31" "X" "W-L3" "9" "5" "02SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04SU-D31" "X" "W-L3" "15" "5" "03SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05SU-D31" "X" "W-L3" "21" "5" "04SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06SU-D31" "X" "W-L3" "27" "5" "05SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07SU-D31" "X" "W-L3" "33" "5" "06SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08SU-D31" "X" "W-L3" "39" "5" "07SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09SU-D31" "X" "W-L3" "45" "4" "08SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10SU-D31" "X" "W-L3" "50" "4" "09SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11SU-D31" "X" "W-L3" "55" "4" "10SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02SU-D3" "X" "W-L3" "60" "6" "SU-D31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-BT" "X" "W-L3" "67" "5" "02SU-D3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-KIN" "X" "W-L3" "73" "8" "SU-BT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-SPC" " " "23" "0" "5" "SU-D3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-KOSU" "X" "23" "6" "3" " " "06C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-HSC" "X" "23" "17" "1" "SU-KOSU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SU-FRC" "X" "23" "24" "1" "SU-HSC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "SN-DS" " " "W-L1" "0" "75" "06C-SPC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01SN-DS" "X" "W-L1" "1" "6" " " "SN-DS" RETURNING RESU.
       CALL "SD_Init" USING 
            "02SN-DS" "X" "W-L1" "8" "48" "01SN-DS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03SN-DS" "X" "W-L1" "57" "7" "02SN-DS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04SN-DS" "X" "W-L1" "65" "5" "03SN-DS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05SN-DS" "X" "W-L1" "71" "9" "04SN-DS" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "713" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "713" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "16" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "20" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "20" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" " " "24" "0" "22" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME6" "X" "24" "15" "16" " " "E-ME6" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME6" "9" "24" "34" "6" "01E-ME6" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME6" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" " " "24" "0" "28" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME7" "X" "24" "15" "22" " " "E-ME7" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME7" "9" "24" "40" "6" "01E-ME7" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME7" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "24" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "20" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "20" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "25" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "25" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "25" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "27" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "15" "28" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "41" "E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "17" "E-ME20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "27" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME23" "X" "24" "15" "17" "E-ME22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME24" "X" "24" "15" "26" "E-ME23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" " " "24" "0" "26" "E-ME24" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME25" "N" "24" "15" "16" " " "E-ME25" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME25" "9999/99/99" "24" "34" "10" "01E-ME25" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02E-ME25" BY REFERENCE W-SDATE "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME26" "X" "24" "15" "24" "E-ME25" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "26" "E-ME26" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" "X" "24" "15" "17" "E-ME27" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME29" "X" "24" "15" "30" "E-ME28" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "30" "E-ME29" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME31" "X" "24" "15" "29" "E-ME30" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME90" "X" "24" "15" "21" "E-ME31" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-03" "X" "24" "15" "24" "E-ME90" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "MG-05" "X" "24" "15" "22" "MG-03" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           COPY LIBCPR.
           MOVE ZERO TO W-CNG.
           MOVE D-NHNG TO W-CNGS.
           IF  W-CN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-CN
           END-IF
           IF  W-CN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-CN
           END-IF
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               PERFORM NGC-RTN THRU NGC-EX
           END-IF
           IF  W-END NOT = 0
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               CALL "SD_Output" USING "A-SEN" A-SEN "p" RETURNING RESU
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" OTHTM_PNAME1 "SHARED" BY REFERENCE OTHTM_IDLST "2"
            "OTHT-KEY" BY REFERENCE OTHT-KEY "OTHT-KEY2" BY REFERENCE
            OTHT-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HSMSF_PNAME1 "SHARED" BY REFERENCE HSMSF_IDLST "1"
            "HSMS-KEY" BY REFERENCE HSMS-KEY.
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-20
           END-IF.
       M-10.
           IF  S-DHC NOT = 0
               CALL "SD_Output" USING
                "E-ME19" E-ME19 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-15
           END-IF
           GO TO M-10.
       M-15.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-15
           END-IF
           IF  CHK = 1
               GO TO M-20
           END-IF
           IF  CHK = 5
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO M-95
           END-IF
           GO TO M-15.
       M-20.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           CALL "DB_F_Open" USING
            "OUTPUT" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-UNC "A-UNC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
      *
           MOVE ZERO TO W-NGP.
           IF  W-UNCD > 1
               GO TO M-25
           END-IF.
       M-30.
           IF  W-UNCD = 1
               GO TO M-35
           END-IF
      *
           PERFORM UAC-RTN THRU UAC-EX.
           IF  ESTAT = BTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "C-MID1" C-MID1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-SEN" A-SEN "p" RETURNING RESU
                   GO TO M-25
               ELSE
                   GO TO M-25
               END-IF
           END-IF
           IF  W-END NOT = 0
               GO TO M-95
           END-IF
           GO TO M-80.
       M-35.
           PERFORM NAC-RTN THRU NAC-EX.
           IF  ESTAT = BTB
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "C-MID1" C-MID1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-SEN" A-SEN "p" RETURNING RESU
                   GO TO M-25
               ELSE
                   GO TO M-25
               END-IF
           END-IF
           IF  W-END NOT = 0
               GO TO M-95
           END-IF.
       M-80.
           PERFORM UPD-RTN THRU UPD-EX.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           IF  W-END = 0
               GO TO M-30
           END-IF.
       M-95.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSMSF_IDLST HSMSF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE OTHTM_IDLST OTHTM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       NGC-RTN.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
       NGC-020.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 1 TO W-END
               GO TO NGC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NGC-020
           END-IF
           IF  W-SEN = 0
               GO TO NGC-040
           END-IF
           IF  W-SEN NOT = 1
               GO TO NGC-020
           END-IF
           ADD 1 TO W-CG.
           IF  W-CG = 13
               MOVE 1 TO W-CG
               ADD 1 TO W-CN
           END-IF.
       NGC-040.
           CALL "DB_F_Open" USING
            "INPUT" BBS-TRAN_PNAME1 " " BY REFERENCE BBS-TRAN_IDLST "0".
      *           READ BBS-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" BBS-TRAN_PNAME1 BY REFERENCE BBS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO NGC-060
           END-IF
           IF  BBS-CNG NOT = W-CNG
               CALL "DB_F_Close" USING
                BY REFERENCE BBS-TRAN_IDLST BBS-TRAN_PNAME1
               CALL "SD_Output" USING
                "E-ME31" E-ME31 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO NGC-EX
           END-IF.
       NGC-060.
           CALL "DB_F_Close" USING
            BY REFERENCE BBS-TRAN_IDLST BBS-TRAN_PNAME1.
       NGC-EX.
           EXIT.
      *----------  出荷　入力  -----------------------------------------
       UAC-RTN.
           IF  W-NGP NOT = ZERO
               GO TO UAC-040
           END-IF
           MOVE ZERO TO W-TCDD W-CSCD W-HNOD.
           MOVE ZERO TO W-NGP.
           MOVE DATE-02R TO W-NGPS.
       UAC-020.
           CALL "SD_Screen_Output" USING "SCHD01" RETURNING RESU.
           CALL "SD_Output" USING "AU-NGP" AU-NGP "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE AU-NGP "AU-NGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 1 TO W-END
               GO TO UAC-EX
           END-IF
           IF  ESTAT = BTB
               GO TO UAC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-020
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-02R TO W-NGPS
               CALL "SD_Output" USING "AU-NGP" AU-NGP "p" RETURNING RESU
           END-IF.
       UAC-040.
           CALL "SD_Screen_Output" USING "SCHD01" RETURNING RESU.
           CALL "SD_Output" USING "AU-NGP" AU-NGP "p" RETURNING RESU.
           MOVE ZERO TO W-ND1.
           IF  W-ND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ND
           END-IF
           IF  W-ND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ND
           END-IF
           IF  W-NGD NOT = W-CNG
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-020
           END-IF
           IF  W-PD < 1 OR > 31
               GO TO UAC-020
           END-IF
           MOVE ZERO TO W-HNOD W-HNC W-CSCD.
       UAC-060.
           CALL "SD_Accept" USING BY REFERENCE AU-HMN "AU-HMN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-060
           END-IF
           IF  W-HMN = ZERO
               CALL "SD_Output" USING "SU-HMN" SU-HMN "p" RETURNING RESU
               GO TO UAC-080
           END-IF
           MOVE SPACE TO HSMS-KEY.
           MOVE W-HMN TO HSMS-01.
      *           START HSMSF KEY NOT < HSMS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSMSF_PNAME1 "HSMS-KEY" " NOT < " HSMS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 1 TO W-HNC
               MOVE W-HMN TO W-HNOD
               GO TO UAC-080
           END-IF
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 1 TO W-HNC
               MOVE W-HMN TO W-HNOD
               GO TO UAC-080
           END-IF
           IF  W-HMN NOT = HSMS-01
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 1 TO W-HNC
               MOVE W-HMN TO W-HNOD
               GO TO UAC-080
           END-IF
           IF  HSMS-02 = 7
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-060
           END-IF
           PERFORM HMN-RTN THRU HMN-EX.
           IF  W-INV = 1
               GO TO UAC-060
           END-IF
           IF  W-EC NOT = 0
               MOVE 0 TO W-EC
               GO TO UAC-060
           END-IF
           IF  T-BC NOT = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-060
           END-IF
           MOVE 1 TO W-DSPC.
           PERFORM DSP-RTN THRU DSP-EX.
           MOVE 0 TO W-RC.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 3 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
       UAC-070.
           ADD 1 TO W-RC.
           ADD 3 TO W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC > 6
               GO TO UAC-800
           END-IF
           MOVE W-AS(W-RC) TO W-SR.
           IF  W-GNO = 0
               GO TO UAC-360
           END-IF
           IF  W-RCB = ZERO
               GO TO UAC-070
           END-IF
           IF  W-RC NOT = W-RCB
               GO TO UAC-070
           END-IF
           MOVE W-RCB TO W-RC.
           MOVE W-LB TO W-L.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           GO TO UAC-360.
       UAC-080.
           MOVE W-SCD TO WK-SCD.
           CALL "SD_Accept" USING BY REFERENCE AU-SOK "AU-SOK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-060
           END-IF
           IF  W-HMN NOT = ZERO
               IF  W-HNC = 0
                   IF  ESTAT = ADV
                       MOVE WK-SCD TO W-SCD
                       MOVE 1 TO W-ADV
                       GO TO UAC-090
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-080
           END-IF.
       UAC-090.
           IF  W-SCD = ZERO
               MOVE W-SCD TO W-SOK
               CALL "SD_Output" USING "SU-SOK" SU-SOK "p" RETURNING RESU
               GO TO UAC-100
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-SCD TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE TO JCON3-03
               CALL "SD_Output" USING
                "DU-SNA" DU-SNA "p" RETURNING RESU
               CALL "SD_Output" USING
                "MG-03" MG-03 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-080
           END-IF
           MOVE W-SCD TO W-SOK.
           CALL "SD_Output" USING "AU-SOK" AU-SOK "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-SNA" DU-SNA "p" RETURNING RESU.
       UAC-100.
           IF  W-HMN NOT = ZERO
               IF  W-HNC = 0
                   IF  ESTAT = ADV
                       GO TO UAC-280
                   END-IF
               END-IF
           END-IF.
       UAC-120.
           CALL "SD_Accept" USING BY REFERENCE AU-TCD "AU-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-120
           END-IF
           PERFORM TM-RTN THRU TM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-120
           END-IF
           IF  T-BC NOT = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-120
           END-IF.
       UAC-260.
           CALL "SD_Accept" USING BY REFERENCE AU-CCD "AU-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-260
           END-IF
           PERFORM TCM-RTN THRU TCM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-260
           END-IF
           IF  W-INV = 9
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 1 TO W-END
               GO TO UAC-EX
           END-IF.
       UAC-280.
           CALL "SD_Output" USING "AU-CSC" AU-CSC "p" RETURNING RESU.
           IF  W-CSCD = ZERO
               GO TO UAC-320
           END-IF.
       UAC-300.
           CALL "SD_Accept" USING BY REFERENCE AU-CSC "AU-CSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-300
           END-IF
           IF  W-CSCD NOT = 0 AND 9
               GO TO UAC-300
           END-IF.
       UAC-320.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 3 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE ZERO TO W-RC W-DCD W-ZC.
           MOVE 8 TO W-HSCD.
           IF (W-HMN = ZERO) OR (W-HNC NOT = 0)
               INITIALIZE W-BR
               MOVE ZERO TO W-ASR
               MOVE ALL "　" TO W-BI
           END-IF.
       UAC-340.
           MOVE 0 TO W-DEL.
           ADD 1 TO W-RC.
           ADD 3 TO W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC > 6
               GO TO UAC-760
           END-IF
           IF  W-RC = 2
               IF  W-DC = 8
                   MOVE 5 TO W-ZC
               END-IF
           END-IF
           IF  W-ZC = 5
               GO TO UAC-420
           END-IF.
       UAC-360.
           MOVE W-AS(W-RC) TO W-SR.
           MOVE W-SCD TO W-SOK.
           MOVE W-NGP TO W-DATE.
           MOVE W-TCDN TO W-TCD.
           MOVE W-TCDD TO W-TCD2.
           MOVE W-CSCD TO W-CSC.
           MOVE T-TNC TO W-TC2.
           MOVE T-FKC TO W-FKC.
           MOVE W-CCDD TO W-CCD.
       UAC-380.
           IF  W-DEL = 9
               MOVE 0 TO W-DEL
               GO TO UAC-400
           END-IF
           IF  W-HMN NOT = ZERO
               IF  W-HNC = 0
                   IF  W-HCD NOT = ZERO
                       GO TO UAC-400
                   END-IF
               END-IF
           END-IF
           MOVE W-DCD TO W-DC.
           CALL "SD_Output" USING "AU-DC" AU-DC "p" RETURNING RESU.
       UAC-400.
           MOVE W-DC TO WK-DC.
           MOVE 0 TO W-ADV.
           CALL "SD_Accept" USING BY REFERENCE AU-DC "AU-DC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF21
               MOVE W-RC TO W-RCD
               MOVE W-L TO W-LD
               GO TO UAC-430
           END-IF
           IF  ESTAT = C1 OR PF6
               MOVE 5 TO W-ZC
               GO TO UAC-420
           END-IF
           IF  ESTAT = ADV
               IF  W-HCD NOT = ZERO
                   MOVE WK-DC TO W-DC
                   CALL "SD_Output" USING
                    "AU-DC" AU-DC "p" RETURNING RESU
                   MOVE 1 TO W-ADV
                   GO TO UAC-440
               END-IF
           END-IF
           IF  ESTAT NOT = BTB
               GO TO UAC-440
           END-IF
           IF (W-HMN = ZERO) OR (W-HNC NOT = 0)
               IF  W-GNO = ZERO
                   MOVE ZERO TO W-SR
                          W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                          W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10)
                   MOVE W-SR TO W-AS(W-RC)
                   CALL "SD_Output" USING
                    "SU-D1" SU-D1 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "SU-D2" SU-D2 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "SU-D3" SU-D3 "p" RETURNING RESU
               END-IF
           END-IF.
       UAC-410.
           SUBTRACT 1 FROM W-RC.
           SUBTRACT 3 FROM W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC = ZERO
               GO TO UAC-300
           END-IF
           MOVE W-AS(W-RC) TO W-SR.
           IF  W-GNO = ZERO
               GO TO UAC-410
           END-IF
           GO TO UAC-400.
       UAC-420.
           IF  W-RC = 1
               MOVE ZERO TO W-HCD W-SIZ W-SUT W-BT W-KIN W-DC
                            W-FT W-BC1 W-BC2 W-BC3 W-SOK
                          W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                          W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10)
               MOVE SPACE TO W-BIK
               MOVE W-RC TO W-GNO
               MOVE W-SR TO W-AS(W-RC)
               CALL "SD_Output" USING
                "SU-D1" SU-D1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "SU-D3" SU-D3 "p" RETURNING RESU
               GO TO UAC-340
           END-IF
           MOVE ZERO TO W-SR.
           MOVE ZERO TO W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                        W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10).
           MOVE SPACE TO W-BIK.
           MOVE W-SR TO W-AS(W-RC).
           CALL "SD_Output" USING "SU-D1" SU-D1 "p" RETURNING RESU.
           CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU.
           CALL "SD_Output" USING "SU-D3" SU-D3 "p" RETURNING RESU.
           GO TO UAC-340.
       UAC-430.
           IF  W-RC NOT = 6
               COMPUTE CNT = W-RC + 1
               MOVE W-AS(CNT) TO W-AS(W-RC)
               ADD 1 TO W-RC
               GO TO UAC-430
           END-IF
           MOVE ZERO TO W-AS(W-RC).
           MOVE 0 TO W-DSPC.
           PERFORM DSP-RTN THRU DSP-EX.
           MOVE W-RCD TO W-RC.
           MOVE W-LD TO W-L.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE 9 TO W-DEL.
           GO TO UAC-360.
       UAC-440.
           IF  ESTAT NOT = HTB AND SKP AND ADV
               GO TO UAC-380
           END-IF
           IF  W-CSCD = 9
               IF  W-DC NOT = 8
                   GO TO UAC-380
               END-IF
           END-IF
           IF  W-DC = 8
               IF  W-CSCD NOT = 9
                   GO TO UAC-380
               END-IF
           END-IF
           IF  W-DC = 6 OR 9
               GO TO UAC-380
           END-IF
           IF  W-DC NOT = 2 AND 3 AND 6 AND 8 AND 9
               IF  W-SOK = 0
                   CALL "SD_Output" USING
                    "MG-05" MG-05 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO  TO  UAC-380
               END-IF
           END-IF
           IF  W-RC NOT = 1
               IF  W-DC = 8
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO UAC-380
               END-IF
           END-IF
           IF  W-CCDD NOT = ZERO
               IF  W-DC = 3 OR  9
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO UAC-380
               END-IF
           END-IF
           IF  W-CSC = 9
               IF  W-DC = 4
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO UAC-380
               END-IF
           END-IF
           IF  W-RC NOT = 1
               GO TO UAC-450
           END-IF
           IF  W-DC = 1 OR 2
               MOVE ZERO TO W-HNOD W-HNC
           END-IF
           MOVE W-DC TO W-DCD.
           IF  W-DC = 4  OR  7
               MOVE ZERO TO W-DCD
           END-IF.
       UAC-450.
           MOVE W-DC TO W-DCC.
           IF  W-DC = 4  OR  7
               MOVE ZERO TO W-DCC
           END-IF
           IF  W-DCD NOT = W-DCC
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-380
           END-IF
           IF  W-DC = 8
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D3" SU-D3 "p" RETURNING RESU
               MOVE ZERO TO W-ZD1 W-ZD2 W-FT
                           W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                           W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10)
               CALL "SD_Output" USING "DU-SZM" DU-SZM "p" RETURNING RESU
               MOVE 8 TO W-HSC
               GO TO UAC-700
           END-IF
           MOVE W-HCD TO W-HCDO.
       UAC-480.
           IF  W-ADV NOT = 0
               GO TO UAC-490
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-HCD "AU-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-400
           END-IF
           IF  ESTAT = C1 OR PF6
               MOVE 5 TO W-ZC
               GO TO UAC-420
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-480
           END-IF
           IF  W-HCD = ZERO
               MOVE W-HCDD TO W-HCD
           END-IF
           IF  W-HCD NOT = W-HCDO
               MOVE ZERO TO W-BT
           END-IF.
       UAC-490.
           MOVE 0 TO W-HIC.
           PERFORM HI-RTN THRU HI-EX.
           IF  W-EC NOT = ZERO
               MOVE 0 TO W-EC W-ADV
               GO TO UAC-480
           END-IF
           IF  W-DC = 3 OR 4 OR 9
               IF  HI-S4(10) NOT = 1
                   CALL "SD_Output" USING
                     "E-ME8" E-ME8 "p" RETURNING RESU
                    CALL "SD_Output" USING
                     "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 0 TO W-ADV
                   GO  TO    UAC-480
               END-IF
           END-IF
           IF  W-HCD > 999899
               MOVE ZERO TO W-SIZ W-FT
                          W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                          W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10)
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D31" SU-D31 "p" RETURNING RESU
               GO TO UAC-520
           END-IF
           IF  W-DC NOT = 3 AND 9
               GO TO UAC-550
           END-IF
           MOVE ZERO TO W-SIZ
                        W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                        W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10).
           CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU.
           CALL "SD_Output" USING "SU-D31" SU-D31 "p" RETURNING RESU.
       UAC-520.
           IF  W-ADV NOT = 0
               GO TO UAC-540
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-SUT "AU-SUT" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               MOVE W-HCD TO W-HCDO
               GO TO UAC-480
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-520
           END-IF
           IF  W-HCD < 999900
               MOVE HI-FT TO W-FT
           END-IF.
       UAC-540.
           IF  W-BT = ZERO
               PERFORM TAN-RTN THRU TAN-EX
           END-IF
           GO TO UAC-660.
       UAC-550.
           IF  W-ADV NOT = 0
               GO TO UAC-570
           END-IF.
       UAC-560.
           CALL "SD_Accept" USING BY REFERENCE AU-SIZ "AU-SIZ" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               GO TO UAC-440
           END-IF.
       UAC-570.
           IF  W-SIZ < 1 OR > 4
               MOVE 0 TO W-ADV
               GO TO UAC-560
           END-IF
           MOVE HI-FT TO W-FT.
           IF  W-SIZ = 1
               CALL "SD_Output" USING "DU-S1" DU-S1 "p" RETURNING RESU
               MOVE HI-SS1 TO W-ASD
           END-IF
           IF  W-SIZ = 2
               CALL "SD_Output" USING "DU-S2" DU-S2 "p" RETURNING RESU
               MOVE HI-SS2 TO W-ASD
           END-IF
           IF  W-SIZ = 3
               CALL "SD_Output" USING "DU-S3" DU-S3 "p" RETURNING RESU
               MOVE HI-SS3 TO W-ASD
           END-IF
           IF  W-SIZ = 4
               CALL "SD_Output" USING "DU-S4" DU-S4 "p" RETURNING RESU
               MOVE HI-SS4 TO W-ASD
           END-IF
           IF  W-ASD = ZERO OR SPACE
               MOVE 0 TO W-ADV
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-560
           END-IF
           IF  W-SIZ = 4
               IF  W-ASD = "0000000001"
                   MOVE 0 TO W-ADV
                   CALL "SD_Output" USING
                    "E-ME9" E-ME9 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO UAC-560
               END-IF
           END-IF
           IF  W-DC = 2 OR 6
               MOVE ZERO TO W-FT
           END-IF
           IF  W-BT = ZERO
               PERFORM TAN-RTN THRU TAN-EX
           END-IF
           MOVE ZERO TO W-SC W-ZC.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       UAC-580.
           ADD 1 TO W-SC.
           IF  W-SC > 8
               ADD 5 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           ELSE
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  W-SC = 11
               MOVE ZERO TO W-SC W-SUT
               GO TO UAC-630
           END-IF
           IF (W-SD(W-SC) = ZERO) OR (W-ZC = 5)
               MOVE ZERO TO W-SU(W-SC)
               GO TO UAC-610
           END-IF
           IF  W-SC = 10
               IF  W-SIZ = 4
                   MOVE ZERO TO W-SU(W-SC)
                   GO TO UAC-580
               END-IF
           END-IF.
       UAC-600.
           IF  W-ADV NOT = 0
               GO TO UAC-610
           END-IF
           IF  W-SC > 7
               CALL "SD_Accept" USING BY REFERENCE AU-SU1 "AU-SU1"
                "S9" "3" BY REFERENCE ESTAT RETURNING RESU
           ELSE
               CALL "SD_Accept" USING BY REFERENCE AU-SU "AU-SU"
                "S9" "4" BY REFERENCE ESTAT RETURNING RESU
           END-IF
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               GO TO UAC-620
           END-IF
           IF  ESTAT = C1 OR PF6
               MOVE 5 TO W-ZC
               MOVE ZERO TO W-SU(W-SC)
               MOVE 0 TO W-ADV
               GO TO UAC-610
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               MOVE 0 TO W-ADV
               GO TO UAC-600
           END-IF
           IF  W-SC > 7
               IF  W-SU(W-SC) > 999 OR < -999
                   GO TO UAC-600
               END-IF
           END-IF.
       UAC-610.
           IF  W-SC < 8
               CALL "SD_Output" USING "DU-SU" DU-SU "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DU-SU1" DU-SU1 "p" RETURNING RESU
           END-IF
           GO TO UAC-580.
       UAC-620.
           IF  W-ADV NOT = ZERO
               MOVE 0 TO W-ADV
           END-IF
           SUBTRACT 1 FROM W-SC.
           IF  W-SC > 7
               SUBTRACT 5 FROM W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           ELSE
               SUBTRACT 6 FROM W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  W-SC = ZERO
               GO TO UAC-560
           END-IF
           IF  W-SD(W-SC) = ZERO
               GO TO UAC-620
           END-IF
           IF  W-SC = 10
               IF  W-SIZ = 4
                   GO TO UAC-620
               END-IF
           END-IF
           GO TO UAC-600.
       UAC-630.
           ADD 1 TO W-SC.
           IF  W-SC < 11
               ADD W-SU(W-SC) TO W-SUT
               GO TO UAC-630
           END-IF
           MOVE ZERO TO W-ZC.
           IF  W-SUT = ZERO
               MOVE ZERO TO W-SC
               MOVE -3 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               MOVE 0 TO W-ADV
               GO TO UAC-580
           END-IF.
       UAC-660.
           CALL "SD_Output" USING "DU-SUT" DU-SUT "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-BT" DU-BT "p" RETURNING RESU.
           IF  W-DC = 7
               MOVE ZERO TO W-BT W-KIN
               CALL "SD_Output" USING "SU-BT" SU-BT "p" RETURNING RESU
               CALL "SD_Output" USING "SU-KIN" SU-KIN "p" RETURNING RESU
               GO TO UAC-720
           END-IF
           IF  W-DC = 4
               COMPUTE W-KIN = W-SUT * W-BT
               MOVE ZERO TO W-FT
               CALL "SD_Output" USING "SU-KIN" SU-KIN "p" RETURNING RESU
               CALL "SD_Output" USING "SU-BT" SU-BT "p" RETURNING RESU
               GO TO UAC-720
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-BT "AU-BT" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO UAC-670
           END-IF
           MOVE 0 TO W-ADV.
           IF  W-HCD > 999899
               GO TO UAC-520
           END-IF
           IF  W-DC = 3 OR 9
               GO TO UAC-520
           END-IF
           MOVE 11 TO W-SC.
           MOVE 60 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO UAC-620.
       UAC-670.
           IF  ESTAT NOT = HTB AND SKP AND PF4
               GO TO UAC-660
           END-IF
           IF  ESTAT = PF4
               PERFORM TAO-RTN THRU TAO-EX
           END-IF
           CALL "SD_Output" USING "DU-BT" DU-BT "p" RETURNING RESU.
           IF  W-HCD < 999900
               IF  W-BT = ZERO
                   CALL "SD_Output" USING
                    "E-ME15" E-ME15 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-HCD > 999899
               IF  W-BT = ZERO
                   CALL "SD_Output" USING
                    "E-ME15" E-ME15 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           COMPUTE W-KIN = W-SUT * W-BT.
           CALL "SD_Output" USING "DU-KIN" DU-KIN "p" RETURNING RESU.
           IF  W-KIN = ZERO
               IF  W-HCD > 999899
                   GO TO UAC-700
               END-IF
           END-IF
           GO TO UAC-720.
       UAC-700.
           CALL "SD_Accept" USING BY REFERENCE AU-KIN "AU-KIN" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-HCD > 999899
                   GO  TO  UAC-660
               ELSE
                   GO TO UAC-400
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-700
           END-IF
           CALL "SD_Output" USING "DU-KIN" DU-KIN "p" RETURNING RESU.
       UAC-720.
           IF  T-BIK = 0
               MOVE SPACE TO W-BIK
               CALL "SD_Output" USING "SU-BIK" SU-BIK "p" RETURNING RESU
               GO TO UAC-740
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-BIK "AU-BIK" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-DC = 4 OR 7
                   GO TO UAC-620
               ELSE
                   GO TO UAC-660
               END-IF
           END-IF.
       UAC-740.
           MOVE W-RC TO W-GNO.
           MOVE W-SR TO W-AS(W-RC).
           GO TO UAC-340.
       UAC-760.
           MOVE ZERO TO W-GSUT W-RC W-ZC.
       UAC-770.
           ADD 1 TO W-RC.
           IF  W-RC = 7
               CALL "SD_Output" USING
                "DU-GSUT" DU-GSUT "p" RETURNING RESU
               IF  W-ZC = 0
                   CALL "SD_Output" USING
                    "E-ME30" E-ME30 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO UAC-780
               ELSE
                   GO TO UAC-780
               END-IF
           END-IF
           MOVE ZERO TO W-SR.
           MOVE W-AS(W-RC) TO W-SR.
           IF  W-HCD < 999900
               ADD W-SUT TO W-GSUT
           END-IF
           IF  W-DATE NOT = ZERO
               MOVE W-RC TO W-HNCD
           END-IF
           IF  ZERO = W-SU(01) AND W-SU(02) AND W-SU(03) AND W-SU(04)
                  AND W-SU(05) AND W-SU(06) AND W-SU(07) AND W-SU(08)
                  AND W-SU(09) AND W-SU(10) AND W-SUT AND W-KIN
               GO TO UAC-770
           END-IF
           MOVE 1 TO W-ZC.
           GO TO UAC-770.
       UAC-780.
           IF  W-BI = SPACE
               CALL "SD_Output" USING "AU-BI" AU-BI "p" RETURNING RESU
           END-IF
           IF  W-HNCD < W-HNC
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF.
       UAC-800.
           MOVE 0 TO W-ADV.
           CALL "SD_Output" USING "AU-BI" AU-BI "p" RETURNING RESU.
           MOVE W-BI TO WK-BI.
           CALL "SD_Accept" USING BY REFERENCE AU-BI "AU-BI" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 7 TO W-RC
               MOVE 22 TO W-L1
               CALL "SD_Arg_Match_Line" USING
                "W-L1" "2" W-L1 RETURNING RESU
               MOVE 23 TO W-L2
               CALL "SD_Arg_Match_Line" USING
                "W-L2" "2" W-L2 RETURNING RESU
               MOVE 24 TO W-L3
               CALL "SD_Arg_Match_Line" USING
                "W-L3" "2" W-L3 RETURNING RESU
               GO TO UAC-410
           END-IF
           IF  ESTAT = ADV
               MOVE WK-BI TO W-BI
               CALL "SD_Output" USING "AU-BI" AU-BI "p" RETURNING RESU
               MOVE 1 TO W-ADV
               GO TO UAC-810
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-780
           END-IF.
       UAC-810.
           MOVE W-HNOD TO W-HANO.
       UAC-840.
           IF  W-DCD = 3
               MOVE ZERO TO W-KOSUR
               GO TO UAC-850
           END-IF
           IF  W-ADV NOT = 0
               GO TO UAC-850
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-KOSU "AU-KOSU" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-800
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-840
           END-IF.
       UAC-850.
           CALL "SD_Output" USING "DU-KOSU" DU-KOSU "p" RETURNING RESU.
      *
           IF  W-DCD = 8
               MOVE 8 TO W-HSCD
               MOVE 0 TO W-FRCD
               CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU
               CALL "SD_Output" USING "SU-FRC" SU-FRC "p" RETURNING RESU
               GO TO UAC-940
           END-IF
           IF  T-ZEI = 1
               MOVE 0 TO W-HSCD
               CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU
               GO TO UAC-920
           END-IF
           IF  T-FKC = 99
               MOVE 0 TO W-HSCD
               CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU
               GO TO UAC-920
           END-IF
           IF  W-DCD = 0 OR 1 OR 2 OR 5
               MOVE 8 TO W-HSCD
               CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU
               GO TO UAC-920
           END-IF
           IF  W-DCD = 3
               MOVE ZERO TO W-FRCD
               MOVE 8 TO W-HSCD
               CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU
               CALL "SD_Output" USING "SU-FRC" SU-FRC "p" RETURNING RESU
               GO TO UAC-940
           END-IF.
       UAC-880.
           IF  W-DCD = 9
               MOVE 0 TO W-HSCD
               MOVE 0 TO W-FRCD
               CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU
               CALL "SD_Output" USING "SU-FRC" SU-FRC "p" RETURNING RESU
               GO TO UAC-940
           END-IF
           IF  W-DCD = 7
               MOVE 0 TO W-HSCD
               GO TO UAC-920
           END-IF
           IF  W-ADV NOT = 0
               GO TO UAC-890
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-HSC "AU-HSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               IF  W-DCD = 3
                   GO TO UAC-800
               ELSE
                   GO TO UAC-840
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-880
           END-IF.
       UAC-890.
           IF  W-HSCD NOT = 0 AND 1 AND 3 AND 5 AND 8
               GO TO UAC-880
           END-IF.
       UAC-920.
           IF  W-DCD NOT = 0 AND 1 AND 2 AND 4 AND 5 AND 7
               MOVE 0 TO W-FRCD
               CALL "SD_Output" USING "SU-FRC" SU-FRC "p" RETURNING RESU
               GO TO UAC-940
           END-IF
           IF  W-ADV NOT = 0
               GO TO UAC-930
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AU-FRC "AU-FRC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-DCD = 7
                   MOVE 0 TO W-ADV
                   GO TO UAC-840
               END-IF
           END-IF
           IF  ESTAT = BTB
               MOVE 0 TO W-ADV
               GO TO UAC-880
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-920
           END-IF.
       UAC-930.
           CALL "SD_Output" USING "AU-FRC" AU-FRC "p" RETURNING RESU.
           IF  W-FRCD > 1
               GO TO UAC-920
           END-IF.
       UAC-940.
           CALL "SD_Output" USING "AU-SDT" AU-SDT "p" RETURNING RESU.
           IF  W-SDTD NOT = ZERO
               GO TO UAC-980
           END-IF
           IF  T-SS = ZERO OR 99
               GO TO UAC-990
           END-IF.
       UAC-960.
           CALL "SD_Accept" USING BY REFERENCE AU-SDT "AU-SDT" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO UAC-970
           END-IF
           MOVE 0 TO W-ADV.
           IF  W-DCD = 8
               GO TO UAC-840
           END-IF
           IF  W-DCD = 9
               IF  W-GSUT > ZERO
                   GO TO UAC-840
               END-IF
           END-IF
           IF  W-DCD = 9
               GO TO UAC-840
           END-IF
           IF  W-DCD = 5 OR 0 OR 4 OR 7
               GO TO UAC-920
           END-IF
           GO TO UAC-880.
       UAC-970.
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-960
           END-IF
           IF  T-SS = ZERO OR 99
               IF  W-SDTD NOT = ZERO
                   GO TO UAC-960
               END-IF
           END-IF
           IF  W-DCD NOT = 3
               IF  W-SDTD = 99999999
                   GO TO UAC-960
               END-IF
           END-IF
           IF  W-SDTD = ZERO OR 99999999
               GO TO UAC-990
           END-IF
           IF  W-SDTG < 1 OR > 12
               GO TO UAC-960
           END-IF
           IF  W-SDTP < 1 OR > 31
               GO TO UAC-960
           END-IF
           IF  W-SDTN NOT = W-ND AND (W-ND + 1)
               GO TO UAC-960
           END-IF.
       UAC-980.
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SDTD
                   CALL "SD_Output" USING
                    "E-ME25" E-ME25 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO UAC-960
               END-IF
           END-IF
           IF  W-NGP > W-SDTD
               GO TO UAC-960
           END-IF.
       UAC-990.
           CALL "SD_Accept" USING BY REFERENCE AU-DMM "AU-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO UAC-960
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO UAC-990
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO UAC-040
           END-IF
           IF  W-DMM NOT = 1
               GO TO UAC-990
           END-IF.
       UAC-EX.
           EXIT.
      *---------------  値引入力  --------------------------------------
       NAC-RTN.
           IF  W-NGP NOT = ZERO
               GO TO NAC-040
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHD11" RETURNING RESU.
       NAC-020.
           CALL "SD_Accept" USING BY REFERENCE AN-NGP "AN-NGP" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 1 TO W-END
               GO TO NAC-EX
           END-IF
           IF  ESTAT = BTB
               GO TO NAC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-020
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-02R TO W-NGPS
               CALL "SD_Output" USING "AN-NGP" AN-NGP "p" RETURNING RESU
           END-IF
           IF  W-GD > 12 OR < 1
               GO TO NAC-020
           END-IF
           IF  W-PD > 31 OR < 1
               GO TO NAC-020
           END-IF
           MOVE ZERO TO W-ND1.
           IF  W-ND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-ND
           END-IF
           IF  W-ND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-ND
           END-IF
           IF  W-CNG NOT = W-NGD
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO NAC-020
           END-IF.
       NAC-040.
           CALL "SD_Screen_Output" USING "SCHD11" RETURNING RESU.
           CALL "SD_Output" USING "AN-NGP" AN-NGP "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE AN-TCD "AN-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               MOVE 1 TO W-END
               GO TO NAC-EX
           END-IF
           IF  ESTAT = BTB
               GO TO NAC-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-040
           END-IF
           PERFORM TM-RTN THRU TM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO NAC-040
           END-IF
           IF  T-BC NOT = ZERO
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO NAC-040
           END-IF.
       NAC-060.
           CALL "SD_Accept" USING BY REFERENCE AN-CSC "AN-CSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-060
           END-IF
           IF  W-CSCD NOT = 0 AND 9
               GO TO NAC-060
           END-IF.
       NAC-080.
           MOVE ZERO TO W-SR W-BR.
           MOVE SPACE TO W-BI.
           MOVE ZERO TO W-RC W-ASR.
           MOVE 8 TO W-HSCD.
           MOVE 7 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHD11" RETURNING RESU.
           CALL "SD_Output" USING "AN-NGP" AN-NGP "p" RETURNING RESU.
           CALL "SD_Output" USING "AN-TCD" AN-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "DN-TNA" DN-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "AN-CSC" AN-CSC "p" RETURNING RESU.
       NAC-100.
           ADD 1 TO W-RC W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  W-RC = 7
               GO TO NAC-260
           END-IF
           MOVE W-NGP TO W-DATE.
           MOVE W-TCDN TO W-TCD.
           MOVE W-CSCD TO W-CSC.
       NAC-120.
           CALL "SD_Accept" USING BY REFERENCE AN-HCD "AN-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = C1 OR PF6
               GO TO NAC-240
           END-IF
           IF  ESTAT NOT = BTB
               GO TO NAC-140
           END-IF
           IF  W-RC = 1
               GO TO NAC-040
           END-IF
           SUBTRACT 1 FROM W-RC W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE W-AS(W-RC) TO W-SR.
           IF  W-HCD = 999977 OR 999991 OR 999992 OR 999993
               GO TO NAC-160
           END-IF
           GO TO NAC-200.
       NAC-140.
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-120
           END-IF
           MOVE 0 TO W-HIC.
           PERFORM HI-RTN THRU HI-EX.
           IF  W-EC NOT = ZERO
               MOVE 0 TO W-EC
               GO TO NAC-120
           END-IF
           MOVE T-TNC TO W-TC2.
           MOVE T-FKC TO W-FKC.
       NAC-160.
           IF  W-HCD NOT = 999977 AND 999991 AND 999992 AND 999993
               GO TO NAC-180
           END-IF
           CALL "SD_Accept" USING BY REFERENCE AN-KIN "AN-KIN" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-160
           END-IF
           MOVE ZERO TO W-SUT W-BT.
           CALL "SD_Output" USING "DN-SUT" DN-SUT "p" RETURNING RESU.
           CALL "SD_Output" USING "DN-BT" DN-BT "p" RETURNING RESU.
           CALL "SD_Output" USING "DN-KIN" DN-KIN "p" RETURNING RESU.
           GO TO NAC-220.
       NAC-180.
           CALL "SD_Accept" USING BY REFERENCE AN-SUT "AN-SUT" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-180
           END-IF
           CALL "SD_Output" USING "DN-SUT" DN-SUT "p" RETURNING RESU.
           IF  W-SUT < 0
               GO TO NAC-180
           END-IF.
       NAC-200.
           CALL "SD_Accept" USING BY REFERENCE AN-BT "AN-BT" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-200
           END-IF
           COMPUTE W-KIN = W-SUT * W-BT.
           CALL "SD_Output" USING "DN-BT" DN-BT "p" RETURNING RESU.
           CALL "SD_Output" USING "DN-KIN" DN-KIN "p" RETURNING RESU.
       NAC-220.
           MOVE W-RC TO W-GNO.
           MOVE ZERO TO W-SU(01) W-SU(02) W-SU(03) W-SU(04) W-SU(05)
                        W-SU(06) W-SU(07) W-SU(08) W-SU(09) W-SU(10).
           MOVE W-SR TO W-AS(W-RC).
           GO TO NAC-100.
       NAC-240.
           MOVE ZERO TO W-SR.
           MOVE W-SR TO W-AS(W-RC).
           CALL "SD_Output" USING "SN-DS" SN-DS "p" RETURNING RESU.
           ADD 1 TO W-RC W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  W-RC NOT = 7
               GO TO NAC-240
           END-IF.
       NAC-260.
           MOVE ZERO TO W-DKIN CNT.
       NAC-280.
           ADD 1 TO CNT.
           IF  CNT = 7
               CALL "SD_Output" USING
                "DN-DKIN" DN-DKIN "p" RETURNING RESU
               GO TO NAC-300
           END-IF
           MOVE W-AS(CNT) TO W-SR.
           ADD W-KIN TO W-DKIN.
           GO TO NAC-280.
       NAC-300.
           CALL "SD_Output" USING "AN-BI" AN-BI "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE AN-BI "AN-BI" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-360
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-300
           END-IF
           IF  T-ZEI = 1
               MOVE 0 TO W-HSCD
           END-IF
           IF  T-FKC = 99
               MOVE 0 TO W-HSCD
           END-IF
           CALL "SD_Output" USING "AN-HSC" AN-HSC "p" RETURNING RESU.
           CALL "SD_Output" USING "AN-SDT" AN-SDT "p" RETURNING RESU.
           GO TO NAC-340.
       NAC-320.
           CALL "SD_Accept" USING BY REFERENCE AN-HSC "AN-HSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-320
           END-IF
           IF  W-HSCD NOT = 0 AND 1 AND 3 AND 5 AND 8
               GO TO NAC-320
           END-IF
           IF  T-SS = 00 OR 99
               MOVE ZERO TO W-SDTD
               CALL "SD_Output" USING "AN-SDT" AN-SDT "p" RETURNING RESU
               GO TO NAC-340
           END-IF.
       NAC-330.
           CALL "SD_Accept" USING BY REFERENCE AN-SDT "AN-SDT" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NAC-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-330
           END-IF
           IF  W-SDTD = ZERO
               GO TO NAC-340
           END-IF
           IF  W-SDTG < 1 OR > 12
               GO TO NAC-330
           END-IF
           IF  W-SDTP < 1 OR > 31
               GO TO NAC-330
           END-IF
           IF  W-SDTN NOT = W-ND AND (W-ND + 1)
               GO TO NAC-330
           END-IF
           IF  W-NGP > W-SDTD
               GO TO NAC-330
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SDTD
                   CALL "SD_Output" USING
                    "E-ME25" E-ME25 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO NAC-330
               END-IF
           END-IF.
       NAC-340.
           CALL "SD_Accept" USING BY REFERENCE AN-DMM "AN-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  T-SS = 00 OR 99
                   GO TO NAC-320
               ELSE
                   GO TO NAC-330
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NAC-340
           END-IF
           IF  W-DMM = 1
               GO TO NAC-EX
           END-IF
           IF  W-DMM = 9
               GO TO NAC-040
           END-IF
           GO TO NAC-340.
       NAC-360.
           IF  W-RC = 1
               GO TO NAC-080
           END-IF
           SUBTRACT 1 FROM W-RC.
           MOVE W-AS(W-RC) TO W-SR.
           SUBTRACT 1 FROM W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           IF  W-HCD = ZERO
               GO TO NAC-360
           END-IF
           IF  W-HCD = 999977 OR 999991 OR 999992 OR 999993
               GO TO NAC-160
           END-IF
           GO TO NAC-200.
       NAC-EX.
           EXIT.
      *-----------------------------------------------------------------
       TAN-RTN.
           MOVE 0 TO WK-TAN.
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD TO THT-HCD.
           MOVE W-SIZ TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           IF  HI-MHCD = HI-HCD
               GO TO TAN-090
           END-IF
           MOVE W-TCD TO THT-TCD.
           MOVE HI-MHCD TO THT-HCD.
           MOVE W-SIZ TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAN-090
           END-IF
           MOVE W-TCD TO THT-TCD.
           MOVE HI-MHCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO THT-T
           END-IF
           MOVE THT-T TO WK-TAN.
       TAN-090.
           IF  WK-TAN NOT = ZERO
               MOVE WK-TAN TO W-BT
               GO TO TAN-EX
           END-IF.
       TAN-EX.
           EXIT.
      *
       TAO-RTN.
           MOVE 0 TO WK-TAN.
           MOVE W-TCD TO OTHT-TCD.
           MOVE W-HCD TO OTHT-HCD.
           MOVE W-SIZ TO OTHT-SIZ.
      *           READ OTHTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OTHTM_PNAME1 BY REFERENCE OTHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO OTHT-T
           END-IF
           MOVE OTHT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAO-090
           END-IF
           MOVE W-TCD TO OTHT-TCD.
           MOVE W-HCD TO OTHT-HCD.
           MOVE 9 TO OTHT-SIZ.
      *           READ OTHTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OTHTM_PNAME1 BY REFERENCE OTHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO OTHT-T
           END-IF
           MOVE OTHT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAO-090
           END-IF
           IF  HI-MHCD = HI-HCD
               GO TO TAO-090
           END-IF
           MOVE W-TCD TO OTHT-TCD.
           MOVE HI-MHCD TO OTHT-HCD.
           MOVE W-SIZ TO OTHT-SIZ.
      *           READ OTHTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OTHTM_PNAME1 BY REFERENCE OTHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO OTHT-T
           END-IF
           MOVE OTHT-T TO WK-TAN.
           IF  WK-TAN NOT = ZERO
               GO TO TAO-090
           END-IF
           MOVE W-TCD TO OTHT-TCD.
           MOVE HI-MHCD TO OTHT-HCD.
           MOVE 9 TO OTHT-SIZ.
      *           READ OTHTM UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" OTHTM_PNAME1 BY REFERENCE OTHT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO OTHT-T
           END-IF
           MOVE OTHT-T TO WK-TAN.
       TAO-090.
           IF  WK-TAN NOT = ZERO
               MOVE WK-TAN TO W-BT
               GO TO TAO-EX
           END-IF.
       TAO-EX.
           EXIT.
      *-----------------------------------------------------------------
       SD-RTN.
           CALL "DB_F_Close" USING
            BY REFERENCE S-TRAN_IDLST S-TRAN_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" S-TRAN_PNAME1 " " BY REFERENCE S-TRAN_IDLST "0".
       SD-020.
      *           READ S-TRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" S-TRAN_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO SD-EX
           END-IF
           IF  W-DNO NOT = S-DNO
               GO TO SD-020
           END-IF
           MOVE X"FF" TO S-R.
      *           REWRITE S-R.
      *///////////////
           CALL "DB_Update" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO SD-020.
       SD-EX.
           EXIT.
      *-----------------------------------------------------------------
       HMN-RTN.
           INITIALIZE W-BR.
           MOVE ZERO TO W-ASR.
           MOVE SPACE TO W-BI.
           MOVE ZERO TO W-DCD.
           MOVE HSMS-061 TO W-TCDD.
           PERFORM TM-RTN THRU TM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HMN-EX
           END-IF.
       HMN-020.
           IF  HSMS-02 = 7
               MOVE HSMS-15 TO W-BI
               GO TO HMN-035
           END-IF
           IF  HSMS-13 = 4 OR 5
               MOVE 4 TO HSMS-03
           END-IF
           MOVE ZERO TO W-SR.
           MOVE SPACE TO W-BIK.
           MOVE HSMS-02 TO W-GNO.
           MOVE W-NGP TO W-DATE.
           MOVE HSMS-03 TO W-DC.
           MOVE W-TCDN TO W-TCD.
           MOVE W-TCDD TO W-TCD2.
           MOVE HSMS-09 TO W-HCD.
           MOVE HSMS-10 TO W-SIZ.
           MOVE HSMS-1211(01) TO W-SU(01).
           MOVE HSMS-1211(02) TO W-SU(02).
           MOVE HSMS-1211(03) TO W-SU(03).
           MOVE HSMS-1211(04) TO W-SU(04).
           MOVE HSMS-1211(05) TO W-SU(05).
           MOVE HSMS-1211(06) TO W-SU(06).
           MOVE HSMS-1211(07) TO W-SU(07).
           MOVE HSMS-1211(08) TO W-SU(08).
           MOVE HSMS-1211(09) TO W-SU(09).
           MOVE HSMS-1211(10) TO W-SU(10).
           MOVE HSMS-122 TO W-SUT.
           MOVE HSMS-07 TO W-SOK.
           MOVE HSMS-14 TO W-KOSU.
           MOVE HSMS-062 TO W-CCD.
           MOVE HSMS-22 TO W-BIK.
           MOVE HSMS-24 TO W-USC.
           MOVE 1 TO W-HIC.
           PERFORM HI-RTN THRU HI-EX.
           PERFORM TAN-RTN THRU TAN-EX.
           COMPUTE W-KIN = W-SUT * W-BT.
           IF  W-DC = 2 OR 6
               MOVE ZERO TO W-FT
           ELSE
               MOVE HI-FT TO W-FT
           END-IF
           MOVE T-TNC TO W-TC2.
           MOVE T-FKC TO W-FKC.
           MOVE W-SR TO W-AS(HSMS-02).
       HMN-035.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO HMN-040
           END-IF
           IF  W-HMN = HSMS-01
               GO TO HMN-020
           END-IF.
       HMN-040.
           MOVE W-SOK TO W-SCD.
           MOVE W-CCD TO W-CCDD.
           MOVE W-KOSU TO W-KOSUR.
           MOVE ZERO TO W-FRCD.
           MOVE 8 TO W-HSCD.
       HMN-060.
           MOVE 3 TO JCON3-01.
           MOVE W-SCD TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE  SPACE TO JCON3-03
           END-IF
           MOVE W-SCD TO W-SOK.
           CALL "SD_Output" USING "AU-SOK" AU-SOK "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-SNA" DU-SNA "p" RETURNING RESU.
      *
           IF  W-DC = 1 OR 2
               MOVE W-DC TO W-DCD
               MOVE ZERO TO W-HNOD
           ELSE
               MOVE W-HMN TO W-HNOD
           END-IF
           PERFORM TCM-RTN THRU TCM-EX.
           IF  W-INV = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO HMN-EX
           END-IF
           MOVE ZERO TO W-FRCD.
           MOVE 8 TO W-HSCD.
           CALL "SD_Output" USING "AU-BI" AU-BI "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-KOSU" DU-KOSU "p" RETURNING RESU.
           CALL "SD_Output" USING "AU-HSC" AU-HSC "p" RETURNING RESU.
           CALL "SD_Output" USING "AU-FRC" AU-FRC "p" RETURNING RESU.
       HMN-EX.
           EXIT.
      *-----------------------------------------------------------------
       DSP-RTN.
           MOVE 1 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           MOVE 2 TO W-L2.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           MOVE 3 TO W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE ZERO TO W-RC W-ZC W-GSUT W-RCB W-LB.
       DSP-020.
           ADD 1 TO W-RC.
           ADD 3 TO W-L1 W-L2 W-L3.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-RC > 6
               GO TO DSP-080
           END-IF
      *
           INITIALIZE W-SR.
           MOVE W-AS(W-RC) TO W-SR.
           IF  W-HCD = ZERO
               CALL "SD_Output" USING "SU-D1" SU-D1 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D3" SU-D3 "p" RETURNING RESU
               GO TO DSP-020
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           CALL "SD_Output" USING "AU-DC" AU-DC "p" RETURNING RESU.
           CALL "SD_Output" USING "AU-HCD" AU-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-HNA" DU-HNA "p" RETURNING RESU.
           IF  W-HCD >= 999900
               CALL "SD_Output" USING "SU-D2" SU-D2 "p" RETURNING RESU
               CALL "SD_Output" USING "SU-D31" SU-D31 "p" RETURNING RESU
               GO TO DSP-060
           END-IF
           CALL "SD_Output" USING "AU-SIZ" AU-SIZ "p" RETURNING RESU.
           IF  W-SIZ = 1
               CALL "SD_Output" USING "DU-S1" DU-S1 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 2
               CALL "SD_Output" USING "DU-S2" DU-S2 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 3
               CALL "SD_Output" USING "DU-S3" DU-S3 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 4
               CALL "SD_Output" USING "DU-S4" DU-S4 "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-SC.
           MOVE -3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       DSP-040.
           ADD 1 TO W-SC.
           IF  W-SC > 8
               ADD 5 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           ELSE
               ADD 6 TO W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
           END-IF
           IF  W-SC = 11
               GO TO DSP-060
           END-IF
           IF  W-SC < 8
               CALL "SD_Output" USING "DU-SU" DU-SU "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DU-SU1" DU-SU1 "p" RETURNING RESU
           END-IF
           GO TO DSP-040.
       DSP-060.
           CALL "SD_Output" USING "DU-SUT" DU-SUT "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-BT" DU-BT "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-KIN" DU-KIN "p" RETURNING RESU.
           CALL "SD_Output" USING "AU-BIK" AU-BIK "p" RETURNING RESU.
           IF  W-DC = 4
               MOVE ZERO TO W-FT
               CALL "SD_Output" USING "SU-KIN" SU-KIN "p" RETURNING RESU
               CALL "SD_Output" USING "SU-BT" SU-BT "p" RETURNING RESU
           END-IF
           IF  W-HCD < 999900
               ADD W-SUT TO W-GSUT
           END-IF
           IF  W-DC NOT = 7
               IF  W-BT = ZERO
                   CALL "SD_Output" USING
                    "E-ME15" E-ME15 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   IF  W-DSPC = 1
                       IF  W-RCB = ZERO
                           MOVE W-RC TO W-RCB
                           MOVE W-L TO W-LB
                       END-IF
                   END-IF
               END-IF
           END-IF
           GO TO DSP-020.
       DSP-080.
           CALL "SD_Output" USING "DU-GSUT" DU-GSUT "p" RETURNING RESU.
       DSP-EX.
           EXIT.
      *-----------------------------------------------------------------
       TM-RTN.
           MOVE 0 TO W-INV.
           MOVE W-TCDD TO W-TCDN.
           IF  W-TCDN = 0460
               MOVE 0458 TO W-TCDN
           END-IF
           MOVE W-TCDN TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
               MOVE 1 TO W-INV
           END-IF
           IF  W-UNCD = 0
               CALL "SD_Output" USING "AU-TCD" AU-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "DU-TNA" DU-TNA "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "AN-TCD" AN-TCD "p" RETURNING RESU
               CALL "SD_Output" USING "DN-TNA" DN-TNA "p" RETURNING RESU
           END-IF
      *
           MOVE W-TCDN TO TSK-KEY.
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
               MOVE TSK-ZNGP(5) TO W-SDATE
           END-IF
      *
           MOVE ZERO TO W-SDTD.
           MOVE W-NGP TO W-SDTD.
       TM-020.
           MOVE T-SS TO W-SDTP.
           IF  W-SDTP = 00 OR 99
               MOVE ZERO TO W-SDTD
               GO TO TM-EX
           END-IF
           IF  W-SDTP = 30 OR 31
               IF  W-SDTG = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   MOVE 31 TO W-SDTP
               ELSE
                   IF  W-SDTG = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO W-SDTP
                   ELSE
                       DIVIDE 4 INTO W-SDTN GIVING W-DTW1
                                                 REMAINDER W-DTW2
                       IF  W-DTW2 = 0
                           MOVE 29 TO W-SDTP
                       ELSE
                           MOVE 28 TO W-SDTP
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-NGP > W-SDTD
               IF  W-GD NOT = W-SDTG
                   MOVE ZERO TO W-SDTD
                   GO TO TM-EX
               ELSE
                   ADD 1 TO W-SDTG
                   GO TO TM-040
               END-IF
           END-IF
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SDTD
                   ADD 1 TO W-SDTG
                   GO TO TM-040
               END-IF
           END-IF
           GO TO TM-EX.
       TM-040.
           IF  W-SDTG = 13
               MOVE 1 TO W-SDTG
               ADD 1 TO W-SDTN
           END-IF
           GO TO TM-020.
       TM-EX.
           EXIT.
      *-----------------------------------------------------------------
       TCM-RTN.
           IF  W-CCDD = ZERO
               CALL "SD_Output" USING "SU-CNA" SU-CNA "p" RETURNING RESU
               GO TO TCM-EX
           END-IF
           MOVE 0 TO W-INV.
           MOVE W-TCDD TO TC-TCD.
           MOVE W-CCDD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE 1 TO W-INV
           END-IF
           CALL "SD_Output" USING "AU-CCD" AU-CCD "p" RETURNING RESU.
           CALL "SD_Output" USING "DU-CNA" DU-CNA "p" RETURNING RESU.
           MOVE 9999999 TO TC-KEY.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-INV
           END-IF.
       TCM-EX.
           EXIT.
      *-----------------------------------------------------------------
       HI-RTN.
           MOVE W-HCD TO W-HCDD.
           IF  W-HIC = 0
               IF  W-UNCD = 0
                   CALL "SD_Output" USING
                    "AU-HCD" AU-HCD "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "AN-HCD" AN-HCD "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-HCD = ZERO
               MOVE 9 TO W-EC
               GO TO HI-EX
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-EC
               GO TO HI-EX
           END-IF
           IF  W-HIC = 0
               IF  W-UNCD = 0
                   CALL "SD_Output" USING
                    "DU-HNA" DU-HNA "p" RETURNING RESU
               ELSE
                   CALL "SD_Output" USING
                    "DN-HNA" DN-HNA "p" RETURNING RESU
               END-IF
           END-IF
           MOVE HI-BC1 TO W-BC1.
           MOVE HI-BC2 TO W-BC2.
           MOVE HI-BC3 TO W-BC3.
           MOVE HI-BMC TO W-BMC.
           MOVE HI-BMNO TO W-BMNO.
           IF  W-UNCD NOT = 0
               GO TO HI-EX
           END-IF
           IF  W-HCD < 999900
               IF  HI-FT = ZERO
                   CALL "SD_Output" USING
                    "E-ME7" E-ME7 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-DC NOT = 0 AND 3 AND 4 AND 5 AND 7
               IF  W-HCD < 999900
                   IF  HI-FT = 1
                       CALL "SD_Output" USING
                        "E-ME7" E-ME7 "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME98" E-ME98 "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
      *
           MOVE W-HCD TO HUH-KEY.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO HI-EX
           END-IF
           IF (HUH-ZK NOT = ZERO) AND (HUH-ZS NOT = ZERO)
               COMPUTE W-FTD = HUH-ZK / HUH-ZS
               IF  HI-FT NOT = W-FTD
                   MOVE 9 TO W-EC
                   CALL "SD_Output" USING
                    "E-ME29" E-ME29 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO HI-EX
               END-IF
           END-IF
           IF (HUH-NK NOT = ZERO) AND (HUH-NS NOT = ZERO)
               COMPUTE W-FTD = HUH-NK / HUH-NS
               IF  HI-FT NOT = W-FTD
                   MOVE 9 TO W-EC
                   CALL "SD_Output" USING
                    "E-ME29" E-ME29 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO HI-EX
               END-IF
           END-IF
           IF (HUH-UG NOT = ZERO) AND (HUH-SS NOT = ZERO)
               COMPUTE W-FTD = HUH-UG / HUH-SS
               IF  HI-FT NOT = W-FTD
                   MOVE 9 TO W-EC
                   CALL "SD_Output" USING
                    "E-ME29" E-ME29 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
                   GO TO HI-EX
               END-IF
           END-IF
           IF (HUH-YK NOT = ZERO) AND (HUH-YS NOT = ZERO)
               COMPUTE W-FTD = HUH-YK / HUH-YS
               IF  HI-FT NOT = W-FTD
                   MOVE 9 TO W-EC
                   CALL "SD_Output" USING
                    "E-ME29" E-ME29 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF.
       HI-EX.
           EXIT.
      *-----------------------------------------------------------------
       UPD-RTN.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE SPACE TO HKB-KEY.
           MOVE "05" TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO UPD-EX
           END-IF
           MOVE HKB-UNN TO W-DNOD.
           ADD 1 TO W-DNOD.
           IF  W-DNOD = ZERO
               MOVE 1 TO W-DNOD
           END-IF
           MOVE W-DNOD TO HKB-UNN.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME24" E-ME24 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 255
               GO TO UPD-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
      *
           MOVE ZERO TO W-RC.
       UPD-020.
           ADD 1 TO W-RC.
           IF  W-RC > 6
               GO TO UPD-180
           END-IF
           MOVE W-AS(W-RC) TO W-SR.
           IF  W-DATE = ZERO
               GO TO UPD-180
           END-IF
           MOVE W-DNOD TO W-DNO.
           MOVE W-SDTD TO W-SDT.
           MOVE W-HSCD TO W-HSC.
           IF  W-UNCD = 0
               MOVE W-KOSUR TO W-KOSU
               MOVE W-FRCD TO W-FRC
           END-IF
           MOVE W-UNCD TO W-UNC.
           MOVE W-SR TO S-R.
      *           WRITE S-R.
      *//////////////
           CALL "DB_Insert" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO UPD-040
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME16" E-ME16 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO UPD-900.
       UPD-040.
           IF  W-UNCD = 1
               GO TO UPD-020
           END-IF
           IF  W-HCD = ZERO
               GO TO UPD-180
           END-IF
           IF  W-DC NOT = 0 AND 3
               GO TO UPD-020
           END-IF
           IF  ZERO = W-TCD OR W-HCD
               GO TO UPD-020
           END-IF
           IF  W-BT = ZERO
               GO TO UPD-020
           END-IF
           MOVE SPACE TO THT-KEY.
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-120
           END-IF
           GO TO UPD-160.
       UPD-120.
           MOVE SPACE TO THT-KEY.
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD TO THT-HCD.
           MOVE W-SIZ TO THT-SIZ.
      *           READ THTM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-140
           END-IF
           GO TO UPD-160.
       UPD-140.
           MOVE ZERO TO THT-R.
           MOVE W-TCD TO THT-TCD THT-TCD2.
           MOVE W-HCD TO THT-HCD.
           MOVE 9 TO THT-SIZ.
           MOVE W-BT TO THT-T.
           MOVE W-TC2 TO THT-TNC.
           MOVE W-NGD TO THT-NG.
      *           WRITE THT-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            THTM_PNAME1 THTM_LNAME THT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO UPD-020
           END-IF
           GO TO UPD-020.
       UPD-160.
           IF  W-NGD NOT = THT-NG
               MOVE W-NGD TO THT-NG
      *               REWRITE THT-R INVALID KEY
      *///////////////
               CALL "DB_Update" USING
                THTM_PNAME1 THTM_LNAME THT-R RETURNING RET
               IF  RET = 1
                   CALL "SD_Output" USING
                    "E-STAT" E-STAT "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME27" E-ME27 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           GO TO UPD-020.
       UPD-180.
           INITIALIZE W-SR.
           MOVE W-BR TO W-DT1.
           MOVE W-DNOD TO W-DNO.
           MOVE 9 TO W-GNO.
           MOVE W-NGP TO W-DATE.
           MOVE W-TCDN TO W-TCD.
           MOVE W-UNCD TO W-UNC.
           MOVE W-SR TO S-R.
      *           WRITE S-R.
      *//////////////
           CALL "DB_Insert" USING
            S-TRAN_PNAME1 S-TRAN_LNAME S-R RETURNING RET.
           IF  ERR-STAT = "00"
               GO TO UPD-200
           END-IF
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME16" E-ME16 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           GO TO UPD-900.
       UPD-200.
      *
           IF (W-HMN = ZERO) OR (W-HNC = 1)
               GO TO UPD-EX
           END-IF.
       UPD-220.
           MOVE SPACE TO HSMS-KEY.
           MOVE W-HMN TO HSMS-01.
       UPD-230.
      *           START HSMSF KEY NOT < HSMS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSMSF_PNAME1 "HSMS-KEY" " NOT < " HSMS-KEY RETURNING RET.
           IF  RET = 1
               GO TO UPD-EX
           END-IF.
       UPD-240.
      *           READ HSMSF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSMSF_PNAME1 BY REFERENCE HSMS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO UPD-EX
           END-IF
           IF  W-HMN NOT = HSMS-01
               GO TO UPD-EX
           END-IF
           IF  HSMS-19 = 9
               GO TO UPD-EX
           END-IF
           ADD 1 TO HSMS-19.
      *           REWRITE HSMS-R1 INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSMSF_PNAME1 HSMSF_LNAME HSMS-R1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO UPD-240.
       UPD-900.
           PERFORM SD-RTN THRU SD-EX.
           MOVE 1 TO W-END.
       UPD-EX.
           EXIT.
