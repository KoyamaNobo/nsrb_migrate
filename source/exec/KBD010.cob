       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD010.
      *********************************************************
      *    PROGRAM         :  購買仕入伝票入力　　　　　　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBD01                          *
      *        変更　　　  :  62/06/05                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(036) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　購買　仕入伝票　プルーフリスト　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  X(029) VALUE
                " 日　付   ｺｰﾄﾞ 仕　入　先　名".
           02  F              PIC  X(060) VALUE SPACE.
           02  F              PIC  X(023) VALUE
                "( 消費税 * ﾊ 内税計算 )".
           02  F              PIC  X(006) VALUE SPACE.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE "<".
           02  F              PIC  N(002) VALUE "工品".
           02  F              PIC  X(001) VALUE ">".
           02  F              PIC  X(013) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  N(002) VALUE "区分".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "材　　料　　名　".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(004) VALUE "数　　量".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "単　　価".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　消費税".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "修正日　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "商品".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(004) VALUE "納品№　".
           02  F              PIC  N(002) VALUE "不良".
       01  W-P1.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(002).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(085).
       01  W-P2.
           02  P-BSC          PIC  Z(001).
           02  F              PIC  X(005).
           02  P-DNO          PIC  9(006).
           02  P-V            PIC  X(001).
           02  P-GNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DC           PIC  9(002).
           02  F              PIC  X(001).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  P-SU           PIC --,---,--9.99.
           02  P-T            PIC ----,--9.99.
           02  P-KIN          PIC --,---,---,--9.
           02  P-SHZ          PIC --,---,--9.
           02  P-X            PIC  X(001).
           02  P-CD           PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-SSD.
             03  P-SJCD       PIC  9(006).
             03  F            PIC  X(002).
           02  P-KHD   REDEFINES P-SSD.
             03  P-F          PIC  X(001).
             03  P-KCO        PIC  X(005).
             03  P-R          PIC  X(001).
             03  F            PIC  X(001).
           02  P-NNO          PIC  9(006).
           02  F              PIC  X(001).
           02  P-FC           PIC  9(001).
           02  F              PIC  X(001).
       01  W-R.
           02  WR-DC          PIC  9(002).
           02  WR-DCD   REDEFINES WR-DC.
             03  WR-DC1       PIC  9(001).
             03  WR-DC2       PIC  9(001).
           02  WR-DATE        PIC  9(008).
           02  WR-SCD         PIC  9(004).
           02  WR-JCD         PIC  9(006).
           02  WR-JCDD  REDEFINES WR-JCD.
             03  WR-JCD1      PIC  9(001).
             03  WR-JCD2      PIC  9(002).
             03  WR-JCD3      PIC  9(003).
           02  WR-SU          PIC S9(007)V9(02).
           02  WR-SUD   REDEFINES WR-SU.
             03  WR-SU1       PIC  9(007).
             03  WR-SU2       PIC  9(002).
           02  WR-T           PIC S9(006)V9(02).
           02  WR-KIN         PIC S9(008).
           02  WR-SHZ         PIC S9(007).
           02  WR-CD          PIC  9(006).
           02  WR-CDD   REDEFINES WR-CD.
             03  WR-CD1       PIC  9(002).
             03  WR-CD2       PIC  9(002).
             03  WR-CD3       PIC  9(002).
           02  WR-SJCD        PIC  9(006).
           02  WR-NNO         PIC  9(006).
           02  WR-FC          PIC  9(001).
           02  WR-YC          PIC  9(001).
           02  WR-TC          PIC  9(001).
           02  WR-SEC         PIC  9(001).
           02  WR-SC          PIC  9(001).
           02  WR-BSC         PIC  9(001).
           02  WR-BKC         PIC  9(002).
           02  WR-KCO         PIC  X(005).
           02  WR-KHC         PIC  9(001).
           02  F              PIC  X(010).
           02  WR-KEY.
             03  WR-DNO       PIC  9(006).
             03  WR-GNO       PIC  9(001).
           02  WR-PCNT        PIC  9(001).
       01  W-DATA.
           02  W-DATE         PIC  9(008).
           02  W-DATED REDEFINES W-DATE.
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
           02  W-SCD          PIC  9(004).
           02  W-SCDD  REDEFINES W-SCD.
             03  W-SCD1       PIC  9(001).
             03  W-SCD2       PIC  9(003).
           02  W-DC           PIC  9(002).
           02  W-SJCD         PIC  9(006).
           02  W-SJCDD REDEFINES W-SJCD.
             03  W-JCD1       PIC  9(001).
             03  W-JCD2       PIC  9(002).
             03  W-JCD3       PIC  9(003).
           02  W-DNO          PIC  9(006).
           02  W-LIN.
             03  W-LIN1       PIC  9(002).
             03  W-LIN2       PIC  9(002).
             03  W-LIN3       PIC  9(002).
           02  W-LIND.
             03  W-LIND1      PIC  9(002).
             03  W-LIND2      PIC  9(002).
             03  W-LIND3      PIC  9(002).
           02  W-GKIN         PIC S9(009).
           02  W-GSHZ         PIC S9(007).
           02  HIZUKE         PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  CNT            PIC  9(001).
           02  CNTD           PIC  9(001).
           02  CHKD.
             03  CHK     OCCURS   6  PIC  9(001).
           02  W-ACT          PIC  9(001) VALUE ZERO.
           02  W-EC           PIC  9(001) VALUE ZERO.
           02  W-C            PIC  9(001).
           02  W-LIST         PIC  9(001).
           02  W-PC           PIC  9(001) VALUE ZERO.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-BNG          PIC  9(004).
           02  W-KNG          PIC  9(004).
           02  W-NC           PIC  9(001).
       01  W-ARD.
           02  W-RD    OCCURS   6  PIC  X(102).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LISM.
           COPY LIJM.
           COPY LIKHM.
           COPY LIHSSF.
           COPY LSPF.
      *FD  JSS-F
       01  JSS-F_KBD010.
           02  JSS-F_PNAME1   PIC  X(004) VALUE "JSSF".
           02  F              PIC  X(001).
           02  JSS-F_LNAME    PIC  X(012) VALUE "JSS-F_KBD010".
           02  F              PIC  X(001).
           02  JSS-F_KEY1     PIC  X(100) VALUE SPACE.
           02  JSS-F_KEY2     PIC  X(100) VALUE SPACE.
           02  JSS-F_SORT     PIC  X(100) VALUE SPACE.
           02  JSS-F_IDLST    PIC  X(100) VALUE SPACE.
           02  JSS-F_RES      USAGE  POINTER.
       01  JSS-R.
           02  JS-DC          PIC  9(002).
           02  JS-DATE        PIC  9(008).
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　購買　仕入伝票　入力リスト　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(040) VALUE
                "全件=1  未作表分=5  作表しない=9    ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-DATE  PIC  9(006).
             03  A-DNO   PIC  9(006).
           02  A-SCD   PIC  9(004).
           02  FILLER.
             03  A-DC    PIC  9(002).
             03  A-JCD   PIC  9(006).
           02  FILLER.
             03  A-SU    PIC S9(007)V9(02).
             03  A-T     PIC S9(006)V9(02).
             03  A-KIN   PIC S9(008).
             03  A-CD    PIC  9(006).
             03  A-NNO   PIC  9(006).
           02  FILLER.
             03  A-SJCD  PIC  9(006).
             03  A-KCO   PIC  X(005).
             03  A-FC    PIC  9(001).
           02  A-LIST  PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-SNA   PIC  N(024).
             03  D-MD1   PIC  X(010) VALUE "<商品仕入>".
             03  D-MD2   PIC  X(010) VALUE "<材料仕入>".
           02  FILLER.
             03  D-JNA   PIC  N(024).
           02  FILLER.
             03  D-SU    PIC ZZZZZZ9.99- .
             03  D-T     PIC ZZZZZ9.99- .
             03  D-KIN   PIC ZZZZZZZ9- .
             03  D-NHM   PIC  N(003) VALUE "納品№".
           02  FILLER.
             03  D-CM    PIC  X(004) VALUE "ｺｰﾄﾞ".
             03  D-NM    PIC  N(003) VALUE "商品名".
             03  D-SHN   PIC  N(024).
             03  D-KM    PIC  N(003) VALUE "工品名".
             03  D-KHN   PIC  X(020).
             03  D-FCM   PIC  N(004) VALUE "不良区分".
           02  D-GKIN  PIC ZZZ,ZZZ,ZZ9- .
       01  C-SPC.
           02  FILLER.
             03  S-DC    PIC  X(002) VALUE "  ".
             03  S-JCD   PIC  X(006) VALUE "      ".
             03  S-JNA   PIC  X(048) VALUE
                  "                                   ".
           02  FILLER.
             03  S-SU    PIC  X(011) VALUE "           ".
             03  S-T     PIC  X(010) VALUE "          ".
             03  S-KIN   PIC  X(009) VALUE "         ".
             03  S-CD    PIC  X(006) VALUE "      ".
             03  S-NHM   PIC  X(006) VALUE "      ".
             03  S-NNO   PIC  X(006) VALUE "      ".
           02  FILLER.
             03  S-CM    PIC  X(004) VALUE "    ".
             03  S-SJCD  PIC  X(006) VALUE "      ".
             03  S-NM    PIC  X(006) VALUE "      ".
             03  S-SHN   PIC  X(048) VALUE
                  "                                   ".
             03  S-FCM   PIC  X(008) VALUE "        ".
             03  S-FC    PIC  X(001) VALUE " ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-ME2   PIC  X(015) VALUE
                  "***  JM ﾅｼ  ***".
             03  E-ME3   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ｺｰﾄﾞ ｴﾗｰ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  KBNOM ﾅｼ  ***".
             03  E-ME6   PIC  X(027) VALUE
                  "***  KBNOM REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME8   PIC  X(024) VALUE
                  "***  JSSF WRITE ｴﾗｰ  ***".
             03  E-ME9   PIC  X(025) VALUE
                  "***  JSSF DELETE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(026) VALUE
                  "***  JSSF REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(022) VALUE
                  "***  ｼﾊﾗｲ ﾃﾞﾝﾋﾟｮｳ  ***".
             03  E-ME12  PIC  X(024) VALUE
                  "***  ｾｲﾋﾝｼｲﾚﾌｧｲﾙ ﾅｼ  ***".
             03  E-ME13  PIC  X(025) VALUE
                  "***  ｾｲﾋﾝｼｲﾚ ﾍﾝｶﾝ ｽﾞﾐ ***".
             03  E-ME14  PIC  X(025) VALUE
                  "***  HSSF REWRITE ｴﾗｰ ***".
             03  E-ME15  PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME16  PIC  X(022) VALUE
                  "***  ｺｳﾋﾝ ﾖｳﾄ ｴﾗｰ  ***".
             03  E-ME17  PIC  X(021) VALUE
                  "***  ｺｳﾋﾝ ﾀﾝｶ ﾅｼ  ***".
             03  E-ME18  PIC  X(023) VALUE
                  "***  ｺｳﾋﾝ ﾋﾂﾞｹ ｴﾗｰ  ***".
             03  E-ME72  PIC  N(013) VALUE
                  "日次更新後，入力して下さい".
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
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "384" " " " " RETURNING RESU.
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
            "08C-MID" "X" "14" "13" "40" "07C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID" "X" "23" "30" "22" "08C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "72" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "1" "69" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "2" "0" "12" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "2" "7" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "2" "74" "6" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "3" "7" "4" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "W-LIN1" "0" "8" "A-SCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DC" "9" "W-LIN1" "7" "2" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DC" BY REFERENCE W-DC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCD" "9" "W-LIN1" "15" "6" "A-DC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCD" BY REFERENCE WR-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "W-LIN2" "0" "33" "04C-ACP" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "W-LIN2" "7" "9" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE WR-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T" "S9" "W-LIN2" "25" "8" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-T" BY REFERENCE WR-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KIN" "S9" "W-LIN2" "42" "8" "A-T" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KIN" BY REFERENCE WR-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CD" "9" "W-LIN2" "59" "6" "A-KIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CD" BY REFERENCE WR-CD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NNO" "9" "W-LIN2" "73" "6" "A-CD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NNO" BY REFERENCE WR-NNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
           "06C-ACP" " " "W-LIN3" "0" "12" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SJCD" "9" "W-LIN3" "7" "6" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SJCD" BY REFERENCE W-SJCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KCO" "X" "W-LIN3" "7" "5" "A-SJCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KCO" BY REFERENCE WR-KCO "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FC" "9" "W-LIN3" "79" "1" "A-KCO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FC" BY REFERENCE WR-FC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-LIST" "9" "14" "48" "1" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-LIST" BY REFERENCE W-LIST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "47" "1" "A-LIST" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "256" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "3" "0" "68" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "3" "21" "48" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD1" "X" "3" "71" "10" "D-SNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MD2" "X" "3" "71" "10" "D-MD1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-LIN1" "0" "48" "01C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JNA" "N" "W-LIN1" "29" "48" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-JNA" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "W-LIN2" "0" "36" "02C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZZZ9.99-" "W-LIN2" "7" "11" " " "03C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE WR-SU "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T" "ZZZZZ9.99-" "W-LIN2" "25" "10" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-T" BY REFERENCE WR-T "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KIN" "ZZZZZZZ9-" "W-LIN2" "42" "9" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-KIN" BY REFERENCE WR-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NHM" "N" "W-LIN2" "66" "6" "D-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "W-LIN3" "0" "92" "03C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CM" "X" "W-LIN3" "2" "4" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NM" "N" "W-LIN3" "14" "6" "D-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SHN" "N" "W-LIN3" "21" "48" "D-NM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SHN" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KM" "N" "W-LIN3" "14" "6" "D-SHN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KHN" "X" "W-LIN3" "21" "20" "D-KM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-KHN" BY REFERENCE KH-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FCM" "N" "W-LIN3" "70" "8" "D-KHN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKIN" "ZZZ,ZZZ,ZZ9-" "22" "39" "12" "04C-DSP" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GKIN" BY REFERENCE W-GKIN "9" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "177" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-SPC" " " "W-LIN1" "0" "56" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-DC" "X" "W-LIN1" "7" "2" " " "01C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-JCD" "X" "W-LIN1" "15" "6" "S-DC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-JNA" "X" "W-LIN1" "29" "48" "S-JCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "02C-SPC" " " "W-LIN2" "0" "48" "01C-SPC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SU" "X" "W-LIN2" "7" "11" " " "02C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-T" "X" "W-LIN2" "25" "10" "S-SU" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-KIN" "X" "W-LIN2" "42" "9" "S-T" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-CD" "X" "W-LIN2" "59" "6" "S-KIN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-NHM" "X" "W-LIN2" "66" "6" "S-CD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-NNO" "X" "W-LIN2" "73" "6" "S-NHM" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "03C-SPC" " " "W-LIN3" "0" "73" "02C-SPC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-CM" "X" "W-LIN3" "2" "4" " " "03C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SJCD" "X" "W-LIN3" "7" "6" "S-CM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-NM" "X" "W-LIN3" "14" "6" "S-SJCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SHN" "X" "W-LIN3" "21" "48" "S-NM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-FCM" "X" "W-LIN3" "70" "8" "S-SHN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-FC" "X" "W-LIN3" "79" "1" "S-FCM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "506" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "506" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "15" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "15" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "21" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "27" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "17" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "24" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "25" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "26" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "22" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "24" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "25" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "25" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "16" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "22" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "21" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" "X" "24" "15" "23" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME72" "N" "24" "15" "26" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-ME72" " " RETURNING RESU.
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
           COPY LIBCPR.
           MOVE D-NBNG TO W-BNG.
           MOVE D-NKNG TO W-KNG.
           MOVE DATE-05R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KBNO-M_PNAME1 " " BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "01" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           MOVE BNO-DATE TO HIZUKE.
           MOVE SPACE TO BNO-KEY.
           MOVE "02" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           CALL "DB_F_Open" USING
            "I-O" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
           CALL "SD_Screen_Output" USING "SCBD01" RETURNING RESU.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-ACT = 9
               GO TO M-80
           END-IF
           IF  W-ACT < 1 OR > 3
               GO TO M-10
           END-IF.
       M-15.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF.
      ****************   ファイル　登録   *****************
       M-20.
           IF  W-ACT NOT = 1
               GO TO M-45
           END-IF
           MOVE ZERO TO CNT.
       M-25.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           ADD 1 TO CNT.
           IF  CNT = 7
               GO TO M-75
           END-IF
           MOVE W-RD(CNT) TO W-R.
           IF  WR-DATE = ZERO
               GO TO M-75
           END-IF
           PERFORM MOV-RTN THRU MOV-EX.
           MOVE ZERO TO JSS-R.
           MOVE W-R TO JSS-R.
      *           WRITE JSS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               GO TO M-30
           END-IF
           GO TO M-35.
       M-30.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME72" E-ME72 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE 5 TO W-EC.
           GO TO M-75.
       M-35.
           IF  CNT NOT = 1
               GO TO M-25
           END-IF
           IF  W-NC NOT = 0
               GO TO M-40
           END-IF
           MOVE W-DNO TO BNO-DNO1.
      *           REWRITE KBNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBNO-M_PNAME1 KBNO-M_LNAME KBNO-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-75
           END-IF
           GO TO M-25.
       M-40.
           MOVE 1 TO HSS-BHC.
      *           REWRITE HSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSS-F_PNAME1 HSS-F_LNAME HSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-75
           END-IF
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HSS-DNO = W-DNO
               GO TO M-40
           END-IF
           GO TO M-25.
      ****************   ファイル　修正   *****************
       M-45.
           IF  W-ACT NOT = 2
               GO TO M-70
           END-IF
           MOVE ZERO TO CNT.
       M-50.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           ADD 1 TO CNT.
           IF  CNT = 7
               GO TO M-75
           END-IF
           MOVE W-RD(CNT) TO W-R.
           IF  WR-DATE = ZERO
               GO TO M-65
           END-IF
           PERFORM MOV-RTN THRU MOV-EX.
           MOVE WR-KEY TO JS-KEY.
      *           READ JSS-F INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-55
           END-IF
           MOVE W-R TO JSS-R.
           MOVE 0 TO JS-PCNT.
      *           REWRITE JSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO M-75
           END-IF
           GO TO M-50.
       M-55.
           MOVE ZERO TO JSS-R.
           MOVE W-R TO JSS-R.
      *           WRITE JSS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               GO TO M-60
           END-IF
           GO TO M-50.
       M-60.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT = "24"
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME72" E-ME72 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           MOVE 5 TO W-EC.
           GO TO M-75.
       M-65.
           MOVE W-DNO TO JS-DNO.
           MOVE CNT TO JS-GNO.
      *           START JSS-F KEY NOT < JS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSS-F_PNAME1 "JS-KEY" " NOT < " JS-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-75
           END-IF
           PERFORM DEL-RTN THRU DEL-EX.
           GO TO M-75.
      ****************   ファイル　削除   *****************
       M-70.
           MOVE ZERO TO JS-KEY.
           MOVE W-DNO TO JS-DNO.
      *           START JSS-F KEY NOT < JS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSS-F_PNAME1 "JS-KEY" " NOT < " JS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-75
           END-IF
           PERFORM DEL-RTN THRU DEL-EX.
      *******************************************************
       M-75.
           IF  W-EC = 5
               GO TO M-80
           END-IF
           MOVE ZERO TO W-R
           GO TO M-15.
      *******************    作　　表    ***********************
       M-80.
           PERFORM LST-RTN THRU LST-EX.
      *******************   Ｅ　Ｎ　Ｄ   ***********************
       M-95.
           CALL "DB_F_Close" USING
            BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *******************   入　　　力   ***********************
       ACP-RTN.
           CALL "SD_Screen_Output" USING "SCBD01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-ACT = 1
               GO TO ACP-040
           END-IF
           MOVE ZERO TO JS-KEY.
           MOVE W-DNO TO JS-DNO.
      *           START JSS-F KEY NOT < JS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JSS-F_PNAME1 "JS-KEY" " NOT < " JS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  JS-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  JS-DC = 30
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           GO TO ACP-060.
       ACP-040.
           IF  W-DNO = ZERO
               MOVE 0 TO W-NC
               GO TO ACP-080
           END-IF
           MOVE ZERO TO HSS-KEY.
           MOVE W-DNO TO HSS-DNO.
      *           START HSS-F KEY NOT < HSS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSS-F_PNAME1 "HSS-KEY" " NOT < " HSS-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  HSS-DNO NOT = W-DNO
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  HSS-BHC = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           MOVE 1 TO W-NC.
       ACP-060.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-ACT = 3
               GO TO ACP-480
           END-IF.
       ACP-080.
           MOVE ZERO TO W-R.
           IF  W-ACT = 1
               IF  W-NC = 0
                   IF  W-DATE = ZERO
                       GO TO ACP-100
                   ELSE
                       GO TO ACP-120
                   END-IF
               END-IF
           END-IF.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT NOT = 1
                   GO TO ACP-020
               ELSE
                   IF  W-NC = 0
                       GO TO ACP-EX
                   ELSE
                       GO TO ACP-020
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           IF (W-GET < 1 OR > 12) OR (W-PEY < 1 OR > 31)
               GO TO ACP-100
           END-IF
           IF  W-NGS NOT = W-BNG
               GO TO ACP-100
           END-IF
           IF  HIZUKE NOT < W-PEY
               GO TO ACP-100
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
       ACP-120.
           IF  W-ACT = 1
               IF  W-NC = 0
                   CALL "SD_Screen_Output" USING "SCBD01" RETURNING RESU
                   MOVE ZERO TO W-ARD W-SCD
                   COMPUTE W-DNO = BNO-DNO1 + 1
                   CALL "SD_Output" USING
                    "A-ACT" A-ACT "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-DNO" A-DNO "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-DATE" A-DATE "p" RETURNING RESU
                   IF  W-DNO = ZERO
                       ADD 1 TO W-DNO
                       CALL "SD_Output" USING
                        "A-DNO" A-DNO "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-120
           END-IF
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  W-SCD1 > 4
               CALL "SD_Output" USING "D-MD1" D-MD1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-MD2" D-MD2 "p" RETURNING RESU
           END-IF
           MOVE 1 TO W-LIN1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
           MOVE ZERO TO CNT W-GKIN CHKD.
       ACP-140.
           ADD 1 TO CNT.
           ADD 3 TO W-LIN1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
           COMPUTE W-LIN2 = W-LIN1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN2" "2" W-LIN2 RETURNING RESU.
           COMPUTE W-LIN3 = W-LIN2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN3" "2" W-LIN3 RETURNING RESU.
           IF  CNT = 7
               GO TO ACP-480
           END-IF
           MOVE W-RD(CNT) TO W-R.
       ACP-160.
           IF  WR-DC NOT = ZERO
               MOVE WR-DC TO W-DC
           END-IF
           CALL "SD_Output" USING "A-DC" A-DC "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DC "A-DC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-440
           END-IF
           IF  ESTAT = ADV
               MOVE CNT TO CNTD
               MOVE W-LIN TO W-LIND
               GO TO ACP-460
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-160
           END-IF
           MOVE W-DC TO WR-DC.
           IF  WR-DC1 NOT = 1
               GO TO ACP-160
           END-IF
           IF  WR-DC2 > 4
               GO TO ACP-160
           END-IF.
       ACP-180.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-180
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           MOVE WR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-180
           END-IF
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           IF  WR-JCD = 999000
               GO TO ACP-200
           END-IF
           IF  W-SCD1 > 4
               IF (WR-JCD2 < 90 OR > 94) AND (WR-JCD1 NOT = 7)
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO ACP-180
               END-IF
           END-IF
           IF  W-SCD1 < 5
               IF (WR-JCD2 > 89 AND < 95) OR (WR-JCD1 = 7)
                   CALL "SD_Output" USING
                    "E-ME4" E-ME4 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO ACP-180
               END-IF
           END-IF.
       ACP-200.
           MOVE J-YC TO WR-YC.
           MOVE J-TC1 TO WR-TC.
           MOVE J-SC TO WR-SEC.
           MOVE J-ST TO WR-T.
           MOVE J-BKC TO WR-BKC
           IF  J-KT NOT = ZERO
               MOVE J-KT TO WR-T
           END-IF
           IF  WR-JCD > 798999
               MOVE ZERO TO WR-SU WR-T
               CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
               CALL "SD_Output" USING "S-T" S-T "p" RETURNING RESU
               GO TO ACP-260
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
       ACP-220.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-220
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF WR-JCD > 004999 AND < 005500
              IF (WR-SU > 99999 OR < -99999) OR (WR-SU2 NOT = 00 AND 50)
                  GO TO ACP-220
              END-IF
           END-IF.
       ACP-240.
           CALL "SD_Accept" USING BY REFERENCE A-T "A-T" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-240
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
           IF  WR-JCD > 004999 AND < 005500
               IF  WR-T > 9999
                   GO TO ACP-240
               END-IF
           END-IF
           IF  ZERO > WR-T OR WR-SU
               COMPUTE WR-KIN = WR-SU * WR-T - 0.9
           ELSE
               COMPUTE WR-KIN = WR-SU * WR-T
           END-IF
           MOVE ZERO TO CHK(CNT).
           IF  WR-KIN NOT = ZERO
               MOVE 5 TO CHK(CNT)
               GO TO ACP-280
           END-IF.
       ACP-260.
           CALL "SD_Accept" USING BY REFERENCE A-KIN "A-KIN" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  WR-JCD > 799000
                   GO TO ACP-180
               ELSE
                   GO TO ACP-240
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-260
           END-IF
           IF  WR-JCD > 799000
               GO TO ACP-280
           END-IF
           MOVE ZERO TO WR-T.
           IF  WR-KIN NOT = ZERO
               IF  WR-SU NOT = ZERO
                   COMPUTE WR-T = WR-KIN / WR-SU
               END-IF
           END-IF
           CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU.
       ACP-280.
           IF  WR-JCD = 999000
               IF  WR-KIN > 9999999 OR < -9999999
                   GO TO ACP-260
               END-IF
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
       ACP-300.
           CALL "SD_Accept" USING BY REFERENCE A-CD "A-CD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  CHK(CNT) = 5
                   GO TO ACP-240
               ELSE
                   GO TO ACP-260
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-300
           END-IF
           IF  WR-CD = ZERO
               CALL "SD_Output" USING "S-CD" S-CD "p" RETURNING RESU
               GO TO ACP-320
           END-IF
           IF (WR-CD2 < 1 OR > 12) OR (WR-CD3 < 1 OR > 31)
               GO TO ACP-300
           END-IF.
       ACP-320.
           IF (WR-JCD NOT = 999000) AND (WR-JCD > 798999)
               CALL "SD_Output" USING "D-CM" D-CM "p" RETURNING RESU
               CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU
               GO TO ACP-340
           END-IF
           IF  WR-JCD > 489999 AND < 499000
               IF  WR-DC NOT = 10
                   MOVE SPACE TO WR-KCO
                   GO TO ACP-420
               ELSE
                   CALL "SD_Output" USING "D-CM" D-CM "p" RETURNING RESU
                   CALL "SD_Output" USING "D-KM" D-KM "p" RETURNING RESU
                   GO TO ACP-360
               END-IF
           END-IF
           MOVE ZERO TO WR-SJCD.
           MOVE SPACE TO WR-KCO.
           CALL "SD_Output" USING "S-CM" S-CM "p" RETURNING RESU.
           CALL "SD_Output" USING "S-SJCD" S-SJCD "p" RETURNING RESU.
           CALL "SD_Output" USING "S-NM" S-NM "p" RETURNING RESU.
           CALL "SD_Output" USING "S-SHN" S-SHN "p" RETURNING RESU.
           IF (WR-JCD < 005000 OR > 005499) OR (WR-T < 1)
               MOVE ZERO TO WR-NNO WR-FC
               CALL "SD_Output" USING "S-NHM" S-NHM "p" RETURNING RESU
               CALL "SD_Output" USING "S-FCM" S-FCM "p" RETURNING RESU
               CALL "SD_Output" USING "S-NNO" S-NNO "p" RETURNING RESU
               CALL "SD_Output" USING "S-FC" S-FC "p" RETURNING RESU
               GO TO ACP-420
           ELSE
               CALL "SD_Output" USING "D-NHM" D-NHM "p" RETURNING RESU
               CALL "SD_Output" USING "D-FCM" D-FCM "p" RETURNING RESU
               GO TO ACP-380
           END-IF.
       ACP-340.
           CALL "SD_Accept" USING BY REFERENCE A-SJCD "A-SJCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-340
           END-IF
           IF  W-JCD2 < 90 OR > 94
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-340
           END-IF
           MOVE W-SJCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-340
           END-IF
           CALL "SD_Output" USING "D-SHN" D-SHN "p" RETURNING RESU.
           MOVE J-KEY TO WR-SJCD.
           GO TO ACP-420.
       ACP-360.
           CALL "SD_Accept" USING BY REFERENCE A-KCO "A-KCO" "X" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-360
           END-IF
           MOVE WR-KCO TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-360
           END-IF
           IF  KH-NC = 0
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
           IF  KH-GT1 = ZERO
               CALL "SD_Output" USING
                "E-ME17" E-ME17 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "D-KHN" D-KHN "p" RETURNING RESU
           IF  W-NGS NOT = W-KNG
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-360
           END-IF
           GO TO ACP-420.
       ACP-380.
           CALL "SD_Accept" USING BY REFERENCE A-NNO "A-NNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-380
           END-IF.
       ACP-400.
           CALL "SD_Accept" USING BY REFERENCE A-FC "A-FC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-380
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-400
           END-IF
           IF  WR-FC > 2
               GO TO ACP-400
           END-IF
           IF  WR-SU > ZERO
               GO TO ACP-420
           END-IF
           IF  WR-DC2 NOT = ZERO
               GO TO ACP-420
           END-IF
           IF  WR-FC = ZERO
               GO TO ACP-400
           END-IF.
       ACP-420.
           MOVE W-DATE TO WR-DATE.
           MOVE W-SCD TO WR-SCD.
           MOVE W-DNO TO WR-DNO.
           MOVE CNT TO WR-GNO.
           MOVE W-R TO W-RD(CNT).
           ADD WR-KIN TO W-GKIN.
           GO TO ACP-140.
       ACP-440.
           IF  CNT = 1
               GO TO ACP-120
           END-IF
           SUBTRACT 1 FROM CNT.
           SUBTRACT 3 FROM W-LIN1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
           COMPUTE W-LIN2 = W-LIN1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN2" "2" W-LIN2 RETURNING RESU.
           COMPUTE W-LIN3 = W-LIN2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN3" "2" W-LIN3 RETURNING RESU.
           MOVE W-RD(CNT) TO W-R.
           SUBTRACT WR-KIN FROM W-GKIN.
           IF  WR-JCD > 004999 AND < 005500
               IF  WR-T > ZERO
                   GO TO ACP-400
               ELSE
                   GO TO ACP-300
               END-IF
           END-IF
           IF (WR-JCD NOT = 999000) AND (WR-JCD > 798999)
               GO TO ACP-340
           END-IF
           IF  WR-JCD > 489999 AND < 499000
               IF  WR-DC NOT = 10
                   GO TO ACP-300
               ELSE
                   GO TO ACP-360
               END-IF
           END-IF
           GO TO ACP-300.
       ACP-460.
           CALL "SD_Output" USING "C-SPC" C-SPC "p" RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE W-R TO W-RD(CNTD)
           ADD 1 TO CNTD.
           ADD 3 TO W-LIN1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
           COMPUTE W-LIN2 = W-LIN1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN2" "2" W-LIN2 RETURNING RESU.
           COMPUTE W-LIN3 = W-LIN2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN3" "2" W-LIN3 RETURNING RESU.
           IF  CNTD NOT = 7
               GO TO ACP-460
           END-IF
           MOVE W-LIND TO W-LIN.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN2" "2" W-LIN2 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN3" "2" W-LIN3 RETURNING RESU.
       ACP-480.
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-020
               END-IF
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-440
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-480
           END-IF
           IF  W-DMM = 9
               MOVE ZERO TO W-R
               IF  W-ACT = 1
                   GO TO ACP-120
               ELSE
                   GO TO ACP-RTN
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-480
           END-IF.
       ACP-EX.
           EXIT.
      **********   ＷＯＲＫ　落し込み　・　画面表示   ***********
       DSP-RTN.
           CALL "SD_Screen_Output" USING "SCBD01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DNO" A-DNO "p" RETURNING RESU.
           MOVE ZERO TO W-ARD CNT W-GKIN.
           MOVE 1 TO W-LIN1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
       DSP-010.
           ADD 1 TO CNT.
           IF  CNT = 7
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
           ADD 3 TO W-LIN1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN1" "2" W-LIN1 RETURNING RESU.
           COMPUTE W-LIN2 = W-LIN1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN2" "2" W-LIN2 RETURNING RESU.
           COMPUTE W-LIN3 = W-LIN2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN3" "2" W-LIN3 RETURNING RESU.
           MOVE ZERO TO W-R.
           IF  W-ACT = 1
               MOVE 10 TO WR-DC
               MOVE HSS-DATE TO WR-DATE
               MOVE HSS-SCD TO WR-SCD
               MOVE HSS-JCD TO WR-JCD
               MOVE HSS-SUT TO WR-SU
               MOVE HSS-CD TO WR-CD
               MOVE 3 TO WR-BSC
               MOVE HSS-DNO TO WR-DNO
               MOVE 1 TO WR-GNO
           ELSE
               MOVE JSS-R TO W-R
           END-IF
           IF  WR-SJCD NOT = ZERO
               MOVE WR-SJCD TO W-SJCD
               MOVE WR-JCD TO WR-SJCD
               MOVE W-SJCD TO WR-JCD
               MOVE WR-SJCD TO W-SJCD
           END-IF
           IF  WR-SHZ = ZERO
               GO TO DSP-020
           END-IF
           IF  WR-JCD = 999000
               MOVE WR-SHZ TO WR-KIN
               MOVE ZERO TO WR-SHZ
               GO TO DSP-020
           END-IF
           COMPUTE WR-KIN = WR-KIN + WR-SHZ.
           MOVE ZERO TO WR-SHZ.
           IF  ZERO = WR-SU OR WR-KIN
               GO TO DSP-020
           END-IF
           IF  WR-SU > ZERO
               COMPUTE WR-T = (WR-KIN / WR-SU) + 0.009
           ELSE
               COMPUTE WR-T = (WR-KIN / WR-SU) - 0.009
           END-IF.
       DSP-020.
           ADD WR-KIN TO W-GKIN.
           MOVE W-R TO W-RD(CNT).
           IF  CNT NOT = 1
               GO TO DSP-030
           END-IF
           MOVE WR-DATE TO W-DATE.
           MOVE WR-SCD TO W-SCD.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "　＊＊　仕入先マスター　なし　＊＊" TO S-NAME
           END-IF
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  W-SCD1 > 4
               CALL "SD_Output" USING "D-MD1" D-MD1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-MD2" D-MD2 "p" RETURNING RESU
           END-IF.
       DSP-030.
           MOVE WR-DC TO W-DC.
           MOVE WR-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊＊　材料マスター　なし　＊＊" TO J-NAME
           END-IF
           CALL "SD_Output" USING "A-DC" A-DC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JCD" A-JCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  WR-JCD > 798999
               CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
               CALL "SD_Output" USING "S-T" S-T "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU
           END-IF
           IF  WR-CD = ZERO
               CALL "SD_Output" USING "S-CD" S-CD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-CD" A-CD "p" RETURNING RESU
           END-IF
      *
           IF  WR-JCD < 799000 OR = 999000
               CALL "SD_Output" USING "S-CM" S-CM "p" RETURNING RESU
               CALL "SD_Output" USING "S-SJCD" S-SJCD "p" RETURNING RESU
               CALL "SD_Output" USING "S-NM" S-NM "p" RETURNING RESU
               CALL "SD_Output" USING "S-SHN" S-SHN "p" RETURNING RESU
               GO TO DSP-040
           END-IF
           MOVE WR-SJCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊＊　材料マスター　なし　＊＊" TO J-NAME
           END-IF
           CALL "SD_Output" USING "D-CM" D-CM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SJCD" A-SJCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NM" D-NM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SHN" D-SHN "p" RETURNING RESU.
       DSP-040.
           IF  WR-JCD < 490000 OR > 498999
               GO TO DSP-050
           END-IF
           IF  WR-DC NOT = 10
               GO TO DSP-050
           END-IF
           MOVE WR-KCO TO KH-KEY.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO KH-NAME
               MOVE " ***  KHM ﾅｼ  ***   " TO KH-NAME
           END-IF
           CALL "SD_Output" USING "D-CM" D-CM "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KM" D-KM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KCO" A-KCO "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KHN" D-KHN "p" RETURNING RESU.
       DSP-050.
           IF (WR-JCD < 005000 OR > 005499) OR (WR-T < 1)
               CALL "SD_Output" USING "S-NHM" S-NHM "p" RETURNING RESU
               CALL "SD_Output" USING "S-FCM" S-FCM "p" RETURNING RESU
               CALL "SD_Output" USING "S-NNO" S-NNO "p" RETURNING RESU
               CALL "SD_Output" USING "S-FC" S-FC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-NHM" D-NHM "p" RETURNING RESU
               CALL "SD_Output" USING "D-FCM" D-FCM "p" RETURNING RESU
               CALL "SD_Output" USING "A-NNO" A-NNO "p" RETURNING RESU
               CALL "SD_Output" USING "A-FC" A-FC "p" RETURNING RESU
           END-IF
      *
           IF  W-ACT = 1
               GO TO DSP-060
           END-IF
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
           IF  JS-DNO = W-DNO
               GO TO DSP-010
           END-IF.
       DSP-060.
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
       DSP-EX.
           EXIT.
      ************   登録・修正　ＲＥＣＯＲＤ　ＭＯＶＥ   *********
       MOV-RTN.
           IF  WR-SJCD NOT = ZERO
               MOVE WR-SJCD TO W-SJCD
               MOVE WR-JCD TO WR-SJCD
               MOVE W-SJCD TO WR-JCD
           END-IF
           IF  WR-JCD = 999000
               MOVE WR-KIN TO WR-SHZ
               MOVE ZERO TO WR-KIN
               GO TO MOV-EX
           END-IF
           IF (WR-KIN = ZERO) OR (S-SZC = 0)
               GO TO MOV-EX
           END-IF
           COMPUTE WR-SHZ ROUNDED = (WR-KIN * 5) / 105.
           SUBTRACT WR-SHZ FROM WR-KIN.
           IF  WR-SU NOT = ZERO
               IF  WR-KIN > ZERO
                   COMPUTE WR-T = (WR-KIN / WR-SU) + 0.009
               ELSE
                   COMPUTE WR-T = (WR-KIN / WR-SU) - 0.009
               END-IF
           END-IF.
       MOV-EX.
           EXIT.
      *****************   削　除   ********************
       DEL-RTN.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO DEL-EX
           END-IF
           IF  JS-DNO NOT = W-DNO
               GO TO DEL-EX
           END-IF
      *           DELETE JSS-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING JSS-F_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 5 TO W-EC
               GO TO DEL-EX
           END-IF
           GO TO DEL-RTN.
       DEL-EX.
           EXIT.
      *******************    作　　表    ***********************
       LST-RTN.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
       LST-020.
           CALL "SD_Accept" USING BY REFERENCE A-LIST "A-LIST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-020
           END-IF
           IF  W-LIST NOT = 1 AND 5 AND 9
               GO TO LST-020
           END-IF.
       LST-040.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-040
           END-IF
           IF  W-DMM = 9
               GO TO LST-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO LST-040
           END-IF
           IF  W-LIST = 9
               GO TO LST-EX
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE JSS-F_IDLST JSS-F_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" JSS-F_PNAME1 "SHARED" BY REFERENCE JSS-F_IDLST "1"
            "JS-KEY" BY REFERENCE JS-KEY.
       LST-060.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO LST-EX
           END-IF
           IF  JS-DC = 30
               GO TO LST-060
           END-IF
           IF  W-LIST = 5
               IF  JS-PCNT NOT = 0
                   GO TO LST-060
               END-IF
           END-IF
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-010 THRU MID-EX.
       LST-080.
           MOVE JS-DNO TO W-DNO.
           MOVE ZERO TO W-GKIN W-GSHZ CNT.
           MOVE JS-DATE TO W-DATE.
           MOVE JS-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "　＊＊　仕入先マスター　なし　＊＊" TO S-NAME
           END-IF.
       LST-100.
           ADD 1 TO CNT.
           IF  CNT = 1
               PERFORM PR1-RTN THRU PR1-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE JS-BSC TO P-BSC.
           MOVE JS-DNO TO P-DNO.
           MOVE "-" TO P-V.
           MOVE JS-GNO TO P-GNO.
           MOVE JS-DC TO P-DC.
           MOVE JS-JCD TO P-JCD.
           MOVE JS-JCD TO J-KEY.
           IF  JS-SJCD NOT = ZERO
               MOVE JS-SJCD TO P-JCD J-KEY
           END-IF
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "　＊＊　マスター　なし　＊＊　　　　" TO J-NAME
           END-IF
           MOVE J-NAME TO P-JNA.
           IF  JS-SJCD = ZERO
               IF  JS-SU NOT = ZERO
                   MOVE JS-SU TO P-SU
               END-IF
           END-IF
           IF  JS-SJCD = ZERO
               IF  JS-T NOT = ZERO
                   MOVE JS-T TO P-T
               END-IF
           END-IF
           IF  JS-JCD = 999000
               MOVE JS-SHZ TO P-SHZ
           END-IF
           IF  JS-JCD NOT = 999000
               MOVE JS-KIN TO P-KIN
               IF  JS-SHZ NOT = ZERO
                   MOVE "*" TO P-X
                   MOVE JS-SHZ TO P-SHZ
               END-IF
           END-IF
           IF  JS-CD NOT = ZERO
               MOVE JS-CD TO P-CD
           END-IF
           IF  JS-SJCD NOT = ZERO
               MOVE JS-JCD TO P-SJCD
           END-IF
           IF  JS-JCD > 489999 AND < 499000
               IF  JS-DC = 10
                   MOVE "<" TO P-F
                   MOVE JS-KCO TO P-KCO
                   MOVE ">" TO P-R
               END-IF
           END-IF
           IF  JS-JCD > 004999 AND < 005500
               IF  JS-T > ZERO
                   MOVE JS-NNO TO P-NNO
                   MOVE JS-FC TO P-FC
               END-IF
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 62
               GO TO LST-120
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           PERFORM PR1-RTN THRU PR1-EX.
       LST-120.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD JS-KIN TO W-GKIN.
           ADD JS-SHZ TO W-GSHZ.
           IF  JS-PCNT NOT = 9
               ADD 1 TO JS-PCNT
           END-IF
      *           REWRITE JSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JSS-F_PNAME1 JSS-F_LNAME JSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       LST-140.
      *           READ JSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JSS-F_PNAME1 BY REFERENCE JSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO LST-160
           END-IF
           IF  JS-DC = 30
               GO TO LST-140
           END-IF
           IF  W-LIST = 5
               IF  JS-PCNT NOT = 0
                   GO TO LST-140
               END-IF
           END-IF
           IF  JS-DNO = W-DNO
               GO TO LST-100
           END-IF
           IF  CNT NOT = 1
               PERFORM PRT-RTN THRU PRT-EX
           END-IF
           GO TO LST-080.
       LST-160.
           IF  CNT NOT = 1
               PERFORM PRT-RTN THRU PRT-EX
           END-IF
           CALL "PR_Close" RETURNING RESP.
       LST-EX.
           EXIT.
      *******************    見　出　し   ***********************
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-010.
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
       MID-EX.
           EXIT.
      *******************    伝票　見出し   ***********************
       PR1-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO W-P1.
           MOVE W-NGPS TO P-DATE.
           MOVE S-KEY TO P-SCD.
           MOVE S-NAME TO P-SNA.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PR1-EX.
           EXIT.
      ******************   ＴＯＴＡＬ　作表   ***********************
       PRT-RTN.
           MOVE SPACE TO W-P2.
           MOVE "　　　　　　＊＊＊　ＴＯＴＡＬ　＊＊＊　" TO P-JNA.
           MOVE W-GKIN TO P-KIN.
           MOVE W-GSHZ TO P-SHZ.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRT-EX.
           EXIT.
