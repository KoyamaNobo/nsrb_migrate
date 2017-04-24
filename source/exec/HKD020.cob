       IDENTIFICATION DIVISION.
       PROGRAM-ID. HKD020.
      **********************************************************
      *    入金伝票発行                　　　　　              *
      *    FORM    : FHK020                                    *
      *    JS-SIGN : 正規=0 , 再発行(伝票№)=1 , (当月得意先)=2*
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       77  WK0128ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0128".
           02  W-FID2         PIC  X(003).
       01  ERR-STAT           PIC  X(002).
       01  W-FILE             PIC  X(013).
       01  HEAD1.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(098) VALUE SPACE.
           02  F              PIC  N(002) VALUE "№　".
           02  P-NO           PIC  9(006).
           02  F              PIC  X(002) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  X(093) VALUE SPACE.
           02  F              PIC  X(001) VALUE "'".
           02  P-NEN          PIC  9(002).
           02  F              PIC  N(002) VALUE "年　".
           02  P-GET          PIC Z9.
           02  F              PIC  N(002) VALUE "月　".
           02  P-PEY          PIC Z9.
           02  F              PIC  N(002) VALUE "日　".
       01  HEAD3.
           02  F              PIC  X(049) VALUE SPACE.
           02  F              PIC  X(008) VALUE X"1A26212068222176".
           02  F              PIC  N(008) VALUE "入　　金　　票　".
           02  F              PIC  X(008) VALUE X"1A26212068212078".
           02  F              PIC  X(036) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(010) VALUE
                "得　　意　　先　　名".
           02  F              PIC  X(063) VALUE SPACE.
       01   HEAD5.
           02  F              PIC  X(013).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(002).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(005).
           02  P-DF           PIC  X(001).
           02  P-TNC1         PIC  9(001).
           02  P-DV           PIC  X(001).
           02  P-DCC          PIC  9(001).
           02  P-DR           PIC  X(001).
           02  F              PIC  X(006).
           02  P-SKD          PIC  99/99/99.
           02  F              PIC  X(027).
       01  HEAD6.
           02  F              PIC  X(020) VALUE SPACE.
           02  F              PIC  N(004) VALUE "摘　　要".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(004) VALUE "手形期日".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(004) VALUE "金　　額".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "請求年月".
           02  F              PIC  X(023) VALUE SPACE.
       01  W-PM.
           02  F              PIC  X(013).
           02  P-NF           PIC  X(001).
           02  P-NC           PIC  9(002).
           02  P-NR           PIC  X(001).
           02  F              PIC  X(001).
           02  P-NCM          PIC  N(006).
           02  F              PIC  X(009).
           02  P-FF           PIC  X(001).
           02  P-FNO          PIC  9(006).
           02  P-FR           PIC  X(001).
           02  F              PIC  X(002).
           02  P-TDN          PIC  N(002).
           02  P-TDV1         PIC  X(001).
           02  P-TDG          PIC  N(002).
           02  P-TDV2         PIC  X(001).
           02  P-TDP          PIC  N(002).
           02  F              PIC  X(003).
           02  P-KIN          PIC  N(012).
           02  F              PIC  X(002).
           02  P-SSN          PIC  N(002).
           02  P-SSV          PIC  X(001).
           02  P-SSG          PIC  N(002).
           02  F              PIC  X(022).
       01  HEAD8.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(002) VALUE "備考".
           02  F              PIC  X(042) VALUE SPACE.
           02  F              PIC  N(004) VALUE "入　　金".
           02  F              PIC  X(010) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　消　費　税".
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  N(004) VALUE "合　　計".
           02  F              PIC  X(007) VALUE SPACE.
       01  W-PT.
           02  F              PIC  X(051).
           02  P-TKIN         PIC  N(012).
           02  F              PIC  X(001).
           02  P-SHZ          PIC  N(012).
           02  F              PIC  X(001).
           02  P-AKIN         PIC  N(012).
           02  F              PIC  X(002).
       01  HEAD9.
           02  F              PIC  X(013) VALUE SPACE.
           02  F              PIC  X(005) VALUE "* 00=".
           02  F              PIC  N(002) VALUE "現金".
           02  F              PIC  X(004) VALUE " 10=".
           02  F              PIC  N(002) VALUE "振込".
           02  F              PIC  X(004) VALUE " 20=".
           02  F              PIC  N(004) VALUE "小切手　".
           02  F              PIC  X(003) VALUE "30=".
           02  F              PIC  N(002) VALUE "約手".
           02  F              PIC  X(004) VALUE " 40=".
           02  F              PIC  N(002) VALUE "為手".
           02  F              PIC  X(004) VALUE " 50=".
           02  F              PIC  N(006) VALUE "買掛金相殺　".
           02  F              PIC  X(003) VALUE "60=".
           02  F              PIC  N(006) VALUE "その他相殺　".
           02  F              PIC  X(003) VALUE "58=".
           02  F              PIC  N(006) VALUE "買掛金消費税".
           02  F              PIC  X(004) VALUE " 79=".
           02  F              PIC  N(006) VALUE "消費税取消　".
           02  F              PIC  X(008) VALUE SPACE.
       01  W-DATA.
           02  W-SE.
             03  W-SDNO       PIC  9(006).
             03  W-EDNO       PIC  9(006).
           02  W-TCD          PIC  9(004).
           02  W-DMM          PIC  9(001).
           02  W-POC          PIC  9(001).
           02  W-TPC          PIC  9(001).
           02  W-DNO          PIC  9(006).
           02  W-SKD          PIC  9(006).
           02  W-AD.
             03  W-AKIN       PIC S9(009).
             03  W-TKIN       PIC S9(009).
             03  W-SHZ        PIC S9(007).
           02  CNT            PIC  9(001).
           02  W-Z.
             03  W-TDGZ       PIC Z9.
             03  W-TDPZ       PIC Z9.
             03  W-SSGZ       PIC Z9.
             03  W-KINZ       PIC ----,---,--9.
           02  W-KDC          PIC  9(001).
           02  W-NOC          PIC  9(001).
           02  W-DTH          PIC  9(001).
           02  W-DTK          PIC  9(001).
           02  W-KG           PIC  9(002).
           02  W-BC           PIC  9(001).
           02  W-END          PIC  9(001).
           COPY LSTAT.
      *
           COPY LIKKBM.
           COPY LITM.
           COPY LIHKBM.
      *FD  NYU-F
       01  NYU-F_HKD020.
           02  NYU-F_PNAME1   PIC  X(004) VALUE "NYUF".
           02  F              PIC  X(001).
           02  NYU-F_LNAME    PIC  X(012) VALUE "NYU-F_HKD020".
           02  F              PIC  X(001).
           02  NYU-F_KEY1     PIC  X(100) VALUE SPACE.
           02  NYU-F_SORT     PIC  X(100) VALUE SPACE.
           02  NYU-F_IDLST    PIC  X(100) VALUE SPACE.
           02  NYU-F_RES      USAGE  POINTER.
       01  NYU-R.
           02  N-DATE         PIC  9(008).
           02  N-TCD          PIC  9(004).
           02  N-KIN          PIC S9(008).
           02  N-NC           PIC  9(002).
           02  N-NSC          PIC  9(001).
           02  N-TD           PIC  9(008).
           02  N-SS           PIC  9(006).
           02  N-BC           PIC  9(001).
           02  N-TC           PIC  9(002).
           02  F              PIC  X(003).
           02  N-KEY.
             03  N-NO         PIC  9(006).
             03  N-GNO        PIC  9(001).
           02  F              PIC  X(032).
           02  N-DPC          PIC  9(001).
           02  N-ACT          PIC  9(001).
           02  N-PRC          PIC  9(001).
           02  F              PIC  X(017).
       77  F                  PIC  X(001).
      *FD  NYUW-F
       01  NYUW-F_HKD020.
           02  NYUW-F_PNAME1  PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  NYUW-F_LNAME   PIC  X(013) VALUE "NYUW-F_HKD020".
           02  F              PIC  X(001).
           02  NYUW-F_KEY1    PIC  X(100) VALUE SPACE.
           02  NYUW-F_SORT    PIC  X(100) VALUE SPACE.
           02  NYUW-F_IDLST   PIC  X(100) VALUE SPACE.
           02  NYUW-F_RES     USAGE  POINTER.
       01  NYUW-R.
           02  NW-DATE.
             03  F            PIC  9(002).
             03  NW-NEN       PIC  9(002).
             03  NW-GET       PIC  9(002).
             03  NW-PEY       PIC  9(002).
           02  NW-TCD         PIC  9(004).
           02  NW-KIN         PIC S9(008).
           02  NW-NC          PIC  9(002).
           02  NW-NCD   REDEFINES NW-NC.
             03  NW-NC1       PIC  9(001).
             03  NW-NC2       PIC  9(001).
           02  NW-NSC         PIC  9(001).
           02  NW-TD.
             03  F            PIC  9(002).
             03  NW-TDN       PIC  9(002).
             03  NW-TDG       PIC  9(002).
             03  NW-TDP       PIC  9(002).
           02  NW-SS.
             03  F            PIC  9(002).
             03  NW-SSN       PIC  9(002).
             03  NW-SSG       PIC  9(002).
           02  NW-BC          PIC  9(001).
           02  NW-TNC.
             03  NW-TNC1      PIC  9(001).
             03  NW-TNC2      PIC  9(001).
           02  F              PIC  X(003).
           02  NW-KEY.
             03  NW-NO        PIC  9(006).
             03  NW-GNO       PIC  9(001).
           02  NW-FDNO.
             03  NW-FNO       PIC  9(006).
             03  NW-FGNO      PIC  9(002).
           02  F              PIC  X(002).
           02  NW-SKD         PIC  9(006).
           02  NW-DCC         PIC  9(001).
           02  F              PIC  X(015).
           02  NW-DPC         PIC  9(001).
           02  NW-ACT         PIC  9(001).
           02  NW-PRC         PIC  9(001).
           02  F              PIC  X(043).
       77  F                  PIC  X(001).
      *
       77  SP-R               PIC  X(170).
      *
       77  ESTAT              PIC  X(002).
       77  RESU               PIC  9(001).
       77  RESP               PIC  9(001).
       77  RET                PIC  9(001) VALUE ZERO.
       77  USER_ID            PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE    PIC  X(003) VALUE ZERO.
      *
       01  C-CLEAR.
           02  01C-CLEAR  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　入　金　伝　票　発　行　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       01  C-DSP.
           02  D-SHM.
             03  FILLER  PIC  N(008) VALUE
                  "（当月分再発行）".
             03  FILLER  PIC  X(028) VALUE
                  "-----   用紙　セット   -----".
             03  FILLER  PIC  X(034) VALUE
                  "[  ﾃｽﾄ ﾌﾟﾘﾝﾄ  ｼﾅｲ=1 ｽﾙ=9   ﾘﾀｰﾝ  ]".
             03  FILLER  PIC  X(022) VALUE
                  "確認  OK=1 NO=9   ﾘﾀｰﾝ".
           02  D-DNM.
             03  FILLER  PIC  X(030) VALUE
                  "伝票№ 000000 より 999999 まで".
           02  D-TSM.
             03  FILLER  PIC  X(009) VALUE
                  "ｺｰﾄﾞ     ".
           02  D-NAME  PIC  N(026).
       01  C-ACP.
           02  A-TPC   PIC  9(001).
           02  FILLER.
             03  A-TCD   PIC  9(004).
             03  A-SDNO  PIC  9(006).
             03  A-EDNO  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-SME04 PIC  N(017) VALUE
                  "＊＊＊　　他で入金入力中　　＊＊＊".
             03  D-SME13 PIC  N(017) VALUE
                  "＊＊＊　　他で日次更新中　　＊＊＊".
             03  D-SME15 PIC  N(017) VALUE
                  "＊＊＊　　他で月次更新中　　＊＊＊".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME7   PIC  X(017) VALUE
                  "***  KKBM ﾅｼ  ***".
             03  E-ME8   PIC  X(026) VALUE
                  "***  KKBM REWRITE ｴﾗｰ  ***".
             03  E-ME11  PIC  X(017) VALUE
                  "***  HKBM ﾅｼ  ***".
             03  E-ME12  PIC  X(018) VALUE
                  "***  ﾌﾞﾓﾝ ｴﾗｰ  ***".
             03  E-ME13  PIC  X(017) VALUE
                  "***  NYUF ﾅｼ  ***".
             03  E-ME14  PIC  X(026) VALUE
                  "***  NYUF REWRITE ｴﾗｰ  ***".
             03  E-ME15  PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME16  PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME90  PIC  N(022) VALUE
                  "【　　工品区分マスターを修正して下さい　　】".
             03  E-KEY   PIC  X(006).
             03  E-TCD   PIC  X(004).
           COPY LSSEM.
       PROCEDURE DIVISION.
      *Initialize
       CALL "SD_Initialize" RETURNING RESU.
       CALL "PR_Initialize" USING "999-FHK020" RETURNING RESP.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
      *C-CLEAR
       CALL "SD_Init" USING
           "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID" "N" "3" "18" "42" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID" "N" "4" "18" "42" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID" "N" "5" "18" "42" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID" "N" "6" "18" "42" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID" "N" "7" "18" "42" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID" "N" "8" "18" "42" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "N" "9" "18" "42" "06C-MID" " " RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "294" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SHM" " " "0" "0" "100" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-SHM" "N" "7" "31" "16" " " "D-SHM" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-SHM" "bX" "12" "28" "28" "01D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03D-SHM" "X" "14" "23" "34" "02D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "04D-SHM" "X" "20" "38" "22" "03D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-DNM" " " "0" "0" "30" "D-SHM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-DNM" "X" "16" "24" "30" " " "D-DNM" RETURNING RESU.
       CALL "SD_Init" USING
           "D-TSM" " " "0" "0" "9" "D-DNM" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-TSM" "X" "16" "14" "9" " " "D-TSM" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "16" "24" "52" "D-TSM" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE T-NAME "52" "0" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "18" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "14" "49" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "16" "0" "16" "A-TPC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "16" "19" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SDNO" "9" "16" "31" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SDNO" BY REFERENCE W-SDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EDNO" "9" "16" "43" "6" "A-SDNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EDNO" BY REFERENCE W-EDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "20" "55" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "102" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "15" "0" "102" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SME04" "bN" "15" "18" "34" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SME13" "bN" "15" "18" "34" "D-SME04" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SME15" "bN" "15" "18" "34" "D-SME13" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "228" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "228" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME7" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "26" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "X" "24" "15" "17" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME12" "X" "24" "15" "18" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME13" "X" "24" "15" "17" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME14" "X" "24" "15" "26" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME15" "X" "24" "15" "17" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME16" "X" "24" "15" "18" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME90" "N" "24" "15" "44" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "65" "6" "E-ME90" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE NW-NO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-TCD" "X" "24" "60" "4" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-TCD" BY REFERENCE T-KEY "4" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           MOVE ZERO TO W-DATA.
           MOVE 999999 TO W-EDNO.
           IF  JS-SIGN NOT = 0
               CALL "SD_Output" USING "D-SHM" D-SHM "p" RETURNING RESU
               IF  JS-SIGN = 1
                   CALL "SD_Output" USING
                    "D-DNM" D-DNM "p" RETURNING RESU
               ELSE
                   IF  JS-SIGN = 2
                       CALL "SD_Output" USING
                        "D-TSM" D-TSM "p" RETURNING RESU
                   END-IF
               END-IF
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
      *
           IF  JS-SIGN NOT = 0
               GO TO M-10
           END-IF
           PERFORM KDC-RTN THRU KDC-EX.
           IF  W-END = 9
               GO TO M-95
           END-IF
           GO TO M-40.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               IF  W-POC = 0
                   GO TO M-95
               ELSE
                   CALL "PR_Close" RETURNING RESP
                   GO TO M-95
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-10
           END-IF
           IF  W-TPC = 1
               IF  JS-SIGN = 0
                   GO TO M-40
               ELSE
                   IF  JS-SIGN = 1
                       GO TO M-20
                   ELSE
                       GO TO M-30
                   END-IF
               END-IF
           END-IF
           IF  W-TPC NOT = 9
               GO TO M-10
           END-IF
      *
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
               MOVE SPACE TO HEAD5 W-PM W-PT
               MOVE 999999 TO P-NO
               MOVE 99 TO P-NEN P-GET P-PEY
               MOVE "9999" TO P-TCD
               MOVE ALL "Ｎ" TO P-NAME
               MOVE "　９９９，９９９，９９９" TO P-TKIN P-AKIN
               MOVE "　　９９，９９９，９９９" TO P-SHZ
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           MOVE 0 TO CNT.
       M-15.
           ADD 1 TO CNT.
           IF  CNT = 9
               MOVE SPACE TO SP-R
               MOVE HEAD8 TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE W-PT TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               MOVE HEAD9 TO SP-R
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
               CALL "PR_LineFeed" USING "2" RETURNING RESP
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO M-10
           END-IF
           IF  CNT = 1
               MOVE SPACE TO W-PM
               MOVE SPACE TO P-NCM P-TDN P-TDG P-TDP P-KIN P-SSN P-SSG
               MOVE "(" TO P-NF P-FF
               MOVE ")" TO P-NR P-FR
               MOVE 99 TO P-NC
               MOVE "９９" TO P-TDN P-TDG P-TDP P-SSN P-SSG
               MOVE "ＮＮＮＮＮＮ" TO P-NCM
               MOVE 999999 TO P-FNO
               MOVE "/" TO P-TDV1 P-TDV2 P-SSV
               MOVE "　９９９，９９９，９９９" TO P-KIN
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PM TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO M-15.
       M-20.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10.
           IF  ESTAT = PF9
               IF  W-POC = 0
                   GO TO M-95
               ELSE
                   CALL "PR_Close" RETURNING RESP
                   GO TO M-95
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-20
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-20
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-25
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO M-25
           END-IF
           GO TO M-35.
       M-30.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-10
           END-IF
           IF  ESTAT = PF9
               GO TO M-90
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-30
           END-IF
           IF  W-TCD = 9999
               GO TO M-90
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               GO TO M-30
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
       M-35.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 1
                   GO TO M-25
               ELSE
                   GO TO M-30
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-35
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 1
                   GO TO M-20
               ELSE
                   GO TO M-30
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-35
           END-IF.
       M-40.
           IF  W-NOC = 0
               MOVE 1 TO W-NOC
               CALL "CBLSTNNO" USING STN-NO USER_ID
               MOVE STN-NO2 TO W-FID2
               MOVE W-FID TO WK0128ID
               MOVE WK0128ID TO NYUW-F_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" NYUW-F_PNAME1 " " BY REFERENCE NYUW-F_IDLST "0"
               CALL "DB_F_Open" USING
                "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
                "HKB-KEY" BY REFERENCE HKB-KEY
               IF JS-SIGN = 0
                   CALL "DB_F_Open" USING
                    "I-O" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
                    "N-KEY" BY REFERENCE N-KEY
               END-IF
           END-IF.
       M-45.
      *           READ NYUW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUW-F_PNAME1 BY REFERENCE NYUW-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JS-SIGN = 0
               IF  NW-DPC NOT = 0
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN NOT = 0
               IF  NW-DPC = 0
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  NW-NO < W-SDNO OR > W-EDNO
                   GO TO M-45
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  NW-TCD NOT = W-TCD
                   GO TO M-45
               END-IF
           END-IF.
       M-50.
           IF  NW-GNO NOT = 1
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE NW-NO TO W-DNO.
           MOVE ZERO TO W-AD.
           MOVE NW-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO T-NAME
           END-IF
           IF  NW-BC NOT = T-BC
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           MOVE NW-BC TO W-BC.
           IF  JS-SIGN = 0
               GO TO M-55
           END-IF
           IF  W-POC = 0
               MOVE 1 TO W-POC
               CALL "PR_Open" RETURNING RESP
           END-IF
           MOVE SPACE TO HEAD5.
           MOVE W-DNO TO P-NO.
           MOVE NW-NEN TO P-NEN.
           MOVE NW-GET TO P-GET.
           MOVE NW-PEY TO P-PEY.
           MOVE NW-TCD TO P-TCD.
           MOVE T-NAME TO P-NAME.
           MOVE "(" TO P-DF.
           MOVE NW-TNC1 TO P-TNC1.
           MOVE "-" TO P-DV.
           MOVE NW-DCC TO P-DCC.
           MOVE ")" TO P-DR.
           IF  NW-SKD NOT = ZERO
               MOVE NW-SKD TO P-SKD
           END-IF
           PERFORM MID-RTN THRU MID-EX.
       M-55.
           MOVE 0 TO CNT.
       M-60.
           ADD 1 TO CNT.
           IF  CNT = 9
               GO TO M-75
           END-IF
           IF  NW-BC NOT = W-BC
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  JS-SIGN NOT = 0
               GO TO M-65
           END-IF
           MOVE NW-KEY TO N-KEY.
      *           READ NYU-F INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-65.
           IF  JS-SIGN NOT = 0
               PERFORM PRI-RTN THRU PRI-EX
           ELSE
               MOVE 1 TO N-DPC
      *               REWRITE NYU-R INVALID KEY
      *///////////////
               CALL "DB_Update" USING
                NYU-F_PNAME1 NYU-F_LNAME NYU-R RETURNING RET
               IF  RET = 1
                   CALL "SD_Output" USING
                    "E-STAT" E-STAT "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME14" E-ME14 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-KEY" E-KEY "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-90
               END-IF
           END-IF.
       M-70.
      *           READ NYUW-F AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" NYUW-F_PNAME1 BY REFERENCE NYUW-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-85
           END-IF
           IF  JS-SIGN = 0
               IF  NW-DPC NOT = 0
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN NOT = 0
               IF  NW-DPC = 0
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  NW-NO < W-SDNO OR > W-EDNO
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  NW-TCD NOT = W-TCD
                   GO TO M-70
               END-IF
           END-IF
           IF  NW-NO = W-DNO
               GO TO M-60
           END-IF.
       M-75.
           IF  JS-SIGN NOT = 0
               PERFORM KEI-RTN THRU KEI-EX
           END-IF
           GO TO M-50.
       M-85.
           IF  JS-SIGN NOT = 0
               PERFORM KEI-RTN THRU KEI-EX
           END-IF.
       M-90.
           IF  ESTAT NOT = PF9
               IF  JS-SIGN = 2
                   IF  W-TCD NOT = 9999
                       CALL "SD_Output" USING
                        "C-CLEAR" C-CLEAR "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "C-MID" C-MID "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "D-SHM" D-SHM "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "D-TSM" D-TSM "p" RETURNING RESU
                       CALL "DB_F_Close" USING
                        BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1
                       CALL "DB_F_Open" USING
                        "INPUT" NYUW-F_PNAME1 " " BY REFERENCE
                        NYUW-F_IDLST "0"
                       GO TO M-30
                   END-IF
               END-IF
           END-IF
           IF  W-POC NOT = 0
               CALL "PR_Close" RETURNING RESP
           END-IF
           IF  W-NOC = 1
               CALL "DB_F_Close" USING
                BY REFERENCE NYUW-F_IDLST NYUW-F_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               IF  JS-SIGN = 0
                   CALL "DB_F_Close" USING
                    BY REFERENCE NYU-F_IDLST NYU-F_PNAME1
               END-IF
           END-IF.
       M-95.
           IF  W-KDC NOT = 0
               PERFORM KKB2-RTN THRU KKB2-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       KDC-RTN.
           CALL "DB_F_Open" USING
            "INPUT" NYU-F_PNAME1 " " BY REFERENCE NYU-F_IDLST "1"
            "N-KEY" BY REFERENCE N-KEY.
       KDC-010.
      *           READ NYU-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" NYU-F_PNAME1 BY REFERENCE NYU-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KDC-020
           END-IF
           IF  N-DPC NOT = 0
               GO TO KDC-010
           END-IF
           IF  N-BC = 0
               IF  W-DTH = 0
                   MOVE 1 TO W-DTH
               END-IF
           END-IF
           IF  N-BC NOT = 0
               MOVE 1 TO W-KDC
               IF  W-DTK = 0
                   MOVE 1 TO W-DTK
               END-IF
           END-IF
           GO TO KDC-010.
       KDC-020.
           CALL "DB_F_Close" USING
            BY REFERENCE NYU-F_IDLST NYU-F_PNAME1.
           IF  W-KDC NOT = 0
               PERFORM KKB1-RTN THRU KKB1-EX
           END-IF.
       KDC-EX.
           EXIT.
      *-------------  ＫＫＢＭ　チェック　------------------------------
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
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO KKB1-020
           END-IF
           IF  KKB-SC15 = 1
               CALL "SD_Output" USING
                "D-SME15" D-SME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO KKB1-020
           END-IF
           IF  KKB-SC13 = 1
               CALL "SD_Output" USING
                "D-SME13" D-SME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO KKB1-020
           END-IF
           IF  KKB-SC04 = 1
               CALL "SD_Output" USING
                "D-SME04" D-SME04 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO KKB1-020
           END-IF
           MOVE 1 TO KKB-SC04.
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
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
           END-IF.
       KKB1-020.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB1-EX.
           EXIT.
      *-------------  見出し　印字　------------------------------------
       MID-RTN.
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
           MOVE HEAD4 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
      *-------------　合計　印字　--------------------------------------
       KEI-RTN.
           IF  CNT > 7
               MOVE 2 TO W-KG
           ELSE
               COMPUTE W-KG = 18 - (CNT * 2)
           END-IF
           MOVE SPACE TO SP-R.
           MOVE HEAD8 TO SP-R.
           CALL "PR_LineFeed" USING W-KG RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE SPACE TO W-PT.
           MOVE W-TKIN TO W-KINZ.
           MOVE W-KINZ TO P-TKIN.
           MOVE W-SHZ TO W-KINZ.
           MOVE W-KINZ TO P-SHZ.
           MOVE W-AKIN TO W-KINZ.
           MOVE W-KINZ TO P-AKIN.
           MOVE W-PT TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           MOVE SPACE TO SP-R.
           MOVE HEAD9 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       KEI-EX.
           EXIT.
      *-------------  ＫＫＢＭ　開放　----------------------------------
       KKB2-RTN.
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
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               GO TO KKB2-EX
           END-IF
           MOVE 0 TO KKB-SC04.
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
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME90" E-ME90 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
       KKB2-EX.
           EXIT.
       PRI-RTN.
           IF  NW-NC = "60"
               GO TO PRI-010
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE NW-NC1 TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NKNA
           END-IF
           GO TO PRI-020.
       PRI-010.
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE NW-NSC TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-NSNA
           END-IF.
       PRI-020.
           MOVE SPACE TO W-PM.
           MOVE SPACE TO P-NCM P-TDN P-TDG P-TDP P-KIN P-SSN P-SSG.
           MOVE "(" TO P-NF.
           MOVE ")" TO P-NR.
           MOVE NW-NC TO P-NC.
           IF  NW-NC2 = 8
               MOVE "消　費　税　" TO P-NCM
           ELSE
               IF  NW-NC = "60"
                   MOVE HKB-NSNA TO P-NCM
               ELSE
                   MOVE HKB-NKNA TO P-NCM
               END-IF
           END-IF
           IF  NW-TD NOT = ZERO
               MOVE NW-TDG TO W-TDGZ
               MOVE NW-TDP TO W-TDPZ
               MOVE "/" TO P-TDV1 P-TDV2
               MOVE NW-TDN TO P-TDN
               MOVE W-TDGZ TO P-TDG
               MOVE W-TDPZ TO P-TDP
           END-IF
           MOVE NW-KIN TO W-KINZ.
           MOVE W-KINZ TO P-KIN.
           IF  NW-SS NOT = ZERO
               MOVE NW-SSG TO W-SSGZ
               MOVE "/" TO P-SSV
               MOVE NW-SSN TO P-SSN
               MOVE W-SSGZ TO P-SSG
           END-IF
           IF  NW-FNO NOT = ZERO
               MOVE "(" TO P-FF
               MOVE ")" TO P-FR
               MOVE NW-FNO TO P-FNO
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-PM TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD NW-KIN TO W-AKIN.
           IF  NW-NC2 > 7
               ADD NW-KIN TO W-SHZ
           ELSE
               ADD NW-KIN TO W-TKIN
           END-IF.
       PRI-EX.
           EXIT.
