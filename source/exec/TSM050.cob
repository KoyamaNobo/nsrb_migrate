       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSM050.
      ************************************************
      *****     受手・支手マスター　修正入力     *****
      ************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WU-R.
           02  WU-KEY         PIC  X(004).
           02  WU-1           PIC  9(002).
           02  WU-2           PIC  9(002).
           02  WU-3           PIC  9(004).
           02  WU-4           PIC  9(002).
           02  WU-5           PIC  9(001).
           02  WU-6           PIC  9(002).
           02  WU-7           PIC  9(004).
           02  WU-9           PIC S9(010).
           02  WU-UNGP.
             03  WU-UN        PIC  9(002).
             03  WU-UG        PIC  9(002).
             03  WU-UP        PIC  9(002).
           02  WU-10    REDEFINES WU-UNGP  PIC  9(006).
           02  WU-11          PIC  9(006).
           02  WU-12          PIC  9(006).
           02  WU-MNGP.
             03  WU-MN        PIC  9(002).
             03  WU-MG        PIC  9(002).
             03  WU-MP        PIC  9(002).
           02  WU-13    REDEFINES WU-MNGP  PIC  9(006).
           02  WU-INGP.
             03  WU-IN        PIC  9(002).
             03  WU-IG        PIC  9(002).
             03  WU-IP        PIC  9(002).
           02  WU-14    REDEFINES WU-INGP  PIC  9(006).
           02  WU-15          PIC  9(004).
           02  WU-8           PIC  N(024).
           02  WU-8X    REDEFINES WU-8     PIC  X(048).
           02  F              PIC  X(039).
           02  WU-OKD         PIC  9(006).
           02  WU-SNU         PIC  9(004).
           02  WU-SNM         PIC  9(004).
           02  WU-SNI         PIC  9(004).
       01  WS-R.
           02  WS-KEY         PIC  X(004).
           02  WS-1           PIC  9(002).
           02  WS-2           PIC  9(002).
           02  WS-3           PIC  9(004).
           02  WS-4           PIC  9(002).
           02  WS-5           PIC  9(004).
           02  WS-6           PIC  9(010).
           02  WS-FNGP.
             03  WS-FN        PIC  9(002).
             03  WS-FG        PIC  9(002).
             03  WS-FP        PIC  9(002).
           02  WS-7     REDEFINES WS-FNGP  PIC  9(006).
           02  WS-MNGP.
             03  WS-MN        PIC  9(002).
             03  WS-MG        PIC  9(002).
             03  WS-MP        PIC  9(002).
           02  WS-8     REDEFINES WS-MNGP  PIC  9(006).
           02  WS-12          PIC  9(008).
           02  WS-14          PIC  9(008).
           02  WS-46          PIC  9(008).
           02  WS-20          PIC  9(008).
           02  WS-22          PIC  9(008).
           02  WS-34          PIC  9(008).
           02  WS-52          PIC  9(008).
           02  F              PIC  X(024).
           02  WS-SNF         PIC  9(004).
           02  WS-SNM         PIC  9(004).
       01  W-DATA.
           02  W-KEY          PIC  9(004).
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-NO           PIC  9(002).
           02  W-TNA          PIC  N(026).
           02  W-KIN          PIC  9(010).
           02  W-KBN          PIC  N(005).
           02  W-SKB          PIC  N(004).
           02  W-BKN.
             03  W-BNA        PIC  N(008).
             03  W-SNA        PIC  N(008).
           02  W-YBK.
             03  W-YBN        PIC  N(008).
             03  W-YSN        PIC  N(008).
           02  W-NGP.
             03  W-NEN        PIC  9(004).
             03  W-NENL  REDEFINES W-NEN.
               04  W-NEN1     PIC  9(002).
               04  W-NEN2     PIC  9(002).
             03  W-GET        PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  CHK            PIC  9(001).
           02  W-TCD.
             03  W-TCD1       PIC  9(001).
             03  W-TCD2       PIC  9(003).
           02  CNT            PIC  9(002).
           02  W-NAAD.
             03  W-NA    OCCURS  48  PIC  X(001).
           02  W-NAD   REDEFINES W-NAAD  PIC  X(048).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LICAL.
           COPY LITM.
           COPY LISM.
           COPY LIBANK.
           COPY LIUKET.
           COPY LISHIT.
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
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　受手・支手マスター　修正　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(024) VALUE
                  "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(050) VALUE
                "[   受取手形ﾏｽﾀｰ = 1  支払手形ﾏｽﾀｰ = 2    ﾘﾀｰﾝ   ]".
       01  C-UM.
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　受取手形マスター　修正　　＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "手形番号          終わり=PF9     NO.".
           02  FILLER  PIC  X(011) VALUE " 1.手形区分".
           02  FILLER  PIC  X(011) VALUE " 2.処理区分".
           02  FILLER  PIC  X(011) VALUE " 3.銀行ｺｰﾄﾞ".
           02  FILLER  PIC  X(011) VALUE " 4.担当ｺｰﾄﾞ".
           02  FILLER  PIC  X(011) VALUE " 5.部門ｺｰﾄﾞ".
           02  FILLER  PIC  X(011) VALUE " 6.府県ｺｰﾄﾞ".
           02  FILLER  PIC  X(013) VALUE " 7.取引先ｺｰﾄﾞ".
           02  FILLER.
             03  FILLER  PIC  X(003) VALUE " 8.".
             03  FILLER  PIC  N(001) VALUE   "裏".
             03  FILLER  PIC  N(001) VALUE   "書".
             03  FILLER  PIC  N(001) VALUE   "人".
           02  FILLER  PIC  X(011) VALUE " 9.金　　額".
           02  FILLER  PIC  X(011) VALUE "10.日　　付".
           02  FILLER  PIC  X(011) VALUE "11.振 出 日".
           02  FILLER  PIC  X(011) VALUE "12.引 受 日".
           02  FILLER  PIC  X(011) VALUE "13.満 期 日".
           02  FILLER  PIC  X(011) VALUE "14.異 動 日".
           02  FILLER  PIC  X(011) VALUE "15.割引銀行".
           02  FILLER  PIC  X(027) VALUE
                "<  確認 OK=1 NO=9   ﾘﾀｰﾝ  >".
       01  C-SM.
           02  FILLER  PIC  N(021) VALUE
                  "＊＊＊　　支払手形マスター　修正　　＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "手形番号          終わり=PF9     NO.".
           02  FILLER  PIC  X(011) VALUE " 1.手形区分".
           02  FILLER  PIC  X(011) VALUE " 2.処理区分".
           02  FILLER  PIC  X(011) VALUE " 3.銀行ｺｰﾄﾞ".
           02  FILLER  PIC  X(011) VALUE " 4.府県ｺｰﾄﾞ".
           02  FILLER  PIC  X(013) VALUE " 5.取引先ｺｰﾄﾞ".
           02  FILLER  PIC  X(011) VALUE " 6.金　　額".
           02  FILLER  PIC  X(011) VALUE " 7.振 出 日".
           02  FILLER  PIC  X(011) VALUE " 8.満 期 日".
           02  FILLER  PIC  X(011) VALUE "12.材　　料".
           02  FILLER  PIC  X(011) VALUE "14.仕入商品".
           02  FILLER  PIC  X(011) VALUE "46.設　　備".
           02  FILLER  PIC  X(011) VALUE "20.外　　注".
           02  FILLER  PIC  X(011) VALUE "22.製造経費".
           02  FILLER  PIC  X(011) VALUE "34.営業経費".
           02  FILLER  PIC  X(011) VALUE "52.そ の 他".
           02  FILLER  PIC  X(027) VALUE
                "<  確認 OK=1 NO=9   ﾘﾀｰﾝ  >".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-KEY   PIC  9(004).
             03  A-NO    PIC  9(002).
           02  FILLER.
             03  A-U1    PIC  9(002).
             03  A-S1    PIC  9(002).
           02  FILLER.
             03  A-U2    PIC  9(002).
             03  A-S2    PIC  9(002).
           02  FILLER.
             03  A-U3    PIC  9(004).
             03  A-S3    PIC  9(004).
           02  FILLER.
             03  A-U4    PIC  9(002).
             03  A-S4    PIC  9(002).
           02  FILLER.
             03  A-U5    PIC  9(001).
             03  A-S5    PIC  9(004).
           02  FILLER.
             03  A-U6    PIC  9(002).
             03  A-S6    PIC S9(010).
           02  FILLER.
             03  A-U7    PIC  9(004).
             03  A-S7    PIC  9(006).
           02  FILLER.
             03  A-U8    PIC  N(024).
             03  A-S8    PIC  9(006).
           02  FILLER.
             03  A-U9    PIC  9(010).
             03  A-S12   PIC S9(008).
           02  FILLER.
             03  A-U10   PIC  9(006).
             03  A-S14   PIC S9(008).
           02  FILLER.
             03  A-U11   PIC  9(006).
             03  A-S46   PIC S9(008).
           02  FILLER.
             03  A-U12   PIC  9(006).
             03  A-S20   PIC S9(008).
           02  FILLER.
             03  A-U13   PIC  9(006).
             03  A-S22   PIC S9(008).
           02  FILLER.
             03  A-U14   PIC  9(006).
             03  A-S34   PIC S9(008).
           02  FILLER.
             03  A-U15   PIC  9(004).
             03  A-S52   PIC S9(008).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-KBM   PIC  N(005).
           02  D-SKB   PIC  N(004).
           02  D-BKN.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  N(008).
           02  D-STNA  PIC  N(026).
           02  D-SKIN  PIC ZZZZZZZZZ9- .
           02  D-UTNA  PIC  N(026).
           02  FILLER.
             03  D-UKIN  PIC ZZZZZZZZZ9 .
             03  D-S12   PIC ZZZZZZZ9- .
           02  D-S14   PIC ZZZZZZZ9- .
           02  D-S46   PIC ZZZZZZZ9- .
           02  D-S20   PIC ZZZZZZZ9- .
           02  D-S22   PIC ZZZZZZZ9- .
           02  D-S34   PIC ZZZZZZZ9- .
           02  FILLER.
             03  D-S52   PIC ZZZZZZZ9- .
             03  D-YBK.
               04  D-YBN   PIC  N(008).
               04  D-YSN   PIC  N(008).
       01  C-SPC.
           02  S-U11   PIC  X(006) VALUE "      ".
           02  S-U12   PIC  X(006) VALUE "      ".
           02  S-U14   PIC  X(006) VALUE "      ".
           02  S-U15   PIC  X(004) VALUE "    ".
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(018) VALUE
                  "***  UKETM ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  SHITM ﾅｼ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  BANKM ﾅｼ  ***".
             03  E-ME4   PIC  X(015) VALUE
                  "***  TM ﾅｼ  ***".
             03  E-ME5   PIC  X(015) VALUE
                  "***  SM ﾅｼ  ***".
             03  E-ME7   PIC  X(020) VALUE
                  "***  ｲﾄﾞｳﾋﾞ ｴﾗｰ  ***".
             03  E-ME8   PIC  X(020) VALUE
                  "***  ﾋｷｳｹﾋﾞ ｴﾗｰ  ***".
             03  E-ME9   PIC  X(021) VALUE
                  "***  ﾌﾘﾀﾞｼﾋﾞ ｴﾗｰ  ***".
             03  E-ME10  PIC  X(025) VALUE
                  "***  ﾜﾘﾋﾞｷ ｷﾞﾝｺｳ ｴﾗｰ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  ｷﾝｶﾞｸ ｴﾗｰ  ***".
             03  E-ME12  PIC  X(025) VALUE
                  "***  ｺﾓｼﾞ ﾉ ｽﾍﾟ-ｽ ｱﾘ  ***".
             03  E-ME13  PIC  X(018) VALUE
                  "***  CALNM ﾅｼ  ***".
             03  E-ME20  PIC  X(027) VALUE
                  "***  UKETM REWRITE ｴﾗｰ  ***".
             03  E-ME21  PIC  X(027) VALUE
                  "***  SHITM REWRITE ｴﾗｰ  ***".
             03  E-ME22  PIC  X(024) VALUE
                  "***  TM REWRITE ｴﾗｰ  ***".
             03  E-NGP   PIC  9(008).
             03  E-ME78  PIC  N(002) VALUE   "連絡".
             03  E-ME98  PIC  X(005) VALUE  X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE  X"1B4205".
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
            "C-MID" " " "0" "0" "386" " " " " RETURNING RESU.
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
            "08C-MID" "X" "12" "9" "50" "07C-MID" " " RETURNING RESU.
      *C-UM
       CALL "SD_Init" USING 
            "C-UM" " " "0" "0" "270" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-UM" "N" "1" "16" "42" " " "C-UM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-UM" "X" "4" "12" "36" "01C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-UM" "X" "5" "9" "11" "02C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-UM" "X" "6" "9" "11" "03C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-UM" "X" "7" "9" "11" "04C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-UM" "X" "8" "9" "11" "05C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-UM" "X" "9" "9" "11" "06C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-UM" "X" "10" "9" "11" "07C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-UM" "X" "11" "9" "13" "08C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-UM" " " "12" "0" "9" "09C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0110C-UM" "X" "12" "9" "3" " " "10C-UM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0210C-UM" "N" "12" "12" "2" "0110C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0310C-UM" "N" "12" "15" "2" "0210C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0410C-UM" "N" "12" "18" "2" "0310C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-UM" "X" "13" "9" "11" "10C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-UM" "X" "14" "9" "11" "11C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-UM" "X" "15" "9" "11" "12C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "14C-UM" "X" "16" "9" "11" "13C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "15C-UM" "X" "17" "9" "11" "14C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "16C-UM" "X" "18" "9" "11" "15C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "17C-UM" "X" "19" "9" "11" "16C-UM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "18C-UM" "X" "22" "20" "27" "17C-UM" " " RETURNING RESU.
      *C-SM
       CALL "SD_Init" USING 
            "C-SM" " " "0" "0" "272" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-SM" "N" "1" "16" "42" " " "C-SM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-SM" "X" "4" "12" "36" "01C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-SM" "X" "5" "9" "11" "02C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-SM" "X" "6" "9" "11" "03C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-SM" "X" "7" "9" "11" "04C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-SM" "X" "8" "9" "11" "05C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-SM" "X" "9" "9" "13" "06C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-SM" "X" "10" "9" "11" "07C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-SM" "X" "11" "9" "11" "08C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-SM" "X" "12" "9" "11" "09C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-SM" "X" "13" "9" "11" "10C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-SM" "X" "14" "9" "11" "11C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-SM" "X" "15" "9" "11" "12C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "14C-SM" "X" "16" "9" "11" "13C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "15C-SM" "X" "17" "9" "11" "14C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "16C-SM" "X" "18" "9" "11" "15C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "17C-SM" "X" "19" "9" "11" "16C-SM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "18C-SM" "X" "22" "20" "27" "17C-SM" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "209" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "12" "50" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "4" "0" "6" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "4" "23" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "4" "48" "2" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "5" "0" "4" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U1" "9" "5" "23" "2" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U1" BY REFERENCE WU-1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S1" "9" "5" "23" "2" "A-U1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S1" BY REFERENCE WS-1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "6" "0" "4" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U2" "9" "6" "23" "2" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U2" BY REFERENCE WU-2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S2" "9" "6" "23" "2" "A-U2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S2" BY REFERENCE WS-2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "7" "0" "8" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U3" "9" "7" "23" "4" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U3" BY REFERENCE WU-3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S3" "9" "7" "23" "4" "A-U3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S3" BY REFERENCE WS-3 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-ACP" " " "8" "0" "4" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U4" "9" "8" "23" "2" " " "06C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U4" BY REFERENCE WU-4 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S4" "9" "8" "23" "2" "A-U4" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S4" BY REFERENCE WS-4 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "9" "0" "5" "06C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U5" "9" "9" "23" "1" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U5" BY REFERENCE WU-5 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S5" "9" "9" "23" "4" "A-U5" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S5" BY REFERENCE WS-5 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-ACP" " " "10" "0" "12" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U6" "9" "10" "23" "2" " " "08C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U6" BY REFERENCE WU-6 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S6" "S9" "10" "23" "10" "A-U6" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S6" BY REFERENCE WS-6 "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-ACP" " " "11" "0" "10" "08C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U7" "9" "11" "23" "4" " " "09C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U7" BY REFERENCE WU-7 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S7" "9" "11" "23" "6" "A-U7" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S7" BY REFERENCE WS-7 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-ACP" " " "12" "0" "54" "09C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U8" "N" "12" "29" "48" " " "10C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U8" BY REFERENCE WU-8 "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S8" "9" "12" "23" "6" "A-U8" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S8" BY REFERENCE WS-8 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-ACP" " " "13" "0" "18" "10C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U9" "9" "13" "23" "10" " " "11C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U9" BY REFERENCE WU-9 "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S12" "S9" "13" "25" "8" "A-U9" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S12" BY REFERENCE WS-12 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-ACP" " " "14" "0" "14" "11C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U10" "9" "14" "23" "6" " " "12C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U10" BY REFERENCE WU-10 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S14" "S9" "14" "25" "8" "A-U10" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S14" BY REFERENCE WS-14 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-ACP" " " "15" "0" "14" "12C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U11" "9" "15" "23" "6" " " "13C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U11" BY REFERENCE WU-11 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S46" "S9" "15" "25" "8" "A-U11" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S46" BY REFERENCE WS-46 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "14C-ACP" " " "16" "0" "14" "13C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U12" "9" "16" "23" "6" " " "14C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U12" BY REFERENCE WU-12 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S20" "S9" "16" "25" "8" "A-U12" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S20" BY REFERENCE WS-20 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "15C-ACP" " " "17" "0" "14" "14C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U13" "9" "17" "23" "6" " " "15C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U13" BY REFERENCE WU-13 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S22" "S9" "17" "25" "8" "A-U13" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S22" BY REFERENCE WS-22 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "16C-ACP" " " "18" "0" "14" "15C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U14" "9" "18" "23" "6" " " "16C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U14" BY REFERENCE WU-14 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S34" "S9" "18" "25" "8" "A-U14" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S34" BY REFERENCE WS-34 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "17C-ACP" " " "19" "0" "12" "16C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-U15" "9" "19" "23" "4" " " "17C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-U15" BY REFERENCE WU-15 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-S52" "S9" "19" "25" "8" "A-U15" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-S52" BY REFERENCE WS-52 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "22" "39" "1" "17C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "270" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KBM" "N" "5" "29" "10" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-KBM" BY REFERENCE W-KBN "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKB" "N" "6" "29" "8" "D-KBM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SKB" BY REFERENCE W-SKB "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BKN" " " "7" "0" "32" "D-SKB" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-BKN" "N" "7" "29" "16" " " "D-BKN" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-BKN" BY REFERENCE W-BNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-BKN" "N" "7" "47" "16" "01D-BKN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-BKN" BY REFERENCE W-SNA "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-STNA" "N" "9" "29" "52" "D-BKN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-STNA" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SKIN" "ZZZZZZZZZ9-" "10" "23" "11" "D-STNA" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-SKIN" BY REFERENCE WS-6 "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UTNA" "N" "11" "29" "52" "D-SKIN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UTNA" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "13" "0" "19" "D-UTNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UKIN" "ZZZZZZZZZ9" "13" "23" "10" " " "07C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UKIN" BY REFERENCE WU-9 "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S12" "ZZZZZZZ9-" "13" "25" "9" "D-UKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S12" BY REFERENCE WS-12 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S14" "ZZZZZZZ9-" "14" "25" "9" "07C-DSP" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S14" BY REFERENCE WS-14 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S46" "ZZZZZZZ9-" "15" "25" "9" "D-S14" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S46" BY REFERENCE WS-46 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S20" "ZZZZZZZ9-" "16" "25" "9" "D-S46" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S20" BY REFERENCE WS-20 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S22" "ZZZZZZZ9-" "17" "25" "9" "D-S20" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S22" BY REFERENCE WS-22 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S34" "ZZZZZZZ9-" "18" "25" "9" "D-S22" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S34" BY REFERENCE WS-34 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "13C-DSP" " " "19" "0" "41" "D-S34" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-S52" "ZZZZZZZ9-" "19" "25" "9" " " "13C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-S52" BY REFERENCE WS-52 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YBK" " " "19" "29" "32" "D-S52" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YBN" "N" "19" "29" "16" " " "D-YBK" RETURNING RESU.
       CALL "SD_From" USING 
            "D-YBN" BY REFERENCE W-YBN "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YSN" "N" "19" "47" "16" "D-YBN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-YSN" BY REFERENCE W-YSN "16" "0" RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-U11" "X" "15" "23" "6" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-U12" "X" "16" "23" "6" "S-U11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-U14" "X" "18" "23" "6" "S-U12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-U15" "X" "19" "23" "4" "S-U14" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "384" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "384" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING 
            "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "15" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "15" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "20" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "20" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "21" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "25" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "25" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "18" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "27" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "27" "E-ME20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "24" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NGP" "9" "24" "50" "8" "E-ME22" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NGP" BY REFERENCE W-NGP "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME78" "N" "24" "5" "4" "E-NGP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-020
           END-IF
           IF  W-ACT NOT = 1 AND 2
               GO TO M-020
           END-IF
           COPY LIBCPR.
           CALL "DB_F_Open" USING
            "INPUT" BANK-M_PNAME1 "SHARED" BY REFERENCE BANK-M_IDLST "1"
            "B-KEY" BY REFERENCE B-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CALNM_PNAME1 "SHARED" BY REFERENCE CALNM_IDLST "1"
            "CL-KEY" BY REFERENCE CL-KEY.
           IF  W-ACT = 2
               GO TO M-500
           END-IF
           CALL "DB_F_Open" USING
            "I-O" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           CALL "DB_F_Open" USING
            "I-O" UKET-M_PNAME1 "SHARED" BY REFERENCE UKET-M_IDLST "1"
            "UT-KEY" BY REFERENCE UT-KEY.
       M-100.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-UM" C-UM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE UKET-M_IDLST UKET-M_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           MOVE W-KEY TO UT-KEY.
      *           READ UKET-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" UKET-M_PNAME1 BY REFERENCE UKET-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           INITIALIZE WU-R.
           MOVE UKET-R TO WU-R.
           PERFORM S-05 THRU S-10.
           PERFORM S-15 THRU S-20.
           MOVE WU-7 TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "　＊＊＊　　得意先　無し　　＊＊＊" TO T-NAME
           END-IF
           MOVE T-NAME TO W-TNA.
           MOVE SPACE TO W-BKN W-YBK.
           MOVE WU-3 TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "ＢＡＮＫＭ　無し" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           IF  WU-15 = ZERO
               GO TO M-120
           END-IF
           MOVE WU-15 TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "ＢＡＮＫＭ　無し" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-BNA TO W-YBN.
           MOVE B-SNA TO W-YSN.
       M-120.
           CALL "SD_Output" USING "A-U1" A-U1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KBM" D-KBM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U2" A-U2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SKB" D-SKB "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U3" A-U3 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U4" A-U4 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U5" A-U5 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U6" A-U6 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U7" A-U7 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UTNA" D-UTNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U8" A-U8 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U10" A-U10 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-U13" A-U13 "p" RETURNING RESU.
           IF  WU-11 NOT = ZERO
               CALL "SD_Output" USING "A-U11" A-U11 "p" RETURNING RESU
           END-IF
           IF  WU-12 NOT = ZERO
               CALL "SD_Output" USING "A-U12" A-U12 "p" RETURNING RESU
           END-IF
           IF  WU-14 NOT = ZERO
               CALL "SD_Output" USING "A-U14" A-U14 "p" RETURNING RESU
           END-IF
           IF  WU-15 NOT = ZERO
               CALL "SD_Output" USING "A-U15" A-U15 "p" RETURNING RESU
               CALL "SD_Output" USING "D-YBK" D-YBK "p" RETURNING RESU
           END-IF.
       M-130.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-130
           END-IF
           IF  W-NO = 99
               GO TO M-340
           END-IF
           IF  W-NO < 1 OR > 15
               GO TO M-130
           END-IF.
       M-140.
           IF  W-NO NOT = 1
               GO TO M-150
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U1 "A-U1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF
           IF  WU-1 NOT = 10 AND 11 AND 12 AND 13
               GO TO M-140
           END-IF
           PERFORM S-05 THRU S-10.
           CALL "SD_Output" USING "D-KBM" D-KBM "p" RETURNING RESU.
           GO TO M-130.
       M-150.
           IF  W-NO NOT = 2
               GO TO M-160
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U2 "A-U2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-150
           END-IF
           IF  WU-2 NOT = 00 AND 19 AND 20 AND 32 AND
                          50 AND 60 AND 70 AND 90
               GO TO M-150
           END-IF
           PERFORM S-15 THRU S-20.
           CALL "SD_Output" USING "D-SKB" D-SKB "p" RETURNING RESU.
           GO TO M-130.
       M-160.
           IF  W-NO NOT = 3
               GO TO M-170
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U3 "A-U3" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           MOVE WU-3 TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-160
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           GO TO M-130.
       M-170.
           IF  W-NO NOT = 4
               GO TO M-180
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U4 "A-U4" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-170
           END-IF
           GO TO M-130.
       M-180.
           IF  W-NO NOT = 5
               GO TO M-190
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U5 "A-U5" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           IF  WU-5 NOT = ZERO AND 1 AND 2 AND 3
               GO TO M-180
           END-IF
           GO TO M-130.
       M-190.
           IF  W-NO NOT = 6
               GO TO M-200
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U6 "A-U6" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-190
           END-IF
           IF  WU-6 < 1 OR > 47
               GO TO M-190
           END-IF
           GO TO M-130.
       M-200.
           IF  W-NO NOT = 7
               GO TO M-240
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U7 "A-U7" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           MOVE WU-7 TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-200
           END-IF
           MOVE T-NAME TO W-TNA.
           CALL "SD_Output" USING "D-UTNA" D-UTNA "p" RETURNING RESU.
           GO TO M-130.
       M-240.
           IF  W-NO NOT = 8
               GO TO M-250
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U8 "A-U8" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           MOVE WU-8X TO W-NAD.
           IF  SPACE = W-NA(1) OR W-NA(3) OR W-NA(5) OR W-NA(7)
                        OR  W-NA(9) OR W-NA(11) OR W-NA(13) OR W-NA(15)
                        OR W-NA(17) OR W-NA(19) OR W-NA(21) OR W-NA(23)
                        OR W-NA(25) OR W-NA(27) OR W-NA(29) OR W-NA(31)
                        OR W-NA(33) OR W-NA(35) OR W-NA(37) OR W-NA(39)
                        OR W-NA(41) OR W-NA(43) OR W-NA(45) OR W-NA(47)
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-240
           END-IF
           GO TO M-130.
       M-250.
           IF  W-NO NOT = 9
               GO TO M-260
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U9 "A-U9" "9" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-250
           END-IF
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
           IF  WU-9 = ZERO
               GO TO M-250
           END-IF
           GO TO M-130.
       M-260.
           IF  W-NO NOT = 10
               GO TO M-270
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U10 "A-U10" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WU-10 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-260
           END-IF
           GO TO M-130.
       M-270.
           IF  W-NO NOT = 11
               GO TO M-280
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U11 "A-U11" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-270
           END-IF
           IF  WU-11 = ZERO
               CALL "SD_Output" USING "S-U11" S-U11 "p" RETURNING RESU
               GO TO M-130
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WU-11 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-270
           END-IF
           GO TO M-130.
       M-280.
           IF  W-NO NOT = 12
               GO TO M-290
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U12 "A-U12" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-280
           END-IF
           IF  WU-12 = ZERO
               CALL "SD_Output" USING "S-U12" S-U12 "p" RETURNING RESU
               GO TO M-130
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WU-12 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-280
           END-IF
           GO TO M-130.
       M-290.
           IF  W-NO NOT = 13
               GO TO M-320
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U13 "A-U13" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-290
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WU-13 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-290
           END-IF
           IF  CL-SJ = 0
               GO TO M-310
           END-IF.
       M-300.
      *           READ CALNM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" CALNM_PNAME1 BY REFERENCE CALN-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NGP" E-NGP "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-290
           END-IF
           IF  CL-SJ = 1
               GO TO M-300
           END-IF.
       M-310.
           MOVE CL-NGPS TO WU-OKD.
           GO TO M-130.
       M-320.
           IF  W-NO NOT = 14
               GO TO M-330
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-U14 "A-U14" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  WU-14 = ZERO
               CALL "SD_Output" USING "S-U14" S-U14 "p" RETURNING RESU
               GO TO M-130
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WU-14 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-320
           END-IF
           GO TO M-130.
       M-330.
           CALL "SD_Accept" USING BY REFERENCE A-U15 "A-U15" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-330
           END-IF
           IF  WU-15 = ZERO
               MOVE SPACE TO W-YBK
               CALL "SD_Output" USING "S-U15" S-U15 "p" RETURNING RESU
               CALL "SD_Output" USING "D-YBK" D-YBK "p" RETURNING RESU
               GO TO M-130
           END-IF
           MOVE WU-15 TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-330
           END-IF
           MOVE B-BNA TO W-YBN.
           MOVE B-SNA TO W-YSN.
           CALL "SD_Output" USING "D-YBK" D-YBK "p" RETURNING RESU.
           GO TO M-130.
       M-340.
           IF  WU-2 NOT = 32 AND 19 AND 20 AND 50
               GO TO M-360
           END-IF
           IF  WU-15 = ZERO
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-130
           END-IF
           IF  WU-11 = ZERO
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-130
           END-IF
           IF  WU-14 = ZERO
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-130
           END-IF
           IF  WU-1 = 12
               GO TO M-350
           END-IF
           IF  WU-12 NOT = ZERO
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-130
           END-IF
           GO TO M-360.
       M-350.
           IF  WU-12 = ZERO
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-130
           END-IF.
       M-360.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-130
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-360
           END-IF
           IF  W-DMM = 9
               GO TO M-130
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-360
           END-IF
      *
           MOVE ZERO TO W-NEN.
           MOVE WU-UN TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NEN TO WU-SNU.
           MOVE ZERO TO W-NEN.
           MOVE WU-MN TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NEN TO WU-SNM.
           MOVE ZERO TO W-NEN.
           MOVE WU-IN TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           IF  WU-14 = ZERO
               MOVE ZERO TO WU-SNI
           ELSE
               MOVE W-NEN TO WU-SNI
           END-IF
           MOVE WU-R TO UKET-R.
      *           REWRITE UKET-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            UKET-M_PNAME1 UKET-M_LNAME UKET-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
      *
           IF  WU-2 NOT = 60
               GO TO M-100
           END-IF
           IF  T-ENG NOT = ZERO
               GO TO M-100
           END-IF
           MOVE WU-7 TO T-KEY.
      *           READ T-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-100
           END-IF
           MOVE D-NTNG TO T-ENG.
      *           REWRITE T-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            T-M_PNAME1 T-M_LNAME T-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           GO TO M-100.
       M-500.
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "I-O" SHIT-M_PNAME1 "SHARED" BY REFERENCE SHIT-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
       M-510.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-SM" C-SM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "DB_F_Close" USING
                BY REFERENCE SHIT-M_IDLST SHIT-M_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1
               GO TO M-900
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-510
           END-IF
           MOVE W-KEY TO ST-KEY.
      *           READ SHIT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" SHIT-M_PNAME1 BY REFERENCE SHIT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-510
           END-IF
           INITIALIZE WS-R.
           MOVE SHIT-R TO WS-R.
           MOVE SPACE TO W-KBN.
           IF  WS-1 = 21
               MOVE   "支払約手　" TO W-KBN
           END-IF
           IF  WS-1 = 22
               MOVE   "支払為手　" TO W-KBN
           END-IF
           MOVE SPACE TO W-SKB.
           IF  WS-2 = 50
               MOVE   "決　　済" TO W-SKB
           END-IF
           IF  WS-2 = 80
               MOVE   "強制決済" TO W-SKB
           END-IF
           IF  WS-2 = 90
               MOVE   "取消し　" TO W-SKB
           END-IF
           MOVE WS-5 TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "＊＊＊　　仕入先　無し　　＊＊＊　" TO S-NAME
           END-IF
           MOVE SPACE TO W-TNA.
           MOVE S-NAME TO W-TNA.
           MOVE SPACE TO W-BKN.
           MOVE WS-3 TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE   "ＢＡＮＫＭ　無し" TO B-BNA
               MOVE SPACE TO B-SNA
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           CALL "SD_Output" USING "A-S1" A-S1 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KBM" D-KBM "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S2" A-S2 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SKB" D-SKB "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S3" A-S3 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S4" A-S4 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S5" A-S5 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-STNA" D-STNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SKIN" D-SKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S7" A-S7 "p" RETURNING RESU.
           CALL "SD_Output" USING "A-S8" A-S8 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S12" D-S12 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S14" D-S14 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S46" D-S46 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S20" D-S20 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S22" D-S22 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S34" D-S34 "p" RETURNING RESU.
           CALL "SD_Output" USING "D-S52" D-S52 "p" RETURNING RESU.
       M-530.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-510
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-530
           END-IF
           IF  W-NO = 99
               GO TO M-690
           END-IF
           IF  W-NO < 1 OR > 52
               GO TO M-530
           END-IF.
       M-540.
           IF  W-NO NOT = 1
               GO TO M-550
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S1 "A-S1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF
           IF  WS-1 NOT = 22
               GO TO M-540
           END-IF
           MOVE   "支払為手　" TO W-KBN.
           CALL "SD_Output" USING "D-KBM" D-KBM "p" RETURNING RESU.
           GO TO M-530.
       M-550.
           IF  W-NO NOT = 2
               GO TO M-560
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S2 "A-S2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-550
           END-IF
           IF  WS-2 NOT = 00 AND 50 AND 80 AND 90
               GO TO M-550
           END-IF
           MOVE SPACE TO W-SKB.
           IF  WS-2 = 50
               MOVE   "決　　済" TO W-SKB
           END-IF
           IF  WS-2 = 80
               MOVE   "強制決済" TO W-SKB
           END-IF
           IF  WS-2 = 90
               MOVE   "取消し　" TO W-SKB
           END-IF
           CALL "SD_Output" USING "D-SKB" D-SKB "p" RETURNING RESU.
           GO TO M-530.
       M-560.
           IF  W-NO NOT = 3
               GO TO M-570
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S3 "A-S3" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-560
           END-IF
           MOVE WS-3 TO B-KEY.
      *           READ BANK-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BANK-M_PNAME1 BY REFERENCE BANK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-560
           END-IF
           MOVE B-BNA TO W-BNA.
           MOVE B-SNA TO W-SNA.
           CALL "SD_Output" USING "D-BKN" D-BKN "p" RETURNING RESU.
           GO TO M-530.
       M-570.
           IF  W-NO NOT = 4
               GO TO M-580
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S4 "A-S4" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-570
           END-IF
           IF  WS-4 < 1 OR > 47
               GO TO M-570
           END-IF
           GO TO M-530.
       M-580.
           IF  W-NO NOT = 5
               GO TO M-590
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S5 "A-S5" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-580
           END-IF
           IF  WS-5 > 4999
               GO TO M-580
           END-IF
           MOVE WS-5 TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-580
           END-IF
           MOVE SPACE TO W-TNA.
           MOVE S-NAME TO W-TNA.
           CALL "SD_Output" USING "D-STNA" D-STNA "p" RETURNING RESU.
           GO TO M-530.
       M-590.
           IF  W-NO NOT = 6
               GO TO M-600
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S6 "A-S6" "S9" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-590
           END-IF
           IF  WS-6 = ZERO
               GO TO M-590
           END-IF
           CALL "SD_Output" USING "D-SKIN" D-SKIN "p" RETURNING RESU.
           GO TO M-530.
       M-600.
           IF  W-NO NOT = 7
               GO TO M-610
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S7 "A-S7" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-600
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WS-7 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-600
           END-IF
           GO TO M-530.
       M-610.
           IF  W-NO NOT = 8
               GO TO M-620
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S8 "A-S8" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-610
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE WS-8 TO W-NGPS.
           PERFORM S-25 THRU S-35.
           IF  CHK = 5
               GO TO M-610
           END-IF
           IF  CL-SJ = 1
               GO TO M-610
           END-IF
           GO TO M-530.
       M-620.
           IF  W-NO NOT = 12
               GO TO M-630
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S12 "A-S12" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-620
           END-IF
           CALL "SD_Output" USING "D-S12" D-S12 "p" RETURNING RESU.
           GO TO M-530.
       M-630.
           IF  W-NO NOT = 14
               GO TO M-640
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S14 "A-S14" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-630
           END-IF
           CALL "SD_Output" USING "D-S14" D-S14 "p" RETURNING RESU.
           IF  WS-14 = ZERO
               GO TO M-530
           END-IF
           MOVE WS-5 TO W-TCD.
           ADD 5 TO W-TCD1.
           MOVE W-TCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-630
           END-IF
           GO TO M-530.
       M-640.
           IF  W-NO NOT = 46
               GO TO M-650
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S46 "A-S46" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-640
           END-IF
           CALL "SD_Output" USING "D-S46" D-S46 "p" RETURNING RESU.
           GO TO M-530.
       M-650.
           IF  W-NO NOT = 20
               GO TO M-660
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S20 "A-S20" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-650
           END-IF
           CALL "SD_Output" USING "D-S20" D-S20 "p" RETURNING RESU.
           GO TO M-530.
       M-660.
           IF  W-NO NOT = 22
               GO TO M-670
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S22 "A-S22" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-660
           END-IF
           CALL "SD_Output" USING "D-S22" D-S22 "p" RETURNING RESU.
           GO TO M-530.
       M-670.
           IF  W-NO NOT = 34
               GO TO M-680
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S34 "A-S34" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-670
           END-IF
           CALL "SD_Output" USING "D-S34" D-S34 "p" RETURNING RESU.
           GO TO M-530.
       M-680.
           IF  W-NO NOT = 52
               GO TO M-530
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-S52 "A-S52" "S9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-680
           END-IF
           CALL "SD_Output" USING "D-S52" D-S52 "p" RETURNING RESU.
           GO TO M-530.
       M-690.
           COMPUTE W-KIN = WS-12 + WS-14 + WS-46 + WS-20
                                 + WS-22 + WS-34 + WS-52.
           IF  W-KIN NOT = WS-6
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-530
           END-IF.
       M-700.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-700
           END-IF
           IF  W-DMM = 9
               GO TO M-530
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-700
           END-IF
      *
           MOVE ZERO TO W-NEN.
           MOVE WS-FN TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NEN TO WS-SNF.
           MOVE ZERO TO W-NEN.
           MOVE WS-MN TO W-NEN2.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NEN TO WS-SNM.
           MOVE WS-R TO SHIT-R.
      *           REWRITE SHIT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            SHIT-M_PNAME1 SHIT-M_LNAME SHIT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-900
           END-IF
           GO TO M-510.
       M-900.
           CALL "DB_F_Close" USING
            BY REFERENCE BANK-M_IDLST BANK-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CALNM_IDLST CALNM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-05.
           MOVE SPACE TO W-KBN.
           IF  WU-1 = 10
               MOVE   "受取小切手" TO W-KBN
           END-IF
           IF  WU-1 = 11
               MOVE   "受取約手　" TO W-KBN
           END-IF
           IF  WU-1 = 12
               MOVE   "受取為手　" TO W-KBN
           END-IF
           IF  WU-1 = 13
               MOVE   "受取電債　" TO W-KBN
           END-IF.
       S-10.
           EXIT.
       S-15.
           MOVE SPACE TO W-SKB.
           IF  WU-2 = 32
               MOVE   "割　　引" TO W-SKB
           END-IF
           IF  WU-2 = 19
               MOVE   "取立入金" TO W-SKB
           END-IF
           IF  WU-2 = 20
               MOVE   "担保差入" TO W-SKB
           END-IF
           IF  WU-2 = 50
               MOVE   "決　　済" TO W-SKB
           END-IF
           IF  WU-2 = 60
               MOVE   "不渡り　" TO W-SKB
           END-IF
           IF  WU-2 = 70
               MOVE   "割手買戻" TO W-SKB
           END-IF
           IF  WU-2 = 90
               MOVE   "取り消し" TO W-SKB
           END-IF.
       S-20.
           EXIT.
       S-25.
           MOVE ZERO TO CHK.
           IF (W-GET < 1 OR > 12) OR (W-PEY < 1 OR > 31)
               MOVE 5 TO CHK
               GO TO S-35
           END-IF
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-YC2 TO W-NEN
               END-IF
           END-IF
           MOVE W-NGP TO CL-KEY.
      *           READ CALNM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" CALNM_PNAME1 BY REFERENCE CALN-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 5 TO CHK
           END-IF.
       S-35.
           EXIT.
