       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         HKD910.
       AUTHOR.             KAMASAKA   1995-12-22.
      ********************************************
      ******    直送先シール　作成          ******
      ******    JS-SIGN  0 : ｺｰﾄﾞ入力       ******
      ******             1 : ALL入力        ******
      ******             2 : 担当・得意先   ******
      ********************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       SOURCE-COMPUTER.    SYSTEM3100.
       OBJECT-COMPUTER.    SYSTEM3100.
       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       77  WK0512ID           PIC  X(009) VALUE SPACE.
       01  STN-NO.
           02  STN-NO1        PIC  X(003).
           02  STN-NO2        PIC  X(003).
       01  W-FID.
           02  W-FID1         PIC  X(006) VALUE "WK0512".
           02  W-FID2         PIC  X(003).
       01  JS-SIGN                   PIC 9(001).
       01  W-15K                     PIC X(005)    VALUE X"1A24212078".
       01  W-P1.
           02  P-15K                 PIC X(005).
           02  P-UM1                 PIC N(002).
           02  P-UB1                 PIC X(008).
           02  F                     PIC X(024).
           02  P-UM2                 PIC N(002).
           02  P-UB2                 PIC X(008).
           02  F                     PIC X(024).
           02  P-UM3                 PIC N(002).
           02  P-UB3                 PIC X(008).
           02  F                     PIC X(024).
           02  P-UM4                 PIC N(002).
           02  P-UB4                 PIC X(008).
       01  W-P2.
           02  P-JU1                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JU2                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JU3                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JU4                 PIC N(020).
       01  W-P3.
           02  P-JS1                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JS2                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JS3                 PIC N(020).
           02  F                     PIC X(005).
           02  P-JS4                 PIC N(020).
       01  W-P5.
           02  F                     PIC X(001).
           02  P-NA1                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA2                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA3                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA4                 PIC N(020).
       01  W-P6.
           02  F                     PIC X(001).
           02  P-NA5                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA6                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA7                 PIC N(020).
           02  F                     PIC X(005).
           02  P-NA8                 PIC N(020).
       01  W-DATA.
           02  W-TPC                 PIC 9(001).
           02  W-STCD                PIC 9(004).
           02  W-ETCD                PIC 9(004).
           02  W-STNC                PIC 9(002).
           02  W-ETNC                PIC 9(002).
           02  W-DMM                 PIC 9(001).
           02  W-EC                  PIC 9(001).
           02  W-C                   PIC 9(002).
           02  W-ZC                  PIC 9(002).
           02  W-L                   PIC 9(002).
           02  CNT                   PIC 9(002).
           02  CNTD                  PIC 9(002).
           02  CNTE                  PIC 9(002).
           02  CNTF                  PIC 9(002).
           02  CNTG                  PIC 9(002).
           02  CNTH                  PIC 9(002).
           02  W-OLD                 PIC 9(007).
           02  W-AD.
               03  W-D    OCCURS  16.
                   04  W-KEY         PIC 9(007).
                   04  W-KEYD    REDEFINES  W-KEY.
                       05  W-KEY1    PIC 9(004).
                       05  W-KEY2    PIC 9(003).
                   04  W-NAME        PIC N(026).
                   04  W-JSU         PIC N(020).
                   04  W-JSS         PIC N(020).
                   04  W-UNO         PIC X(008).
           02  W-BD.
               03  W-BKEY            PIC 9(007).
               03  W-BNAME           PIC N(026).
               03  W-BJSU            PIC N(020).
               03  W-BJSS            PIC N(020).
               03  W-BNO             PIC X(008).
           02  W-END                 PIC 9(001) VALUE 0.
       01  WN-AREA.
           02  WN-DCHK        PIC  9(001).
           02  WN-CNT1        PIC  9(002).
           02  WN-CNT2        PIC  9(002).
           02  WN-CNT3        PIC  9(002).
           02  WN-CNT4        PIC  9(002).
           02  WN-NAME        PIC  N(026).
           02  WN-ANA   REDEFINES WN-NAME.
             03  WN-NAD   OCCURS  26.
               04  WN-NA      PIC  N(001).
           02  WN-WNAME       PIC  N(026).
           02  WN-WANA   REDEFINES WN-WNAME.
             03  WN-WNAD   OCCURS  26.
               04  WN-WNA     PIC  N(001).
           02  WN-ONAME       PIC  N(019).
           02  WN-AONA  REDEFINES WN-ONAME.
             03  WN-ONAD  OCCURS  19.
               04  WN-ONA     PIC  N(001).
           02  WN-UNAME       PIC  N(019).
           02  WN-AUNA  REDEFINES WN-UNAME.
             03  WN-UNAD  OCCURS  19.
               04  WN-UNA     PIC  N(001).
           02  WN-KUM         PIC  N(004).
           02  WN-AKUM  REDEFINES WN-KUM.
             03  WN-KUMD  OCCURS   4.
               04  WN-KU      PIC  N(001).
       01  ERR-STAT                  PIC X(002).
           COPY LSTAT.
      *
           COPY  LITCM.
           COPY  LITM.
      *FD  TMW
       01  TMW_HKD920.
           02  TMW_PNAME1     PIC  X(009) VALUE SPACE.
           02  F              PIC  X(001).
           02  TMW_LNAME      PIC  X(010) VALUE "TMW_HKD920".
           02  F              PIC  X(001).
           02  TMW_KEY1       PIC  X(100) VALUE SPACE.
           02  TMW_SORT       PIC  X(100) VALUE SPACE.
           02  TMW_IDLST      PIC  X(100) VALUE SPACE.
           02  TMW_RES        USAGE  POINTER.
       01  TMW-R.
           02  TMW-KEY2.
             03  TMW-NTCD     PIC  9(004).
             03  TMW-KEY.
               04  TMW-TCD    PIC  9(004).
           02  TMW-NAME       PIC  N(026).
           02  TMW-JSU        PIC  N(020).
           02  TMW-JSS        PIC  N(020).
           02  TMW-UNO        PIC  X(008).
           02  TMW-TEL        PIC  X(014).
           02  TMW-FAX        PIC  X(014).
           02  TMW-FKC        PIC  9(002).
           02  TMW-BC         PIC  9(001).
           02  F              PIC  9(002).
           02  TMW-TNC        PIC  9(002).
           02  TMW-SS         PIC  9(002).
           02  TMW-NKY        PIC  9(003).
           02  TMW-DCC        PIC  9(001).
           02  TMW-TGC        PIC  9(001).
           02  TMW-YG         PIC  9(006).
           02  TMW-BIK        PIC  9(001).
           02  TMW-ZEI        PIC  9(001).
           02  F              PIC  X(009).
           02  TMW-TNA        PIC  N(016).
           02  TMW-SNA        PIC  N(026).
           02  TMW-SJSU       PIC  N(020).
           02  TMW-SJSS       PIC  N(020).
           02  TMW-SUNO       PIC  X(008).
           02  TMW-STEL       PIC  X(014).
           02  TMW-SFAX       PIC  X(014).
           02  TMW-SHD        PIC  9(002).
           02  TMW-SSI        PIC  9(003).
           02  TMW-SHC1       PIC  9(001).
           02  TMW-SHC2       PIC  9(001).
           02  TMW-SGT        PIC  9(001).
           02  TMW-SGR        PIC  9(001)V9(01).
           02  TMW-STT        PIC  9(001).
           02  TMW-STR        PIC  9(001)V9(01).
           02  TMW-SKR        PIC  9(004).
           02  F              PIC  9(001).
           02  TMW-KSC        PIC  9(001).
           02  TMW-SSC        PIC  9(001).
      *
           02  F              PIC  X(035).
           02  TMW-KANA       PIC  X(036).
           02  TMW-DNG        PIC  9(006).
           02  TMW-SNG        PIC  9(004).
           02  TMW-ENG        PIC  9(004).
       77  F                  PIC  X(001).
      *FD  SP-F
       77  SP-R               PIC  X(204).
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
           02  C-CL    PIC X(012)    VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　　郵便　宛名シール　作成　　　＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC X(038)    VALUE
                   "[  TEST PRINT  ｽﾙ=9  ｼﾅｲ=1     ﾘﾀｰﾝ  ]".
       01  C-MID1.
           02  FILLER  PIC N(024)    VALUE
                 "＊＊＊　　直送先・得意先シール　作成　　＊＊＊".
           02  FILLER  PIC X(018)    VALUE
                   "同上=F5 , 終了=F9 ".
           02  FILLER.
               03  FILLER  PIC X(026)    VALUE
                     "ｺｰﾄﾞ      直送先・得意先名".
               03  FILLER  PIC X(012)    VALUE
                     "確認=F10    ".
           02  FILLER  PIC X(022)    VALUE
                     "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID2.
           02  FILLER  PIC N(020)    VALUE
                 "＊＊＊　　郵便宛名シール　作成　　＊＊＊".
           02  FILLER.
               03  FILLER  PIC N(004)    VALUE
                 "郵便番号".
               03  FILLER  PIC X(008)    VALUE
                   "終了=F9 ".
           02  FILLER  PIC X(008)    VALUE
                   "住所(上)".
           02  FILLER  PIC X(008)    VALUE
                   "    (下)".
           02  FILLER  PIC N(004)    VALUE
                 "氏　　名".
           02  FILLER  PIC N(007)    VALUE
                 "様・殿の入力要".
           02  FILLER  PIC X(022)    VALUE
                     "確認  OK=1 NO=9  1ﾘﾀｰﾝ".
       01  C-MID3.
           02  FILLER  PIC N(020)    VALUE
                 "＊＊＊　　郵便宛名シール　作成　　＊＊＊".
           02  FILLER  PIC X(024)    VALUE
                   "担当者ｺｰﾄﾞ    00 ～ 99  ".
           02  FILLER  PIC X(024)    VALUE
                   "得意先ｺｰﾄﾞ  0000 ～ 9999".
           02  FILLER  PIC X(022)    VALUE
                     "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-TPC      PIC 9(001).
           02  A-KEY      PIC 9(007).
           02  A-UNO      PIC X(008).
           02  A-JSU      PIC N(020).
           02  A-JSS      PIC N(020).
           02  A-NAME     PIC N(026).
           02  FILLER.
             03  A-STNC     PIC 9(002).
             03  A-ETNC     PIC 9(002).
           02  FILLER.
             03  A-STCD     PIC 9(004).
             03  A-ETCD     PIC 9(004).
           02  A-DMM      PIC 9(001).
       01  C-DSP.
           02  D-NAME     PIC N(026).
           02  D-CLEAR.
               03  FILLER  PIC X(007)    VALUE  "       ".
               03  FILLER  PIC X(052)    VALUE
                "                                                    ".
       01  C-ERR.
           02  FILLER.
               03  E-ME1   PIC X(026)   VALUE
                     "***  ﾁｮｸｿｳｻｷ･ﾄｸｲｻｷ ﾅｼ  ***".
               03  E-ME98  PIC X(005)   VALUE X"1B4A05".
               03  E-ME99  PIC X(005)   VALUE X"1B4205".
               03  E-STAT  PIC X(002).
               03  E-CL    PIC X(050)   VALUE
                   "                                                  ".
       PROCEDURE           DIVISION.
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
            "C-MID" " " "0" "0" "374" " " " " RETURNING RESU.
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
            "08C-MID" "X" "15" "12" "38" "07C-MID" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "126" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "N" "1" "12" "48" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "2" "58" "18" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID1" " " "3" "0" "38" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID1" "X" "3" "8" "26" " " "03C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID1" "X" "3" "68" "12" "0103C-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID1" "X" "23" "40" "22" "03C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "116" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" "N" "1" "20" "40" " " "C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID2" " " "5" "0" "16" "01C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID2" "N" "5" "11" "8" " " "02C-MID2" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID2" "X" "5" "33" "8" "0102C-MID2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID2" "X" "7" "11" "8" "02C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID2" "X" "8" "11" "8" "03C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID2" "N" "10" "11" "8" "04C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID2" "N" "11" "60" "14" "05C-MID2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID2" "X" "23" "40" "22" "06C-MID2" " " RETURNING RESU.
      *C-MID3
       CALL "SD_Init" USING 
            "C-MID3" " " "0" "0" "110" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID3" "N" "1" "20" "40" " " "C-MID3" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID3" "X" "6" "28" "24" "01C-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID3" "X" "8" "28" "24" "02C-MID3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID3" "X" "23" "40" "22" "03C-MID3" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "161" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "15" "41" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "9" "W-L" "8" "7" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KEY" BY REFERENCE W-KEY(1) "7" "1" BY REFERENCE W-C 147
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UNO" "X" "5" "21" "8" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UNO" BY REFERENCE W-UNO(1) "8" "1" BY REFERENCE W-C 147
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JSU" "N" "7" "21" "40" "A-UNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JSU" BY REFERENCE W-JSU(1) "40" "1" BY REFERENCE W-C 147
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JSS" "N" "8" "21" "40" "A-JSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JSS" BY REFERENCE W-JSS(1) "40" "1" BY REFERENCE W-C 147
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "N" "10" "21" "52" "A-JSS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NAME" BY REFERENCE W-NAME(1) "52" "1" BY REFERENCE W-C
            147 RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "6" "0" "4" "A-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STNC" "9" "6" "42" "2" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STNC" BY REFERENCE W-STNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETNC" "9" "6" "48" "2" "A-STNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETNC" BY REFERENCE W-ETNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-ACP" " " "8" "0" "8" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STCD" "9" "8" "40" "4" " " "08C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STCD" BY REFERENCE W-STCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ETCD" "9" "8" "48" "4" "A-STCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ETCD" BY REFERENCE W-ETCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "57" "1" "08C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "63" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L" "18" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-NAME" BY REFERENCE W-NAME(1) "52" "1" BY REFERENCE W-C
            147 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CLEAR" "X" "W-L" "0" "11" "D-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CLEAR" "X" "W-L" "13" "7" " " "D-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CLEAR" "X" "W-L" "18" "52" "01D-CLEAR" " "
            RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "88" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "88" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "26" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME1" " " RETURNING RESU.
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
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "PR_Open" RETURNING RESP.
       M-10.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               CALL "PR_Close" RETURNING RESP
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP  RUN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-10
           END-IF
           IF  W-TPC = 1
               GO  TO  M-12
           END-IF
           IF  W-TPC NOT = 9
               GO  TO  M-10
           END-IF
           MOVE  SPACE       TO  W-P1    W-P2    W-P3    W-P5    W-P6.
           MOVE  W-15K       TO  P-15K.
           MOVE  "〒　"    TO  P-UM1     P-UM2     P-UM3     P-UM4.
           MOVE  "XXXXXXXX"  TO  P-UB1     P-UB2     P-UB3     P-UB4.
           MOVE  ALL "Ｎ"  TO  P-JU1     P-JU2     P-JU3     P-JU4
                                 P-JS1     P-JS2     P-JS3     P-JS4.
           MOVE  "ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ　"  TO
               P-NA1  P-NA2  P-NA3  P-NA4  P-NA5  P-NA6  P-NA7  P-NA8.
           PERFORM  S-05  THRU  S-10.
           GO  TO  M-10.
       M-12.
           IF  JS-SIGN = 0
               MOVE ZERO TO W-OLD
               CALL "DB_F_Open" USING
                "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
                "TC-KEY" BY REFERENCE TC-KEY
               CALL "DB_F_Open" USING
                "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
                "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2
           ELSE
               CALL "CBLSTNNO" USING STN-NO USER_ID
               MOVE STN-NO2 TO W-FID2
               MOVE W-FID TO WK0512ID
               MOVE WK0512ID TO TMW_PNAME1
               CALL "DB_F_Open" USING
                "INPUT" TMW_PNAME1 " " BY REFERENCE TMW_IDLST "0"
           END-IF
           INITIALIZE W-BD.
       M-15.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 1
               CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU
           END-IF
           IF  JS-SIGN = 2
               MOVE 9999 TO W-ETCD
               MOVE 99 TO W-ETNC
               CALL "SD_Output" USING "C-MID3" C-MID3 "p" RETURNING RESU
           END-IF
           MOVE  ZERO  TO  W-C  W-EC.
           PERFORM  S-15  THRU  S-25.
           IF  JS-SIGN = 1
               GO TO M-41
           END-IF
           IF  JS-SIGN = 2
               GO TO M-57
           END-IF
           MOVE  3     TO  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       M-20.
           ADD  1  TO  W-C  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-C > 16
               GO  TO  M-50
           END-IF
           IF  W-EC NOT = ZERO
               GO  TO  M-40
           END-IF.
       M-25.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-35
           END-IF
           IF  ESTAT = PF5
               IF  W-OLD NOT = ZERO
                   MOVE W-OLD TO W-KEY(W-C)
                   CALL "SD_Output" USING
                    "A-KEY" A-KEY "p" RETURNING RESU
                   GO TO M-27
               END-IF
           END-IF
           IF  ESTAT = ADV
               MOVE  1  TO  W-EC
               GO  TO  M-40
           END-IF
           IF  ESTAT = PF9
               IF  W-C = 1
                   GO  TO  M-95
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-25
           END-IF.
       M-27.
           MOVE  W-KEY(W-C)  TO  TC-KEY
      *           READ  TC-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO  TO  M-30
           END-IF
           MOVE  TC-NAME  TO  W-NAME(W-C).
           MOVE  TC-JSU   TO  W-JSU(W-C).
           MOVE  TC-JSS   TO  W-JSS(W-C).
           MOVE  TC-UNO   TO  W-UNO(W-C).
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE  W-KEY(W-C)  TO  W-OLD.
           GO  TO  M-20.
       M-30.
           MOVE  W-KEY1(W-C)  TO  T-KEY.
      *           READ  T-M  WITH  UNLOCK  INVALID  KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO  TO  M-25
           END-IF
           MOVE  T-NAME  TO  W-NAME(W-C).
           MOVE  T-JSU   TO  W-JSU(W-C).
           MOVE  T-JSS   TO  W-JSS(W-C).
           MOVE  T-UNO   TO  W-UNO(W-C).
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           MOVE  W-KEY(W-C)  TO  W-OLD.
           GO  TO  M-20.
       M-35.
           SUBTRACT  1  FROM  W-C  W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-C = ZERO
               GO  TO  M-15
           END-IF
           IF  W-KEY(W-C) = ZERO
               GO  TO  M-35
           END-IF
           GO  TO  M-25.
       M-40.
           MOVE  ZERO   TO  W-KEY(W-C).
           MOVE  SPACE  TO  W-NAME(W-C)  W-JSU(W-C)  W-JSS(W-C).
           MOVE  SPACE  TO  W-UNO(W-C).
           CALL "SD_Output" USING "D-CLEAR" D-CLEAR "p" RETURNING RESU.
           GO  TO  M-20.
       M-41.
           ADD  1  TO  W-C.
           IF  W-C = 17
               GO TO M-52
           END-IF
           IF  W-C = 1
               IF  W-BNAME NOT = SPACE
                   MOVE W-BD TO W-D(W-C)
                   CALL "SD_Output" USING
                    "A-UNO" A-UNO "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-JSU" A-JSU "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-JSS" A-JSS "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "A-NAME" A-NAME "p" RETURNING RESU
               END-IF
           END-IF.
       M-42.
           CALL "SD_Accept" USING BY REFERENCE A-UNO "A-UNO" "X" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               MOVE 9 TO W-END
               GO TO M-52
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-42
           END-IF.
       M-43.
           CALL "SD_Accept" USING BY REFERENCE A-JSU "A-JSU" "N" "40"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-42
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-43
           END-IF.
       M-44.
           CALL "SD_Accept" USING BY REFERENCE A-JSS "A-JSS" "N" "40"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-43
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-44
           END-IF.
       M-45.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "N" "52"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-44
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-45
           END-IF.
       M-50.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 1
                   GO TO M-45
               ELSE
                   MOVE  ZERO  TO  W-EC
                   GO  TO  M-35
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-50
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 1
                   GO TO M-42
               ELSE
                   MOVE  ZERO  TO  W-C  W-EC
                   MOVE  3     TO  W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
                   GO  TO  M-20
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-50
           END-IF
           IF  JS-SIGN = 1
               MOVE 9999999 TO W-KEY(W-C)
               MOVE W-D(W-C) TO W-BD
               CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU
               GO TO M-41
           END-IF.
       M-52.
           MOVE  ZERO  TO  W-C  CNT.
       M-55.
           ADD  1  TO  W-C.
           IF  W-C > 16
               GO  TO  M-80
           END-IF
           IF  W-KEY(W-C) = ZERO
               GO  TO  M-80
           END-IF
           MOVE W-NAME(W-C) TO WN-NAME.
           PERFORM NAM-RTN THRU NAM-EX.
           IF  CNT = ZERO
               MOVE  SPACE  TO  W-P1  W-P2  W-P3  W-P5  W-P6
               MOVE  W-15K  TO  P-15K
           END-IF
           ADD  1  TO  CNT.
           IF  CNT = 1
               MOVE  "〒　"    TO  P-UM1
               MOVE  W-UNO(W-C)  TO  P-UB1
               MOVE  W-JSU(W-C)  TO  P-JU1
               MOVE  W-JSS(W-C)  TO  P-JS1
               MOVE  WN-ONAME    TO  P-NA1
               MOVE  WN-UNAME    TO  P-NA5
               GO  TO  M-55
           END-IF
           IF  CNT = 2
               MOVE  "〒　"    TO  P-UM2
               MOVE  W-UNO(W-C)  TO  P-UB2
               MOVE  W-JSU(W-C)  TO  P-JU2
               MOVE  W-JSS(W-C)  TO  P-JS2
               MOVE  WN-ONAME    TO  P-NA2
               MOVE  WN-UNAME    TO  P-NA6
               GO  TO  M-55
           END-IF
           IF  CNT = 3
               MOVE  "〒　"    TO  P-UM3
               MOVE  W-UNO(W-C)  TO  P-UB3
               MOVE  W-JSU(W-C)  TO  P-JU3
               MOVE  W-JSS(W-C)  TO  P-JS3
               MOVE  WN-ONAME    TO  P-NA3
               MOVE  WN-UNAME    TO  P-NA7
               GO  TO  M-55
           END-IF
           MOVE  "〒　"    TO  P-UM4.
           MOVE  W-UNO(W-C)  TO  P-UB4.
           MOVE  W-JSU(W-C)  TO  P-JU4.
           MOVE  W-JSS(W-C)  TO  P-JS4.
           MOVE  WN-ONAME    TO  P-NA4.
           MOVE  WN-UNAME    TO  P-NA8.
           PERFORM  S-05  THRU  S-10.
           MOVE  ZERO  TO  CNT.
           GO  TO  M-55.
      *-----------------------------------------------------------------
       M-57.
           CALL "SD_Accept" USING BY REFERENCE A-STNC "A-STNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = PF9
               GO  TO  M-95
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-57
           END-IF.
       M-58.
           CALL "SD_Accept" USING BY REFERENCE A-ETNC "A-ETNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-57
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-58
           END-IF
           IF  W-STNC > W-ETNC
               GO  TO  M-58
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-STCD "A-STCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-58
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-60
           END-IF.
       M-62.
           CALL "SD_Accept" USING BY REFERENCE A-ETCD "A-ETCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-60
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-62
           END-IF
           IF  W-STCD > W-ETCD
               GO  TO  M-62
           END-IF.
       M-65.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO  TO  M-62
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO  TO  M-65
           END-IF
           IF  W-DMM = 9
               GO  TO  M-60
           END-IF
           IF  W-DMM NOT = 1
               GO  TO  M-65
           END-IF
      *
           MOVE  ZERO  TO  CNT.
       M-70.
      *           READ TMW AT END
      *//////////////////////     
           CALL "DB_Read" USING
            "AT END" TMW_PNAME1 BY REFERENCE TMW-R " " RETURNING RET.
           IF  RET = 1
               GO TO M-80
           END-IF
           IF  TMW-TNC < W-STNC OR > W-ETNC
               GO TO M-70
           END-IF
           IF  TMW-TCD < W-STCD
               GO TO M-70
           END-IF
           IF  TMW-TCD > W-ETCD
               GO TO M-70
           END-IF
           IF  TMW-ENG NOT = ZERO
               GO TO M-70
           END-IF
           IF  TMW-JSU = SPACE
               GO TO M-70
           END-IF
           PERFORM SET2-RTN THRU SET2-EX.
           GO TO M-70.
       M-80.
           IF  CNT NOT = ZERO
               PERFORM  S-05  THRU  S-10
           END-IF
           IF  JS-SIGN = 1
               IF  W-END = 9
                   GO TO M-95
               END-IF
           END-IF
           IF  JS-SIGN = 2
               GO TO M-95
           END-IF
           GO  TO  M-15.
       M-95.
           CALL "PR_Close" RETURNING RESP.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE TC-M_IDLST TC-M_PNAME1
               CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1
           END-IF
           IF  JS-SIGN = 2
               CALL "DB_F_Close" USING BY REFERENCE TMW_IDLST TMW_PNAME1
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP  RUN.
       S-05.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P1   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P2   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P3   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P5   TO  SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           MOVE   W-P6   TO  SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE   SPACE  TO  SP-R.
           CALL "PR_LineFeed" USING "3" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       S-10.
           EXIT.
       S-15.
           MOVE  ZERO  TO  W-ZC.
       S-20.
           ADD  1  TO  W-ZC.
           IF  W-ZC > 16
               MOVE  ZERO   TO  W-KEY(W-ZC)
               MOVE  SPACE  TO  W-NAME(W-ZC)  W-JSU(W-ZC)  W-JSS(W-ZC)
               MOVE  SPACE  TO  W-UNO(W-ZC)
               GO  TO  S-20
           END-IF.
       S-25.
           EXIT.
       NAM-RTN.
           MOVE 0 TO WN-DCHK.
       NAM-010.
           MOVE SPACE TO WN-ONAME WN-UNAME.
           MOVE 27 TO WN-CNT1.
       NAM-020.
           SUBTRACT 1 FROM WN-CNT1.
           IF  WN-CNT1 = 17
               MOVE SPACE TO WN-ONAME
               MOVE WN-NAME TO WN-UNAME
               GO TO NAM-510
           END-IF
           IF  WN-NA(WN-CNT1) = SPACE
               GO TO NAM-020
           END-IF
           IF  WN-DCHK = 1
               GO TO NAM-310
           END-IF
           IF  WN-DCHK = 2
               GO TO NAM-410
           END-IF.
       NAM-100.
           MOVE 18 TO WN-CNT2.
       NAM-110.
           IF  WN-CNT1 < 9
               GO TO NAM-210
           END-IF
           IF  WN-NA(WN-CNT1) = SPACE
               GO TO NAM-120
           END-IF
           SUBTRACT 1 FROM WN-CNT2.
           IF  WN-CNT2 NOT = ZERO
               MOVE WN-NA(WN-CNT1) TO WN-UNA(WN-CNT2)
               SUBTRACT 1 FROM WN-CNT1
               GO TO NAM-110
           END-IF
           GO TO NAM-210.
       NAM-120.
           IF  WN-CNT1 > 20
               SUBTRACT 1 FROM WN-CNT1
               GO TO NAM-110
           END-IF
      *
           MOVE ZERO TO WN-CNT3.
       NAM-130.
           ADD 1 TO WN-CNT3.
           IF  WN-CNT1 = WN-CNT3
               GO TO NAM-510
           END-IF
           IF  WN-CNT3 < 20
               MOVE WN-NA(WN-CNT3) TO WN-ONA(WN-CNT3)
               GO TO NAM-130
           END-IF.
       NAM-210.
           MOVE WN-NAME TO WN-WNAME.
           MOVE SPACE TO WN-NAME.
           MOVE ZERO TO WN-CNT1 WN-CNT2.
       NAM-220.
           ADD 1 TO WN-CNT1.
           IF  WN-CNT1 > 26
               MOVE 1 TO WN-DCHK
               GO TO NAM-010
           END-IF
           IF  WN-WNA(WN-CNT1) NOT = SPACE
               ADD 1 TO WN-CNT2
               MOVE WN-WNA(WN-CNT1) TO WN-NA(WN-CNT2)
           END-IF
           GO TO NAM-220.
       NAM-310.
           MOVE WN-NAME TO WN-WNAME.
           MOVE SPACE TO WN-NAME.
           MOVE ZERO TO WN-CNT1.
       NAM-320.
           ADD 1 TO WN-CNT1.
           IF  WN-CNT1 > 23
               MOVE SPACE TO WN-NAME
               MOVE WN-WNAME TO WN-NAME
               GO TO NAM-410
           END-IF
           MOVE WN-WNA(WN-CNT1) TO WN-NA(WN-CNT1).
           MOVE SPACE TO WN-KUM.
           MOVE WN-CNT1 TO WN-CNT2.
           MOVE ZERO TO WN-CNT3.
       NAM-330.
           ADD 1 TO WN-CNT3.
           IF  WN-CNT3 NOT = 5
               MOVE WN-WNA(WN-CNT2) TO WN-KU(WN-CNT3)
               ADD 1 TO WN-CNT2
               GO TO NAM-330
           END-IF
           IF  WN-KUM NOT = "株式会社" AND "有限会社"
               GO TO NAM-320
           END-IF
      *
           IF  WN-KUM = "株式会社"
               MOVE "㈱" TO WN-NA(WN-CNT1)
           END-IF
           IF  WN-KUM = "有限会社"
               MOVE "㈲" TO WN-NA(WN-CNT1)
           END-IF
           MOVE WN-CNT1 TO WN-CNT2.
           ADD 4 TO WN-CNT2.
       NAM-340.
           ADD 1 TO WN-CNT1 WN-CNT2.
           IF  WN-CNT2 < 27
               MOVE WN-WNA(WN-CNT2) TO WN-NA(WN-CNT1)
               GO TO NAM-340
           END-IF
           MOVE 2 TO WN-DCHK.
           GO TO NAM-010.
       NAM-410.
           MOVE SPACE TO WN-ONAME WN-UNAME.
           MOVE ZERO TO WN-CNT1.
       NAM-420.
           ADD 1 TO WN-CNT1.
           IF  WN-CNT1 > 26
               GO TO NAM-510
           END-IF
           IF  WN-CNT1 < 19
               MOVE WN-NA(WN-CNT1) TO WN-ONA(WN-CNT1)
               GO TO NAM-420
           END-IF
           IF  WN-CNT1 = 19
               MOVE 9 TO WN-CNT2
           END-IF
           ADD 1 TO WN-CNT2.
           MOVE WN-NA(WN-CNT1) TO WN-UNA(WN-CNT2)
           GO TO NAM-420.
       NAM-510.
           IF  JS-SIGN = 1
               GO TO NAM-EX
           END-IF
           MOVE 20 TO WN-CNT1.
       NAM-520.
           SUBTRACT 1 FROM WN-CNT1.
           IF  WN-CNT1 = ZERO
               MOVE "殿" TO WN-UNA(19)
               GO TO NAM-EX
           END-IF
           IF  WN-UNA(WN-CNT1) = SPACE
               GO TO NAM-520
           END-IF
           ADD 2 TO WN-CNT1.
           IF  WN-CNT1 > 19
               MOVE 19 TO WN-CNT1
           END-IF
           MOVE "殿" TO WN-UNA(WN-CNT1).
       NAM-EX.
           EXIT.
       SET2-RTN.
           MOVE TMW-NAME TO WN-NAME.
           PERFORM NAM-RTN THRU NAM-EX.
           IF  CNT = ZERO
               MOVE  SPACE  TO  W-P1  W-P2  W-P3  W-P5  W-P6
               MOVE  W-15K  TO  P-15K
           END-IF
           ADD  1  TO  CNT.
           IF  CNT = 1
               MOVE  "〒　"    TO  P-UM1
               MOVE  TMW-UNO     TO  P-UB1
               MOVE  TMW-JSU     TO  P-JU1
               MOVE  TMW-JSS     TO  P-JS1
               MOVE  WN-ONAME    TO  P-NA1
               MOVE  WN-UNAME    TO  P-NA5
               GO  TO  SET2-EX
           END-IF
           IF  CNT = 2
               MOVE  "〒　"    TO  P-UM2
               MOVE  TMW-UNO     TO  P-UB2
               MOVE  TMW-JSU     TO  P-JU2
               MOVE  TMW-JSS     TO  P-JS2
               MOVE  WN-ONAME    TO  P-NA2
               MOVE  WN-UNAME    TO  P-NA6
               GO  TO  SET2-EX
           END-IF
           IF  CNT = 3
               MOVE  "〒　"    TO  P-UM3
               MOVE  TMW-UNO     TO  P-UB3
               MOVE  TMW-JSU     TO  P-JU3
               MOVE  TMW-JSS     TO  P-JS3
               MOVE  WN-ONAME    TO  P-NA3
               MOVE  WN-UNAME    TO  P-NA7
               GO  TO  SET2-EX
           END-IF
           MOVE  "〒　"    TO  P-UM4.
           MOVE  TMW-UNO     TO  P-UB4.
           MOVE  TMW-JSU     TO  P-JU4.
           MOVE  TMW-JSS     TO  P-JS4.
           MOVE  WN-ONAME    TO  P-NA4.
           MOVE  WN-UNAME    TO  P-NA8.
           PERFORM  S-05  THRU  S-10.
           MOVE  ZERO  TO  CNT.
       SET2-EX.
           EXIT.
