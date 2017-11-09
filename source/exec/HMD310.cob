       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMD310.
      *********************************************************
      *    PROGRAM         :  仕上・受入入力　　　　　　　　  *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHD31                          *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  0=入力 , 1=購買変換             *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  ERR-STAT           PIC  X(002).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(038) VALUE SPACE.
           02  H-MID          PIC  N(025) VALUE
                "＊＊＊　　仕上・受入入力　プルーフリスト　　＊＊＊".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "入力".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　日付　".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "倉庫".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "受入№　".
           02  F              PIC  N(002) VALUE "返品".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "生産".
           02  F              PIC  X(007) VALUE " ｺｰﾄﾞ  ".
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(092) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(021) VALUE
                "   28.0   29.0   30.0".
           02  F              PIC  X(030) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "2   12.5   13.0   13.5   14.0   15.0   ".
           02  F              PIC  X(032) VALUE
                "16.0   17.0   18.0   19.0   20.0".
           02  F              PIC  X(030) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "3   21.0   21.5   22.0   22.5   23.0   ".
           02  F              PIC  X(032) VALUE
                "23.5   24.0   24.5   25.0       ".
           02  F              PIC  X(030) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  X(039) VALUE
                "4   24.0   24.5   25.0   25.5   26.0   ".
           02  F              PIC  X(032) VALUE
                "26.5   27.0   27.5              ".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売価金額".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "振替金額".
       01  W-MID1             PIC  N(025) VALUE
            "＊＊＊　　製品仕入　変換　プルーフリスト　　＊＊＊".
       01  W-P1.
           02  F              PIC  X(001).
           02  P-NRC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-SKC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-UNO          PIC  9(006).
           02  F              PIC  X(002).
           02  P-HPC          PIC  9(001).
           02  F              PIC  X(003).
           02  P-SSC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(065).
       01  W-P2.
           02  F              PIC  X(035).
           02  P-SIZ          PIC  9(001).
           02  P-SU           PIC ---,---       OCCURS  10.
           02  P-GS           PIC ----,--9.
           02  P-BK           PIC ---,---,--9.
           02  P-FK           PIC ---,---,--9.
       01  W-P3.
           02  F              PIC  X(094).
           02  P-GOK          PIC  N(008).
           02  P-GGS          PIC ----,--9.
           02  P-GBK          PIC ---,---,--9.
           02  P-GFK          PIC ---,---,--9.
       01  W-ID.
           02  W-UNO          PIC  9(006).
           02  W-SCD          PIC  9(001).
           02  W-DATE         PIC  9(008).
           02  W-DATEL REDEFINES W-DATE.
             03  F            PIC  9(002).
             03  W-DATES      PIC  9(006).
           02  W-SSC          PIC  9(001).
           02  W-ID1.
             03  W-HCD        PIC  9(006).
             03  W-SIZ        PIC  9(001).
           02  W-ID2.
             03  W-SU         PIC S9(004)       OCCURS  10.
             03  W-GS         PIC S9(006).
           02  W-NAME         PIC  N(024).
       01  W-DATA.
           02  W-NRC          PIC  9(001).
           02  W-AIDD.
             03  W-AID        PIC  X(117)       OCCURS   6.
           02  W-IDD          PIC  9(007).
           02  W-NGD          PIC  9(006).
           02  W-NGDD  REDEFINES W-NGD.
             03  W-NEND       PIC  9(004).
             03  W-NENDD REDEFINES W-NEND.
               04  W-NEND1    PIC  9(002).
               04  W-NEND2    PIC  9(002).
             03  W-GETD       PIC  9(002).
           02  W-NGDSD REDEFINES W-NGD.
             03  F            PIC  9(002).
             03  W-NGDS       PIC  9(004).
           02  W-NGP          PIC  9(008).
           02  W-NGPD  REDEFINES W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-PEY        PIC  9(002).
           02  W-NGPSD REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-SKC          PIC  9(001).
           02  W-SON          PIC  N(006).
           02  W-SUD          PIC S9(004).
           02  W-HPC          PIC  9(001).
           02  W-D.
             03  W-NRCD       PIC  9(001).
             03  W-DATED      PIC  9(008).
             03  W-SSCD       PIC  9(001).
             03  W-HCDD       PIC  9(006).
             03  W-SKCD       PIC  9(001).
             03  W-HPCD       PIC  9(001).
           02  W-BK           PIC S9(008).
           02  W-FK           PIC S9(008).
           02  W-LC.
             03  W-L.
               04  W-L1       PIC  9(002).
               04  W-L2       PIC  9(002).
               04  W-L3       PIC  9(002).
             03  W-C          PIC  9(002).
           02  W-GN           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-DMM          PIC  9(001).
           02  W-PMCD.
             03  W-PMC        PIC  9(001)       OCCURS   6.
           02  W-BIT.
             03  W-BITD       PIC  9(001)       OCCURS  10.
           02  W-EC           PIC  9(001).
           02  W-NCMA         PIC  N(025) VALUE SPACE.
           02  W-NCMD  REDEFINES W-NCMA.
             03  W-NCM        PIC  N(005)       OCCURS   5.
           02  CHK            PIC  9(001) VALUE ZERO.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-POC          PIC  9(001) VALUE ZERO.
           02  W-END          PIC  9(001) VALUE ZERO.
           02  WK-UNO         PIC  9(006).
           02  W-FHCD         PIC  9(006) VALUE ZERO.
           02  W-DC           PIC  9(004).
           02  W-DEC          PIC  9(001).
           02  W-FT           PIC  9(005).
           02  W-ERR          PIC  9(001).
           02  W-SNGP.
             03  W-SNEN       PIC  9(002).
             03  W-SGET       PIC  9(002).
             03  W-SPEY       PIC  9(002).
       01  G-AREA.
           02  G-GS           PIC S9(006).
           02  G-BK           PIC S9(008).
           02  G-FK           PIC S9(008).
       01  WA-D.
           02  WA-GS          PIC S9(006).
           02  WA-BK          PIC S9(008).
           02  WA-FK          PIC S9(008).
           COPY LSTAT.
           COPY LWMSG.
      *
           COPY LIBFDD.
           COPY LIHIM.
           COPY L-JCON.
           COPY LIHKBM.
           COPY LIHSSF.
           COPY LIHUHM.
           COPY LUTRAN.
           COPY LSPF.
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
           02  FILLER  PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NRC   PIC  9(001).
           02  A-DATE  PIC  9(006).
           02  A-SCD   PIC  9(001).
           02  FILLER.
             03  A-HCD   PIC  9(006).
             03  A-SK    PIC  9(001).
           02  FILLER.
             03  A-SIZ   PIC  9(001).
             03  A-SU    PIC S9(004).
           02  A-HPC   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-MID1.
             03  FILLER  PIC  N(020) VALUE
                  "＊＊＊　　仕上・受入伝票　入力　　＊＊＊".
             03  FILLER.
               04  0102D-MID1  PIC  N(005).
               04  FILLER      PIC  X(004) VALUE " = 1".
             03  FILLER.
               04  0103D-MID1  PIC  N(005).
               04  FILLER      PIC  X(004) VALUE " = 3".
             03  FILLER.
               04  0104D-MID1  PIC  N(005).
               04  FILLER      PIC  X(004) VALUE " = 5".
             03  FILLER  PIC  X(028) VALUE
                  "終　　　了 = 9 .......  ﾘﾀｰﾝ".
           02  D-MID2  PIC  N(020) VALUE
                "＊＊＊　　製品仕入　変換処理　　　＊＊＊".
           02  D-NCM   PIC  N(005).
           02  D-SNM   PIC  N(006).
           02  FILLER.
             03  D-NAME  PIC  N(024).
           02  FILLER.
             03  D-TBL1.
               04  FILLER  PIC  N(002) VALUE "３号".
               04  FILLER  PIC  N(002) VALUE "２号".
               04  FILLER  PIC  N(002) VALUE "１号".
               04  FILLER  PIC  N(002) VALUE "０号".
               04  FILLER  PIC  X(005) VALUE "  中 ".
               04  FILLER  PIC  X(005) VALUE "  大 ".
               04  FILLER  PIC  N(002) VALUE "特大".
               04  FILLER  PIC  X(005) VALUE "28.0 ".
               04  FILLER  PIC  X(005) VALUE "29.0 ".
               04  FILLER  PIC  X(005) VALUE "30.0 ".
             03  D-TBL2.
               04  FILLER  PIC  X(005) VALUE "12.5 ".
               04  FILLER  PIC  X(005) VALUE "13.0 ".
               04  FILLER  PIC  X(005) VALUE "13.5 ".
               04  FILLER  PIC  X(005) VALUE "14.0 ".
               04  FILLER  PIC  X(005) VALUE "15.0 ".
               04  FILLER  PIC  X(005) VALUE "16.0 ".
               04  FILLER  PIC  X(005) VALUE "17.0 ".
               04  FILLER  PIC  X(005) VALUE "18.0 ".
               04  FILLER  PIC  X(005) VALUE "19.0 ".
               04  FILLER  PIC  X(005) VALUE "20.0 ".
             03  D-TBL3.
               04  FILLER  PIC  X(005) VALUE "21.0 ".
               04  FILLER  PIC  X(005) VALUE "21.5 ".
               04  FILLER  PIC  X(005) VALUE "22.0 ".
               04  FILLER  PIC  X(005) VALUE "22.5 ".
               04  FILLER  PIC  X(005) VALUE "23.0 ".
               04  FILLER  PIC  X(005) VALUE "23.5 ".
               04  FILLER  PIC  X(005) VALUE "24.0 ".
               04  FILLER  PIC  X(005) VALUE "24.5 ".
               04  FILLER  PIC  X(005) VALUE "25.0 ".
               04  FILLER  PIC  X(005) VALUE "     ".
             03  D-TBL4.
               04  FILLER  PIC  X(005) VALUE "24.0 ".
               04  FILLER  PIC  X(005) VALUE "24.5 ".
               04  FILLER  PIC  X(005) VALUE "25.0 ".
               04  FILLER  PIC  X(005) VALUE "25.5 ".
               04  FILLER  PIC  X(005) VALUE "26.0 ".
               04  FILLER  PIC  X(005) VALUE "26.5 ".
               04  FILLER  PIC  X(005) VALUE "27.0 ".
               04  FILLER  PIC  X(005) VALUE "27.5 ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
           02  FILLER.
             03  D-SU    PIC ZZZZ- .
             03  D-GS    PIC ---,--- .
             03  D-SU1   PIC ZZZZ- .
             03  D-SU2   PIC ZZZZ- .
             03  D-SU3   PIC ZZZZ- .
             03  D-SU4   PIC ZZZZ- .
             03  D-SU5   PIC ZZZZ- .
             03  D-SU6   PIC ZZZZ- .
             03  D-SU7   PIC ZZZZ- .
             03  D-SU8   PIC ZZZZ- .
             03  D-SU9   PIC ZZZZ- .
             03  D-SU10  PIC ZZZZ- .
           02  D-PRN   PIC  N(021) VALUE
                "＊＊＊　　仕上・受入　入力リスト　　＊＊＊".
       01  C-SPC.
           02  S-SPACE.
             03  FILLER.
               04  S-HCD   PIC  X(006) VALUE "      ".
               04  S-NAME  PIC  X(048) VALUE
                    "                                                ".
               04  S-SK    PIC  X(001) VALUE " ".
             03  FILLER.
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
             03  FILLER.
               04  S-SIZ   PIC  X(001) VALUE " ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  FILLER  PIC  X(005) VALUE "     ".
               04  S-GS    PIC  X(008) VALUE "        ".
           02  FILLER.
             03  S-SU    PIC  X(005) VALUE "     ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1.
               04  FILLER  PIC  X(016) VALUE
                    "***  HIM ﾅｼ  ***".
               04  02E-ME1 PIC  9(006).
             03  E-ME2   PIC  X(021) VALUE
                  "***  PROGRAM ｴﾗｰ  ***".
             03  E-ME3   PIC  X(026) VALUE
                  "***  振替 単価 未決定  ***".
             03  E-ME4   PIC  X(019) VALUE
                  "***  ﾈﾝｹﾞﾂ ｴﾗｰ  ***".
             03  E-ME5   PIC  X(018) VALUE
                  "***  ｸﾌﾞﾝ ｴﾗｰ  ***".
             03  E-ME6   PIC  N(011) VALUE
                  "コントロールＦ　未登録".
             03  E-ME7   PIC  N(007) VALUE
                  "倉庫名　未登録".
             03  E-ME8   PIC  N(009) VALUE
                  "未更新データ　有り".
             03  E-ME9   PIC  X(021) VALUE
                  "***  ｸﾌﾞﾝﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(027) VALUE
                  "***  JCON ﾃﾞﾝﾋﾟｮｳNO ﾅｼ  ***".
             03  E-ME13  PIC  X(026) VALUE
                  "***  JCON REWRITE ｴﾗｰ  ***".
             03  E-ME14  PIC  X(026) VALUE
                  "***  HSSF REWRITE ｴﾗｰ  ***".
             03  E-ME18.
               04  FILLER    PIC  X(017) VALUE
                    "***  HUHM ﾅｼ  ***".
               04  02E-ME18  PIC  9(006).
             03  E-ME19  PIC  X(032) VALUE
                  "***  品名マスタ　原価エラー  ***".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-CL    PIC  X(050) VALUE
                  "                                                  ".
           COPY LIBSCR.
           COPY LSMSG.
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
      *C-MID
       CALL "SD_Init" USING 
            "C-MID" " " "0" "0" "302" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "3" "10" "40" " " "C-MID" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID" "N" "4" "10" "40" "01C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID" "N" "5" "10" "40" "02C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID" "N" "6" "10" "40" "03C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID" "N" "7" "10" "40" "04C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID" "N" "8" "10" "40" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID" "N" "9" "10" "40" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID" "X" "23" "33" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NRC" "9" "17" "39" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
           "A-NRC" BY REFERENCE W-NRC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "3" "23" "6" "A-NRC" " " RETURNING RESU.
       CALL "SD_Using" USING 
           "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "3" "35" "1" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING 
           "A-SCD" BY REFERENCE W-SKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "W-L1" "0" "7" "A-SCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-L1" "9" "6" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
           "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SK" "9" "W-L1" "75" "1" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
           "A-SK" BY REFERENCE W-SSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "W-L3" "0" "5" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SIZ" "9" "W-L3" "5" "1" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
           "A-SIZ" BY REFERENCE W-SIZ "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "S9" "W-L3" "W-C" "4" "A-SIZ" " " RETURNING RESU.
       CALL "SD_Using" USING 
           "A-SU" BY REFERENCE W-SUD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HPC" "9" "22" "20" "1" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
           "A-HPC" BY REFERENCE W-HPC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "50" "1" "A-HPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "519" " " " "  RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID1" " " "0" "0" "110" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MID1" "N" "6" "10" "40" " " "D-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MID1" " " "12" "0" "14" "01D-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MID1" "N" "12" "16" "10" " " "02D-MID1"
             RETURNING RESU.
       CALL "SD_From" USING 
            "0102D-MID1" BY REFERENCE W-NCM(1) "10" "1" "1" 10
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MID1" "X" "12" "26" "4" "0102D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-MID1" " " "14" "0" "14" "02D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0103D-MID1" "N" "14" "16" "10" " " "03D-MID1"
            RETURNING RESU.
       CALL "SD_From" USING 
           "0103D-MID1" BY REFERENCE W-NCM(1) "10" "1" "3" 10
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0203D-MID1" "X" "14" "26" "4" "0103D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-MID1" " " "16" "0" "14" "03D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0104D-MID1" "N" "16" "16" "10" " " "04D-MID1"
            RETURNING RESU.
       CALL "SD_From" USING 
           "0104D-MID1" BY REFERENCE W-NCM(1) "10" "1" "5" 10
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0204D-MID1" "X" "16" "26" "4" "0104D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-MID1" "X" "17" "16" "28" "04D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MID2" "N" "6" "10" "40" "D-MID1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NCM" "RN" "3" "67" "10" "D-MID2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-NCM" BY REFERENCE W-NCM(1) "10" "1" BY REFERENCE W-NRC 10
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNM" "N" "3" "37" "12" "D-NCM" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SNM" BY REFERENCE W-SON "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "W-L1" "0" "48" "D-SNM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NAME" "N" "W-L1" "21" "48" " " "05C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-NAME" BY REFERENCE W-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "W-L2" "0" "195" "05C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TBL1" " " "W-L2" "0" "45" " " "06C-DSP"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TBL1" "N" "W-L2" "9" "4" " " "D-TBL1"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TBL1" "N" "W-L2" "15" "4" "01D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TBL1" "N" "W-L2" "21" "4" "02D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TBL1" "N" "W-L2" "27" "4" "03D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-TBL1" "X" "W-L2" "34" "5" "04D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-TBL1" "X" "W-L2" "40" "5" "05D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-TBL1" "N" "W-L2" "45" "4" "06D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-TBL1" "X" "W-L2" "51" "5" "07D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-TBL1" "X" "W-L2" "57" "5" "08D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-TBL1" "X" "W-L2" "63" "5" "09D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TBL2" " " "W-L2" "0" "50" "D-TBL1" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TBL2" "X" "W-L2" "9" "5" " " "D-TBL2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TBL2" "X" "W-L2" "15" "5" "01D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TBL2" "X" "W-L2" "21" "5" "02D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TBL2" "X" "W-L2" "27" "5" "03D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-TBL2" "X" "W-L2" "33" "5" "04D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-TBL2" "X" "W-L2" "39" "5" "05D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-TBL2" "X" "W-L2" "45" "5" "06D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-TBL2" "X" "W-L2" "51" "5" "07D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-TBL2" "X" "W-L2" "57" "5" "08D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-TBL2" "X" "W-L2" "63" "5" "09D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TBL3" " " "W-L2" "0" "50" "D-TBL2" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TBL3" "X" "W-L2" "9" "5" " " "D-TBL3"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TBL3" "X" "W-L2" "15" "5" "01D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TBL3" "X" "W-L2" "21" "5" "02D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TBL3" "X" "W-L2" "27" "5" "03D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-TBL3" "X" "W-L2" "33" "5" "04D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-TBL3" "X" "W-L2" "39" "5" "05D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-TBL3" "X" "W-L2" "45" "5" "06D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-TBL3" "X" "W-L2" "51" "5" "07D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-TBL3" "X" "W-L2" "57" "5" "08D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-TBL4" "X" "W-L2" "63" "5" "COLUMN" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TBL4" " " "W-L2" "0" "50" "D-TBL3" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TBL4" "X" "W-L2" "9" "5" " " "D-TBL4"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TBL4" "X" "W-L2" "15" "5" "01D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-TBL4" "X" "W-L2" "21" "5" "02D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-TBL4" "X" "W-L2" "27" "5" "03D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-TBL4" "X" "W-L2" "33" "5" "04D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06D-TBL4" "X" "W-L2" "39" "5" "05D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07D-TBL4" "X" "W-L2" "45" "5" "06D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08D-TBL4" "X" "W-L2" "51" "5" "07D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09D-TBL4" "X" "W-L2" "57" "5" "08D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "10D-TBL4" "X" "W-L2" "63" "5" "09D-TBL4" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-DSP" " " "W-L3" "0" "62" "06C-DSP" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "ZZZZ-" "W-L3" "W-C" "5" " " "07C-DSP"
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU" BY REFERENCE W-SUD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GS" "---,---" "W-L3" "70" "7" "D-SU" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-GS" BY REFERENCE W-GS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU1" "ZZZZ-" "W-L3" "9" "5" "D-GS" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU1" BY REFERENCE W-SU(1) "4" "1" "1" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU2" "ZZZZ-" "W-L3" "15" "5" "D-SU1" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU2" BY REFERENCE W-SU(1) "4" "1" "2" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU3" "ZZZZ-" "W-L3" "21" "5" "D-SU2" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU3" BY REFERENCE W-SU(1) "4" "1" "3" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU4" "ZZZZ-" "W-L3" "27" "5" "D-SU3" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU4" BY REFERENCE W-SU(1) "4" "1" "4" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU5" "ZZZZ-" "W-L3" "33" "5" "D-SU4" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU5" BY REFERENCE W-SU(1) "4" "1" "5" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU6" "ZZZZ-" "W-L3" "39" "5" "D-SU5" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU6" BY REFERENCE W-SU(1) "4" "1" "6" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU7" "ZZZZ-" "W-L3" "45" "5" "D-SU6" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU7" BY REFERENCE W-SU(1) "4" "1" "7" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU8" "ZZZZ-" "W-L3" "51" "5" "D-SU7" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU8" BY REFERENCE W-SU(1) "4" "1" "8" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU9" "ZZZZ-" "W-L3" "57" "5" "D-SU8" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU9" BY REFERENCE W-SU(1) "4" "1" "9" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU10" "ZZZZ-" "W-L3" "63" "5" "D-SU9" " "
            RETURNING RESU.
       CALL "SD_From" USING 
           "D-SU10" BY REFERENCE W-SU(1) "4" "1" "10" 4 RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRN" "N" "1" "19" "42" "07C-DSP" " " RETURNING RESU.
      *C-SPC
       CALL "SD_Init" USING 
            "C-SPC" " " "0" "0" "169" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SPACE" " " "0" "0" "164" " " "C-SPC" RETURNING RESU.
       CALL "SD_Init" USING 
           "01S-SPACE" " " "W-L1" "0" "55" " " "S-SPACE" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-HCD" "X" "W-L1" "9" "6" " " "01S-SPACE" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-NAME" "X" "W-L1" "21" "48" "S-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SK" "X" "W-L1" "75" "1" "S-NAME" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-SPACE" " " "W-L2" "0" "50" "01S-SPACE" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0102S-SPACE" "X" "W-L2" "9" "5" " " "02S-SPACE"
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0202S-SPACE" "X" "W-L2" "15" "5" "0102S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0302S-SPACE" "X" "W-L2" "21" "5" "0202S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0402S-SPACE" "X" "W-L2" "27" "5" "0302S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0502S-SPACE" "X" "W-L2" "33" "5" "0402S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0602S-SPACE" "X" "W-L2" "39" "5" "0502S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0702S-SPACE" "X" "W-L2" "45" "5" "0602S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0802S-SPACE" "X" "W-L2" "51" "5" "0702S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0902S-SPACE" "X" "W-L2" "57" "5" "0802S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1002S-SPACE" "X" "W-L2" "63" "5" "0902S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03S-SPACE" " " "W-L3" "0" "59" "02S-SPACE" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SIZ" "X" "W-L3" "5" "1" " " "03S-SPACE" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203S-SPACE" "X" "W-L3" "9" "5" "S-SIZ" " "
             RETURNING RESU.
       CALL "SD_Init" USING 
            "0303S-SPACE" "X" "W-L3" "15" "5" "0203S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0403S-SPACE" "X" "W-L3" "21" "5" "0303S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0503S-SPACE" "X" "W-L3" "27" "5" "0403S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0603S-SPACE" "X" "W-L3" "33" "5" "0503S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0703S-SPACE" "X" "W-L3" "39" "5" "0603S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0803S-SPACE" "X" "W-L3" "45" "5" "0703S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0903S-SPACE" "X" "W-L3" "51" "5" "0803S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1003S-SPACE" "X" "W-L3" "57" "5" "0903S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "1103S-SPACE" "X" "W-L3" "63" "5" "1003S-SPACE"
           " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-GS" "X" "W-L3" "69" "8" "1103S-SPACE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-SPC" " " "W-L3" "0" "5" "S-SPACE" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SU" "X" "W-L3" "W-C" "5" " " "02C-SPC" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "394" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "394" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" " " "24" "0" "22" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME1" "X" "24" "15" "16" " " "E-ME1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME1" "9" "24" "35" "6" "01E-ME1" " " RETURNING RESU.
       CALL "SD_From" USING 
           "02E-ME1" BY REFERENCE HI-HCD "6" "0"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "21" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "26" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "19" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "18" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "N" "24" "15" "22" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "N" "24" "15" "14" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "N" "24" "15" "18" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "21" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "27" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "26" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "26" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" " " "24" "0" "23" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01E-ME18" "X" "24" "15" "17" " " "E-ME18" RETURNING RESU.
       CALL "SD_Init" USING 
            "02E-ME18" "9" "24" "35" "6" "01E-ME18" " " RETURNING RESU.
       CALL "SD_From" USING 
           "02E-ME18" BY REFERENCE HUH-HCD "6" "0"  RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "32" "32" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME98" "X" "24" "75" "5" "E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-CL" "X" "24" "10" "50" "E-ME99" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
           COPY LSMSG_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-NGD.
           MOVE D-NHNG TO W-NGDS.
           IF  W-NEND2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEND
           END-IF
           IF  W-NEND2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEND
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU
           ELSE
               MOVE W-MID1 TO H-MID
               CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE UTRAN_IDLST UTRAN_PNAME1
               GO TO M-040
           END-IF
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "C3_Set_Jrcode" USING 
            USER_ID BY REFERENCE COMPLETION_CODE  255.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       M-040.
           IF  JS-SIGN = 1
               GO TO M-070
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           PERFORM HKB-RTN THRU HKB-EX.
           IF  W-END = 9
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "DB_Close"
               STOP RUN
           END-IF
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON3-KEY" BY REFERENCE JCON3-KEY.
           MOVE "18" TO JCON5-KEY.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "DISP-BUZ-B" DISP-BUZ-B "p" RETURNING RESU
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               CALL "DB_Close"
               STOP RUN
           END-IF.
       M-070.
           MOVE ZERO TO W-D.
           INITIALIZE  G-AREA.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" HUH-M_PNAME1 "SHARED" BY REFERENCE HUH-M_IDLST "1"
            "HUH-KEY" BY REFERENCE HUH-KEY.
           CALL "DB_F_Open" USING
            "OUTPUT" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
       M-080.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "D-MID1" D-MID1 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-MID2" D-MID2 "p" RETURNING RESU
               GO TO M-120
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-NRC "A-NRC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           IF  W-NRC NOT = 1 AND 3 AND 5 AND 9
               GO TO M-100
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  JS-SIGN = 0
               IF  ESTAT = BTB
                   GO TO M-100
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 1
                   CALL "DB_F_Close" USING
                    BY REFERENCE HI-M_IDLST HI-M_PNAME1
                   CALL "DB_F_Close" USING
                    BY REFERENCE HUH-M_IDLST HUH-M_PNAME1
                   CALL "DB_F_Close" USING
                    BY REFERENCE UTRAN_IDLST UTRAN_PNAME1
                   CALL "C3_Set_Jrcode" USING 
                    USER_ID BY REFERENCE COMPLETION_CODE  255
                   CALL "SD_Output" USING
                    "C-CLEAR" C-CLEAR "p" RETURNING RESU
                   CALL "DB_Close"
                   STOP RUN
               ELSE
                   GO TO M-100
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-120
           END-IF
           IF  JS-SIGN = 1
               GO TO M-680
           END-IF
      *-----------------------------------------------------------------
           IF W-NRC = 9
               GO TO M-800
           END-IF
           MOVE ZERO TO W-D.
       M-200.
           CALL "SD_Screen_Output" USING "SCHD31" RETURNING RESU.
           CALL "SD_Output" USING "D-NCM" D-NCM "p" RETURNING RESU.
           MOVE ZERO TO W-AIDD.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = C2 OR PF9
               GO TO M-800
           END-IF
           IF  ESTAT = BTB
               GO TO M-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-NGPS = ZERO
               MOVE DATE-02R TO W-NGPS
               CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           IF  W-GET < 1 OR > 12
               GO TO M-220
           END-IF
           IF  W-PEY < 1 OR > 31
               GO TO M-220
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NG NOT = W-NGD
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-220
           END-IF.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           IF  W-SKC = 9
               GO TO M-240
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-SKC TO JCON3-02.
      *           READ JCON UNLOCK INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               GO TO M-240
           END-IF
           MOVE JCON3-03 TO W-SON.
           CALL "SD_Output" USING "D-SNM" D-SNM "p" RETURNING RESU.
           MOVE W-SKC TO W-SCD.
       M-260.
           CALL "SD_Screen_Output" USING "SCHD31" RETURNING RESU.
           CALL "SD_Output" USING "D-NCM" D-NCM "p" RETURNING RESU.
           MOVE ZERO TO W-ID.
           MOVE W-NGP TO W-DATE.
           MOVE W-SKC TO W-SCD.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNM" D-SNM "p" RETURNING RESU.
           MOVE 1 TO W-GN.
           MOVE 4 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           COMPUTE W-L3 = W-L2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           MOVE W-AID(W-GN) TO W-ID.
           MOVE ZERO TO W-PMCD.
       M-280.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-520
           END-IF
           IF  ESTAT = C1 OR PF6
               GO TO M-440
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-280
           END-IF
           IF  W-HCD = ZERO
               IF  W-FHCD NOT = ZERO
                   MOVE W-FHCD TO W-HCD
               END-IF
           END-IF
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-280
           END-IF
           MOVE W-HCD TO W-FHCD.
           MOVE W-NGP TO W-DATE.
           MOVE W-SKC TO W-SCD.
           MOVE HI-NAME TO W-NAME.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
      *
           PERFORM TAN-RTN THRU TAN-EX.
           IF  W-ERR = 1
               GO TO M-280
           END-IF
           IF  HI-FT < 2
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               IF  W-NRC = 5
                   GO TO M-280
               END-IF
           END-IF
           IF  HI-BC22 NOT = 1
               IF  W-NRC = 1 OR 2
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           IF  HI-BC22 = 1
               IF  W-NRC NOT = 3
                   MOVE HI-KRC TO W-SSC
                   CALL "SD_Output" USING "A-SK" A-SK "p" RETURNING RESU
               END-IF
           END-IF.
       M-300.
           CALL "SD_Accept" USING BY REFERENCE A-SK "A-SK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-300
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "42" TO HKB-NO.
           MOVE W-SSC TO HKB-SSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-300
           END-IF
           CALL "SD_Output" USING "A-SK" A-SK "p" RETURNING RESU.
           IF  W-SSC = 1 OR 2 OR 3
               IF  W-NRC = 3
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO M-300
               END-IF
           END-IF
           IF  W-SSC = 0 OR 4 OR 5 OR 6
               IF  W-NRC = 1 OR 2
                   CALL "SD_Output" USING
                    "E-ME5" E-ME5 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO M-300
               END-IF
           END-IF.
       M-320.
           CALL "SD_Accept" USING BY REFERENCE A-SIZ "A-SIZ" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           IF  W-SIZ < 1 OR > 4
               GO TO M-320
           END-IF
           IF  W-ID1 NOT = W-IDD
               MOVE ZERO TO W-ID2
               PERFORM CLR-RTN THRU CLR-EX
           END-IF
           PERFORM BIT-RTN THRU BIT-EX.
           MOVE ZERO TO CNT W-EC.
           MOVE 3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       M-340.
           IF  CNT = 10
               MOVE ZERO TO CHK CNT W-GS W-PMC(W-GN)
               GO TO M-400
           END-IF
           ADD 1 TO CNT.
           ADD 6 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-BITD(CNT) = 0
               MOVE ZERO TO W-SUD
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               MOVE W-SUD TO W-SU(CNT)
               GO TO M-340
           END-IF
           IF  W-EC NOT = 5
               GO TO M-380
           END-IF
           MOVE ZERO TO W-SUD.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           MOVE W-SUD TO W-SU(CNT).
           GO TO M-340.
       M-360.
           SUBTRACT 1 FROM CNT.
           SUBTRACT 6 FROM W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  CNT = 0
               GO TO M-320
           END-IF
           IF  W-BITD(CNT) = 0
               GO TO M-360
           END-IF.
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-360
           END-IF
           IF  ESTAT = ADV
               MOVE 5 TO W-EC
           END-IF
           IF  ESTAT NOT = HTB AND SKP AND ADV
               GO TO M-380
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           MOVE W-SUD TO W-SU(CNT).
           GO TO M-340.
       M-400.
           IF  CNT = 10
               GO TO M-420
           END-IF
           ADD 1 TO CNT.
           ADD W-SU(CNT) TO W-GS.
           IF  CHK NOT = 9
               IF  W-SU(CNT) NOT = ZERO
                   MOVE 9 TO CHK
               END-IF
           END-IF
           IF  W-SU(CNT) > 0
               MOVE 1 TO W-PMC(W-GN)
           END-IF
           GO TO M-400.
       M-420.
           CALL "SD_Output" USING "D-GS" D-GS "p" RETURNING RESU.
           MOVE ZERO TO W-EC.
           MOVE W-ID TO W-AID(W-GN).
           MOVE ZERO TO W-ID.
           IF  CHK NOT = 9
               GO TO M-280
           END-IF
           ADD 1 TO W-GN.
           ADD 3 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           COMPUTE W-L3 = W-L2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-GN = 7
               GO TO M-460
           END-IF
           MOVE W-AID(W-GN) TO W-ID.
           MOVE W-ID1 TO W-IDD.
           GO TO M-280.
       M-440.
           MOVE ZERO TO W-ID.
           MOVE W-ID TO W-AID(W-GN).
           MOVE 0 TO W-PMC(W-GN).
           CALL "SD_Output" USING "S-SPACE" S-SPACE "p" RETURNING RESU.
           ADD 1 TO W-GN.
           ADD 3 TO W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           COMPUTE W-L3 = W-L2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-GN NOT = 7
               GO TO M-440
           END-IF.
       M-460.
       M-480.
           CALL "SD_Accept" USING BY REFERENCE A-HPC "A-HPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-520
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-480
           END-IF
           IF  W-HPC NOT = 0 AND 1
               GO TO M-480
           END-IF.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING
            "DISP-MSG-SPACE" DISP-MSG-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-480
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           IF  W-DMM = 1
               GO TO M-600
           END-IF
           IF  W-DMM = 9
               GO TO M-260
           END-IF
           GO TO M-500.
       M-520.
           SUBTRACT 1 FROM W-GN.
           SUBTRACT 3 FROM W-L1.
           CALL "SD_Arg_Match_Line" USING
            "W-L1" "2" W-L1 RETURNING RESU.
           COMPUTE W-L2 = W-L1 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L2" "2" W-L2 RETURNING RESU.
           COMPUTE W-L3 = W-L2 + 1.
           CALL "SD_Arg_Match_Line" USING
            "W-L3" "2" W-L3 RETURNING RESU.
           IF  W-GN = ZERO
               GO TO M-200
           END-IF
           MOVE 11 TO CNT.
           MOVE 69 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO W-ID.
           MOVE W-AID(W-GN) TO W-ID.
           IF  W-HCD = ZERO
               GO TO M-520
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           PERFORM BIT-RTN THRU BIT-EX.
           MOVE W-ID1 TO W-IDD.
           GO TO M-360.
       M-600.
      *<<< 更新　＆　作表　処理 >>>*
           MOVE ZERO TO W-GN.
           MOVE ZERO TO WK-UNO.
       M-620.
           ADD 1 TO W-GN.
           IF  W-GN = 7
               GO TO M-660
           END-IF
           MOVE ZERO TO W-ID.
           MOVE W-AID(W-GN) TO W-ID.
           IF  W-HCD = ZERO
               GO TO M-660
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           MOVE ZERO TO CNT W-BK W-FK.
           IF (W-NRC = 2) OR (W-GN NOT = 1)
               GO TO M-640
           END-IF
           MOVE "18" TO JCON5-KEY  ERR-K.
      *           READ JCON INVALID
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JCON_PNAME1 BY REFERENCE JCON-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           ADD 1 TO JCON5-03.
           MOVE JCON5-03 TO WK-UNO.
      *           REWRITE JCON5-R  INVALID
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON5-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF.
       M-640.
           COMPUTE W-BK = W-GS * HI-SB.
           COMPUTE W-FK = W-GS * HI-FT.
      *
           MOVE ZERO TO UTRAN-R.
           MOVE WK-UNO TO UTRAN-UNO.
           MOVE W-GN TO UTRAN-GYO.
           MOVE W-DATE TO UTRAN-DATE.
           MOVE W-HCD TO UTRAN-HCD.
           MOVE W-SIZ TO UTRAN-SIZ.
           MOVE W-SU(1) TO UTRAN-SU(1).
           MOVE W-SU(2) TO UTRAN-SU(2).
           MOVE W-SU(3) TO UTRAN-SU(3).
           MOVE W-SU(4) TO UTRAN-SU(4).
           MOVE W-SU(5) TO UTRAN-SU(5).
           MOVE W-SU(6) TO UTRAN-SU(6).
           MOVE W-SU(7) TO UTRAN-SU(7).
           MOVE W-SU(8) TO UTRAN-SU(8).
           MOVE W-SU(9) TO UTRAN-SU(9).
           MOVE W-SU(10) TO UTRAN-SU(10).
           MOVE W-GS TO UTRAN-SUT.
           MOVE W-BK TO UTRAN-BKIN.
           MOVE W-FK TO UTRAN-FKIN.
           MOVE W-NRC TO UTRAN-NRC.
           MOVE W-SSC TO UTRAN-SSC.
           MOVE W-HPC TO UTRAN-HPC.
           MOVE W-SCD TO UTRAN-SKC.
           MOVE HI-BC1 TO UTRAN-BC1.
           MOVE HI-BC2 TO UTRAN-BC2.
           MOVE HI-BC3 TO UTRAN-BC3.
           MOVE HI-BMC TO UTRAN-BMC.
           MOVE HI-BMNO TO UTRAN-BMNO.
      *           WRITE UTRAN-R.
      *//////////////
           CALL "DB_Insert" USING
            UTRAN_PNAME1 UTRAN_LNAME UTRAN-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-800
           END-IF
           GO TO M-620.
       M-660.
           MOVE ZERO TO W-AIDD.
           GO TO M-260.
       M-680.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  COMPLETION_CODE = 255
               GO TO M-980
           END-IF
           PERFORM KOB-RTN THRU KOB-EX.
       M-800.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRN" D-PRN "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
       M-820.
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
               GO TO M-980
           END-IF
           IF  UTRAN-PRC = 9
               GO TO M-820
           END-IF
           MOVE ZERO TO WA-D W-DEC.
           MOVE ZERO TO W-D G-AREA W-DC.
       M-840.
           PERFORM MEI-RTN THRU MEI-EX.
       M-860.
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-DEC
               GO TO M-980
           END-IF
           IF  UTRAN-PRC = 9
               GO TO M-860
           END-IF
           GO TO M-840.
       M-980.
           IF  W-POC NOT = ZERO
               PERFORM KEI-RTN THRU KEI-EX
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE HUH-M_IDLST HUH-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       BIT-RTN.
           IF  W-SIZ = 1
               MOVE HI-SS1 TO W-BIT
               CALL "SD_Output" USING
                "D-TBL1" D-TBL1 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 2
               MOVE HI-SS2 TO W-BIT
               CALL "SD_Output" USING
                "D-TBL2" D-TBL2 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 3
               MOVE HI-SS3 TO W-BIT
               CALL "SD_Output" USING
                "D-TBL3" D-TBL3 "p" RETURNING RESU
           END-IF
           IF  W-SIZ = 4
               MOVE HI-SS4 TO W-BIT
               MOVE ZERO TO W-BITD(10)
               CALL "SD_Output" USING
                "D-TBL4" D-TBL4 "p" RETURNING RESU
           END-IF.
       BIT-EX.
           EXIT.
       CLR-RTN.
           CALL "SD_Output" USING "S-GS" S-GS "p" RETURNING RESU.
           MOVE 3 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       CLR-020.
           ADD 6 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-C < 63
               CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
               GO TO CLR-020
           END-IF.
       CLR-EX.
           EXIT.
       TAN-RTN.
           MOVE 0 TO W-ERR.
           MOVE W-HCD TO HUH-KEY.
      *           READ HUH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HUH-M_PNAME1 BY REFERENCE HUH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO TAN-100
           END-IF
           IF (HUH-ZK NOT = ZERO) AND (HUH-ZS NOT = ZERO)
               COMPUTE W-FT = HUH-ZK / HUH-ZS
               IF  HI-FT NOT = W-FT
                   CALL "SD_Output" USING
                    "E-ME19" E-ME19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO TAN-100
               END-IF
           END-IF
           IF (HUH-NK NOT = ZERO) AND (HUH-NS NOT = ZERO)
               COMPUTE W-FT = HUH-NK / HUH-NS
               IF  HI-FT NOT = W-FT
                   CALL "SD_Output" USING
                    "E-ME19" E-ME19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO TAN-100
               END-IF
           END-IF
           IF (HUH-UG NOT = ZERO) AND (HUH-SS NOT = ZERO)
               COMPUTE W-FT = HUH-UG / HUH-SS
               IF  HI-FT NOT = W-FT
                   CALL "SD_Output" USING
                    "E-ME19" E-ME19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO TAN-100
               END-IF
           END-IF
           IF (HUH-YK NOT = ZERO) AND (HUH-YS NOT = ZERO)
               COMPUTE W-FT = HUH-YK / HUH-YS
               IF  HI-FT NOT = W-FT
                   CALL "SD_Output" USING
                    "E-ME19" E-ME19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO TAN-100
               END-IF
           END-IF
           GO TO TAN-EX.
       TAN-100.
           IF  JS-SIGN = 0
               MOVE 1 TO W-ERR
           ELSE
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  255
           END-IF.
       TAN-EX.
           EXIT.
       CHK-RTN.
           CALL "DB_F_Open" USING
            "INPUT" HSS-F_PNAME1 " " BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       CHK-020.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-100
           END-IF
           IF  HSS-NG NOT = W-NGD
               GO TO CHK-020
           END-IF
           IF  HSS-HHC = 1
               GO TO CHK-020
           END-IF
           IF  HSS-HKC NOT = 1
               GO TO CHK-020
           END-IF
      *
           MOVE HSS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO CHK-020
           END-IF
           IF  HI-FT = ZERO
               GO TO CHK-020
           END-IF
      *
           MOVE HSS-HCD TO W-HCD.
           PERFORM TAN-RTN THRU TAN-EX.
           IF  COMPLETION_CODE = 255
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               GO TO CHK-EX
           END-IF
           GO TO CHK-020.
       CHK-100.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
      *
       CHK-EX.
           EXIT.
       KOB-RTN.
           CALL "DB_F_Open" USING
            "I-O" HSS-F_PNAME1 " " BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
       KOB-020.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KOB-140
           END-IF
           IF  HSS-NG NOT = W-NGD
               GO TO KOB-020
           END-IF
           IF  HSS-HHC = 1
               GO TO KOB-020
           END-IF
           IF  HSS-HKC NOT = 1
               GO TO KOB-020
           END-IF
      *
           MOVE HSS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO KOB-020
           END-IF
           IF  HI-FT = ZERO
               GO TO KOB-020
           END-IF
      *
           MOVE ZERO TO W-SIZ W-GN.
       KOB-040.
           ADD 1 TO W-SIZ.
           IF  W-SIZ = 5
               GO TO KOB-120
           END-IF
           IF  ZERO = HSS-SU(W-SIZ,01) AND HSS-SU(W-SIZ,02) AND
                     HSS-SU(W-SIZ,03) AND HSS-SU(W-SIZ,04) AND
                     HSS-SU(W-SIZ,05) AND HSS-SU(W-SIZ,06) AND
                     HSS-SU(W-SIZ,07) AND HSS-SU(W-SIZ,08) AND
                     HSS-SU(W-SIZ,09) AND HSS-SU(W-SIZ,10)
               GO TO KOB-040
           END-IF
      *
           ADD 1 TO W-GN.
           MOVE ZERO TO UTRAN-R.
           MOVE HSS-UNO TO UTRAN-UNO.
           MOVE W-GN TO UTRAN-GYO.
           MOVE HSS-DATE TO UTRAN-DATE.
           MOVE HSS-HCD TO UTRAN-HCD.
           MOVE W-SIZ TO UTRAN-SIZ.
           MOVE 3 TO UTRAN-NRC.
           MOVE 0 TO UTRAN-SSC.
           MOVE 0 TO UTRAN-HPC.
           MOVE HSS-SKC TO UTRAN-SKC.
           MOVE HI-BC TO UTRAN-BC.
           MOVE HI-BMC TO UTRAN-BMC.
           MOVE HI-BMNO TO UTRAN-BMNO.
           MOVE HSS-DNO TO UTRAN-KBN.
      *
           MOVE ZERO TO CNT.
       KOB-060.
           ADD 1 TO CNT.
           IF  CNT NOT = 11
               MOVE HSS-SU(W-SIZ,CNT) TO UTRAN-SU(CNT)
               ADD HSS-SU(W-SIZ,CNT) TO UTRAN-SUT
               GO TO KOB-060
           END-IF
           COMPUTE UTRAN-BKIN = UTRAN-SUT * HI-SB.
           COMPUTE UTRAN-FKIN = UTRAN-SUT * HI-FT.
      *           WRITE UTRAN-R.
      *//////////////
           CALL "DB_Insert" USING
            UTRAN_PNAME1 UTRAN_LNAME UTRAN-R RETURNING RET.
           IF  ERR-STAT NOT = "00"
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KOB-080
           END-IF
           GO TO KOB-040.
       KOB-080.
           CALL "DB_F_Close" USING
            BY REFERENCE UTRAN_IDLST UTRAN_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" UTRAN_PNAME1 " " BY REFERENCE UTRAN_IDLST "0".
       KOB-100.
      *           READ UTRAN AT END
      *///////////////
           CALL "DB_Read" USING
            "AT END" UTRAN_PNAME1 BY REFERENCE UTRAN-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO KOB-140
           END-IF
           IF  HSS-UNO NOT = UTRAN-UNO
               GO TO KOB-100
           END-IF
           MOVE ALL X"FF" TO UTRAN-R.
      *           REWRITE UTRAN-R.
      *///////////////
           CALL "DB_Update" USING
            UTRAN_PNAME1 UTRAN_LNAME UTRAN-R RETURNING RET.
           GO TO KOB-100.
       KOB-120.
           MOVE 1 TO HSS-HHC.
      *           REWRITE HSS-R INVALID
      *///////////////
           CALL "DB_Update" USING
            HSS-F_PNAME1 HSS-F_LNAME HSS-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO KOB-140
           END-IF
           GO TO KOB-020.
       KOB-140.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
       KOB-EX.
           EXIT.
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
           MOVE HEAD4 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD5 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE HEAD6 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
       MEI-RTN.
           IF  W-POC = ZERO
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-010 THRU MID-EX
           END-IF
           MOVE SPACE TO W-P1 W-P2.
           MOVE SPACE TO P-NAME.
           IF  UTRAN-NRC = W-NRCD
               IF  UTRAN-DATE = W-DATED
                   IF  UTRAN-SSC = W-SSCD
                       IF  UTRAN-HCD = W-HCDD
                           IF  UTRAN-SKC = W-SKCD
                               IF  UTRAN-HPC = W-HPCD
                                   GO TO MEI-020
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE UTRAN-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           MOVE HI-NAME TO W-NAME.
           IF (UTRAN-NRC NOT = W-NRCD) OR (UTRAN-DATE NOT = W-DATED)
               PERFORM KEI-RTN THRU KEI-EX
               MOVE UTRAN-NRC TO W-NRCD P-NRC
               MOVE UTRAN-DATE TO W-DATED
               MOVE UTRAN-NGPS TO P-DATE
           END-IF
           MOVE UTRAN-HPC TO W-HPCD.
           MOVE UTRAN-SSC TO W-SSCD P-SSC.
           MOVE UTRAN-HCD TO W-HCDD P-HCD.
           MOVE UTRAN-SKC TO W-SKCD.
           MOVE W-NAME TO P-NAME.
           IF  UTRAN-NRC NOT = 2
               MOVE UTRAN-UNO TO P-UNO
               MOVE UTRAN-SKC TO P-SKC
           END-IF
           MOVE UTRAN-HPC TO P-HPC.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               MOVE UTRAN-NRC TO P-NRC
               MOVE UTRAN-NGPS TO P-DATE
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-020.
           MOVE UTRAN-SIZ TO P-SIZ.
           MOVE ZERO TO CNT.
       MEI-040.
           IF  CNT NOT = 10
               ADD 1 TO CNT
               MOVE UTRAN-SU(CNT) TO P-SU(CNT)
               GO TO MEI-040
           END-IF
           MOVE UTRAN-SUT TO P-GS.
           MOVE UTRAN-BKIN TO P-BK.
           MOVE UTRAN-FKIN TO P-FK.
      *
           ADD UTRAN-SUT TO G-GS.
           ADD UTRAN-BKIN TO G-BK.
           ADD UTRAN-FKIN TO G-FK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER < 61
               GO TO MEI-060
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           MOVE UTRAN-NRC TO P-NRC.
           MOVE UTRAN-NGPS TO P-DATE.
           MOVE UTRAN-SSC TO P-SSC.
           MOVE UTRAN-HCD TO P-HCD.
           MOVE W-NAME TO P-NAME.
           IF  UTRAN-NRC NOT = 2
               MOVE UTRAN-UNO TO P-UNO
               MOVE UTRAN-SKC TO P-SKC
           END-IF
           MOVE UTRAN-HPC TO P-HPC.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MEI-060.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           ADD 1 TO W-DC.
      *
           MOVE 9 TO UTRAN-PRC.
      *           REWRITE UTRAN-R.
      *///////////////
           CALL "DB_Update" USING
            UTRAN_PNAME1 UTRAN_LNAME UTRAN-R RETURNING RET.
       MEI-EX.
           EXIT.
       KEI-RTN.
           IF  W-DC = 0
               GO TO KEI-020
           END-IF
           IF  W-DC = 1
               MOVE SPACE TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               GO TO KEI-010
           END-IF
           IF  W-POC = ZERO
               MOVE 5 TO W-POC
               GO TO KEI-EX
           END-IF
           MOVE "　［　合　計　］" TO P-GOK.
           MOVE G-GS TO P-GGS.
           MOVE G-BK TO P-GBK.
           MOVE G-FK TO P-GFK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       KEI-010.
           IF  JS-SIGN NOT = 1
               GO TO KEI-020
           END-IF
           IF  W-NRCD = 1 OR 3
               ADD G-GS TO WA-GS
               ADD G-BK TO WA-BK
               ADD G-FK TO WA-FK
           END-IF
           IF  W-DEC = 0
               GO TO KEI-020
           END-IF
           MOVE "【　総合計　】　" TO P-GOK.
           MOVE WA-GS TO P-GGS.
           MOVE WA-BK TO P-GBK.
           MOVE WA-FK TO P-GFK.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 60
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
       KEI-020.
           INITIALIZE G-AREA.
           MOVE ZERO TO W-DC.
       KEI-EX.
           EXIT.
       HKB-RTN.
           MOVE SPACE TO HKB-KEY.
           MOVE "41" TO HKB-NO.
      *           START HKBM KEY NOT < HKB-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HKBM_PNAME1 "HKB-KEY" " NOT < " HKB-KEY RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               GO TO HKB-EX
           END-IF.
       HKB-020.
      *           READ HKBM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO HKB-100
           END-IF
           IF  HKB-NO NOT = "41"
               GO TO HKB-100
           END-IF
           IF  HKB-SUC < 1 OR > 5
               GO TO HKB-100
           END-IF
           MOVE HKB-SUNA TO W-NCM(HKB-SUC).
           GO TO HKB-020.
       HKB-100.
           IF  W-NCMA = SPACE
               MOVE 9 TO W-END
           END-IF.
       HKB-EX.
           EXIT.
