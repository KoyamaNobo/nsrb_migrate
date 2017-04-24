       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHM110.
      *********************************************************
      *    PROGRAM         :  工品区分マスターメンテナンス    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ------                          *
      *        変更　　　  :  62/03/25                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(031) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　工品区分マスター　プルーフリスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(005) VALUE "区　分　名".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "名　　称".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(005) VALUE "区　分　名".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "名　　称".
           02  F              PIC  X(024) VALUE SPACE.
       01  W-P.
           02  W-PD    OCCURS  59.
             03  P-NO1        PIC  X(002).
             03  F            PIC  X(002).
             03  P-NON1       PIC  N(008).
             03  F            PIC  X(002).
             03  P-BC1        PIC  X(005).
             03  F            PIC  X(002).
             03  P-MS1        PIC  N(016).
             03  P-MSA1  REDEFINES P-MS1  PIC  X(032).
             03  P-JSD1  REDEFINES P-MS1.
               04  P-JSN1     PIC  N(008).
               04  F          PIC  X(002).
               04  P-SCO1     PIC  9(004).
               04  F          PIC  X(010).
             03  P-JJD1  REDEFINES P-MS1.
               04  P-JRC11    PIC  9(006).
               04  F          PIC  X(002).
               04  P-JRC12    PIC  9(006).
               04  F          PIC  X(002).
               04  P-JRC13    PIC  9(006).
               04  F          PIC  X(002).
               04  P-JRC14    PIC  9(006).
               04  F          PIC  X(002).
             03  F            PIC  X(002).
             03  P-X          PIC  X(001).
             03  F            PIC  X(002).
             03  P-NO2        PIC  X(002).
             03  F            PIC  X(002).
             03  P-NON2       PIC  N(008).
             03  F            PIC  X(002).
             03  P-BC2        PIC  X(005).
             03  F            PIC  X(002).
             03  P-MS2        PIC  N(016).
             03  P-MSA2  REDEFINES P-MS2  PIC  X(032).
             03  P-JSD2  REDEFINES P-MS2.
               04  P-JSN2     PIC  N(008).
               04  F          PIC  X(002).
               04  P-SCO2     PIC  9(004).
               04  F          PIC  X(010).
             03  P-JJD2  REDEFINES P-MS2.
               04  P-JRC21    PIC  9(006).
               04  F          PIC  X(002).
               04  P-JRC22    PIC  9(006).
               04  F          PIC  X(002).
               04  P-JRC23    PIC  9(006).
               04  F          PIC  X(002).
               04  P-JRC24    PIC  9(006).
               04  F          PIC  X(002).
       01  W-R.
           02  W-KEY.
             03  W-NO         PIC  9(002).
             03  W-BC         PIC  X(005).
             03  W-BC1  REDEFINES W-BC.
               04  W-YC       PIC  9(002).
               04  F          PIC  X(003).
             03  W-BC4  REDEFINES W-BC.
               04  W-KS2      PIC  9(002).
               04  F          PIC  X(003).
             03  W-BC5  REDEFINES W-BC.
               04  W-FRC      PIC  9(002).
               04  F          PIC  X(003).
      *    * * *   N A M E  ｺ ｳ ﾓ ｸ   * * *
           02  W-NAME1.
             03  W-YCN        PIC  N(016).
             03  W-YCND  REDEFINES W-YCN.
               04  W-YCN1     PIC  N(012).
               04  W-YCN2     PIC  N(004).
             03  F            PIC  X(025).
           02  W-NAME4  REDEFINES W-NAME1.
             03  W-KSN2       PIC  X(006).
             03  F            PIC  X(051).
           02  W-NAME5  REDEFINES W-NAME1.
             03  W-FRN        PIC  N(006).
             03  F            PIC  X(045).
           02  W-NAME90 REDEFINES W-NAME1.
             03  W-ASCD.
               04  W-SCD      PIC  9(001) OCCURS  15.
             03  W-SC   REDEFINES W-ASCD.
               04  W-SC01     PIC  9(001).
               04  W-SC02     PIC  9(001).
               04  W-SC03     PIC  9(001).
               04  W-SC04     PIC  9(001).
               04  W-SC05     PIC  9(001).
               04  W-SC06     PIC  9(001).
               04  W-SC07     PIC  9(001).
               04  W-SC08     PIC  9(001).
               04  W-SC09     PIC  9(001).
               04  W-SC10     PIC  9(001).
               04  W-SC11     PIC  9(001).
               04  W-SC12     PIC  9(001).
               04  W-SC13     PIC  9(001).
               04  W-SC14     PIC  9(001).
               04  W-SC15     PIC  9(001).
             03  F            PIC  X(042).
       01  W-DATA.
           02  W-D.
             03  W-NOD        PIC  9(002).
             03  W-NON        PIC  N(008).
             03  W-ACT        PIC  9(001).
             03  W-BKC        PIC  X(005).
             03  W-DMM        PIC  9(001).
             03  CHK          PIC  9(001).
             03  W-PC         PIC  9(001).
             03  W-LIN        PIC  9(002).
             03  W-PAGE       PIC  9(002).
           02  W-ALC.
             03  F            PIC  X(032) VALUE
                  "06360736083609361036113612361336".
             03  F            PIC  X(028) VALUE
                  "0651075108510951105111511251".
           02  W-ALCD  REDEFINES W-ALC.
             03  W-CLC        PIC  9(004) OCCURS  15.
           02  W-LC.
             03  W-L          PIC  9(002).
             03  W-C          PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-ANO.
             03  W-NO01       PIC  N(008) VALUE "用　途　区　分　".
             03  W-NO04       PIC  N(008) VALUE "機種（入力）　　".
             03  W-NO05       PIC  N(008) VALUE "廃　却　不　良　".
             03  W-NO70       PIC  N(008) VALUE "作　　　　表　　".
             03  W-NO80       PIC  N(008) VALUE "システム使用区分".
             03  W-NO99       PIC  N(008) VALUE "終　　　　了　　".
           02  W-LCD.
             03  W-LD         PIC  9(002).
             03  W-CD         PIC  9(001).
           02  W-POC          PIC  9(001).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKKBM.
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
           02  C-CL    PIC  X(12) VALUE "CLEAR SCREEN".
       01  C-MID.
           02  FILLER  PIC  N(025) VALUE
                "＊＊＊　　工品区分マスター　メンテナンス　　＊＊＊".
       01  C-MID0.
           02  FILLER.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  X(005) VALUE " = 01".
           02  FILLER.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  X(005) VALUE " = 04".
           02  FILLER.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  X(005) VALUE " = 05".
           02  FILLER  PIC  X(021) VALUE
                "---------------------".
           02  FILLER.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  X(005) VALUE " = 70".
           02  FILLER.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  X(005) VALUE " = 80".
           02  FILLER  PIC  X(021) VALUE
                "---------------------".
           02  FILLER.
             03  FILLER  PIC  N(008).
             03  FILLER  PIC  X(012) VALUE " = 99 ...   ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  X(033) VALUE
                "ACT : 登録=1 修正=2 削除=3 終了=9".
           02  FILLER  PIC  X(016) VALUE
                "確認 : OK=1 NO=9".
       01  C-MID2.
           02  FILLER  PIC  X(047) VALUE
                "ACT ｺｰﾄﾞ   名　称                          確認".
       01  C-MID07.
           02  FILLER  PIC  X(047) VALUE
                "ACT ｺｰﾄﾞ   名　称         仕入先ｺｰﾄﾞ       確認".
       01  C-MID09.
           02  FILLER  PIC  X(047) VALUE
                "ACT      材料ｺｰﾄﾞ1      2      3      4    確認".
       01  C-MID80.
           02  FILLER  PIC  X(026) VALUE
                "売　　上       －－－－   ".
           02  FILLER  PIC  X(026) VALUE
                "売上変換       －－－－   ".
           02  FILLER  PIC  X(026) VALUE
                "値　　引       －－－－   ".
           02  FILLER  PIC  X(026) VALUE
                "入　　金       払出更新   ".
           02  FILLER  PIC  X(026) VALUE
                "加　　硫       日次更新   ".
           02  FILLER  PIC  X(026) VALUE
                "廃　　却       予定変換   ".
           02  FILLER  PIC  X(026) VALUE
                "－－－－       月次更新   ".
           02  FILLER  PIC  X(026) VALUE
                "－－－－                  ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NO    PIC  9(002).
           02  FILLER.
             03  A-ACT   PIC  9(001).
      *    -------------  ﾖ ｳ ﾄ  ---------------------
             03  A-YC    PIC  9(002).
             03  A-YCN   PIC  N(012).
      *    ------------  ｷ ｼ ｭ 2  --------------------
             03  A-KS2   PIC  9(002).
             03  A-KSN2  PIC  X(006).
      *    ------------  ﾌ ﾘ ｮ ｳ  --------------------
             03  A-FRC   PIC  9(002).
             03  A-FRN   PIC  N(006).
      *    -------------------------------------------
             03  A-DMM   PIC  9(001).
      *    ++++++++++++  ｼﾖｳ ｸﾌﾞﾝ  +++++++++++++++++++
           02  A-SCD   PIC  9(001).
           02  A-DMMD  PIC  9(001).
       01  C-DSP.
           02  D-NON.
             03  FILLER  PIC  X(018) VALUE
                  "                  ".
             03  FILLER  PIC  N(008).
           02  FILLER.
             03  D-CL    PIC  X(050) VALUE
                  "                                                  ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  ﾏｽﾀｰ ﾅｼ  ***".
             03  E-ME3   PIC  X(015) VALUE
                  "***  ｷｬﾝｾﾙ  ***".
             03  E-ME11  PIC  X(019) VALUE
                  "***  WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(021) VALUE
                  "***  REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(020) VALUE
                  "***  DELETE ｴﾗｰ  ***".
           COPY LSSEM.
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
            "C-MID" " " "0" "0" "50" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID" "N" "1" "14" "50" " " "C-MID" RETURNING RESU.
      *C-MID0
       CALL "SD_Init" USING 
            "C-MID0" " " "0" "0" "197" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID0" " " "4" "0" "21" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0101C-MID0" "N" "4" "25" "16" " " "01C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0101C-MID0" BY REFERENCE W-NO01 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0201C-MID0" "X" "4" "41" "5" "0101C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID0" " " "7" "0" "21" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102C-MID0" "N" "7" "25" "16" " " "02C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0102C-MID0" BY REFERENCE W-NO04 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202C-MID0" "X" "7" "41" "5" "0102C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID0" " " "8" "0" "21" "02C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0103C-MID0" "N" "8" "25" "16" " " "03C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0103C-MID0" BY REFERENCE W-NO05 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0203C-MID0" "X" "8" "41" "5" "0103C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID0" "X" "13" "25" "21" "03C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID0" " " "14" "0" "21" "04C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105C-MID0" "N" "14" "25" "16" " " "05C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0105C-MID0" BY REFERENCE W-NO70 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205C-MID0" "X" "14" "41" "5" "0105C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID0" " " "15" "0" "21" "05C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0106C-MID0" "N" "15" "25" "16" " " "06C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0106C-MID0" BY REFERENCE W-NO80 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0206C-MID0" "X" "15" "41" "5" "0106C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID0" "X" "16" "25" "21" "06C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID0" " " "17" "0" "28" "07C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0108C-MID0" "N" "17" "25" "16" " " "08C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING 
            "0108C-MID0" BY REFERENCE W-NO99 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "0208C-MID0" "X" "17" "41" "12" "0108C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID0" "X" "22" "28" "22" "08C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING 
            "C-MID1" " " "0" "0" "49" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID1" "X" "4" "18" "33" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID1" "X" "22" "45" "16" "01C-MID1" " " RETURNING RESU.
      *C-MID2
       CALL "SD_Init" USING 
            "C-MID2" " " "0" "0" "47" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID2" "X" "6" "15" "47" " " "C-MID2" RETURNING RESU.
      *C-MID07
       CALL "SD_Init" USING 
            "C-MID07" " " "0" "0" "47" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID07" "X" "6" "15" "47" " " "C-MID07" RETURNING RESU.
      *C-MID09
       CALL "SD_Init" USING 
            "C-MID09" " " "0" "0" "47" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID09" "X" "6" "15" "47" " " "C-MID09" RETURNING RESU.
      *C-MID80
       CALL "SD_Init" USING 
            "C-MID80" " " "0" "0" "230" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-MID80" "X" "6" "26" "26" " " "C-MID80" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-MID80" "X" "7" "26" "26" "01C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-MID80" "X" "8" "26" "26" "02C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-MID80" "X" "9" "26" "26" "03C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-MID80" "X" "10" "26" "26" "04C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-MID80" "X" "11" "26" "26" "05C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-MID80" "X" "12" "26" "26" "06C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-MID80" "X" "13" "26" "26" "07C-MID80" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "09C-MID80" "X" "22" "28" "22" "08C-MID80" " "
            RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "54" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "17" "51" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NOD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "W-LIN" "0" "50" "A-NO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "W-LIN" "16" "1" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YC" "9" "W-LIN" "19" "2" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YC" BY REFERENCE W-YC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YCN" "N" "W-LIN" "26" "24" "A-YC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-YCN" BY REFERENCE W-YCN1 "24" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KS2" "9" "W-LIN" "19" "2" "A-YCN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KS2" BY REFERENCE W-KS2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KSN2" "X" "W-LIN" "26" "6" "A-KS2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KSN2" BY REFERENCE W-KSN2 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FRC" "9" "W-LIN" "19" "2" "A-KSN2" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FRC" BY REFERENCE W-FRC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FRN" "N" "W-LIN" "26" "12" "A-FRC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FRN" BY REFERENCE W-FRN "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-LIN" "60" "1" "A-FRN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "W-L" "W-C" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD(1) "1" "1" BY REFERENCE CNT 1
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMMD" "9" "22" "45" "1" "A-SCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMMD" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "84" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NON" " " "3" "0" "34" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-NON" "RX" "3" "45" "18" " " "D-NON" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-NON" "N" "3" "46" "16" "01D-NON" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-NON" BY REFERENCE W-NON "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-L" "0" "50" "D-NON" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CL" "X" "W-L" "15" "50" " " "02C-DSP" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "110" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "110" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "15" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "19" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "21" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "20" "E-ME12" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           COPY LIBCPR.
           MOVE DATE-03R TO H-DATE.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE ZERO TO W-D.
           MOVE SPACE TO W-NON.
       M-040.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID0" C-MID0 "p" RETURNING RESU.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-NO "A-NO" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-NOD NOT = 01 AND 04 AND 05
                              AND 70 AND 80 AND 99
               GO TO M-060
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-DMMD "A-DMMD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
           IF  W-NOD = 99
               GO TO M-980
           END-IF
           IF  W-NOD = 80
               GO TO M-720
           END-IF
           IF  W-NOD = 70
               GO TO M-820
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID2" C-MID2 "p" RETURNING RESU.
           PERFORM S-620 THRU S-640.
           CALL "SD_Output" USING "D-NON" D-NON "p" RETURNING RESU.
           MOVE 6 TO W-LIN.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN" "2" W-LIN RETURNING RESU.
       M-120.
           ADD 1 TO W-LIN.
           CALL "SD_Arg_Match_Line" USING
            "W-LIN" "2" W-LIN RETURNING RESU.
           IF  W-LIN = 21
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               CALL "SD_Output" USING "D-NON" D-NON "p" RETURNING RESU
               MOVE 7 TO W-LIN
               CALL "SD_Arg_Match_Line" USING
                "W-LIN" "2" W-LIN RETURNING RESU
           END-IF.
       M-220.
           IF  W-ACT NOT = ZERO
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.           
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT < 1 OR > 3
               GO TO M-220
           END-IF
      *
           MOVE SPACE TO W-R.
           MOVE W-NOD TO W-NO.
           IF  W-NOD = 5
               GO TO M-420
           END-IF
           IF  W-NOD = 4
               GO TO M-380
           END-IF.
      *    -------------  ﾖ ｳ ﾄ  ---------------------
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-YC "A-YC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           PERFORM S-100 THRU S-140.
           IF  CHK = 5
               GO TO M-240
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-YCN
           END-IF
           CALL "SD_Output" USING "A-YCN" A-YCN "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-540
           END-IF.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-YCN "A-YCN" "N" "24"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           MOVE SPACE TO W-YCN2.
           GO TO M-540.
      *    ------------  ｷ ｼ ｭ 2  --------------------
       M-380.
           CALL "SD_Accept" USING BY REFERENCE A-KS2 "A-KS2" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-380
           END-IF
           PERFORM S-100 THRU S-140.
           IF  CHK = 5
               GO TO M-380
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-KSN2
           END-IF
           CALL "SD_Output" USING "A-KSN2" A-KSN2 "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-540
           END-IF.
       M-400.
           CALL "SD_Accept" USING BY REFERENCE A-KSN2 "A-KSN2" "X" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-380
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-400
           END-IF
           GO TO M-540.
      *    ------------  ﾊ ｲ ｷ ｬ ｸ ﾌ ﾘ ｮ ｳ  ----------
       M-420.
           CALL "SD_Accept" USING BY REFERENCE A-FRC "A-FRC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-420
           END-IF
           PERFORM S-100 THRU S-140.
           IF  CHK = 5
               GO TO M-420
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-FRN
           END-IF
           CALL "SD_Output" USING "A-FRN" A-FRN "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO M-540
           END-IF.
       M-440.
           CALL "SD_Accept" USING BY REFERENCE A-FRN "A-FRN" "N" "12"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-420
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-440
           END-IF
           GO TO M-540.
      *    -------------------------------------------
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = BTB
               GO TO M-580
           END-IF
           IF  W-NO = 1
               IF  W-ACT = 3
                   GO TO M-240
               ELSE
                   GO TO M-260
               END-IF
           END-IF
           IF  W-NO = 4
               IF  W-ACT = 3
                   GO TO M-380
               ELSE
                   GO TO M-400
               END-IF
           END-IF
           IF  W-NO = 5
               IF  W-ACT = 3
                   GO TO M-420
               ELSE
                   GO TO M-440
               END-IF
           END-IF.
       M-580.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-220
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-540
           END-IF
           IF  W-ACT = 3
               GO TO M-660
           END-IF.
       M-600.
           INITIALIZE KKB-R.
           MOVE W-R TO KKB-R.
           IF  W-ACT = 2
               GO TO M-640
           END-IF
      *           WRITE KKB-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            KKB-M_PNAME1 KKB-M_LNAME KKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-620
           END-IF
           GO TO M-120.
       M-620.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           MOVE "KKBM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           GO TO M-600.
       M-640.
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
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-120.
       M-660.
      *           DELETE KKB-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING KKB-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-120.
      *-----------------------------------------------------------------
       M-720.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID80" C-MID80 "p" RETURNING RESU.
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
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-040
           END-IF
           MOVE KKB-R TO W-R.
           MOVE ZERO TO CNT.
       M-740.
           ADD 1 TO CNT.
           IF  CNT > 0 AND < 16
               MOVE W-CLC(CNT) TO W-LC
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU
               GO TO M-740
           END-IF
           MOVE ZERO TO CNT.
       M-760.
           ADD 1 TO CNT.
           IF  CNT = 0 OR > 15
               GO TO M-800
           END-IF.
       M-780.
           MOVE W-CLC(CNT) TO W-LC.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT
               IF  CNT = 0
                   GO TO M-040
               ELSE
                   GO TO M-780
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-780
           END-IF
           IF  W-SCD(CNT) > 1
               GO TO M-780
           END-IF
           GO TO M-760.
       M-800.
           CALL "SD_Accept" USING BY REFERENCE A-DMMD "A-DMMD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               SUBTRACT 1 FROM CNT
               GO TO M-780
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-800
           END-IF
           IF  W-DMM = 9
               GO TO M-720
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-800
           END-IF
      *
           INITIALIZE KKB-R.
           MOVE W-R TO KKB-R.
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
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           GO TO M-040.
       M-820.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           PERFORM S-220 THRU S-360.
           CALL "DB_F_Open" USING
            "I-O" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           GO TO M-040.
       M-980.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       S-020.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       S-040.
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
       S-060.
           EXIT.
       S-100.
           MOVE ZERO TO CHK.
           MOVE W-KEY TO KKB-KEY.
      *           READ KKB-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO S-120
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 5 TO CHK
           ELSE
               MOVE KKB-R TO W-R
           END-IF
           GO TO S-140.
       S-120.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 5 TO CHK
           END-IF.
       S-140.
           EXIT.
       S-220.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
       S-240.
      *           READ KKB-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KKB-M_PNAME1 BY REFERENCE KKB-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KKB-M_IDLST KKB-M_PNAME1
               GO TO S-360
           END-IF
           IF  KKB-NO = 90
               GO TO S-240
           END-IF
           MOVE ZERO TO W-LCD.
           MOVE SPACE TO W-P.
       S-260.
           IF  W-LD NOT = ZERO
               PERFORM S-420 THRU S-460
           END-IF
           MOVE ZERO TO W-PC.
           MOVE KKB-NO TO W-NOD.
           PERFORM S-620 THRU S-640.
       S-280.
           PERFORM S-420 THRU S-460.
           IF  W-CD NOT = 0
               GO TO S-300
           END-IF
           IF  W-PC = 0
               MOVE 5 TO W-PC
               MOVE KKB-NO TO P-NO1(W-LD)
               MOVE W-NON TO P-NON1(W-LD)
           END-IF
           MOVE KKB-BC TO P-BC1(W-LD).
           IF  KKB-NO = 1
               MOVE KKB-YCN TO P-MS1(W-LD)
           END-IF
           IF  KKB-NO = 4
               MOVE KKB-KSN2 TO P-MSA1(W-LD)
           END-IF
           IF  KKB-NO = 5
               MOVE KKB-FRN TO P-MS1(W-LD)
           END-IF
           GO TO S-320.
       S-300.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               MOVE KKB-NO TO P-NO2(W-LD)
               MOVE W-NON TO P-NON2(W-LD)
           END-IF
           MOVE KKB-BC TO P-BC2(W-LD).
           IF  KKB-NO = 1
               MOVE KKB-YCN TO P-MS2(W-LD)
           END-IF
           IF  KKB-NO = 4
               MOVE KKB-KSN2 TO P-MSA2(W-LD)
           END-IF
           IF  KKB-NO = 5
               MOVE KKB-FRN TO P-MS2(W-LD)
           END-IF.
       S-320.
      *           READ KKB-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KKB-M_PNAME1 BY REFERENCE KKB-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO S-340
           END-IF
           IF  KKB-NO = 90
               GO TO S-320
           END-IF
           IF  KKB-NO = W-NOD
               GO TO S-280
           END-IF
           GO TO S-260.
       S-340.
           PERFORM S-520 THRU S-560.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       S-360.
           EXIT.
       S-420.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 60
               GO TO S-440
           END-IF
           IF  W-CD = 0
               ADD 1 TO W-CD
               MOVE ZERO TO W-LD W-PC
               GO TO S-420
           END-IF
           PERFORM S-520 THRU S-560.
           MOVE ZERO TO W-LCD W-PC.
           MOVE SPACE TO W-P.
           GO TO S-420.
       S-440.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       S-460.
           EXIT.
       S-520.
           IF  W-POC = 0
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM S-040 THRU S-060
           ELSE
               PERFORM S-020 THRU S-060
           END-IF
           MOVE ZERO TO W-LD.
       S-540.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 60
               IF  P-X(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO S-540
               END-IF
           END-IF.
       S-560.
           EXIT.
       S-620.
           MOVE SPACE TO W-NON.
           IF  W-NOD = 01
               MOVE W-NO01 TO W-NON
           END-IF
           IF  W-NOD = 04
               MOVE W-NO04 TO W-NON
           END-IF
           IF  W-NOD = 05
               MOVE W-NO05 TO W-NON
           END-IF.
       S-640.
           EXIT.
