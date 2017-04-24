       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM910.
      *********************************************************
      *    PROGRAM         :  履物区分マスターメンテナンス    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  ------                          *
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
           02  F              PIC  X(034) VALUE SPACE.
           02  F              PIC  N(021) VALUE
                "＊＊＊　　履物区分マスター　プルーフリスト".
           02  F              PIC  N(005) VALUE "　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
           02  F              PIC  X(001) VALUE SPACE.
       01  HEAD2.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(005) VALUE "区　分　名".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(005) VALUE "名　称　他".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(001) VALUE ":".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(001) VALUE "№".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(005) VALUE "区　分　名".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(005) VALUE "名　称　他".
           02  F              PIC  X(027) VALUE SPACE.
           02  F              PIC  X(001) VALUE SPACE.
       01  W-P.
           02  W-PD    OCCURS  59.
             03  P-NO1        PIC  9(002).
             03  F            PIC  X(001).
             03  P-NON1       PIC  N(008).
             03  F            PIC  X(001).
             03  P-BC1        PIC  X(005).
             03  F            PIC  X(001).
             03  P-MS1        PIC  N(019).
             03  P-FKD1  REDEFINES P-MS1.
               04  P-FKNA1    PIC  N(004).
               04  F          PIC  X(003).
               04  P-SU1      PIC ZZ,ZZ9.
               04  F          PIC  X(005).
               04  P-KTKCD1   PIC  9(001).
               04  F          PIC  X(015).
             03  P-BRD1  REDEFINES P-MS1.
               04  P-BRN1     PIC  N(008).
               04  F          PIC  X(022).
             03  P-KHR1  REDEFINES P-MS1.
               04  P-CKR1     PIC  9.999.
               04  F          PIC  X(003).
               04  P-KKR1     PIC  9.999.
               04  F          PIC  X(025).
             03  F            PIC  X(001).
             03  P-X          PIC  X(001).
             03  F            PIC  X(002).
             03  P-NO2        PIC  9(002).
             03  F            PIC  X(001).
             03  P-NON2       PIC  N(008).
             03  F            PIC  X(001).
             03  P-BC2        PIC  X(005).
             03  F            PIC  X(001).
             03  P-MS2        PIC  N(019).
             03  P-FKD2  REDEFINES P-MS2.
               04  P-FKNA2    PIC  N(004).
               04  F          PIC  X(003).
               04  P-SU2      PIC ZZ,ZZ9.
               04  F          PIC  X(005).
               04  P-KTKCD2   PIC  9(001).
               04  F          PIC  X(015).
             03  P-BRD2  REDEFINES P-MS2.
               04  P-BRN2     PIC  N(008).
               04  F          PIC  X(022).
             03  F            PIC  X(001).
       01  W-R.
           02  W-KEY.                                                       ｺｰﾄﾞ
             03  W-NO         PIC  9(002).                                №
             03  W-BC         PIC  X(005).
             03  W-BC01  REDEFINES W-BC.
               04  W-TDFK     PIC  9(002).                              都道府県
               04  F          PIC  X(003).
             03  W-BC02  REDEFINES W-BC.
               04  W-BM       PIC  9(001).                              部門
               04  F          PIC  X(004).
             03  W-BC04  REDEFINES W-BC.
               04  W-TNC      PIC  9(002).                              担当
               04  F          PIC  X(003).
             03  W-BC08  REDEFINES W-BC.
               04  W-KTKC     PIC  9(001).                              教育地区
               04  F          PIC  X(004).
             03  W-BC11  REDEFINES W-BC.
               04  W-BR1      PIC  9(002).                              分類１
               04  F          PIC  X(003).
             03  W-BC13  REDEFINES W-BC.
               04  W-BR22     PIC  9(001).                              分類２
               04  F          PIC  X(004).
             03  W-BC14  REDEFINES W-BC.
               04  W-BR3      PIC  9(002).
               04  F          PIC  X(003).
             03  W-BC16  REDEFINES W-BC.
               04  W-BMC      PIC  9(002).
               04  F          PIC  X(003).
             03  W-BC31  REDEFINES W-BC.
               04  W-NKC1     PIC  9(001).                              入金
               04  F          PIC  X(004).
             03  W-BC32  REDEFINES W-BC.
               04  W-NSC      PIC  9(001).
               04  F          PIC  X(004).
             03  W-BC41  REDEFINES W-BC.
               04  W-SUC      PIC  9(001).                              仕上受入
               04  F          PIC  X(004).
             03  W-BC42  REDEFINES W-BC.
               04  W-SSC      PIC  9(001).                              生産
               04  F          PIC  X(004).
      *    * * *   N A M E  ｺ ｳ ﾓ ｸ   * * *
           02  W-NAME         PIC  X(057).
           02  W-NA01  REDEFINES W-NAME.
             03  W-FKNA       PIC  N(004).                              府県名
             03  W-SU         PIC  9(005).                              人口
             03  W-KIN        PIC S9(010).                              売上金額
             03  W-KTKCD      PIC  9(001).
             03  F            PIC  X(033).
           02  W-NA02  REDEFINES W-NAME.
             03  W-BMNA       PIC  N(006).                              部門名
             03  F            PIC  X(045).
           02  W-NA04  REDEFINES W-NAME.
             03  W-TNNA       PIC  N(014).                              担当名
             03  F            PIC  X(029).
           02  W-NA08  REDEFINES W-NAME.
             03  W-KTNA       PIC  N(003).                              教育地区
             03  F            PIC  X(051).
           02  W-NA11  REDEFINES W-NAME.
             03  W-BRN1       PIC  N(008).                              分類名１
             03  F            PIC  X(041).
           02  W-NA13  REDEFINES W-NAME.
             03  W-BRN22      PIC  N(003).                              分類名２
             03  F            PIC  X(051).
           02  W-NA14  REDEFINES W-NAME.
             03  W-BRN3       PIC  N(003).
             03  F            PIC  X(051).
           02  W-NA16  REDEFINES W-NAME.
             03  W-BMN        PIC  N(003).
             03  F            PIC  X(051).
           02  W-NA31  REDEFINES W-NAME.
             03  W-NKNA       PIC  N(006).                              入金名
             03  F            PIC  X(045).
           02  W-NA32  REDEFINES W-NAME.
             03  W-NSNA       PIC  N(006).
             03  F            PIC  X(045).
           02  W-NA41  REDEFINES W-NAME.
             03  W-SUNA       PIC  N(005).                              仕上受入
             03  F            PIC  X(047).
           02  W-NA42  REDEFINES W-NAME.
             03  W-SSNA       PIC  N(004).                              生産
             03  F            PIC  X(049).
           02  W-NA90  REDEFINES W-NAME.
             03  W-ASCD.
               04  W-SCD   OCCURS  15.
                 05  W-SC     PIC  9(001).
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
             03  W-BS         PIC  9(001).
             03  W-INV        PIC  9(001).
           02  W-ANO.
             03  W-NO01       PIC  N(008) VALUE "都　道　府　県　".
             03  W-NO02       PIC  N(008) VALUE "部　　　　門　　".
             03  W-NO04       PIC  N(008) VALUE "担　　　　当　　".
             03  W-NO08       PIC  N(008) VALUE "教　育　地　区　".
             03  W-NO11       PIC  N(008) VALUE "分　類　①　　　".
             03  W-NO13       PIC  N(008) VALUE "分　類　②　　　".
             03  W-NO14       PIC  N(008) VALUE "分　類　③　　　".
             03  W-NO16       PIC  N(008) VALUE "部　門　管　理　".
             03  W-NO31       PIC  N(008) VALUE "入　　　　金　　".
             03  W-NO32       PIC  N(008) VALUE "入金その他相殺　".
             03  W-NO41       PIC  N(008) VALUE "仕上・受入入力　".
             03  W-NO42       PIC  N(008) VALUE "生　　　　産　　".
             03  W-NO70       PIC  N(008) VALUE "作　　　　表　　".
             03  W-NO80       PIC  N(008) VALUE "システム使用区分".
             03  W-NO99       PIC  N(008) VALUE "終　　　　了　　".
           02  W-MID          PIC  X(040).
           02  W-MID1  REDEFINES W-MID.
             03  W-MID11      PIC  N(005).
             03  F            PIC  X(001).
             03  W-MID12      PIC  N(010).
             03  F            PIC  X(009).
           02  W-MID2  REDEFINES W-MID.
             03  W-MID21      PIC  N(015).
             03  F            PIC  X(010).
           02  W-MID3  REDEFINES W-MID.
             03  W-MID31      PIC  N(008).
             03  F            PIC  X(001).
             03  W-MID32      PIC  N(008).
             03  F            PIC  X(001).
             03  W-MID33      PIC  N(003).
           02  W-ALC.
             03  F            PIC  X(032) VALUE
                  "06340734083409341034113412341334".
             03  F            PIC  X(028) VALUE
                  "0654075408540954105411541254".
           02  W-ALCD  REDEFINES W-ALC.
             03  W-CLC        PIC  9(004) OCCURS  15.
           02  W-LC.
             03  W-L          PIC  9(002).
             03  W-C          PIC  9(002).
           02  CNT            PIC  9(002).
           02  W-LCD.
             03  W-LD         PIC  9(002).
             03  W-CD         PIC  9(001).
           02  W-POC          PIC  9(001).
           02  W-FILE         PIC  X(013).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIHKBM.
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
                "＊＊＊　　履物区分マスター　メンテナンス　　＊＊＊".
       01  C-MID0.
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 01".
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 70".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 02".
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 80".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 99".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 04".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 08".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 11".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 13".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 14".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 16".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 31".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 32".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 41".
           02  FILLER.
             03  FILLER    PIC  N(008).
             03  FILLER    PIC  X(005) VALUE " = 42".
           02  FILLER    PIC  N(001) VALUE "№".
           02  FILLER    PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER    PIC  X(033) VALUE
                "ACT : 登録=1 修正=2 削除=3 終了=9".
           02  FILLER.
             03  FILLER    PIC  X(018) VALUE "ACT ｺｰﾄﾞ".
             03  04C-MID1  PIC  X(040).
             03  FILLER    PIC  N(002) VALUE "確認".
           02  FILLER    PIC  X(016) VALUE
                "確認 : OK=1 NO=9".
       01  C-MID80.
           02  FILLER    PIC  X(036) VALUE
                "売上値引入力        － － － －     ".
           02  FILLER    PIC  X(036) VALUE
                "売上値引伝票        日 計 更 新     ".
           02  FILLER    PIC  X(036) VALUE
                "仕上受入入力        － － － －     ".
           02  FILLER    PIC  X(036) VALUE
                "仕上受入変換        請   求   書    ".
           02  FILLER    PIC  X(036) VALUE
                "入 金 変 換         － － － －     ".
           02  FILLER    PIC  X(036) VALUE
                "入 金 入 力         月末一括振替    ".
           02  FILLER    PIC  X(036) VALUE
                "入 金 伝 票         月 次 更 新     ".
           02  FILLER    PIC  X(036) VALUE
                "－ － － －     ".
           02  FILLER    PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-NO    PIC  9(002).
           02  A-SC    PIC  9(001).
           02  A-DMMD  PIC  9(001).
           02  FILLER.
             03  A-ACT   PIC  9(001).
             03  A-TDFK  PIC  9(002).
             03  A-FKNA  PIC  N(004).
             03  A-SU    PIC  9(005).
             03  A-KTKCD PIC  9(001).
             03  A-BM    PIC  9(001).
             03  A-BMNA  PIC  N(006).
             03  A-TNC   PIC  9(002).
             03  A-TNNA  PIC  N(014).
             03  A-KTKC  PIC  9(001).
             03  A-KTNA  PIC  N(003).
             03  A-BR1   PIC  9(002).
             03  A-BRN1  PIC  N(008).
             03  A-BR22  PIC  9(001).
             03  A-BRN22 PIC  N(003).
             03  A-BR3   PIC  9(002).
             03  A-BRN3  PIC  N(003).
             03  A-BMC   PIC  9(002).
             03  A-BMN   PIC  N(003).
             03  A-NKC1  PIC  9(001).
             03  A-NKNA  PIC  N(006).
             03  A-NSC   PIC  9(001).
             03  A-NSNA  PIC  N(006).
             03  A-SUC   PIC  9(001).
             03  A-SUNA  PIC  N(005).
             03  A-SSC   PIC  9(001).
             03  A-SSNA  PIC  N(004).
             03  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-CL.
               04  FILLER    PIC  X(040) VALUE
                    "                                        ".
               04  FILLER    PIC  X(040) VALUE
                    "                                        ".
           02  FILLER.
             03  D-SU    PIC ZZZZ9 .
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
            "C-MID0" " " "0" "0" "339" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID0" " " "4" "0" "42" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING
            "0101C-MID0" "N" "4" "15" "16" " " "01C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0101C-MID0" BY REFERENCE W-NO01 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0201C-MID0" "X" "4" "31" "5" "0101C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0301C-MID0" "N" "4" "42" "16" "0201C-MID0" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0301C-MID0" BY REFERENCE W-NO70 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0401C-MID0" "X" "4" "58" "5" "0301C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID0" " " "5" "0" "42" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0102C-MID0" "N" "5" "15" "16" " " "02C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0102C-MID0" BY REFERENCE W-NO02 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0202C-MID0" "X" "5" "31" "5" "0102C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0302C-MID0" "N" "5" "42" "16" "0202C-MID0" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "0302C-MID0" BY REFERENCE W-NO80 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0402C-MID0" "X" "5" "58" "5" "0302C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID0" " " "6" "0" "21" "02C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0103C-MID0" "N" "6" "42" "16" " " "03C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0103C-MID0" BY REFERENCE W-NO99 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0203C-MID0" "X" "6" "58" "5" "0103C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID0" " " "7" "0" "21" "03C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0104C-MID0" "N" "7" "15" "16" " " "04C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0104C-MID0" BY REFERENCE W-NO04 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0204C-MID0" "X" "7" "31" "5" "0104C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID0" " " "8" "0" "21" "04C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0105C-MID0" "N" "8" "15" "16" " " "05C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0105C-MID0" BY REFERENCE W-NO08 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0205C-MID0" "X" "8" "31" "5" "0105C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID0" " " "9" "0" "21" "05C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0106C-MID0" "N" "9" "15" "16" " " "06C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0106C-MID0" BY REFERENCE W-NO11 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0206C-MID0" "X" "9" "31" "5" "0106C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID0" " " "11" "0" "21" "06C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "0107C-MID0" "N" "11" "15" "16" " " "07C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0107C-MID0" BY REFERENCE W-NO13 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0207C-MID0" "X" "11" "31" "5" "0107C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID0" " " "12" "0" "21" "07C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0108C-MID0" "N" "12" "15" "16" " " "08C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0108C-MID0" BY REFERENCE W-NO14 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0208C-MID0" "X" "12" "31" "5" "0108C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID0" " " "13" "0" "21" "08C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0109C-MID0" "N" "13" "15" "16" " " "09C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0109C-MID0" BY REFERENCE W-NO16 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0209C-MID0" "X" "13" "31" "5" "0109C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "10C-MID0" " " "15" "0" "21" "09C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0110C-MID0" "N" "15" "15" "16" " " "10C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0110C-MID0" BY REFERENCE W-NO31 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0210C-MID0" "X" "15" "31" "5" "0110C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "11C-MID0" " " "16" "0" "21" "10C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0111C-MID0" "N" "16" "15" "16" " " "11C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0111C-MID0" BY REFERENCE W-NO32 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0211C-MID0" "X" "16" "31" "5" "0111C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "12C-MID0" " " "17" "0" "21" "11C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0112C-MID0" "N" "17" "15" "16" " " "12C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0112C-MID0" BY REFERENCE W-NO41 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0212C-MID0" "X" "17" "31" "5" "0112C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "13C-MID0" " " "18" "0" "21" "12C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "0113C-MID0" "N" "18" "15" "16" " " "13C-MID0"
            RETURNING RESU.
       CALL "SD_From" USING
            "0113C-MID0" BY REFERENCE W-NO42 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "0213C-MID0" "X" "18" "31" "5" "0113C-MID0" " "
            RETURNING RESU.
       CALL "SD_Init" USING
            "14C-MID0" "N" "19" "58" "2" "13C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "15C-MID0" "X" "22" "28" "22" "14C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
           "C-MID1" " " "0" "0" "111" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-MID1" "X" "4" "14" "33" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-MID1" " " "6" "0" "62" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "03C-MID1" "X" "6" "11" "18" " " "02C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
           "04C-MID1" "X" "6" "21" "40" "03C-MID1" " " RETURNING RESU.
       CALL "SD_From" USING
           "04C-MID1" BY REFERENCE W-MID "40" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "05C-MID1" "N" "6" "62" "4" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "06C-MID1" "X" "22" "49" "16" "02C-MID1" " " RETURNING RESU.
      *C-MID80
       CALL "SD_Init" USING
           "C-MID80" " " "0" "0" "310" " " " " RETURNING RESU.
       CALL "SD_Init" USING
         "01C-MID80" "X" "6" "19" "36" " " "C-MID80" RETURNING RESU.
       CALL "SD_Init" USING
         "02C-MID80" "X" "7" "19" "36" "01C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "03C-MID80" "X" "8" "19" "36" "02C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "04C-MID80" "X" "9" "19" "36" "03C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "05C-MID80" "X" "10" "19" "36" "04C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "06C-MID80" "X" "11" "19" "36" "05C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "07C-MID80" "X" "12" "19" "36" "06C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "08C-MID80" "X" "13" "19" "36" "07C-MID80" " " RETURNING RESU.
       CALL "SD_Init" USING
         "09C-MID80" "X" "22" "28" "22" "08C-MID80" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "159" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NO" "9" "19" "61" "2" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NO" BY REFERENCE W-NOD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SC" "9" "W-L" "W-C" "1" "A-NO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SC" BY REFERENCE W-SC(1) "1" "1" BY REFERENCE CNT 1
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMMD" "9" "22" "45" "1" "A-SC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMMD" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "W-LIN" "0" "155" "A-DMMD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "W-LIN" "12" "1" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TDFK" "9" "W-LIN" "16" "2" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TDFK" BY REFERENCE W-TDFK "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FKNA" "N" "W-LIN" "21" "8" "A-TDFK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FKNA" BY REFERENCE W-FKNA "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "9" "W-LIN" "31" "5" "A-FKNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KTKCD" "9" "W-LIN" "42" "1" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KTKCD" BY REFERENCE W-KTKCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BM" "9" "W-LIN" "17" "1" "A-KTKCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BM" BY REFERENCE W-BM "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BMNA" "N" "W-LIN" "21" "12" "A-BM" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BMNA" BY REFERENCE W-BMNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNC" "9" "W-LIN" "16" "2" "A-BMNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNC" BY REFERENCE W-TNC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNNA" "N" "W-LIN" "21" "28" "A-TNC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNNA" BY REFERENCE W-TNNA "28" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KTKC" "9" "W-LIN" "17" "1" "A-TNNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KTKC" BY REFERENCE W-KTKC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KTNA" "N" "W-LIN" "21" "6" "A-KTKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KTNA" BY REFERENCE W-KTNA "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BR1" "9" "W-LIN" "16" "2" "A-KTNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BR1" BY REFERENCE W-BR1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BRN1" "N" "W-LIN" "21" "16" "A-BR1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BRN1" BY REFERENCE W-BRN1 "16" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BR22" "9" "W-LIN" "17" "1" "A-BRN1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BR22" BY REFERENCE W-BR22 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BRN22" "N" "W-LIN" "21" "6" "A-BR22" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BRN22" BY REFERENCE W-BRN22 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BR3" "9" "W-LIN" "16" "2" "A-BRN22" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BR3" BY REFERENCE W-BR3 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BRN3" "N" "W-LIN" "21" "6" "A-BR3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BRN3" BY REFERENCE W-BRN3 "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BMC" "9" "W-LIN" "16" "2" "A-BRN3" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BMC" BY REFERENCE W-BMC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BMN" "N" "W-LIN" "21" "6" "A-BMC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BMN" BY REFERENCE W-BMN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NKC1" "9" "W-LIN" "17" "1" "A-BMN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NKC1" BY REFERENCE W-NKC1 "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NKNA" "N" "W-LIN" "21" "12" "A-NKC1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NKNA" BY REFERENCE W-NKNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NSC" "9" "W-LIN" "17" "1" "A-NKNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NSC" BY REFERENCE W-NSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NSNA" "N" "W-LIN" "21" "12" "A-NSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NSNA" BY REFERENCE W-NSNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SUC" "9" "W-LIN" "17" "1" "A-NSNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SUC" BY REFERENCE W-SUC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SUNA" "N" "W-LIN" "21" "10" "A-SUC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SUNA" BY REFERENCE W-SUNA "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSC" "9" "W-LIN" "17" "1" "A-SUNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSC" BY REFERENCE W-SSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSNA" "N" "W-LIN" "21" "8" "A-SSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSNA" BY REFERENCE W-SSNA "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "W-LIN" "64" "1" "A-SSNA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "85" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "W-LIN" "0" "80" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CL" " " "W-LIN" "0" "80" " " "01C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CL" "X" "W-LIN" "1" "40" " " "D-CL" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CL" "X" "W-LIN" "41" "40" "01D-CL" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "W-LIN" "0" "5" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
           "D-SU" "ZZZZ9" "W-LIN" "31" "5" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU "5" "0" RETURNING RESU.
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
           MOVE DATE-02R TO H-DATE.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
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
           IF  W-NOD NOT = 01 AND 02 AND 04 AND 08 AND 11
                             AND 13 AND 14 AND 16
                             AND 31 AND 32 AND 41 AND 42
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
           IF  W-DMM = 9
               GO TO M-060
           END-IF
           IF  W-DMM NOT = 1
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
           MOVE SPACE TO W-MID.
           IF  W-NOD = 01
               MOVE "都道府県名" TO W-MID11
               MOVE "人口　教育地区　　　" TO W-MID12
           END-IF
           IF  W-NOD = 02
               MOVE "部門名　　　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 04
               MOVE "担　当　者　名　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 08
               MOVE "教育地区名　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 11
               MOVE "分類名①　　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 13
               MOVE "分類名②　　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 14
               MOVE "分類名③　　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 16
               MOVE "部門管理名　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 31
               MOVE "入金名　　　　　　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 32
               MOVE "入金その他相殺名　　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 41
               MOVE "仕上受入入力区分名　　　　　　" TO W-MID21
           END-IF
           IF  W-NOD = 42
               MOVE "生産区分名　　　　　　　　　　" TO W-MID21
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU.
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
               CALL "SD_Output" USING
                "C-MID" C-MID "p" RETURNING RESU
               CALL "SD_Output" USING
                "C-MID1" C-MID1 "p" RETURNING RESU
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
           MOVE 0 TO W-BS.
       M-240.
           IF  W-NOD = 01
               PERFORM TDFK-RTN THRU TDFK-EX
           END-IF
           IF  W-NOD = 02
               PERFORM BM-RTN THRU BM-EX
           END-IF
           IF  W-NOD = 04
               PERFORM TNC-RTN THRU TNC-EX
           END-IF
           IF  W-NOD = 08
               PERFORM KTKC-RTN THRU KTKC-EX
           END-IF
           IF  W-NOD = 11
               PERFORM BR1-RTN THRU BR1-EX
           END-IF
           IF  W-NOD = 13
               PERFORM BR22-RTN THRU BR22-EX
           END-IF
           IF  W-NOD = 14
               PERFORM BR3-RTN THRU BR3-EX
           END-IF
           IF  W-NOD = 16
               PERFORM BMC-RTN THRU BMC-EX
           END-IF
           IF  W-NOD = 31
               PERFORM NKC1-RTN THRU NKC1-EX
           END-IF
           IF  W-NOD = 32
               PERFORM NSC-RTN THRU NSC-EX
           END-IF
           IF  W-NOD = 41
               PERFORM SUC-RTN THRU SUC-EX
           END-IF
           IF  W-NOD = 42
               PERFORM SSC-RTN THRU SSC-EX
           END-IF
           IF  W-BS = 9
               GO TO M-220
           END-IF.
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-240
               ELSE
                   MOVE 5 TO W-BS
                   GO TO M-240
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-240
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-540
           END-IF
      *
           PERFORM WRD-RTN THRU WRD-EX.
           IF  W-INV = 9
               GO TO M-980
           END-IF
           GO TO M-120.
       M-720.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU.
           CALL "SD_Output" USING "C-MID80" C-MID80 "p" RETURNING RESU.
           MOVE SPACE TO HKB-KEY.
           MOVE 90 TO HKB-NO.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
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
           MOVE HKB-R TO W-R.
           MOVE ZERO TO CNT.
       M-740.
           ADD 1 TO CNT.
           IF  CNT > 0 AND < 16
               MOVE W-CLC(CNT) TO W-LC
               CALL "SD_Arg_Match_Line" USING
                "W-L" "2" W-L RETURNING RESU
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               CALL "SD_Output" USING "A-SC" A-SC "p" RETURNING RESU
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
           CALL "SD_Accept" USING BY REFERENCE A-SC "A-SC" "9" "1"
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
           IF  W-SC(CNT) > 1
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
           MOVE 2 TO W-ACT.
           PERFORM WRD-RTN THRU WRD-EX.
           IF  W-INV = 9
               GO TO M-980
           END-IF
           GO TO M-040.
       M-820.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           PERFORM PRI-RTN THRU PRI-EX.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           GO TO M-040.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *-------------　都道府県　入力  ----------------------------------
       TDFK-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO TDFK-060
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-TDFK "A-TDFK" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO TDFK-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TDFK-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO TDFK-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-FKNA
               MOVE ZERO TO W-SU W-KTKCD
           END-IF
           CALL "SD_Output" USING "A-FKNA" A-FKNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KTKCD" A-KTKCD "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO TDFK-EX
           END-IF.
       TDFK-020.
           CALL "SD_Accept" USING BY REFERENCE A-FKNA "A-FKNA" "N" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TDFK-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TDFK-020
           END-IF.
       TDFK-040.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TDFK-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TDFK-040
           END-IF.
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
       TDFK-060.
           CALL "SD_Accept" USING BY REFERENCE A-KTKCD "A-KTKCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TDFK-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TDFK-060
           END-IF.
       TDFK-EX.
           EXIT.
      *-------------　部門　入力  --------------------------------------
       BM-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO BM-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-BM "A-BM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO BM-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BM-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO BM-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-BMNA
           END-IF
           CALL "SD_Output" USING "A-BMNA" A-BMNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO BM-EX
           END-IF.
       BM-020.
           CALL "SD_Accept" USING BY REFERENCE A-BMNA "A-BMNA" "N" "12"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO BM-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BM-020
           END-IF.
       BM-EX.
           EXIT.
      *-------------　担当　入力  --------------------------------------
       TNC-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO TNC-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-TNC "A-TNC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO TNC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TNC-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO TNC-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-TNNA
           END-IF
           CALL "SD_Output" USING "A-TNNA" A-TNNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO TNC-EX
           END-IF.
       TNC-020.
           CALL "SD_Accept" USING BY REFERENCE A-TNNA "A-TNNA" "N" "28"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO TNC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO TNC-020
           END-IF.
       TNC-EX.
           EXIT.
      *-------------　教育地区　入力  ----------------------------------
       KTKC-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO KTKC-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-KTKC "A-KTKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO KTKC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KTKC-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO KTKC-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-KTNA
           END-IF
           CALL "SD_Output" USING "A-KTNA" A-KTNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO KTKC-EX
           END-IF.
       KTKC-020.
           CALL "SD_Accept" USING BY REFERENCE A-KTNA "A-KTNA" "N" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO KTKC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO KTKC-020
           END-IF.
       KTKC-EX.
           EXIT.
      *-------------　分類①　入力  ------------------------------------
       BR1-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO BR1-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-BR1 "A-BR1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO BR1-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BR1-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO BR1-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-BRN1
           END-IF
           CALL "SD_Output" USING "A-BRN1" A-BRN1 "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO BR1-EX
           END-IF.
       BR1-020.
           CALL "SD_Accept" USING BY REFERENCE A-BRN1 "A-BRN1" "N" "16"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO BR1-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BR1-020
           END-IF.
       BR1-EX.
           EXIT.
      *-------------　分類②　入力  ------------------------------------
       BR22-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO BR22-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-BR22 "A-BR22" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO BR22-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BR22-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO BR22-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-BRN22
           END-IF
           CALL "SD_Output" USING "A-BRN22" A-BRN22 "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO BR22-EX
           END-IF.
       BR22-020.
           CALL "SD_Accept" USING BY REFERENCE A-BRN22 "A-BRN22" "N" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO BR22-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BR22-020
           END-IF.
       BR22-EX.
           EXIT.
      *-------------　分類③　入力  ------------------------------------
       BR3-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO BR3-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-BR3 "A-BR3" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO BR3-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BR3-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO BR3-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-BRN3
           END-IF
           CALL "SD_Output" USING "A-BRN3" A-BRN3 "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO BR3-EX
           END-IF.
       BR3-020.
           CALL "SD_Accept" USING BY REFERENCE A-BRN3 "A-BRN3" "N" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO BR3-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BR3-020
           END-IF.
       BR3-EX.
           EXIT.
      *-------------　部門管理入力  ------------------------------------
       BMC-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO BMC-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-BMC "A-BMC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO BMC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BMC-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO BMC-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-BMN
           END-IF
           CALL "SD_Output" USING "A-BMN" A-BMN "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO BMC-EX
           END-IF.
       BMC-020.
           CALL "SD_Accept" USING BY REFERENCE A-BMN "A-BMN" "N" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO BMC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO BMC-020
           END-IF.
       BMC-EX.
           EXIT.
      *-------------　入金　入力  --------------------------------------
       NKC1-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO NKC1-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-NKC1 "A-NKC1" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO NKC1-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NKC1-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO NKC1-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-NKNA
           END-IF
           CALL "SD_Output" USING "A-NKNA" A-NKNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO NKC1-EX
           END-IF.
       NKC1-020.
           CALL "SD_Accept" USING BY REFERENCE A-NKNA "A-NKNA" "N" "12"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NKC1-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NKC1-020
           END-IF.
       NKC1-EX.
           EXIT.
      *-------------　入金その他相殺名　入力  --------------------------
       NSC-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO NSC-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-NSC "A-NSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO NSC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NSC-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO NSC-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-NSNA
           END-IF
           CALL "SD_Output" USING "A-NSNA" A-NSNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO NSC-EX
           END-IF.
       NSC-020.
           CALL "SD_Accept" USING BY REFERENCE A-NSNA "A-NSNA" "N" "12"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO NSC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO NSC-020
           END-IF.
       NSC-EX.
           EXIT.
      *-------------　仕上受入入力　入力  ------------------------------
       SUC-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO SUC-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SUC "A-SUC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO SUC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SUC-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO SUC-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-SUNA
           END-IF
           CALL "SD_Output" USING "A-SUNA" A-SUNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO SUC-EX
           END-IF.
       SUC-020.
           CALL "SD_Accept" USING BY REFERENCE A-SUNA "A-SUNA" "N" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SUC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SUC-020
           END-IF.
       SUC-EX.
           EXIT.
      *-------------　生産区分　入力  ----------------------------------
       SSC-RTN.
           IF  W-BS = 5
               MOVE 0 TO W-BS
               GO TO SSC-020
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-SSC "A-SSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO SSC-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SSC-RTN
           END-IF
           PERFORM RED-RTN THRU RED-EX.
           IF  CHK = 5
               GO TO SSC-RTN
           END-IF
           IF  W-ACT = 1
               MOVE SPACE TO W-SSNA
           END-IF
           CALL "SD_Output" USING "A-SSNA" A-SSNA "p" RETURNING RESU.
           IF  W-ACT = 3
               GO TO SSC-EX
           END-IF.
       SSC-020.
           CALL "SD_Accept" USING BY REFERENCE A-SSNA "A-SSNA" "N" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO SSC-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO SSC-020
           END-IF.
       SSC-EX.
           EXIT.
      *-------------　ＲＥＡＤ  ----------------------------------------
       RED-RTN.
           MOVE ZERO TO CHK.
           MOVE W-KEY TO HKB-KEY.
      *           READ HKBM INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO RED-020
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 5 TO CHK
           ELSE
               MOVE HKB-R TO W-R
           END-IF
           GO TO RED-EX.
       RED-020.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 5 TO CHK
           END-IF.
       RED-EX.
           EXIT.
      *-------------　ＷＲＩＴＥ・ＲＥＷＲＩＴＥ・ＤＥＬＥＴＥ　--------
       WRD-RTN.
           IF  W-ACT = 3
               GO TO WRD-220
           END-IF.
       WRD-020.
           INITIALIZE HKB-R.
           MOVE W-R TO HKB-R.
           IF  W-ACT = 2
               GO TO WRD-120
           END-IF
      *           WRITE HKB-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-040
           END-IF
           GO TO WRD-EX.
       WRD-040.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-INV
               GO TO WRD-EX
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           MOVE "KKBM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           GO TO WRD-020.
       WRD-120.
      *           REWRITE HKB-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HKBM_PNAME1 HKBM_LNAME HKB-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-INV
               GO TO WRD-EX
           END-IF
           GO TO WRD-EX.
       WRD-220.
      *           DELETE HKBM INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HKBM_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-INV
               GO TO WRD-EX
           END-IF
           GO TO WRD-EX.
       WRD-EX.
           EXIT.
      *-------------　作表　ＭＯＶＥ  ----------------------------------
       PRI-RTN.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       PRI-020.
      *           READ HKBM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE HKBM_IDLST HKBM_PNAME1
               GO TO PRI-EX
           END-IF
           IF  HKB-NO = 03 OR 05 OR 90
               GO TO PRI-020
           END-IF
           MOVE ZERO TO W-LCD.
           MOVE SPACE TO W-P.
       PRI-040.
           IF  W-LD NOT = ZERO
               PERFORM PST-RTN THRU PST-EX
           END-IF
           MOVE ZERO TO W-PC.
           MOVE HKB-NO TO W-NOD.
           IF  W-NOD = 01
               MOVE W-NO01 TO W-NON
           END-IF
           IF  W-NOD = 02
               MOVE W-NO02 TO W-NON
           END-IF
           IF  W-NOD = 04
               MOVE W-NO04 TO W-NON
           END-IF
           IF  W-NOD = 08
               MOVE W-NO08 TO W-NON
           END-IF
           IF  W-NOD = 11
               MOVE W-NO11 TO W-NON
           END-IF
           IF  W-NOD = 13
               MOVE W-NO13 TO W-NON
           END-IF
           IF  W-NOD = 14
               MOVE W-NO14 TO W-NON
           END-IF
           IF  W-NOD = 16
               MOVE W-NO16 TO W-NON
           END-IF
           IF  W-NOD = 31
               MOVE W-NO31 TO W-NON
           END-IF
           IF  W-NOD = 32
               MOVE W-NO32 TO W-NON
           END-IF
           IF  W-NOD = 41
               MOVE W-NO41 TO W-NON
           END-IF
           IF  W-NOD = 42
               MOVE W-NO42 TO W-NON
           END-IF.
       PRI-060.
           PERFORM PST-RTN THRU PST-EX.
           IF  W-CD NOT = 0
               GO TO PRI-080
           END-IF
           IF  W-PC = 0
               MOVE 5 TO W-PC
               MOVE HKB-NO TO P-NO1(W-LD)
               MOVE W-NON TO P-NON1(W-LD)
           END-IF
           MOVE HKB-BC TO P-BC1(W-LD).
           IF  HKB-NO = 01
               MOVE HKB-FKNA TO P-FKNA1(W-LD)
               MOVE HKB-SU TO P-SU1(W-LD)
               MOVE HKB-KTKCD TO P-KTKCD1(W-LD)
           END-IF
           IF  HKB-NO = 02
               MOVE HKB-BMNA TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 04
               MOVE HKB-TNNA TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 08
               MOVE HKB-KTNA TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 11
               MOVE HKB-BRN1 TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 13
               MOVE HKB-BRN22 TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 14
               MOVE HKB-BRN3 TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 16
               MOVE HKB-BMN TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 31
               MOVE HKB-NKNA TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 32
               MOVE HKB-NSNA TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 41
               MOVE HKB-SUNA TO P-MS1(W-LD)
           END-IF
           IF  HKB-NO = 42
               MOVE HKB-SSNA TO P-MS1(W-LD)
           END-IF
           GO TO PRI-100.
       PRI-080.
           IF  W-PC = 0
               MOVE 5 TO W-PC
               MOVE HKB-NO TO P-NO2(W-LD)
               MOVE W-NON TO P-NON2(W-LD)
           END-IF
           MOVE HKB-BC TO P-BC2(W-LD).
           IF  HKB-NO = 01
               MOVE HKB-FKNA TO P-FKNA2(W-LD)
               MOVE HKB-SU TO P-SU2(W-LD)
               MOVE HKB-KTKCD TO P-KTKCD2(W-LD)
           END-IF
           IF  HKB-NO = 02
               MOVE HKB-BMNA TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 04
               MOVE HKB-TNNA TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 08
               MOVE HKB-KTNA TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 11
               MOVE HKB-BRN1 TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 13
               MOVE HKB-BRN22 TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 14
               MOVE HKB-BRN3 TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 16
               MOVE HKB-BMN TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 31
               MOVE HKB-NKNA TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 32
               MOVE HKB-NSNA TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 41
               MOVE HKB-SUNA TO P-MS2(W-LD)
           END-IF
           IF  HKB-NO = 42
               MOVE HKB-SSNA TO P-MS2(W-LD)
           END-IF.
       PRI-100.
      *           READ HKBM NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO PRI-120
           END-IF
           IF  HKB-NO = 03 OR 05 OR 90
               GO TO PRI-100
           END-IF
           IF  HKB-NO = W-NOD
               GO TO PRI-060
           END-IF
           GO TO PRI-040.
       PRI-120.
           PERFORM WRI-RTN THRU WRI-EX.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "PR_Close" RETURNING RESP.
       PRI-EX.
           EXIT.
      *-------------　作表　見出し  ------------------------------------
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
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
       MID-EX.
           EXIT.
      *-------------　作表　改頁・改行  --------------------------------
       PST-RTN.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 60
               GO TO PST-020
           END-IF
           IF  W-CD = 0
               ADD 1 TO W-CD
               MOVE ZERO TO W-LD W-PC
               GO TO PST-RTN
           END-IF
           PERFORM WRI-RTN THRU WRI-EX.
           MOVE ZERO TO W-LCD W-PC.
           MOVE SPACE TO W-P.
           GO TO PST-RTN.
       PST-020.
           IF  W-CD = 0
               MOVE ":" TO P-X(W-LD)
           END-IF.
       PST-EX.
           EXIT.
      *-------------　作表　ＷＲＩＴＥ  --------------------------------
       WRI-RTN.
           IF  W-POC = 0
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           ELSE
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE ZERO TO W-LD.
       WRI-020.
           ADD 1 TO W-LD.
           IF  W-LD NOT = 60
               IF  P-X(W-LD) NOT = SPACE
                   MOVE SPACE TO SP-R
                   MOVE W-PD(W-LD) TO SP-R
                   CALL "PR_Write" USING SP-R RETURNING RESP
                   MOVE SPACE TO SP-R
                   GO TO WRI-020
               END-IF
           END-IF.
       WRI-EX.
           EXIT.
