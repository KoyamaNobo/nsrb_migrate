       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMM210.
      *********************************************************
      *    PROGRAM         :  直送先マスターメンテナンス      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHM21                          *
      *        変更　　　  :  62/03/27                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  W-FILE             PIC  X(013).
       01  ERR-STAT           PIC  X(002).
       01  W-20K              PIC  X(005) VALUE X"1A24212474".
       01  W-15K              PIC  X(005) VALUE X"1A24212078".
       01  HEAD1.
           02  P-20           PIC  X(005).
           02  F              PIC  X(038) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　直送先マスター　プルーフリスト　　＊＊＊".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZZ9.
       01  HEAD2.
           02  P-15A          PIC  X(005).
           02  F              PIC  X(009) VALUE "  ｺｰﾄﾞ   ".
           02  F              PIC  N(010) VALUE
                "直　　送　　先　　名".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(008) VALUE "住　　　　　所　".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(004) VALUE "郵便番号".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "電話番号".
           02  F              PIC  X(008) VALUE SPACE.
           02  F              PIC  N(002) VALUE "府県".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "運送".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "出指".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "売上".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "備考".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "作表".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終日付".
           02  F              PIC  X(001) VALUE SPACE.
           02  P-20A          PIC  X(005).
       01  W-P.
           02  P-15B          PIC  X(005).
           02  P-TCD          PIC  9(004).
           02  P-V            PIC  X(001).
           02  P-CCD          PIC  9(003).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(026).
           02  F              PIC  X(001).
           02  P-JS           PIC  N(020).
           02  F              PIC  X(001).
           02  P-UNO          PIC  X(008).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(001).
           02  P-FKC          PIC  9(002).
           02  F              PIC  X(003).
           02  P-UCD          PIC  9(001).
           02  F              PIC  X(003).
           02  P-SSC          PIC  9(001).
           02  F              PIC  X(003).
           02  P-DHC          PIC  9(001).
           02  F              PIC  X(003).
           02  P-BIK          PIC  9(001).
           02  F              PIC  X(002).
           02  P-PNO          PIC  9(002).
           02  F              PIC  X(001).
           02  P-YMD          PIC  99/99/99.
           02  P-20B          PIC  X(005).
       01  W-R.
           02  W-KEY.
             03  W-TCD        PIC  9(004).
             03  W-CCD        PIC  9(003).
           02  W-NAME         PIC  N(026).
           02  W-JSU          PIC  N(020).
           02  W-JSS          PIC  N(020).
           02  W-UNO          PIC  X(008).
           02  W-TEL          PIC  X(014).
           02  W-FKC          PIC  9(002).
           02  W-UCD          PIC  9(001).
           02  W-SSC          PIC  9(001).
           02  W-BIK          PIC  9(001).
           02  W-DHC          PIC  9(001).
           02  F              PIC  X(014).
           02  W-MIC          PIC  9(001).
           02  W-MZC          PIC  9(001).
           02  W-NFN          PIC  9(001).
           02  W-PNO          PIC  9(002).
           02  W-YMD          PIC  9(006).
       01  W-DATA.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-TCDD         PIC  9(004).
           02  W-PC           PIC  9(001).
           02  W-BIKD         PIC  9(001).
           02  CHK            PIC  9(001).
           02  W-SEKEY.
             03  W-SKEY       PIC  9(004).
             03  W-EKEY       PIC  9(004).
           02  W-PAGE         PIC  9(004).
           02  W-DATE         PIC  9(006).
           02  W-YMDD         PIC  9(006).
           02  W-END          PIC  9(001) VALUE 0.
           02  W-BS           PIC  9(001).
           02  W-ACP          PIC  9(001).
           02  W-INV          PIC  9(001).
           02  W-SNGP         PIC  9(008).
           02  W-ENGP         PIC  9(008).
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
           02  W-DHCC         PIC  9(001).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LITM.
           COPY L-JCON.
           COPY LIHKBM.
           COPY LITCM.
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-TCD   PIC  9(004).
           02  FILLER.
             03  A-CCD   PIC  9(003).
             03  A-NAME  PIC  N(026).
           02  A-PNO   PIC  9(002).
           02  A-FKC   PIC  9(002).
           02  A-JSU   PIC  N(020).
           02  A-JSS   PIC  N(020).
           02  A-UNO   PIC  X(008).
           02  A-TEL   PIC  X(014).
           02  A-UCD   PIC  9(001).
           02  A-DHC   PIC  9(001).
           02  A-SSC   PIC  9(001).
           02  A-MZC   PIC  9(001).
           02  A-NFN   PIC  9(001).
           02  A-MIC   PIC  9(001).
           02  FILLER.
             03  A-SKEY  PIC  9(004).
             03  A-EKEY  PIC  9(004).
           02 A-DMM    PIC  9(001).
       01  C-DSP.
           02  D-TNA   PIC  N(026).
           02  FILLER.
             03  D-FKN   PIC  N(004).
             03  S-FKN   PIC  X(008) VALUE "        ".
           02  FILLER.
             03  D-UN    PIC  N(006).
             03  S-UN    PIC  X(012) VALUE "            ".
           02  D-SSM.
             03  FILLER  PIC  N(004) VALUE "出荷指示".
             03  FILLER.
               04  FILLER  PIC  N(002) VALUE "あり".
               04  FILLER  PIC  X(003) VALUE "=0,".
               04  FILLER  PIC  N(002) VALUE "なし".
               04  FILLER  PIC  X(002) VALUE "=1".
           02  S-SSM.
             03  FILLER  PIC  X(008) VALUE "        ".
             03  FILLER  PIC  X(001) VALUE " ".
             03  FILLER  PIC  X(013) VALUE
                  "             ".
           02  D-NFN   PIC  Z(001).
           02  D-PM    PIC  X(040) VALUE
                "<  得意先ｺｰﾄﾞ      より      迄打出し  >".
           02  D-EQ.
             03  FILLER  PIC  X(010) VALUE "直送先ｺｰﾄﾞ".
             03  FILLER  PIC  9(003).
             03  FILLER  PIC  N(004) VALUE "直送先名".
             03  FILLER  PIC  N(026).
           02  D-EQC.
             03  FILLER  PIC  X(025) VALUE
                  "                         ".
             03  FILLER  PIC  X(052) VALUE
                 "                                                    ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(023) VALUE
                  "***  TC-M ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2   PIC  X(017) VALUE
                  "***  TC-M ﾅｼ  ***".
             03  E-ME3   PIC  X(017) VALUE
                  "***  ｳﾝｿｳ ﾅｼ  ***".
             03  E-ME4   PIC  X(019) VALUE
                  "***  キャンセル ***".
             03  E-ME5   PIC  X(024) VALUE
                  "***  TC-M WRITE ｴﾗｰ  ***".
             03  E-ME6   PIC  X(026) VALUE
                  "***  TC-M REWRITE ｴﾗｰ  ***".
             03  E-ME7   PIC  X(025) VALUE
                  "***  TC-M DELETE ｴﾗｰ  ***".
             03  E-ME10  PIC  X(019) VALUE
                  "***  TEL.NO ｱﾘ  ***".
             03  E-ME11  PIC  X(020) VALUE
                  "***  ﾄﾄﾞｳﾌｹﾝ ﾅｼ  ***".
             03  E-ME12  PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME13  PIC  X(14)  VALUE
                  "他で使用中ｺｰﾄﾞ".
             03  E-ME14  PIC  N(04)  VALUE
                  "削除不可".
             03  E-ME20  PIC  X(13)  VALUE
                  "売上区分 ﾁｪｯｸ".
             03  E-SPACE PIC  X(27)  VALUE
                  "                           ".
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "181" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "56" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "5" "10" "4" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "6" "0" "55" "A-TCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CCD" "9" "6" "11" "3" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CCD" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "N" "6" "24" "52" "A-CCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NAME" BY REFERENCE W-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-PNO" "9" "7" "74" "2" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-PNO" BY REFERENCE W-PNO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FKC" "9" "8" "16" "2" "A-PNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FKC" BY REFERENCE W-FKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JSU" "N" "9" "16" "40" "A-FKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JSU" BY REFERENCE W-JSU "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JSS" "N" "10" "16" "40" "A-JSU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JSS" BY REFERENCE W-JSS "40" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UNO" "X" "11" "16" "8" "A-JSS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UNO" BY REFERENCE W-UNO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TEL" "X" "12" "16" "14" "A-UNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TEL" BY REFERENCE W-TEL "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UCD" "9" "13" "16" "1" "A-TEL" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UCD" BY REFERENCE W-UCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DHC" "9" "14" "16" "1" "A-UCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DHC" BY REFERENCE W-DHC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SSC" "9" "14" "66" "1" "A-DHC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SSC" BY REFERENCE W-SSC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MZC" "9" "15" "16" "1" "A-SSC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MZC" BY REFERENCE W-MZC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NFN" "9" "16" "16" "1" "A-MZC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NFN" BY REFERENCE W-NFN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-MIC" "9" "17" "16" "1" "A-NFN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-MIC" BY REFERENCE W-MIC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "16C-ACP" " " "22" "0" "8" "A-MIC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKEY" "9" "22" "19" "4" " " "16C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKEY" BY REFERENCE W-SKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-EKEY" "9" "22" "29" "4" "A-SKEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-EKEY" BY REFERENCE W-EKEY "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "66" "1" "16C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "326" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "5" "24" "52" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "8" "0" "16" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-FKN" "N" "8" "19" "8" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-FKN" BY REFERENCE HKB-FKNA "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-FKN" "X" "8" "19" "8" "D-FKN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "13" "0" "24" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UN" "N" "13" "18" "12" " " "03C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-UN" BY REFERENCE JCON2-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "S-UN" "X" "13" "18" "12" "D-UN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SSM" " " "0" "0" "21" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-SSM" "N" "14" "57" "8" " " "D-SSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-SSM" " " "15" "24" "13" "01D-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-SSM" "N" "15" "66" "4" " " "02D-SSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-SSM" "X" "15" "70" "3" "0102D-SSM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0302D-SSM" "N" "15" "73" "4" "0202D-SSM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0402D-SSM" "X" "15" "77" "2" "0302D-SSM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "S-SSM" " " "0" "0" "22" "D-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01S-SSM" "X" "14" "57" "8" " " "S-SSM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02S-SSM" "X" "14" "66" "1" "01S-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03S-SSM" "X" "15" "66" "13" "02S-SSM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NFN" "Z" "16" "16" "1" "S-SSM" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NFN" BY REFERENCE W-NFN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "22" "5" "40" "D-NFN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EQ" " " "22" "0" "73" "D-PM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-EQ" "X" "22" "4" "10" " " "D-EQ" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-EQ" "9" "22" "16" "3" "01D-EQ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "02D-EQ" BY REFERENCE TC-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-EQ" "N" "22" "20" "8" "02D-EQ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-EQ" "N" "22" "29" "52" "03D-EQ" " " RETURNING RESU.
       CALL "SD_From" USING 
            "04D-EQ" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-EQC" " " "22" "0" "77" "D-EQ" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-EQC" "X" "22" "4" "25" " " "D-EQC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-EQC" "X" "22" "29" "52" "01D-EQC" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "270" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "270" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "23" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "17" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "19" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "24" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "26" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "25" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "19" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "20" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "18" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "14" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "N" "24" "15" "8" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "13" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-SPACE" "X" "24" "15" "27" "E-ME20" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "C3_Set_Jrcode" USING
            USER_ID BY REFERENCE COMPLETION_CODE 000.
           COPY LIBCPR.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" T-M_PNAME1 "SHARED" BY REFERENCE T-M_IDLST "2"
            "T-KEY" BY REFERENCE T-KEY "T-KEY2" BY REFERENCE T-KEY2.
           ACCEPT W-DATE FROM DATE.
      *
           MOVE ZERO TO W-NGP.
           MOVE W-DATE TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NGP TO W-ENGP.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               SUBTRACT 1 FROM W-NEN
               MOVE 12 TO W-GET
           END-IF
           MOVE W-NGP TO W-SNGP.
      *
           MOVE DATE-02R TO H-DATE.
           MOVE ZERO TO W-PAGE W-TCDD W-PC.
       M-040.
           CALL "SD_Screen_Output" USING "SCHM21" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT = 4
               GO TO M-700
           END-IF
           IF  W-ACT < 1 OR > 4
               GO TO M-040
           END-IF
           MOVE 0 TO W-ACP.
       M-100.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-BS = 9
               GO TO M-040
           END-IF.
       M-500.
           PERFORM WRD-RTN THRU WRD-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF
           GO TO M-100.
       M-700.
           PERFORM LST-RTN THRU LST-EX.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE T-M_IDLST T-M_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      *---------------　入　　力　--------------------------------------
       ACP-RTN.
           IF  W-ACP = 1
               GO TO ACP-020
           END-IF
           MOVE 0 TO W-BS.
           CALL "SD_Screen_Output" USING "SCHM21" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           IF  W-TCDD NOT = ZERO
               CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           IF  W-TCDD = 9999
               GO TO ACP-RTN
           END-IF.
       ACP-020.
           MOVE ZERO TO TC-KEY.
           MOVE W-TCDD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           CALL "SD_Screen_Output" USING "SCHM21" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           MOVE TC-BIK TO W-BIKD.
           MOVE TC-DHC TO W-DHCC.
           INITIALIZE W-R.
           MOVE ALL "　"  TO W-NAME W-JSU W-JSS.
           IF  W-ACT NOT = 1
               GO TO ACP-060
           END-IF
           MOVE W-BIKD TO W-BIK.
           MOVE 1 TO W-CCD.
       ACP-040.
           ADD 1 TO W-CCD.
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-060
           END-IF
           IF  TC-TCD NOT = W-TCDD
               GO TO ACP-060
           END-IF
           IF  TC-CCD = W-CCD
               GO TO ACP-040
           END-IF.
       ACP-060.
           CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-CCD "A-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-SPACE" E-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 0 TO W-ACP
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           IF  W-CCD = 000 OR 999
               GO TO ACP-060
           END-IF
           IF  W-CCD = 001
               CALL "SD_Output" USING "D-SSM" D-SSM "p" RETURNING RESU
           ELSE
               MOVE 0 TO W-SSC
               CALL "SD_Output" USING "S-SSM" S-SSM "p" RETURNING RESU
           END-IF
           MOVE W-TCDD TO W-TCD.
           MOVE W-KEY TO TC-KEY.
      *           READ TC-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-080
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           MOVE TC-R TO W-R.
           MOVE W-BIKD TO W-BIK.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-PNO" A-PNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JSU" A-JSU "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JSS" A-JSS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UNO" A-UNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-FKC" A-FKC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UCD" A-UCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TEL" A-TEL "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DHC" A-DHC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-MZC" A-MZC "p" RETURNING RESU.
           CALL "SD_Output" USING "A-MIC" A-MIC "p" RETURNING RESU.
           IF  W-TCDD = 5000
               CALL "SD_Output" USING "D-NFN" D-NFN "p" RETURNING RESU
           END-IF.
       ACP-065.
           PERFORM HKBM-RTN THRU HKBM-EX.
           PERFORM JCON-RTN THRU JCON-EX.
           IF  W-FKC NOT = ZERO
               CALL "SD_Output" USING "D-FKN" D-FKN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "S-FKN" S-FKN "p" RETURNING RESU
           END-IF
           IF  W-UCD NOT = 0
               CALL "SD_Output" USING "D-UN" D-UN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "S-UN" S-UN "p" RETURNING RESU
           END-IF
           IF  W-CCD = 001
               CALL "SD_Output" USING "A-SSC" A-SSC "p" RETURNING RESU
           END-IF
           IF  W-ACT = 2
               GO TO ACP-100
           END-IF
           IF  W-CCD = 001
               GO TO ACP-060
           END-IF
           IF  W-YMD = ZERO
               GO TO ACP-520
           END-IF
           MOVE ZERO TO W-NGP.
           MOVE W-YMD TO W-NGPS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-SNGP < W-NGP
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           GO TO ACP-520.
       ACP-080.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF.
       ACP-100.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "N" "52"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF.
       ACP-105.
           CALL "SD_Output" USING "A-PNO" A-PNO "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-PNO "A-PNO" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-105
           END-IF.
       ACP-110.
           CALL "SD_Accept" USING BY REFERENCE A-FKC "A-FKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-105
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-110
           END-IF
           PERFORM HKBM-RTN THRU HKBM-EX.
           IF  W-INV = 9
               MOVE 0 TO W-INV
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-110
           END-IF
           IF  W-FKC = ZERO
               GO TO ACP-110
           END-IF
           CALL "SD_Output" USING "D-FKN" D-FKN "p" RETURNING RESU.
           IF  W-ACT = 1
               IF  W-JSU = SPACE
                   MOVE HKB-FKNA TO W-JSU
               END-IF
           END-IF.
       ACP-120.
           CALL "SD_Output" USING "A-JSU" A-JSU "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-JSU "A-JSU" "N" "40"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-110
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF.
       ACP-140.
           CALL "SD_Output" USING "A-JSS" A-JSS "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-JSS "A-JSS" "N" "40"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-140
           END-IF.
       ACP-160.
           CALL "SD_Accept" USING BY REFERENCE A-UNO "A-UNO" "X" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-160
           END-IF
           IF  W-FKC NOT = 99
               IF  W-UNO = SPACE
                   GO TO ACP-160
               END-IF
           END-IF.
       ACP-170.
           CALL "SD_Accept" USING BY REFERENCE A-TEL "A-TEL" "X" "14"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-170
           END-IF
           IF  W-ACT = 1
               IF  W-TEL NOT = SPACE
                   PERFORM TEL-RTN THRU TEL-EX
               END-IF
           END-IF.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-UCD "A-UCD" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-SPACE" E-SPACE "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-170
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-200
           END-IF
           PERFORM JCON-RTN THRU JCON-EX.
           IF  W-INV = 9
               MOVE 0 TO W-INV
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-200
           END-IF
           IF  W-UCD = ZERO
               CALL "SD_Output" USING "S-UN" S-UN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-UN" D-UN "p" RETURNING RESU
           END-IF
           IF  W-CCD = 001
               GO TO ACP-240
           END-IF
           MOVE W-TCD TO T-KEY.
      *           READ T-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" T-M_PNAME1 BY REFERENCE T-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 99 TO T-SS
           END-IF
           IF  T-SS NOT = ZERO
               IF  W-DHCC NOT = 5
                   IF  W-DHC = 0
                       CALL "SD_Output" USING
                        "A-DHC" A-DHC "p" RETURNING RESU
                       GO TO ACP-250
                   END-IF
               END-IF
           END-IF.
       ACP-220.
           CALL "SD_Accept" USING BY REFERENCE A-DHC "A-DHC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-220
           END-IF
           IF  W-DHC NOT = 0 AND 5
               GO TO ACP-220
           END-IF
           IF  W-CCD NOT = 001
               GO TO ACP-250
           END-IF.
       ACP-240.
           CALL "SD_Accept" USING BY REFERENCE A-SSC "A-SSC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-240
           END-IF
           IF  W-SSC > 1
               GO TO ACP-240
           END-IF.
       ACP-250.
           IF  W-CCD = 001
               GO TO ACP-260
           END-IF
           IF  W-DHCC = 5
               IF  W-DHC = 0
                   CALL "SD_Output" USING
                    "E-ME20" E-ME20 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
               END-IF
           END-IF
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
       ACP-260.
           CALL "SD_Accept" USING BY REFERENCE A-MZC "A-MZC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-CCD = 001
                   GO TO ACP-240
               ELSE
                   GO TO ACP-220
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-260
           END-IF
           IF  W-MZC NOT = 0 AND 1
               GO TO ACP-260
           END-IF
           IF  W-TCDD NOT = 5000
               MOVE 0 TO W-NFN
               CALL "SD_Output" USING "D-NFN" D-NFN "p" RETURNING RESU
               GO TO ACP-280
           END-IF.
       ACP-270.
           CALL "SD_Accept" USING BY REFERENCE A-NFN "A-NFN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-270
           END-IF
           CALL "SD_Output" USING "D-NFN" D-NFN "p" RETURNING RESU.
           IF  W-NFN < 1 OR > 2
               GO TO ACP-270
           END-IF.
       ACP-280.
           CALL "SD_Accept" USING BY REFERENCE A-MIC "A-MIC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-TCDD = 5000
                   GO TO ACP-270
               ELSE
                   GO TO ACP-260
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-280
           END-IF
           IF  W-MIC > 1
               GO TO ACP-280
           END-IF.
       ACP-520.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-020
               ELSE
                   GO TO ACP-280
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-520
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-020
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-520
           END-IF.
       ACP-EX.
           EXIT.
      *---------------　ＨＫＢＭ　ＲＥＡＤ　----------------------------
       HKBM-RTN.
           MOVE 0 TO W-INV.
           IF  W-FKC = ZERO
               GO TO HKBM-EX
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           MOVE 01 TO HKB-NO.
           MOVE W-FKC TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　　　　" TO HKB-FKNA
               MOVE 9 TO W-INV
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
       HKBM-EX.
           EXIT.
      *---------------　ＪＣＯＮ　ＲＥＡＤ　----------------------------
       JCON-RTN.
           MOVE 0 TO W-INV.
           IF  W-UCD = ZERO
               GO TO JCON-EX
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE 2 TO JCON2-01.
           MOVE W-UCD TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "　　　　　　" TO JCON2-03
               MOVE 9 TO W-INV
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
       JCON-EX.
           EXIT.
      *---------------　ＴＥＬ　チェック　------------------------------
       TEL-RTN.
           MOVE ZERO TO TC-KEY.
           MOVE W-TCDD TO TC-TCD.
      *           START TC-M KEY NOT < TC-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TC-M_PNAME1 "TC-KEY" " NOT < " TC-KEY RETURNING RET.
           IF  RET = 1
               GO TO TEL-EX
           END-IF.
       TEL-020.
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TEL-EX
           END-IF
           IF  TC-TCD NOT = W-TCDD
               GO TO TEL-EX
           END-IF
           IF  TC-TEL NOT = SPACE
               IF  TC-TEL = W-TEL
                   CALL "SD_Output" USING
                    "D-EQ" D-EQ "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME10" E-ME10 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "D-EQC" D-EQC "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-SPACE" E-SPACE "p" RETURNING RESU
               END-IF
           END-IF
           GO TO TEL-020.
       TEL-EX.
           EXIT.
      *----------  ＷＲＩＴＥ・ＲＥＷＲＩＴＥ・ＤＥＬＥＴＥ　-----------
       WRD-RTN.
           MOVE W-R TO TC-R.
           IF  W-ACT NOT = 1
               GO TO WRD-060
           END-IF
      *           WRITE TC-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-020
           END-IF
           GO TO WRD-900.
       WRD-020.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO WRD-EX
           END-IF
           CALL "SD_Output" USING "E-SPACE" E-SPACE "p" RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           MOVE "TCM          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-SPACE" E-SPACE "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           GO TO WRD-RTN.
       WRD-060.
           IF  W-ACT NOT = 2
               GO TO WRD-100
           END-IF
      *           REWRITE TC-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TC-M_PNAME1 TC-M_LNAME TC-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME6" E-ME6 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO WRD-EX
           END-IF
           GO TO WRD-900.
       WRD-100.
      *           DELETE TC-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TC-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               MOVE 9 TO W-END
               GO TO WRD-EX
           END-IF.
       WRD-900.
           IF  W-ACT = 1 OR 2
               MOVE 1 TO W-ACP
           END-IF.
       WRD-EX.
           EXIT.
      *---------------　作　　表　--------------------------------------
       LST-RTN.
           MOVE ZERO TO W-SEKEY.
           CALL "SD_Output" USING "D-PM" D-PM "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-SKEY "A-SKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-RTN
           END-IF.
       LST-020.
           CALL "SD_Accept" USING BY REFERENCE A-EKEY "A-EKEY" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-020
           END-IF
           IF  W-SKEY > W-EKEY
               GO TO LST-RTN
           END-IF.
       LST-040.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-RTN
           END-IF
           IF  W-DMM = 9
               GO TO LST-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO LST-040
           END-IF
      *
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
      *
           MOVE ZERO TO TC-KEY.
           MOVE W-SKEY TO TC-TCD.
      *           START TC-M KEY NOT < TC-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TC-M_PNAME1 "TC-KEY" " NOT < " TC-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO LST-RTN
           END-IF
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO LST-RTN
           END-IF
           IF  TC-TCD > W-EKEY
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO LST-RTN
           END-IF
           IF  W-PC = ZERO
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           END-IF.
       LST-060.
           MOVE 0 TO CHK.
           MOVE TC-TCD TO W-TCD.
       LST-080.
           MOVE SPACE TO W-P.
           MOVE SPACE TO P-NAME P-JS.
           IF  CHK = 0
               MOVE 5 TO CHK
               MOVE W-TCD TO P-TCD
               MOVE "-" TO P-V
           END-IF
           MOVE TC-CCD TO P-CCD.
           MOVE TC-NAME TO P-NAME.
           MOVE TC-JSU TO P-JS.
           MOVE TC-UNO TO P-UNO.
           IF  TC-FKC NOT = ZERO
               MOVE TC-FKC TO P-FKC
           END-IF
           IF  TC-UCD NOT = ZERO
               MOVE TC-UCD TO P-UCD
           END-IF
           MOVE TC-TEL TO P-TEL.
           IF  TC-DATE NOT = ZERO
               MOVE TC-DATE TO P-YMD
           END-IF
           MOVE TC-BIK TO P-BIK.
           MOVE TC-DHC TO P-DHC.
           IF  TC-PNO NOT = ZERO
               MOVE TC-PNO TO P-PNO
           END-IF
           IF  TC-CCD = 001
               MOVE TC-SSC TO P-SSC
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               MOVE W-TCD TO P-TCD
               MOVE "-" TO P-V
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE W-15K TO P-15B.
           MOVE W-20K TO P-20B.
           MOVE SPACE TO SP-R.
           MOVE W-P TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF  TC-JSS NOT = SPACE
               MOVE SPACE TO W-P
               MOVE SPACE TO P-NAME P-JS
               MOVE TC-JSS TO P-JS
               MOVE W-15K TO P-15B
               MOVE W-20K TO P-20B
               MOVE SPACE TO SP-R
               MOVE W-P TO SP-R
               CALL "PR_Write" USING SP-R RETURNING RESP
               MOVE SPACE TO SP-R
           END-IF.
       LST-100.
      *           READ TC-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO LST-900
           END-IF
           IF  TC-TCD > W-EKEY
               GO TO LST-900
           END-IF
           IF  TC-TCD = 9999
               GO TO LST-900
           END-IF
           IF  TC-TCD NOT = W-TCD
               GO TO LST-060
           END-IF
           GO TO LST-080.
       LST-900.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Open" USING
            "I-O" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
       LST-EX.
           EXIT.
      *---------------　見出し　印字　----------------------------------
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-020.
           ADD 1 TO W-PAGE.
           MOVE W-PAGE TO H-PAGE.
           MOVE W-20K TO P-20.
           MOVE SPACE TO SP-R.
           MOVE HEAD1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           MOVE W-15K TO P-15A.
           MOVE W-20K TO P-20A.
           MOVE HEAD2 TO SP-R.
           CALL "PR_LineFeed" USING "2" RETURNING RESP.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
