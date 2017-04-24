       IDENTIFICATION   DIVISION.
       PROGRAM-ID. JTN05I.
      ************************************************************
      *    PROGRAM         :  統一伝票入力（トラスコ他ＦＡＸ分） *
      *    PRINTER TYPE    :  JIPS                               *
      *    SCREEN          :  SJN05I                             *
      *    COMPILE TYPE    :  COBOL                              *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE  SECTION.
       77  ERR-STAT           PIC  X(002).
       77  W-FILE             PIC  X(013).
       77  W-END              PIC  9(001) VALUE 0.
       01  W-DATEA.
           02  W-NNGP         PIC  9(006).
           02  W-TIMED        PIC  9(008).
           02  W-TIMEW REDEFINES W-TIMED.
             03  W-TIME       PIC  9(006).
             03  F            PIC  9(002).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  H-FNGP         PIC  99/99/99.
           02  F              PIC  X(004) VALUE " ～ ".
           02  H-TNGP         PIC  99/99/99.
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  N(020) VALUE
                "＊＊＊　　統一伝票　入力リスト　　＊＊＊".
           02  F              PIC  X(029) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(004) VALUE "　伝票№".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "日　　付".
           02  F              PIC  X(007) VALUE "  ｺｰﾄﾞ ".
           02  F              PIC  N(008) VALUE "得　意　先　名　".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(004) VALUE "ｺｰﾄﾞ".
           02  F              PIC  N(014) VALUE
                "直　送　先　名　・　店　名　".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  N(004) VALUE "倉　庫　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "運　送　".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "発注№　".
           02  F              PIC  X(004) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(006) VALUE SPACE.
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　受注№".
           02  F              PIC  X(001) VALUE "-".
           02  F              PIC  N(002) VALUE "行　".
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｻｲｽﾞ  ".
           02  F              PIC  N(002) VALUE "名称".
           02  F              PIC  X(006) VALUE "(ﾄﾗｽｺ)".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(002) VALUE "数量".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　原単価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "原価金額".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　売単価".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売価金額".
       01  W-P1.
           02  P-DNO          PIC  9(006).
           02  F              PIC  X(002).
           02  P-DATE         PIC  99/99/99.
           02  F              PIC  X(001).
           02  P-TCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-TNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-TPC          PIC  9(004).
           02  P-CCDD  REDEFINES P-TPC.
             03  P-CCD        PIC  9(003).
             03  F            PIC  X(001).
           02  P-CNA          PIC  N(026).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(006).
           02  F              PIC  X(001).
           02  P-UNA          PIC  N(006).
           02  F              PIC  X(001).
           02  P-HNO          PIC  X(010).
       01  W-P2.
           02  F              PIC  X(006).
           02  P-V1           PIC  X(001).
           02  P-GNO          PIC  9.
           02  F              PIC  X(009).
           02  P-JNO          PIC  9(006).
           02  P-V2           PIC  X(001).
           02  P-JGN          PIC  9.
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  X(004).
           02  F              PIC  X(002).
           02  P-TMS          PIC  X(020).
           02  P-SU           PIC ------.
           02  P-GT           PIC ZZZ,ZZZ.
           02  P-GKIN         PIC --,---,---.
           02  P-UT           PIC ZZZ,ZZZ.
           02  P-UKIN         PIC --,---,---.
       01  W-P3.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  N(002) VALUE "配達".
           02  F              PIC  X(001) VALUE ":".
           02  P-THT          PIC  N(010).
           02  F              PIC  N(002) VALUE "摘要".
           02  F              PIC  X(001) VALUE ":".
           02  P-TTE          PIC  N(020).
           02  F              PIC  X(002).
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(001) VALUE ":".
           02  P-TSU          PIC ------.
           02  F              PIC  X(007).
           02  P-TGKIN        PIC --,---,---.
           02  F              PIC  X(007).
           02  P-TUKIN        PIC --,---,---.
       01  W-DATA.
           02  W-CRT.
             03  W-ACT        PIC  9(001).
             03  W-JCN        PIC  9(006).
             03  W-DNO        PIC  9(006).
             03  W-TCD        PIC  9(004).
             03  W-CCD        PIC  9(003).
             03  W-TPC        PIC  9(004).
             03  W-DATE       PIC  9(006).
             03  W-NGPD REDEFINES W-DATE.
               04  W-NEN      PIC  9(002).
               04  W-GET      PIC  9(002).
               04  W-PEY      PIC  9(002).
             03  W-HNO        PIC  X(010).
             03  W-SOK        PIC  9(001).
             03  W-UNS        PIC  9(001).
             03  W-TEKI       PIC  N(028).
             03  W-TEKD  REDEFINES W-TEKI.
               04  W-THT      PIC  N(009).
               04  W-TTE      PIC  N(019).
             03  W-PRC        PIC  9(001).
             03  W-UPC        PIC  9(001).
             03  W-MEI.
               04  W-MEID  OCCURS    6.
                 05  W-JNOD.
                   06  W-JNO  PIC  9(006).
                   06  W-JGN  PIC  9(001).
                 05  W-HCD    PIC  9(006).
                 05  W-SZN    PIC  X(003).
                 05  W-SKB    PIC  9(001).
                 05  W-SNO    PIC  9(002).
                 05  W-SU     PIC S9(005).
                 05  W-GT     PIC  9(007).
                 05  W-UT     PIC  9(007).
                 05  W-GKIN   PIC S9(008).
                 05  W-UKIN   PIC S9(008).
                 05  W-ISU    PIC  9(003).
                 05  W-TRN    PIC  X(020).
                 05  W-JAN    PIC  X(013).
             03  W-DMM        PIC  9(001).
             03  W-SEN        PIC  9(001).
             03  W-FNGP       PIC  9(006).
             03  W-FNGPD REDEFINES W-FNGP.
               04  W-FNEN     PIC  9(002).
               04  W-FGET     PIC  9(002).
               04  W-FPEY     PIC  9(002).
             03  W-TNGP       PIC  9(006) VALUE 999999.
             03  W-TNGPD REDEFINES W-TNGP.
               04  W-TNEN     PIC  9(002).
               04  W-TGET     PIC  9(002).
               04  W-TPEY     PIC  9(002).
             03  W-DNOF       PIC  9(006).
             03  W-DNOT       PIC  9(006) VALUE 999999.
      *
           02  W-PAGE         PIC  9(003).
           02  W-SNGP         PIC  9(006).
           02  W-ENGP         PIC  9(006).
           02  W-NGP          PIC  9(006).
           02  W-INV          PIC  9(001).
           02  W-LC1          PIC  9(002).
           02  W-LC2          PIC  9(002).
           02  W-GN           PIC  9(001).
           02  W-GND          PIC  9(001).
           02  W-DC           PIC  9(001).
           02  W-SUC          PIC  9(001).
           02  W-FHCD         PIC  9(006).
           02  W-TNA          PIC  N(026).
           02  W-SNA          PIC  N(006).
           02  W-UNA          PIC  N(006).
           02  W-SIZM         PIC  X(003).
           02  W-SIZN         PIC  X(004).
           02  WT-D.
             03  WT-SU        PIC  9(006).
             03  WT-GKIN      PIC  9(009).
             03  WT-UKIN      PIC  9(009).
           02  CNT            PIC  9(002).
           02  W-UCHK         PIC  9(001).
           02  W-SCC          PIC  9(001).
           02  W-SC           PIC  9(001).
           02  W-C            PIC  9(002).
           02  W-ASZD.
             03  W-ASZ   OCCURS   5.
               04  W-SZD   OCCURS  10.
                 05  W-SZ     PIC  X(003).
           02  W-MSZ.
             03  F            PIC  X(030) VALUE
                  "000   SS S  M  L  LL XL XXL   ".
             03  F            PIC  X(030) VALUE
                  "                     280290300".
             03  F            PIC  X(030) VALUE
                  "125130135140150160170180190200".
             03  F            PIC  X(030) VALUE
                  "210215220225230235240245250   ".
             03  F            PIC  X(030) VALUE
                  "240245250255260265270275      ".
           02  W-ASIZD.
             03  W-ASIZ  OCCURS   5.
               04  W-SIZD  OCCURS  10.
                 05  W-SIZ    PIC  X(004).
           02  W-MSIZ.
             03  F            PIC  X(040) VALUE
                  "----      SS   S   M   L  LL  XL XXL    ".
             03  F            PIC  X(040) VALUE
                  "                            28.029.030.0".
             03  F            PIC  X(040) VALUE
                  "12.513.013.514.015.016.017.018.019.020.0".
             03  F            PIC  X(040) VALUE
                  "21.021.522.022.523.023.524.024.525.0    ".
             03  F            PIC  X(040) VALUE
                  "24.024.525.025.526.026.527.027.5    ----".
           02  W-ATYD.
             03  W-TYD   OCCURS  56.
               04  W-TY       PIC  X(001).
           COPY LSTAT.
      *
           COPY L-TDIF.
           COPY L-JCON.
           COPY LIDTHT.
           COPY LITCM.
           COPY LWTNAF.
           COPY LIHIM2.
           COPY LJMSTD.
           COPY LTRUIJ.
           COPY LNJZAI.
           COPY LRCODE.
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
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-TCD   PIC  9(004).
             03  A-JCN   PIC  9(006).
           02  FILLER.
             03  A-TPC   PIC  9(004).
             03  A-DNO   PIC  9(006).
           02  FILLER.
             03  A-CCD   PIC  9(003).
             03  A-DATE  PIC  9(006).
           02  FILLER.
             03  A-HNO   PIC  X(010).
             03  A-SOK   PIC  9(001).
             03  A-UNS   PIC  9(001).
           02  A-HM.
             03  A-JNO   PIC  9(006).
             03  A-JGN   PIC  9(001).
             03  A-SZN   PIC  X(003).
           02  FILLER.
             03  A-HCD   PIC  9(006).
             03  A-SU    PIC  9(005).
             03  A-GT    PIC  9(007).
             03  A-GKIN  PIC  9(008).
             03  A-UT    PIC  9(007).
           02  FILLER.
             03  A-THT   PIC  N(009).
             03  A-TTE   PIC  N(019).
           02  A-SEN   PIC  9(001).
           02  FILLER.
             03  A-FNEN  PIC  9(002).
             03  A-FGET  PIC  9(002).
             03  A-FPEY  PIC  9(002).
             03  A-TNEN  PIC  9(002).
             03  A-TGET  PIC  9(002).
             03  A-TPEY  PIC  9(002).
           02  FILLER.
             03  A-DNOF  PIC  9(006).
             03  A-DNOT  PIC  9(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-TNA   PIC  N(026).
             03  D-JCNC  PIC  X(006) VALUE "      ".
           02  FILLER.
             03  D-MNA   PIC  N(026).
             03  D-TPCC.
               04  FILLER  PIC  X(041) VALUE
                    "                                         ".
               04  FILLER  PIC  X(016) VALUE
                    "                ".
             03  D-DNOC  PIC  X(006) VALUE "      ".
           02  FILLER.
             03  D-CNA   PIC  N(026).
             03  D-CNAC.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(012) VALUE
                    "            ".
           02  FILLER.
             03  D-HNOC  PIC  X(010) VALUE "          ".
             03  D-SNA   PIC  N(006).
             03  D-UNA   PIC  N(006).
           02  FILLER.
             03  D-JNOC  PIC  X(008) VALUE "        ".
             03  D-JNOV  PIC  X(001) VALUE "-".
             03  D-HNA   PIC  N(024).
             03  D-SIZD  PIC  X(004).
           02  FILLER.
             03  D-TRN   PIC  X(020).
             03  D-TRNC  PIC  X(020) VALUE
                  "                    ".
           02  D-MEI.
             03  D-SU    PIC -----9 .
             03  D-GT    PIC ZZZZZZZ .
             03  D-GKIN  PIC --,---,--- .
             03  D-UT    PIC ZZZZZZZ .
             03  D-UKIN  PIC --,---,---  .
           02  D-KEI.
             03          FILLER  PIC -----9 .
             03          FILLER  PIC --,---,--- .
             03          FILLER  PIC --,---,--- .
           02  D-BDNO  PIC  9(006).
           02  D-MEIC.
             03  FILLER.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
             03  FILLER.
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
               04  FILLER  PIC  X(040) VALUE
                    "                                        ".
           02  D-PRM.
             03  FILLER  PIC  N(024) VALUE
                  "＊＊＊　　統一伝票入力　チェックリスト　　＊＊＊".
             03  FILLER  PIC  X(012) VALUE
                  "未印字分 = 1".
             03  FILLER  PIC  X(018) VALUE
                  "印字済分 = 2 ...  ".
             03  FILLER  PIC  X(032) VALUE
                  "’00年00月00日 ～ ’99年99月99日".
             03  FILLER.
               04  FILLER  PIC  N(002) VALUE "確認".
               04  FILLER  PIC  X(023) VALUE
                    "(OK=1,NO=9) --->   ﾘﾀｰﾝ".
           02  FILLER.
             03  D-DNM   PIC  X(024) VALUE
                  "伝票№  000000 ～ 999999".
             03  D-DNC   PIC  X(024) VALUE
                  "                        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME2   PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME3   PIC  X(018) VALUE
                  "***  ﾄｸｲｻｷ ﾅｼ  ***".
             03  E-ME4   PIC  X(018) VALUE
                  "***  ﾁｮｸｿｳ ﾅｼ  ***".
             03  E-ME5   PIC  X(017) VALUE
                  "***  ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-ME6   PIC  X(017) VALUE
                  "***  ｻｲｽﾞ ﾅｼ  ***".
             03  E-ME7   PIC  N(005) VALUE
                  "キャンセル".
             03  E-ME8   PIC  X(018) VALUE
                  "***  JCON ﾅｼ   ***".
             03  E-ME9   PIC  X(027) VALUE
                  "***  JCON REWRITE ｴﾗｰ   ***".
             03  E-ME10  PIC  X(023) VALUE
                  "***  TDIF DATA ｴﾗｰ  ***".
             03  E-ME11  PIC  X(024) VALUE
                  "***  TDIF WRITE ｴﾗｰ  ***".
             03  E-ME12  PIC  X(026) VALUE
                  "***  TDIF REWRITE ｴﾗｰ  ***".
             03  E-ME13  PIC  X(025) VALUE
                  "***  TDIF DELETE ｴﾗｰ  ***".
             03  E-ME14  PIC  X(018) VALUE
                  "***  JMSTD ﾅｼ  ***".
             03  E-ME15  PIC  X(019) VALUE
                  "***  ｱｽﾞｶﾘ ｴﾗｰ  ***".
             03  E-ME16  PIC  X(018) VALUE
                  "***  ﾋﾝﾒｲ ｴﾗｰ  ***".
             03  E-ME17  PIC  X(022) VALUE
                  "***  ﾄﾗｽｺ ﾋﾝﾒｲ ﾅｼ  ***".
             03  E-ME18  PIC  X(022) VALUE
                  "***  ｻｲｽﾞｸﾌﾞﾝ ｴﾗｰ  ***".
             03  E-ME19  PIC  X(021) VALUE
                  "***  ﾁｮｸｿｳｻｷ ｴﾗｰ  ***".
             03  E-ME20  PIC  X(019) VALUE
                  "***  ﾄｸｲｻｷ ｴﾗｰ  ***".
             03  E-ME21  PIC  X(026) VALUE
                  "***  ｺｳｼﾝｽﾞﾐ ｼｭｳｾｲ ﾌｶ  ***".
             03  E-ME22  PIC  X(018) VALUE
                  "***  NJZAI ﾅｼ  ***".
             03  E-ME23  PIC  X(019) VALUE
                  "***  ﾀﾝｶ ﾐﾄｳﾛｸ  ***".
             03  E-ME24  PIC  X(019) VALUE
                  "***  ｽｳﾘｮｳ ｴﾗｰ  ***".
             03  E-ME25  PIC  X(027) VALUE
                  "***  JMSTD REWRITE ｴﾗｰ  ***".
             03  E-ME26  PIC  X(027) VALUE
                  "***  NJZAI REWRITE ｴﾗｰ  ***".
             03  E-ME27  PIC  X(025) VALUE
                  "***  NJZAI WRITE ｴﾗｰ  ***".
             03  E-ME28  PIC  X(022) VALUE
                  "***  ｼﾞｭﾁｭｳNO ｴﾗｰ  ***".
             03  E-ME29  PIC  X(020) VALUE
                  "***  JANｺｰﾄﾞ ﾅｼ  ***".
             03  E-ME30  PIC  X(018) VALUE
                  "***  ﾀﾝｶ ﾁｪｯｸ  ***".
             03  E-KEY   PIC  X(007).
             03  E-NJZAI PIC  X(008).
             03  E-JMST  PIC  X(007).
           COPY LSSEM.
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "167" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "2" "53" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "3" "0" "10" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TCD" "9" "3" "8" "4" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TCD" BY REFERENCE W-TCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JCN" "9" "3" "75" "6" "A-TCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JCN" BY REFERENCE W-JCN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "4" "0" "10" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPC" "9" "4" "8" "4" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPC" BY REFERENCE W-TPC "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNO" "9" "4" "75" "6" "A-TPC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "5" "0" "9" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-CCD" "9" "5" "9" "3" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-CCD" BY REFERENCE W-CCD "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DATE" "9" "5" "75" "6" "A-CCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DATE" BY REFERENCE W-DATE "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-ACP" " " "6" "0" "12" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HNO" "X" "6" "8" "10" " " "05C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HNO" BY REFERENCE W-HNO "10" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SOK" "9" "6" "24" "1" "A-HNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SOK" BY REFERENCE W-SOK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UNS" "9" "6" "44" "1" "A-SOK" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UNS" BY REFERENCE W-UNS "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HM" " " "W-LC1" "0" "10" "05C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JNO" "9" "W-LC1" "1" "6" " " "A-HM" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JNO" BY REFERENCE W-JNO(1) "6" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JGN" "9" "W-LC1" "8" "1" "A-JNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JGN" BY REFERENCE W-JGN(1) "6" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SZN" "X" "W-LC1" "60" "3" "A-JGN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SZN" BY REFERENCE W-SZN(1) "3" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "07C-ACP" " " "W-LC2" "0" "33" "A-HM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCD" "9" "W-LC2" "2" "6" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-HCD" BY REFERENCE W-HCD(1) "6" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SU" "9" "W-LC2" "38" "5" "A-HCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SU" BY REFERENCE W-SU(1) "5" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GT" "9" "W-LC2" "44" "7" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GT" BY REFERENCE W-GT(1) "7" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-GKIN" "9" "W-LC2" "52" "8" "A-GT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-GKIN" BY REFERENCE W-GKIN(1) "8" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UT" "9" "W-LC2" "63" "7" "A-GKIN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UT" BY REFERENCE W-UT(1) "7" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "08C-ACP" " " "22" "0" "56" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-THT" "N" "22" "6" "18" " " "08C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-THT" BY REFERENCE W-THT "18" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TTE" "N" "22" "30" "38" "A-THT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TTE" BY REFERENCE W-TTE "38" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SEN" "9" "8" "43" "1" "08C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SEN" BY REFERENCE W-SEN "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "10C-ACP" " " "11" "0" "12" "A-SEN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FNEN" "9" "11" "26" "2" " " "10C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FNEN" BY REFERENCE W-FNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FGET" "9" "11" "30" "2" "A-FNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FGET" BY REFERENCE W-FGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FPEY" "9" "11" "34" "2" "A-FGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FPEY" BY REFERENCE W-FPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNEN" "9" "11" "44" "2" "A-FPEY" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNEN" BY REFERENCE W-TNEN "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TGET" "9" "11" "48" "2" "A-TNEN" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TGET" BY REFERENCE W-TGET "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TPEY" "9" "11" "52" "2" "A-TGET" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TPEY" BY REFERENCE W-TPEY "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "11C-ACP" " " "13" "0" "12" "10C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNOF" "9" "13" "36" "6" " " "11C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNOF" BY REFERENCE W-DNOF "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNOT" "9" "13" "46" "6" "A-DNOF" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DNOT" BY REFERENCE W-DNOT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "65" "1" "11C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "829" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "3" "0" "58" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNA" "N" "3" "13" "52" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TNA" BY REFERENCE W-TNA "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JCNC" "X" "3" "75" "6" "D-TNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "4" "0" "115" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MNA" "N" "4" "13" "52" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-MNA" BY REFERENCE WTNA-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TPCC" " " "4" "0" "57" "D-MNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-TPCC" "X" "4" "8" "41" " " "D-TPCC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-TPCC" "X" "4" "49" "16" "01D-TPCC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNOC" "X" "4" "75" "6" "D-TPCC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-DSP" " " "5" "0" "104" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CNA" "N" "5" "13" "52" " " "03C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-CNA" BY REFERENCE TC-NAME "52" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-CNAC" " " "5" "0" "52" "D-CNA" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-CNAC" "X" "5" "13" "40" " " "D-CNAC" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-CNAC" "X" "5" "53" "12" "01D-CNAC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-DSP" " " "6" "0" "34" "03C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNOC" "X" "6" "8" "10" " " "04C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SNA" "N" "6" "26" "12" "D-HNOC" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SNA" BY REFERENCE W-SNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UNA" "N" "6" "46" "12" "D-SNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-UNA" BY REFERENCE W-UNA "12" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "05C-DSP" " " "W-LC1" "0" "61" "04C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JNOC" "X" "W-LC1" "1" "8" " " "05C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-JNOV" "X" "W-LC1" "7" "1" "D-JNOC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-HNA" "N" "W-LC1" "10" "48" "D-JNOV" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SIZD" "X" "W-LC1" "64" "4" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-SIZD" BY REFERENCE W-SIZN "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "06C-DSP" " " "W-LC2" "0" "40" "05C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TRN" "X" "W-LC2" "13" "20" " " "06C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-TRN" BY REFERENCE W-TRN(1) "20" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TRNC" "X" "W-LC2" "13" "20" "D-TRN" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEI" " " "W-LC2" "0" "40" "06C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-SU" "-----9" "W-LC2" "37" "6" " " "D-MEI" RETURNING RESU.
       CALL "SD_From" USING 
            "D-SU" BY REFERENCE W-SU(1) "5" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GT" "ZZZZZZZ" "W-LC2" "44" "7" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-GT" BY REFERENCE W-GT(1) "7" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GKIN" "--,---,---" "W-LC2" "52" "10" "D-GT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GKIN" BY REFERENCE W-GKIN(1) "8" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UT" "ZZZZZZZ" "W-LC2" "63" "7" "D-GKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UT" BY REFERENCE W-UT(1) "7" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-UKIN" "--,---,---" "W-LC2" "71" "10" "D-UT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-UKIN" BY REFERENCE W-UKIN(1) "8" "1" BY REFERENCE W-GN 90
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KEI" " " "21" "0" "26" "D-MEI" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-KEI" "-----9" "21" "37" "6" " " "D-KEI" RETURNING RESU.
       CALL "SD_From" USING 
            "01D-KEI" BY REFERENCE WT-SU "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-KEI" "--,---,---" "21" "52" "10" "01D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "02D-KEI" BY REFERENCE WT-GKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-KEI" "--,---,---" "21" "71" "10" "02D-KEI" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "03D-KEI" BY REFERENCE WT-UKIN "9" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-BDNO" "9" "23" "75" "6" "D-KEI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-BDNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-MEIC" " " "0" "0" "160" "D-BDNO" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-MEIC" " " "W-LC1" "0" "80" " " "D-MEIC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0101D-MEIC" "X" "W-LC1" "1" "40" " " "01D-MEIC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0201D-MEIC" "X" "W-LC1" "41" "40" "0101D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-MEIC" " " "W-LC2" "0" "80" "01D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0102D-MEIC" "X" "W-LC2" "1" "40" " " "02D-MEIC"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "0202D-MEIC" "X" "W-LC2" "41" "40" "0102D-MEIC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PRM" " " "0" "0" "137" "D-MEIC" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01D-PRM" "N" "1" "16" "48" " " "D-PRM" RETURNING RESU.
       CALL "SD_Init" USING 
            "02D-PRM" "X" "6" "26" "12" "01D-PRM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "03D-PRM" "X" "8" "26" "18" "02D-PRM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "04D-PRM" "X" "11" "24" "32" "03D-PRM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "05D-PRM" " " "23" "0" "27" "04D-PRM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "0105D-PRM" "N" "23" "43" "4" " " "05D-PRM" RETURNING RESU.
       CALL "SD_Init" USING 
            "0205D-PRM" "X" "23" "47" "23" "0105D-PRM" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "12C-DSP" " " "13" "0" "48" "D-PRM" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNM" "X" "13" "28" "24" " " "12C-DSP" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNC" "X" "13" "28" "24" "D-DNM" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "640" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "640" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "17" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "18" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "18" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "18" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "17" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "17" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "N" "24" "15" "10" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "18" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "27" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "23" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "24" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "26" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "25" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "18" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "19" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME16" "X" "24" "15" "18" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME17" "X" "24" "15" "22" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME18" "X" "24" "15" "22" "E-ME17" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME19" "X" "24" "15" "21" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME20" "X" "24" "15" "19" "E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME21" "X" "24" "15" "26" "E-ME20" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME22" "X" "24" "15" "18" "E-ME21" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME23" "X" "24" "15" "19" "E-ME22" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME24" "X" "24" "15" "19" "E-ME23" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME25" "X" "24" "15" "27" "E-ME24" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME26" "X" "24" "15" "27" "E-ME25" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME27" "X" "24" "15" "25" "E-ME26" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME28" "X" "24" "15" "22" "E-ME27" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME29" "X" "24" "15" "20" "E-ME28" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME30" "X" "24" "15" "18" "E-ME29" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-KEY" "X" "24" "45" "7" "E-ME30" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-KEY" BY REFERENCE TDI-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-NJZAI" "X" "24" "53" "8" "E-KEY" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-NJZAI" BY REFERENCE NJZAI-KEY "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-JMST" "X" "24" "62" "7" "E-NJZAI" " " RETURNING RESU.
       CALL "SD_From" USING 
            "E-JMST" BY REFERENCE JMSTD-KEY1 "7" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJN05I" RETURNING RESU.
           MOVE ZERO TO W-CRT W-FHCD.
           MOVE SPACE TO W-TEKI W-TNA W-SNA W-UNA.
           CALL "SD_Output" USING "A-THT" A-THT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TTE" A-TTE "p" RETURNING RESU.
      *
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" THTM_PNAME1 "SHARED" BY REFERENCE THTM_IDLST "2"
            "THT-KEY" BY REFERENCE THT-KEY "THT-KEY2" BY REFERENCE
            THT-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
           CALL "DB_F_Open" USING
            "INPUT" JT-RUIJ_PNAME1 "SHARED" BY REFERENCE JT-RUIJ_IDLST
            "1" "RUIJ-KEY" BY REFERENCE RUIJ-KEY.
           CALL "DB_F_Open" USING
            "I-O" JMSTD_PNAME1 "SHARED" BY REFERENCE JMSTD_IDLST "3"
            "JMSTD-KEY1" BY REFERENCE JMSTD-KEY1 "JMSTD-KEY2"
            BY REFERENCE JMSTD-KEY2 "JMSTD-KEY3" BY REFERENCE
            JMSTD-KEY3.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           MOVE W-MSZ TO W-ASZD.
           MOVE W-MSIZ TO W-ASIZD.
           ACCEPT W-NGP FROM DATE.
           MOVE W-NGP TO W-NNGP.
           MOVE W-NGP TO W-DATE.
           SUBTRACT 1 FROM W-GET.
           IF  W-GET = ZERO
               MOVE 12 TO W-GET
               SUBTRACT 1 FROM W-NEN
           END-IF
           MOVE W-DATE TO W-SNGP.
           MOVE W-NGP TO W-DATE.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               MOVE 1 TO W-GET
               ADD 1 TO W-NEN
           END-IF
           MOVE W-DATE TO W-ENGP.
       M-040.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-420
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-420
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF
           IF  W-ACT NOT = 1
               MOVE ZERO TO W-JCN
               CALL "SD_Output" USING "D-JCNC" D-JCNC "p" RETURNING RESU
               GO TO M-060
           END-IF.
       M-050.
           CALL "SD_Accept" USING BY REFERENCE A-JCN "A-JCN" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-050
           END-IF
           IF  W-JCN = ZERO
               CALL "SD_Output" USING "D-JCNC" D-JCNC "p" RETURNING RESU
               GO TO M-080
           END-IF
      *
           MOVE SPACE TO JMSTD-KEY1.
           MOVE W-JCN TO JMSTD-07.
      *           START JMSTD KEY NOT < JMSTD-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-050
           END-IF
      *           READ JMSTD NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-050
           END-IF
           IF  W-JCN NOT = JMSTD-07
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-050
           END-IF
           IF  JMSTD-01 NOT = 0 AND 6
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-050
           END-IF
           IF  JMSTD-04 NOT = 4990 AND 9850 AND 6010
               CALL "SD_Output" USING
                "E-ME28" E-ME28 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-050
           END-IF
           PERFORM JCS-RTN THRU JCS-EX.
           IF  W-TCD NOT = 9850
               MOVE ZERO TO W-TPC
               CALL "SD_Output" USING "D-TPCC" D-TPCC "p" RETURNING RESU
               GO TO M-120
           END-IF
           CALL "SD_Output" USING "D-DNOC" D-DNOC "p" RETURNING RESU.
           GO TO M-100.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-040
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-DNO = ZERO
               GO TO M-060
           END-IF
      *
           PERFORM SET-RTN THRU SET-EX.
      *
           IF  W-INV NOT = 0
               GO TO M-060
           END-IF
           IF  W-UPC NOT = 0
               CALL "SD_Output" USING
                "E-ME21" E-ME21 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-060
           END-IF
           IF  W-ACT = 3
               GO TO M-340
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-TCD "A-TCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-050
               ELSE
                   GO TO M-060
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF
      *
           IF W-TCD NOT = 4990 AND 9850 AND 6010
               GO TO M-080
           END-IF
           MOVE W-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           MOVE TC-NAME TO W-TNA.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           IF  W-TCD NOT = 9850
               MOVE ZERO TO W-TPC
               CALL "SD_Output" USING "D-TPCC" D-TPCC "p" RETURNING RESU
               GO TO M-120
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-TPC "A-TPC" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-JCN NOT = ZERO
                   GO TO M-050
               ELSE
                   GO TO M-080
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           IF  W-TPC = ZERO
               CALL "SD_Output" USING "D-TPCC" D-TPCC "p" RETURNING RESU
               GO TO M-120
           END-IF
      *
           MOVE W-TPC TO WTNA-KEY.
      *           READ WTNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           CALL "SD_Output" USING "D-MNA" D-MNA "p" RETURNING RESU.
           IF  WTNA-OSN = 9
               GO TO M-120
           END-IF
           IF  WTNA-OSN = 0
               MOVE 002 TO W-CCD
           ELSE
               IF  WTNA-OSN = 1
                   MOVE 003 TO W-CCD
               ELSE
                   MOVE 000 TO W-CCD
               END-IF
           END-IF
           MOVE W-TCD TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-100
           END-IF
           CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
           GO TO M-140.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-CCD "A-CCD" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-TCD = 9850
                   GO TO M-100
               ELSE
                   IF  W-JCN NOT = ZERO
                       GO TO M-050
                   ELSE
                       GO TO M-080
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF
           IF  W-TCD NOT = 6010
               IF  W-CCD = 001
                   GO TO M-120
               END-IF
           END-IF
           IF  W-TCD NOT = 6010
               IF  W-CCD = ZERO
                   CALL "SD_Output" USING
                    "D-CNAC" D-CNAC "p" RETURNING RESU
                   GO TO M-140
               END-IF
           END-IF
           MOVE W-TCD TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-120
           END-IF
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
       M-140.
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-140
           END-IF
           IF  W-DATE = ZERO
               MOVE W-NGP TO W-DATE
               CALL "SD_Output" USING
                "A-DATE" A-DATE "p" RETURNING RESU
           END-IF
           IF (W-NEN < 08) OR (W-GET < 01 OR > 12)
                           OR (W-PEY < 01 OR > 31)
               GO TO M-140
           END-IF
           IF  W-DATE < W-SNGP OR > W-ENGP
               GO TO M-140
           END-IF
           IF  W-TCD NOT = 4990
               MOVE SPACE TO W-HNO
               CALL "SD_Output" USING "A-HNO" A-HNO "p" RETURNING RESU
               GO TO M-150
           END-IF.
       M-145.
           CALL "SD_Accept" USING BY REFERENCE A-HNO "A-HNO" "X" "10"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-145
           END-IF.
       M-150.
           CALL "SD_Accept" USING BY REFERENCE A-SOK "A-SOK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-TCD NOT = 4990
                   GO TO M-140
               ELSE
                   GO TO M-145
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-150
           END-IF
      *
           MOVE 3 TO JCON3-01.
           MOVE W-SOK TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "倉庫　なし　" TO JCON3-03
           END-IF
           MOVE JCON3-03 TO W-SNA.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
       M-155.
           CALL "SD_Accept" USING BY REFERENCE A-UNS "A-UNS" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-150
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-155
           END-IF
      *
           MOVE 2 TO JCON2-01.
           MOVE W-UNS TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "運送　なし　" TO JCON2-03
           END-IF
           MOVE JCON2-03 TO W-UNA.
           CALL "SD_Output" USING "D-UNA" D-UNA "p" RETURNING RESU.
      *
           MOVE ZERO TO W-GN W-DC.
           MOVE 7 TO W-LC1.
           CALL "SD_Arg_Match_Line" USING
            "W-LC1" "2" W-LC1 RETURNING RESU.
           MOVE 8 TO W-LC2.
           CALL "SD_Arg_Match_Line" USING
            "W-LC2" "2" W-LC2 RETURNING RESU.
       M-160.
           ADD 1 TO W-GN.
           ADD 2 TO W-LC1 W-LC2.
           CALL "SD_Arg_Match_Line" USING
            "W-LC1" "2" W-LC1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-LC2" "2" W-LC2 RETURNING RESU.
           IF  W-GN = 7
               GO TO M-300
           END-IF
           IF  W-DC NOT = 0
               MOVE ZERO TO W-MEID(W-GN)
               CALL "SD_Output" USING "D-MEIC" D-MEIC "p" RETURNING RESU
               GO TO M-160
           END-IF.
       M-165.
           CALL "SD_Accept" USING BY REFERENCE A-JNO "A-JNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  W-GN > 1
               IF  ESTAT = ADV
                   MOVE ZERO TO W-MEID(W-GN)
                   CALL "SD_Output" USING
                    "D-MEIC" D-MEIC "p" RETURNING RESU
                   MOVE 1 TO W-DC
                   GO TO M-160
               END-IF
           END-IF
           IF  ESTAT = BTB
               GO TO M-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-165
           END-IF
           IF  W-JNO(W-GN) = ZERO
               MOVE ZERO TO W-JGN(W-GN)
               CALL "SD_Output" USING "D-JNOC" D-JNOC "p" RETURNING RESU
               GO TO M-175
           END-IF
           CALL "SD_Output" USING "D-JNOV" D-JNOV "p" RETURNING RESU.
           MOVE SPACE TO JMSTD-KEY1.
           MOVE W-JNO(W-GN) TO JMSTD-07.
      *           START JMSTD KEY NOT < JMSTD-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            JMSTD_PNAME1 "JMSTD-KEY1" " NOT < " JMSTD-KEY1
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-165
           END-IF
      *           READ JMSTD NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" JMSTD_PNAME1 BY REFERENCE JMSTD-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-165
           END-IF
           IF  W-JNO(W-GN) NOT = JMSTD-07
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-165
           END-IF
           IF  JMSTD-01 NOT = 0 AND 6
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-165
           END-IF
           IF  JMSTD-04 NOT = W-TCD
               CALL "SD_Output" USING
                "E-ME20" E-ME20 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-165
           END-IF.
       M-170.
           CALL "SD_Accept" USING BY REFERENCE A-JGN "A-JGN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-165
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-170
           END-IF
           MOVE SPACE TO JMSTD-KEY1.
           MOVE W-JNO(W-GN) TO JMSTD-07.
           MOVE W-JGN(W-GN) TO JMSTD-08.
      *           READ JMSTD WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-165
           END-IF
           IF  JMSTD-01 NOT = 0 AND 6
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-165
           END-IF
           MOVE JMSTD-03 TO W-HCD(W-GN).
           MOVE W-HCD(W-GN) TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
           END-IF
           MOVE 0 TO HI-S(4,10).
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           IF  JMSTD-10 NOT = 001
               IF  JMSTD-10 NOT = W-CCD
                   CALL "SD_Output" USING
                    "E-ME19" E-ME19 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
               END-IF
           END-IF
           IF  W-ACT = 1
               MOVE JMSTD-13 TO W-TEKI
               CALL "SD_Output" USING "A-THT" A-THT "p" RETURNING RESU
               CALL "SD_Output" USING "A-TTE" A-TTE "p" RETURNING RESU
           END-IF
           GO TO M-180.
       M-175.
           IF  W-ACT = 1
               IF  W-FHCD NOT = ZERO
                   MOVE W-FHCD TO W-HCD(W-GN)
                   CALL "SD_Output" USING
                    "A-HCD" A-HCD "p" RETURNING RESU
               END-IF
           END-IF.
       M-180.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-165
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-180
           END-IF
           IF  W-HCD(W-GN) = ZERO
               MOVE W-FHCD TO W-HCD(W-GN)
               CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU
           END-IF
      *
           MOVE W-HCD(W-GN) TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-180
           END-IF
           MOVE HI-ISU TO W-ISU(W-GN).
           MOVE 0 TO HI-S(4,10).
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           IF  W-TCD NOT = 4990 AND 6010
               IF  W-HCD(W-GN) = 999999
                   GO TO M-180
               END-IF
           END-IF
      *
           IF  W-JNO(W-GN) = ZERO
               GO TO M-200
           END-IF
           IF  JMSTD-03 = W-HCD(W-GN)
               GO TO M-200
           END-IF
           MOVE 20 TO RUIJ-01.
           MOVE JMSTD-03 TO RUIJ-02.
           MOVE W-HCD(W-GN) TO RUIJ-03.
      *           READ JT-RUIJ WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID" JT-RUIJ_PNAME1 BY REFERENCE RUIJ-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME16" E-ME16 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-165
           END-IF.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-SZN "A-SZN" "X" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
      *
           MOVE W-SZN(W-GN) TO W-SIZM.
           PERFORM SIZ-RTN THRU SIZ-EX.
           CALL "SD_Output" USING "D-SIZD" D-SIZD "p" RETURNING RESU.
           IF  W-INV NOT = 0
               GO TO M-200
           END-IF
           MOVE W-SCC TO W-SKB(W-GN).
           MOVE W-C TO W-SNO(W-GN).
           IF  W-ACT = 1
               IF  W-JNO(W-GN) NOT = ZERO
                   PERFORM JMS-RTN THRU JMS-EX
               ELSE
                   PERFORM NJS-RTN THRU NJS-EX
               END-IF
           END-IF
           IF  W-INV NOT = 0
               GO TO M-200
           END-IF
           IF (W-ACT = 1) OR ((W-ACT = 2) AND (W-GT(W-GN) = ZERO))
               PERFORM TAN-RTN THRU TAN-EX
           END-IF
           IF  W-TCD = 9850
               MOVE SPACE TO W-TRN(W-GN)
               CALL "SD_Output" USING "D-TRNC" D-TRNC "p" RETURNING RESU
               GO TO M-220
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
      *           SELECT CODEF WHERE 0000 = CODE-TCD AND
      *                              W-HCD(W-GN) = CODE-HCD AND
      *                              W-SCC = CODE-SIZ AND W-C = CODE-SNO.
      *///////////////
           CALL "DB_Select" USING
            CODEF_PNAME1 "WHERE" 
            "CODE-TCD" "=" "0000" "AND"
            "CODE-HCD" "=" W-HCD(W-GN) "AND"
            "CODE-SIZ" "=" W-SCC "AND"
            "CODE-SNO" "=" W-C RETURNING RET.
      *           READ CODEF AT END SCRATCH CODEF
      *///////////////
           CALL "DB_Read" USING
            "AT END" CODEF_PNAME1 BY REFERENCE CODE-R " " RETURNING RET.
           IF  RET = 1
               CALL "DB_Scratch" USING CODEF_PNAME1
               MOVE SPACE TO CODE-JAN CODE-NAME
           END-IF
           MOVE CODE-JAN  TO W-JAN(W-GN).
           MOVE CODE-NAME TO W-TRN(W-GN).
           IF  W-TCD NOT = 4990
               CALL "SD_Output" USING "D-TRNC" D-TRNC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-TRN" D-TRN "p" RETURNING RESU
           END-IF
           IF  W-TCD = 4990
               IF  CODE-NAME = SPACE
                   CALL "SD_Output" USING
                    "E-ME17" E-ME17 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO M-200
               END-IF
           END-IF
           IF  W-TCD = 4990 OR 6010
               IF  W-HCD(W-GN) = 999999
                   MOVE ZERO TO W-SU(W-GN) W-GT(W-GN)
                   CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
                   CALL "SD_Output" USING "D-GT" D-GT "p" RETURNING RESU
                   GO TO M-250
               END-IF
           END-IF.
       M-220.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-220
           END-IF
           IF  W-SU(W-GN) <= ZERO
               GO TO M-220
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
       M-240.
           CALL "SD_Accept" USING BY REFERENCE A-GT "A-GT" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-220
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-240
           END-IF
           IF  W-GT(W-GN) = ZERO
               CALL "SD_Output" USING
                "E-ME30" E-ME30 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           COMPUTE W-GKIN(W-GN) = W-SU(W-GN) * W-GT(W-GN).
           CALL "SD_Output" USING "D-GT" D-GT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
           IF  W-TCD = 4990 OR 6010
               MOVE ZERO TO W-UT(W-GN) W-UKIN(W-GN)
               CALL "SD_Output" USING "D-UT" D-UT "p" RETURNING RESU
               CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU
               GO TO M-270
           END-IF
           GO TO M-260.
       M-250.
           CALL "SD_Output" USING "A-GKIN" A-GKIN "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-GKIN "A-GKIN" "9" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-250
           END-IF
           MOVE ZERO TO W-UT(W-GN) W-UKIN(W-GN)
           CALL "SD_Output" USING "D-GKIN" D-GKIN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UT" D-UT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
           GO TO M-270.
       M-260.
           CALL "SD_Accept" USING BY REFERENCE A-UT "A-UT" "9" "7"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-240
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-260
           END-IF
           IF  W-UT(W-GN) = ZERO
               CALL "SD_Output" USING
                "E-ME30" E-ME30 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           COMPUTE W-UKIN(W-GN) = W-SU(W-GN) * W-UT(W-GN).
           CALL "SD_Output" USING "D-UT" D-UT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UKIN" D-UKIN "p" RETURNING RESU.
       M-270.
           MOVE W-HCD(W-GN) TO W-FHCD.
           GO TO M-160.
       M-280.
           SUBTRACT 1 FROM W-GN.
           SUBTRACT 2 FROM W-LC1 W-LC2.
           CALL "SD_Arg_Match_Line" USING
            "W-LC1" "2" W-LC1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-LC2" "2" W-LC2 RETURNING RESU.
           IF  W-GN = 0
               GO TO M-140
           END-IF
           IF  W-HCD(W-GN) = ZERO
               GO TO M-280
           END-IF
           IF  W-TCD = 4990 OR 6010
               IF  W-HCD(W-GN) = 999999
                   GO TO M-250
               ELSE
                   GO TO M-240
               END-IF
           END-IF
           GO TO M-260.
       M-300.
           MOVE ZERO TO W-GN WT-D W-SUC W-UCHK.
       M-310.
           ADD 1 TO W-GN.
           IF  W-GN = 7
               GO TO M-320
           END-IF
           IF  W-HCD(1) = 999999
               MOVE 1 TO W-UCHK
           END-IF
           IF  W-UCHK = 1
               IF  W-GN NOT = 1
                   IF  W-HCD(W-GN) NOT = 000000 AND 999999
                       MOVE 0 TO W-UCHK
                   END-IF
               END-IF
           END-IF
           ADD W-SU(W-GN) TO WT-SU
           ADD W-GKIN(W-GN) TO WT-GKIN
           ADD W-UKIN(W-GN) TO WT-UKIN
           IF  W-GN = 1
               IF  W-SU(W-GN) < ZERO
                   MOVE 1 TO W-SUC
               ELSE
                   MOVE 0 TO W-SUC
               END-IF
           END-IF
           IF  W-SU(W-GN) < ZERO
               IF  W-SUC = 0
                   CALL "SD_Output" USING
                    "E-ME24" E-ME24 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 7 TO W-GN
                   GO TO M-280
               END-IF
           END-IF
           IF W-SU(W-GN) >= ZERO
               IF W-SUC = 1
                   CALL "SD_Output" USING
                    "E-ME24" E-ME24 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   MOVE 7 TO W-GN
                   GO TO M-280
               END-IF
           END-IF
           GO TO M-310.
       M-320.
           CALL "SD_Output" USING "D-KEI" D-KEI "p" RETURNING RESU.
           IF W-JCN NOT = ZERO
               IF (W-JCN NOT = W-JNO(1)) AND (W-JCN NOT = W-JNO(2)) AND
                  (W-JCN NOT = W-JNO(3)) AND (W-JCN NOT = W-JNO(4)) AND
                  (W-JCN NOT = W-JNO(5)) AND (W-JCN NOT = W-JNO(6))
                   CALL "SD_Output" USING
                    "E-ME28" E-ME28 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO M-280
               END-IF
           END-IF
           MOVE 0 TO W-DC.
       M-325.
           CALL "SD_Accept" USING BY REFERENCE A-THT "A-THT" "N" "18"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-325
           END-IF
           MOVE W-TEKI TO W-ATYD.
           IF  SPACE = W-TY(01) OR W-TY(03) OR W-TY(05) OR W-TY(07)
                   OR W-TY(09) OR W-TY(11) OR W-TY(13) OR W-TY(15)
                   OR W-TY(17)
               GO TO M-325
           END-IF.
       M-330.
           CALL "SD_Accept" USING BY REFERENCE A-TTE "A-TTE" "N" "38"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-325
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-330
           END-IF
           MOVE W-TEKI TO W-ATYD.
           IF  SPACE = W-TY(19) OR W-TY(21) OR W-TY(23) OR W-TY(25)
                   OR W-TY(27) OR W-TY(29) OR W-TY(31) OR W-TY(33)
                   OR W-TY(35) OR W-TY(37) OR W-TY(39) OR W-TY(41)
                   OR W-TY(43) OR W-TY(45) OR W-TY(47) OR W-TY(49)
                   OR W-TY(51) OR W-TY(53) OR W-TY(55)
               GO TO M-330
           END-IF.
       M-340.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-060
               ELSE
                   GO TO M-330
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-340
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-080
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-340
           END-IF
      *
           ACCEPT W-TIMED FROM TIME.
           MOVE ZERO TO W-GN.
           IF  W-ACT NOT = 1
               GO TO M-350
           END-IF
           MOVE "19" TO JCON1-KEY.
      *           READ JCON INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-420
           END-IF
           ADD 1 TO JCON1-04.
           IF  JCON1-04 = ZERO
               ADD 1 TO JCON1-04
           END-IF
      *           REWRITE JCON1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON1-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-420
           END-IF
           MOVE JCON1-04 TO W-DNO.
           GO TO M-380.
       M-350.
           MOVE ZERO TO TDI-KEY.
           MOVE W-DNO TO TDI-DNO.
      *           START TDIF KEY NOT < TDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDIF_PNAME1 "TDI-KEY" " NOT < " TDI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               GO TO M-380
           END-IF.
       M-360.
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-380
           END-IF
           IF  W-DNO NOT = TDI-DNO
               GO TO M-380
           END-IF
           IF  TDI-JNO NOT = ZERO
               PERFORM JMD-RTN THRU JMD-EX
           END-IF
           IF  W-END NOT = 0
               GO TO M-420
           END-IF
           IF  TDI-HCD < 999900
               PERFORM NJD-RTN THRU NJD-EX
           END-IF
           IF  W-END NOT = 0
               GO TO M-420
           END-IF
      *           DELETE TDIF INVALID KEY
      *///////////////
           CALL "DB_Delete" USING TDIF_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-420
           END-IF
           GO TO M-360.
       M-380.
           IF  W-ACT = 3
               GO TO M-400
           END-IF
           ADD 1 TO W-GN.
           IF  W-GN = 7
               GO TO M-400
           END-IF
           IF  W-HCD(W-GN) = ZERO
               GO TO M-400
           END-IF
      *
           IF  W-JNO(W-GN) NOT = ZERO
               PERFORM JMW-RTN THRU JMW-EX
           END-IF
           IF  W-END NOT = 0
               GO TO M-420
           END-IF
           IF  W-HCD(W-GN) < 999900
               PERFORM NJW-RTN THRU NJW-EX
           END-IF
           IF  W-END NOT = 0
               GO TO M-420
           END-IF.
       M-385.
           INITIALIZE TDI-R.
           MOVE W-DNO TO TDI-DNO.
           MOVE W-GN TO TDI-GNO.
           MOVE W-DATE TO TDI-DATE.
           MOVE W-TCD TO TDI-TCD.
           MOVE W-CCD TO TDI-CCD.
           MOVE W-TPC TO TDI-TPC.
           MOVE W-SOK TO TDI-SOK.
           MOVE W-UNS TO TDI-UNS.
           MOVE W-HNO TO TDI-HNO.
           MOVE W-THT TO TDI-THT.
           MOVE W-TTE TO TDI-TTE.
           IF  W-ACT NOT = 1
               MOVE W-PRC TO TDI-PRC
               MOVE W-UPC TO TDI-UPC
           ELSE
               MOVE 0 TO TDI-PRC TDI-UPC
           END-IF
           MOVE W-JNO(W-GN) TO TDI-JNO.
           MOVE W-JGN(W-GN) TO TDI-JGN.
           MOVE W-HCD(W-GN) TO TDI-HCD.
           MOVE W-SZN(W-GN) TO TDI-SIZ.
           MOVE W-SKB(W-GN) TO TDI-SKB.
           MOVE W-SNO(W-GN) TO TDI-SNO.
           MOVE W-SU(W-GN) TO TDI-SU.
           MOVE W-GT(W-GN) TO TDI-GT.
           MOVE W-UT(W-GN) TO TDI-UT.
           MOVE W-GKIN(W-GN) TO TDI-GKIN.
           MOVE W-UKIN(W-GN) TO TDI-UKIN.
           MOVE W-ISU(W-GN) TO TDI-ISU.
           MOVE W-TRN(W-GN) TO TDI-TRN.
           MOVE W-JAN(W-GN) TO TDI-JAN.
           MOVE W-NNGP TO TDI-NNGP.
           MOVE W-TIME TO TDI-NHMS.
           IF  W-UCHK = 1
               MOVE 1 TO TDI-UPC
           END-IF
      *           WRITE TDI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               GO TO M-390
           END-IF
           GO TO M-380.
       M-390.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME11" E-ME11 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-KEY" E-KEY "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = 24
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-420
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           MOVE "TDIF         " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
           GO TO M-385.
       M-400.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SJN05I" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-BDNO" D-BDNO "p" RETURNING RESU.
           IF  W-ACT NOT = 1
               GO TO M-060
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SOK" A-SOK "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UNS" A-UNS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-THT" A-THT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TTE" A-TTE "p" RETURNING RESU.
           IF  W-TCD = 4990
               CALL "SD_Output" USING "A-HNO" A-HNO "p" RETURNING RESU
           END-IF
           IF  W-CCD NOT = ZERO
               CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU
           END-IF
           IF  W-TPC NOT = ZERO
               CALL "SD_Output" USING "A-TPC" A-TPC "p" RETURNING RESU
               CALL "SD_Output" USING "D-MNA" D-MNA "p" RETURNING RESU
           END-IF
           GO TO M-050.
       M-420.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE THTM_IDLST THTM_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JT-RUIJ_IDLST JT-RUIJ_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE JMSTD_IDLST JMSTD_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-PRM" D-PRM "p" RETURNING RESU.
       M-520.
           CALL "SD_Accept" USING BY REFERENCE A-SEN "A-SEN" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF9
               GO TO M-980
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-520
           END-IF
           IF  W-SEN NOT = 1 AND 2
               GO TO M-520
           END-IF.
       M-522.
           CALL "SD_Accept" USING BY REFERENCE A-FNEN "A-FNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-520
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-522
           END-IF.
       M-524.
           CALL "SD_Accept" USING BY REFERENCE A-FGET "A-FGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-522
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-524
           END-IF.
       M-526.
           CALL "SD_Accept" USING BY REFERENCE A-FPEY "A-FPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-524
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-526
           END-IF.
       M-528.
           CALL "SD_Accept" USING BY REFERENCE A-TNEN "A-TNEN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-526
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-528
           END-IF
           IF  W-FNEN > W-TNEN
               GO TO M-528
           END-IF.
       M-530.
           CALL "SD_Accept" USING BY REFERENCE A-TGET "A-TGET" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-528
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-530
           END-IF.
       M-532.
           CALL "SD_Accept" USING BY REFERENCE A-TPEY "A-TPEY" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-530
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-532
           END-IF
           IF  W-FNGP > W-TNGP
               GO TO M-528
           END-IF
           IF  W-SEN = 1
               CALL "SD_Output" USING "D-DNC" D-DNC "p" RETURNING RESU
               GO TO M-580
           END-IF
           CALL "SD_Output" USING "D-DNM" D-DNM "p" RETURNING RESU.
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-DNOF "A-DNOF" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-532
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF.
       M-560.
           CALL "SD_Accept" USING BY REFERENCE A-DNOT "A-DNOT" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-540
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-560
           END-IF
           IF  W-DNOF > W-DNOT
               GO TO M-540
           END-IF.
       M-580.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-SEN = 1
                   GO TO M-532
               ELSE
                   GO TO M-560
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-580
           END-IF
           IF  W-DMM = 9
               GO TO M-520
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-580
           END-IF
      *
           CALL "DB_F_Open" USING
            "I-O" TDIF_PNAME1 "SHARED" BY REFERENCE TDIF_IDLST "1"
            "TDI-KEY" BY REFERENCE TDI-KEY.
       M-600.
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "DB_F_Close" USING
                BY REFERENCE TDIF_IDLST TDIF_PNAME1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-980
           END-IF
           IF  TDI-DATE < W-FNGP OR > W-TNGP
               GO TO M-600
           END-IF
           IF  W-SEN = 1
               IF  TDI-PRC NOT = 0
                   GO TO M-600
               END-IF
           END-IF
           IF  W-SEN = 2
               IF  TDI-PRC = 0
                   GO TO M-600
               ELSE
                   IF  TDI-DNO < W-DNOF OR > W-DNOT
                       GO TO M-600
                   ELSE
                       IF  TDI-UPC NOT = 0
                           GO TO M-600
                       END-IF
                   END-IF
               END-IF
           END-IF
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TC-M_PNAME1 "SHARED" BY REFERENCE TC-M_IDLST "1"
            "TC-KEY" BY REFERENCE TC-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI2-M_PNAME1 "SHARED" BY REFERENCE HI2-M_IDLST "1"
            "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "INPUT" WTNAF_PNAME1 "SHARED" BY REFERENCE WTNAF_IDLST "1"
            "WTNA-KEY" BY REFERENCE WTNA-KEY.
           CALL "DB_F_Open" USING
            "INPUT" CODEF_PNAME1 "SHARED" BY REFERENCE CODEF_IDLST "0".
           CALL "PR_Open" RETURNING RESP.
           MOVE ZERO TO W-PAGE.
           MOVE W-FNGP TO H-FNGP.
           MOVE W-TNGP TO H-TNGP.
           ACCEPT H-DATE FROM DATE.
           PERFORM MID-10 THRU MID-EX.
       M-620.
           MOVE TDI-DNO TO W-DNO.
           MOVE ZERO TO WT-D.
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-TNA P-CNA P-SNA P-UNA P-THT P-TTE.
           MOVE W-DNO TO P-DNO.
           MOVE TDI-DATE TO P-DATE.
           MOVE TDI-TCD TO P-TCD.
           IF  TDI-TPC NOT = ZERO
               IF  TDI-CCD = 2 OR 3
                   MOVE TDI-TPC TO P-TPC
               ELSE
                   IF  TDI-CCD NOT = ZERO AND 001
                       MOVE TDI-CCD TO P-CCD
                   END-IF
               END-IF
           END-IF
           IF  TDI-TPC = ZERO
               IF  TDI-CCD NOT = ZERO AND 001
                   MOVE TDI-CCD TO P-CCD
               END-IF
           END-IF
           IF  TDI-HNO NOT = SPACE AND ZERO
               MOVE TDI-HNO TO P-HNO
           END-IF
           MOVE TDI-THT TO P-THT.
           MOVE TDI-TTE TO P-TTE.
      *
           MOVE 2 TO JCON2-01.
           MOVE TDI-UNS TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "運送　なし　" TO JCON2-03
           END-IF
           MOVE JCON2-03 TO P-UNA.
      *
           MOVE 3 TO JCON3-01.
           MOVE TDI-SOK TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "倉庫　なし　" TO JCON3-03
           END-IF
           MOVE JCON3-03 TO P-SNA.
      *
           MOVE TDI-TCD TO TC-TCD.
           MOVE 1 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "＊　得意先なし　＊" TO TC-NAME
           END-IF
           MOVE TC-NAME TO P-TNA.
      *
           IF  TDI-TPC NOT = ZERO AND 9998 AND 9999
               IF  TDI-CCD = 2 OR 3
                   GO TO M-640
               END-IF
           END-IF
           IF  TDI-CCD = ZERO OR 001
               GO TO M-660
           END-IF
           MOVE TDI-TCD TO TC-TCD.
           MOVE TDI-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "＊　直送先なし　＊" TO TC-NAME
           END-IF
           MOVE TC-NAME TO P-CNA.
           GO TO M-660.
       M-640.
           MOVE TDI-TPC TO WTNA-KEY.
      *           READ WTNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO WTNA-NAME
               MOVE "＊　店名　なし　＊" TO WTNA-NAME
           END-IF
           MOVE WTNA-NAME TO P-CNA.
       M-660.
           PERFORM PR2-RTN THRU PR2-EX.
           IF  W-SEN = 2
               GO TO M-680
           END-IF
           MOVE 1 TO TDI-PRC.
      *           REWRITE TDI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDIF_PNAME1 TDIF_LNAME TDI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       M-680.
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-700
           END-IF
           IF  TDI-DATE < W-FNGP OR > W-TNGP
               GO TO M-680
           END-IF
           IF  W-SEN = 1
               IF  TDI-PRC NOT = 0
                   GO TO M-680
               END-IF
           END-IF
           IF  W-SEN = 2
               IF  TDI-PRC = 0
                   GO TO M-680
               ELSE
                   IF  TDI-DNO < W-DNOF OR > W-DNOT
                       GO TO M-680
                   ELSE
                       IF  TDI-UPC NOT = 0
                           GO TO M-680
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  TDI-DNO = W-DNO
               GO TO M-660
           END-IF
           PERFORM PR3-RTN THRU PR3-EX.
           GO TO M-620.
       M-700.
           PERFORM PR3-RTN THRU PR3-EX.
           CALL "PR_Close" RETURNING RESP.
           CALL "DB_F_Close" USING BY REFERENCE TDIF_IDLST TDIF_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TC-M_IDLST TC-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HI2-M_IDLST HI2-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE WTNAF_IDLST WTNAF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE CODEF_IDLST CODEF_PNAME1.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
      ******************************************************************
       JCS-RTN.
           MOVE JMSTD-04 TO W-TCD.
           MOVE JMSTD-10 TO W-CCD.
           MOVE JMSTD-23 TO W-TPC.
           MOVE JMSTD-13 TO W-TEKI.
           MOVE W-JCN TO W-JNO(1).
           MOVE W-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "得意先　なし" TO TC-NAME
           END-IF
           MOVE TC-NAME TO W-TNA.
      *
           IF  W-TPC = ZERO
               GO TO JCS-10
           END-IF
           MOVE W-TPC TO WTNA-KEY.
      *           READ WTNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO WTNA-NAME
               MOVE 9 TO WTNA-OSN
               MOVE "店名　なし" TO WTNA-NAME
           END-IF.
       JCS-10.
           MOVE W-TCD TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "直送先　なし" TO TC-NAME
           END-IF
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-THT" A-THT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TTE" A-TTE "p" RETURNING RESU.
           IF  W-TPC = ZERO
               CALL "SD_Output" USING "D-TPCC" D-TPCC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TPC" A-TPC "p" RETURNING RESU
               CALL "SD_Output" USING "D-MNA" D-MNA "p" RETURNING RESU
           END-IF
           IF  W-CCD = ZERO OR 001
               CALL "SD_Output" USING "D-CNAC" D-CNAC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU
           END-IF
           IF  W-TCD NOT = 4990
               CALL "SD_Output" USING "D-HNOC" D-HNOC "p" RETURNING RESU
           END-IF
           MOVE 1 TO W-GN.
           MOVE 9 TO W-LC1.
           CALL "SD_Arg_Match_Line" USING
            "W-LC1" "2" W-LC1 RETURNING RESU.
           CALL "SD_Output" USING "A-JNO" A-JNO "p" RETURNING RESU.
       JCS-EX.
           EXIT.
       SET-RTN.
           MOVE ZERO TO TDI-KEY W-INV.
           MOVE W-DNO TO TDI-DNO.
      *           START TDIF KEY NOT < TDI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            TDIF_PNAME1 "TDI-KEY" " NOT < " TDI-KEY RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           IF  W-DNO NOT = TDI-DNO
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
      *
           MOVE TDI-TCD TO W-TCD.
           MOVE TDI-CCD TO W-CCD.
           MOVE TDI-TPC TO W-TPC.
           MOVE TDI-DATE TO W-DATE.
           MOVE TDI-SOK TO W-SOK.
           MOVE TDI-UNS TO W-UNS.
           MOVE TDI-HNO TO W-HNO.
           MOVE TDI-THT TO W-THT.
           MOVE TDI-TTE TO W-TTE.
           MOVE TDI-PRC TO W-PRC.
           MOVE TDI-UPC TO W-UPC.
           MOVE ZERO TO W-MEI W-GN.
       SET-10.
           ADD 1 TO W-GN.
           IF  W-GN > 6
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           IF  W-GN NOT = TDI-GNO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           MOVE TDI-JNO TO W-JNO(W-GN).
           MOVE TDI-JGN TO W-JGN(W-GN).
           MOVE TDI-HCD TO W-HCD(W-GN).
           MOVE TDI-SIZ TO W-SZN(W-GN).
           MOVE TDI-SKB TO W-SKB(W-GN).
           MOVE TDI-SNO TO W-SNO(W-GN).
           MOVE TDI-SU TO W-SU(W-GN).
           MOVE TDI-GT TO W-GT(W-GN).
           MOVE TDI-UT TO W-UT(W-GN).
           MOVE TDI-GKIN TO W-GKIN(W-GN).
           MOVE TDI-UKIN TO W-UKIN(W-GN).
           MOVE TDI-ISU TO W-ISU(W-GN).
           MOVE TDI-TRN TO W-TRN(W-GN).
           MOVE TDI-JAN TO W-JAN(W-GN).
      *
      *           READ TDIF NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" TDIF_PNAME1 BY REFERENCE TDI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-20
           END-IF
           IF  W-DNO NOT = TDI-DNO
               GO TO SET-20
           END-IF
           IF (TDI-TCD NOT = W-TCD) OR (TDI-TPC NOT = W-TPC) OR
              (TDI-DATE NOT = W-DATE) OR (TDI-CCD NOT = W-CCD) OR
              (TDI-HNO NOT = W-HNO)
               MOVE 1 TO W-INV
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME9" E-ME9 "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           GO TO SET-10.
       SET-20.
           MOVE W-TCD TO TC-TCD.
           MOVE 001 TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "得意先　なし" TO TC-NAME
           END-IF
           MOVE TC-NAME TO W-TNA.
      *
           IF  W-TPC = ZERO
               GO TO SET-30
           END-IF
           MOVE W-TPC TO WTNA-KEY.
      *           READ WTNAF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" WTNAF_PNAME1 BY REFERENCE WTNA-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO WTNA-NAME
               MOVE 9 TO WTNA-OSN
               MOVE "店名　なし" TO WTNA-NAME
           END-IF.
       SET-30.
           IF  W-CCD = ZERO
               GO TO SET-40
           END-IF
           MOVE W-TCD TO TC-TCD.
           MOVE W-CCD TO TC-CCD.
      *           READ TC-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TC-M_PNAME1 BY REFERENCE TC-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO TC-NAME
               MOVE "直送先　なし" TO TC-NAME
           END-IF.
       SET-40.
           MOVE 2 TO JCON2-01.
           MOVE W-UNS TO JCON2-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "運送　なし　" TO JCON2-03
           END-IF
           MOVE JCON2-03 TO W-UNA.
      *
           MOVE 3 TO JCON3-01.
           MOVE W-SOK TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "倉庫　なし　" TO JCON3-03
           END-IF
           MOVE JCON3-03 TO W-SNA.
      *
           CALL "SD_Output" USING "A-TCD" A-TCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-TNA" D-TNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-CCD" A-CCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SOK" A-SOK "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UNS" A-UNS "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UNA" D-UNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-THT" A-THT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TTE" A-TTE "p" RETURNING RESU.
           IF  W-TPC = ZERO
               CALL "SD_Output" USING "D-TPCC" D-TPCC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TPC" A-TPC "p" RETURNING RESU
               CALL "SD_Output" USING "D-MNA" D-MNA "p" RETURNING RESU
           END-IF
           IF W-CCD = ZERO OR 001
               CALL "SD_Output" USING "D-CNAC" D-CNAC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-CNA" D-CNA "p" RETURNING RESU
           END-IF
           IF W-TCD NOT = 4990
               CALL "SD_Output" USING "D-HNOC" D-HNOC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-HNO" A-HNO "p" RETURNING RESU
           END-IF
           MOVE ZERO TO W-GN WT-D.
           MOVE 7 TO W-LC1.
           CALL "SD_Arg_Match_Line" USING
            "W-LC1" "2" W-LC1 RETURNING RESU.
           MOVE 8 TO W-LC2.
           CALL "SD_Arg_Match_Line" USING
            "W-LC2" "2" W-LC2 RETURNING RESU.
       SET-50.
           ADD 1 TO W-GN.
           IF  W-GN = 7
               CALL "SD_Output" USING "D-KEI" D-KEI "p" RETURNING RESU
               GO TO SET-EX
           END-IF
           ADD 2 TO W-LC1 W-LC2.
           CALL "SD_Arg_Match_Line" USING
            "W-LC1" "2" W-LC1 RETURNING RESU.
           CALL "SD_Arg_Match_Line" USING
            "W-LC2" "2" W-LC2 RETURNING RESU.
           IF  W-HCD(W-GN) = ZERO
               CALL "SD_Output" USING "D-MEIC" D-MEIC "p" RETURNING RESU
               GO TO SET-50
           END-IF
           IF  W-JNO(W-GN) = ZERO
               CALL "SD_Output" USING "D-JNOC" D-JNOC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-JNO" A-JNO "p" RETURNING RESU
               CALL "SD_Output" USING "A-JGN" A-JGN "p" RETURNING RESU
           END-IF
      *
           MOVE W-HCD(W-GN) TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "品名　なし" TO HI-NAME
           END-IF
           MOVE 0 TO HI-S(4,10).
      *
           MOVE W-SZN(W-GN) TO W-SIZM.
           PERFORM SIZ-RTN THRU SIZ-EX.
      *
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SZN" A-SZN "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SIZD" D-SIZD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-MEI" D-MEI "p" RETURNING RESU.
           IF  W-INV = 1
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF  W-TCD = 4990
               CALL "SD_Output" USING "D-TRN" D-TRN "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-TRNC" D-TRNC "p" RETURNING RESU
           END-IF
      *
           ADD W-SU(W-GN) TO WT-SU.
           ADD W-GKIN(W-GN) TO WT-GKIN.
           ADD W-UKIN(W-GN) TO WT-UKIN.
           GO TO SET-50.
       SET-EX.
           EXIT.
       SIZ-RTN.
           MOVE SPACE TO W-SIZN.
           MOVE ZERO TO W-SC W-INV.
       SIZ-10.
           ADD 1 TO W-SC.
           IF  W-SC = 6
               MOVE 5 TO W-SC
               MOVE 10 TO W-C
               GO TO SIZ-30
           END-IF
           MOVE ZERO TO W-C.
       SIZ-20.
           ADD 1 TO W-C.
           IF  W-C = 11
               GO TO SIZ-10
           END-IF
           IF  W-SIZM NOT = W-SZ(W-SC,W-C)
               GO TO SIZ-20
           END-IF.
       SIZ-30.
           COMPUTE W-SCC = W-SC - 1.
           IF  W-SCC = 0
               MOVE 1 TO W-SCC
           END-IF
           IF  HI-S(W-SCC,W-C) = 0
               IF (W-SIZM = 240 OR 245 OR 250) AND (W-SCC = 3)
                   GO TO SIZ-10
               ELSE
                   MOVE 1 TO W-INV
                   CALL "SD_Output" USING
                    "E-ME6" E-ME6 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO SIZ-EX
               END-IF
           END-IF
           MOVE W-SIZ(W-SC,W-C) TO W-SIZN.
           IF  W-SC = 1
               IF (HI-SS2 NOT = ZERO) OR (HI-SS3 NOT = ZERO)
                                      OR (HI-SS4 NOT = ZERO)
                   MOVE 1 TO W-INV
                   CALL "SD_Output" USING
                    "E-ME6" E-ME6 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO SIZ-EX
               END-IF
           END-IF
           IF W-SC = 2
               IF (HI-SS2 = ZERO) AND (HI-SS3 = ZERO)
                                  AND (HI-SS4 = ZERO)
                   MOVE 1 TO W-INV
                   CALL "SD_Output" USING
                    "E-ME6" E-ME6 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME98" E-ME98 "p" RETURNING RESU
                   GO TO SIZ-EX
               END-IF
           END-IF.
       SIZ-EX.
           EXIT.
       TAN-RTN.
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD(W-GN) TO THT-HCD.
           MOVE W-SCC TO THT-SIZ.
      *           READ THTM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO TAN-10
           END-IF
           GO TO TAN-20.
       TAN-10.
           MOVE W-TCD TO THT-TCD.
           MOVE W-HCD(W-GN) TO THT-HCD.
           MOVE 9 TO THT-SIZ.
      *           READ THTM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" THTM_PNAME1 BY REFERENCE THT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME23" E-ME23 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE ZERO TO THT-T THT-TT
           END-IF.
       TAN-20.
           MOVE THT-T TO W-GT(W-GN).
           MOVE THT-TT TO W-UT(W-GN).
           IF  W-JCN NOT = ZERO
               IF  JMSTD-17 NOT = ZERO
                   MOVE JMSTD-17 TO W-GT(W-GN)
               END-IF
           END-IF
           CALL "SD_Output" USING "D-GT" D-GT "p" RETURNING RESU.
           CALL "SD_Output" USING "D-UT" D-UT "p" RETURNING RESU.
       TAN-EX.
           EXIT.
       JMS-RTN.
           MOVE 0 TO W-INV.
           IF  W-SCC NOT = JMSTD-09
               CALL "SD_Output" USING
                "E-ME18" E-ME18 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               MOVE 1 TO W-INV
           ELSE
               COMPUTE W-SU(W-GN) = JMSTD-1111(W-C) - JMSTD-1211(W-C)
                                  - JMSTD-141(W-C)  - JMSTD-151(W-C)
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
           END-IF.
       JMS-EX.
           EXIT.
       NJS-RTN.
           MOVE 0 TO W-INV.
           MOVE SPACE TO NJZAI-KEY.
           MOVE W-SOK TO NJZAI-01.
           MOVE W-HCD(W-GN) TO NJZAI-02.
           MOVE W-SKB(W-GN) TO NJZAI-03.
      *           READ NJZAI WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO NJZAI-0411(W-C) NJZAI-0511(W-C)
                            NJZAI-0611(W-C) NJZAI-0711(W-C)
                            NJZAI-0811(W-C) NJZAI-0911(W-C)
                            NJZAI-1111(W-C)
           END-IF
           COMPUTE W-SU(W-GN) = NJZAI-0411(W-C) - NJZAI-0511(W-C)
                              + NJZAI-0611(W-C) + NJZAI-0711(W-C)
                              - NJZAI-0811(W-C) - NJZAI-0911(W-C)
                              + NJZAI-1111(W-C).
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
       NJS-EX.
           EXIT.
       JMW-RTN.
           MOVE SPACE TO JMSTD-KEY1.
           MOVE W-JNO(W-GN) TO JMSTD-07.
           MOVE W-JGN(W-GN) TO JMSTD-08.
      *           READ JMSTD INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JMST" E-JMST "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO JMW-EX
           END-IF
           MOVE W-SNO(W-GN) TO CNT.
           ADD W-SU(W-GN) TO JMSTD-151(CNT).
      *           REWRITE JMSTD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME25" E-ME25 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JMST" E-JMST "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       JMW-EX.
           EXIT.
       JMD-RTN.
           MOVE SPACE TO JMSTD-KEY1.
           MOVE TDI-JNO TO JMSTD-07.
           MOVE TDI-JGN TO JMSTD-08.
      *           READ JMSTD INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JMSTD_PNAME1 BY REFERENCE JMSTD-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME14" E-ME14 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JMST" E-JMST "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO JMD-EX
           END-IF
           SUBTRACT TDI-SU FROM JMSTD-151(TDI-SNO).
      *           REWRITE JMSTD-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JMSTD_PNAME1 JMSTD_LNAME JMSTD-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME25" E-ME25 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-JMST" E-JMST "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF.
       JMD-EX.
           EXIT.
       NJW-RTN.
           INITIALIZE NJZAI-R.
           MOVE W-SOK TO NJZAI-01.
           MOVE W-HCD(W-GN) TO NJZAI-02.
           MOVE W-SKB(W-GN) TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJW-10
           END-IF
           MOVE W-SNO(W-GN) TO CNT.
           ADD W-SU(W-GN) TO NJZAI-0911(CNT).
      *           REWRITE NJZAI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJW-EX
           END-IF
           GO TO NJW-30.
       NJW-10.
           MOVE W-SNO(W-GN) TO CNT.
           ADD W-SU(W-GN) TO NJZAI-0911(CNT).
      *           WRITE NJZAI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO TO NJW-20
           END-IF
           GO TO NJW-30.
       NJW-20.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME27" E-ME27 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-NJZAI" E-NJZAI "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = 24
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJW-EX
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           MOVE "NJZAI        " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           GO TO NJW-RTN.
       NJW-30.
           INITIALIZE NJZAI-R.
           MOVE 9 TO NJZAI-01.
           MOVE W-HCD(W-GN) TO NJZAI-02.
           MOVE W-SKB(W-GN) TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJW-40
           END-IF
           MOVE W-SNO(W-GN) TO CNT.
           ADD W-SU(W-GN) TO NJZAI-0911(CNT).
      *           REWRITE NJZAI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           GO TO NJW-EX.
       NJW-40.
           MOVE W-SNO(W-GN) TO CNT.
           ADD W-SU(W-GN) TO NJZAI-0911(CNT).
      *           WRITE NJZAI-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               GO TO NJW-50
           END-IF
           GO TO NJW-EX.
       NJW-50.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME27" E-ME27 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-NJZAI" E-NJZAI "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = 24
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJW-EX
           END-IF
           CALL "DB_F_Close" USING
            BY REFERENCE NJZAI_IDLST NJZAI_PNAME1.
           MOVE "NJZAI        " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" NJZAI_PNAME1 "SHARED" BY REFERENCE NJZAI_IDLST "1"
            "NJZAI-KEY" BY REFERENCE NJZAI-KEY.
           GO TO NJW-30.
       NJW-EX.
           EXIT.
       NJD-RTN.
           MOVE SPACE TO NJZAI-KEY.
           MOVE TDI-SOK TO NJZAI-01.
           MOVE TDI-HCD TO NJZAI-02.
           MOVE TDI-SKB TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJD-EX
           END-IF
           SUBTRACT TDI-SU FROM NJZAI-0911(TDI-SNO).
      *           REWRITE NJZAI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJD-EX
           END-IF
      *
           MOVE SPACE TO NJZAI-KEY.
           MOVE 9 TO NJZAI-01.
           MOVE TDI-HCD TO NJZAI-02.
           MOVE TDI-SKB TO NJZAI-03.
      *           READ NJZAI INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NJZAI_PNAME1 BY REFERENCE NJZAI-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME22" E-ME22 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJD-EX
           END-IF
           SUBTRACT TDI-SU FROM NJZAI-0911(TDI-SNO).
      *           REWRITE NJZAI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NJZAI_PNAME1 NJZAI_LNAME NJZAI-R RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-END
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME26" E-ME26 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-NJZAI" E-NJZAI "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO NJD-EX
           END-IF.
       NJD-EX.
           EXIT.
       MID-RTN.
           MOVE SPACE TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           CALL "PR_NewPage" RETURNING RESP.
       MID-10.
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
       PR1-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PR1-EX.
           EXIT.
       PR2-RTN.
           MOVE TDI-HCD TO HI-MHCD HI-HCD.
      *           READ HI2-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI2-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊　品名なし　＊" TO HI-NAME
           END-IF
           MOVE 0 TO HI-S(4,10).
           IF  TDI-GNO = 1
               PERFORM PR1-RTN THRU PR1-EX
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA
           MOVE "-" TO P-V1.
           MOVE TDI-GNO TO P-GNO.
           IF  TDI-JNO NOT = ZERO
               MOVE TDI-JNO TO P-JNO
               MOVE "-" TO P-V2
               MOVE TDI-JGN TO P-JGN
           END-IF
           MOVE TDI-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           MOVE TDI-SIZ TO W-SIZM.
           PERFORM SIZ-RTN THRU SIZ-EX.
           MOVE W-SIZN TO P-SIZ.
           MOVE TDI-SU TO P-SU.
           MOVE TDI-GT TO P-GT.
           MOVE TDI-GKIN TO P-GKIN.
           IF  ZERO NOT = TDI-UT AND TDI-UKIN
               MOVE TDI-UT TO P-UT
               MOVE TDI-UKIN TO P-UKIN
           END-IF
           IF  TDI-TCD = 4990
               MOVE TDI-TRN TO P-TMS
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 64
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
      *
           ADD TDI-SU TO WT-SU.
           ADD TDI-GKIN TO WT-GKIN.
           ADD TDI-UKIN TO WT-UKIN.
       PR2-EX.
           EXIT.
       PR3-RTN.
           MOVE WT-SU TO P-TSU.
           MOVE WT-GKIN TO P-TGKIN.
           MOVE WT-UKIN TO P-TUKIN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PR3-EX.
           EXIT.
