       IDENTIFICATION DIVISION.
       PROGRAM-ID. HMN110.
      *********************************************************
      *    PROGRAM         :  実棚卸入力　　　　              *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCHN11                          *
      *        変更　　　  :  94/04/04                        *
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
           02  F              PIC  X(039) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　履物　棚卸伝票　入力リスト　　＊＊＊".
           02  F              PIC  X(028) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  N(002) VALUE "倉庫".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票№　".
           02  F              PIC  X(008) VALUE "  ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(002) VALUE "入数".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(076) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(037) VALUE SPACE.
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
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(028) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(028) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(035) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(037) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(016) VALUE SPACE.
           02  F              PIC  N(004) VALUE "ケース計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "足数合計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　総合計".
       01  W-P1.
           02  F              PIC  X(001).
           02  P-SNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-DNO1         PIC  9(002).
           02  P-V            PIC  X(001).
           02  P-DNO2         PIC  X(004).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-F            PIC  X(001).
           02  P-ISU          PIC ZZZ.
           02  P-R            PIC  X(001).
           02  F              PIC  X(076).
       01  W-P2.
           02  F              PIC  X(028).
           02  P-SM           PIC  N(006).
           02  P-SIZ          PIC  9(001).
           02  P-SUD.
             03  P-SU         PIC ---,--9  OCCURS  10.
           02  P-KST          PIC ----,--9.
           02  P-SST          PIC --,---,--9.
           02  P-ASU          PIC --,---,--9.
       01  W-RD.
           02  W-SUD1.
             03  W-SU1        PIC S9(006)  OCCURS  10.
           02  W-SUD2.
             03  W-SU2        PIC S9(006)  OCCURS  10.
           02  W-SUD3.
             03  W-SU3        PIC S9(006)  OCCURS  10.
           02  W-SUD4.
             03  W-SU4        PIC S9(006)  OCCURS  10.
           02  W-SUD5.
             03  W-SU5        PIC S9(006)  OCCURS  10.
           02  W-SUD6.
             03  W-SU6        PIC S9(006)  OCCURS  10.
           02  W-SUD7.
             03  W-SU7        PIC S9(006)  OCCURS  10.
           02  W-SUD8.
             03  W-SU8        PIC S9(006)  OCCURS  10.
           02  W-SUG.
             03  W-KST        PIC S9(006).
             03  W-SST        PIC S9(007).
             03  W-HST        PIC S9(007).
             03  W-ASU        PIC S9(007).
       01  W-SUD.
           02  W-SU           PIC S9(006)  OCCURS  10.
       01  W-SSD.
           02  W-SS           PIC  9(001)  OCCURS  10.
       01  W-DATA.
           02  W-SNO          PIC  9(001).
           02  W-SN           PIC  N(006).
           02  W-DNO          PIC  X(006).
           02  W-DNOD  REDEFINES W-DNO.
             03  W-DNO1       PIC  9(002).
             03  W-DNO2.
               04  W-DNO21    PIC  9(003).
               04  W-DNO22    PIC  X(001).
           02  W-HCD          PIC  9(006) VALUE ZERO.
           02  W-PAGE         PIC  9(003) VALUE ZERO.
           02  CNT            PIC  9(002).
           02  W-L            PIC  9(002).
           02  W-C            PIC  9(002).
           02  W-SC           PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-SUF          PIC S9(006).
           02  CHK.
             03  CHK1         PIC  9(001).
             03  CHK2         PIC  9(001).
           02  W-ACT          PIC  9(001) VALUE ZERO.
           02  W-DMM          PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-SE.
             03  W-SDNO       PIC  X(006).
             03  W-EDNO       PIC  X(006) VALUE "999999".
           02  W-POC          PIC  9(001) VALUE ZERO.
           02  W-TC           PIC  9(001).
           02  W-TCD          PIC  9(001).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY L-JCON.
           COPY LIHIM.
           COPY LIHTIM.
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
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　履物　棚卸伝票　入力リスト　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊　　　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(023) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(036) VALUE
                "作表  :  する=1  しない=9 ..... ﾘﾀｰﾝ".
           02  FILLER  PIC  X(024) VALUE
                "伝票№         ～ 999999".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-SNO   PIC  9(001).
             03  A-DNO1  PIC  9(002).
             03  A-DNO2  PIC  X(004).
             03  A-HCD   PIC  9(006).
           02  A-SU    PIC S9(005).
           02  A-PC    PIC  9(001).
           02  FILLER.
             03  A-SDNO  PIC  X(006).
             03  A-EDNO  PIC  X(006).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  FILLER.
             03  D-SN    PIC  N(006).
             03  D-NAME  PIC  N(024).
           02  D-ISU   PIC ZZ9.
           02  FILLER.
             03  D-SU    PIC ZZZZ9-.
             03  S-SU    PIC  X(006) VALUE "      ".
           02  D-SUG.
             03  D-KT.
               04  01D-KT  PIC  ZZZZZ9-.
               04  02D-KT  PIC ZZZZZZ9-.
             03  D-AT.
               04  01D-AT  PIC ZZZZZZ9-.
               04  02D-AT  PIC Z,ZZZ,ZZ9-.
       01  C-ERR.
           02  FILLER.
             03  E-STAT  PIC  X(002).
             03  E-ME1   PIC  X(025) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***       ".
             03  E-ME2   PIC  X(025) VALUE
                  "***  ﾐﾄｳﾛｸ  ***          ".
             03  E-ME3   PIC  X(025) VALUE
                  "***  ｿｳｺ ﾅｼ  ***         ".
             03  E-ME4   PIC  X(025) VALUE
                  "***  HIM ﾅｼ  ***         ".
             03  E-ME5   PIC  X(025) VALUE
                  "***  DATA ZERO  ***      ".
             03  E-ME8   PIC  X(025) VALUE
                  "***  ｲﾘｽｳ ﾅｼ  ***        ".
             03  E-ME11  PIC  X(025) VALUE
                  "***  WRITE ｴﾗｰ  ***      ".
             03  E-ME12  PIC  X(025) VALUE
                  "***  REWRITE ｴﾗｰ  ***    ".
             03  E-ME13  PIC  X(025) VALUE
                  "***  DELETE ｴﾗｰ  ***     ".
             03  E-ME78  PIC  N(002) VALUE "連絡".
             03  E-ME98  PIC  X(005) VALUE X"1B4A05".
             03  E-ME99  PIC  X(005) VALUE X"1B4205".
             03  E-KEY   PIC  X(007).
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
           "01C-CLEAR" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-MID
       CALL "SD_Init" USING
           "C-MID" " " "0" "0" "312" " " " " RETURNING RESU.
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
           "06C-MID" "X" "12" "15" "36" "05C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "07C-MID" "X" "15" "21" "24" "06C-MID" " " RETURNING RESU.
       CALL "SD_Init" USING
           "08C-MID" "X" "23" "40" "22" "07C-MID" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
           "C-ACP" " " "0" "0" "33" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-ACT" "9" "1" "60" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ACP" " " "3" "0" "13" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SNO" "9" "3" "2" "1" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SNO" BY REFERENCE W-SNO "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DNO1" "9" "3" "17" "2" "A-SNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DNO1" BY REFERENCE W-DNO1 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DNO2" "X" "3" "20" "4" "A-DNO1" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DNO2" BY REFERENCE W-DNO2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-HCD" "9" "3" "25" "6" "A-DNO2" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-HCD" BY REFERENCE W-HCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-SU" "S9" "W-L" "W-C" "5" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-SU" BY REFERENCE W-SU(1) "6" "1" BY REFERENCE CNT 6
            RETURNING RESU.
       CALL "SD_Init" USING
           "A-PC" "9" "12" "46" "1" "A-SU" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-PC" BY REFERENCE W-PC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-ACP" " " "15" "0" "12" "A-PC" " " RETURNING RESU.
       CALL "SD_Init" USING
           "A-SDNO" "X" "15" "29" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
           "A-SDNO" BY REFERENCE W-SDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-EDNO" "X" "15" "39" "6" "A-SDNO" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-EDNO" BY REFERENCE W-EDNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "A-DMM" "9" "23" "57" "1" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
           "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
           "C-DSP" " " "0" "0" "111" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-DSP" " " "3" "0" "63" " " "C-DSP" RETURNING RESU.
       CALL "SD_Init" USING
           "D-SN" "N" "3" "4" "12" " " "01C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-SN" BY REFERENCE W-SN "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-NAME" "N" "3" "32" "48" "D-SN" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-NAME" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-ISU" "ZZ9" "4" "76" "3" "01C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING
           "D-ISU" BY REFERENCE HI-ISU "3" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02C-DSP" " " "W-L" "0" "12" "D-ISU" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SU" "ZZZZ9-" "W-L" "W-C" "6" " " "02C-DSP" RETURNING RESU.
       CALL "SD_From" USING
           "D-SU" BY REFERENCE W-SU(1) "6" "1" BY REFERENCE CNT 6
           RETURNING RESU.
       CALL "SD_Init" USING
           "S-SU" "X" "W-L" "W-C" "6" "D-SU" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-SUG" " " "0" "0" "33" "02C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
           "D-KT" " " "0" "0" "15" " " "D-SUG" RETURNING RESU.
       CALL "SD_Init" USING
           "01D-KT" "ZZZZZ9-" "10" "73" "7" " " "D-KT" RETURNING RESU.
       CALL "SD_From" USING
           "01D-KT" BY REFERENCE W-KST "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
         "02D-KT" "ZZZZZZ9-" "12" "72" "8" "01D-KT" " " RETURNING RESU.
       CALL "SD_From" USING
         "02D-KT" BY REFERENCE W-SST "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "D-AT" " " "0" "0" "18" "D-KT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01D-AT" "ZZZZZZ9-" "21" "72" "8" " " "D-AT" RETURNING RESU.
       CALL "SD_From" USING
           "01D-AT" BY REFERENCE W-HST "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "02D-AT" "Z,ZZZ,ZZ9-" "22" "70" "10" "01D-AT" " "
           RETURNING RESU.
       CALL "SD_From" USING
           "02D-AT" BY REFERENCE W-ASU "7" "0" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
           "C-ERR" " " "0" "0" "328" " " " " RETURNING RESU.
       CALL "SD_Init" USING
           "01C-ERR" " " "24" "0" "328" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
           "E-STAT" "X" "24" "10" "2" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
           "E-STAT" BY REFERENCE ERR-STAT "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME1" "X" "24" "15" "25" "E-STAT" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME2" "X" "24" "15" "25" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME3" "X" "24" "15" "25" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME4" "X" "24" "15" "25" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME5" "X" "24" "15" "25" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME8" "X" "24" "15" "25" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME11" "X" "24" "15" "25" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME12" "X" "24" "15" "25" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME13" "X" "24" "15" "25" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME78" "N" "24" "5" "4" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME98" "X" "24" "75" "5" "E-ME78" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-ME99" "X" "24" "75" "5" "E-ME98" " " RETURNING RESU.
       CALL "SD_Init" USING
           "E-KEY" "X" "24" "45" "7" "E-ME99" " " RETURNING RESU.
       CALL "SD_From" USING
           "E-KEY" BY REFERENCE HTI-KEY "7" "0" RETURNING RESU.
       CALL "SD_Init" USING
           "E-CL" " " "24" "0" "80" "E-KEY" " " RETURNING RESU.
       CALL "SD_Init" USING
           "01E-CL" "X" "24" "1" "40" " " "E-CL" RETURNING RESU.
       CALL "SD_Init" USING
           "02E-CL" "X" "24" "41" "40" "01E-CL" " " RETURNING RESU.
      *
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-020.
           COPY LIBCPR.
           MOVE DATE-02R TO H-DATE.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
       M-040.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "SD_Screen_Output" USING "SCHN11" RETURNING RESU.
           IF  W-ACT NOT = 0
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
           END-IF.
       M-060.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-060
           END-IF
           IF  W-ACT = 9
               CALL "DB_F_Close" USING
                BY REFERENCE HTI-M_IDLST HTI-M_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE HI-M_IDLST HI-M_PNAME1
               CALL "SD_Output" USING
                "C-CLEAR" C-CLEAR "p" RETURNING RESU
               CALL "SD_Output" USING "C-MID" C-MID "p" RETURNING RESU
               GO TO M-700
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-060
           END-IF
           IF  W-ACT = 2 OR 3
               GO TO M-120
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-SNO "A-SNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 2
                   GO TO M-160
               ELSE
                   GO TO M-060
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF
           MOVE SPACE TO W-SN.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE 3 TO JCON3-01.
           MOVE W-SNO TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               GO TO M-100
           END-IF
           MOVE JCON3-03 TO W-SN.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           CALL "SD_Output" USING "D-SN" D-SN "p" RETURNING RESU.
           IF  W-ACT = 2
               GO TO M-200
           END-IF.
       M-120.
           CALL "SD_Accept" USING BY REFERENCE A-DNO1 "A-DNO1" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO M-100
               ELSE
                   GO TO M-060
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-120
           END-IF.
       M-140.
           CALL "SD_Screen_Output" USING "SCHN11" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DNO1" A-DNO1 "p" RETURNING RESU.
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "A-SNO" A-SNO "p" RETURNING RESU
               CALL "SD_Output" USING
                "D-SN" D-SN "p" RETURNING RESU
           END-IF.
       M-160.
           CALL "SD_Accept" USING BY REFERENCE A-DNO2 "A-DNO2" "X" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-160
           END-IF
           IF  W-DNO21 NOT NUMERIC
               GO TO M-160
           END-IF
      *
           MOVE SPACE TO HTI-KEY.
           MOVE W-DNO TO HTI-DNO.
      *           START HTI-M KEY NOT < HTI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HTI-M_PNAME1 "HTI-KEY" "NOT <" HTI-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-180
           END-IF
           IF  HTI-DNO NOT = W-DNO
               GO TO M-180
           END-IF
      *
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-KST NOT = ZERO
               IF  HI-ISU = ZERO
                   CALL "SD_Output" USING
                    "E-ME8" E-ME8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-160
               END-IF
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-140
           END-IF
           IF  W-ACT = 3
               GO TO M-500
           END-IF
           GO TO M-100.
       M-180.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-160
           END-IF
           MOVE ZERO TO W-RD.
       M-200.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 2
                   GO TO M-100
               ELSE
                   GO TO M-160
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-200
           END-IF
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME4" E-ME4 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-200
           END-IF
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ISU" D-ISU "p" RETURNING RESU.
           IF  W-HCD > 999899
               GO TO M-200
           END-IF.
       M-220.
           MOVE ZERO TO W-SC CHK.
       M-240.
           ADD 1 TO W-SC.
           IF  W-SC NOT = 5
               GO TO M-250
           END-IF
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-KST NOT = ZERO
               IF  HI-ISU = ZERO
                   CALL "SD_Output" USING
                    "E-ME8" E-ME8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-220
               END-IF
           END-IF.
       M-250.
           IF  W-SC = 9
               GO TO M-420
           END-IF
           MOVE ZERO TO CHK1.
           PERFORM FST-RTN THRU FST-EX.
       M-260.
           MOVE 2 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       M-280.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-300
           END-IF
           IF  W-SS(CNT) = 0
               MOVE ZERO TO W-SU(CNT)
               CALL "SD_Output" USING
                "S-SU" S-SU "p" RETURNING RESU
               GO TO M-340
           END-IF
           IF  9 = CHK1 OR CHK2
               MOVE ZERO TO W-SU(CNT)
               CALL "SD_Output" USING
                "D-SU" D-SU "p" RETURNING RESU
               GO TO M-340
           END-IF
           GO TO M-320.
       M-300.
           PERFORM RST-RTN THRU RST-EX.
           GO TO M-240.
       M-320.
           MOVE W-SU(CNT) TO W-SUF.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "5"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-360
           END-IF
           IF  ESTAT = PF6
               MOVE 9 TO CHK1
               GO TO M-260
           END-IF
           IF  ESTAT = ADV
               MOVE 9 TO CHK2
               MOVE ZERO TO W-SU(CNT)
               CALL "SD_Output" USING
                "D-SU" D-SU "p" RETURNING RESU
               GO TO M-340
           END-IF
           IF  ESTAT = PF7
               GO TO M-400
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-320
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
       M-340.
           ADD 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-280.
       M-360.
           SUBTRACT 1 FROM CNT.
           IF  CNT = ZERO
               GO TO M-380
           END-IF
           SUBTRACT 7 FROM W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-SS(CNT) = 0
               GO TO M-360
           END-IF
           GO TO M-320.
       M-380.
           PERFORM RST-RTN THRU RST-EX.
           SUBTRACT 1 FROM W-SC.
           IF  W-SC = ZERO
               GO TO M-200
           END-IF
           PERFORM FST-RTN THRU FST-EX.
           MOVE 11 TO CNT.
           MOVE 72 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO M-360.
       M-400.
           MOVE W-SUF TO W-SU(CNT).
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           PERFORM RST-RTN THRU RST-EX.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-KST NOT = ZERO
               IF  HI-ISU = ZERO
                   CALL "SD_Output" USING
                    "E-ME8" E-ME8 "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   GO TO M-220
               END-IF
           END-IF
           IF  W-SC > 4
               GO TO M-220
           ELSE
               MOVE ZERO TO CHK
               MOVE 4 TO W-SC
               GO TO M-240
           END-IF.
       M-420.
           PERFORM KEI-RTN THRU KEI-EX.
           IF  W-ASU = ZERO
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO M-220
           END-IF.
       M-500.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO M-160
               ELSE
                   MOVE ZERO TO CHK
                   MOVE 8 TO W-SC
                   MOVE 72 TO W-C
                   CALL "SD_Arg_Match_Col" USING
                    "W-C" "2" W-C RETURNING RESU
                   GO TO M-360
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-500
           END-IF
           IF  W-DMM = 9
               IF  W-ACT = 3
                   GO TO M-160
               ELSE
                   GO TO M-220
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-500
           END-IF
      *
           IF  W-ACT = 3
               GO TO M-620
           END-IF
           MOVE ZERO TO W-SC.
       M-520.
           ADD 1 TO W-SC.
           IF  W-SC = 9
               GO TO M-140
           END-IF
           MOVE ZERO TO W-SUD CNT W-ZC.
           IF  W-SC = 1
               MOVE W-SUD1 TO W-SUD
           END-IF
           IF  W-SC = 2
               MOVE W-SUD2 TO W-SUD
           END-IF
           IF  W-SC = 3
               MOVE W-SUD3 TO W-SUD
           END-IF
           IF  W-SC = 4
               MOVE W-SUD4 TO W-SUD
           END-IF
           IF  W-SC = 5
               MOVE W-SUD5 TO W-SUD
           END-IF
           IF  W-SC = 6
               MOVE W-SUD6 TO W-SUD
           END-IF
           IF  W-SC = 7
               MOVE W-SUD7 TO W-SUD
           END-IF
           IF  W-SC = 8
               MOVE W-SUD8 TO W-SUD
           END-IF.
       M-540.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-560
           END-IF
           IF  W-SU(CNT) = ZERO
               GO TO M-540
           END-IF
           MOVE 5 TO W-ZC.
       M-560.
           MOVE W-DNO TO HTI-DNO.
           MOVE W-SC TO HTI-GNO.
      *           READ HTI-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-600
           END-IF
      *
           IF  W-ZC = 0
               GO TO M-580
           END-IF
           MOVE W-HCD TO HTI-HCD.
           IF  HTI-GNO > 4
               COMPUTE HTI-SIZ = HTI-GNO - 3
           ELSE
               COMPUTE HTI-SIZ = HTI-GNO + 1
           END-IF
           IF  HTI-SIZ = 5
               MOVE 1 TO HTI-SIZ
           END-IF
           MOVE W-SUD TO HTI-SUD.
           MOVE W-SNO TO HTI-SNO.
           MOVE HI-BC TO HTI-BC.
           MOVE HI-ISU TO HTI-ISU.
      *           REWRITE HTI-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HTI-M_PNAME1 HTI-M_LNAME HTI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           GO TO M-520.
       M-580.
      *           DELETE HTI-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HTI-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           GO TO M-520.
       M-600.
           IF  W-ZC = ZERO
               GO TO M-520
           END-IF
           INITIALIZE HTI-R.
           MOVE W-DNO TO HTI-DNO.
           MOVE W-SC TO HTI-GNO.
           MOVE W-HCD TO HTI-HCD.
           IF  HTI-GNO > 4
               COMPUTE HTI-SIZ = HTI-GNO - 3
           ELSE
               COMPUTE HTI-SIZ = HTI-GNO + 1
           END-IF
           IF  HTI-SIZ = 5
               MOVE 1 TO HTI-SIZ
           END-IF
           MOVE W-SUD TO HTI-SUD.
           MOVE W-SNO TO HTI-SNO.
           MOVE HI-BC TO HTI-BC.
           MOVE HI-ISU TO HTI-ISU.
      *           WRITE HTI-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            HTI-M_PNAME1 HTI-M_LNAME HTI-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           GO TO M-520.
       M-620.
           MOVE SPACE TO HTI-KEY.
           MOVE W-DNO TO HTI-DNO.
      *           START HTI-M KEY NOT < HTI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HTI-M_PNAME1 "HTI-KEY" "NOT <" HTI-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-040
           END-IF.
       M-640.
      *           READ HTI-M NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-040
           END-IF
           IF  HTI-DNO NOT = W-DNO
               GO TO M-040
           END-IF
      *           DELETE HTI-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HTI-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-KEY" E-KEY "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           GO TO M-640.
       M-700.
           CALL "SD_Accept" USING BY REFERENCE A-PC "A-PC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-700
           END-IF
           IF  W-PC = 9
               GO TO M-760
           END-IF
           IF  W-PC NOT = 1
               GO TO M-700
           END-IF.
       M-720.
           CALL "SD_Accept" USING BY REFERENCE A-SDNO "A-SDNO" "X" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-700
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-720
           END-IF.
       M-740.
           CALL "SD_Accept" USING BY REFERENCE A-EDNO "A-EDNO" "X" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-720
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-740
           END-IF
           IF  W-SDNO > W-EDNO
               GO TO M-740
           END-IF.
       M-760.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-PC = 9
                   GO TO M-700
               ELSE
                   GO TO M-740
               END-IF
           END-IF
           IF  ESTAT = HTB AND SKP
               GO TO M-760
           END-IF
           IF  W-DMM = 9
               GO TO M-700
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-760
           END-IF
           IF  W-PC = 9
               GO TO M-980
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" HTI-M_PNAME1 "SHARED" BY REFERENCE HTI-M_IDLST "1"
            "HTI-KEY" BY REFERENCE HTI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           MOVE SPACE TO HTI-KEY.
           MOVE W-SDNO TO HTI-DNO.
      *           START HTI-M KEY NOT < HTI-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HTI-M_PNAME1 "HTI-KEY" "NOT <" HTI-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           IF  HTI-DNO > W-EDNO
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-960
           END-IF
           MOVE 5 TO W-POC.
           CALL "PR_Open" RETURNING RESP.
           PERFORM MID-010 THRU MID-EX.
       M-780.
           MOVE HTI-SNO TO W-SNO.
           MOVE ZERO TO CHK.
       M-800.
           MOVE HTI-DNO TO W-DNO.
           MOVE ZERO TO W-SUG.
           MOVE HTI-HCD TO W-HCD.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HI-ISU
               MOVE SPACE TO HI-NAME
               MOVE ALL "1" TO HI-ASSD
               MOVE "　＊＊　マスター　なし　＊＊　　　" TO HI-NAME
           END-IF
           PERFORM PR1-RTN THRU PR1-EX.
       M-810.
           IF  HTI-GNO < 5
               MOVE 1 TO W-TC
           ELSE
               MOVE 2 TO W-TC
           END-IF
           MOVE 0 TO W-TCD.
       M-820.
           MOVE ZERO TO W-SSD.
           IF  HTI-SIZ = 1
               MOVE HI-SS1 TO W-SSD
           END-IF
           IF  HTI-SIZ = 2
               MOVE HI-SS2 TO W-SSD
           END-IF
           IF  HTI-SIZ = 3
               MOVE HI-SS3 TO W-SSD
           END-IF
           IF  HTI-SIZ = 4
               MOVE HI-SS4 TO W-SSD
           END-IF
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-SM.
           IF  W-TC NOT = W-TCD
               IF  W-TC = 1
                   MOVE "［ケース］" TO P-SM
               ELSE
                   MOVE "［端　数］" TO P-SM
               END-IF
           END-IF
           MOVE HTI-SIZ TO P-SIZ.
           MOVE ZERO TO CNT.
       M-840.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO M-860
           END-IF
           IF  HTI-GNO = 2 OR 6
               IF  CNT > 9
                   GO TO M-840
               END-IF
           END-IF
           IF  HTI-GNO = 3 OR 7
               IF  CNT > 8
                   GO TO M-840
               END-IF
           END-IF
           IF (HTI-SU(CNT) NOT = ZERO) OR (W-SS(CNT) NOT = ZERO)
               MOVE HTI-SU(CNT) TO P-SU(CNT)
           END-IF
           IF  HTI-GNO < 5
               ADD HTI-SU(CNT) TO W-KST
           ELSE
               ADD HTI-SU(CNT) TO W-HST
           END-IF
           GO TO M-840.
       M-860.
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO M-900
           END-IF
           IF  HTI-DNO > W-EDNO
               GO TO M-900
           END-IF
           IF  HTI-GNO < 5
               MOVE 1 TO W-TCD
           ELSE
               MOVE 2 TO W-TCD
           END-IF
           IF  HTI-SNO = W-SNO
               IF  HTI-DNO = W-DNO
                   IF  W-TCD = W-TC
                       GO TO M-880
                   END-IF
               END-IF
           END-IF
           IF  W-TCD NOT = W-TC
               IF  W-TC = 1
                   COMPUTE W-SST = W-KST * HI-ISU
                   MOVE W-KST TO P-KST
                   MOVE W-SST TO P-SST
               ELSE
                   MOVE W-HST TO P-SST
               END-IF
           END-IF
           IF  HTI-DNO = W-DNO
               GO TO M-880
           END-IF
           IF  W-TC = 1
               COMPUTE W-SST = W-KST * HI-ISU
               MOVE W-KST TO P-KST
               MOVE W-SST TO P-SST
           ELSE
               MOVE W-HST TO P-SST
           END-IF
           COMPUTE W-ASU = W-SST + W-HST.
           MOVE W-ASU TO P-ASU.
       M-880.
           PERFORM PR2-RTN THRU PR2-EX.
           IF  HTI-SNO NOT = W-SNO
               GO TO M-780
           END-IF
           IF  HTI-DNO NOT = W-DNO
               GO TO M-800
           END-IF
           IF  W-TCD NOT = W-TC
               GO TO M-810
           END-IF
           GO TO M-820.
       M-900.
           IF  W-TC = 1
               COMPUTE W-SST = W-KST * HI-ISU
               MOVE W-KST TO P-KST
               MOVE W-SST TO P-SST
           ELSE
               MOVE W-HST TO P-SST
           END-IF
           COMPUTE W-ASU = W-SST + W-HST.
           MOVE W-ASU TO P-ASU.
           PERFORM PR2-RTN THRU PR2-EX.
       M-960.
           CALL "DB_F_Close" USING
            BY REFERENCE HTI-M_IDLST HTI-M_PNAME1.
           IF  W-POC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-980.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
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
       PR1-RTN.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NAME.
           IF  CHK1 = 0
               MOVE 5 TO CHK1
               MOVE W-SNO TO P-SNO
           END-IF
           MOVE W-DNO1 TO P-DNO1.
           MOVE "-" TO P-V.
           MOVE W-DNO2 TO P-DNO2.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           MOVE "(" TO P-F.
           MOVE HI-ISU TO P-ISU.
           MOVE ")" TO P-R.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 59
               MOVE W-SNO TO P-SNO
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PR1-EX.
           EXIT.
       PR2-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 60
               GO TO PR2-010
           END-IF
           PERFORM MID-RTN THRU MID-EX.
           MOVE W-SNO TO P-SNO.
           MOVE W-DNO1 TO P-DNO1.
           MOVE "-" TO P-V.
           MOVE W-DNO2 TO P-DNO2.
           MOVE W-HCD TO P-HCD.
           MOVE HI-NAME TO P-NAME.
           IF  W-TC = 1
               MOVE "［ケース］" TO P-SM
           ELSE
               MOVE "［端　数］" TO P-SM
           END-IF.
       PR2-010.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PR2-EX.
           EXIT.
       DSP-RTN.
           MOVE HTI-SNO TO W-SNO.
           MOVE HTI-HCD TO W-HCD.
           MOVE SPACE TO W-SN.
           CALL "DB_F_Open" USING
            "INPUT" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           MOVE 3 TO JCON3-01.
           MOVE W-SNO TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "倉庫なし　　" TO JCON3-03.
           MOVE JCON3-03 TO W-SN.
           CALL "DB_F_Close" USING BY REFERENCE JCON_IDLST JCON_PNAME1.
           MOVE W-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               INITIALIZE HI-R
           END-IF
           CALL "SD_Output" USING "A-SNO" A-SNO "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SN" D-SN "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-NAME" D-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "D-ISU" D-ISU "p" RETURNING RESU.
           MOVE ZERO TO W-RD.
       DSP-010.
           IF  HTI-GNO = 1
               MOVE HTI-SUD TO W-SUD1
           END-IF
           IF  HTI-GNO = 2
               MOVE HTI-SUD TO W-SUD2
           END-IF
           IF  HTI-GNO = 3
               MOVE HTI-SUD TO W-SUD3
           END-IF
           IF  HTI-GNO = 4
               MOVE HTI-SUD TO W-SUD4
           END-IF
           IF  HTI-GNO = 5
               MOVE HTI-SUD TO W-SUD5
           END-IF
           IF  HTI-GNO = 6
               MOVE HTI-SUD TO W-SUD6
           END-IF
           IF  HTI-GNO = 7
               MOVE HTI-SUD TO W-SUD7
           END-IF
           IF  HTI-GNO = 8
               MOVE HTI-SUD TO W-SUD8
           END-IF
      *           READ HTI-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HTI-M_PNAME1 BY REFERENCE HTI-R
            "UNLOCK" RETURNING RET.
           IF  RET = 1
               GO TO DSP-020
           END-IF
           IF  HTI-DNO = W-DNO
               GO TO DSP-010
           END-IF.
       DSP-020.
           MOVE ZERO TO W-SC.
       DSP-030.
           ADD 1 TO W-SC.
           IF  W-SC = 9
               COMPUTE W-SST = W-KST * HI-ISU
               COMPUTE W-ASU = W-SST + W-HST
               CALL "SD_Output" USING "D-SUG" D-SUG "p" RETURNING RESU
               GO TO DSP-EX
           END-IF
           PERFORM FST-RTN THRU FST-EX.
           MOVE 2 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           MOVE ZERO TO CNT.
       DSP-040.
           ADD 1 TO CNT.
           IF  CNT = 11
               GO TO DSP-030
           END-IF
           IF  W-SS(CNT) = 0
               MOVE ZERO TO W-SU(CNT)
               CALL "SD_Output" USING "S-SU" S-SU "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               IF  W-SC < 5
                   ADD W-SU(CNT) TO W-KST
               ELSE
                   ADD W-SU(CNT) TO W-HST
               END-IF
           END-IF
           ADD 7 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO DSP-040.
       DSP-EX.
           EXIT.
       FST-RTN.
           MOVE ZERO TO W-SUD W-SSD.
           IF  W-SC = 1 OR 5
               MOVE HI-SS2 TO W-SSD
               IF  W-SC = 1
                   MOVE W-SUD1 TO W-SUD
                   MOVE  6 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               ELSE
                   MOVE W-SUD5 TO W-SUD
                   MOVE 15 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               END-IF
           END-IF
           IF  W-SC = 2 OR 6
               MOVE HI-SS3 TO W-SSD
               IF  W-SC = 2
                   MOVE W-SUD2 TO W-SUD
                   MOVE  8 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               ELSE
                   MOVE W-SUD6 TO W-SUD
                   MOVE 17 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               END-IF
           END-IF
           IF  W-SC = 3 OR 7
               MOVE HI-SS4 TO W-SSD
               MOVE 0 TO W-SS(10)
               IF  W-SC = 3
                   MOVE W-SUD3 TO W-SUD
                   MOVE 10 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               ELSE
                   MOVE W-SUD7 TO W-SUD
                   MOVE 19 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               END-IF
           END-IF
           IF  W-SC = 4 OR 8
               MOVE HI-SS1 TO W-SSD
               IF  W-SC = 4
                   MOVE W-SUD4 TO W-SUD
                   MOVE 12 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               ELSE
                   MOVE W-SUD8 TO W-SUD
                   MOVE 21 TO W-L
                   CALL "SD_Arg_Match_Line" USING
                    "W-L" "2" W-L RETURNING RESU
               END-IF
           END-IF.
       FST-EX.
           EXIT.
       RST-RTN.
           IF  W-SC = 1
               MOVE W-SUD TO W-SUD1
           END-IF
           IF  W-SC = 2
               MOVE W-SUD TO W-SUD2
           END-IF
           IF  W-SC = 3
               MOVE W-SUD TO W-SUD3
           END-IF
           IF  W-SC = 4
               MOVE W-SUD TO W-SUD4
           END-IF
           IF  W-SC = 5
               MOVE W-SUD TO W-SUD5
           END-IF
           IF  W-SC = 6
               MOVE W-SUD TO W-SUD6
           END-IF
           IF  W-SC = 7
               MOVE W-SUD TO W-SUD7
           END-IF
           IF  W-SC = 8
               MOVE W-SUD TO W-SUD8
           END-IF.
       RST-EX.
           EXIT.
       KEI-RTN.
           MOVE ZERO TO W-SUG CNT.
       KEI-010.
           ADD 1 TO CNT.
           IF  CNT = 11
               COMPUTE W-SST = W-KST * HI-ISU
               COMPUTE W-ASU = W-SST + W-HST
               CALL "SD_Output" USING "D-SUG" D-SUG "p" RETURNING RESU
               GO TO KEI-EX
           END-IF
           ADD W-SU1(CNT) TO W-KST.
           ADD W-SU2(CNT) TO W-KST.
           ADD W-SU3(CNT) TO W-KST.
           ADD W-SU4(CNT) TO W-KST.
           ADD W-SU5(CNT) TO W-HST.
           ADD W-SU6(CNT) TO W-HST.
           ADD W-SU7(CNT) TO W-HST.
           ADD W-SU8(CNT) TO W-HST.
           GO TO KEI-010.
       KEI-EX.
           EXIT.
