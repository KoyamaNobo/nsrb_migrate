       IDENTIFICATION DIVISION.
       PROGRAM-ID. KHM010.
      *********************************************************
      *    PROGRAM         :  工品品名マスターメンテナンス    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCKM01                          *
      *    DATA WRITTN     :  57/05/13                        *
      *        変更　　　  :  62/03/24                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  メンテナンス=0 , 問合せ･作表=1  *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE     SECTION.
       77  ERR-STAT           PIC  X(002).
       77  JS-SIGN            PIC  9(001).
       01  HEAD1.
           02  F              PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(035) VALUE SPACE.
           02  F              PIC  N(023) VALUE
                "＊＊＊　　工品品名マスター　プルーフリスト　　".
           02  F              PIC  N(003) VALUE "＊＊＊".
           02  F              PIC  X(026) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC ZZ.
       01  HEAD2.
           02  F              PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(006) VALUE "品　　　　名".
           02  F              PIC  X(011) VALUE SPACE.
           02  F              PIC  N(002) VALUE "用途".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "入庫".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "取数".
           02  F              PIC  X(005) VALUE SPACE.
           02  F              PIC  N(004) VALUE "売　　価".
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(004) VALUE "　補　用".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "原　　価".
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(004) VALUE "　補　用".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "予定原価".
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(004) VALUE "　補　用".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  N(004) VALUE "　登録日".
           02  F              PIC  N(004) VALUE "　廃止日".
           02  F              PIC  X(031) VALUE SPACE.
       01  W-P1.
           02  P-KEY          PIC  X(005).
           02  F              PIC  X(001).
           02  P-NAME         PIC  X(020).
           02  F              PIC  X(001).
           02  P-YC           PIC  9(002).
           02  F              PIC  X(003).
           02  P-NC           PIC  9(001).
           02  P-TRS          PIC  Z(004).
           02  P-T1           PIC ZZZZ,ZZ9.99.
           02  P-F1           PIC  X(001).
           02  P-T2           PIC ZZ9.99.
           02  P-R1           PIC  X(001).
           02  P-GT1          PIC ZZZ,ZZ9.99.
           02  P-F2           PIC  X(001).
           02  P-GT2          PIC ZZ9.99.
           02  P-R2           PIC  X(001).
           02  P-YGT1         PIC ZZZ,ZZ9.99.
           02  P-F3           PIC  X(001).
           02  P-YGT2         PIC ZZ9.99.
           02  P-R3           PIC  X(001).
           02  F              PIC  X(001).
           02  P-ADD          PIC 99/99.
           02  F              PIC  X(001).
           02  P-DNG          PIC 99/99.
           02  F              PIC  X(001).
           02  P-KNA          PIC  N(020).
       01  W-R.
           02  W-HCD          PIC  X(005).
           02  W-KEY  REDEFINES W-HCD.                                  ｺｰﾄﾞ
             03  W-KEY1       PIC  X(002).
             03  W-KEY2       PIC  9(003).
           02  W-NAME         PIC  X(020).                              ﾋﾝﾒｲ
           02  W-YC           PIC  9(002).                              ﾖｳﾄｸﾌﾞﾝ
           02  W-TN.
             03  W-TGM        PIC  9(004)V9(02).                        ｺﾞﾑﾀﾝｶ
             03  W-TKN        PIC  9(004)V9(02).                        ｶﾅｸﾞﾀﾝｶ
             03  W-TSZ        PIC  9(002)V9(02).                        ｾｯﾁｬｸﾀﾝｶ
           02  W-YGT1         PIC  9(006)V9(02).
           02  W-YGT2         PIC  9(006)V9(02).
           02  W-MKR          PIC  9(002)V9(03).
           02  W-KKH          PIC  9(004)V9(02).                        ｶｺｳﾋ
           02  W-SID.
             03  W-SBB        PIC  9(002)V9(02).                        ﾊﾞﾘﾊﾞﾌ
             03  W-STS        PIC  9(002)V9(02).                        ﾄｿｳ
             03  W-SNE        PIC  9(002)V9(02).                        ﾈｼﾞ
             03  W-SKP        PIC  9(002)V9(02).                        ｺﾝﾎﾟｳ
             03  W-SKY        PIC  9(002)V9(02).                        ｷｬｯﾌﾟ
             03  W-SMK        PIC  9(002)V9(02).                        ﾏｰｸ
             03  W-SPK        PIC  9(002)V9(02).
             03  W-SKG        PIC  9(002)V9(02).                        ｹﾞｰｼﾞ
             03  W-SAN        PIC  9(002)V9(02).                        ｼﾎﾞﾘ
             03  W-SET        PIC  9(002)V9(02).                        -----
             03  W-SST        PIC  9(003)V9(02).                        ｿﾉﾀ
           02  W-DRH          PIC  9(003)V9(02).
           02  W-KPS          PIC  9(003)V9(02).
           02  W-SKH          PIC  9(002)V9(02).
           02  W-SHY          PIC  9(002)V9(02).                        ﾎﾖｳﾋﾝ
           02  W-T1           PIC  9(006)V9(02).                        ﾊﾞｲｶA
           02  W-T2           PIC  9(006)V9(02).                        ﾊﾞｲｶB
           02  W-KC.
             03  W-KIS        PIC  9(001).                              ｷｼｭｸﾌﾞﾝ
             03  W-SYS        PIC  9(003).                              ｼｮｯﾄｽｳ
             03  W-TRS        PIC  9(002).                              ﾄﾘｺｽｳ
             03  W-MS         PIC  9(001).                              ﾒﾝｽｳ
             03  W-KCO        PIC  X(005).                              ｶﾘｭｳｺｰﾄﾞ
           02  W-USG          PIC  9(004)V9(02).
           02  W-NC           PIC  9(001).
           02  W-GT1          PIC  9(006)V9(02).
           02  W-GT2          PIC  9(006)V9(02).
           02  W-KNA          PIC  N(024).
           02  W-DNG          PIC  9(004).
           02  W-DNGD  REDEFINES W-DNG.
             03  W-DNEN       PIC  9(002).
             03  W-DGET       PIC  9(002).
           02  F              PIC  X(006).
           02  W-ENG          PIC  9(004).
           02  W-ADD          PIC  9(004).
           02  W-ADDD  REDEFINES W-ADD.                                 ﾄｳﾛｸ
             03  W-ADN        PIC  9(002).                                ﾈﾝ
             03  W-ADG        PIC  9(002).                                ﾂｷ
           02  W-COD          PIC  9(006).
           02  W-CODD  REDEFINES W-COD.                                 ﾀﾝｶｼｭｳｾｲ
             03  W-CON        PIC  9(002).                                ﾈﾝ
             03  W-COG        PIC  9(002).                                ﾂｷ
             03  W-COP        PIC  9(002).                                ﾋ
       01  W-D.
           02  W-ACT          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-GT1B         PIC  9(006)V9(02).
           02  W-SU           PIC S9(006)V9(02).
           02  W-PC           PIC  9(001) VALUE ZERO.
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-HCDSE.
             03  W-HCDS       PIC  X(005).
             03  W-HCDE       PIC  X(005).
           02  W-NCN          PIC  N(003).
           02  W-FILE         PIC  X(013).
           COPY LSTAT.
           COPY LIBFDD.
           COPY LIKHM.
           COPY LIKHTM.
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
           02  C-CL  PIC  X(12)  VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT     PIC  9(001).
           02  FILLER.
             03  A-KEY     PIC  X(005).
             03  A-NAME    PIC  X(020).
           02  A-KNA     PIC  N(024).
           02  A-YC      PIC  9(002).
           02  A-NC      PIC  9(001).
           02  A-T1      PIC  9(006)V9(02).
           02  A-T2      PIC  9(006)V9(02).
           02  FILLER.
             03  A-GT1     PIC  9(006)V9(02).
             03  A-YGT1    PIC  9(006)V9(02).
           02  FILLER.
             03  A-GT2     PIC  9(006)V9(02).
             03  A-YGT2    PIC  9(006)V9(02).
           02  A-TRS       PIC  9(002).
           02  A-ADD       PIC  9(004).
           02  A-DNG       PIC  9(004).
           02  FILLER.
             03  A-HCDS    PIC  X(005).
             03  A-HCDE    PIC  X(005).
           02  A-DMM     PIC  9(001).
       01  C-DSP.
           02  D-YCN     PIC  N(016).
           02  D-NCN     PIC  N(003).
           02  D-T1      PIC ZZZZZ9.99.
           02  D-T2      PIC ZZZZZ9.99.
           02  FILLER.
             03  D-GT1     PIC ZZZZZ9.99.
             03  D-YGT1    PIC ZZZZZ9.99.
           02  FILLER.
             03  D-GT2     PIC ZZZZZ9.99.
             03  D-YGT2    PIC ZZZZZ9.99.
           02  D-TRS     PIC  Z(002).
           02  D-ADDS    PIC  X(004)  VALUE "    ".
           02  D-DNGS    PIC  X(004)  VALUE "    ".
           02  D-PM      PIC  X(027) VALUE
                "< 品名ｺｰﾄﾞ       ～ 99999 >". 
       01  C-ERR.
           02  FILLER.
             03  E-ME1     PIC  X(018) VALUE
                  "***  ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME2     PIC  X(017) VALUE
                  "***  DATA ﾅｼ  ***".
             03  E-ME3     PIC  X(022) VALUE
                  "***  KHM ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME4     PIC  X(016) VALUE
                  "***  KHM ﾅｼ  ***".
             03  E-ME5     PIC  X(023) VALUE
                  "***  KHTM ﾄｳﾛｸ ｽﾞﾐ  ***".
             03  E-ME6     PIC  X(017) VALUE
                  "***  KHTM ﾅｼ  ***".
             03  E-ME7     PIC  N(013) VALUE
                  "＊＊＊　キャンセル　＊＊＊".
             03  E-ME8     PIC  X(018) VALUE
                  "***  DATA ｴﾗｰ  ***".
             03  E-ME10    PIC  X(023) VALUE
                  "***  KHM WRITE ｴﾗｰ  ***".
             03  E-ME11    PIC  X(025) VALUE
                  "***  KHM REWRITE ｴﾗｰ  ***".
             03  E-ME12    PIC  X(024) VALUE
                  "***  KHM DELETE ｴﾗｰ  ***".
             03  E-ME13    PIC  X(024) VALUE
                  "***  KHTM WRITE ｴﾗｰ  ***".
             03  E-ME14    PIC  X(026) VALUE
                  "***  KHTM REWRITE ｴﾗｰ  ***".
             03  E-ME15    PIC  X(025) VALUE
                  "***  KHTM DELETE ｴﾗｰ  ***".
             03  E-ME16    PIC  X(033) VALUE
                  "***  ﾄｳｹｲ ｺｳﾓｸ ｶﾞ ZERO ﾃﾞ ﾅｲ  ***".
             03  E-ME18    PIC  X(016) VALUE
                  "***  ﾖｳﾄ ﾅｼ  ***".
             03  E-ME19.
               04  FILLER  PIC  N(011) VALUE
                    "廃棄品以外は原価０不可".
               04  FILLER  PIC  X(005) VALUE X"1B4210".
             03  E-ME20    PIC  X(032) VALUE
                  "***  在庫有り　単価修正不可  ***".
           COPY LSSEM.
           COPY LIBSCR.
       PROCEDURE DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *C-CLEAR
       CALL "SD_Init" USING 
            "C-CLEAR" " " "0" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "C-CL" "X" "1" "0" "12" " " "C-CLEAR" RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "146" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "60" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ACP" " " "5" "0" "25" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KEY" "X" "5" "19" "5" " " "01C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-KEY" BY REFERENCE W-HCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "X" "5" "30" "20" "A-KEY" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-NAME" BY REFERENCE W-NAME "20" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KNA" "N" "6" "30" "48" "01C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-KNA" BY REFERENCE W-KNA "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-YC" "9" "8" "19" "2" "A-KNA" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-YC" BY REFERENCE W-YC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NC" "9" "9" "19" "1" "A-YC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-NC" BY REFERENCE W-NC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T1" "999999V99" "10" "19" "8" "A-NC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-T1" BY REFERENCE W-T1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-T2" "999999V99" "11" "19" "8" "A-T1" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-T2" BY REFERENCE W-T2 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-ACP" " " "12" "0" "16" "A-T2" " " RETURNING RESU.
       CALL "SD_Init" USING 
        "A-GT1" "999999V99" "12" "19" "8" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-GT1" BY REFERENCE W-GT1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
        "A-YGT1" "999999V99" "12" "38" "8" "A-GT1" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-YGT1" BY REFERENCE W-YGT1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "13" "0" "16" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING 
        "A-GT2" "999999V99" "13" "19" "8" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-GT2" BY REFERENCE W-GT2 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
        "A-YGT2" "999999V99" "13" "38" "8" "A-GT2" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-YGT2" BY REFERENCE W-YGT2 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TRS" "9" "14" "23" "2" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-TRS" BY REFERENCE W-TRS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ADD" "9" "15" "19" "4" "A-TRS" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-ADD" BY REFERENCE W-ADD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DNG" "9" "16" "19" "4" "A-ADD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DNG" BY REFERENCE W-DNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "04C-ACP" " " "18" "0" "10" "A-DNG" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDS" "X" "18" "34" "5" " " "04C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCDS" BY REFERENCE W-HCDS "5" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-HCDE" "X" "18" "43" "5" "A-HCDS" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCDE" BY REFERENCE W-HCDE "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "22" "48" "1" "04C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "129" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YCN" "N" "8" "22" "32" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-YCN" BY REFERENCE KKB-YCN "32" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-NCN" "N" "9" "22" "6" "D-YCN" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-NCN" BY REFERENCE W-NCN "6" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T1" "ZZZZZ9.99" "10" "19" "9" "D-NCN" " " 
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-T1" BY REFERENCE W-T1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-T2" "ZZZZZ9.99" "11" "19" "9" "D-T1" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-T2" BY REFERENCE W-T2 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-DSP" " " "12" "0" "18" "D-T2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GT1" "ZZZZZ9.99" "12" "19" "9" " " "01C-DSP" 
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GT1" BY REFERENCE W-GT1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YGT1" "ZZZZZ9.99" "12" "38" "9" "D-GT1" " " 
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-YGT1" BY REFERENCE W-YGT1 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "02C-DSP" " " "13" "0" "18" "01C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-GT2" "ZZZZZ9.99" "13" "19" "9" " " "02C-DSP" 
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-GT2" BY REFERENCE W-GT2 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-YGT2" "ZZZZZ9.99" "13" "38" "9" "D-GT2" " " 
            RETURNING RESU.
       CALL "SD_From" USING 
            "D-YGT2" BY REFERENCE W-YGT2 "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TRS" "Z" "14" "23" "2" "02C-DSP" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-TRS" BY REFERENCE W-TRS "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-ADDS" "X" "15" "19" "4" "D-TRS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-DNGS" "X" "16" "19" "4" "D-ADDS" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-PM" "X" "18" "23" "27" "D-DNGS" " " RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "412" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "412" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME1" "X" "24" "15" "18" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME2" "X" "24" "15" "17" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME3" "X" "24" "15" "22" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME4" "X" "24" "15" "16" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME5" "X" "24" "15" "23" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME6" "X" "24" "15" "17" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME7" "X" "24" "1" "26" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME8" "X" "24" "15" "18" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME10" "X" "24" "15" "23" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME11" "X" "24" "15" "25" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME12" "X" "24" "15" "24" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME13" "X" "24" "15" "24" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME14" "X" "24" "15" "26" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME15" "X" "24" "15" "25" "E-ME14" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME16" "X" "24" "15" "33" "E-ME15" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME18" "X" "24" "15" "16" "E-ME16" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME19" " " "24" "0" "27" "E-ME18" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01E-ME19" "N" "24" "15" "22" " " "E-ME19" RETURNING RESU.
       CALL "SD_Init" USING
            "02E-ME19" "X" "24" "75" "5" "01E-ME19" " " RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME20" "X" "24" "15" "32" "E-ME19" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-010.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 1
	           CALL "DB_Close"
               STOP RUN
           END-IF.
           COPY LIBCPR.
           IF  JS-SIGN = 1
               CALL "DB_F_Open" USING
                "INPUT" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
                "KH-KEY" BY REFERENCE KH-KEY
           ELSE
               CALL "DB_F_Open" USING
                "I-O" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
                "KH-KEY" BY REFERENCE KH-KEY
               CALL "DB_F_Open" USING
                "I-O" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "2"
                "KHT-KEY" BY REFERENCE KHT-KEY "KHT-KEYD" BY REFERENCE
                KHT-KEYD
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" KKB-M_PNAME1 "SHARED" BY REFERENCE KKB-M_IDLST "1"
            "KKB-KEY" BY REFERENCE KKB-KEY.
           MOVE DATE-03R TO H-DATE.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                  RETURNING RESU.
       M-020.
           CALL "SD_Screen_Output" USING "SCKM01" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-020
           END-IF.
           IF  W-ACT = 9
               GO TO M-980
           END-IF.
           IF  JS-SIGN = 0
               IF  W-ACT > 5 OR < 1
                   GO TO M-020
               END-IF
           END-IF.
           IF  JS-SIGN = 1
               IF  W-ACT > 5 OR < 4
                   GO TO M-020
               END-IF
           END-IF.
           IF  W-ACT NOT = 4
               GO TO M-030
           END-IF.
           PERFORM LST-RTN THRU LST-EX.
           GO TO M-020.
       M-030.
           CALL "SD_Screen_Output" USING "SCKM01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" 
                                  RETURNING RESU.
           MOVE ZERO TO W-R.
           MOVE SPACE TO W-NAME W-KCO W-KNA.
           CALL "SD_Accept" USING BY REFERENCE A-KEY "A-KEY" "X" "5" 
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-020
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-030
           END-IF.
           IF  W-ACT = 5
               GO TO M-032
           END-IF.
           MOVE W-HCD TO KH-HCD.
      *           READ KH-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-040
           END-IF.
           MOVE W-HCD TO KHT-KEY.
      *           READ KHT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME8" E-ME8 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME6" E-ME6 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-030
           END-IF.
           IF  W-ACT = 1
               CALL "SD_Output" USING "E-ME1" E-ME1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
               GO TO M-030
           END-IF.
           COMPUTE W-SU = KHT-ZSU + KHT-KSU - KHT-HSU + KHT-ISU
                                                      - KHT-SSU.
           GO TO M-035.
       M-032.
           MOVE W-HCD TO KH-HCD.
      *           READ KH-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-030
           END-IF.
       M-035.
           MOVE KH-R TO W-R.
           PERFORM DSP-RTN THRU DSP-EX.
           IF  W-ACT = 5
               GO TO M-620
           END-IF.
           IF  W-ACT NOT = 3
               MOVE W-GT1 TO W-GT1B
               GO TO M-080
           END-IF.
           IF  ZERO NOT = KHT-KSU OR KHT-HSU OR KHT-ISU OR KHT-KKIN
                      OR KHT-SSU OR KHT-UKIN OR KHT-NKIN OR KHT-GKIN
                      OR KHT-ZSU OR KHT-ZKIN
                      OR KHT-AZS OR KHT-AAS OR KHT-AUS OR KHT-ASS
               CALL "SD_Output" USING "E-ME16" E-ME16 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
           END-IF
           GO TO M-620.
       M-040.
           MOVE W-HCD TO KHT-KEY.
      *           READ KHT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KHT-M_PNAME1 BY REFERENCE KHT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-050
           END-IF.
           CALL "SD_Output" USING "E-ME8" E-ME8 "p" 
                              RETURNING RESU
           CALL "SD_Output" USING "E-ME5" E-ME5 "p" 
                              RETURNING RESU
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                              RETURNING RESU
           GO TO M-030.
       M-050.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING "E-ME2" E-ME2 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
               GO TO M-030
           END-IF.
       M-080.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "X" "20" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-030
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-080
           END-IF.
       M-090.
           CALL "SD_Accept" USING BY REFERENCE A-KNA "A-KNA" "N" "48" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-080
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-090
           END-IF.
           IF  W-KNA = SPACE
               GO TO M-090
           END-IF.
       M-100.
           CALL "SD_Accept" USING BY REFERENCE A-YC "A-YC" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-090
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-100
           END-IF.
           MOVE SPACE TO KKB-KEY.
           MOVE 01 TO KKB-NO.
           MOVE W-YC TO KKB-YC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-ME18" E-ME18 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" 
                                  RETURNING RESU
               GO TO M-100
           END-IF.
           CALL "SD_Output" USING "D-YCN" D-YCN "p" 
                                  RETURNING RESU.
           IF  W-YC = 10 OR 11
               MOVE 0 TO W-NC
               CALL "SD_Output" USING "A-NC" A-NC "p" 
                                  RETURNING RESU
               GO TO M-490
           END-IF.
           MOVE ZERO TO W-TN W-MKR W-KKH W-DRH W-SID W-SHY W-SKH
                        W-KPS W-KC W-USG W-T2 W-GT2 W-YGT2.
           CALL "SD_Output" USING "D-T2" D-T2 "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "D-GT2" D-GT2 "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "D-YGT2" D-YGT2 "p" 
                                  RETURNING RESU.
           CALL "SD_Output" USING "D-TRS" D-TRS "p" 
                                  RETURNING RESU.
           IF  W-YC = 00
               MOVE ZERO TO W-T1 W-GT1 W-YGT1 W-NC
               CALL "SD_Output" USING "D-T1" D-T1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "D-GT1" D-GT1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "D-YGT1" D-YGT1 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "A-NC" A-NC "p" 
                                  RETURNING RESU
               GO TO M-570
           END-IF.
       M-110.
           CALL "SD_Accept" USING BY REFERENCE A-NC "A-NC" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-100
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-110
           END-IF.
           IF  W-NC NOT = 0 AND 1 AND 9
               GO TO M-110
           END-IF.
           MOVE SPACE TO W-NCN.
           IF  W-NC = 0
               MOVE "生産品" TO W-NCN
           ELSE
               IF  W-NC = 1
                   MOVE "仕入品" TO W-NCN
               ELSE
                   IF  W-NC = 9
                       MOVE "在庫未" TO W-NCN
                   END-IF
               END-IF
           END-IF.
           CALL "SD_Output" USING "D-NCN" D-NCN "p" 
                                  RETURNING RESU.
       M-490.
           CALL "SD_Accept" USING BY REFERENCE A-T1 "A-T1" "9" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YC NOT = 00 AND 10 AND 11
                   GO TO M-110
               ELSE
                   GO TO M-100
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-490
           END-IF.
           CALL "SD_Output" USING "D-T1" D-T1 "p" 
                                  RETURNING RESU.
           IF  W-YC NOT = 10 AND 11
               MOVE ZERO TO W-T2
               CALL "SD_Output" USING "D-T2" D-T2 "p" 
                                         RETURNING RESU
               GO TO M-520
           END-IF.
       M-510.
           CALL "SD_Accept" USING BY REFERENCE A-T2 "A-T2" "9" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-490
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-510
           END-IF.
           CALL "SD_Output" USING "D-T2" D-T2 "p" 
                                  RETURNING RESU.
       M-520.
           CALL "SD_Accept" USING BY REFERENCE A-GT1 "A-GT1" "9" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YC NOT = 10 AND 11
                   GO TO M-490
               ELSE
                   GO TO M-510
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-520
           END-IF.
           CALL "SD_Output" USING "D-GT1" D-GT1 "p" 
                                  RETURNING RESU.
           IF  W-YC NOT = 00 AND 99
               IF  W-GT1 = ZERO
                   CALL "SD_Output" USING "E-ME19" E-ME19 "p" 
                                  RETURNING RESU
               END-IF
           END-IF.
           IF  W-ACT = 2
               IF  W-GT1 NOT = W-GT1B
                   IF  W-SU NOT = ZERO
                       MOVE W-GT1B TO W-GT1
                       CALL "SD_Output" USING "D-GT1" D-GT1 "p" 
                                  RETURNING RESU
                       CALL "SD_Output" USING "E-ME20" E-ME20 "p" 
                                  RETURNING RESU
                       GO TO M-520
                   END-IF
               END-IF
           END-IF.
           IF  W-YC NOT = 10 AND 11
               MOVE ZERO TO W-GT2
               CALL "SD_Output" USING "D-GT2" D-GT2 "p" 
                                  RETURNING RESU
               GO TO M-532
           END-IF.
       M-530.
           CALL "SD_Accept" USING BY REFERENCE A-GT2 "A-GT2" "9" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-520
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-530
           END-IF.
           CALL "SD_Output" USING "D-GT2" D-GT2 "p" 
                                  RETURNING RESU.
       M-532.
           CALL "SD_Accept" USING BY REFERENCE A-YGT1 "A-YGT1" "9" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YC NOT = 10 AND 11
                   GO TO M-520
               ELSE
                   GO TO M-530
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-532
           END-IF.
           CALL "SD_Output" USING "D-YGT1" D-YGT1 "p" 
                                  RETURNING RESU.
           IF  W-YC NOT = 10 AND 11
               MOVE ZERO TO W-YGT2 W-TRS
               CALL "SD_Output" USING "D-YGT2" D-YGT2 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "D-TRS" D-TRS "p" 
                                  RETURNING RESU
               GO TO M-570
           END-IF.
       M-535.
           CALL "SD_Accept" USING BY REFERENCE A-YGT2 "A-YGT2" "9" "8" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-532
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-535
           END-IF.
           CALL "SD_Output" USING "D-YGT2" D-YGT2 "p" 
                                  RETURNING RESU.
       M-540.
           CALL "SD_Accept" USING BY REFERENCE A-TRS "A-TRS" "9" "2" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-535
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-540
           END-IF.
           CALL "SD_Output" USING "D-TRS" D-TRS "p" 
                                  RETURNING RESU.
       M-570.
           CALL "SD_Accept" USING BY REFERENCE A-ADD "A-ADD" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-YC = 00
                   GO TO M-100
               ELSE
                   IF  W-YC = 10 OR 11
                       GO TO M-540
                   ELSE
                       GO TO M-532
                   END-IF
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-570
           END-IF.
           IF  W-ADD = ZERO
               CALL "SD_Output" USING "D-ADDS" D-ADDS "p" 
                                  RETURNING RESU
               GO TO M-600
           END-IF.
           IF  W-ADG > 12 OR < 1
               GO TO M-570
           END-IF.
       M-600.
           CALL "SD_Accept" USING BY REFERENCE A-DNG "A-DNG" "9" "4" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO M-570
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-600
           END-IF.
           IF  W-DNG = ZERO
               CALL "SD_Output" USING "D-DNGS" D-DNGS "p" 
                                  RETURNING RESU
               GO TO M-620
           END-IF.
           IF  W-DGET > 12 OR < 1
               GO TO M-600
           END-IF.
       M-620.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3 OR 5
                   GO TO M-030
               ELSE
                   GO TO M-600
               END-IF
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-620
           END-IF.
           IF  W-DMM = 9
               CALL "SD_Output" USING "E-ME7" E-ME7 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-030
           END-IF.
           IF  W-DMM NOT = 1
               GO TO M-620
           END-IF.
           IF  W-ACT = 5
               GO TO M-030
           END-IF.
           IF  W-ACT = 3
               GO TO M-680
           END-IF.
       M-630.
           MOVE W-R TO KH-R.
           IF  W-ACT = 2
               GO TO M-670
           END-IF.
      *           WRITE KH-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            KH-M_PNAME1 KH-M_LNAME KH-R RETURNING RET.
           IF  RET = 1 
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME10" E-ME10 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-640
           END-IF.
           GO TO M-650.
       M-640.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-980
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           MOVE "KHM          " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KH-M_PNAME1 "SHARED" BY REFERENCE KH-M_IDLST "1"
            "KH-KEY" BY REFERENCE KH-KEY.
           GO TO M-630.
       M-650.
           MOVE ZERO TO KHT-R.
           MOVE W-HCD TO KHT-KEY.
           MOVE W-YC TO KHT-YC.
           MOVE W-NC TO KHT-NC.
      *           WRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1 
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME13" E-ME13 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-660
           END-IF.
           GO TO M-030.
       M-660.
           IF  ERR-STAT NOT = "24"
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-980
           END-IF.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE KHT-M_IDLST KHT-M_PNAME1.
           MOVE "KHTM         " TO W-FILE.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KHT-M_PNAME1 "SHARED" BY REFERENCE KHT-M_IDLST "2"
            "KHT-KEY" BY REFERENCE KHT-KEY "KHT-KEYD" BY REFERENCE
            KHT-KEYD.
           GO TO M-650.
       M-670.
      *           REWRITE KH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KH-M_PNAME1 KH-M_LNAME KH-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME11" E-ME11 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-980
           END-IF.
           MOVE W-YC TO KHT-YC.
           MOVE W-NC TO KHT-NC.
      *           REWRITE KHT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KHT-M_PNAME1 KHT-M_LNAME KHT-R RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME14" E-ME14 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-980
           END-IF.
           GO TO M-030.
       M-680.
      *           DELETE KH-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING KH-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME12" E-ME12 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-980
           END-IF.
      *           DELETE KHT-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING KHT-M_PNAME1 RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "E-STAT" E-STAT "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME78" E-ME78 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME15" E-ME15 "p" 
                                  RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" 
                                  RETURNING RESU
               GO TO M-980
           END-IF.
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE 150
           END-IF.
           GO TO M-030.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE KH-M_IDLST KH-M_PNAME1.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE KHT-M_IDLST KHT-M_PNAME1
           END-IF.
           CALL "DB_F_Close" USING
            BY REFERENCE KKB-M_IDLST KKB-M_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" 
                                        RETURNING RESU.
	       CALL "DB_Close".
           STOP RUN.
       DSP-RTN.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "A-KNA" A-KNA "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "A-YC" A-YC "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "A-NC" A-NC "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-T1" D-T1 "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-T2" D-T2 "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-GT1" D-GT1 "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-GT2" D-GT2 "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-YGT1" D-YGT1 "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-YGT2" D-YGT2 "p" 
                                        RETURNING RESU.
           CALL "SD_Output" USING "D-TRS" D-TRS "p" 
                                        RETURNING RESU.
           MOVE SPACE TO KKB-KEY.
           MOVE 01 TO KKB-NO.
           MOVE W-YC TO KKB-YC.
      *           READ KKB-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KKB-M_PNAME1 BY REFERENCE KKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "＊＊　用途区分なし　＊＊　　　　" TO KKB-YCN
           END-IF.
           CALL "SD_Output" USING "D-YCN" D-YCN "p" 
                                  RETURNING RESU.
           MOVE SPACE TO W-NCN.
           IF  W-NC = 0
               MOVE "生産品" TO W-NCN
           ELSE
               IF  W-NC = 1
                   MOVE "仕入品" TO W-NCN
               ELSE
                   IF  W-NC = 9
                       MOVE "在庫未" TO W-NCN
                   END-IF
               END-IF
           END-IF.
           CALL "SD_Output" USING "D-NCN" D-NCN "p" 
                                  RETURNING RESU.
           IF  W-ADD = ZERO
               CALL "SD_Output" USING "D-ADDS" D-ADDS "p" 
                                  RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-ADD" A-ADD "p" 
                                  RETURNING RESU
           END-IF.
           IF  W-DNG = ZERO
               CALL "SD_Output" USING "D-DNGS" D-DNGS "p" 
                                  RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-DNG" A-DNG "p" 
                                  RETURNING RESU
           END-IF.
       DSP-EX.
           EXIT.
       LST-RTN.
           CALL "SD_Output" USING "D-PM" D-PM "p" 
                                  RETURNING RESU.
           MOVE SPACE TO W-HCDS.
           MOVE "99999" TO W-HCDE.
       LST-020.
           CALL "SD_Accept" USING BY REFERENCE A-HCDS "A-HCDS" "X" "5" 
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" 
                                  RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-EX
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-020
           END-IF.
       LST-040.
           CALL "SD_Accept" USING BY REFERENCE A-HCDE "A-HCDE" "X" "5" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-020
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-040
           END-IF.
           IF  W-HCDS > W-HCDE
               GO TO LST-020
           END-IF.
       LST-060.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1" 
                 BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO LST-040
           END-IF.
           IF  ESTAT NOT = HTB AND SKP
               GO TO LST-060
           END-IF.
           IF  W-DMM = 9
               GO TO LST-EX
           END-IF.
           IF  W-DMM NOT = 1
               GO TO LST-060
           END-IF.
           MOVE W-HCDS TO KH-HCD.
      *           START KH-M KEY NOT < KH-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            KH-M_PNAME1 "KH-KEY" " NOT < " KH-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO LST-020
           END-IF.
       LST-080.
      *           READ KH-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" KH-M_PNAME1 BY REFERENCE KH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO LST-EX
           END-IF.
           IF  KH-HCD < W-HCDS
               GO TO LST-080
           END-IF.
           IF  KH-HCD > W-HCDE
               GO TO LST-EX
           END-IF.
           IF  W-PC = ZERO
               MOVE 5 TO W-PC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           END-IF.
           MOVE KH-R TO W-R.
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-KNA.
           MOVE W-HCD TO P-KEY.
           MOVE W-NAME TO P-NAME.
           MOVE W-YC TO P-YC.
           MOVE W-NC TO P-NC.
           IF  W-T1 NOT = ZERO
               MOVE W-T1 TO P-T1
           END-IF.
           IF  W-T2 NOT = ZERO
               MOVE "(" TO P-F1
               MOVE ")" TO P-R1
               MOVE W-T2 TO P-T2
           END-IF.
           IF  W-GT1 NOT = ZERO
               MOVE W-GT1 TO P-GT1
           END-IF.
           IF  W-GT2 NOT = ZERO
               MOVE "(" TO P-F2
               MOVE ")" TO P-R2
               MOVE W-GT2 TO P-GT2
           END-IF.
           IF  W-YGT1 NOT = ZERO
               MOVE W-YGT1 TO P-YGT1
           END-IF.
           IF  W-YGT2 NOT = ZERO
               MOVE "(" TO P-F3
               MOVE ")" TO P-R3
               MOVE W-YGT2 TO P-YGT2
           END-IF.
           MOVE W-TRS TO P-TRS.
           IF  W-ADD NOT = ZERO
               MOVE W-ADD TO P-ADD
           END-IF.
           IF  W-DNG NOT = ZERO
               MOVE W-DNG TO P-DNG
           END-IF.
           MOVE W-KNA TO P-KNA.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 63
               PERFORM MID-RTN THRU MID-EX
           END-IF.
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO LST-080.
       LST-EX.
           EXIT.
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
