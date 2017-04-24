       IDENTIFICATION DIVISION.
       PROGRAM-ID.  KBM010.
      *********************************************************
      *    PROGRAM         :  仕入先マスターメンテナンス      *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBM01                          *
      *        変更　　　  :  62/06/02                        *
      *    COMPILE TYPE    :  COBOL                           *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE   SECTION.
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(040) VALUE SPACE.
           02  F              PIC  N(025) VALUE
                "＊＊＊　　仕入先マスター　プルーフリスト　　＊＊＊".
           02  F              PIC  X(019) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC  Z(002).
           02  F              PIC  X(001) VALUE SPACE.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(005) VALUE "ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "仕　　入　　先　　名".
           02  F              PIC  X(022) VALUE SPACE.
           02  F              PIC  N(008) VALUE "住　　　　　所　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  N(004) VALUE "郵便番号".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(004) VALUE "電話番号".
           02  F              PIC  X(009) VALUE SPACE.
           02  F              PIC  N(002) VALUE "都道".
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　支".
           02  F              PIC  N(002) VALUE "　送".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "支手".
           02  F              PIC  N(002) VALUE "　部".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "最終年月".
           02  F              PIC  X(003) VALUE SPACE.
       01  HEAD3.
           02  F              PIC  X(087) VALUE SPACE.
           02  F              PIC  X(001) VALUE "(".
           02  F              PIC  N(006) VALUE "ＦＡＸ番号　".
           02  F              PIC  X(006) VALUE "     )".
           02  F              PIC  N(002) VALUE "府県".
           02  F              PIC  N(002) VALUE "　税".
           02  F              PIC  N(002) VALUE "　払".
           02  F              PIC  N(002) VALUE "　金".
           02  F              PIC  X(004) VALUE " ｻｲﾄ".
           02  F              PIC  N(002) VALUE "　門".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(004) VALUE "停止日　".
       01  W-P1.
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-NAME         PIC  N(024).
           02  F              PIC  X(001).
           02  P-JUP          PIC  N(024).
           02  F              PIC  X(001).
           02  P-UBNO         PIC  X(008).
           02  F              PIC  X(001).
           02  P-TEL          PIC  X(014).
           02  F              PIC  X(001).
           02  F              PIC  X(001).
           02  P-KCD          PIC  9(002).
           02  F              PIC  X(002).
           02  P-SZC          PIC  9(001).
           02  F              PIC  X(002).
           02  P-SHH          PIC  Z(001).
           02  F              PIC  X(002).
           02  P-SKR          PIC  Z(001).
           02  P-STS          PIC  Z(004).
           02  F              PIC  X(001).
           02  P-BKC          PIC  Z(002).
           02  F              PIC  X(001).
           02  P-ENG          PIC 99/99.
           02  F              PIC  X(004).
       01  W-P2.
           02  F              PIC  X(005).
           02  P-KANA         PIC  X(036).
           02  F              PIC  X(019).
           02  P-JSS          PIC  N(012).
           02  F              PIC  X(009).
           02  P-X1           PIC  X(001).
           02  P-FAX          PIC  X(014).
           02  P-X2           PIC  X(001).
           02  F              PIC  X(019).
           02  P-TNG          PIC 99/99.
           02  F              PIC  X(001).
       01  W-R.
           02  W-SCD          PIC  9(004).
           02  W-NAME         PIC  N(024).
           02  W-JUP          PIC  N(024).
           02  W-JDW          PIC  N(012).
           02  W-UBNO         PIC  X(008).
           02  W-TEL          PIC  X(014).
           02  W-FAX          PIC  X(014).
           02  W-KCD          PIC  9(002).
           02  W-SZC          PIC  9(001).
           02  W-STS          PIC  9(003).
           02  F              PIC  X(002).
           02  W-SKR          PIC  9(001).
           02  W-PC           PIC  9(002).
           02  W-BKC          PIC  9(002).
           02  W-SHH          PIC  9(001).
           02  FILLER         PIC  X(029).
           02  W-TNG          PIC  9(004).
           02  W-TNGD  REDEFINES W-TNG.
             03  W-TNEN       PIC  9(002).
             03  W-TGET       PIC  9(002).
           02  W-KANA         PIC  X(036).
           02  W-KAND  REDEFINES W-KANA.
             03  W-KND   OCCURS  36.
               04  W-KN       PIC  X(001).
           02  FILLER         PIC  X(006).
           02  W-ENG          PIC  9(004).
           02  FILLER         PIC  X(003).
       01  W-DATA.
           02  W-DMM          PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-SCDD         PIC  9(004).
           02  W-SCD1         PIC  9(004).
           02  W-SCD2         PIC  9(004).
           02  W-PAGE         PIC  9(002) VALUE ZERO.
           02  W-POC          PIC  9(001) VALUE ZERO.
           02  W-FILE         PIC  X(013).
           02  W-END          PIC  9(001) VALUE 0.
           02  W-BS           PIC  9(001) VALUE 0.
           02  W-ATBLD        PIC  X(050) VALUE
                "ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔ ﾕ ﾖﾗﾘﾙﾚﾛﾜ   ﾝ".
           02  W-ATBL  REDEFINES W-ATBLD.
             03  W-TBLD       PIC  X(001) OCCURS  50.
           02  W-APNOD.
             03  F            PIC  X(050) VALUE
                  "01020304051112131415212223242531323334354142434445".
             03  F            PIC  X(050) VALUE
                  "51525354556162636465710073007581828384859100000095".
           02  W-APNO  REDEFINES W-APNOD.
             03  W-PNOD       PIC  9(002) OCCURS  50.
           02  CNT            PIC  9(002).
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LISM.
           COPY LISTM.
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
           02  C-CL    PIC  X(012) VALUE "CLEAR SCREEN".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  A-SCD   PIC  9(004).
           02  FILLER.
             03  A-SCD1  PIC  9(004).
             03  A-SCD2  PIC  9(004).
           02  A-NAME  PIC  N(024).
           02  A-KANA  PIC  X(036).
           02  A-JUP   PIC  N(024).
           02  A-JDW   PIC  N(012).
           02  A-UBNO  PIC  X(008).
           02  A-TEL   PIC  X(014).
           02  A-FAX   PIC  X(014).
           02  A-KCD   PIC  9(002).
           02  A-SZC   PIC  9(001).
           02  A-SHH   PIC  9(001).
           02  A-SKR   PIC  9(001).
           02  A-STS   PIC  9(003).
           02  A-BKC   PIC  9(002).
           02  A-TNG   PIC  9(004).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-KNA   PIC  N(004).
           02  D-STS   PIC  Z(003).
           02  D-TNGC  PIC  X(004) VALUE "    ".
       01  C-PM.
           02  FILLER  PIC  X(034) VALUE
                "[  ｺｰﾄﾞ      より      迄打出し  ]".
       01  C-ERR.
           02  FILLER.
             03  E-ME1   PIC  X(040) VALUE
                  "***  SM ﾅｼ  ***                         ".
             03  E-ME2   PIC  X(040) VALUE
                  "***  STM ﾅｼ  ***                        ".
             03  E-ME3   PIC  X(040) VALUE
                  "***  SM ﾄｳﾛｸｽﾞﾐ  ***                    ".
             03  E-ME4   PIC  X(040) VALUE
                  "***  STM ﾄｳﾛｸｽﾞﾐ  ***                   ".
             03  E-ME5   PIC  X(040) VALUE
                  "***  キャンセル  ***                    ".
             03  E-ME6   PIC  X(040) VALUE
                  "***  SM WRITE ｴﾗｰ  ***                  ".
             03  E-ME7   PIC  X(040) VALUE
                  "***  SM REWRITE ｴﾗｰ  ***                ".
             03  E-ME8   PIC  X(040) VALUE
                  "***  SM DELETE ｴﾗｰ  ***                 ".
             03  E-ME9   PIC  X(040) VALUE
                  "***  STM WRITE ｴﾗｰ  ***                 ".
             03  E-ME10  PIC  X(040) VALUE
                  "***  STM DELETE ｴﾗｰ  ***                ".
             03  E-ME11  PIC  X(040) VALUE
                  "***  ﾄｳｹｲ ｺｳﾓｸ ｶﾞ ZERO ﾃﾞ ﾅｲ  ***       ".
             03  E-ME12  PIC  X(040) VALUE
                  "***  ﾃｶﾞﾀ ･ ﾌﾘｺﾐ ﾃﾞ ｼﾖｳ  ***            ".
             03  E-ME13  PIC  X(040) VALUE
                  "***  ｻﾞｲﾘｮｳ ｼｲﾚｻｷ ｺｰﾄﾞ ｶﾞ ﾐﾄｳﾛｸ  ***    ".
             03  E-ME14  PIC  X(040) VALUE
                  "***  ｾｲﾋﾝｼｲﾚｼｲﾚｻｷ ｺｰﾄﾞ ｶﾞ ﾉｺｯﾃｲﾙ  ***   ".
             03  E-ME15  PIC  X(040) VALUE
                  "***  ﾃﾞｰﾀ ｴﾗｰ (SM)  ***                 ".
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
      *C-ACP
       CALL "SD_Init" USING 
            "C-ACP" " " "0" "0" "220" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-ACT" "9" "3" "50" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD" "9" "5" "26" "4" "A-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD" BY REFERENCE W-SCD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "03C-ACP" " " "6" "0" "8" "A-SCD" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD1" "9" "6" "27" "4" " " "03C-ACP" RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD1" BY REFERENCE W-SCD1 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SCD2" "9" "6" "37" "4" "A-SCD1" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SCD2" BY REFERENCE W-SCD2 "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-NAME" "N" "7" "26" "48" "03C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-NAME" BY REFERENCE W-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KANA" "X" "8" "26" "36" "A-NAME" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KANA" BY REFERENCE W-KANA "36" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JUP" "N" "9" "26" "48" "A-KANA" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JUP" BY REFERENCE W-JUP "48" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-JDW" "N" "10" "26" "24" "A-JUP" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-JDW" BY REFERENCE W-JDW "24" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-UBNO" "X" "11" "26" "8" "A-JDW" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-UBNO" BY REFERENCE W-UBNO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TEL" "X" "12" "26" "14" "A-UBNO" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TEL" BY REFERENCE W-TEL "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-FAX" "X" "12" "52" "14" "A-TEL" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-FAX" BY REFERENCE W-FAX "14" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-KCD" "9" "13" "26" "2" "A-FAX" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-KCD" BY REFERENCE W-KCD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SZC" "9" "14" "26" "1" "A-KCD" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SZC" BY REFERENCE W-SZC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SHH" "9" "15" "26" "1" "A-SZC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SHH" BY REFERENCE W-SHH "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-SKR" "9" "16" "26" "1" "A-SHH" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-SKR" BY REFERENCE W-SKR "1" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-STS" "9" "17" "26" "3" "A-SKR" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-STS" BY REFERENCE W-STS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-BKC" "9" "18" "26" "2" "A-STS" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-BKC" BY REFERENCE W-BKC "2" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-TNG" "9" "19" "26" "4" "A-BKC" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-TNG" BY REFERENCE W-TNG "4" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "A-DMM" "9" "23" "40" "1" "A-TNG" " " RETURNING RESU.
       CALL "SD_Using" USING 
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING 
            "C-DSP" " " "0" "0" "15" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "D-KNA" "N" "13" "29" "8" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING 
            "D-KNA" BY REFERENCE HKB-FKNA "8" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-STS" "Z" "17" "26" "3" "D-KNA" " " RETURNING RESU.
       CALL "SD_From" USING 
            "D-STS" BY REFERENCE W-STS "3" "0" RETURNING RESU.
       CALL "SD_Init" USING 
            "D-TNGC" "X" "18" "26" "4" "D-STS" " " RETURNING RESU.
      *C-PM
       CALL "SD_Init" USING 
            "C-PM" " " "0" "0" "34" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-PM" "X" "6" "19" "34" " " "C-PM" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING 
            "C-ERR" " " "0" "0" "600" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "01C-ERR" " " "24" "0" "600" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME1" "X" "24" "15" "40" " " "01C-ERR" RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME2" "X" "24" "15" "40" "E-ME1" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME3" "X" "24" "15" "40" "E-ME2" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME4" "X" "24" "15" "40" "E-ME3" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME5" "X" "24" "15" "40" "E-ME4" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME6" "X" "24" "15" "40" "E-ME5" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME7" "X" "24" "15" "40" "E-ME6" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME8" "X" "24" "15" "40" "E-ME7" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME9" "X" "24" "15" "40" "E-ME8" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME10" "X" "24" "15" "40" "E-ME9" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME11" "X" "24" "15" "40" "E-ME10" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME12" "X" "24" "15" "40" "E-ME11" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME13" "X" "24" "15" "40" "E-ME12" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME14" "X" "24" "15" "40" "E-ME13" " " RETURNING RESU.
       CALL "SD_Init" USING 
            "E-ME15" "X" "24" "15" "40" "E-ME14" " " RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       M-020.
           COPY LIBCPR.
           MOVE DATE-05R TO H-DATE.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "I-O" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
       M-040.
           CALL "SD_Screen_Output" USING "SCBM01" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-040
           END-IF
           IF  W-ACT = 9
               GO TO M-980
           END-IF
           IF  W-ACT = 4
               CALL "SD_Output" USING "C-PM" C-PM "p" RETURNING RESU
               GO TO M-540
           END-IF
           IF  W-ACT NOT = 1 AND 2 AND 3
               GO TO M-040
           END-IF.
       M-060.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-BS = 9
               MOVE 0 TO W-BS
               GO TO M-040
           END-IF
           PERFORM WRD-RTN THRU WRD-EX.
           IF  W-END = 9
               GO TO M-980
           END-IF
           GO TO M-060.
       M-540.
           PERFORM PRI-RTN THRU PRI-EX.
           GO TO M-040.
       M-980.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           IF  W-POC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           CALL "SD_Screen_Output" USING "SCBM01" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           INITIALIZE W-R.
           MOVE ALL "　" TO W-NAME W-JUP W-JDW.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               MOVE 9 TO W-BS
               GO TO ACP-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-RTN
           END-IF
           MOVE W-SCD TO S-KEY
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO ACP-020
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING
                "E-ME3" E-ME3 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           MOVE S-R TO W-R.
           MOVE SPACE TO HKB-KEY.
           MOVE "01" TO HKB-NO.
           MOVE W-KCD TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-FKNA
           END-IF
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KANA" A-KANA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JUP" A-JUP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JDW" A-JDW "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UBNO" A-UBNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TEL" A-TEL "p" RETURNING RESU.
           CALL "SD_Output" USING "A-FAX" A-FAX "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KCD" A-KCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SZC" A-SZC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KNA" D-KNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SHH" A-SHH "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SKR" A-SKR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-STS" D-STS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BKC" A-BKC "p" RETURNING RESU.
           IF  W-TNG = ZERO
               CALL "SD_Output" USING "D-TNGC" D-TNGC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TNG" A-TNG "p" RETURNING RESU
           END-IF
           MOVE W-SCD TO ST-KEY
      *           READ ST-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R " "
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME2" E-ME2 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           IF  W-ACT NOT = 3
               GO TO ACP-060
           END-IF
           IF  ZERO NOT = ST-ZKZ OR ST-ZKZZ OR ST-KZ OR ST-KZZ OR ST-TSK
                                OR ST-TSKZ OR ST-THK OR ST-THKZ
               CALL "SD_Output" USING
                "E-ME11" E-ME11 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF  1 = S-SFC OR S-TGC
               CALL "SD_Output" USING
                "E-ME12" E-ME12 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
           END-IF
           IF  W-SCD > 4999
               GO TO ACP-350
           END-IF
           MOVE W-SCD TO W-SCDD.
           ADD 5000 TO W-SCD.
           MOVE W-SCD TO S-KEY.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               GO TO ACP-010
           END-IF
           CALL "SD_Output" USING "E-ME14" E-ME14 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO ACP-RTN.
       ACP-010.
           MOVE W-SCDD TO W-SCD.
           MOVE W-SCD TO S-KEY.
      *           READ S-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R " " RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME15" E-ME15 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           GO TO ACP-350.
       ACP-020.
           IF  W-ACT NOT = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           MOVE W-SCD TO ST-KEY
      *           READ ST-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" ST-M_PNAME1 BY REFERENCE ST-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO ACP-040
           END-IF
           CALL "SD_Output" USING "E-ME4" E-ME4 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           GO TO ACP-RTN.
       ACP-040.
           IF  W-SCD < 5000
               GO TO ACP-060
           END-IF
           MOVE W-SCD TO W-SCDD.
           SUBTRACT 5000 FROM W-SCD.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME13" E-ME13 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           MOVE S-R TO W-R.
           MOVE W-SCDD TO W-SCD.
           MOVE SPACE TO HKB-KEY.
           MOVE "01" TO HKB-NO.
           MOVE W-KCD TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-FKNA
           END-IF
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KANA" A-KANA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JUP" A-JUP "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JDW" A-JDW "p" RETURNING RESU.
           CALL "SD_Output" USING "A-UBNO" A-UBNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-TEL" A-TEL "p" RETURNING RESU.
           CALL "SD_Output" USING "A-FAX" A-FAX "p" RETURNING RESU.
           CALL "SD_Output" USING "A-KCD" A-KCD "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SZC" A-SZC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-KNA" D-KNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SHH" A-SHH "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SKR" A-SKR "p" RETURNING RESU.
           CALL "SD_Output" USING "D-STS" D-STS "p" RETURNING RESU.
           CALL "SD_Output" USING "A-BKC" A-BKC "p" RETURNING RESU.
           IF  W-TNG = ZERO
               CALL "SD_Output" USING "D-TNGC" D-TNGC "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-TNG" A-TNG "p" RETURNING RESU
           END-IF.
       ACP-060.
           CALL "SD_Output" USING "A-NAME" A-NAME "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-NAME "A-NAME" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF.
       ACP-070.
           CALL "SD_Output" USING "A-KANA" A-KANA "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-KANA "A-KANA" "X" "36"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-060
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-070
           END-IF
           IF  W-KANA = SPACE
               GO TO ACP-070
           END-IF
      *
           MOVE ZERO TO CNT.
       ACP-075.
           ADD 1 TO CNT.
           IF  CNT > 50
               GO TO ACP-070
           END-IF
           IF  W-KN(1) NOT = W-TBLD(CNT)
               GO TO ACP-075
           END-IF
           IF  W-PNOD(CNT) = ZERO
               GO TO ACP-075
           END-IF
           MOVE W-PNOD(CNT) TO W-PC.
       ACP-080.
           CALL "SD_Output" USING "A-JUP" A-JUP "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-JUP "A-JUP" "N" "48"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-070
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF.
       ACP-100.
           CALL "SD_Output" USING "A-JDW" A-JDW "p" RETURNING RESU.
           CALL "SD_Accept" USING BY REFERENCE A-JDW "A-JDW" "N" "24"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF.
       ACP-120.
           CALL "SD_Accept" USING BY REFERENCE A-UBNO "A-UBNO" "X" "8"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-100
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF.
       ACP-140.
           CALL "SD_Accept" USING BY REFERENCE A-TEL "A-TEL" "X" "14"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-140
           END-IF.
       ACP-160.
           CALL "SD_Accept" USING BY REFERENCE A-FAX "A-FAX" "X" "14"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-140
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-160
           END-IF.
       ACP-180.
           CALL "SD_Accept" USING BY REFERENCE A-KCD "A-KCD" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-180
           END-IF
           MOVE SPACE TO HKB-KEY.
           MOVE "01" TO HKB-NO.
           MOVE W-KCD TO HKB-TDFK.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HKB-FKNA
           END-IF
           CALL "SD_Output" USING "D-KNA" D-KNA "p" RETURNING RESU.
       ACP-200.
           CALL "SD_Accept" USING BY REFERENCE A-SZC "A-SZC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-180
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-200
           END-IF
           IF  W-SZC > 1
               GO TO ACP-200
           END-IF.
       ACP-260.
           CALL "SD_Accept" USING BY REFERENCE A-SHH "A-SHH" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-200
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-260
           END-IF
           IF  W-SHH > 3
               GO TO ACP-260
           END-IF.
       ACP-280.
           CALL "SD_Accept" USING BY REFERENCE A-SKR "A-SKR" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-260
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-280
           END-IF
           IF  W-SKR > 1
               GO TO ACP-280
           END-IF.
       ACP-300.
           CALL "SD_Accept" USING BY REFERENCE A-STS "A-STS" "9" "3"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-280
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-300
           END-IF
           CALL "SD_Output" USING "D-STS" D-STS "p" RETURNING RESU.
       ACP-320.
           CALL "SD_Accept" USING BY REFERENCE A-BKC "A-BKC" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-300
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-320
           END-IF
           IF  W-BKC NOT = 00 AND 10 AND 21 AND 22 AND 23 AND 24 AND
                          25 AND 26 AND 27 AND 28 AND 29 AND
                          31 AND 32 AND 33 AND 41
               GO TO ACP-320
           END-IF.
       ACP-330.
           CALL "SD_Accept" USING BY REFERENCE A-TNG "A-TNG" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-330
           END-IF
           IF  W-TNG = ZERO
               CALL "SD_Output" USING "D-TNGC" D-TNGC "p" RETURNING RESU
               GO TO ACP-350
           END-IF
           IF  W-TGET < 1 OR > 12
               GO TO ACP-330
           END-IF.
       ACP-350.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-RTN
               ELSE
                   GO TO ACP-330
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-350
           END-IF
           IF  W-DMM = 9
               CALL "SD_Output" USING
                "E-ME5" E-ME5 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-RTN
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-350
           END-IF.
       ACP-EX.
           EXIT.
       WRD-RTN.
           IF  W-ACT = 3
               GO TO WRD-120
           END-IF.
       WRD-020.
           IF  W-ACT = 1
               MOVE SPACE TO S-R ST-R
               INITIALIZE S-R ST-R
           END-IF
           MOVE W-R TO S-R.
           IF  W-ACT = 2
               GO TO WRD-100
           END-IF
      *           WRITE S-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               GO TO WRD-040
           END-IF
           GO TO WRD-060.
       WRD-040.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME6" E-ME6 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           MOVE "SM           " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           GO TO WRD-020.
       WRD-060.
           MOVE W-SCD TO ST-KEY.
      *           WRITE ST-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            ST-M_PNAME1 ST-M_LNAME ST-R RETURNING RET.
           IF  RET = 1
               GO TO WRD-080
           END-IF
           GO TO WRD-EX.
       WRD-080.
           CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME9" E-ME9 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
           IF  ERR-STAT NOT = "24"
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-EX
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE ST-M_IDLST ST-M_PNAME1.
           MOVE "STM          " TO W-FILE.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME71" E-ME71 "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU.
           CALL "SD_Output" USING " " " " "STOP" RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" ST-M_PNAME1 "SHARED" BY REFERENCE ST-M_IDLST "1"
            "ST-KEY" BY REFERENCE ST-KEY.
           GO TO WRD-060.
       WRD-100.
      *           REWRITE S-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            S-M_PNAME1 S-M_LNAME S-R RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME7" E-ME7 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-EX
           END-IF
           GO TO WRD-EX.
       WRD-120.
      *           DELETE S-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING S-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME8" E-ME8 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-EX
           END-IF
           IF  COMPLETION_CODE = 000
               CALL "C3_Set_Jrcode" USING 
                USER_ID BY REFERENCE COMPLETION_CODE  150
           END-IF
      *           DELETE ST-M INVALID KEY
      *///////////////
           CALL "DB_Delete" USING ST-M_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE 9 TO W-END
               CALL "SD_Output" USING
                "E-ME78" E-ME78 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME10" E-ME10 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO WRD-EX
           END-IF.
       WRD-EX.
           EXIT.
       PRI-RTN.
           CALL "SD_Accept" USING BY REFERENCE A-SCD1 "A-SCD1" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-EX
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-RTN
           END-IF.
       PRI-020.
           CALL "SD_Accept" USING BY REFERENCE A-SCD2 "A-SCD2" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-RTN
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-020
           END-IF
           IF  W-SCD1 > W-SCD2
               GO TO PRI-020
           END-IF.
       PRI-040.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO PRI-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO PRI-040
           END-IF
           IF  W-DMM = 9
               GO TO PRI-EX
           END-IF
           IF  W-DMM NOT = 1
               GO TO PRI-040
           END-IF
      *
           MOVE W-SCD1 TO S-KEY.
      *           START S-M KEY NOT < S-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            S-M_PNAME1 "S-KEY" " NOT < " S-KEY RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING
                "E-ME1" E-ME1 "p" RETURNING RESU
               CALL "SD_Output" USING
                "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO PRI-RTN
           END-IF.
       PRI-060.
      *           READ S-M NEXT RECORD WITH UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO PRI-EX
           END-IF
           IF  S-TCD > W-SCD2
               GO TO PRI-EX
           END-IF
           IF  W-POC = ZERO
               MOVE 5 TO W-POC
               CALL "PR_Open" RETURNING RESP
               PERFORM MID-020 THRU MID-EX
           END-IF
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-NAME P-JUP.
           MOVE S-TCD TO P-SCD.
           MOVE S-NAME TO P-NAME.
           MOVE S-JSU TO P-JUP.
           MOVE S-UNO TO P-UBNO.
           MOVE S-TEL TO P-TEL.
           MOVE S-FKC TO P-KCD.
           MOVE S-SZC TO P-SZC.
           MOVE S-SHH TO P-SHH.
           MOVE S-SKR TO P-SKR.
           MOVE S-STS TO P-STS.
           MOVE S-BKC TO P-BKC.
           IF  S-ENG NOT = ZERO AND SPACE
               MOVE S-ENG TO P-ENG
           END-IF
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 61
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           IF (S-KANA = SPACE) AND (S-JSS = SPACE) AND (S-FAX = SPACE)
               GO TO PRI-060
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-JSS.
           MOVE S-KANA TO P-KANA.
           MOVE S-JSS TO P-JSS.
           IF  S-FAX NOT = SPACE
               MOVE "(" TO P-X1
               MOVE S-FAX TO P-FAX
               MOVE ")" TO P-X2
           END-IF
           IF  S-TNG NOT = ZERO AND SPACE
               MOVE S-TNG TO P-TNG
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
           GO TO PRI-060.
       PRI-EX.
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
           MOVE HEAD3 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       MID-EX.
           EXIT.
