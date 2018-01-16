       IDENTIFICATION DIVISION.
       PROGRAM-ID. KBD110.
      *********************************************************
      *    PROGRAM         :  製品仕入入力、未変換リスト　    *
      *    PRINTER TYPE    :  JIPS                            *
      *    SCREEN          :  SCBD11                          *
      *        変更　　　  :  00/12/11                        *
      *    COMPILE TYPE    :  COBOL                           *
      *    JS-SIGN         :  入力 = 0 , 購買･販売=1 , 販売=2 *
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SYSTEM3100.
       OBJECT-COMPUTER. SYSTEM3100.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  JS-SIGN            PIC  9(001).
       01  HEAD1.
           02  W-20K          PIC  X(005) VALUE X"1A24212474".
           02  F              PIC  X(042) VALUE SPACE.
           02  H-MID          PIC  N(021) VALUE SPACE.
           02  F              PIC  X(030) VALUE SPACE.
           02  F              PIC  X(005) VALUE "DATE ".
           02  H-DATE         PIC 99B99B99.
           02  F              PIC  X(007) VALUE "     P.".
           02  H-PAGE         PIC Z9.
       01  HEAD2.
           02  W-15K          PIC  X(005) VALUE X"1A24212078".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　日　付".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(004) VALUE "伝票番号".
           02  F              PIC  X(006) VALUE " ｺｰﾄﾞ ".
           02  F              PIC  N(010) VALUE
                "仕　　入　　先　　名".
           02  F              PIC  X(023) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "材　　料　　名　".
           02  F              PIC  X(024) VALUE SPACE.
           02  F              PIC  X(003) VALUE "(  ".
           02  F              PIC  N(004) VALUE "合計数量".
           02  F              PIC  X(001) VALUE ")".
           02  F              PIC  X(007) VALUE SPACE.
           02  F              PIC  N(004) VALUE "　修正日".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "購買".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "販売".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  N(002) VALUE "営業".
       01  HEAD3.
           02  F              PIC  X(003) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　№".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(006) VALUE "　ロット№　".
           02  F              PIC  N(002) VALUE "　返".
           02  F              PIC  N(002) VALUE "　完".
           02  F              PIC  N(002) VALUE "　倉".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  X(006) VALUE "ｺｰﾄﾞ  ".
           02  F              PIC  N(008) VALUE "品　　　　　名　".
           02  F              PIC  X(025) VALUE SPACE.
           02  F              PIC  X(001) VALUE "1".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "３号".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "２号".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "１号".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "０号".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　中".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　大".
           02  F              PIC  X(002) VALUE SPACE.
           02  F              PIC  N(002) VALUE "特大".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "28.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "29.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "30.0".
           02  F              PIC  X(014) VALUE SPACE.
       01  HEAD4.
           02  F              PIC  X(017) VALUE SPACE.
           02  F              PIC  N(002) VALUE "　品".
           02  F              PIC  N(002) VALUE "　了".
           02  F              PIC  N(002) VALUE "　庫".
           02  F              PIC  X(045) VALUE SPACE.
           02  F              PIC  X(001) VALUE "2".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "12.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "13.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "14.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "15.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "16.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "17.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "18.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "19.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "20.0".
           02  F              PIC  X(014) VALUE SPACE.
       01  HEAD5.
           02  F              PIC  X(071) VALUE SPACE.
           02  F              PIC  X(001) VALUE "3".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "21.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "22.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "23.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(019) VALUE SPACE.
       01  HEAD6.
           02  F              PIC  X(071) VALUE SPACE.
           02  F              PIC  X(001) VALUE "4".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "24.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "25.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "26.5".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.0".
           02  F              PIC  X(001) VALUE SPACE.
           02  F              PIC  X(004) VALUE "27.5".
           02  F              PIC  X(014) VALUE SPACE.
           02  F              PIC  N(002) VALUE "合計".
           02  F              PIC  X(004) VALUE SPACE.
           02  F              PIC  N(002) VALUE "単価".
       01  W-P1.
           02  P-DATE         PIC 99/99/99.
           02  F              PIC  X(001).
           02  P-DNO          PIC  9(006).
           02  F              PIC  X(001).
           02  P-SCD          PIC  9(004).
           02  F              PIC  X(001).
           02  P-SNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-JCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-JNA          PIC  N(024).
           02  P-F            PIC  X(001).
           02  P-SUT          PIC ----,--9.
           02  P-R            PIC  X(001).
           02  F              PIC  X(006).
           02  P-CD           PIC 99/99/99.
           02  P-BH           PIC  N(002).
           02  F              PIC  X(001).
           02  P-HH           PIC  N(002).
           02  F              PIC  X(001).
           02  P-HK           PIC  N(002).
       01  W-P2.
           02  F              PIC  X(005).
           02  P-SNO          PIC  9(001).
           02  F              PIC  X(001).
           02  P-RSN          PIC  9(002).
           02  P-V1           PIC  X(001).
           02  P-RNG          PIC  9(004).
           02  P-V2           PIC  X(001).
           02  P-RND          PIC  9(002).
           02  F              PIC  X(002).
           02  P-HPC          PIC  Z(001).
           02  F              PIC  X(002).
           02  P-KRC          PIC  Z(001).
           02  F              PIC  X(002).
           02  P-SKC          PIC  9(001).
           02  F              PIC  X(001).
           02  P-HCD          PIC  9(006).
           02  F              PIC  X(001).
           02  P-HNA          PIC  N(024).
           02  F              PIC  X(001).
           02  P-SIZ          PIC  9(001).
           02  P-ASU.
             03  P-SUD   OCCURS  10.
               04  P-SU       PIC  -(005).
           02  P-TSU          PIC ---,--9.
           02  P-T            PIC ZZZ,ZZ9.
       01  W-DATA.
           02  W-AID.
             03  W-DATE       PIC  9(008).
             03  W-CD         PIC  9(006).
             03  W-SCD        PIC  9(004).
             03  W-JCD        PIC  9(006).
             03  W-SUT        PIC S9(006).
             03  W-ID    OCCURS   9.
               04  W-SKC      PIC  9(001).
               04  W-RNO      PIC  9(008).
               04  W-RNOD  REDEFINES W-RNO.
                 05  W-RSN    PIC  9(002).
                 05  W-RNG    PIC  9(004).
                 05  W-RND    PIC  9(002).
               04  W-HCD      PIC  9(006).
               04  W-ASUD.
                 05  W-ASU   OCCURS   4.
                   06  W-SUD   OCCURS  10.
                     07  W-SU PIC S9(004).
               04  W-HPC      PIC  9(001).
               04  W-KRC      PIC  9(001).
               04  W-AZSUD.
                 05  W-AZSU   OCCURS   4.
                   06  W-ZSUD   OCCURS  10.
                     07  W-ZSU PIC S9(004).
           02  W-DNO          PIC  9(006).
           02  W-SNO          PIC  9(002).
           02  W-SNOD         PIC  9(002).
           02  W-SNOF         PIC  9(002).
           02  W-SNOR         PIC  9(002).
           02  W-TSU          PIC S9(005).
           02  W-CHK          PIC  9(001).
           02  W-DMM          PIC  9(001).
           02  W-ACT          PIC  9(001).
           02  W-NGP.
             03  W-NG.
               04  W-NEN      PIC  9(004).
               04  W-NENL  REDEFINES W-NEN.
                 05  W-NEN1   PIC  9(002).
                 05  W-NEN2   PIC  9(002).
               04  W-GET      PIC  9(002).
             03  W-NGL   REDEFINES W-NG.
               04  F          PIC  9(002).
               04  W-NGS      PIC  9(004).
             03  W-PEY        PIC  9(002).
           02  W-NGPL  REDEFINES W-NGP.
             03  F            PIC  9(002).
             03  W-NGPS       PIC  9(006).
           02  W-CDD.
             03  W-CNEN       PIC  9(002).
             03  W-CGET       PIC  9(002).
             03  W-CPEY       PIC  9(002).
           02  W-SCDD         PIC  9(004).
           02  W-SKCD         PIC  9(001).
           02  W-SUTD         PIC S9(006).
           02  W-BSU          PIC S9(004).
           02  W-SKP          PIC  9(001).
           02  W-NC           PIC  9(001).
           02  W-L            PIC  9(002).
           02  W-C            PIC S9(002).
           02  W-GC           PIC  9(001).
           02  W-ZC           PIC  9(001).
           02  W-ZCF          PIC  9(001).
           02  W-ZCR          PIC  9(001).
           02  W-ADC.
             03  W-DCD   OCCURS   4.
               04  W-DC       PIC  9(001).
           02  W-SC           PIC  9(002).
           02  W-ASSD.
             03  W-SSD   OCCURS  10.
               04  W-SS       PIC  9(001).
           02  W-PC           PIC  9(001).
           02  W-PRC          PIC  9(001).
           02  W-LIST         PIC  9(001).
           02  W-PAGE         PIC  9(002).
           02  W-BNG          PIC  9(006).
           02  W-HNG          PIC  9(006).
           02  W-SNG          PIC  9(006).
           02  W-ENGP         PIC  9(008).
           02  HIZUKE         PIC  9(002).
           02  W-S            PIC  9(001).
           02  W-SD           PIC  9(001).
           02  CNT            PIC  9(002).
           02  W-KIN          PIC S9(008).
           02  W-INV          PIC  9(001).
           02  W-NO           PIC  9(002).
           02  W-UNO          PIC  9(006).
       01  W-SPC              PIC  X(055) VALUE SPACE.
       01  W-EM               PIC  X(030) VALUE SPACE.
       01  W-CO               PIC  X(006) VALUE SPACE.
       01  W-FILE             PIC  X(013) VALUE SPACE.
       01  ERR-STAT           PIC  X(002).
           COPY LSTAT.
      *
           COPY LIBFDD.
           COPY LIKBNO.
           COPY LISM.
           COPY LIJM.
           COPY LIHIM.
           COPY L-JCON.
           COPY LIHSHF.
           COPY LIHSSF.
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
       01  C-MID0.
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　製品仕入　入力リスト　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(020) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(044) VALUE
                "当月全件=1  未作表分=5  作表しない=9    ﾘﾀｰﾝ".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-MID1.
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　製品仕入　未変換リスト　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊　　　　　　　　　　　　　　　＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  N(021) VALUE
                "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
           02  FILLER  PIC  X(022) VALUE
                "確認  OK=1 NO=9   ﾘﾀｰﾝ".
       01  C-ACP.
           02  A-ACT   PIC  9(001).
           02  FILLER.
             03  A-DATE  PIC  9(006).
             03  A-CD    PIC  9(006).
             03  A-DNO   PIC  9(006).
           02  A-SCD   PIC  9(004).
           02  A-JCD   PIC  9(006).
           02  A-SUT   PIC S9(006).
           02  A-SNO   PIC  9(001).
           02  FILLER.
             03  A-RSN   PIC  9(002).
             03  A-RNG   PIC  9(004).
             03  A-RND   PIC  9(002).
           02  A-SKC   PIC  9(001).
           02  A-HCD   PIC  9(006).
           02  FILLER.
             03  A-SU    PIC S9(004).
           02  FILLER.
             03  A-HPC   PIC  9(001).
             03  A-KRC   PIC  9(001).
           02  A-LIST  PIC  9(001).
           02  A-CHK   PIC  9(001).
           02  A-DMM   PIC  9(001).
       01  C-DSP.
           02  D-SNA   PIC  N(024).
           02  D-JNA   PIC  N(024).
           02  D-SUT   PIC ZZZZZ9- .
           02  D-SKN   PIC  N(006).
           02  D-HNA   PIC  N(024).
           02  D-SU    PIC ZZZZ- .
           02  D-TSU   PIC ZZ,ZZ9- .
           02  D-SUTD  PIC ZZZ,ZZ9- .
           02  FILLER.
             03  D-T     PIC ZZ,ZZ9 .
             03  D-KIN   PIC ---,---,--9 .
           02  D-CLEAR.
             03  FILLER.
               04  S-CD    PIC  X(006) VALUE "      ".
               04  S-DNO   PIC  X(006) VALUE "      ".
             03  S-SCD   PIC  X(055).
             03  S-JCD   PIC  X(055).
             03  S-HD.
               04  S-RNO.
                 05  FILLER  PIC  X(002) VALUE "  ".
                 05  FILLER  PIC  X(004) VALUE "    ".
                 05  FILLER  PIC  X(002) VALUE "  ".
               04  S-SKC   PIC  X(014) VALUE
                    "              ".
               04  S-HCD.
                 05  FILLER  PIC  X(055).
               04  S-ASUD.
                 05  S-SIZ2.
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                 05  S-SIZ3.
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                 05  S-SIZ4.
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                 05  S-SIZ1.
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
                   06  FILLER  PIC  X(005) VALUE "     ".
               04  S-SUT.
                 05  FILLER  PIC  X(007) VALUE "       ".
               04  FILLER.
                 05  S-T     PIC  X(006) VALUE "      ".
                 05  S-KIN   PIC  X(011) VALUE
                      "           ".
               04  FILLER.
                 05  S-HPC   PIC  X(001) VALUE " ".
                 05  S-KRC   PIC  X(001) VALUE " ".
               04  S-CHK   PIC  X(001) VALUE " ".
               04  S-SUTD.
                 05  FILLER  PIC  X(008) VALUE "        ".
       01  C-ERR.
           02  FILLER.
             03  E-ME    PIC  X(030).
             03  E-CO    PIC  X(006).
             03  E-RNO   PIC  X(008).
             03  E-ENGP  PIC 99/99/99 .
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
      *C-MID0
       CALL "SD_Init" USING
            "C-MID0" " " "0" "0" "346" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID0" "N" "3" "10" "40" " " "C-MID0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID0" "N" "4" "10" "40" "01C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID0" "N" "5" "10" "40" "02C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID0" "N" "6" "10" "40" "03C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID0" "N" "7" "10" "40" "04C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID0" "N" "8" "10" "40" "05C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID0" "N" "9" "10" "40" "06C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID0" "X" "14" "10" "44" "07C-MID0" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09C-MID0" "X" "23" "42" "22" "08C-MID0" " " RETURNING RESU.
      *C-MID1
       CALL "SD_Init" USING
            "C-MID1" " " "0" "0" "316" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-MID1" "N" "3" "10" "42" " " "C-MID1" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-MID1" "N" "4" "10" "42" "01C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03C-MID1" "N" "5" "10" "42" "02C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04C-MID1" "N" "6" "10" "42" "03C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05C-MID1" "N" "7" "10" "42" "04C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06C-MID1" "N" "8" "10" "42" "05C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07C-MID1" "N" "9" "10" "42" "06C-MID1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08C-MID1" "X" "23" "42" "22" "07C-MID1" " " RETURNING RESU.
      *C-ACP
       CALL "SD_Init" USING
            "C-ACP" " " "0" "0" "60" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-ACT" "9" "1" "63" "1" " " "C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-ACT" BY REFERENCE W-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "02C-ACP" " " "3" "0" "18" "A-ACT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-DATE" "9" "3" "13" "6" " " "02C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-DATE" BY REFERENCE W-NGPS "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-CD" "9" "3" "27" "6" "A-DATE" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-CD" BY REFERENCE W-CDD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DNO" "9" "3" "62" "6" "A-CD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DNO" BY REFERENCE W-DNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SCD" "9" "4" "13" "4" "02C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SCD" BY REFERENCE W-SCDD "4" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-JCD" "9" "5" "13" "6" "A-SCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-JCD" BY REFERENCE W-JCD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SUT" "S9" "6" "13" "6" "A-JCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SUT" BY REFERENCE W-SUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-SNO" "9" "8" "9" "1" "A-SUT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SNO" BY REFERENCE W-SNO "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "07C-ACP" " " "8" "0" "8" "A-SNO" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-RSN" "9" "8" "45" "2" " " "07C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-RSN" BY REFERENCE W-RSN(1) "2" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-RNG" "9" "8" "48" "4" "A-RSN" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-RNG" BY REFERENCE W-RNG(1) "4" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-RND" "9" "8" "53" "2" "A-RNG" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-RND" BY REFERENCE W-RND(1) "2" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-SKC" "9" "9" "13" "1" "07C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-SKC" BY REFERENCE W-SKC(1) "1" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-HCD" "9" "10" "13" "6" "A-SKC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-HCD" BY REFERENCE W-HCD(1) "6" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "10C-ACP" " " "W-L" "0" "4" "A-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-SU" "S9" "W-L" "W-C" "4" " " "10C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-SU" BY REFERENCE W-SU(1,1,1) "4" "3" BY REFERENCE W-SNO
            337 BY REFERENCE W-GC 40 BY REFERENCE W-SC 4 RETURNING RESU.
       CALL "SD_Init" USING
            "11C-ACP" " " "22" "0" "2" "10C-ACP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "A-HPC" "9" "22" "15" "1" " " "11C-ACP" RETURNING RESU.
       CALL "SD_Using" USING
            "A-HPC" BY REFERENCE W-HPC(1) "1" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-KRC" "9" "22" "26" "1" "A-HPC" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-KRC" BY REFERENCE W-KRC(1) "1" "1" BY REFERENCE W-SNO 337
            RETURNING RESU.
       CALL "SD_Init" USING
            "A-LIST" "9" "14" "49" "1" "11C-ACP" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-LIST" BY REFERENCE W-LIST "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-CHK" "9" "22" "38" "1" "A-LIST" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-CHK" BY REFERENCE W-CHK "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "A-DMM" "9" "23" "59" "1" "A-CHK" " " RETURNING RESU.
       CALL "SD_Using" USING
            "A-DMM" BY REFERENCE W-DMM "1" "0" RETURNING RESU.
      *C-DSP
       CALL "SD_Init" USING
            "C-DSP" " " "0" "0" "619" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-SNA" "N" "4" "18" "48" " " "C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-SNA" BY REFERENCE S-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-JNA" "N" "5" "20" "48" "D-SNA" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-JNA" BY REFERENCE J-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SUT" "ZZZZZ9-" "6" "13" "7" "D-JNA" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-SUT" BY REFERENCE W-SUT "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SKN" "N" "9" "15" "12" "D-SUT" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-SKN" BY REFERENCE JCON3-03 "12" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-HNA" "N" "10" "20" "48" "D-SKN" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-HNA" BY REFERENCE HI-NAME "48" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SU" "ZZZZ-" "W-L" "W-C" "5" "D-HNA" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-SU" BY REFERENCE W-SU(1,1,1) "4" "3" BY REFERENCE W-SNO
            337 BY REFERENCE W-GC 40 BY REFERENCE W-SC 4 RETURNING RESU.
       CALL "SD_Init" USING
            "D-TSU" "ZZ,ZZ9-" "19" "66" "7" "D-SU" " " RETURNING RESU.
       CALL "SD_From" USING
            "D-TSU" BY REFERENCE W-TSU "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-SUTD" "ZZZ,ZZ9-" "21" "65" "8" "D-TSU" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-SUTD" BY REFERENCE W-SUTD "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "09C-DSP" " " "21" "0" "17" "D-SUTD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "D-T" "ZZ,ZZ9" "21" "11" "6" " " "09C-DSP" RETURNING RESU.
       CALL "SD_From" USING
            "D-T" BY REFERENCE HSH-T "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-KIN" "---,---,--9" "21" "23" "11" "D-T" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "D-KIN" BY REFERENCE W-KIN "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "D-CLEAR" " " "0" "0" "419" "09C-DSP" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01D-CLEAR" " " "3" "0" "12" " " "D-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING
            "S-CD" "X" "3" "27" "6" " " "01D-CLEAR" RETURNING RESU.
       CALL "SD_Init" USING
            "S-DNO" "X" "3" "62" "6" "S-CD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SCD" "X" "4" "13" "55" "01D-CLEAR" " " RETURNING RESU.
       CALL "SD_From" USING
            "S-SCD" BY REFERENCE W-SPC "55" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "S-JCD" "X" "5" "13" "55" "S-SCD" " " RETURNING RESU.
       CALL "SD_From" USING
            "S-JCD" BY REFERENCE W-SPC "55" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "S-HD" " " "0" "0" "297" "S-JCD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-RNO" " " "8" "0" "8" " " "S-HD" RETURNING RESU.
       CALL "SD_Init" USING
            "01S-RNO" "X" "8" "45" "2" " " "S-RNO" RETURNING RESU.
       CALL "SD_Init" USING
            "02S-RNO" "X" "8" "48" "4" "01S-RNO" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03S-RNO" "X" "8" "53" "2" "02S-RNO" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SKC" "X" "9" "13" "14" "S-RNO" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-HCD" " " "10" "0" "55" "S-SKC" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01S-HCD" "X" "10" "13" "55" " " "S-HCD" RETURNING RESU.
       CALL "SD_From" USING
            "01S-HCD" BY REFERENCE W-SPC "55" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "S-ASUD" " " "0" "0" "185" "S-HCD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SIZ2" " " "13" "0" "50" " " "S-ASUD" RETURNING RESU.
       CALL "SD_Init" USING
            "01S-SIZ2" "X" "13" "6" "5" " " "S-SIZ2" RETURNING RESU.
       CALL "SD_Init" USING
            "02S-SIZ2" "X" "13" "12" "5" "01S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03S-SIZ2" "X" "13" "18" "5" "02S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04S-SIZ2" "X" "13" "24" "5" "03S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05S-SIZ2" "X" "13" "30" "5" "04S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06S-SIZ2" "X" "13" "36" "5" "05S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07S-SIZ2" "X" "13" "42" "5" "06S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08S-SIZ2" "X" "13" "48" "5" "07S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09S-SIZ2" "X" "13" "54" "5" "08S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10S-SIZ2" "X" "13" "60" "5" "09S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SIZ3" " " "15" "0" "45" "S-SIZ2" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01S-SIZ3" "X" "15" "6" "5" " " "S-SIZ3" RETURNING RESU.
       CALL "SD_Init" USING
            "02S-SIZ3" "X" "15" "12" "5" "01S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03S-SIZ3" "X" "15" "18" "5" "02S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04S-SIZ3" "X" "15" "24" "5" "03S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05S-SIZ3" "X" "15" "30" "5" "04S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06S-SIZ3" "X" "15" "36" "5" "05S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07S-SIZ3" "X" "15" "42" "5" "06S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08S-SIZ3" "X" "15" "48" "5" "07S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09S-SIZ3" "X" "15" "54" "5" "08S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SIZ4" " " "17" "0" "40" "S-SIZ3" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01S-SIZ4" "X" "17" "6" "5" " " "S-SIZ4" RETURNING RESU.
       CALL "SD_Init" USING
            "02S-SIZ4" "X" "17" "12" "5" "01S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03S-SIZ4" "X" "17" "18" "5" "02S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04S-SIZ4" "X" "17" "24" "5" "03S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05S-SIZ4" "X" "17" "30" "5" "04S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06S-SIZ4" "X" "17" "36" "5" "05S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07S-SIZ4" "X" "17" "42" "5" "06S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08S-SIZ4" "X" "17" "48" "5" "07S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SIZ1" " " "19" "0" "50" "S-SIZ4" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01S-SIZ1" "X" "19" "6" "5" " " "S-SIZ1" RETURNING RESU.
       CALL "SD_Init" USING
            "02S-SIZ1" "X" "19" "12" "5" "01S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "03S-SIZ1" "X" "19" "18" "5" "02S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "04S-SIZ1" "X" "19" "24" "5" "03S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "05S-SIZ1" "X" "19" "30" "5" "04S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "06S-SIZ1" "X" "19" "36" "5" "05S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07S-SIZ1" "X" "19" "42" "5" "06S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "08S-SIZ1" "X" "19" "48" "5" "07S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "09S-SIZ1" "X" "19" "54" "5" "08S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "10S-SIZ1" "X" "19" "60" "5" "09S-SIZ1" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SUT" " " "19" "0" "7" "S-ASUD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01S-SUT" "X" "19" "66" "7" " " "S-SUT" RETURNING RESU.
       CALL "SD_Init" USING
            "06S-HD" " " "21" "0" "17" "S-SUT" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-T" "X" "21" "11" "6" " " "06S-HD" RETURNING RESU.
       CALL "SD_Init" USING
            "S-KIN" "X" "21" "23" "11" "S-T" " " RETURNING RESU.
       CALL "SD_Init" USING
            "07S-HD" " " "22" "0" "2" "06S-HD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-HPC" "X" "22" "15" "1" " " "07S-HD" RETURNING RESU.
       CALL "SD_Init" USING
            "S-KRC" "X" "22" "26" "1" "S-HPC" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-CHK" "X" "22" "38" "1" "07S-HD" " " RETURNING RESU.
       CALL "SD_Init" USING
            "S-SUTD" " " "21" "0" "8" "S-CHK" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01S-SUTD" "X" "21" "65" "8" " " "S-SUTD" RETURNING RESU.
      *C-ERR
       CALL "SD_Init" USING
            "C-ERR" " " "0" "0" "52" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "01C-ERR" " " "24" "0" "52" " " "C-ERR" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ME" "X" "24" "15" "30" " " "01C-ERR" RETURNING RESU.
       CALL "SD_From" USING
            "E-ME" BY REFERENCE W-EM "30" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-CO" "X" "24" "46" "6" "E-ME" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-CO" BY REFERENCE W-CO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-RNO" "X" "24" "46" "8" "E-CO" " " RETURNING RESU.
       CALL "SD_From" USING
            "E-RNO" BY REFERENCE HSS-RNO "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "E-ENGP" "99/99/99" "24" "46" "8" "E-RNO" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "E-ENGP" BY REFERENCE HSH-ENGP "6" "0" RETURNING RESU.
      *
           COPY LSSEM_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
       M-05.
           ACCEPT JS-SIGN FROM ARGUMENT-VALUE.
           IF  JS-SIGN > 2
               GO TO M-95
           END-IF
           COPY LIBCPR.
           MOVE ZERO TO W-DATA.
           ACCEPT W-NGPS FROM DATE.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-SNG.
           MOVE ZERO TO W-NGP.
           MOVE D-NBNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-BNG.
           ADD 1 TO W-GET.
           IF  W-GET = 13
               ADD 1 TO W-NEN
               MOVE 1 TO W-GET
           END-IF
           MOVE 31 TO W-PEY.
           MOVE W-NGP TO W-ENGP.
           MOVE ZERO TO W-NGP.
           MOVE D-NHNG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           MOVE W-NG TO W-HNG.
           MOVE DATE-05R TO H-DATE.
           IF  JS-SIGN = 1 OR 2
               GO TO M-50
           END-IF
      *
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_F_Open" USING
            "I-O" KBNO-M_PNAME1 " " BY REFERENCE KBNO-M_IDLST "1"
            "BNO-KEY" BY REFERENCE BNO-KEY.
           MOVE SPACE TO BNO-KEY.
           MOVE "01" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "***  KBNOM ﾅｼ (01)  ***       " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               GO TO M-95
           END-IF
           MOVE BNO-DATE TO HIZUKE.
           MOVE SPACE TO BNO-KEY.
           MOVE "02" TO BNO-KEYD.
      *           READ KBNO-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KBNO-M_PNAME1 BY REFERENCE KBNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "***  KBNOM ﾅｼ (02)  ***       " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
               GO TO M-95
           END-IF
      *
           CALL "DB_F_Open" USING
            "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
            "S-KEY" BY REFERENCE S-KEY.
           CALL "DB_F_Open" USING
            "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
            "J-KEY" BY REFERENCE J-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
            "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE HI-KEY2.
           CALL "DB_F_Open" USING
            "I-O" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
            "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
            HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3.
           CALL "DB_F_Open" USING
            "I-O" JCON_PNAME1 "SHARED" BY REFERENCE JCON_IDLST "1"
            "JCON1-KEY" BY REFERENCE JCON1-KEY.
           CALL "DB_F_Open" USING
            "I-O" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
            "HSS-KEY" BY REFERENCE HSS-KEY.
           CALL "SD_Screen_Output" USING "SCBD11" RETURNING RESU.
       M-10.
           PERFORM ACP-RTN THRU ACP-EX.
           IF  W-ACT = 9
               GO TO M-50
           END-IF
      *
           IF  W-ACT = 1
               GO TO M-25
           END-IF
           MOVE SPACE TO HSS-KEY.
           MOVE W-DNO TO HSS-DNO.
      *           START HSS-F KEY NOT < HSS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSS-F_PNAME1 "HSS-KEY" " NOT < " HSS-KEY RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF.
       M-15.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-25
           END-IF
           IF  HSS-DNO NOT = W-DNO
               GO TO M-25
           END-IF
           IF  HSS-RNO = ZERO
               GO TO M-20
           END-IF
           MOVE HSS-RNO TO HSH-KEY.
      *           READ HSHF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "***  HSHF ﾛｯﾄNO ﾅｼ (SUB)  *** " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-RNO" E-RNO "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           PERFORM SUB-RTN THRU SUB-EX.
      *           REWRITE HSH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSHF_PNAME1 HSHF_LNAME HSH-R RETURNING RET.
           IF  RET = 1
               MOVE "***  HSHF REWRITE ｴﾗｰ (SUB)  *" TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-RNO" E-RNO "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-20.
      *           DELETE HSS-F INVALID KEY
      *///////////////
           CALL "DB_Delete" USING HSS-F_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "***  HSSF DELETE ｴﾗｰ  ***     " TO W-EM
               CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-15.
       M-25.
           IF  W-ACT = 3
               GO TO M-10
           END-IF
           IF  W-ACT = 1
               COMPUTE W-DNO = BNO-DNO1 + 1
               IF  W-DNO = ZERO
                   ADD 1 TO W-DNO
               END-IF
           END-IF
           MOVE ZERO TO W-SNO.
       M-30.
           ADD 1 TO W-SNO.
           IF  W-SNO = 10
               GO TO M-35
           END-IF
           IF (W-SKC(W-SNO) = ZERO) AND (W-HCD(W-SNO) = ZERO)
               GO TO M-30
           END-IF
      *
           MOVE "18" TO JCON5-KEY.
      *           READ JCON INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON5-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "***  JCON ﾃﾞﾝﾋﾟｮｳNO ﾅｼ  ***   " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           ADD 1 TO JCON5-03.
           MOVE JCON5-03 TO W-UNO.
      *           REWRITE JCON5-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            JCON_PNAME1 JCON_LNAME JCON5-R RETURNING RET.
           IF  RET = 1
               MOVE "***  JCON REWRITE ｴﾗｰ  ***    " TO W-EM
               CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
      *
           MOVE ZERO TO HSS-R.
           MOVE W-DNO TO HSS-DNO.
           MOVE W-SNO TO HSS-SNO.
           MOVE W-DATE TO HSS-DATE.
           MOVE W-CD TO HSS-CD.
           MOVE W-SCD TO HSS-SCD.
           MOVE W-JCD TO HSS-JCD.
           MOVE W-SUT TO HSS-SUT.
           MOVE W-RNO(W-SNO) TO HSS-RNO.
           MOVE W-HCD(W-SNO) TO HSS-HCD.
           MOVE W-SKC(W-SNO) TO HSS-SKC.
           MOVE W-ASU(W-SNO,4) TO HSS-ASU(1).
           MOVE W-ASU(W-SNO,1) TO HSS-ASU(2).
           MOVE W-ASU(W-SNO,2) TO HSS-ASU(3).
           MOVE W-ASU(W-SNO,3) TO HSS-ASU(4).
           MOVE W-HPC(W-SNO) TO HSS-HPC.
           MOVE W-KRC(W-SNO) TO HSS-KRC.
           MOVE W-UNO TO HSS-UNO.
           IF  W-ACT = 2
               MOVE W-NC TO HSS-BHC
           END-IF
           MOVE 0 TO HSS-PRC.
      *           WRITE HSS-R INVALID KEY
      *//////////////
           CALL "DB_Insert" USING
            HSS-F_PNAME1 HSS-F_LNAME HSS-R RETURNING RET.
           IF  RET = 1
               MOVE "***  HSSF WRITE ｴﾗｰ  ***      " TO W-EM
               CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           IF  HSS-RNO = ZERO
               GO TO M-30
           END-IF
           MOVE HSS-RNO TO HSH-KEY.
      *           READ HSHF INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "***  HSHF ﾛｯﾄNO ﾅｼ (ADD)  *** " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-RNO" E-RNO "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           PERFORM ADD-RTN THRU ADD-EX.
      *           REWRITE HSH-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSHF_PNAME1 HSHF_LNAME HSH-R RETURNING RET.
           IF  RET = 1
               MOVE "***  HSHF REWRITE ｴﾗｰ (ADD)  *" TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-RNO" E-RNO "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-30.
       M-35.
           IF  W-ACT = 2
               GO TO M-10
           END-IF
           MOVE W-DNO TO BNO-DNO1.
      *           REWRITE KBNO-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            KBNO-M_PNAME1 KBNO-M_LNAME KBNO-R RETURNING RET.
           IF  RET = 1
               MOVE "***  KBNOM REWRITE ｴﾗｰ  ***   " TO W-EM
               CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF
           GO TO M-10.
       M-50.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           IF  JS-SIGN = 0
               CALL "SD_Output" USING "C-MID0" C-MID0 "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "C-MID1" C-MID1 "p" RETURNING RESU
               IF  JS-SIGN = 2
                   GO TO M-65
               ELSE
                   GO TO M-60
               END-IF
           END-IF.
       M-55.
           CALL "SD_Accept" USING BY REFERENCE A-LIST "A-LIST" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-55
           END-IF
           IF  W-LIST NOT = 1 AND 5 AND 9
               GO TO M-55
           END-IF.
       M-60.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  JS-SIGN = 0
                   GO TO M-55
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO M-60
           END-IF
           IF  W-DMM = 9
               IF  JS-SIGN = 0
                   GO TO M-55
               ELSE
                   GO TO M-95
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO M-60
           END-IF
           IF  W-LIST = 9
               GO TO M-95
           END-IF
           IF  W-LIST = 1
               IF  W-BNG > W-HNG
                   MOVE W-HNG TO W-NG
               ELSE
                   MOVE W-BNG TO W-NG
               END-IF
           END-IF.
       M-65.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE HSS-F_IDLST HSS-F_PNAME1
               CALL "DB_F_Open" USING
                "I-O" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST "1"
                "HSS-KEY" BY REFERENCE HSS-KEY
           ELSE
               CALL "DB_F_Open" USING
                "INPUT" S-M_PNAME1 "SHARED" BY REFERENCE S-M_IDLST "1"
                "S-KEY" BY REFERENCE S-KEY
               CALL "DB_F_Open" USING
                "INPUT" J-M_PNAME1 "SHARED" BY REFERENCE J-M_IDLST "1"
                "J-KEY" BY REFERENCE J-KEY
               CALL "DB_F_Open" USING
                "INPUT" HI-M_PNAME1 "SHARED" BY REFERENCE HI-M_IDLST "2"
                "HI-KEY" BY REFERENCE HI-KEY "HI-KEY2" BY REFERENCE
                HI-KEY2
               CALL "DB_F_Open" USING
                "INPUT" HSHF_PNAME1 "SHARED" BY REFERENCE HSHF_IDLST "3"
                "HSH-KEY" BY REFERENCE HSH-KEY "HSH-KEY2" BY REFERENCE
                HSH-KEY2 "HSH-KEY3" BY REFERENCE HSH-KEY3
               CALL "DB_F_Open" USING
                "INPUT" HSS-F_PNAME1 "SHARED" BY REFERENCE HSS-F_IDLST
                "1" "HSS-KEY" BY REFERENCE HSS-KEY
           END-IF.
       M-70.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-LIST = 5
               IF  HSS-PRC NOT = 0
                   GO TO M-70
               END-IF
           END-IF
           IF  W-LIST = 1
               IF  W-NG > HSS-NG
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  1 = HSS-BHC AND HSS-HHC
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  0 = HSS-HHC AND HSS-BHC
                   IF  HSS-NG NOT = W-BNG AND W-HNG
                       GO TO M-70
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF (HSS-HHC = 0) AND (HSS-BHC = 1)
                   IF  HSS-NG NOT = W-HNG
                       GO TO M-70
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF (HSS-HHC = 1) AND (HSS-BHC = 0)
                   IF  HSS-NG NOT = W-BNG
                       GO TO M-70
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSS-HCD >= 999900
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSS-HHC = 1
                   GO TO M-70
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSS-NG NOT = W-HNG
                   GO TO M-70
               END-IF
           END-IF
           MOVE 1 TO W-PC.
           CALL "PR_Open" RETURNING RESP.
           IF  JS-SIGN = 0
               MOVE  "＊＊＊　　製品仕入　入力リスト　　＊＊＊　"
                                                              TO H-MID
           ELSE
               MOVE  "＊＊＊　　製品仕入　未変換リスト　　＊＊＊"
                                                              TO H-MID
           END-IF
           PERFORM MID-020 THRU MID-EX.
           MOVE ZERO TO W-DNO.
       M-75.
           PERFORM MEI-RTN THRU MEI-EX.
           IF  JS-SIGN = 1 OR 2
               GO TO M-80
           END-IF
           IF  HSS-PRC = 1
               GO TO M-80
           END-IF
           MOVE 1 TO HSS-PRC.
      *           REWRITE HSS-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            HSS-F_PNAME1 HSS-F_LNAME HSS-R RETURNING RET.
           IF  RET = 1
               MOVE "***  HSSF REWRITE ｴﾗｰ  ***    " TO W-EM
               CALL "SD_Output" USING "E-STAT" E-STAT "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO M-90
           END-IF.
       M-80.
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO M-90
           END-IF
           IF  W-LIST = 5
               IF  HSS-PRC NOT = 0
                   GO TO M-80
               END-IF
           END-IF
           IF  W-LIST = 1
               IF  W-NG > HSS-NG
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  1 = HSS-BHC AND HSS-HHC
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF  0 = HSS-HHC AND HSS-BHC
                   IF  HSS-NG NOT = W-BNG AND W-HNG
                       GO TO M-80
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF (HSS-HHC = 0) AND (HSS-BHC = 1)
                   IF  HSS-NG NOT = W-HNG
                       GO TO M-80
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 1
               IF (HSS-HHC = 1) AND (HSS-BHC = 0)
                   IF  HSS-NG NOT = W-BNG
                       GO TO M-80
                   END-IF
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSS-HCD >= 999900
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSS-HHC = 1
                   GO TO M-80
               END-IF
           END-IF
           IF  JS-SIGN = 2
               IF  HSS-NG NOT = W-HNG
                   GO TO M-80
               END-IF
           END-IF
           GO TO M-75.
       M-90.
           IF  JS-SIGN = 0
               CALL "DB_F_Close" USING
                BY REFERENCE JCON_IDLST JCON_PNAME1
               CALL "DB_F_Close" USING
                BY REFERENCE KBNO-M_IDLST KBNO-M_PNAME1
           END-IF
           CALL "DB_F_Close" USING BY REFERENCE S-M_IDLST S-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE J-M_IDLST J-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HI-M_IDLST HI-M_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HSHF_IDLST HSHF_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE HSS-F_IDLST HSS-F_PNAME1.
           IF  W-PC NOT = ZERO
               CALL "PR_Close" RETURNING RESP
           END-IF.
       M-95.
           CALL "SD_Output" USING "C-CLEAR" C-CLEAR "p" RETURNING RESU.
           CALL "DB_Close".
           STOP RUN.
       ACP-RTN.
           IF  W-ACT = 2 OR 3
               GO TO ACP-040
           END-IF
           IF  W-ACT NOT = 1
               GO TO ACP-020
           END-IF
           CALL "SD_Screen_Output" USING "SCBD11" RETURNING RESU.
           MOVE ZERO TO W-AID.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           IF  W-CD NOT = ZERO
               CALL "SD_Output" USING "A-CD" A-CD "p" RETURNING RESU
           END-IF
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           GO TO ACP-120.
       ACP-020.
           CALL "SD_Accept" USING BY REFERENCE A-ACT "A-ACT" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-020
           END-IF
           IF  W-ACT = 9
               GO TO ACP-EX
           END-IF
           IF  W-ACT < 1 OR > 3
               GO TO ACP-020
           END-IF
           IF  W-ACT = 1
               CALL "SD_Output" USING "S-DNO" S-DNO "p" RETURNING RESU
               GO TO ACP-080
           END-IF.
       ACP-040.
           CALL "SD_Screen_Output" USING "SCBD11" RETURNING RESU.
           CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU.
       ACP-060.
           CALL "SD_Accept" USING BY REFERENCE A-DNO "A-DNO" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-020
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-060
           END-IF
           MOVE SPACE TO HSS-KEY.
           MOVE W-DNO TO HSS-DNO.
      *           START HSS-F KEY NOT < HSS-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            HSS-F_PNAME1 "HSS-KEY" " NOT < " HSS-KEY RETURNING RET.
           IF  RET = 1
               MOVE "***  DATA ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "***  DATA ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           IF  W-DNO NOT = HSS-DNO
               MOVE "***  DATA ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           IF  HSS-HKC NOT = 0
               MOVE "***  ｴｲｷﾞｮｳ ﾍﾝｶﾝ ｽﾞﾐ  ***     " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               GO TO ACP-060
           END-IF
           MOVE HSS-BHC TO W-NC.
           PERFORM SET-RTN THRU SET-EX.
           PERFORM DSP1-RTN THRU DSP1-EX.
           PERFORM DSP2-RTN THRU DSP2-EX.
           IF  W-ACT = 3
               GO TO ACP-780
           END-IF
           IF  W-ACT = 2
               IF  W-NC = 1
                   GO TO ACP-200
               END-IF
           END-IF.
       ACP-080.
           IF  W-ACT = 1
               MOVE ZERO TO W-AID
               CALL "SD_Screen_Output" USING "SCBD11" RETURNING RESU
               CALL "SD_Output" USING "A-ACT" A-ACT "p" RETURNING RESU
           END-IF
           CALL "SD_Accept" USING BY REFERENCE A-DATE "A-DATE" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 1
                   GO TO ACP-020
               ELSE
                   GO TO ACP-060
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-080
           END-IF
           IF (W-GET < 1 OR > 12) OR (W-PEY < 1 OR > 31)
               GO TO ACP-080
           END-IF
           MOVE ZERO TO W-NEN1.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF
           IF  W-NG < W-BNG OR < W-HNG
               GO TO ACP-080
           END-IF
           IF  W-NG = W-BNG
               IF  HIZUKE NOT < W-PEY
                   GO TO ACP-080
               END-IF
           END-IF
           IF  W-NGP > W-ENGP
               GO TO ACP-080
           END-IF
           MOVE W-NGP TO W-DATE.
       ACP-100.
           CALL "SD_Accept" USING BY REFERENCE A-CD "A-CD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-100
           END-IF
           IF  W-CDD = ZERO
               CALL "SD_Output" USING "S-CD" S-CD "p" RETURNING RESU
               GO TO ACP-120
           END-IF
           IF (W-CGET < 1 OR > 12) OR (W-CPEY < 1 OR > 31)
               GO TO ACP-100
           END-IF.
       ACP-120.
           CALL "SD_Accept" USING BY REFERENCE A-SCD "A-SCD" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-080
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-120
           END-IF
           MOVE W-SCDD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ｼｲﾚｻｷ ﾅｼ  ***            " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-120
           END-IF
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           IF  W-SCDD < 5000
               GO TO ACP-120
           END-IF
           MOVE W-NGP TO W-DATE.
           MOVE W-CDD TO W-CD.
           MOVE W-SCDD TO W-SCD.
       ACP-160.
           CALL "SD_Accept" USING BY REFERENCE A-JCD "A-JCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-120
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-160
           END-IF
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ｻﾞｲﾘｮｳ ﾅｼ  ***           " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-160
           END-IF
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           IF  W-JCD < 790000 OR > 794999
               GO TO ACP-160
           END-IF
           IF  J-YC NOT = 4
               GO TO ACP-160
           END-IF
           IF  J-SC NOT = 1 AND 2
               GO TO ACP-160
           END-IF
           IF  J-BKC NOT = 22 AND 23 AND 24 AND 26 AND 29
               GO TO ACP-160
           END-IF.
       ACP-180.
           CALL "SD_Accept" USING BY REFERENCE A-SUT "A-SUT" "S9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-160
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-180
           END-IF
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
       ACP-200.
           MOVE 0 TO W-SNO.
       ACP-220.
           ADD 1 TO W-SNO.
           IF  W-SNO = 10
               GO TO ACP-720
           END-IF
           CALL "SD_Output" USING "A-SNO" A-SNO "p" RETURNING RESU.
           GO TO ACP-300.
       ACP-240.
           CALL "SD_Accept" USING BY REFERENCE A-SNO "A-SNO" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT NOT = PF1
               GO TO ACP-260
           END-IF
           PERFORM DEL-RTN THRU DEL-EX.
           GO TO ACP-300.
       ACP-260.
           IF  ESTAT NOT = PF2
               GO TO ACP-280
           END-IF
           IF (W-SKC(9) NOT = ZERO) OR (W-HCD(9) NOT = ZERO)
               MOVE "***  ｿｳﾆｭｳ ﾌｶ  ***            " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-240
           END-IF
           PERFORM INS-RTN THRU INS-EX.
           GO TO ACP-300.
       ACP-280.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               SUBTRACT 1 FROM W-SNO
               IF  W-SNO NOT = ZERO
                   GO TO ACP-300
               ELSE
                   IF  W-ACT NOT = 2
                       GO TO ACP-180
                   ELSE
                       IF  W-NC = 0
                           GO TO ACP-180
                       ELSE
                           GO TO ACP-060
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-240
           END-IF.
       ACP-300.
           PERFORM DSP2-RTN THRU DSP2-EX.
           IF  W-SNO NOT = 1
               IF  W-SKC(W-SNO) = 0
                   MOVE W-SKCD TO W-SKC(W-SNO)
                   CALL "SD_Output" USING
                    "A-SKC" A-SKC "p" RETURNING RESU
               END-IF
           END-IF.
       ACP-320.
           CALL "SD_Accept" USING BY REFERENCE A-RSN "A-RSN" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-240
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-320
           END-IF
           IF  W-RSN(W-SNO) = ZERO
               MOVE ZERO TO W-RNO(W-SNO) W-AZSUD(W-SNO)
               CALL "SD_Output" USING "S-RNO" S-RNO "p" RETURNING RESU
               GO TO ACP-380
           END-IF.
       ACP-340.
           CALL "SD_Accept" USING BY REFERENCE A-RNG "A-RNG" "9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-320
           END-IF
           IF  ESTAT NOT = NOC AND HTB AND SKP
               GO TO ACP-340
           END-IF
           IF  W-RNG(W-SNO) = ZERO
               GO TO ACP-340
           END-IF.
       ACP-360.
           CALL "SD_Accept" USING BY REFERENCE A-RND "A-RND" "9" "2"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-340
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-360
           END-IF
           IF  W-RND(W-SNO) = ZERO
               GO TO ACP-360
           END-IF
           PERFORM RTC-RTN THRU RTC-EX.
           IF  W-NO NOT = 10
               MOVE "***  ﾆｼﾞｭｳ ﾛｯﾄNO  ***         " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-320
           END-IF
      *
           MOVE W-RNO(W-SNO) TO HSH-KEY.
      *           READ HSHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ﾊｯﾁｭｳNO ﾅｼ  ***          " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-320
           END-IF
           IF  W-SCDD NOT = HSH-SCD
               MOVE "***  ｼｲﾚｻｷ ｴﾗｰ  ***           " TO W-EM
               MOVE SPACE TO W-CO
               MOVE HSH-SCD TO W-CO
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-CO" E-CO "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-320
           END-IF
           IF  HSH-ENGP NOT = ZERO
               MOVE "***  ｶﾝﾘｮｳ ｽﾞﾐ  ***           " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ENGP" E-ENGP "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF
           MOVE HSH-HCD TO W-HCD(W-SNO).
           PERFORM ZAN-RTN THRU ZAN-EX.
           PERFORM DSP2-RTN THRU DSP2-EX.
           IF  W-INV = 1
               GO TO ACP-320
           END-IF.
       ACP-380.
           CALL "SD_Accept" USING BY REFERENCE A-SKC "A-SKC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-320
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-380
           END-IF.
       ACP-400.
           IF  W-SKC(W-SNO) = 9
               GO TO ACP-380
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-SKC(W-SNO) TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ｿｳｺ ﾅｼ  ***              " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-380
           END-IF
           CALL "SD_Output" USING "A-SKC" A-SKC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SKN" D-SKN "p" RETURNING RESU.
           IF  W-SNO = 1
               MOVE W-SKC(W-SNO) TO W-SKCD
           END-IF
           IF  W-RNO(W-SNO) NOT = ZERO
               GO TO ACP-440
           END-IF.
       ACP-420.
           CALL "SD_Accept" USING BY REFERENCE A-HCD "A-HCD" "9" "6"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-380
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-420
           END-IF
           IF  W-HCD(W-SNO) > 999899
               GO TO ACP-420
           END-IF
           MOVE W-HCD(W-SNO) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "***  ﾋﾝﾒｲ ﾅｼ  ***             " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME98" E-ME98 "p" RETURNING RESU
               GO TO ACP-420
           END-IF
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           IF  HI-BC3 = 30
               IF  HI-HCD NOT = HI-MHCD
                   GO TO ACP-420
               END-IF
           END-IF
           IF  HI-BC22 = 1
               MOVE "***  ﾆｭｳｺｸﾌﾞﾝ ｴﾗｰ  ***        " TO W-EM
               CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
               CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU
               CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
           END-IF.
       ACP-440.
           IF  HI-BC1 = 26
               IF  W-RNO(W-SNO) = ZERO
                   MOVE "***  ｾｲｷｮｳ ﾛｯﾄNO ﾅｼ  ***      " TO W-EM
                   CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           IF  HI-BC1 NOT = 26
               IF  W-RNO(W-SNO) NOT = ZERO
                   MOVE "***  ﾛｯﾄNO ｴﾗｰ  ***           " TO W-EM
                   CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU
                   CALL "SD_Output" USING
                    "E-ME99" E-ME99 "p" RETURNING RESU
                   CALL "SD_Output" USING "E-CL" E-CL "p" RETURNING RESU
               END-IF
           END-IF
           MOVE 0 TO HI-S4(10).
       ACP-460.
           MOVE 11 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           MOVE ZERO TO W-GC W-SKP.
       ACP-480.
           ADD 1 TO W-GC.
           ADD 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-GC = 5
               GO TO ACP-580
           END-IF
           MOVE ZERO TO W-ASSD.
           IF  W-GC = 1
               MOVE HI-SS(2) TO W-ASSD
           END-IF
           IF  W-GC = 2
               MOVE HI-SS(3) TO W-ASSD
           END-IF
           IF  W-GC = 3
               MOVE HI-SS(4) TO W-ASSD
           END-IF
           IF  W-GC = 4
               MOVE HI-SS(1) TO W-ASSD
           END-IF
           IF  W-ASSD = ZERO
               GO TO ACP-480
           END-IF
           MOVE ZERO TO W-SC W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
       ACP-500.
           ADD 1 TO W-SC.
           ADD 6 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-SC > 10
               GO TO ACP-480
           END-IF
           IF  W-SS(W-SC) = 0
               MOVE ZERO TO W-SU(W-SNO,W-GC,W-SC)
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO ACP-500
           END-IF
           IF  W-SKP NOT = 0
               GO TO ACP-500
           END-IF.
       ACP-520.
           MOVE W-SU(W-SNO,W-GC,W-SC) TO W-BSU.
           CALL "SD_Accept" USING BY REFERENCE A-SU "A-SU" "S9" "4"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = PF6
               MOVE 1 TO W-SKP
               MOVE W-BSU TO W-SU(W-SNO,W-GC,W-SC)
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO ACP-500
           END-IF
           IF  ESTAT = ADV
               MOVE 2 TO W-SKP
               MOVE W-BSU TO W-SU(W-SNO,W-GC,W-SC)
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO ACP-500
           END-IF
           IF  ESTAT = BTB
               GO TO ACP-540
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-520
           END-IF
           CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU.
           IF  W-RNO(W-SNO) NOT = ZERO
               IF  HSH-ENGP NOT = ZERO
                   IF  W-SU(W-SNO,W-GC,W-SC) > 0
                       MOVE "***  ｶﾝﾘｮｳ ｽﾞﾐ  ***           " TO W-EM
                       CALL "SD_Output" USING
                        "E-ME" E-ME "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ENGP" E-ENGP "p" RETURNING RESU
                       CALL "SD_Output" USING
                        "E-ME98" E-ME98 "p" RETURNING RESU
                       GO TO ACP-520
                   END-IF
               END-IF
           END-IF
           GO TO ACP-500.
       ACP-540.
           SUBTRACT 1 FROM W-SC.
           SUBTRACT 6 FROM W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-SC NOT = ZERO
               IF  W-SS(W-SC) = 0
                   GO TO ACP-540
               ELSE
                   GO TO ACP-520
               END-IF
           END-IF.
       ACP-560.
           SUBTRACT 1 FROM W-GC.
           SUBTRACT 2 FROM W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-GC = 0
               IF  W-RNO(W-SNO) NOT = ZERO
                   GO TO ACP-380
               ELSE
                   GO TO ACP-420
               END-IF
           END-IF
           MOVE ZERO TO W-ASSD.
           IF  W-GC = 1
               MOVE HI-SS(2) TO W-ASSD
           END-IF
           IF  W-GC = 2
               MOVE HI-SS(3) TO W-ASSD
           END-IF
           IF  W-GC = 3
               MOVE HI-SS(4) TO W-ASSD
           END-IF
           IF  W-GC = 4
               MOVE HI-SS(1) TO W-ASSD
           END-IF
           IF  W-ASSD = ZERO
               GO TO ACP-560
           END-IF
           MOVE 11 TO W-SC.
           MOVE 66 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           GO TO ACP-540.
       ACP-580.
           MOVE ZERO TO W-GC W-TSU W-ZC.
       ACP-600.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               GO TO ACP-640
           END-IF
           MOVE ZERO TO W-SC.
       ACP-620.
           ADD 1 TO W-SC.
           IF  W-SC = 11
               GO TO ACP-600
           END-IF
           ADD W-SU(W-SNO,W-GC,W-SC) TO W-TSU.
           IF  W-ZC = 0
               IF  W-SU(W-SNO,W-GC,W-SC) NOT = ZERO
                   MOVE 1 TO W-ZC
               END-IF
           END-IF
           GO TO ACP-620.
       ACP-640.
           CALL "SD_Output" USING "D-TSU" D-TSU "p" RETURNING RESU.
           IF  W-RNO(W-SNO) NOT = ZERO
               COMPUTE W-KIN = W-TSU * HSH-T
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           IF  W-ZC = 0
               GO TO ACP-460
           END-IF
           PERFORM SUM-RTN THRU SUM-EX.
           CALL "SD_Output" USING "D-SUTD" D-SUTD "p" RETURNING RESU.
           IF  W-SKP = 2
               GO TO ACP-720
           END-IF
           IF  W-RNO(W-SNO) = ZERO
               MOVE 0 TO W-HPC(W-SNO) W-KRC(W-SNO)
               CALL "SD_Output" USING "S-HPC" S-HPC "p" RETURNING RESU
               CALL "SD_Output" USING "S-KRC" S-KRC "p" RETURNING RESU
               GO TO ACP-700
           END-IF
           IF  W-ACT NOT = 1
               GO TO ACP-660
           END-IF
           IF  W-KRC(W-SNO) = 0
               IF  W-ASUD(W-SNO) = W-AZSUD(W-SNO)
                   MOVE 1 TO W-KRC(W-SNO)
               END-IF
           END-IF
           IF  W-KRC(W-SNO) = 1
               IF  W-ASUD(W-SNO) NOT = W-AZSUD(W-SNO)
                   MOVE 0 TO W-KRC(W-SNO)
               END-IF
           END-IF
           CALL "SD_Output" USING "A-KRC" A-KRC "p" RETURNING RESU.
       ACP-660.
           CALL "SD_Accept" USING BY REFERENCE A-HPC "A-HPC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-560
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-660
           END-IF
           IF  W-HPC(W-SNO) > 1
               GO TO ACP-660
           END-IF.
       ACP-680.
           CALL "SD_Accept" USING BY REFERENCE A-KRC "A-KRC" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               GO TO ACP-660
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-680
           END-IF
           IF  W-KRC(W-SNO) > 1
               GO TO ACP-680
           END-IF.
       ACP-700.
           CALL "SD_Accept" USING BY REFERENCE A-CHK "A-CHK" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "S-CHK" S-CHK "p" RETURNING RESU.
           IF  ESTAT = ADV
               GO TO ACP-720
           END-IF
           IF  ESTAT = BTB
               IF  W-RNO(W-SNO) NOT = ZERO
                   GO TO ACP-680
               ELSE
                   GO TO ACP-560
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-700
           END-IF
           IF  W-ACT = 1
               IF  W-SUT = W-SUTD
                   GO TO ACP-780
               END-IF
           END-IF
           GO TO ACP-220.
       ACP-720.
           PERFORM SUM-RTN THRU SUM-EX.
           CALL "SD_Output" USING "D-SUTD" D-SUTD "p" RETURNING RESU.
           IF  W-SUT = W-SUTD
               GO TO ACP-780
           END-IF
           MOVE "***  ｽｳﾘｮｳ ｺﾞｳｹｲ ｴﾗｰ  ***     " TO W-EM.
           CALL "SD_Output" USING "E-ME" E-ME "p" RETURNING RESU.
           CALL "SD_Output" USING "E-ME99" E-ME99 "p" RETURNING RESU.
       ACP-740.
           MOVE 10 TO W-SNO.
       ACP-760.
           SUBTRACT 1 FROM W-SNO.
           IF  W-SNO = ZERO
               IF  W-ACT NOT = 2
                   GO TO ACP-180
               ELSE
                   IF  W-NC = 0
                       GO TO ACP-180
                   ELSE
                       GO TO ACP-060
                   END-IF
               END-IF
           END-IF
           IF (W-SKC(W-SNO) = ZERO) AND (W-HCD(W-SNO) = ZERO)
               GO TO ACP-760
           END-IF
           GO TO ACP-300.
       ACP-780.
           CALL "SD_Accept" USING BY REFERENCE A-DMM "A-DMM" "9" "1"
            BY REFERENCE ESTAT RETURNING RESU.
           IF  ESTAT = BTB
               IF  W-ACT = 3
                   GO TO ACP-060
               ELSE
                   GO TO ACP-740
               END-IF
           END-IF
           IF  ESTAT NOT = HTB AND SKP
               GO TO ACP-780
           END-IF
           IF  W-DMM = 9
               IF  W-ACT = 3
                   GO TO ACP-060
               ELSE
                   IF  W-ACT NOT = 2
                       GO TO ACP-120
                   ELSE
                       IF  W-NC = 0
                           GO TO ACP-120
                       ELSE
                           GO TO ACP-060
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF  W-DMM NOT = 1
               GO TO ACP-780
           END-IF.
       ACP-EX.
           EXIT.
       SET-RTN.
           MOVE ZERO TO W-AID W-PRC.
      *
           MOVE HSS-DATE TO W-DATE W-NGP.
           MOVE HSS-CD TO W-CD W-CDD.
           MOVE HSS-SCD TO W-SCD W-SCDD.
           MOVE HSS-JCD TO W-JCD.
           MOVE HSS-SUT TO W-SUT.
       SET-020.
           MOVE HSS-RNO TO W-RNO(HSS-SNO).
           MOVE HSS-SKC TO W-SKC(HSS-SNO).
           MOVE HSS-HCD TO W-HCD(HSS-SNO).
           MOVE HSS-ASU(1) TO W-ASU(HSS-SNO,4).
           MOVE HSS-ASU(2) TO W-ASU(HSS-SNO,1).
           MOVE HSS-ASU(3) TO W-ASU(HSS-SNO,2).
           MOVE HSS-ASU(4) TO W-ASU(HSS-SNO,3).
           MOVE HSS-HPC TO W-HPC(HSS-SNO).
           MOVE HSS-KRC TO W-KRC(HSS-SNO).
           IF  W-PRC = 0
               IF  HSS-PRC = 0
                   MOVE 1 TO W-PRC
               END-IF
           END-IF
      *
      *           READ HSS-F NEXT RECORD AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT RECORD AT END" HSS-F_PNAME1 BY REFERENCE HSS-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO SET-EX
           END-IF
           IF  W-DNO = HSS-DNO
               GO TO SET-020
           END-IF.
       SET-EX.
           EXIT.
       DSP1-RTN.
           MOVE W-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "＊＊＊　仕入先　なし　　＊＊＊" TO S-NAME
           END-IF
      *
           MOVE W-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "＊＊＊　材料　なし　　＊＊＊　" TO J-NAME
           END-IF
      *
           CALL "SD_Output" USING "A-DATE" A-DATE "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SCD" A-SCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-JCD" A-JCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-JNA" D-JNA "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SUT" D-SUT "p" RETURNING RESU.
           IF  W-CD NOT = ZERO
               CALL "SD_Output" USING "A-CD" A-CD "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "S-CD" S-CD "p" RETURNING RESU
           END-IF
           MOVE 1 TO W-SNO.
       DSP1-EX.
           EXIT.
       DSP2-RTN.
           IF  ZERO = W-SKC(W-SNO) AND W-HCD(W-SNO)
               CALL "SD_Output" USING "S-HD" S-HD "p" RETURNING RESU
               GO TO DSP2-EX
           END-IF
           MOVE 3 TO JCON3-01.
           MOVE W-SKC(W-SNO) TO JCON3-02.
      *           READ JCON WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" JCON_PNAME1 BY REFERENCE JCON3-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE "倉庫　なし　" TO JCON3-03
           END-IF
      *
           MOVE 0 TO W-INV.
           MOVE W-HCD(W-SNO) TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1 TO W-INV
               MOVE SPACE TO HI-NAME
               MOVE "＊＊＊　品名　なし　　＊＊＊　" TO HI-NAME
           END-IF
      *
           CALL "SD_Output" USING "A-SNO" A-SNO "p" RETURNING RESU.
           CALL "SD_Output" USING "A-SKC" A-SKC "p" RETURNING RESU.
           CALL "SD_Output" USING "D-SNA" D-SNA "p" RETURNING RESU.
           CALL "SD_Output" USING "A-HCD" A-HCD "p" RETURNING RESU.
           CALL "SD_Output" USING "D-HNA" D-HNA "p" RETURNING RESU.
           IF  W-RNO(W-SNO) = ZERO
               CALL "SD_Output" USING "S-RNO" S-RNO "p" RETURNING RESU
           ELSE
               CALL "SD_Output" USING "A-RSN" A-RSN "p" RETURNING RESU
               CALL "SD_Output" USING "A-RNG" A-RNG "p" RETURNING RESU
               CALL "SD_Output" USING "A-RND" A-RND "p" RETURNING RESU
               CALL "SD_Output" USING "D-T" D-T "p" RETURNING RESU
               CALL "SD_Output" USING "A-HPC" A-HPC "p" RETURNING RESU
               CALL "SD_Output" USING "A-KRC" A-KRC "p" RETURNING RESU
           END-IF
      *
           MOVE ZERO TO W-GC W-TSU.
           MOVE 11 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
       DSP2-020.
           ADD 1 TO W-GC.
           ADD 2 TO W-L.
           CALL "SD_Arg_Match_Line" USING "W-L" "2" W-L RETURNING RESU.
           IF  W-GC NOT = 5
               MOVE ZERO TO W-SC W-C
               CALL "SD_Arg_Match_Col" USING
                "W-C" "2" W-C RETURNING RESU
               GO TO DSP2-040
           END-IF
           CALL "SD_Output" USING "D-TSU" D-TSU "p" RETURNING RESU.
           IF  W-RNO(W-SNO) NOT = ZERO
               COMPUTE W-KIN = W-TSU * HSH-T
           END-IF
           CALL "SD_Output" USING "D-KIN" D-KIN "p" RETURNING RESU.
           GO TO DSP2-EX.
       DSP2-040.
           ADD 1 TO W-SC.
           ADD 6 TO W-C.
           CALL "SD_Arg_Match_Col" USING "W-C" "2" W-C RETURNING RESU.
           IF  W-SC < 11
               ADD W-SU(W-SNO,W-GC,W-SC) TO W-TSU
               CALL "SD_Output" USING "D-SU" D-SU "p" RETURNING RESU
               GO TO DSP2-040
           END-IF
           GO TO DSP2-020.
       DSP2-EX.
           EXIT.
       RTC-RTN.
           MOVE ZERO TO W-NO.
       RTC-020.
           ADD 1 TO W-NO.
           IF  W-NO = 10
               GO TO RTC-EX
           END-IF
           IF  W-NO = W-SNO
               GO TO RTC-020
           END-IF
           IF  W-RNO(W-NO) NOT = W-RNO(W-SNO)
               GO TO RTC-020
           END-IF.
       RTC-EX.
           EXIT.
       ZAN-RTN.
           IF  HSH-ENGP NOT = ZERO
               MOVE 1 TO W-KRC(W-SNO)
           END-IF
           IF  W-ASUD(W-SNO) NOT = ZERO
               GO TO ZAN-EX
           END-IF
           MOVE ZERO TO W-SD.
       ZAN-020.
           ADD 1 TO W-SD.
           IF  W-SD > 4
               GO TO ZAN-EX
           END-IF
           IF  W-SD = 1
               MOVE 4 TO W-S
           END-IF
           IF  W-SD = 2
               MOVE 1 TO W-S
           END-IF
           IF  W-SD = 3
               MOVE 2 TO W-S
           END-IF
           IF  W-SD = 4
               MOVE 3 TO W-S
           END-IF
           MOVE ZERO TO CNT.
       ZAN-040.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO ZAN-020
           END-IF
           COMPUTE W-SU(W-SNO,W-S,CNT) = HSH-HSU(W-SD,CNT) -
                              HSH-NSU(W-SD,CNT) - HSH-ISU(W-SD,CNT).
           MOVE W-SU(W-SNO,W-S,CNT) TO W-ZSU(W-SNO,W-S,CNT).
           GO TO ZAN-040.
       ZAN-EX.
           EXIT.
       DEL-RTN.
           MOVE W-SNO TO W-SNOF W-SNOR.
           ADD 1 TO W-SNOF.
       DEL-020.
           IF  W-SNOF NOT = 10
               MOVE W-ID(W-SNOF) TO W-ID(W-SNOR)
               ADD 1 TO W-SNOF W-SNOR
               GO TO DEL-020
           END-IF
           MOVE ZERO TO W-ID(W-SNOR).
       DEL-EX.
           EXIT.
       INS-RTN.
           MOVE W-SNO TO W-SNOF W-SNOR.
           ADD 1 TO W-SNOF.
       INS-020.
           IF  W-SNOF NOT = 10
               MOVE W-ID(W-SNOR) TO W-ID(W-SNOF)
               ADD 1 TO W-SNOF W-SNOR
               GO TO INS-020
           END-IF
           MOVE ZERO TO W-ID(W-SNO).
       INS-EX.
           EXIT.
       SUM-RTN.
           MOVE ZERO TO W-SUTD W-SNOD.
       SUM-020.
           ADD 1 TO W-SNOD.
           IF  W-SNOD = 10
               GO TO SUM-EX
           END-IF
           MOVE ZERO TO W-GC W-ZC.
       SUM-040.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               GO TO SUM-080
           END-IF
           MOVE ZERO TO W-SC.
       SUM-060.
           ADD 1 TO W-SC.
           IF  W-SC = 11
               GO TO SUM-040
           END-IF
           ADD W-SU(W-SNOD,W-GC,W-SC) TO W-SUTD.
           IF  W-ZC = 0
               IF  W-SU(W-SNOD,W-GC,W-SC) NOT = ZERO
                   MOVE 1 TO W-ZC
               END-IF
           END-IF
           GO TO SUM-060.
       SUM-080.
           IF  W-ZC = 0
               IF (W-SKC(W-SNOD) NOT = ZERO) OR
                                  (W-HCD(W-SNOD) NOT = ZERO)
                   MOVE ZERO TO W-ID(W-SNOD)
               END-IF
           END-IF
           IF  W-ZC = 1
               IF (W-SKC(W-SNOD) = ZERO) OR (W-HCD(W-SNOD) = ZERO)
                   MOVE ZERO TO W-ID(W-SNOD)
               END-IF
           END-IF
           GO TO SUM-020.
       SUM-EX.
           EXIT.
       SUB-RTN.
           IF  HSS-KRC = 1
               MOVE ZERO TO HSH-ENGP
           END-IF
           MOVE ZERO TO W-SD HSH-ICHK.
       SUB-020.
           ADD 1 TO W-SD.
           IF  W-SD > 4
               GO TO SUB-EX
           END-IF
           MOVE ZERO TO CNT.
       SUB-040.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO SUB-020
           END-IF
           IF  HSS-SU(W-SD,CNT) NOT = ZERO
               SUBTRACT HSS-SU(W-SD,CNT) FROM HSH-ISU(W-SD,CNT)
           END-IF
           IF  HSH-ICHK = 0
               IF  HSH-ISU(W-SD,CNT) NOT = ZERO
                   MOVE 1 TO HSH-ICHK
               END-IF
           END-IF
           GO TO SUB-040.
       SUB-EX.
           EXIT.
       ADD-RTN.
           IF  HSS-KRC = 1
               MOVE HSS-NGPS TO HSH-ENGP
           END-IF
           MOVE ZERO TO W-SD HSH-ICHK.
       ADD-020.
           ADD 1 TO W-SD.
           IF  W-SD > 4
               GO TO ADD-EX
           END-IF
           MOVE ZERO TO CNT.
       ADD-040.
           ADD 1 TO CNT.
           IF  CNT > 10
               GO TO ADD-020
           END-IF
           IF  HSS-SU(W-SD,CNT) NOT = ZERO
               ADD HSS-SU(W-SD,CNT) TO HSH-ISU(W-SD,CNT)
           END-IF
           IF  HSH-ICHK = 0
               IF  HSH-ISU(W-SD,CNT) NOT = ZERO
                   MOVE 1 TO HSH-ICHK
               END-IF
           END-IF
           GO TO ADD-040.
       ADD-EX.
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
       PRI1-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER > 58
               PERFORM MID-RTN THRU MID-EX
           END-IF
           MOVE SPACE TO SP-R.
           MOVE W-P1 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI1-EX.
           EXIT.
       PRI2-RTN.
           CALL "PR_Get_Linage" RETURNING LINAGECOUNTER.
           IF  LINAGECOUNTER NOT > 59
               GO TO PRI2-020
           END-IF
           MOVE HSS-SNO TO P-SNO.
           IF  HSS-RNO NOT = ZERO
               MOVE "-" TO P-V1 P-V2
               MOVE HSS-RSN TO P-RSN
               MOVE HSS-RNG TO P-RNG
               MOVE HSS-RND TO P-RND
           END-IF
           MOVE HSS-HPC TO P-HPC.
           MOVE HSS-KRC TO P-KRC.
           MOVE HSS-SKC TO P-SKC.
           MOVE HSS-HCD TO P-HCD.
           MOVE HI-NAME TO P-HNA.
           PERFORM MID-RTN THRU MID-EX.
           PERFORM PRI1-RTN THRU PRI1-EX.
       PRI2-020.
           MOVE SPACE TO SP-R.
           MOVE W-P2 TO SP-R.
           CALL "PR_Write" USING SP-R RETURNING RESP.
           MOVE SPACE TO SP-R.
       PRI2-EX.
           EXIT.
       MEI-RTN.
           IF  HSS-DNO = W-DNO
               GO TO MEI-020
           END-IF
           MOVE HSS-DNO TO W-DNO.
      *
           MOVE HSS-SCD TO S-KEY.
      *           READ S-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" S-M_PNAME1 BY REFERENCE S-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO S-NAME
               MOVE "　＊＊　仕入先マスター　なし　＊＊" TO S-NAME
           END-IF
      *
           MOVE HSS-JCD TO J-KEY.
      *           READ J-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" J-M_PNAME1 BY REFERENCE J-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO J-NAME
               MOVE "＊＊＊　材料　なし　　＊＊＊　" TO J-NAME
           END-IF
      *
           MOVE SPACE TO W-P1.
           MOVE SPACE TO P-BH P-HH P-HK.
           MOVE HSS-DATE TO W-DATE.
           MOVE HSS-NGPS TO P-DATE.
           MOVE HSS-DNO TO P-DNO.
           MOVE HSS-SCD TO P-SCD.
           MOVE S-NAME TO P-SNA.
           MOVE HSS-JCD TO P-JCD.
           MOVE J-NAME TO P-JNA.
           MOVE "(" TO P-F.
           MOVE HSS-SUT TO P-SUT.
           MOVE ")" TO P-R.
           IF  HSS-CD NOT = ZERO
               MOVE HSS-CD TO P-CD
           END-IF
           IF  HSS-BHC = 0
               MOVE "　未" TO P-BH
           ELSE
               MOVE "　済" TO P-BH
           END-IF
           IF  HSS-HHC = 0
               MOVE "　未" TO P-HH
           ELSE
               MOVE "　済" TO P-HH
           END-IF
           IF  HSS-HKC = 0
               MOVE "　未" TO P-HK
           ELSE
               MOVE "　済" TO P-HK
           END-IF
           PERFORM PRI1-RTN THRU PRI1-EX.
       MEI-020.
           MOVE HSS-HCD TO HI-KEY.
      *           READ HI-M WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HI-M_PNAME1 BY REFERENCE HI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE TO HI-NAME
               MOVE "＊＊＊　品名　なし　　＊＊＊　" TO HI-NAME
           END-IF
      *
           IF  HSS-RNO = ZERO
               GO TO MEI-030
           END-IF
           MOVE HSS-RNO TO HSH-KEY.
      *           READ HSHF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HSHF_PNAME1 BY REFERENCE HSH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO HSH-T
           END-IF.
       MEI-030.
           MOVE ZERO TO W-ASUD(HSS-SNO) W-ADC W-ZCF W-ZCR.
           MOVE 1 TO W-GC.
       MEI-040.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               MOVE 1 TO W-GC
           END-IF
           IF  ZERO = HSS-SU(W-GC,01) AND HSS-SU(W-GC,02) AND
                      HSS-SU(W-GC,03) AND HSS-SU(W-GC,04) AND
                      HSS-SU(W-GC,05) AND HSS-SU(W-GC,06) AND
                      HSS-SU(W-GC,07) AND HSS-SU(W-GC,08) AND
                      HSS-SU(W-GC,09) AND HSS-SU(W-GC,10)
               IF  W-GC = 1
                   GO TO MEI-060
               ELSE
                   GO TO MEI-040
               END-IF
           END-IF
           IF  W-GC = 2
               MOVE 1 TO W-ZCF W-ZCR
               MOVE 1 TO W-DC(W-ZCR)
               MOVE HSS-ASU(2) TO W-ASU(HSS-SNO,1)
           END-IF
           IF  W-GC = 3
               MOVE 2 TO W-ZCR
               MOVE 1 TO W-DC(W-ZCR)
               MOVE HSS-ASU(3) TO W-ASU(HSS-SNO,2)
               IF  W-ZCF = 0
                   MOVE W-ZCR TO W-ZCF
               END-IF
           END-IF
           IF  W-GC = 4
               MOVE 3 TO W-ZCR
               MOVE 1 TO W-DC(W-ZCR)
               MOVE HSS-ASU(4) TO W-ASU(HSS-SNO,3)
               IF  W-ZCF = 0
                   MOVE W-ZCR TO W-ZCF
               END-IF
           END-IF
           IF  W-GC = 1
               MOVE 4 TO W-ZCR
               MOVE 1 TO W-DC(W-ZCR)
               MOVE HSS-ASU(1) TO W-ASU(HSS-SNO,4)
               IF  W-ZCF = 0
                   MOVE W-ZCR TO W-ZCF
               END-IF
           END-IF
           IF  W-GC NOT = 1
               GO TO MEI-040
           END-IF.
       MEI-060.
           MOVE ZERO TO W-GC W-TSU.
       MEI-080.
           ADD 1 TO W-GC.
           IF  W-GC = 5
               GO TO MEI-EX
           END-IF
           IF  W-DC(W-GC) = 0
               GO TO MEI-080
           END-IF
      *
           MOVE SPACE TO W-P2.
           MOVE SPACE TO P-HNA.
           IF  W-GC = W-ZCF
               MOVE HSS-SNO TO P-SNO
               MOVE HSS-HPC TO P-HPC
               MOVE HSS-KRC TO P-KRC
               MOVE HSS-SKC TO P-SKC
               MOVE HSS-HCD TO P-HCD
               MOVE HI-NAME TO P-HNA
               IF  HSS-RNO NOT = ZERO
                   MOVE "-" TO P-V1 P-V2
                   MOVE HSS-RSN TO P-RSN
                   MOVE HSS-RNG TO P-RNG
                   MOVE HSS-RND TO P-RND
               END-IF
           END-IF
      *
           IF  W-GC = 1
               MOVE 2 TO P-SIZ
           END-IF
           IF  W-GC = 2
               MOVE 3 TO P-SIZ
           END-IF
           IF  W-GC = 3
               MOVE 4 TO P-SIZ
           END-IF
           IF  W-GC = 4
               MOVE 1 TO P-SIZ
           END-IF
      *
           MOVE ZERO TO W-SC.
       MEI-100.
           ADD 1 TO W-SC.
           IF  W-SC NOT = 11
               MOVE W-SU(HSS-SNO,W-GC,W-SC) TO P-SU(W-SC)
               ADD W-SU(HSS-SNO,W-GC,W-SC) TO W-TSU
               GO TO MEI-100
           END-IF
      *
           IF  W-GC = W-ZCR
               MOVE W-TSU TO P-TSU
               IF  HSS-RNO NOT = ZERO
                   MOVE HSH-T TO P-T
               END-IF
           END-IF
      *
           PERFORM PRI2-RTN THRU PRI2-EX.
           GO TO MEI-080.
       MEI-EX.
           EXIT.
