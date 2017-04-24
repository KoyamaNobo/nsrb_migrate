       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 PRD200.
       AUTHOR.                     OZAKI.
      *===============================================================*
      *    振替伝票入力（手形・領収書要・売掛金・買掛金関係）         *
      *                            --- 90/01/17 ---                   *
      *===============================================================*
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SOURCE-COMPUTER.           NEAC-SYSTEM100.
       OBJECT-COMPUTER.           NEAC-SYSTEM100.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
       77  ERR-STAT            PIC  X(02).
       77  WKSP                PIC  X(40) VALUE SPACE.
       77  WKNSP               PIC  N(10) VALUE SPACE.
       77  WKZERO              PIC  9(10) VALUE ZERO.
       01  CRT-WK1.
           02  CRT-ITEM    OCCURS  5.
             03  C-SIWAKE.
               04  C-TENO      PIC  9(06).
               04  C-TENOD  REDEFINES C-TENO.
                 05  C-TENO1   PIC  9(02).
                 05  C-TENO2   PIC  9(04).
               04  C-KARI.
                 05  C-KRCDM   PIC  9(04).
                 05  C-KRCDS   PIC  9(04).
                 05  C-KRSECT  PIC  9(04).
                 05  F         PIC  9(03).
                 05  C-KRTAX   PIC  X(01).
                 05  C-KRKIN   PIC S9(10).
                 05  C-KR-TB   PIC  9(02).
               04  C-KR-AREA.
                 05  C-KR-HO   PIC  9(01).
                 05  C-KR-KH   PIC  9(01).
                 05  C-KR-TAX  PIC  X(01).
                 05  C-KR-BSPL PIC  9(01).
                 05  C-KR-CDMN PIC  N(10).
                 05  C-KR-CDSN PIC  N(10).
               04  C-KASI.
                 05  C-KSCDM   PIC  9(04).
                 05  C-KSCDS   PIC  9(04).
                 05  C-KSSECT  PIC  9(04).
                 05  F         PIC  9(03).
                 05  C-KSTAX   PIC  X(01).
                 05  C-KSKIN   PIC S9(10).
                 05  C-KS-TB   PIC  9(02).
               04  C-KS-AREA.
                 05  C-KS-HO   PIC  9(01).
                 05  C-KS-KH   PIC  9(01).
                 05  C-KS-TAX  PIC  X(01).
                 05  C-KS-BSPL PIC  9(01).
                 05  C-KS-CDMN PIC  N(10).
                 05  C-KS-CDSN PIC  N(10).
               04  C-TEKICD    PIC  9(03).
               04  C-TEKI      PIC  N(20).
               04  C-TEKI1  REDEFINES C-TEKI.
                 05  C-TEKI11  PIC  N(05).
                 05  C-TEKI12  PIC  N(07).
                 05  C-TEKI13  PIC  N(08).
               04  C-TEKI2  REDEFINES C-TEKI.
                 05  C-TEKI21  PIC  N(03).
                 05  C-TEKI22  PIC  N(05).
                 05  C-TEKI23  PIC  N(12).
               04  C-KRNKCD    PIC  9(02).
               04  C-KRNKCDD  REDEFINES  C-KRNKCD.
                 05  C-KRNKCD1 PIC  9(01).
                 05  C-KRNKCD2 PIC  9(01).
               04  C-KRNSC     PIC  9(01).
               04  C-KRSKNG    PIC  9(04).
               04  C-KRSKNGD  REDEFINES  C-KRSKNG.
                 05  C-KRSKN   PIC  9(02).
                 05  C-KRSKG   PIC  9(02).
               04  C-KSNKCD    PIC  9(02).
               04  C-KSNKCDD  REDEFINES  C-KSNKCD.
                 05  C-KSNKCD1 PIC  9(01).
                 05  C-KSNKCD2 PIC  9(01).
               04  C-KSNSC     PIC  9(01).
               04  C-KSSKNG    PIC  9(04).
               04  C-KSSKNGD  REDEFINES  C-KSSKNG.
                 05  C-KSSKN   PIC  9(02).
                 05  C-KSSKG   PIC  9(02).
               04  C-TKD.
                 05  C-TKDY    PIC  9(04).
                 05  C-TKDM    PIC  9(02).
                 05  C-TKDD    PIC  9(02).
               04  C-ZHC       PIC  9(01).
       01  CRT-WK2.
           02  C-ACT           PIC  9(01).
           02  C-NCD           PIC  9(01).
           02  CRDATE.
             03  CRYM.
               04  CRYY        PIC  9(04).
               04  CRYYL  REDEFINES CRYY.
                 05  CRYY1     PIC  9(02).
                 05  CRYY2     PIC  9(02).
               04  CRMM        PIC  9(02).
             03  CRDD          PIC  9(02).
           02  C-DNNO          PIC  9(06).
           02  C-TKCD          PIC  9(05).
           02  C-NAMEN         PIC  N(10).
           02  C-TCD           PIC  9(04).
           02  C-OKC           PIC  X(01).
       01  W-WORK.
           02  SOE             PIC  9(02).
           02  I               PIC  9(02).
           02  K               PIC  9(02).
           02  LIN             PIC  9(02).
           02  LIN1            PIC  9(02).
           02  LIN2            PIC  9(02).
           02  SV-I            PIC  9(02).
           02  SV-LIN          PIC  9(02).
           02  SV-LIN1         PIC  9(02).
           02  SV-LIN2         PIC  9(02).
           02  INV-SW          PIC  9(01).
           02  ERR-SW          PIC  9(01).
           02  DRCR-SW         PIC  9(01).
           02  HOJO-SW         PIC  9(01).
           02  C-KRTOT         PIC S9(10).
           02  C-KSTOT         PIC S9(10).
           02  W-NG            PIC  9(06).
           02  W-NGD    REDEFINES W-NG.
             03  W-NEN         PIC  9(04).
             03  W-NENL   REDEFINES W-NEN.
               04  W-NEN1      PIC  9(02).
               04  W-NEN2      PIC  9(02).
             03  W-GET         PIC  9(02).
           02  W-NGL    REDEFINES W-NG.
             03  F             PIC  9(02).
             03  W-NGS         PIC  9(04).
           02  B-CRDATE        PIC  9(08).
           02  B-TKCD          PIC  9(05).
           02  B-NAMEN         PIC  N(10).
           02  B-TENO          PIC  9(06).
           02  W-GPZ.
             03  W-GETZ        PIC  Z(02).
             03  F             PIC  X(01) VALUE "/".
             03  W-PEYZ        PIC  Z(02).
           02  W-GPDZ          PIC  X(05).
           02  W-KIN           PIC S9(10).
           02  W-DC            PIC  9(01).
           02  W-HHC           PIC  9(01).
           02  W-CHK           PIC  9(01).
           02  W-SKDD.
             03  W-SKDN        PIC  9(04).
             03  W-SKDG        PIC  9(02).
             03  W-SKDP        PIC  9(02).
           02  W-DTW.
             03  W-DTW1        PIC  9(03).
             03  W-DTW2        PIC  9(01).
           02  W-SDATE         PIC  9(08).
           02  W-HNG           PIC  9(06).
           02  W-HNGD  REDEFINES W-HNG.
             03  W-HNEN        PIC  9(04).
             03  W-HNEND REDEFINES W-HNEN.
               04  W-HNEN1     PIC  9(02).
               04  W-HNEN2     PIC  9(02).
             03  W-HGET        PIC  9(02).
           02  W-HNGPL REDEFINES W-HNG.
             03  F             PIC  9(02).
             03  W-HNGS        PIC  9(04).
       01  W-BU.
           02  WBU-DATA    OCCURS  5.
             03  WBU-TENO      PIC  9(06).
       01  SAVE-AREA.
           02  WKONYMD.
             03  WKONYY        PIC  9(04).
             03  WKONYYL  REDEFINES WKONYY.
               04  WKONYY1     PIC  9(02).
               04  WKONYY2     PIC  9(02).
             03  WKONMM        PIC  9(02).
             03  WKONDD        PIC  9(02).
           02  WKONYMDR REDEFINES WKONYMD   PIC  9(08).
           02  WKONYMDL REDEFINES WKONYMD.
             03  F             PIC  9(02).
             03  WKONYMDS      PIC  9(06).
           02  WUPDYM          PIC  9(06).
           02  WSUB5           PIC  9(01).
           02  W-KRCDM         PIC  9(04).
           02  W-KSCDM         PIC  9(04).
      *
           COPY LWMSG_PR.
           COPY LIBFDD.
           COPY KANGEL.
           COPY ACCUNT.
           COPY BUMONF.
           COPY L-BANK.
           COPY TKLIB.
           COPY LTKI.
           COPY KEIHI.
           COPY LGYM.
           COPY FCTL.
           COPY LITSKF.
      *
       01  SDH_PRD200.
           02  SDH_PNAME1   PIC  X(009)  VALUE "SIWAKE-H1".
           02  F            PIC  X(001).
           02  SDH_LNAME    PIC  X(003)  VALUE "SDH".
           02  F            PIC  X(001).
           02  SDH_KEY1     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY2     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY3     PIC  X(100)  VALUE SPACE.
           02  SDH_KEY4     PIC  X(100)  VALUE SPACE.
           02  SDH_SORT     PIC  X(100)  VALUE SPACE.
           02  SDH_IDLST    PIC  X(100)  VALUE SPACE.
           02  SDH_RES      USAGE  POINTER.
       COPY SIWAKH.
       77  F                PIC  X(001).
      *
           COPY SIWAIW.
           COPY LNSDNO.
           COPY LIHKBM.
      *
       01  TDT-M_PRD200.
           02  TDT-M_PNAME1        PIC  X(004)  VALUE "TDTM".
           02  F                   PIC  X(001).
           02  TDT-M_LNAME         PIC  X(012)  VALUE "TDT-M_PRD200".
           02  F                   PIC  X(001).
           02  TDT-M_KEY1          PIC  X(100)  VALUE SPACE.
           02  TDT-M_KEY2          PIC  X(100)  VALUE SPACE.
           02  TDT-M_SORT          PIC  X(100)  VALUE SPACE.
           02  TDT-M_IDLST         PIC  X(100)  VALUE SPACE.
           02  TDT-M_RES           USAGE  POINTER.
       01  TDT-R.
           02  TD-KEY.
             03  TD-TKC.
               04  TD-C1      PIC  9(001).
               04  TD-C2      PIC  9(001).
             03  TD-TNO       PIC  9(004).
           02  TD-TCD         PIC  9(004).
           02  TD-DATE        PIC  9(006).
           02  TD-NGP   REDEFINES TD-DATE.
             03  TD-NG.
               04  TD-NEN     PIC  9(002).
               04  TD-GET     PIC  9(002).
             03  TD-PEY       PIC  9(002).
           02  TD-MND         PIC  9(006).
           02  TD-MNGP  REDEFINES TD-MND.
             03  TD-MNG.
               04  TD-MN      PIC  9(002).
               04  TD-MG      PIC  9(002).
             03  TD-MP        PIC  9(002).
           02  TD-KIN         PIC S9(010).
           02  TD-BKC         PIC  9(004).
           02  TD-FRN         PIC  N(024).
           02  TD-SAD.
             03  TD-S     OCCURS   7  PIC S9(008).
           02  TD-ZSHZ        PIC S9(007).
           02  TD-SSHZ        PIC S9(007).
           02  F              PIC  X(006).
           02  TD-SNEN.
             03  TD-SNEN1     PIC  9(002).
             03  TD-SNEN2     PIC  9(002).
           02  TD-HCR         PIC  9(001).
           02  TD-HCT         PIC  9(001).
           02  TD-HCK         PIC  9(001).
           02  TD-HCZ         PIC  9(001).
           02  TD-PC          PIC  9(001).
           02  TD-RSC         PIC  9(001).
       77  F                  PIC  X(001).
      *
       77  USER_ID                 PIC  X(006) VALUE SPACE.
       77  COMPLETION_CODE         PIC  X(003) VALUE ZERO.
       77  LINAGECOUNTER           PIC  9(003).
       77  ESTAT                   PIC  X(002).
       77  RESU                    PIC  9(001).
       77  RESP                    PIC  9(001).
       77  RET                     PIC  9(001) VALUE ZERO.
      *
       01  DISP-C.
           02  DISP-CLE    PIC  X(12)  VALUE "CLEAR SCREEN".
       01  ACP-AREA.
           02  ACP-ACT             PIC 9(01).
           02  ACP-NCD             PIC 9(01).
           02  ACP-DNNO            PIC 9(06).
           02  ACP-CRDATE.
               03  ACP-CRYY        PIC 9(02).
               03  ACP-CRMM        PIC 9(02).
               03  ACP-CRDD        PIC 9(02).
           02  ACP-TKCD            PIC 9(05).
           02  ACP-NAMEN           PIC N(10).
           02  ACP-HHC             PIC 9(01).
           02  ACP-SKD             PIC 9(08).
           02  ACP-OKC             PIC X(01).
       01  ACP-AREA1.
           02  ACP-TENO            PIC 9(06).
           02  ACP-KRCDM           PIC 9(04).
           02  ACP-KRCDS           PIC 9(04).
           02  ACP-KRSECT          PIC 9(04).
           02  ACP-KRNKCD          PIC 9(02).
           02  ACP-KRNSC           PIC 9(01).
           02  ACP-KRSKNG          PIC 9(04).
           02  ACP-KRKIN           PIC S9(10).
           02  ACP-KRTAX           PIC X(01).
           02  ACP-KSCDM           PIC 9(04).
           02  ACP-KSCDS           PIC 9(04).
           02  ACP-KSSECT          PIC 9(04).
           02  ACP-KSNKCD          PIC 9(02).
           02  ACP-KSNSC           PIC 9(01).
           02  ACP-KSSKNG          PIC 9(04).
           02  ACP-KSKIN           PIC S9(10).
           02  ACP-KSTAX           PIC X(01).
           02  ACP-TEKICD          PIC 9(03).
           02  ACP-TEKI            PIC N(20).
      ******
       01  DSP-AREA.
           02  DSP-WNK             PIC N(04)
               VALUE "複数入金".
           02  DSP-WNKC            PIC X(08)
               VALUE   "        ".
           02  DSP-TKNM            PIC N(10).
      ***
       01  DSP-AREA1.
           02  DSP-TENO            PIC Z(06).
           02  DSP-KRCDMN          PIC N(10).
           02  DSP-KRCDSN          PIC N(10).
           02  DSP-KRKIN           PIC ZZZZZZZZZZ-.
           02  DSP-KSCDMN          PIC N(10).
           02  DSP-KSCDSN          PIC N(10).
           02  DSP-KSKIN           PIC ZZZZZZZZZZ-.
      ***
       01  DSP-AREA2.
           02  DSP-KRTOT           PIC ZZZZZZZZZZ-.
           02  DSP-KSTOT           PIC ZZZZZZZZZZ-.
      ***
       01  CLE-AREA.
           02  CLE-CRDATE  .
               03  CLE-CRYY        PIC Z(02).
               03  CLE-CRMM        PIC Z(02).
               03  CLE-CRDD        PIC Z(02).
           02  CLE-DNNO            PIC Z(06).
           02  CLE-TKCD            PIC Z(05).
      ***
       01  CLE-AREA1.
           02  CLE-KRSECT          PIC Z(04).
           02  CLE-KSSECT          PIC Z(04).
      ***
       01  CLE-AREA2.
           02  CLE-KRNKCD          PIC X(02) VALUE "  ".
           02  CLE-KSNKCD          PIC X(02) VALUE "  ".
           02  CLE-KRNSC           PIC Z(01).
           02  CLE-KSNSC           PIC Z(01).
           02  CLE-KRSKNG          PIC X(04) VALUE "    ".
           02  CLE-KSSKNG          PIC X(04) VALUE "    ".
           02  CLE-HHC             PIC X(01) VALUE " ".
           02  CLE-SKD             PIC X(08) VALUE
                "        ".
      ***
       01  SP-AREA.
           02  CLE-SP20-KR1        PIC X(20).
           02  CLE-SP20-KR2        PIC X(20).
           02  CLE-SP20-KS1        PIC X(20).
           02  CLE-SP20-KS2        PIC X(20).
           02  CLE-SP40            PIC X(40).
      ***
       01  MSG-AREA   .
           02  MG-03.
               03  FILLER   PIC N(20) VALUE
                   "金額アンマッチ　入力不可".
           02  MG-04.
               03  FILLER   PIC N(20) VALUE
                   "金額バランスエラー　処理不可".
           02  MG-05.
               03  FILLER   PIC N(04) VALUE
                   "修正不可".
           02  MG-06.
               03  FILLER   PIC N(09) VALUE
                   "日付・取引先エラー".
           02  MG-07.
               03  FILLER   PIC N(07) VALUE
                   "手形他№エラー".
           02  MG-08.
               03  FILLER   PIC N(06) VALUE
                   "必要科目なし".
           02  MG-09.
               03  FILLER   PIC N(08) VALUE
                   "入金区分チェック".
           02  MG-10.
               03  FILLER   PIC N(14) VALUE
                   "得意先なし（取引先チェック）".
           02  MG-11.
               03  FILLER   PIC N(20) VALUE
                   "税区分エラー　処理不可　　　　　　　　　".
           02  MG-12.
               03  FILLER   PIC N(20) VALUE
                   "取引先マスタ　ＲＥＷＲＩＴＥエラー　　　".
           02  MG-13.
               03  FILLER   PIC N(20) VALUE
                   "手形・領収書・買掛支払データ　なし　　　".
           02  MG-14.
               03  FILLER   PIC N(20) VALUE
                   "手形・領収書・買掛支払データ　使用済み　".
           02  MG-15.
               03  FILLER   PIC N(20) VALUE
                   "請求書　発行済み　　　　　　　　　　　　".
           02  MG-16.
               03  FILLER   PIC N(20) VALUE
                   "日付　エラー　　　　　　　　　　　　　　".
      *********
       COPY LSMSG_PR.
       COPY LIBSCR.
       PROCEDURE                   DIVISION.
       CALL "DB_Initialize" USING BY REFERENCE ERR-STAT RETURNING RET.
       CALL "DB_Open".
       CALL "PR_Initialize" USING "999" RETURNING RESP.
       CALL "SD_Initialize" RETURNING RESU.
      *DISP-C
       CALL "SD_Init" USING
            "DISP-C" " " "1" "0" "12" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DISP-CLE" "X" "1" "0" "12" " " "DISP-C" RETURNING RESU.
      *ACP-AREA
       CALL "SD_Init" USING
            "ACP-AREA" " " "0" "0" "49" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-ACT" "9" "1" "58" "1" " " "ACP-AREA" RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-ACT" BY REFERENCE C-ACT "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-NCD" "9" "2" "11" "1" "ACP-ACT" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-NCD" BY REFERENCE C-NCD "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-DNNO" "9" "2" "22" "6" "ACP-NCD" " " RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-DNNO" BY REFERENCE C-DNNO "6" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CRDATE" " " "2" "0" "6" "ACP-DNNO" " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CRYY" "9" "2" "34" "2" " " "ACP-CRDATE"
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-CRYY" BY REFERENCE CRYY2 "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CRMM" "9" "2" "37" "2" "ACP-CRYY" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-CRMM" BY REFERENCE CRMM "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-CRDD" "9" "2" "40" "2" "ACP-CRMM" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-CRDD" BY REFERENCE CRDD "2" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TKCD" "9" "2" "53" "5" "ACP-CRDATE" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TKCD" BY REFERENCE C-TKCD "5" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-NAMEN" "N" "2" "59" "20" "ACP-TKCD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-NAMEN" BY REFERENCE C-NAMEN "20" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-HHC" "9" "22" "45" "1" "ACP-NAMEN" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-HHC" BY REFERENCE W-HHC "1" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-SKD" "9" "23" "47" "8" "ACP-HHC" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-SKD" BY REFERENCE W-SKDD "8" "0" RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-OKC" "X" "24" "77" "1" "ACP-SKD" " "
            RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-OKC" BY REFERENCE C-OKC "1" "0" RETURNING RESU.
      *ACP-AREA1
       CALL "SD_Init" USING
            "ACP-AREA1" " " "0" "0" "109" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TENO" "9" "LIN2" "3" "6" " " "ACP-AREA1"
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TENO" BY REFERENCE C-TENO(1) "6" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRCDM" "9" "LIN" "2" "4" "ACP-TENO" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRCDM" BY REFERENCE C-KRCDM(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRCDS" "9" "LIN1" "2" "4" "ACP-KRCDM" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRCDS" BY REFERENCE C-KRCDS(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRSECT" "9" "LIN" "23" "4" "ACP-KRCDS" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRSECT" BY REFERENCE C-KRSECT(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRNKCD" "9" "LIN" "23" "2" "ACP-KRSECT" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRNKCD" BY REFERENCE C-KRNKCD(1) "2" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRNSC" "9" "LIN" "26" "1" "ACP-KRNKCD" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRNSC" BY REFERENCE C-KRNSC(1) "1" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRSKNG" "9" "LIN1" "23" "4" "ACP-KRNSC" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRSKNG" BY REFERENCE C-KRSKNG(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRKIN" "S9" "LIN" "28" "10" "ACP-KRSKNG" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRKIN" BY REFERENCE C-KRKIN(1) "10" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KRTAX" "X" "LIN1" "37" "1" "ACP-KRKIN" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KRTAX" BY REFERENCE C-KRTAX(1) "1" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSCDM" "9" "LIN" "40" "4" "ACP-KRTAX" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSCDM" BY REFERENCE C-KSCDM(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSCDS" "9" "LIN1" "40" "4" "ACP-KSCDM" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSCDS" BY REFERENCE C-KSCDS(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSSECT" "9" "LIN" "61" "4" "ACP-KSCDS" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSSECT" BY REFERENCE C-KSSECT(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSNKCD" "9" "LIN" "61" "2" "ACP-KSSECT" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSNKCD" BY REFERENCE C-KSNKCD(1) "2" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSNSC" "9" "LIN" "64" "1" "ACP-KSNKCD" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSNSC" BY REFERENCE C-KSNSC(1) "1" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSSKNG" "9" "LIN1" "61" "4" "ACP-KSNSC" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSSKNG" BY REFERENCE C-KSSKNG(1) "4" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSKIN" "S9" "LIN" "66" "10" "ACP-KSSKNG" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSKIN" BY REFERENCE C-KSKIN(1) "10" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-KSTAX" "X" "LIN1" "75" "1" "ACP-KSKIN" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-KSTAX" BY REFERENCE C-KSTAX(1) "1" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEKICD" "9" "LIN2" "40" "3" "ACP-KSTAX" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TEKICD" BY REFERENCE C-TEKICD(1) "3" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "ACP-TEKI" "N" "LIN2" "40" "40" "ACP-TEKICD" " "
             RETURNING RESU.
       CALL "SD_Using" USING
            "ACP-TEKI" BY REFERENCE C-TEKI(1) "40" "1"
            BY REFERENCE I 216 RETURNING RESU.
      *DSP-AREA.
       CALL "SD_Init" USING
            "DSP-AREA" " " "0" "0" "36" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-WNK" "bN" "1" "71" "8" " " "DSP-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-WNKC" "X" "1" "71" "8" "DSP-WNK" " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TKNM" "N" "2" "59" "20" "DSP-WNKC" " " RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TKNM" BY REFERENCE C-NAMEN "20" "0"
            RETURNING RESU.
      *DSP-AREA1
       CALL "SD_Init" USING
            "DSP-AREA1" " " "0" "0" "108" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-TENO" "Z" "LIN2" "3" "6" " " "DSP-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-TENO" BY REFERENCE C-TENO(1) "6" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KRCDMN" "N" "LIN" "2" "20" "DSP-TENO" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KRCDMN" BY REFERENCE C-KR-CDMN(1) "20" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KRCDSN" "N" "LIN1" "2" "20" "DSP-KRCDMN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KRCDSN" BY REFERENCE C-KR-CDSN(1) "20" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KRKIN" "ZZZZZZZZZZ-" "LIN" "28" "11" "DSP-KRCDSN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KRKIN" BY REFERENCE C-KRKIN(1) "10" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KSCDMN" "N" "LIN" "40" "20" "DSP-KRKIN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KSCDMN" BY REFERENCE C-KS-CDMN(1) "20" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KSCDSN" "N" "LIN1" "40" "20" "DSP-KSCDMN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KSCDSN" BY REFERENCE C-KS-CDSN(1) "20" "1"
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KSKIN" "ZZZZZZZZZZ-" "LIN" "66" "11" "DSP-KSCDSN" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KSKIN" BY REFERENCE C-KSKIN(1) "10" "1"
            BY REFERENCE I 216 RETURNING RESU.
      *DSP-AREA2
       CALL "SD_Init" USING
            "DSP-AREA2" " " "0" "0" "22" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KRTOT" "ZZZZZZZZZZ-" "22" "28" "11" " " "DSP-AREA2"
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KRTOT" BY REFERENCE C-KRTOT "10" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "DSP-KSTOT" "ZZZZZZZZZZ-" "22" "66" "11" "DSP-KRTOT" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "DSP-KSTOT" BY REFERENCE C-KSTOT "10" "0"
            RETURNING RESU.
      *CLE-AREA
       CALL "SD_Init" USING
            "CLE-AREA" " " "0" "0" "17" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-CRDATE" " " "2" "0" "6" " " "CLE-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-CRYY" "Z" "2" "34" "2" " " "CLE-CRDATE"
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-CRYY" BY REFERENCE CRYY2 "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-CRMM" "Z" "2" "37" "2" "CLE-CRYY" " " RETURNING RESU.
       CALL "SD_From" USING
            "CLE-CRMM" BY REFERENCE CRMM "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-CRDD" "Z" "2" "40" "2" "CLE-CRMM" " " RETURNING RESU.
       CALL "SD_From" USING
            "CLE-CRDD" BY REFERENCE CRDD "2" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-DNNO" "Z" "2" "22" "6" "CLE-CRDATE" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-DNNO" BY REFERENCE C-DNNO "6" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-TKCD" "Z" "2" "53" "5" "CLE-DNNO" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-TKCD" BY REFERENCE C-TKCD "5" "0"
            RETURNING RESU.
      *CLE-AREA1
       CALL "SD_Init" USING 
            "CLE-AREA1" " " "0" "0" "8" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KRSECT" "Z" "LIN" "23" "4" " " "CLE-AREA1"
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-KRSECT" BY REFERENCE C-KRSECT(1) "4" "1" 
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KSSECT" "Z" "LIN" "61" "4" "CLE-KRSECT" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-KSSECT" BY REFERENCE C-KSSECT(1) "4" "1" 
            BY REFERENCE I 216 RETURNING RESU.
      *CLE-AREA2
      *
       CALL "SD_Init" USING 
            "CLE-AREA2" " " "0" "0" "23" " " " " RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KRNKCD" "X" "LIN" "23" "2" " " "CLE-AREA2"
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KSNKCD" "X" "LIN" "61" "2" "CLE-KRNKCD" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KRNSC" "Z" "LIN" "26" "1" "CLE-KSNKCD" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-KRNSC" BY REFERENCE C-KRNSC(1) "1" "1" 
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KSNSC" "Z" "LIN" "64" "1" "CLE-KRNSC" " "
            RETURNING RESU.
       CALL "SD_From" USING 
            "CLE-KSNSC" BY REFERENCE C-KSNSC(1) "1" "1" 
            BY REFERENCE I 216 RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KRSKNG" "X" "LIN1" "23" "4" "CLE-KSNSC" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-KSSKNG" "X" "LIN1" "61" "4" "CLE-KRSKNG" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-HHC" "X" "22" "45" "1" "CLE-KSSKNG" " "
            RETURNING RESU.
       CALL "SD_Init" USING 
            "CLE-SKD" "X" "23" "47" "8" "CLE-HHC" " " RETURNING RESU.
      *SP-AREA
       CALL "SD_Init" USING
            "SP-AREA" " " "0" "0" "120" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-SP20-KR1" "X" "LIN" "2" "20" " " "SP-AREA"
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-SP20-KR1" BY REFERENCE WKSP "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-SP20-KR2" "X" "LIN1" "2" "20" "CLE-SP20-KR1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-SP20-KR2" BY REFERENCE WKSP "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-SP20-KS1" "X" "LIN" "40" "20" "CLE-SP20-KR2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-SP20-KS1" BY REFERENCE WKSP "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-SP20-KS2" "X" "LIN1" "40" "20" "CLE-SP20-KS1" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-SP20-KS2" BY REFERENCE WKSP "40" "0"
            RETURNING RESU.
       CALL "SD_Init" USING
            "CLE-SP40" "X" "LIN2" "40" "40" "CLE-SP20-KS2" " "
            RETURNING RESU.
       CALL "SD_From" USING
            "CLE-SP40" BY REFERENCE WKSP "40" "0"
            RETURNING RESU.
      *MSG-AREA
       CALL "SD_Init" USING
            "MSG-AREA" " " "24" "0" "416" " " " " RETURNING RESU.
       CALL "SD_Init" USING
            "MG-03" " " "24" "0" "40" " " "MSG-AREA" RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-03" "N" "24" "2" "40" " " "MG-03" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-04" " " "24" "0" "40" "MG-03" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-04" "N" "24" "2" "40" " " "MG-04" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-05" " " "24" "0" "8" "MG-04" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-05" "N" "24" "2" "8" " " "MG-05" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-06" " " "24" "0" "18" "MG-05" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-06" "N" "24" "2" "18" " " "MG-06" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-07" " " "24" "0" "14" "MG-06" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-07" "N" "24" "2" "14" " " "MG-07" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-08" " " "24" "0" "12" "MG-07" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-08" "N" "24" "2" "12" " " "MG-08" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-09" " " "24" "0" "16" "MG-08" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-09" "N" "24" "2" "16" " " "MG-09" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-10" " " "24" "0" "28" "MG-09" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-10" "N" "24" "2" "28" " " "MG-10" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-11" " " "24" "0" "40" "MG-10" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-11" "N" "24" "2" "40" " " "MG-11" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-12" " " "24" "0" "40" "MG-11" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-12" "N" "24" "2" "40" " " "MG-12" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-13" " " "24" "0" "40" "MG-12" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-13" "N" "24" "2" "40" " " "MG-13" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-14" " " "24" "0" "40" "MG-13" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-14" "N" "24" "2" "40" " " "MG-14" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-15" " " "24" "0" "40" "MG-14" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-15" "N" "24" "2" "40" " " "MG-15" RETURNING RESU.
       CALL "SD_Init" USING
            "MG-16" " " "24" "0" "40" "MG-15" " " RETURNING RESU.
       CALL "SD_Init" USING
            "01MG-16" "N" "24" "2" "40" " " "MG-16" RETURNING RESU.
      *
           COPY LSMSG_PR_P.
           COPY LIBSCR_P.
      *
           ACCEPT USER_ID FROM ARGUMENT-VALUE.
           ACCEPT COMPLETION_CODE FROM ARGUMENT-VALUE.
      *
       MR000.
           PERFORM INI-RTN THRU INI-EX.
           PERFORM OPEN-RTN THRU OPEN-EX.
       MR010.
           PERFORM ACT-RTN THRU ACT-EX.
           IF  ESTAT = "P9"
               GO TO MR999
           END-IF.
           MOVE 1     TO I.
           PERFORM CRE-RTN THRU CRE-EX.
           MOVE ZERO     TO CRDATE C-DNNO C-TKCD.
           MOVE SPACE    TO C-NAMEN.
           CALL "SD_Output" USING "CLE-AREA" CLE-AREA "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-TKNM" DSP-TKNM "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-WNKC" DSP-WNKC "p"
                                         RETURNING RESU.
           PERFORM DISP-000 THRU DISP-EX.
           IF  C-ACT NOT = 1
               GO TO MR020
           END-IF.
       MR012.
           PERFORM NCD-RTN THRU NCD-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
           IF  C-ACT NOT = 1
               GO TO MR020
           END-IF.
           IF  C-NCD = 1
               GO TO MR020
           END-IF.
           PERFORM GET-RTN THRU GET-EX.
           PERFORM DISP-RTN THRU DISP-EX.
           PERFORM GOKEI-RTN THRU GOKEI-EX.
           GO TO MR027.
       MR020.
           PERFORM DNNO-RTN THRU DNNO-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
           MOVE 0     TO ERR-SW.
           IF  C-ACT NOT = 1
               PERFORM SDWG-RTN THRU SDWG-EX
               IF  ERR-SW = 1
                   GO TO MR020
               END-IF
           END-IF.
           PERFORM GET-RTN THRU GET-EX.
           PERFORM DISP-RTN THRU DISP-EX.
           PERFORM GOKEI-RTN THRU GOKEI-EX.
           IF  C-ACT = 3
               GO TO MR400
           END-IF.
       MR021.
           PERFORM DATE-RTN THRU DATE-EX.
           IF  ESTAT = "09"
               GO TO MR010
           END-IF.
       MR025.
           PERFORM TK-RTN THRU TK-EX.
           IF  ESTAT = "09"
               GO TO MR021
           END-IF.
       MR027.
           MOVE 7     TO LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           MOVE 8     TO LIN1.
           CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                          RETURNING RESU.
           MOVE 9     TO LIN2.
           CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                          RETURNING RESU.
           MOVE 1     TO I.
           MOVE 0     TO W-DC.
       MR028.
           MOVE C-TENO(I) TO B-TENO.
           PERFORM TENO-RTN THRU TENO-EX.
           IF  ESTAT = "04"
               MOVE I     TO SV-I
               MOVE LIN   TO SV-LIN
               MOVE LIN1  TO SV-LIN1
               MOVE LIN2  TO SV-LIN2
               PERFORM CRE-RTN THRU CRE-EX
               MOVE SV-I     TO I
               PERFORM DISP-100 THRU DISP-EX
               GO TO MR300
           END-IF.
           IF  ESTAT = "09"
               IF  I NOT = 1
                   SUBTRACT 3     FROM LIN LIN1 LIN2
                   CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                            RETURNING RESU
                   CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                            RETURNING RESU
                   CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                            RETURNING RESU
                   SUBTRACT 1     FROM I
                   GO TO MR028
               ELSE
                   IF  C-NCD = 0
                       GO TO MR012
                   ELSE
                       GO TO MR025
                   END-IF
               END-IF
           END-IF.
           IF  I     = 1
               MOVE CRDATE   TO B-CRDATE
               MOVE C-TKCD   TO B-TKCD
               MOVE C-NAMEN  TO B-NAMEN
           END-IF.
       MR030.
           PERFORM KRCDM-RTN THRU KRCDM-EX.
           IF  ESTAT = "04"
               MOVE I     TO SV-I
               MOVE LIN   TO SV-LIN
               MOVE LIN1  TO SV-LIN1
               MOVE LIN2  TO SV-LIN2
               PERFORM CRE-RTN THRU CRE-EX
               MOVE SV-I     TO I
               PERFORM DISP-100 THRU DISP-EX
               GO TO MR300
           END-IF.
           IF  ESTAT = "09"
               GO TO MR028
           END-IF.
       MR035.
           PERFORM KRCDS-RTN THRU KRCDS-EX.
           IF  ESTAT = "09"
               GO TO MR030
           END-IF.
           IF  C-KRCDM(I) = 0140
               MOVE ZERO TO C-KRSECT(I)
               CALL "SD_Output" USING "CLE-KRSECT" CLE-KRSECT "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KRNKCD" ACP-KRNKCD "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "CLE-KRNSC" CLE-KRNSC "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KRSKNG" ACP-KRSKNG "p"
                                             RETURNING RESU
               GO TO MR045
           END-IF.
           MOVE ZERO TO C-KRNKCD(I) C-KRNSC(I) C-KRSKNG(I).
           CALL "SD_Output" USING "CLE-KRNKCD" CLE-KRNKCD "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "CLE-KRNSC" CLE-KRNSC "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "CLE-KRSKNG" CLE-KRSKNG "p"
                                         RETURNING RESU.
       MR040.
           PERFORM KRSECT-RTN THRU KRSECT-EX.
           IF  ESTAT = "09"
               GO TO MR035
           END-IF.
           GO TO MR050.
       MR045.
           PERFORM KRNKCD-RTN THRU KRNKCD-EX.
           IF  ESTAT = "09"
               GO TO MR035
           END-IF.
           IF  C-KRNKCD(I) NOT = 60
               MOVE ZERO TO C-KRNSC(I)
               CALL "SD_Output" USING "CLE-KRNSC" CLE-KRNSC "p"
                                             RETURNING RESU
               GO TO MR047
           END-IF.
       MR046.
           PERFORM KRNSC-RTN THRU KRNSC-EX.
           IF  ESTAT = "09"
               GO TO MR045
           END-IF.
       MR047.
           PERFORM KRSKNG-RTN THRU KRSKNG-EX.
           IF  ESTAT = "09"
               IF  C-KRNKCD(I) = 60
                   GO TO MR046
               ELSE
                   GO TO MR045
               END-IF
           END-IF.
       MR050.
           PERFORM KRKIN-RTN THRU KRKIN-EX.
           IF  ESTAT = "09"
               IF  C-KRCDM(I) = 0140
                   GO TO MR047
               ELSE
                   GO TO MR040
               END-IF
           END-IF.
       MR055.
           PERFORM KRTAX-RTN THRU KRTAX-EX.
           IF  ESTAT = "09"
               GO TO MR050
           END-IF.
       MR100.
           PERFORM KSCDM-RTN THRU KSCDM-EX.
           IF  ESTAT = "09"
               GO TO MR055
           END-IF.
       MR105.
           PERFORM KSCDS-RTN THRU KSCDS-EX.
           IF  ESTAT = "09"
               GO TO MR100
           END-IF.
           IF (C-KSCDM(I) = 0140) AND (C-KRCDM(I) < 5000 OR > 5999)
               MOVE ZERO TO C-KSSECT(I)
               CALL "SD_Output" USING "ACP-KSNKCD" ACP-KSNKCD "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "CLE-KSNSC" CLE-KSNSC "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KSSKNG" ACP-KSSKNG "p"
                                             RETURNING RESU
               GO TO MR115
           END-IF.
           MOVE ZERO TO C-KSNKCD(I) C-KSNSC(I) C-KSSKNG(I).
           CALL "SD_Output" USING "CLE-KSNKCD" CLE-KSNKCD "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "CLE-KSNSC" CLE-KSNSC "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "CLE-KSSKNG" CLE-KSSKNG "p"
                                         RETURNING RESU.
       MR110.
           PERFORM KSSECT-RTN THRU KSSECT-EX.
           IF  ESTAT = "09"
               GO TO MR105
           END-IF.
           GO TO MR120.
       MR115.
           PERFORM KSNKCD-RTN THRU KSNKCD-EX.
           IF  ESTAT = "09"
               GO TO MR105
           END-IF.
           IF  C-KSNKCD(I) NOT = 60
               MOVE ZERO TO C-KSNSC(I)
               CALL "SD_Output" USING "CLE-KSNSC" CLE-KSNSC "p"
                                             RETURNING RESU
               GO TO MR117
           END-IF.
       MR116.
           PERFORM KSNSC-RTN THRU KSNSC-EX.
           IF  ESTAT = "09"
               GO TO MR115
           END-IF.
       MR117.
           PERFORM KSSKNG-RTN THRU KSSKNG-EX.
           IF  ESTAT = "09"
               IF  C-KSNKCD(I) = 60
                   GO TO MR116
               ELSE
                   GO TO MR115
               END-IF
           END-IF.
       MR120.
           PERFORM KSKIN-RTN THRU KSKIN-EX.
           IF  ESTAT = "09"
               IF  (C-KSCDM(I) = 0140) AND (C-KRCDM(I) < 5000 OR > 5999)
                   GO TO MR117
               ELSE
                   GO TO MR110
               END-IF
           END-IF.
       MR125.
           PERFORM KSTAX-RTN THRU KSTAX-EX.
           IF  ESTAT = "09"
               GO TO MR120
           END-IF.
       MR200.
           PERFORM TEKI-RTN THRU TEKI-EX.
           IF  ESTAT = "09"
               GO TO MR125
           END-IF.
           IF  I NOT = 5
               ADD 1     TO I
               ADD 3     TO LIN LIN1 LIN2
               CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                              RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                              RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                           RETURNING RESU
               GO TO MR028
           END-IF.
           MOVE I     TO SV-I.
           MOVE LIN   TO SV-LIN.
           MOVE LIN1  TO SV-LIN1.
           MOVE LIN2  TO SV-LIN2.
       MR300.
           PERFORM GOKEI-RTN THRU GOKEI-EX.
           IF  ERR-SW = 1
               MOVE SV-I     TO I
               MOVE SV-LIN   TO LIN
               CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                        RETURNING RESU
               MOVE SV-LIN1  TO LIN1
               CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                        RETURNING RESU
               MOVE SV-LIN2  TO LIN2
               CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                        RETURNING RESU
               GO TO MR028
           END-IF.
           PERFORM CHK-RTN THRU CHK-EX.
           IF  ERR-SW = 1
               MOVE SV-I     TO I
               MOVE SV-LIN   TO LIN
               CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                              RETURNING RESU
               MOVE SV-LIN1  TO LIN1
               CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                              RETURNING RESU
               MOVE SV-LIN2  TO LIN2
               CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                           RETURNING RESU
               GO TO MR028
           END-IF.
           IF ((C-ACT = 2) AND (W-HHC = 1 OR 9)) OR
              (W-CHK NOT = 1)
               GO TO MR310
           END-IF.
           GO TO MR320.
       MR310.
           IF  C-ACT NOT = 1
               GO TO MR320
           END-IF.
           MOVE 0 TO W-HHC.
           CALL "SD_Output" USING "CLE-HHC" CLE-HHC "p"
                                         RETURNING RESU.
      *
           IF  TK-SS = ZERO OR 99
               MOVE ZERO TO W-SKDD
               CALL "SD_Output" USING "CLE-SKD" CLE-SKD "p"
                                             RETURNING RESU
               GO TO MR400
           END-IF.
           MOVE TK-TCD TO TSK-KEY.
      *           READ TSKF WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TSKF_PNAME1 BY REFERENCE TSK-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE ZERO TO TSK-R
           END-IF.
           MOVE TSK-ZNGP(4) TO W-SDATE.
           IF  TSK-ZNGP(5) NOT = ZERO
               MOVE 99999999 TO W-SDATE
               MOVE ZERO TO W-SKDD
               CALL "SD_Output" USING "CLE-SKD" CLE-SKD "p"
                                             RETURNING RESU
               GO TO MR400
           END-IF.
           MOVE CRDATE TO W-SKDD.
       MR312.
           IF  W-SKDG = 13
               MOVE 1 TO W-SKDG
               ADD 1 TO W-SKDN
           END-IF.
           MOVE TK-SS TO W-SKDP.
           IF  W-SKDP = 30 OR 31
               IF  W-SKDG = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                   MOVE 31 TO W-SKDP
               ELSE
                   IF  W-SKDG = 4 OR 6 OR 9 OR 11
                       MOVE 30 TO W-SKDP
                   ELSE
                       DIVIDE 4 INTO W-SKDN GIVING W-DTW1
                                                 REMAINDER W-DTW2
                       IF  W-DTW2 = 0
                           MOVE 29 TO W-SKDP
                       ELSE
                           MOVE 28 TO W-SKDP
                       END-IF
                   END-IF
               END-IF
           END-IF.
           IF  CRDATE > W-SKDD
               IF  CRMM NOT = W-SKDG
                   MOVE ZERO TO W-SKDD
                   GO TO MR330
               ELSE
                   ADD 1 TO W-SKDG
                   GO TO MR312
               END-IF
           END-IF.
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SKDD
                   ADD 1 TO W-SKDG
                   GO TO MR312
               END-IF
           END-IF.
           CALL "SD_Output" USING "ACP-SKD" ACP-SKD "p"
                                         RETURNING RESU.
           GO TO MR400.
       MR320.
           PERFORM HHC-RTN THRU HHC-EX.
           IF  ESTAT = "09"
               MOVE SV-I     TO I
               MOVE SV-LIN   TO LIN
               CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                             RETURNING RESU
               MOVE SV-LIN1  TO LIN1
               CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                             RETURNING RESU
               MOVE SV-LIN2  TO LIN2
               CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                          RETURNING RESU
               GO TO MR028
           END-IF.
           IF  W-HHC = 8 OR 9
               MOVE ZERO TO W-SKDD
               CALL "SD_Output" USING "CLE-SKD" CLE-SKD "p"
                                             RETURNING RESU
               GO TO MR400
           END-IF.
       MR330.
           PERFORM SKD-RTN THRU SKD-EX.
           IF  ESTAT = "09"
               GO TO MR320
           END-IF.
      **                                                                確認
       MR400.
           PERFORM OKC-RTN THRU OKC-EX.
           IF  ESTAT = "09"
               IF  C-ACT = 3
                   GO TO MR020
               ELSE
                   IF ((C-ACT = 2) AND (W-HHC = 1 OR 9)) OR
                     ((W-CHK NOT = 1) AND (TK-WNK NOT = 1))
                       MOVE SV-I     TO I
                       MOVE SV-LIN   TO LIN
                       CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                               RETURNING RESU
                       MOVE SV-LIN1  TO LIN1
                       CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                               RETURNING RESU
                       MOVE SV-LIN2  TO LIN2
                       CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                               RETURNING RESU
                       GO TO MR028
                   ELSE
                       GO TO MR330
                   END-IF
               END-IF
           END-IF.
           IF  C-OKC = "9"
               GO TO MR500
           END-IF.
      **                                                                更新
           IF  C-ACT = 1
               PERFORM DNOU-RTN THRU DNOU-EX
           END-IF.
           IF  C-ACT = 2 OR 3
               PERFORM SDWDU-RTN THRU SDWDU-EX
               PERFORM TDTDU-RTN THRU TDTDU-EX
           END-IF.
           IF  C-ACT NOT = 3
               PERFORM SDWWU-RTN THRU SDWWU-EX
               PERFORM TDTWU-RTN THRU TDTWU-EX
           END-IF.
       MR500.
           MOVE 1     TO I.
           PERFORM CRE-RTN THRU CRE-EX.
           MOVE ZERO     TO CRDATE C-DNNO C-TKCD W-SKDD.
           MOVE SPACE    TO C-NAMEN.
           IF  C-ACT NOT = 1
               PERFORM DISP-RTN THRU DISP-EX
               PERFORM GOKEI-RTN THRU GOKEI-EX
           END-IF.
      *
           IF  C-OKC = "1"
               CALL "SD_Output" USING "OK-01" OK-01 "p"
                                              RETURNING RESU
           ELSE
               CALL "SD_Output" USING "CAN-01" CAN-01 "p"
                                              RETURNING RESU
           END-IF.
           MOVE SPACE     TO C-OKC.
           CALL "SD_Output" USING "ACP-OKC" ACP-OKC "p"
                                         RETURNING RESU.
           IF  C-ACT = 1
               GO TO MR012
           END-IF.
           GO TO MR020.
       MR999.
           PERFORM CLSE-ENT THRU CLSE-EXT.
           STOP RUN.
      ************************
       INI-RTN.
           CALL "SD_Screen_Output" USING "GRD200" RETURNING RESU.
           COPY LIBCPR.
           MOVE ZERO            TO W-HNG.
           MOVE D-NHNG          TO W-HNGS.
           IF  W-HNEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-HNEN
           END-IF.
           IF  W-HNEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-HNEN
           END-IF.
           MOVE ZERO            TO WKONYMD.
           ACCEPT WKONYMDS      FROM DATE.
           IF  WKONYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO WKONYY
           END-IF.
           IF  WKONYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO WKONYY
           END-IF.
           CALL "DB_F_Open" USING
            "INPUT" FCTL-F_PNAME1 "SHARED" BY REFERENCE FCTL-F_IDLST "1"
            "FCTL-KEY" BY REFERENCE FCTL-KEY.
           MOVE "DATE  "     TO FCTL-KEY1 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO INI-ERR
           END-IF.
           MOVE FCTL-UPDYM      TO WUPDYM.
      *
           MOVE "SUB   "     TO FCTL-KEY2 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO INI-ERR
           END-IF.
           MOVE FCTL-SUB5     TO WSUB5.
      *
           MOVE "TAX   "     TO FCTL-KEY4 ERR-K.
      *           READ FCTL-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" FCTL-F_PNAME1 BY REFERENCE FCTL-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO INI-ERR
           END-IF.
           MOVE TAX-CODE      TO W-KRCDM.
           MOVE TAX-CODE1     TO W-KSCDM.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
      *
           GO TO INI-EX.
       INI-ERR.
           MOVE "FCTL-F"     TO ERR-F.
           MOVE "G"          TO ERR-M.
           CALL "SD_Output" USING "ERR-DIS" ERR-DIS "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                         RETURNING RESU.
           CALL "DB_F_Close" USING
            BY REFERENCE FCTL-F_IDLST FCTL-F_PNAME1.
           CALL "DB_Close".
           STOP RUN.
       INI-EX.
           EXIT.
      *********
       OPEN-RTN.
           CALL "DB_F_Open" USING
            "INPUT" HKBM_PNAME1 "SHARED" BY REFERENCE HKBM_IDLST "1"
            "HKB-KEY" BY REFERENCE HKB-KEY.
           CALL "DB_F_Open" USING
            "INPUT" KNG_PNAME1 "SHARED" BY REFERENCE KNG_IDLST "1"
            "KNG-KEY" BY REFERENCE KNG-KEY.
           CALL "DB_F_Open" USING
            "INPUT" AM_PNAME1 "SHARED" BY REFERENCE AM_IDLST "1"
            "AM-KEY" BY REFERENCE AM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BNM_PNAME1 "SHARED" BY REFERENCE BNM_IDLST "1"
            "BNM-KEY" BY REFERENCE BNM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" BM_PNAME1 "SHARED" BY REFERENCE BM_IDLST "1"
            "BM-KEY" BY REFERENCE BM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TKI_PNAME1 "SHARED" BY REFERENCE TKI_IDLST "1"
            "TKI-KEY" BY REFERENCE TKI-KEY.
           CALL "DB_F_Open" USING
            "INPUT" HH-F_PNAME1 "SHARED" BY REFERENCE HH-F_IDLST "1"
            "HH-KEY" BY REFERENCE HH-KEY.
           CALL "DB_F_Open" USING
            "INPUT" GYM_PNAME1 "SHARED" BY REFERENCE GYM_IDLST "1"
            "GYM-KEY" BY REFERENCE GYM-KEY.
           CALL "DB_F_Open" USING
            "INPUT" SDH_PNAME1 "SHARED" BY REFERENCE SDH_IDLST "1"
            "SH-KEY1" BY REFERENCE SH-KEY1.
           CALL "DB_F_Open" USING
            "I-O" SDW_PNAME1 "SHARED" BY REFERENCE SDW_IDLST "1"
            "SDW-KEY" BY REFERENCE SDW-KEY.
           CALL "DB_F_Open" USING
            "I-O" TDT-M_PNAME1 "SHARED" BY REFERENCE TDT-M_IDLST "1"
            "TD-KEY" BY REFERENCE TD-KEY.
           CALL "DB_F_Open" USING
            "I-O" NS-DNO_PNAME1 "SHARED" BY REFERENCE NS-DNO_IDLST "1"
            "DNO1-KEY" BY REFERENCE DNO1-KEY.
           CALL "DB_F_Open" USING
            "I-O" TK_PNAME1 "SHARED" BY REFERENCE TK_IDLST "1"
            "TK-KEY" BY REFERENCE TK-KEY.
           CALL "DB_F_Open" USING
            "INPUT" TSKF_PNAME1 "SHARED" BY REFERENCE TSKF_IDLST "1"
            "TSK-KEY" BY REFERENCE TSK-KEY.
       OPEN-EX.
           EXIT.
      ***
       ACT-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-ACT "ACP-ACT" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "P9"
               GO TO ACT-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO ACT-RTN
           END-IF.
           IF  C-ACT NOT = 1 AND 2 AND 3
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO ACT-RTN
           END-IF.
       ACT-EX.
           EXIT.
      ***
       NCD-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-NCD "ACP-NCD" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO NCD-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO NCD-RTN
           END-IF.
           IF  C-NCD NOT = 0 AND 1
               GO TO NCD-RTN
           END-IF.
       NCD-EX.
           EXIT.
      ***
       DATE-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-CRDATE "ACP-CRDATE" " " "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO DATE-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO DATE-RTN
           END-IF.
           IF  CRDATE = ZERO
               MOVE WKONYMD     TO CRDATE
               CALL "SD_Output" USING "ACP-CRDATE" ACP-CRDATE "p"
                                              RETURNING RESU
               GO TO DATE-EX
           END-IF.
           IF  (CRMM < 1 OR > 12) OR
               (CRDD < 1 OR > 31)
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO DATE-RTN
           END-IF.
           MOVE ZERO TO CRYY1.
           IF  CRYY2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO CRYY
           END-IF.
           IF  CRYY2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO CRYY
           END-IF.
           IF (WKONYMD   < CRDATE)  OR  (WUPDYM  >  CRYM)
               GO TO DATE-RTN
           END-IF.
           IF  CRYM  <  W-HNG
               CALL "SD_Output" USING "MG-16" MG-16 "p"
                                              RETURNING RESU
               GO TO DATE-RTN
           END-IF.
       DATE-EX.
           EXIT.
      ***
       DNNO-RTN.
           IF  C-ACT = 1
               MOVE ZERO     TO C-DNNO
               CALL "SD_Output" USING "CLE-DNNO" CLE-DNNO "p"
                                              RETURNING RESU
               GO TO DNNO-EX
           END-IF.
       DNNO-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-DNNO "ACP-DNNO" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO DNNO-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO DNNO-000
           END-IF.
           IF  C-DNNO   <  400000  OR  >  599999
               GO TO DNNO-000
           END-IF.
       DNNO-EX.
           EXIT.
      *********
       SDWG-RTN.
           MOVE ZERO       TO W-BU.
           MOVE 1     TO I.
           PERFORM CRE-RTN THRU CRE-EX.
           MOVE 1          TO ERR-SW.
           MOVE C-DNNO     TO SDWJNO.
           MOVE ZERO       TO SDWLNO.
      *           START SDW KEY NOT LESS SDW-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDW_PNAME1 "SDW-KEY" " NOT < " SDW-KEY RETURNING RET.
           IF  RET = 1
                 GO TO SDWG-999
           END-IF.
       SDWG-000.
      *           READ SDW NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDW_PNAME1 BY REFERENCE SDW-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                GO TO SDWG-999
           END-IF.
           IF  C-DNNO NOT = SDWJNO
               GO TO SDWG-999
           END-IF.
           MOVE 0     TO ERR-SW.
      *
           IF  SDWLNO = 1
               MOVE SDWYMD      TO CRDATE   B-CRDATE
               MOVE SDWCUST     TO C-TKCD   B-TKCD
               MOVE SDWNAMEN    TO C-NAMEN  B-NAMEN
           END-IF.
           MOVE SDWTENO     TO C-TENO(SDWLNO) WBU-TENO(SDWLNO).
           MOVE SDWKARI     TO C-KARI(SDWLNO).
           MOVE SDWKASI     TO C-KASI(SDWLNO).
           MOVE SDWTEKICD   TO C-TEKICD(SDWLNO).
           MOVE SDWTEKI     TO C-TEKI(SDWLNO).
           IF  C-KRCDM(SDWLNO) = 0140
               MOVE SDWNKCD     TO C-KRNKCD(SDWLNO)
               MOVE SDWNSC      TO C-KRNSC(SDWLNO)
               MOVE SDWSKNG     TO C-KRSKNG(SDWLNO)
           END-IF.
           IF  C-KSCDM(SDWLNO) = 0140
               MOVE SDWNKCD     TO C-KSNKCD(SDWLNO)
               MOVE SDWNSC      TO C-KSNSC(SDWLNO)
               MOVE SDWSKNG     TO C-KSSKNG(SDWLNO)
           END-IF.
           IF  C-KRCDM(SDWLNO) = W-KRCDM OR W-KSCDM
               MOVE SDWETAX    TO C-KRTAX(SDWLNO)
           END-IF.
           IF  C-KSCDM(SDWLNO) = W-KRCDM OR W-KSCDM
               MOVE SDWETAX    TO C-KSTAX(SDWLNO)
           END-IF.
           MOVE SDWZHC      TO C-ZHC(SDWLNO).
           MOVE SDWSKD      TO W-SKDD.
           IF  SDWHHC          = 1
               MOVE 1          TO W-HHC
           END-IF.
           IF  SDWHHC          = 9
               IF  W-HHC           = 0
                   MOVE 9          TO W-HHC
               END-IF
           END-IF.
           IF  SDWHHC          = 8
               IF  W-HHC           = 0
                   MOVE 8          TO W-HHC
               END-IF
           END-IF.
           GO TO SDWG-000.
       SDWG-999.
           IF  ERR-SW = 1
               CALL "SD_Output" USING "INV-D01" INV-D01 "p"
                                              RETURNING RESU
           END-IF.
       SDWG-EX.
           EXIT.
      *********
       GET-RTN.
           MOVE 1     TO I.
       GET-000.
           IF  C-KRCDM(I) NOT = ZERO
               MOVE 1     TO DRCR-SW
               PERFORM AMG-RTN THRU AMG-EX
               MOVE 0     TO HOJO-SW
               PERFORM KNGG-RTN THRU KNGG-EX
               IF  C-KRCDS(I) NOT = ZERO
                   IF  C-KR-HO(I) = 1
                       MOVE 1     TO HOJO-SW
                       PERFORM KNGG-RTN THRU KNGG-EX
                   ELSE
                       PERFORM BMG-RTN THRU BMG-EX
                       MOVE BANKNMN     TO C-KR-CDSN(I)
                   END-IF
               END-IF
           END-IF.
           IF  C-KSCDM(I) NOT = ZERO
               MOVE 2     TO DRCR-SW
               PERFORM AMG-RTN THRU AMG-EX
               MOVE 0     TO HOJO-SW
               PERFORM KNGG-RTN THRU KNGG-EX
               IF  C-KSCDS(I) NOT = ZERO
                   IF  C-KS-HO(I) = 1
                       MOVE 1     TO HOJO-SW
                       PERFORM KNGG-RTN THRU KNGG-EX
                   ELSE
                       PERFORM BMG-RTN THRU BMG-EX
                       MOVE BANKNMN     TO C-KS-CDSN(I)
                   END-IF
               END-IF
           END-IF.
           IF  I NOT = 5
               ADD 1     TO I
               GO TO GET-000
           END-IF.
       GET-EX.
           EXIT.
      *********
       DISP-RTN.
           CALL "SD_Output" USING "ACP-CRDATE" ACP-CRDATE "p"
                                         RETURNING RESU.
           IF  C-TKCD = ZERO
               CALL "SD_Output" USING "CLE-TKCD" CLE-TKCD "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "DSP-TKNM" DSP-TKNM "p"
                                              RETURNING RESU
               GO TO DISP-000
           END-IF.
           CALL "SD_Output" USING "ACP-TKCD" ACP-TKCD "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-TKNM" DSP-TKNM "p"
                                         RETURNING RESU.
           MOVE C-TKCD     TO TK-KEY.
           PERFORM TKG-RTN THRU TKG-EX.
           IF  INV-SW = 1
               GO TO DISP-000
           END-IF.
           IF  TK-WNK = 1
               CALL "SD_Output" USING "DSP-WNK" DSP-WNK "p"
                                             RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DSP-WNKC" DSP-WNKC "p"
                                             RETURNING RESU
           END-IF.
       DISP-000.
           MOVE 1     TO I.
           MOVE 7     TO LIN.
           CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                          RETURNING RESU.
           MOVE 8     TO LIN1.
           CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                          RETURNING RESU.
           MOVE 9     TO LIN2.
           CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                          RETURNING RESU.
       DISP-100.
           CALL "SD_Output" USING "DSP-AREA1" DSP-AREA1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KRTAX" ACP-KRTAX "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KSTAX" ACP-KSTAX "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                         RETURNING RESU.
           IF  0140 NOT = C-KRCDM(I) AND C-KSCDM(I)
               CALL "SD_Output" USING "CLE-KRSECT" CLE-KRSECT "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "CLE-KSSECT" CLE-KSSECT "p"
                                              RETURNING RESU
           END-IF.
           IF  C-KSCDM(I) = 0140
               CALL "SD_Output" USING "ACP-KSNKCD" ACP-KSNKCD "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "ACP-KSSKNG" ACP-KSSKNG "p"
                                              RETURNING RESU
               IF  C-KSNKCD(I) = 60
                   CALL "SD_Output" USING "ACP-KSNSC" ACP-KSNSC "p"
                                             RETURNING RESU
               ELSE
                   CALL "SD_Output" USING "CLE-KSNSC" CLE-KSNSC "p"
                                             RETURNING RESU
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0140
               CALL "SD_Output" USING "ACP-KRNKCD" ACP-KRNKCD "p"
                                              RETURNING RESU
               CALL "SD_Output" USING "ACP-KRSKNG" ACP-KRSKNG "p"
                                              RETURNING RESU
               IF  C-KRNKCD(I) = 60
                   CALL "SD_Output" USING "ACP-KRNSC" ACP-KRNSC "p"
                                             RETURNING RESU
               ELSE
                   CALL "SD_Output" USING "CLE-KRNSC" CLE-KRNSC "p"
                                             RETURNING RESU
               END-IF
           END-IF.
           IF  C-KRSKNG(I) = ZERO
               CALL "SD_Output" USING "CLE-KRSKNG" CLE-KRSKNG "p"
                                              RETURNING RESU
           END-IF.
           IF  C-KSSKNG(I) = ZERO
               CALL "SD_Output" USING "CLE-KSSKNG" CLE-KSSKNG "p"
                                              RETURNING RESU
           END-IF.
           IF  I NOT = 5
               ADD 1     TO I
               ADD 3     TO LIN LIN1 LIN2
               CALL "SD_Arg_Match_Line" USING "LIN" "2" LIN
                              RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN1" "2" LIN1
                              RETURNING RESU
               CALL "SD_Arg_Match_Line" USING "LIN2" "2" LIN2
                           RETURNING RESU
               GO TO DISP-100
           END-IF.
           CALL "SD_Output" USING "ACP-HHC" ACP-HHC "p"
                                         RETURNING RESU.
           IF  W-SKDD NOT = ZERO
               CALL "SD_Output" USING "ACP-SKD" ACP-SKD "p"
                                              RETURNING RESU
           ELSE
               CALL "SD_Output" USING "CLE-SKD" CLE-SKD "p"
                                              RETURNING RESU
           END-IF.
       DISP-EX.
           EXIT.
      ***
       TK-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TKCD "ACP-TKCD" "9" "5"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO TK-EX
           END-IF.
           IF  ESTAT = "P0"
               MOVE 99999 TO C-TKCD
               CALL "SD_Output" USING "ACP-TKCD" ACP-TKCD "p"
                                              RETURNING RESU
               GO TO TK-100
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO TK-RTN
           END-IF.
           IF  C-TKCD = 99999
               GO TO TK-100
           END-IF.
           IF  C-TKCD = ZERO
               MOVE SPACE     TO C-NAMEN
               GO TO TK-999
           END-IF.
           MOVE C-TKCD     TO TK-KEY.
           PERFORM TKG-RTN THRU TKG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                              RETURNING RESU
               GO TO TK-RTN
           END-IF.
           IF  TK-WNK = 1
               CALL "SD_Output" USING "DSP-WNK" DSP-WNK "p"
                                              RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DSP-WNKC" DSP-WNKC "p"
                                              RETURNING RESU
           END-IF.
           GO TO TK-999.
       TK-100.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-NAMEN "ACP-NAMEN" "N" "20"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO TK-RTN
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO TK-100
           END-IF.
       TK-999.
           CALL "SD_Output" USING "DSP-TKNM" DSP-TKNM "p"
                                         RETURNING RESU.
       TK-EX.
           EXIT.
      *****
       TENO-RTN.
           CALL "SD_Output" USING "DSP-TENO" DSP-TENO "p"
                                         RETURNING RESU.
           IF  W-DC NOT = 0
               GO TO TENO-050
           END-IF.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TENO "ACP-TENO" "9" "6"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DSP-TENO" DSP-TENO "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "04"
               IF  I = 1
                   GO TO TENO-RTN
               ELSE
                   GO TO TENO-EX
               END-IF
           END-IF.
           IF  ESTAT = "09"
               GO TO TENO-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO TENO-EX
           END-IF.
           IF  C-TENO1(I) NOT = 00 AND 01 AND 02 AND 03 AND 04 AND
                                11 AND 12 AND 13 AND 22 AND
                                30 AND 31 AND 32 AND 33 AND 34
               GO TO TENO-RTN
           END-IF.
      *
           IF  C-TENO(I) = ZERO
               GO TO TENO-EX
           END-IF.
           IF  I = 2
               IF  C-TENO(2) NOT = ZERO
                   IF  C-TENO(2) = C-TENO(1)
                       GO TO TENO-RTN
                   END-IF
               END-IF
           END-IF.
           IF  I = 3
               IF  C-TENO(3) NOT = ZERO
                   IF  C-TENO(3) = C-TENO(1) OR C-TENO(2)
                       GO TO TENO-RTN
                   END-IF
               END-IF
           END-IF.
           IF  I = 4
               IF  C-TENO(4) NOT = ZERO
                   IF  C-TENO(4) = C-TENO(1) OR C-TENO(2) OR C-TENO(3)
                       GO TO TENO-RTN
                   END-IF
               END-IF
           END-IF.
           IF  I = 5
               IF  C-TENO(5) NOT = ZERO
                   IF  C-TENO(5) = C-TENO(1) OR C-TENO(2) OR C-TENO(3)
                                                         OR C-TENO(4)
                       GO TO TENO-RTN
                   END-IF
               END-IF
           END-IF.
       TENO-005.
           IF  B-TENO NOT = ZERO
               IF  C-TENO(I) = B-TENO
                   GO TO TENO-EX
               END-IF
           END-IF.
           MOVE C-TENO(I)      TO TD-KEY.
      *           READ TDT-M  UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDT-M_PNAME1 BY REFERENCE TDT-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               CALL "SD_Output" USING "MG-13" MG-13 "p"
                                             RETURNING RESU
               GO TO TENO-RTN
           END-IF.
           IF  TD-HCZ NOT = 0
               CALL "SD_Output" USING "MG-14" MG-14 "p"
                                             RETURNING RESU
               GO TO TENO-RTN
           END-IF.
           IF  CRDATE NOT = ZERO
               GO TO TENO-010
           END-IF.
           MOVE TD-SNEN     TO CRYY.
           MOVE TD-GET      TO CRMM.
           MOVE TD-PEY      TO CRDD.
           MOVE TD-TCD      TO C-TKCD.
           IF  TD-C1 < 2
               ADD  10000       TO C-TKCD
           ELSE
               ADD  20000       TO C-TKCD
           END-IF.
           CALL "SD_Output" USING "ACP-CRDATE" ACP-CRDATE "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-TKCD" ACP-TKCD "p"
                                         RETURNING RESU.
           MOVE C-TKCD     TO TK-KEY.
           PERFORM TKG-RTN THRU TKG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                             RETURNING RESU
               GO TO TENO-RTN
           END-IF.
           IF  TK-WNK = 1
               CALL "SD_Output" USING "DSP-WNK" DSP-WNK "p"
                                             RETURNING RESU
           ELSE
               CALL "SD_Output" USING "DSP-WNKC" DSP-WNKC "p"
                                             RETURNING RESU
           END-IF.
           CALL "SD_Output" USING "DSP-TKNM" DSP-TKNM "p"
                                         RETURNING RESU.
           GO TO TENO-020.
       TENO-010.
           IF (CRDATE NOT = B-CRDATE) OR (C-TKCD NOT = B-TKCD)
               CALL "SD_Output" USING "MG-06" MG-06 "p"
                                             RETURNING RESU
               GO TO TENO-RTN
           END-IF.
       TENO-020.
           IF  C-TENO1(I) = 00
               MOVE 0110 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               GO TO TENO-EX
           END-IF.
           IF  C-TENO1(I) = 01 OR 02
               MOVE 0120 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               GO TO TENO-EX
           END-IF.
           IF  C-TENO1(I) = 03
               MOVE 2120 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE 0140 TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE "相殺" TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-EX
           END-IF.
           IF  C-TENO1(I) = 04
               MOVE ZERO TO C-KRCDM(I) C-KRCDS(I)
               MOVE 0140 TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE "相殺" TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-EX
           END-IF.
           IF  C-TENO1(I) = 11
               MOVE 0130 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE ZERO TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE TD-MG TO W-GETZ
               MOVE TD-MP TO W-PEYZ
               MOVE W-GPZ TO W-GPDZ
               MOVE W-GPDZ TO C-TEKI11(I)
               MOVE "約手入　　　　" TO C-TEKI12(I)
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) = 12
               MOVE 0130 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE ZERO TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE TD-MG TO W-GETZ
               MOVE TD-MP TO W-PEYZ
               MOVE W-GPZ TO W-GPDZ
               MOVE W-GPDZ TO C-TEKI11(I)
               MOVE "為手入　　　　" TO C-TEKI12(I)
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) = 13
               MOVE 0130 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE ZERO TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               MOVE TD-KIN TO C-KRKIN(I) C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE TD-MG TO W-GETZ
               MOVE TD-MP TO W-PEYZ
               MOVE W-GPZ TO W-GPDZ
               MOVE W-GPDZ TO C-TEKI11(I)
               MOVE "電債入　　　　" TO C-TEKI12(I)
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) = 30
               MOVE 2120 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE 0130 TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               COMPUTE W-KIN = TD-S(1) + TD-S(2)
               MOVE W-KIN TO C-KRKIN(I)
               MOVE TD-KIN TO C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE "品代払" TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) = 31 OR 32
               MOVE 2120 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE 0120 TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               COMPUTE W-KIN = TD-S(1) + TD-S(2)
               MOVE W-KIN TO C-KRKIN(I)
               MOVE TD-KIN TO C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE "品代払" TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) = 33
               MOVE 2120 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE 0140 TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               COMPUTE W-KIN = TD-S(1) + TD-S(2)
               MOVE W-KIN TO C-KRKIN(I)
               MOVE TD-KIN TO C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE "相殺" TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) = 34
               MOVE 2120 TO C-KRCDM(I)
               MOVE ZERO TO C-KRCDS(I)
               MOVE ZERO TO C-KSCDM(I)
               MOVE ZERO TO C-KSCDS(I)
               COMPUTE W-KIN = TD-S(1) + TD-S(2)
               MOVE W-KIN TO C-KRKIN(I)
               MOVE TD-KIN TO C-KSKIN(I)
               MOVE SPACE TO C-TEKI(I)
               MOVE "相殺" TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
               GO TO TENO-030
           END-IF.
           IF  C-TENO1(I) NOT = 22
               GO TO TENO-EX
           END-IF.
           COMPUTE W-KIN = TD-S(1) + TD-S(2).
           IF  W-KIN = ZERO
               MOVE ZERO TO C-KRCDM(I)
               MOVE SPACE TO C-TEKI21(I)
           ELSE
               MOVE 2120 TO C-KRCDM(I)
               MOVE "品代払" TO C-TEKI21(I)
           END-IF.
           MOVE ZERO TO C-KRCDS(I).
           MOVE 2110 TO C-KSCDM(I).
           MOVE ZERO TO C-KSCDS(I).
           MOVE W-KIN TO C-KRKIN(I).
           MOVE TD-KIN TO C-KSKIN(I).
           MOVE SPACE TO C-TEKI(I).
           MOVE TD-MG TO W-GETZ.
           MOVE TD-MP TO W-PEYZ.
           MOVE W-GPZ TO W-GPDZ.
           MOVE W-GPDZ TO C-TEKI22(I).
           IF  TD-BKC = 2200
               MOVE "商工中金　　　　　　　　" TO C-TEKI23(I)
           END-IF.
           IF  TD-BKC = 3200
               MOVE "中国銀行　　　　　　　　" TO C-TEKI23(I)
           END-IF.
           CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                         RETURNING RESU.
       TENO-030.
           IF  TD-MND = ZERO
               GO TO TENO-040
           END-IF.
           MOVE ZERO TO W-NG.
           MOVE TD-MNG TO W-NGS.
           IF  W-NEN2 >= DATE-YF1 AND <= DATE-YT1
               ADD DATE-YC1 TO W-NEN
           ELSE
               IF  W-NEN2 >= DATE-YF2 AND <= DATE-YT2
                   ADD DATE-NC2 TO W-NEN
               END-IF
           END-IF.
           MOVE W-NEN TO C-TKDY(I).
           MOVE TD-MG TO C-TKDM(I).
           MOVE TD-MP TO C-TKDD(I).
           IF  C-TENO1(I) < 20
               GO TO TENO-EX
           END-IF.
       TENO-040.
           COMPUTE W-KIN = TD-ZSHZ + TD-SSHZ.
           IF  W-KIN NOT = ZERO
               MOVE 1 TO W-DC
           END-IF.
           GO TO TENO-EX.
       TENO-050.
           MOVE ZERO TO C-TENO(I).
           MOVE 2120 TO C-KRCDM(I).
           MOVE ZERO TO C-KRCDS(I).
           MOVE ZERO TO C-KSCDM(I).
           MOVE ZERO TO C-KSCDS(I).
           COMPUTE W-KIN = TD-ZSHZ + TD-SSHZ.
           MOVE W-KIN TO C-KRKIN(I).
           MOVE ZERO TO C-KSKIN(I).
           MOVE SPACE TO C-TEKI(I).
           MOVE "消費税額" TO C-TEKI(I).
           CALL "SD_Output" USING "DSP-TENO" DSP-TENO "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                         RETURNING RESU.
           MOVE 0     TO W-DC.
       TENO-EX.
           EXIT.
      ****************** ＜　借方　＞ ******************************************
       KRCDM-RTN.
           CALL "SD_Output" USING "CLE-SP20-KR1" CLE-SP20-KR1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KRCDM" ACP-KRCDM "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRCDM "ACP-KRCDM" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "04"
               IF  I = 1
                   GO TO KRCDM-RTN
               ELSE
                   GO TO KRCDM-999
               END-IF
           END-IF.
           IF  ESTAT = "09"
               GO TO KRCDM-999
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRCDM-RTN
           END-IF.
           IF  C-KRCDM(I) = ZERO
               INITIALIZE C-KR-CDMN(I)
                          C-KR-TB(I)
                          C-KR-HO(I)
                          C-KR-KH(I)
                          C-KR-TAX(I)
                          C-KR-BSPL(I)
               GO TO KRCDM-999
           END-IF.
           MOVE 1     TO DRCR-SW.
           PERFORM AMG-RTN THRU AMG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                             RETURNING RESU
               GO TO KRCDM-RTN
           END-IF.
           MOVE 1     TO DRCR-SW.
           MOVE 0     TO HOJO-SW.
           PERFORM KNGG-RTN THRU KNGG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                             RETURNING RESU
               GO TO KRCDM-RTN
           END-IF.
       KRCDM-999.
           CALL "SD_Output" USING "DSP-KRCDMN" DSP-KRCDMN "p"
                                         RETURNING RESU.
           IF  C-TENO(I) = ZERO
               IF  C-KRCDM(I) = 0130 OR 2110
                   CALL "SD_Output" USING "MG-07" MG-07 "p"
                                              RETURNING RESU
                   GO TO KRCDM-RTN
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 2120
               IF  C-KSCDM(I) = 2110
                   IF  C-TEKI21(I) = SPACE
                       MOVE "品代払" TO C-TEKI21(I)
                       CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
                   END-IF
               END-IF
           END-IF.
       KRCDM-EX.
           EXIT.
      ***
       KRCDS-RTN.
           IF  C-KRCDM(I) = ZERO
               GO TO KRCDS-998
           END-IF.
           IF  C-KR-HO(I) = 1
               GO TO KRCDS-000
           END-IF.
           IF (C-KR-TB(I) NOT < 01) AND
              (C-KR-TB(I) NOT > 12)
               GO TO KRCDS-000
           END-IF.
           GO TO KRCDS-998.
       KRCDS-000.
           CALL "SD_Output" USING "CLE-SP20-KR2" CLE-SP20-KR2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KRCDS" ACP-KRCDS "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRCDS "ACP-KRCDS" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRCDS-999
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRCDS-000
           END-IF.
           IF  C-KRCDS(I) = ZERO
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO KRCDS-000
           END-IF.
           MOVE SPACE     TO C-KR-CDSN(I).
           IF  C-KR-HO(I) = 1
               MOVE 1     TO DRCR-SW
               MOVE 1     TO HOJO-SW
               PERFORM KNGG-RTN THRU KNGG-EX
               IF  INV-SW = 1
                   GO TO KRCDS-ERR
               END-IF
           END-IF.
           IF (C-KR-TB(I) NOT < 01) AND
              (C-KR-TB(I) NOT > 12)
               CONTINUE
           ELSE
               GO TO KRCDS-999
           END-IF.
           MOVE 1     TO DRCR-SW.
           PERFORM BMG-RTN THRU BMG-EX.
           IF  INV-SW = 1
               GO TO KRCDS-ERR
           END-IF.
           IF  C-KR-HO(I) = 0
               MOVE BANKNMN     TO C-KR-CDSN(I)
           END-IF.
           MOVE 1     TO DRCR-SW.
           PERFORM GYMG-RTN THRU GYMG-EX.
           IF  INV-SW = 1
               GO TO KRCDS-ERR
           END-IF.
           GO TO KRCDS-999.
       KRCDS-ERR.
           CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                         RETURNING RESU.
           GO TO KRCDS-000.
       KRCDS-998.
           INITIALIZE C-KRCDS(I) C-KR-CDSN(I).
       KRCDS-999.
           CALL "SD_Output" USING "DSP-KRCDSN" DSP-KRCDSN "p"
                                         RETURNING RESU.
       KRCDS-EX.
           EXIT.
      ***
       KRSECT-RTN.
           IF  C-KRCDM(I) = ZERO
               GO TO KRSECT-999
           END-IF.
           IF  C-KRCDM(I) >= 1110 AND <= 1500
               GO TO KRSECT-000
           END-IF.
           IF  C-KR-KH(I) = 1
               GO TO KRSECT-000
           END-IF.
           IF (C-KR-BSPL(I) = 1) AND
              (WSUB5 = 0)
               GO TO KRSECT-000
           END-IF.
           GO TO KRSECT-999.
       KRSECT-000.
           IF  C-ACT = 1
               IF  C-KRSECT(I) = ZERO
                   IF  C-TKCD NOT = ZERO
                       COMPUTE C-KRSECT(I) = TK-BKC * 100
                   END-IF
               END-IF
           END-IF.
           CALL "SD_Output" USING "ACP-KRSECT" ACP-KRSECT "p"
                                         RETURNING RESU.
       KRSECT-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRSECT "ACP-KRSECT" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRSECT-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRSECT-010
           END-IF.
           MOVE 1     TO DRCR-SW.
           PERFORM BNMG-RTN THRU BNMG-EX.
           IF  INV-SW = 1
               GO TO KRSECT-ERR
           END-IF.
           IF  C-KR-KH(I) = 1
               MOVE 1     TO DRCR-SW
               PERFORM HHFG-RTN THRU HHFG-EX
               IF  INV-SW = 1
                   GO TO KRSECT-ERR
               END-IF
           END-IF.
           GO TO KRSECT-EX.
       KRSECT-ERR.
           CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                         RETURNING RESU.
           GO TO KRSECT-010.
       KRSECT-999.
           INITIALIZE C-KRSECT(I).
           CALL "SD_Output" USING "CLE-KRSECT" CLE-KRSECT "p"
                                         RETURNING RESU.
       KRSECT-EX.
           EXIT.
      ***
       KRNKCD-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRNKCD "ACP-KRNKCD" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRNKCD-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRNKCD-RTN
           END-IF.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE C-KRNKCD1(I) TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KRNKCD-RTN
           END-IF.
      *
           IF  C-KRNKCD2(I) NOT = 0 AND 8 AND 9
               GO TO KRNKCD-RTN
           END-IF.
           IF  C-KRNKCD(I) = "78" OR "98"
               GO TO KRNKCD-RTN
           END-IF.
           IF  C-KRNKCD1(I) NOT = 7
               IF  C-KRNKCD2(I) = 9
                   GO TO KRNKCD-RTN
               END-IF
           END-IF.
       KRNKCD-EX.
           EXIT.
      ***
       KRNSC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRNSC "ACP-KRNSC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRNSC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRNSC-RTN
           END-IF.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE C-KRNSC(I) TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KRNSC-RTN
           END-IF.
       KRNSC-EX.
           EXIT.
      ***
       KRSKNG-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRSKNG "ACP-KRSKNG" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRSKNG-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRSKNG-RTN
           END-IF.
           IF  C-KRSKNG(I) = ZERO
               CALL "SD_Output" USING "CLE-KRSKNG" CLE-KRSKNG "p"
                                              RETURNING RESU
               GO TO KRSKNG-RTN
           END-IF.
           IF  C-KRSKG(I) < 1 OR > 12
               GO TO KRSKNG-RTN
           END-IF.
           MOVE ZERO TO W-NG.
           MOVE C-KRSKNG(I) TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           IF  CRYM < W-NG
               GO TO KRSKNG-RTN
           END-IF.
       KRSKNG-EX.
           EXIT.
      ***
       KRKIN-RTN.
           IF  C-KRCDM(I) = ZERO
               MOVE ZERO     TO C-KRKIN(I)
               GO TO KRKIN-999
           END-IF.
           IF  C-KRCDM(I) = 0130
               GO TO KRKIN-999
           END-IF.
       KRKIN-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRKIN "ACP-KRKIN" "S9" "10"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRKIN-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRKIN-000
           END-IF.
           IF  C-KRKIN(I) = ZERO
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO KRKIN-000
           END-IF.
       KRKIN-999.
           CALL "SD_Output" USING "DSP-KRKIN" DSP-KRKIN "p"
                                         RETURNING RESU.
       KRKIN-EX.
           EXIT.
      ***
       KRTAX-RTN.
           IF (C-DNNO    < 300000) OR (C-DNNO > 399999 AND < 600000)
               IF C-KRCDM(I) = W-KRCDM OR W-KSCDM
                   GO TO KRTAX-000
               END-IF
           END-IF.
           IF (C-KRCDM(I) = ZERO) OR
              (C-KR-TAX(I) = SPACE)
               MOVE SPACE     TO C-KRTAX(I)
               GO TO KRTAX-999
           END-IF.
           IF  C-KR-TAX(I) = "1" OR "3" OR "5" OR "07"
               MOVE C-KR-TAX(I)     TO C-KRTAX(I)
               GO TO KRTAX-999
           END-IF.
       KRTAX-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KRTAX "ACP-KRTAX" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KRTAX-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KRTAX-000
           END-IF.
      *
           IF  C-KR-TAX(I) = "2"
               IF  C-KRTAX(I) NOT = " " AND "1" AND "3"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KRTAX-000
               END-IF
           END-IF.
           IF  C-KR-TAX(I) = "6"
               IF  C-KRTAX(I) NOT = " " AND "5" AND "7"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KRTAX-000
               END-IF
           END-IF.
      *
           IF  C-KRCDM(I) = W-KRCDM
               IF  C-KRTAX(I) NOT = " " AND "1" AND "3"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KRTAX-000
               END-IF
           END-IF.
           IF  C-KRCDM(I) = W-KSCDM
               IF  C-KRTAX(I) NOT = " " AND "5" AND "7"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KRTAX-000
               END-IF
           END-IF.
       KRTAX-999.
           CALL "SD_Output" USING "ACP-KRTAX" ACP-KRTAX "p"
                                         RETURNING RESU.
       KRTAX-EX.
           EXIT.
       KSCDM-RTN.
           CALL "SD_Output" USING "CLE-SP20-KS1" CLE-SP20-KS1 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KSCDM" ACP-KSCDM "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSCDM "ACP-KSCDM" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSCDM-999
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSCDM-RTN
           END-IF.
           IF  C-KSCDM(I) = ZERO
               INITIALIZE C-KS-CDMN(I)
                          C-KS-TB(I)
                          C-KS-HO(I)
                          C-KS-KH(I)
                          C-KS-TAX(I)
                          C-KS-BSPL(I)
               GO TO KSCDM-999
           END-IF.
           MOVE 2     TO DRCR-SW.
           PERFORM AMG-RTN THRU AMG-EX.
           IF INV-SW = 1
              CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                             RETURNING RESU
              GO TO KSCDM-RTN
           END-IF.
           MOVE 2     TO DRCR-SW.
           MOVE 0     TO HOJO-SW.
           PERFORM KNGG-RTN THRU KNGG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                              RETURNING RESU
               GO TO KSCDM-RTN
           END-IF.
       KSCDM-999.
           CALL "SD_Output" USING "DSP-KSCDMN" DSP-KSCDMN "p"
                                         RETURNING RESU.
           IF  C-TENO(I) = ZERO
               IF  C-KSCDM(I) = 0130 OR 2110
                   CALL "SD_Output" USING "MG-07" MG-07 "p"
                                              RETURNING RESU
                   GO TO KSCDM-RTN
               END-IF
           END-IF.
           IF  C-KSCDM(I) NOT = 0140
               GO TO KSCDM-EX
           END-IF.
           IF  C-KRCDM(I) = 0110
               IF  C-TEKI(I) = SPACE
                   MOVE "品代入" TO C-TEKI(I)
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0120
               IF  C-TEKI(I) = SPACE
                   MOVE "小切手取立　品代入　　　" TO C-TEKI(I)
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0130
               IF  C-TEKI12(I) = "約手入　　　　"
                   MOVE "約手入　品代入" TO C-TEKI12(I)
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0130
               IF  C-TEKI12(I) = "為手入　　　　"
                   MOVE "為手入　品代入" TO C-TEKI12(I)
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0130
               IF  C-TEKI12(I) = "電債入　　　　"
                   MOVE "電債入　品代入" TO C-TEKI12(I)
               END-IF
           END-IF.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                         RETURNING RESU.
       KSCDM-EX.
           EXIT.
      ***
       KSCDS-RTN.
           IF  C-KSCDM(I) = ZERO
               GO TO KSCDS-998
           END-IF.
           IF  C-KS-HO(I) = 1
               GO TO KSCDS-000
           END-IF.
           IF (C-KS-TB(I) NOT < 01) AND
              (C-KS-TB(I) NOT > 12)
               GO TO KSCDS-000
           END-IF.
           IF  C-KS-TB(I) = 22 OR 23
               GO TO KSCDS-000
           END-IF.
           GO TO KSCDS-998.
       KSCDS-000.
           CALL "SD_Output" USING "CLE-SP20-KS2" CLE-SP20-KS2 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-KSCDS" ACP-KSCDS "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSCDS "ACP-KSCDS" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSCDS-999
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSCDS-000
           END-IF.
           IF  C-KSCDS(I) = ZERO
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO KSCDS-000
           END-IF.
           MOVE SPACE     TO C-KS-CDSN(I).
           IF  C-KS-HO(I) = 1
               MOVE 2     TO DRCR-SW
               MOVE 1     TO HOJO-SW
               PERFORM KNGG-RTN THRU KNGG-EX
               IF  INV-SW = 1
                   GO TO KSCDS-ERR
               END-IF
           END-IF.
           IF  C-KS-TB(I) = 22 OR 23
               GO TO KSCDS-100
           END-IF.
           IF (C-KS-TB(I) NOT < 01) AND
              (C-KS-TB(I) NOT > 12)
               GO TO KSCDS-100
           END-IF.
           GO TO KSCDS-999.
       KSCDS-100.
           MOVE 2     TO DRCR-SW.
           PERFORM BMG-RTN THRU BMG-EX.
           IF  INV-SW = 1
               GO TO KSCDS-ERR
           END-IF.
           IF  C-KS-HO(I) = 0
               MOVE BANKNMN     TO C-KS-CDSN(I)
           END-IF.
           IF  C-KS-TB(I) = 22 OR 23
               GO TO KSCDS-999
           END-IF.
       KSCDS-200.
           MOVE 2     TO DRCR-SW.
           PERFORM GYMG-RTN THRU GYMG-EX.
           IF  INV-SW = 1
               GO TO KSCDS-ERR
           END-IF.
           GO TO KSCDS-999.
       KSCDS-ERR.
           CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                             RETURNING RESU.
           GO TO KSCDS-000.
       KSCDS-998.
           INITIALIZE C-KSCDS(I) C-KS-CDSN(I).
       KSCDS-999.
           CALL "SD_Output" USING "DSP-KSCDSN" DSP-KSCDSN "p"
                                         RETURNING RESU.
       KSCDS-EX.
           EXIT.
      ***
       KSSECT-RTN.
           IF  C-KSCDM(I) = ZERO
               GO TO KSSECT-999
           END-IF.
           IF  C-KSCDM(I) >= 1110 AND <= 1500
               GO TO KSSECT-000
           END-IF.
           IF  C-KS-KH(I) = 1
               GO TO KSSECT-000
           END-IF.
           IF (C-KS-BSPL(I) = 1) AND
              (WSUB5 = 0)
               GO TO KSSECT-000
           END-IF.
           GO TO KSSECT-999.
       KSSECT-000.
           IF  C-ACT = 1
               IF  C-KSSECT(I) = ZERO
                   IF  C-TKCD NOT = ZERO
                       COMPUTE C-KSSECT(I) = TK-BKC * 100
                   END-IF
               END-IF
           END-IF.
           CALL "SD_Output" USING "ACP-KSSECT" ACP-KSSECT "p"
                                         RETURNING RESU.
       KSSECT-010.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSSECT "ACP-KSSECT" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSSECT-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSSECT-010
           END-IF.
           MOVE 2     TO DRCR-SW.
           PERFORM BNMG-RTN THRU BNMG-EX.
           IF  INV-SW = 1
               GO TO KSSECT-ERR
           END-IF.
           IF  C-KS-KH(I) = 1
               MOVE 2     TO DRCR-SW
               PERFORM HHFG-RTN THRU HHFG-EX
               IF  INV-SW = 1
                   GO TO KSSECT-ERR
               END-IF
           END-IF.
           GO TO KSSECT-EX.
       KSSECT-ERR.
           CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                             RETURNING RESU.
           GO TO KSSECT-010.
       KSSECT-999.
           INITIALIZE C-KSSECT(I).
           CALL "SD_Output" USING "CLE-KSSECT" CLE-KSSECT "p"
                                         RETURNING RESU.
       KSSECT-EX.
           EXIT.
      ***
       KSNKCD-RTN.
           IF  C-KRNKCD(I) NOT = ZERO
               MOVE ZERO TO C-KSNKCD(I) C-KSNSC(I) C-KSSKNG(I)
               CALL "SD_Output" USING "CLE-KSNKCD" CLE-KSNKCD "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "CLE-KSNSC" CLE-KSNSC "p"
                                             RETURNING RESU
               CALL "SD_Output" USING "CLE-KSSKNG" CLE-KSSKNG "p"
                                             RETURNING RESU
               GO TO KSNKCD-EX
           END-IF.
       KSNKCD-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSNKCD "ACP-KSNKCD" "9" "2"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSNKCD-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSNKCD-000
           END-IF.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "31" TO HKB-NO.
           MOVE C-KSNKCD1(I) TO HKB-NKC1.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KSNKCD-000
           END-IF.
      *
           IF  C-KSNKCD2(I) NOT = 0 AND 8 AND 9
               GO TO KSNKCD-000
           END-IF.
           IF  C-KSNKCD(I) = "78" OR "98"
               GO TO KSNKCD-000
           END-IF.
           IF  C-KSNKCD1(I) NOT = 7
               IF  C-KSNKCD2(I) = 9
                   GO TO KSNKCD-000
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0110
               IF  C-KSNKCD1(I) NOT = 0
                   CALL "SD_Output" USING "MG-09" MG-09 "p"
                                             RETURNING RESU
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0120
               IF  C-KSNKCD1(I) NOT = 1 AND 2
                   CALL "SD_Output" USING "MG-09" MG-09 "p"
                                             RETURNING RESU
               END-IF
           END-IF.
           IF  C-KRCDM(I) = 0130
               IF  C-KSNKCD1(I) NOT = 3 AND 4
                   CALL "SD_Output" USING "MG-09" MG-09 "p"
                                             RETURNING RESU
               END-IF
           END-IF.
       KSNKCD-EX.
           EXIT.
      ***
       KSNSC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSNSC "ACP-KSNSC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSNSC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSNSC-RTN
           END-IF.
      *
           MOVE SPACE TO HKB-KEY.
           MOVE "32" TO HKB-NO.
           MOVE C-KSNSC(I) TO HKB-NSC.
      *           READ HKBM WITH UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HKBM_PNAME1 BY REFERENCE HKB-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO KSNSC-RTN
           END-IF.
      *
           IF  C-KSNSC(I) = 1 OR 2
               IF  C-KRCDM(I) NOT = 7119
                   GO TO KSNSC-RTN
               ELSE
                   IF  C-KRCDS(I) NOT = 0001
                       GO TO KSNSC-RTN
                   END-IF
               END-IF
           END-IF.
           IF  C-KSNSC(I) = 3
               IF  C-KRCDM(I) NOT = 8202
                   GO TO KSNSC-RTN
               END-IF
           END-IF.
           IF  C-KSNSC(I) = 4
               IF  C-KRCDM(I) NOT = 7119
                   GO TO KSNSC-RTN
               ELSE
                   IF  C-KRCDS(I) NOT = 0003
                       GO TO KSNSC-RTN
                   END-IF
               END-IF
           END-IF.
           IF  C-KSNSC(I) = 5
               IF  C-KRCDM(I) NOT = 7119
                   GO TO KSNSC-RTN
               ELSE
                   IF  C-KRCDS(I) NOT = 0002
                       GO TO KSNSC-RTN
                   END-IF
               END-IF
           END-IF.
           IF  C-KSNSC(I) = 6
               IF  C-KRCDM(I) NOT = 7130
                   GO TO KSNSC-RTN
               END-IF
           END-IF.
           IF  C-TEKI(I)  = SPACE
               MOVE HKB-NSNA TO C-TEKI(I)
               CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                             RETURNING RESU
           END-IF.
       KSNSC-EX.
           EXIT.
      ***
       KSSKNG-RTN.
           IF  C-KRSKNG(I) NOT = ZERO
               CALL "SD_Output" USING "CLE-KSSKNG" CLE-KSSKNG "p"
                                             RETURNING RESU
               GO TO KSSKNG-EX
           END-IF.
       KSSKNG-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSSKNG "ACP-KSSKNG" "9" "4"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSSKNG-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSSKNG-000
           END-IF.
           IF  C-KSSKNG(I) = ZERO
               CALL "SD_Output" USING "CLE-KSSKNG" CLE-KSSKNG "p"
                                             RETURNING RESU
               GO TO KSSKNG-EX
           END-IF.
           IF  C-KSSKG(I) < 1 OR > 12
               GO TO KSSKNG-000
           END-IF.
           MOVE ZERO TO W-NG.
           MOVE C-KSSKNG(I) TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           IF  CRYM < W-NG
               GO TO KSSKNG-000
           END-IF.
       KSSKNG-EX.
           EXIT.
      ***
       KSKIN-RTN.
           IF  C-KSCDM(I) = ZERO
               MOVE ZERO     TO C-KSKIN(I)
               GO TO KSKIN-999
           END-IF.
           IF  C-KSCDM(I) = 2110
               GO TO KSKIN-999
           END-IF.
       KSKIN-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSKIN "ACP-KSKIN" "S9" "10"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSKIN-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSKIN-000
           END-IF.
           IF  C-KSKIN(I) = ZERO
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO KSKIN-000
           END-IF.
       KSKIN-999.
           CALL "SD_Output" USING "DSP-KSKIN" DSP-KSKIN "p"
                                         RETURNING RESU.
       KSKIN-EX.
           EXIT.
      ***
       KSTAX-RTN.
           IF (C-DNNO    < 300000) OR (C-DNNO > 399999 AND < 600000)
               IF  C-KSCDM(I) = W-KRCDM OR W-KSCDM
                   GO TO KSTAX-000
               END-IF
           END-IF.
           IF (C-KSCDM(I) = ZERO) OR
              (C-KS-TAX(I) = SPACE)
               MOVE SPACE     TO C-KSTAX(I)
               GO TO KSTAX-999
           END-IF.
           IF  C-KS-TAX(I) = "1" OR "3" OR "5" OR "7"
               MOVE C-KS-TAX(I)     TO C-KSTAX(I)
               GO TO KSTAX-999
           END-IF.
       KSTAX-000.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-KSTAX "ACP-KSTAX" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO KSTAX-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO KSTAX-000
           END-IF.
      *
           IF  C-KS-TAX(I) = "2"
               IF  C-KSTAX(I) NOT = " " AND "1" AND "3"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KSTAX-000
               END-IF
           END-IF.
           IF C-KS-TAX(I) = "6"
              IF C-KSTAX(I) NOT = " " AND "5" AND "7"
                 CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                             RETURNING RESU
                 GO TO KSTAX-000
              END-IF
           END-IF.
      *
           IF  C-KSCDM(I) = W-KRCDM
               IF  C-KSTAX(I) NOT = " " AND "1" AND "3"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KSTAX-000
               END-IF
           END-IF.
           IF  C-KSCDM(I) = W-KSCDM
               IF  C-KSTAX(I) NOT = " " AND "5" AND "7"
                   CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                               RETURNING RESU
                   GO TO KSTAX-000
               END-IF
           END-IF.
       KSTAX-999.
           CALL "SD_Output" USING "ACP-KSTAX" ACP-KSTAX "p"
                                         RETURNING RESU.
       KSTAX-EX.
           EXIT.
      *********
       TEKI-RTN.
           IF  (C-KRCDM(I) = ZERO) AND
               (C-KSCDM(I) = ZERO)
               INITIALIZE C-TEKICD(I) C-TEKI(I)
               GO TO TEKI-999
           END-IF.
       TEKI-000.
           CALL "SD_Output" USING "CLE-SP40" CLE-SP40 "p"
                                         RETURNING RESU.
           CALL "SD_Output" USING "ACP-TEKICD" ACP-TEKICD "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TEKICD "ACP-TEKICD" "9" "3"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO TEKI-999
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO TEKI-000
           END-IF.
           IF  C-TEKICD(I) = ZERO
               GO TO TEKI-500
           END-IF.
           PERFORM TKIG-RTN THRU TKIG-EX.
           IF  INV-SW = 1
               CALL "SD_Output" USING "INV-M01" INV-M01 "p"
                                              RETURNING RESU
               GO TO TEKI-000
           END-IF.
           MOVE ZERO     TO C-TEKICD(I).
       TEKI-500.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                         RETURNING RESU.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-TEKI "ACP-TEKI" "N" "40"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO TEKI-000
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO TEKI-500
           END-IF.
           GO TO TEKI-EX.
       TEKI-999.
           CALL "SD_Output" USING "ACP-TEKI" ACP-TEKI "p"
                                         RETURNING RESU.
       TEKI-EX.
           EXIT.
      ***
       GOKEI-RTN.
           MOVE ZERO     TO C-KRTOT C-KSTOT ERR-SW.
           MOVE 1        TO SOE.
       GOKEI-000.
           ADD C-KRKIN(SOE)     TO C-KRTOT.
           ADD C-KSKIN(SOE)     TO C-KSTOT.
           IF  C-KRCDM(SOE) = W-KRCDM OR W-KSCDM
               IF  C-KRTAX(SOE) NOT = " "
                   IF  C-KS-TAX(SOE) = " "
                       MOVE 1 TO ERR-SW
                       CALL "SD_Output" USING "MG-11" MG-11 "p"
                                             RETURNING RESU
                       GO TO GOKEI-EX
                   END-IF
               END-IF
           END-IF.
           IF  C-KSCDM(SOE) = W-KRCDM OR W-KSCDM
               IF  C-KSTAX(SOE) NOT = " "
                   IF  C-KR-TAX(SOE) = " "
                       MOVE 1 TO ERR-SW
                       CALL "SD_Output" USING "MG-11" MG-11 "p"
                                             RETURNING RESU
                       GO TO GOKEI-EX
                   END-IF
               END-IF
           END-IF.
           IF  SOE NOT = 5
               ADD 1     TO SOE
               GO TO GOKEI-000
           END-IF.
           CALL "SD_Output" USING "DSP-AREA2" DSP-AREA2 "p"
                                         RETURNING RESU.
           IF  C-KRTOT NOT = C-KSTOT
               MOVE 1     TO ERR-SW
               CALL "SD_Output" USING "MG-04" MG-04 "p"
                                              RETURNING RESU
           END-IF.
       GOKEI-EX.
           EXIT.
      *****
       CHK-RTN.
           MOVE 0        TO ERR-SW.
           MOVE 1        TO SOE.
       CHK-010.
           IF  C-KRCDM(SOE) = 0110 OR 0120 OR 0130 OR 0140 OR
                              2110 OR 2120
               GO TO CHK-020
           END-IF.
           IF  C-KSCDM(SOE) = 0130 OR 0140 OR 2110 OR 2120
               GO TO CHK-020
           END-IF.
           IF  SOE NOT = 5
               ADD 1     TO SOE
               GO TO CHK-010
           END-IF.
           CALL "SD_Output" USING "MG-08" MG-08 "p"
                                         RETURNING RESU.
           MOVE 1        TO ERR-SW.
           GO TO CHK-EX.
       CHK-020.
           MOVE ZERO     TO SOE C-TCD W-CHK.
       CHK-030.
           ADD 1     TO SOE.
           IF  SOE = 6
               GO TO CHK-EX
           END-IF.
           IF (C-KRCDM(SOE) NOT = 0140) OR
              (C-KSCDM(SOE) > 4999 AND < 6000)
               GO TO CHK-040
           END-IF.
           IF  C-TKCD > 09999 AND < 20000
               MOVE C-TKCD TO C-TCD
               GO TO CHK-EX
           END-IF.
           IF  C-TKCD > 29999
               IF  TK-TCD NOT = ZERO
                   MOVE TK-TCD TO C-TCD
                   MOVE 1 TO W-CHK
                   GO TO CHK-EX
               END-IF
           END-IF.
           GO TO CHK-050.
       CHK-040.
           IF (C-KSCDM(SOE) NOT = 0140) OR
              (C-KRCDM(SOE) > 4999 AND < 6000)
               GO TO CHK-030
           END-IF.
           IF  C-TKCD > 09999 AND < 20000
               MOVE C-TKCD TO C-TCD
               GO TO CHK-EX
           END-IF.
           IF  C-TKCD > 29999
               IF  TK-TCD NOT = TK-CD2
                   MOVE 1 TO W-CHK
                   MOVE TK-TCD TO C-TCD
                   GO TO CHK-EX
               END-IF
           END-IF.
       CHK-050.
           CALL "SD_Output" USING "MG-10" MG-10 "p"
                                         RETURNING RESU.
           MOVE 1        TO ERR-SW.
       CHK-EX.
           EXIT.
      ***
       HHC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-HHC "ACP-HHC" "9" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO HHC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO HHC-RTN
           END-IF.
           IF  W-HHC NOT = 0 AND 8
               GO TO HHC-RTN
           END-IF.
       HHC-EX.
           EXIT.
      ***
       SKD-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-SKD "ACP-SKD" "9" "8"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO SKD-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO SKD-RTN
           END-IF.
           IF  W-SKDD NOT = ZERO
               IF  W-SKDG < 1 OR > 12
                   GO TO SKD-RTN
               END-IF
           END-IF.
           IF  W-SKDD NOT = ZERO
               IF  W-SKDP < 1 OR > 31
                   GO TO SKD-RTN
               END-IF
           END-IF.
           IF  W-SKDD NOT = ZERO
               IF  CRDATE > W-SKDD
                   GO TO SKD-RTN
               END-IF
           END-IF.
           IF  W-SDATE NOT = ZERO
               IF  W-SDATE >= W-SKDD
                   GO TO SKD-RTN
               END-IF
           END-IF.
       SKD-EX.
           EXIT.
      ***
       OKC-RTN.
           CALL "SD_Accept" USING
                 BY REFERENCE ACP-OKC "ACP-OKC" "X" "1"
                 BY REFERENCE ESTAT RETURNING RESU.
           CALL "SD_Output" USING "DISP-MSG-SPACE" DISP-MSG-SPACE "p"
                                         RETURNING RESU.
           IF  ESTAT = "09"
               GO TO OKC-EX
           END-IF.
           IF  ESTAT NOT = "01" AND "06"
               GO TO OKC-RTN
           END-IF.
           IF  C-OKC NOT = "1" AND "9"
               CALL "SD_Output" USING "ERR-01" ERR-01 "p"
                                              RETURNING RESU
               GO TO OKC-RTN
           END-IF.
       OKC-EX.
           EXIT.
      ***
       CRE-RTN.
           INITIALIZE CRT-ITEM(I).
           IF  I NOT = 5
               ADD 1     TO I
               GO TO CRE-RTN
           END-IF.
           MOVE 0 TO W-HHC.
       CRE-EX.
           EXIT.
      *********
       DNOU-RTN.
           MOVE "11"     TO DNO1-KEY ERR-K.
      *           READ NS-DNO INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" NS-DNO_PNAME1 BY REFERENCE DNO-R " "
            RETURNING RET.
           IF  RET = 1
               MOVE "NS-DNO"     TO ERR-F
               MOVE "G"          TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       DNOU-000.
           IF  DNO1-023 = DNO1-022
               MOVE DNO1-021     TO DNO1-023
           ELSE
               ADD 1     TO DNO1-023
           END-IF.
      *
           MOVE DNO1-023   TO SDWJNO.
           MOVE ZERO       TO SDWLNO.
      *           START SDW KEY NOT LESS SDW-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDW_PNAME1 "SDW-KEY" " NOT < " SDW-KEY RETURNING RET.
           IF  RET = 1
                 GO TO DNOU-100
           END-IF
      *           READ SDW NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDW_PNAME1 BY REFERENCE SDW-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
                GO TO DNOU-100
           END-IF.
           IF  DNO1-023 = SDWJNO
               GO TO DNOU-000
           END-IF.
       DNOU-100.
           MOVE CRDATE     TO HTRDATE.
           MOVE DNO1-023   TO HJUNLNO.
           MOVE ZERO       TO HLINENO HDR-CR.
      *           START SDH KEY NOT LESS SH-KEY1 INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDH_PNAME1 "SH-KEY1" " NOT LESS " SH-KEY1 RETURNING RET.
           IF  RET = 1
               GO TO DNOU-999
           END-IF
      *           READ SDH NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDH_PNAME1 BY REFERENCE SH-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO DNOU-999
           END-IF.
           IF (CRDATE = HTRDATE) AND
              (DNO1-023 = HJUNLNO)
               GO TO DNOU-000
           END-IF.
       DNOU-999.
           MOVE DNO1-023     TO C-DNNO.
      *           REWRITE DNO1-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            NS-DNO_PNAME1 NS-DNO_LNAME DNO1-R RETURNING RET.
           IF  RET = 1
               MOVE "NS-DNO"     TO ERR-F
               MOVE "R"          TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       DNOU-EX.
           EXIT.
      ***
       SDWDU-RTN.
           MOVE C-DNNO     TO SDWJNO.
           MOVE ZERO       TO SDWLNO.
      *           START SDW KEY NOT LESS SDW-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDW_PNAME1 "SDW-KEY" " NOT LESS " SDW-KEY RETURNING RET.
           IF  RET = 1
               GO TO SDWDU-EX
           END-IF.
       SDWDU-000.
      *           READ SDW NEXT AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDW_PNAME1 BY REFERENCE SDW-REC " "
            RETURNING RET.
           IF  RET = 1
                GO TO SDWDU-999
           END-IF.
           IF  C-DNNO NOT = SDWJNO
               GO TO SDWDU-999
           END-IF.
           MOVE SDW-KEY     TO ERR-K.
      *           DELETE SDW INVALID KEY
      *///////////////
           CALL "DB_Delete" USING SDW_PNAME1 RETURNING RET.
           IF  RET = 1
               MOVE "SDW"     TO ERR-F
               MOVE "D"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
           GO TO SDWDU-000.
       SDWDU-999.
           MOVE LOW-VALUE     TO SDW-KEY.
      *           START SDW KEY NOT LESS SDW-KEY INVALID KEY
      *///////////////
           CALL "DB_Start" USING
            SDW_PNAME1 "SDW-KEY" " NOT LESS " SDW-KEY RETURNING RET.
           IF  RET = 1
               GO TO SDWDU-EX
           END-IF
      *           READ SDW NEXT UNLOCK AT END
      *///////////////
           CALL "DB_Read" USING
            "NEXT AT END" SDW_PNAME1 BY REFERENCE SDW-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               GO TO SDWDU-EX
           END-IF.
       SDWDU-EX.
           EXIT.
      ***
       SDWWU-RTN.
           MOVE 1     TO I.
       SDWWU-000.
           IF (C-KRCDM(I) = ZERO) AND
              (C-KSCDM(I) = ZERO)
               GO TO SDWWU-999
           END-IF.
           MOVE SPACE     TO SDW-REC.
           INITIALIZE SDW-REC.
           MOVE CRDATE          TO SDWYMD.
           MOVE C-DNNO          TO SDWJNO.
           MOVE I               TO SDWLNO.
           MOVE C-KARI(I)       TO SDWKARI.
           MOVE C-KASI(I)       TO SDWKASI.
           MOVE C-TKCD          TO SDWCUST.
           MOVE C-NAMEN         TO SDWNAMEN.
           MOVE C-TEKICD(I)     TO SDWTEKICD.
           MOVE C-TEKI(I)       TO SDWTEKI.
           IF  C-KRCDM(I) = 0140
               MOVE C-KRNKCD(I)     TO SDWNKCD
               MOVE C-KRNSC(I)      TO SDWNSC
               MOVE C-KRSKNG(I)     TO SDWSKNG
           END-IF.
           IF  C-KSCDM(I) = 0140
               MOVE C-KSNKCD(I)     TO SDWNKCD
               MOVE C-KSNSC(I)      TO SDWNSC
               MOVE C-KSSKNG(I)     TO SDWSKNG
           END-IF.
           MOVE C-TKD(I)        TO SDWTKD.
           IF  C-KRCDM(I) = W-KRCDM OR W-KSCDM
               MOVE C-KRTAX(I)  TO SDWETAX
               MOVE " " TO KRTAXW
           END-IF.
           MOVE W-SKDD          TO SDWSKD.
           MOVE W-HHC           TO SDWHHC.
           MOVE C-ZHC(I)        TO SDWZHC.
           IF  SDWHHC NOT = 1 AND 9
               IF  0140 NOT = C-KRCDM(I) AND C-KSCDM(I)
                   MOVE 8           TO SDWHHC
               ELSE
                   IF (C-KRCDM(I) > 4999 AND < 6000) OR
                      (C-KSCDM(I) > 4999 AND < 6000)
                       MOVE 8          TO SDWHHC
                   END-IF
               END-IF
           END-IF.
           IF  C-KSCDM(I) = W-KRCDM OR W-KSCDM
               MOVE C-KSTAX(I)  TO SDWETAX
               MOVE " "         TO KSTAXW
           END-IF.
           MOVE C-TENO(I)       TO SDWTENO.
           MOVE C-TCD           TO SDWTCD.
           MOVE SPACE           TO SDWSIN.
           MOVE SDW-KEY         TO ERR-K.
      *           WRITE SDW-REC INVALID KEY
      *///////////////
           CALL "DB_Insert" USING
            SDW_PNAME1 SDW_LNAME SDW-REC RETURNING RET.
           IF  RET = 1
               MOVE "SDW"     TO ERR-F
               MOVE "W"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       SDWWU-999.
           IF  I NOT = 5
               ADD 1     TO I
               GO TO SDWWU-000
           END-IF.
           IF  C-TKCD = ZERO OR 99999
               GO TO SDWWU-EX
           END-IF.
           MOVE ZERO TO W-NG.
           MOVE TK-NG TO W-NGS.
           IF  W-NEN2 >= DATE-NF1 AND <= DATE-NT1
               ADD DATE-NC1 TO W-NEN
           END-IF.
           IF  W-NEN2 >= DATE-NF2 AND <= DATE-NT2
               ADD DATE-NC2 TO W-NEN
           END-IF.
           IF  CRYM > W-NG
               MOVE CRYM TO W-NG
               MOVE W-NGS TO TK-NG
      *               REWRITE TK-REC INVALID KEY
      *///////////////
               CALL "DB_Update" USING
                TK_PNAME1 TK_LNAME TK-REC RETURNING RET
               IF  RET = 1
                   CALL "SD_Output" USING "MG-12" MG-12 "p"
                                                  RETURNING RESU
                   CALL "SD_Output" USING "DISP-BUZ-B" DISP-BUZ-B "p"
                                             RETURNING RESU
                   CALL "SD_Output" USING "DISP-MSG-SPACE"
                                         DISP-MSG-SPACE "p"
                                         RETURNING RESU
               END-IF
           END-IF.
       SDWWU-EX.
           EXIT.
      ***
       TDTDU-RTN.
           MOVE 1     TO I.
       TDTDU-000.
           IF  WBU-TENO(I) = ZERO
               GO TO TDTDU-999
           END-IF.
           MOVE WBU-TENO(I) TO TD-KEY.
      *           READ TDT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TDTDU-999
           END-IF.
           IF  TD-HCZ = 0
               GO TO TDTDU-999
           END-IF.
           MOVE 0           TO TD-HCZ.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               MOVE "TDTM"    TO ERR-F
               MOVE "R"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       TDTDU-999.
           IF  I     NOT =  5
               ADD  1  TO  I
               GO TO TDTDU-000
           END-IF.
       TDTDU-EX.
           EXIT.
      ***
       TDTWU-RTN.
           MOVE 1     TO I.
       TDTWU-000.
           IF  C-TENO(I) = ZERO
               GO TO TDTWU-999
           END-IF.
           MOVE C-TENO(I) TO TD-KEY.
      *           READ TDT-M INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TDT-M_PNAME1 BY REFERENCE TDT-R " "
            RETURNING RET.
           IF  RET = 1
               GO TO TDTWU-999
           END-IF.
           IF  TD-HCZ = 1 OR 9
               GO TO TDTDU-999
           END-IF.
           MOVE 1           TO TD-HCZ.
      *           REWRITE TDT-R INVALID KEY
      *///////////////
           CALL "DB_Update" USING
            TDT-M_PNAME1 TDT-M_LNAME TDT-R RETURNING RET.
           IF  RET = 1
               MOVE "TDTM"    TO ERR-F
               MOVE "R"       TO ERR-M
               PERFORM ERR-ENT THRU ERR-EXT
           END-IF.
       TDTWU-999.
           IF  I     NOT =  5
               ADD  1  TO  I
               GO TO TDTWU-000
           END-IF.
       TDTWU-EX.
           EXIT.
      ***
       CLSE-ENT.
           CALL "DB_F_Close" USING BY REFERENCE HKBM_IDLST HKBM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE KNG_IDLST KNG_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE AM_IDLST AM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BNM_IDLST BNM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE BM_IDLST BM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TK_IDLST TK_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TKI_IDLST TKI_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE HH-F_IDLST HH-F_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE GYM_IDLST GYM_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDH_IDLST SDH_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE SDW_IDLST SDW_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE TDT-M_IDLST TDT-M_PNAME1.
           CALL "DB_F_Close" USING
            BY REFERENCE NS-DNO_IDLST NS-DNO_PNAME1.
           CALL "DB_F_Close" USING BY REFERENCE TSKF_IDLST TSKF_PNAME1.
           CALL "SD_Output" USING "DISP-C" DISP-C "p"
                                         RETURNING RESU.
       CLSE-EXT.
           EXIT.
      *********
       AMG-RTN.
           MOVE 0     TO INV-SW.
           IF  DRCR-SW = 1
               MOVE C-KRCDM(I)     TO AM-KEY
           ELSE
               MOVE C-KSCDM(I)     TO AM-KEY
           END-IF.
      *           READ AM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" AM_PNAME1 BY REFERENCE AM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
               GO TO AMG-EX
           END-IF.
           IF  DRCR-SW = 1
               MOVE TEG-BAN     TO C-KR-TB(I)
               MOVE HOJYO       TO C-KR-HO(I)
               MOVE KEIHI       TO C-KR-KH(I)
               MOVE BS-PL       TO C-KR-BSPL(I)
           ELSE
               MOVE TEG-BAN     TO C-KS-TB(I)
               MOVE HOJYO       TO C-KS-HO(I)
               MOVE KEIHI       TO C-KS-KH(I)
               MOVE BS-PL       TO C-KS-BSPL(I)
           END-IF.
       AMG-EX.
           EXIT.
      ***
       KNGG-RTN.
           MOVE 0     TO INV-SW.
           IF  DRCR-SW = 1
               MOVE C-KRCDM(I)     TO K-ACCD
               IF  HOJO-SW = 0
                   MOVE ZERO           TO K-HOCD
               ELSE
                   MOVE C-KRCDS(I)     TO K-HOCD
               END-IF
           ELSE
               MOVE C-KSCDM(I)     TO K-ACCD
               IF  HOJO-SW = 0
                   MOVE ZERO           TO K-HOCD
               ELSE
                   MOVE C-KSCDS(I)     TO K-HOCD
               END-IF
           END-IF.
      *           READ KNG UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" KNG_PNAME1 BY REFERENCE KNG-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
               GO TO KNGG-EX
           END-IF.
           IF  DRCR-SW = 1
               MOVE KNGTAX     TO C-KR-TAX(I)
               IF  HOJO-SW = 0
                   MOVE KNGNMN     TO C-KR-CDMN(I)
               ELSE
                   MOVE KNGNMN     TO C-KR-CDSN(I)
               END-IF
           ELSE
               MOVE KNGTAX     TO C-KS-TAX(I)
               IF  HOJO-SW = 0
                   MOVE KNGNMN     TO C-KS-CDMN(I)
               ELSE
                   MOVE KNGNMN     TO C-KS-CDSN(I)
               END-IF
           END-IF.
       KNGG-EX.
           EXIT.
      ***
       TKG-RTN.
           MOVE 0     TO INV-SW.
      *           READ TK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TK_PNAME1 BY REFERENCE TK-REC " "
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE     TO TK-NAMEN
               MOVE 0         TO TK-SS
               MOVE 1         TO INV-SW
           END-IF.
           MOVE TK-NAMEN TO C-NAMEN.
       TKG-EX.
           EXIT.
      ***
       BMG-RTN.
           MOVE 0     TO INV-SW.
           IF  DRCR-SW = 1
               MOVE C-KRCDS(I)     TO BM-KEY
           ELSE
               MOVE C-KSCDS(I)     TO BM-KEY
           END-IF.
      *           READ BM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BM_PNAME1 BY REFERENCE BM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE     TO BANKNMN
               MOVE 1     TO INV-SW
           END-IF.
       BMG-EX.
           EXIT.
      ***
       GYMG-RTN.
           MOVE 0     TO INV-SW.
           IF  DRCR-SW = 1
               MOVE C-KRCDM(I)     TO GYM-011
               MOVE C-KRCDS(I)     TO GYM-012
           ELSE
               MOVE C-KSCDM(I)     TO GYM-011
               MOVE C-KSCDS(I)     TO GYM-012
           END-IF.
      *           READ GYM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" GYM_PNAME1 BY REFERENCE GYM-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
           END-IF.
       GYMG-EX.
           EXIT.
      ***
       BNMG-RTN.
           MOVE 0     TO INV-SW.
           IF  DRCR-SW = 1
               MOVE C-KRSECT(I)     TO BNM-KEY
           ELSE
               MOVE C-KSSECT(I)     TO BNM-KEY
           END-IF.
      *           READ BNM UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" BNM_PNAME1 BY REFERENCE BNM-REC "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
               GO TO BNMG-EX
           END-IF.
           IF  BNM-BUMONKBN = 1
               MOVE 1     TO INV-SW
           END-IF.
       BNMG-EX.
           EXIT.
      ***
       HHFG-RTN.
           MOVE 0     TO INV-SW.
           IF  DRCR-SW = 1
               MOVE C-KRSECT(I)     TO HH-BUCD
               MOVE C-KRCDM(I)      TO HH-KACD
               MOVE C-KRCDS(I)      TO HH-HOCD
           ELSE
               MOVE C-KSSECT(I)     TO HH-BUCD
               MOVE C-KSCDM(I)      TO HH-KACD
               MOVE C-KSCDS(I)      TO HH-HOCD
           END-IF.
      *           READ HH-F UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" HH-F_PNAME1 BY REFERENCE HH-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE 1     TO INV-SW
           END-IF.
       HHFG-EX.
           EXIT.
      ***
       TKIG-RTN.
           MOVE 0     TO INV-SW.
           MOVE C-TEKICD(I)     TO TKI-KEY.
      *           READ TKI UNLOCK INVALID KEY
      *///////////////
           CALL "DB_Read" USING
            "INVALID KEY" TKI_PNAME1 BY REFERENCE TKI-R "UNLOCK"
            RETURNING RET.
           IF  RET = 1
               MOVE SPACE     TO TKI-02
               MOVE 1         TO INV-SW
           END-IF.
           MOVE TKI-02     TO C-TEKI(I).
       TKIG-EX.
           EXIT.
      *********
       COPY LPMSG_PR.
      *********
